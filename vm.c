#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "object.h"
#include "memory.h"
#include "vm.h"

/**
 * Here we have a static VM instance. This is not necessarily a sound eng
 * choice. Although a VM designed to be embedded in other host applications,
 * it does give you more flexibility if we were to pass around a pointer to
 * the VM instance to control where/when memory is allocated.
 */
VM vm;

static Value clockNative(int argCount, Value* args) {
  return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

static void resetStack() {
  vm.stackTop = vm.stack;
  // on startup, the call frame stack is empty
  vm.frameCount = 0;
  // the list of open upvalues starts out empty
  vm.openUpvalues = NULL;
}

static void runtimeError(const char* format, ...) {
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);

  for (int i = vm.frameCount - 1; i >= 0; i--) {
    CallFrame* frame = &vm.frames[i];
    ObjFunction* function = frame->closure->function;
    // -1 because the IP is sitting on the next instruction to be executed
    size_t instruction = frame->ip - function->chunk.code - 1;
    fprintf(stderr, "[line %d] in ", function->chunk.lines[instruction]);
    if (function->name == NULL) {
      fprintf(stderr, "script\n");
    } else {
      fprintf(stderr, "%s()\n", function->name->chars);
    }
  }

  resetStack();
}

static void defineNative(const char* name, NativeFn function) {
  push(OBJ_VAL(copyString(name, (int)strlen(name))));
  push(OBJ_VAL(newNative(function)));
  tableSet(&vm.globals, AS_STRING(vm.stack[0]), vm.stack[1]);
  pop();
  pop();
}

void initVM() {
  resetStack();
  vm.objects = NULL;
  vm.bytesAllocated = 0;
  vm.nextGC = 1024 * 1024;

  vm.grayCount = 0;
  vm.grayCapacity = 0;
  vm.grayStack = NULL;

  initTable(&vm.globals);
  initTable(&vm.strings);

  vm.initString = NULL; // prevent reading initstring before initialized for GC
  vm.initString = copyString("init", 4);

  defineNative("clock", clockNative);
}

void freeVM() {
  freeTable(&vm.globals);
  freeTable(&vm.strings);
  vm.initString = NULL;
  freeObjects();
}

void push(Value value) {
  *vm.stackTop = value;
  vm.stackTop++;
}

Value pop() {
  vm.stackTop--;
  return *vm.stackTop;
}

static Value peek(int distance) {
  return vm.stackTop[-1 - distance];
}

static bool call(ObjClosure* closure, int argCount) {
  if (argCount != closure->function->arity) {
    runtimeError("Expected %d arguments but got %d.",
        closure->function->arity, argCount);
    return false;
  }

  if (vm.frameCount == FRAMES_MAX) {
    runtimeError("Stack overflow.");
    return false;
  }

  CallFrame* frame = &vm.frames[vm.frameCount++];
  frame->closure = closure;
  frame->ip = closure->function->chunk.code;

  // -1 accounts for slot zero, see image in this chapter
  frame->slots = vm.stackTop - argCount - 1;
  return true;
}

static bool callValue(Value callee, int argCount) {
  if (IS_OBJ(callee)) {
    switch (OBJ_TYPE(callee)) {
      case OBJ_BOUND_METHOD: {
        ObjBoundMethod* bound = AS_BOUND_METHOD(callee);
        // the top of the stack has all args, and under those is the closure of
        // the called method (slot 0 for the CallFrame)
        vm.stackTop[-argCount - 1] = bound->receiver;
        Value initializer;
        return (call(bound->method, argCount));
      }
      case OBJ_CLASS: {
        ObjClass* klass = AS_CLASS(callee);
        vm.stackTop[-argCount - 1] = OBJ_VAL(newInstance(klass));
        Value initializer;
        if (tableGet(&klass->methods, vm.initString, &initializer)) {
          // push a new CallFrame on to the stack for init()
          // implicitly forward any args to the init method, use `call` to check arity
          return call(AS_CLOSURE(initializer), argCount);
        } else if (argCount != 0) {
          // we don't need to declare init() on a method, but it's an error to
          // pass a non-zero amount of args to create a class w/o one
          runtimeError("Expected 0 arguments but got %d.", argCount);
          return false;
        }
        return true;
      }
      case OBJ_CLOSURE: {
        return call(AS_CLOSURE(callee), argCount);
      }
      case OBJ_NATIVE: {
        NativeFn native = AS_NATIVE(callee);
        Value result = native(argCount, vm.stackTop - argCount);
        vm.stackTop -= argCount + 1;
        push(result);
        return true;
      }

      default: {
        break; // non-callable object type
      }
    }
  }

  runtimeError("Can only call functions and classes.");
  return false;
}

static bool invokeFromClass(ObjClass* klass, ObjString* name, int argCount) {
  Value method;
  if (!tableGet(&klass->methods, name, &method)) {
    runtimeError("Undefined property '%s'.", name->chars);
    return false;
  }

  return call(AS_CLOSURE(method), argCount);
}

static bool invoke(ObjString* name, int argCount) {
  // the receiver sits below the args on the stack, let's grab it
  Value receiver = peek(argCount);

  if (!IS_INSTANCE(receiver)) {
    runtimeError("Only instances have methods.");
    return false;
  }

  ObjInstance* instance = AS_INSTANCE(receiver);

  // look for a field with the same name (see 28.5.1)
  Value value;
  if (tableGet(&instance->fields, name, &value)) {
    vm.stackTop[-argCount - 1] = value;
    return callValue(value, argCount);
  }
  return invokeFromClass(instance->klass, name, argCount);
}

static bool bindMethod(ObjClass* klass, ObjString* name) {
  Value method;
  if (!tableGet(&klass->methods, name, &method)) {
    runtimeError("Undefined property '%s'.", name->chars);
    return false;
  }

  // bind the receiver on the top of the stack
  ObjBoundMethod* bound = newBoundMethod(peek(0), AS_CLOSURE(method));
  // pop the instance
  pop();
  // push the top of the stack with the bound method
  push(OBJ_VAL(bound));
  return true;
}

static ObjUpvalue* captureUpvalue(Value* local) {
  ObjUpvalue* prevUpvalue = NULL;
  ObjUpvalue* upvalue = vm.openUpvalues;

  // start at the head of the list, which is the upvalue closest to the top of the stack
  while (upvalue != NULL && upvalue->location > local) {
    prevUpvalue = upvalue;
    upvalue = upvalue->next;
  }

  // if we found the slot that we're looking for, return the upvalue
  if (upvalue != NULL && upvalue->location == local) {
    return upvalue;
  }

  ObjUpvalue* createdUpvalue = newUpvalue(local);
  createdUpvalue->next = upvalue;

  if (prevUpvalue == NULL) {
    // we ran out of values to search (or the list was empty)
    vm.openUpvalues = createdUpvalue;
  } else {
    // we found an upvalue whose local slot is below the one we're looking for
    prevUpvalue->next = createdUpvalue;
  }

  return createdUpvalue;
}

static void closeUpvalues(Value* last) {
  while (vm.openUpvalues != NULL && vm.openUpvalues->location >= last) {
    ObjUpvalue* upvalue = vm.openUpvalues;
    upvalue->closed = *upvalue->location;
    upvalue->location = &upvalue->closed;
    vm.openUpvalues = upvalue->next;
  }
}

static void defineMethod(ObjString* name) {
  Value method = peek(0);
  ObjClass* klass = AS_CLASS(peek(1));
  tableSet(&klass->methods, name, method);
  pop();
}

static bool isFalsey(Value value) {
  return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static void concatenate() {
  ObjString* b = AS_STRING(peek(0));
  ObjString* a = AS_STRING(peek(1));

  int length = a->length + b->length;
  char* chars = ALLOCATE(char, length + 1);
  memcpy(chars, a->chars, a->length);
  memcpy(chars + a->length, b->chars, b->length);
  chars[length] = '\0';

  ObjString* result = takeString(chars, length);
  // pop the operands off the stack to make this GC-safe
  pop();
  pop();
  push(OBJ_VAL(result));
}

/**
 * This function is the heart of the entire VM. It runs for every instruction.
 * Performance therefore is critical! But, for this book we don't want to get
 * too in the weeds. Look up 'direct threaded code', 'jump table' &
 * 'computed goto' for more info
 */
static InterpretResult run() {
  // store the current topmost call frame, makes the macros below it a little
  // less verbose and keeps this value in a register (hopefully)
  CallFrame* frame = &vm.frames[vm.frameCount - 1];
/**
 * reads the byte at IP, *then* advances the pointer
 * eq to (*(frame->ip))++
 */
#define READ_BYTE() (*frame->ip++)

/**
 * Grab the next two bytes from the chunk and create an unsigned 16 bit int
 */
#define READ_SHORT() \
  (frame->ip += 2, \
  (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))

/**
 * first, read the next byte at the IP, use that value to look up the constant
 */
#define READ_CONSTANT() \
  (frame->closure->function->chunk.constants.values[READ_BYTE()])

/**
 * read the next byte at the IP, use that value to look up the constant, which
 * then must be converted to a lox string
 */
#define READ_STRING() AS_STRING(READ_CONSTANT())

// Macro for performing binary operations
#define BINARY_OP(valueType, op) \
    do { \
      if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) { \
        runtimeError("Operands must be numbers."); \
        return INTERPRET_RUNTIME_ERROR; \
      } \
      double b = AS_NUMBER(pop()); \
      double a = AS_NUMBER(pop()); \
      push(valueType(a op b)); \
    } while (false)

  for (;;) {
// on the fly, disassemble the instruction
#ifdef DEBUG_TRACE_EXECUTION
  printf("          ");
  // print from the bottom of the stack to the top (first in to last in)
  for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
    printf("[ ");
    printValue(*slot);
    printf(" ]");
  }
  printf("\n");
  disassembleInstruction(&frame->closure->function->chunk,
      (int)(frame->ip - frame->closure->function->chunk.code));
#endif
    uint8_t instruction;
    // this isn't the _fastest_ thing in the world, but it's better than non-standard C or hand written assembly
    // Additional terms: "Direct threaded code", "jump table", "computed goto"
    switch (instruction = READ_BYTE()) {
      case OP_CONSTANT: {
        Value constant = READ_CONSTANT();
        push(constant);
        break;
      }
      case OP_NIL: {
        push(NIL_VAL);
        break;
      }
      case OP_TRUE: {
        push(BOOL_VAL(true));
        break;
      }
      case OP_FALSE: {
        push(BOOL_VAL(false));
        break;
      }
      case OP_POP: {
        pop();
        break;
      }
      case OP_GET_LOCAL: {
        // take single byte when the local lives and push it on the stack
        uint8_t slot = READ_BYTE();
        push(frame->slots[slot]);
        break;
      }
      case OP_SET_LOCAL: {
        // take single byte when the local lives
        uint8_t slot = READ_BYTE();
        // assignment is just an expression, which should produce a value.
        // that value is the assigned value itself.
        frame->slots[slot] = peek(0);
        break;
      }
      case OP_GET_GLOBAL: {
        ObjString* name = READ_STRING();
        Value value;
        if (!tableGet(&vm.globals, name, &value)) {
          runtimeError("Undefined variable '%s'.", name->chars);
          return INTERPRET_RUNTIME_ERROR;
        }
        push(value);
        break;
      }
      case OP_DEFINE_GLOBAL: {
        ObjString* name = READ_STRING();
        // wait to pop, we need to read this and the gc could have run
        tableSet(&vm.globals, name, peek(0));
        pop();
        break;
      }
      case OP_SET_GLOBAL: {
        ObjString* name = READ_STRING();
        if (tableSet(&vm.globals, name, peek(0))) {
          // this is helpful in REPL sessions, we need to clean it up
          tableDelete(&vm.globals, name);
          runtimeError("Undefined variable '%s'.", name->chars);
          return INTERPRET_RUNTIME_ERROR;
        }
        break;
      }
      case OP_GET_UPVALUE: {
        uint8_t slot = READ_BYTE();
        push(*frame->closure->upvalues[slot]->location);
        break;
      }
      case OP_SET_UPVALUE: {
        uint8_t slot = READ_BYTE();
        *frame->closure->upvalues[slot]->location = peek(0);
        break;
      }
      case OP_GET_PROPERTY: {
        if (!IS_INSTANCE(peek(0))) {
          runtimeError("Only instances have properties.");
          return INTERPRET_RUNTIME_ERROR;
        }

        // the expression to the left of the dot has been executed and is at
        // top of the stack
        ObjInstance* instance = AS_INSTANCE(peek(0));
        ObjString* name = READ_STRING();

        Value value;
        // fields take priority over methods, shadows them
        if (tableGet(&instance->fields, name, &value)) {
          pop(); // Instance.
          push(value);
          break;
        }

        if (!bindMethod(instance->klass, name)) {
          return INTERPRET_RUNTIME_ERROR;
        }
        break;
      }
      case OP_SET_PROPERTY: {
        if (!IS_INSTANCE(peek(1))) {
          runtimeError("Only instances have fields.");
          return INTERPRET_RUNTIME_ERROR;
        }

        // instance who is having the property set
        ObjInstance* instance = AS_INSTANCE(peek(1));
        // the value being stored is just above that
        tableSet(&instance->fields, READ_STRING(), peek(0));

        Value value = pop();
        pop();
        push(value);
        break;
      }
      case OP_GET_SUPER: {
        ObjString* name = READ_STRING();
        ObjClass* superclass = AS_CLASS(pop());
        // fields are not inherited, so this always resolves to a method, which
        // differs from say OP_GET_PROPERTY (above)
        if (!bindMethod(superclass, name)) {
          return INTERPRET_RUNTIME_ERROR;
        }
        break;
      }
      case OP_EQUAL: {
        Value b = pop();
        Value a = pop();
        push(BOOL_VAL(valuesEqual(a, b)));
        break;
      }
      case OP_GREATER: {
        BINARY_OP(BOOL_VAL, >);
        break;
      }
      case OP_LESS: {
        BINARY_OP(BOOL_VAL, <);
        break;
      }
      case OP_ADD: {
        if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
          concatenate();
        } else if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
          double b = AS_NUMBER(pop());
          double a = AS_NUMBER(pop());
          push(NUMBER_VAL(a + b));
        } else {
          runtimeError("Operands must be two numbers or two strings.");
          return INTERPRET_RUNTIME_ERROR;
        }
        break;
      }
      case OP_SUBTRACT: {
        BINARY_OP(NUMBER_VAL, -);
        break;
      }
      case OP_MULTIPLY: {
        BINARY_OP(NUMBER_VAL, *);
        break;
      }
      case OP_DIVIDE: {
        BINARY_OP(NUMBER_VAL, /);
        break;
      }
      case OP_NOT: {
        push(BOOL_VAL(isFalsey(pop())));
        break;
      }
      case OP_NEGATE: {
        if(!IS_NUMBER(peek(0))) {
          runtimeError("Operand must be a number.");
          return INTERPRET_RUNTIME_ERROR;
        }

        push(NUMBER_VAL(-AS_NUMBER(pop())));
        break;
      }
      case OP_PRINT: {
        // we've already executed the code for the print statement, just need
        // to pop and print it at this point
        printValue(pop());
        printf("\n");
        break;
      }
      case OP_JUMP: {
        uint16_t offset = READ_SHORT();
        frame->ip += offset;
        break;
      }
      case OP_JUMP_IF_FALSE: {
        uint16_t offset = READ_SHORT();
        // if the condition from the if statement is falsy, jump
        if (isFalsey(peek(0))) {
          frame->ip += offset;
        }
        break;
      }
      case OP_LOOP: {
        uint16_t offset = READ_SHORT();
        frame->ip -= offset;
        break;
      }
      case OP_CALL: {
        int argCount = READ_BYTE();
        if (!callValue(peek(argCount), argCount)) {
          return INTERPRET_RUNTIME_ERROR;
        }
        // there's a new callframe on the stack for the called fn
        // update the cached pointer to the current frame
        frame = &vm.frames[vm.frameCount - 1];
        break;
      }
      case OP_INVOKE: {
        ObjString* method = READ_STRING();
        int argCount = READ_BYTE();
        if (!invoke(method, argCount)) {
          return INTERPRET_RUNTIME_ERROR;
        }
        frame = &vm.frames[vm.frameCount - 1];
        break;
      }
      case OP_SUPER_INVOKE: {
        ObjString* method = READ_STRING();
        int argCount = READ_BYTE();
        ObjClass* superclass = AS_CLASS(pop());
        if (!invokeFromClass(superclass, method, argCount)) {
          return INTERPRET_RUNTIME_ERROR;
        }
        frame = &vm.frames[vm.frameCount - 1];
        break;
      }
      case OP_CLOSURE: {
        // Load the function from the constant table
        ObjFunction* function = AS_FUNCTION(READ_CONSTANT());
        // wrap it
        ObjClosure* closure = newClosure(function);
        // put it on the stack
        push(OBJ_VAL(closure));
        for (int i = 0; i < closure->upvalueCount; i++) {
          uint8_t isLocal = READ_BYTE();
          uint8_t index = READ_BYTE();
          if (isLocal) {
            closure->upvalues[i] = captureUpvalue(frame->slots + index);
          } else {
            closure->upvalues[i] = frame->closure->upvalues[index];
          }
        }
        break;
      }
      case OP_CLOSE_UPVALUE: {
        // move to the heap
        closeUpvalues(vm.stackTop - 1);
        // pop of the stack
        pop();
        break;
      }
      case OP_RETURN: {
        Value result = pop();

        closeUpvalues(frame->slots);

        // get rid of the entire call frame
        vm.frameCount--;
        // the entire programe is done, exit gracefully
        if (vm.frameCount == 0) {
          pop();
          return INTERPRET_OK;
        }

        // discard all of the slots the callee was using for params, locals
        vm.stackTop = frame->slots;
        push(result);

        // update the cached pointer to the current frame
        frame = &vm.frames[vm.frameCount - 1];
        break;
      }
      case OP_CLASS: {
        push(OBJ_VAL(newClass(READ_STRING())));
        break;
      }
      case OP_INHERIT: {
        // this differs from jlox in that we don't 'walk' the chain of 
        // superclasses. here we copy all the super class's methods onto the
        // child. 'copy down inheritance' - but only works bc classes are 
        // closed, unlike JS (monkey patching)
        //
        // this won't override method overrides since we emit OP_INHERIT after
        // OP_CLASS but before any method declarations. neat.
        Value superclass = peek(1);
        if (!IS_CLASS(superclass)) {
          runtimeError("Superclass must be a class.");
          return INTERPRET_RUNTIME_ERROR;
        }

        ObjClass* subclass = AS_CLASS(peek(0));
        tableAddAll(&AS_CLASS(superclass)->methods, &subclass->methods);
        pop(); // subclass
        break;
      }
      case OP_METHOD: {
        defineMethod(READ_STRING());
        break;
      }
    }
  }

#undef READ_BYTE
#undef READ_SHORT
#undef READ_CONSTANT
#undef READ_STRING
#undef BINARY_OP
}

InterpretResult interpret(const char* source) {
  // compile source, get the top level function
  ObjFunction* function = compile(source);
  if (function == NULL) {
    return INTERPRET_COMPILE_ERROR;
  }

  // store the function on the stack and prepare the initial call frame
  push(OBJ_VAL(function));
  ObjClosure* closure = newClosure(function);
  pop();
  push(OBJ_VAL(closure));
  callValue(OBJ_VAL(closure), 0);

  return run();
}