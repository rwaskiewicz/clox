#include <stdio.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "vm.h"

/**
 * Here we have a static VM instance. This is not necessarily a sound eng
 * choice. Although a VM designed to be embedded in other host applications,
 * it does give you more flexibility if we were to pass around a pointer to
 * the VM instance to control where/when memory is allocated.
 */
VM vm;

static void resetStack() {
  vm.stackTop = vm.stack;
}

void initVM() {
  resetStack();
}

void freeVM() {

}

void push(Value value) {
  *vm.stackTop = value;
  vm.stackTop++;
}

Value pop() {
  vm.stackTop--;
  return *vm.stackTop;
}

/**
 * This function is the heart of the entire VM. It runs for every instruction.
 * Performance therefore is critical! But, for this book we don't want to get
 * too in the weeds. Look up 'direct threaded code', 'jump table' &
 * 'computed goto' for more info
 */
static InterpretResult run() {
// reads the byte at IP, *then* advances the pointer
#define READ_BYTE() (*vm.ip++)

// first, read the next byte at the IP, use that value to look up the constant
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])

// Macro for performing binary operations
#define BINARY_OP(op) \
    do { \
      double b = pop(); \
      double a = pop(); \
      push(a op b); \
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
  disassembleInstruction(vm.chunk, (int)(vm.ip - vm.chunk->code));
#endif
    uint8_t instruction;
    switch (instruction = READ_BYTE()) {
      case OP_CONSTANT: {
        Value constant = READ_CONSTANT();
        push(constant);
        break;
      }
      case OP_ADD: {
        BINARY_OP(+);
        break;
      }
      case OP_SUBTRACT: {
        BINARY_OP(-);
        break;
      }
      case OP_MULTIPLY: {
        BINARY_OP(*);
        break;
      }
      case OP_DIVIDE: {
        BINARY_OP(/);
        break;
      }
      case OP_NEGATE: {
        push(-pop());
        break;
      }
      case OP_RETURN: {
        printValue(pop());
        printf("\n");
        return INTERPRET_OK;
      }
    }
  }

#undef READ_BYTE
#undef READ_CONSTANT
#undef BINARY_OP
}

InterpretResult interpret(const char* source) {
  compile(source);
  return INTERPRET_OK;
}