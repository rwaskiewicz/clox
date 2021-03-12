#ifndef clox_vm_h
#define clox_vm_h

#include "object.h"
#include "table.h"
#include "value.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

/*
 * Data structure for storing information related to fn calls. Specifically, it
 * represents a single ongoing fn call.
 */
typedef struct {
  // pointer to the closure of the fn being called
  ObjClosure* closure;
  // ip of the caller to jump back to
  uint8_t* ip;
  // points to the VM's value stack at the first position the fn can use
  Value* slots;
} CallFrame;

typedef struct {
  CallFrame frames[FRAMES_MAX];
  // current height of the frame stack
  int frameCount;
  Value stack[STACK_MAX];
  // pointer to track the top of the stack, specifically just passed the top of
  // the stack (the last full item is technically at stackTop-1). IE stackTop
  // points to where the next item will go
  Value* stackTop;
  // Storage for global variables
  Table globals;
  // Table of interned strings
  Table strings;
  // HEAD pointer to the list of open upvalues
  ObjUpvalue* openUpvalues;
  // Head of our intrusive list for basic memory leak prevention
  Obj* objects;
  // the number of objects we have colored gray
  int grayCount;
  // the number of objects we can hold that we've colored gray
  int grayCapacity;
  // the objects we've colored gray
  Obj** grayStack;
} VM;

typedef enum {
  INTERPRET_OK,
  INTERPRET_COMPILE_ERROR,
  INTERPRET_RUNTIME_ERROR
} InterpretResult;

extern VM vm;

void initVM();
void freeVM();
InterpretResult interpret(const char* source);
void push(Value value);
Value pop();

#endif