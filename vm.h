#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "value.h"

#define STACK_MAX 256

typedef struct {
  Chunk* chunk;
  // pointer to the current instruction about to be executed - this value will
  // need to be accessed by other functions other than `run` in this impl
  // AKA a "Program Counter" (PC)
  uint8_t* ip;
  Value stack[STACK_MAX];
  // pointer to track the top of the stack, specifically just passed the top of
  // the stack (the last full item is technically at stackTop-1). IE stackTop
  // points to where the next item will go
  Value* stackTop;
} VM;

typedef enum {
  INTERPRET_OK,
  INTERPRET_COMPILE_ERROR,
  INTERPRET_RUNTIME_ERROR
} InterpretResult;

void initVM();
void freeVM();
InterpretResult interpret(const char* source);
void push(Value value);
Value pop();

#endif