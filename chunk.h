#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

// In clox, an instruction has a one byte operation code (i.e. opcode)
typedef enum {
  OP_CONSTANT, // "load the constant for use"
  OP_NIL,
  OP_TRUE,
  OP_FALSE,
  OP_POP,
  OP_GET_GLOBAL,
  OP_GET_LOCAL,
  OP_DEFINE_GLOBAL,
  OP_SET_LOCAL,
  OP_SET_GLOBAL,
  OP_EQUAL,
  OP_GREATER,
  OP_LESS,
  OP_ADD,
  OP_SUBTRACT,
  OP_MULTIPLY,
  OP_DIVIDE,
  OP_NOT,           // "!true"
  OP_NEGATE,        // "negate a single operand"
  OP_PRINT,         // "print"
  OP_JUMP,          // "unconditoinal jump"
  OP_JUMP_IF_FALSE, // "jump for falsy if statements"
  OP_LOOP,          // "while, for loops"
  OP_RETURN,        // "return from the current function"
} OpCode;

typedef struct {
  // the number of entries
  int count;
  // the size of the array (potentially)
  int capacity;
  // dynamically sized array of bytes is needed, since we don't know how big
  // this will be at compile time (note this is still jsut a pointer).
  uint8_t* code;
  // line number error info, mirrors that byte in the bytecode by array index - so we don't need a separate count/capactiy
  int* lines;
  // constants associated with the instructions in the chunk
  ValueArray constants;
} Chunk;

void initChunk(Chunk* chunk);
void freeChunk(Chunk* chunk);
void writeChunk(Chunk* chunk, uint8_t byte, int line);
int addConstant(Chunk* chunk, Value value);

#endif