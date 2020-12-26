#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"

// In clox, an instruction has a one byte operation code (i.e. opcode)
typedef enum {
  OP_RETURN, // "return from the current function"
} OpCode;

typedef struct {
  // the number of entries
  int count;
  // the size of the array (potentially)
  int capacity;
  // dynamically sized array of bytes is needed, since we don't know how big 
  // this will be at compile time (note this is still jsut a pointer).
  uint8_t* code;
} Chunk;

void initChunk(Chunk* chunk);
void freeChunk(Chunk* chunk);
void writeChunk(Chunk* chunk, uint8_t byte);

#endif