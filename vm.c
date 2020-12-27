#include <stdio.h>

#include "common.h"
#include "debug.h"
#include "vm.h"

/**
 * Here we have a static VM instance. This is not necessarily a sound eng
 * choice. Although a VM designed to be embedded in other host applications,
 * it does give you more flexibility if we were to pass around a pointer to
 * the VM instance to control where/when memory is allocated.
 */
VM vm;

void initVM() {

}

void freeVM() {

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

  for (;;) {
// on the fly, disassemble the instruction
#ifdef DEBUG_TRACE_EXECUTION
  disassembleInstruction(vm.chunk, (int)(vm.ip - vm.chunk->code));
#endif
    uint8_t instruction;
    switch (instruction = READ_BYTE()) {
      case OP_CONSTANT: {
        Value constant = READ_CONSTANT();
        printValue(constant);
        printf("\n");
        break;
      }
      case OP_RETURN: {
        return INTERPRET_OK;
      }
    }
  }

#undef READ_BYTE
#undef READ_CONSTANT
}

InterpretResult interpret(Chunk* chunk) {
  vm.chunk = chunk;
  vm.ip = vm.chunk->code;
  return run();
}