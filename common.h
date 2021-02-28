#ifndef clox_common_h
#define clox_commmon_h

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

// Flag for printing out chunk bytecode
#define DEBUG_PRINT_CODE
#undef DEBUG_PRINT_CODE
// Flag for diagnostic logging
#define DEBUG_TRACE_EXECUTION
#undef DEBUG_TRACE_EXECUTION

#define UINT8_COUNT (UINT8_MAX + 1)

#endif