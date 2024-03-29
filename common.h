#ifndef clox_common_h
#define clox_common_h

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

// Flag for NaN boxing support
#define NAN_BOXING

// Flag for printing out chunk bytecode
#define DEBUG_PRINT_CODE
#undef DEBUG_PRINT_CODE
// Flag for diagnostic logging
#define DEBUG_TRACE_EXECUTION
#undef DEBUG_TRACE_EXECUTION

// Flag for debugging garbage collection via forcing GC
#define DEBUG_STRESS_GC
#undef DEBUG_STRESS_GC
// Flag for debugging garbage collection via logging
#define DEBUG_LOG_GC
#undef DEBUG_LOG_GC

#define UINT8_COUNT (UINT8_MAX + 1)

#endif