#include <stdio.h>
#include <stdlib.h>

#include "common.h"
#include "scanner.h"

/**
 * Wrap the state of the Scanner in a struct. It tracks how far it's gone
 * through the user's source with `start` and `current`
 */
typedef struct {
  // the beginning of the current lexeme being scanned
  const char* start;
  // the current character being looked at
  const char* current;
  // the line that the lexeme is on
  int line;
} Scanner;

/**
 * Similar to VM, we create a global module variable of the type `Scanner` so
 * we don't have to pass it around all the time
 */
Scanner scanner;

void initScanner(const char* source) {
  scanner.start = source;
  scanner.current = source;
  scanner.line = 1;
}