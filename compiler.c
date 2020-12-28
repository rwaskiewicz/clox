#include <stdio.h>
#include <stdlib.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"

typedef struct {
  Token current;
  Token previous;
  bool hadError;
  bool panicMode;
} Parser;

typedef enum {
  PREC_NONE,
  PREC_ASSIGNMENT, // =
  PREC_OR,         // or
  PREC_AND,        // and
  PREC_EQUALITY,   // == !=
  PREC_COMPARISON, // < > <= >=
  PREC_TERM,       // + -
  PREC_FACTOR,     // * /
  PREC_UNARY,      // ! -
  PREC_CALL,       // . ()
  PREC_PRIMARY,
} Precedence;

Parser parser;

Chunk *compilingChunk;

/**
 * Return the chunk we're writing
 */
static Chunk* currentChunk() {
  return compilingChunk;
}

static void errorAt(Token* token, const char* message) {
  if (parser.panicMode) {
    return;
  }

  parser.panicMode = true;

  fprintf(stderr, "[line %d] Error", token->line);

  if (token->type == EOF) {
    fprintf(stderr, " at end");
  } else if (token->type == TOKEN_ERROR) {
    // Do Nothing
  } else {
    fprintf(stderr, " at '%.*s'", token->length, token->start);
  }

  fprintf(stderr, ": %s\n", message);
  parser.hadError = true;
}

static void error(const char* message) {
  errorAt(&parser.previous, message);
}

static void errorAtCurrent(const char* message) {
  errorAt(&parser.current, message);
}

static void advance() {
  parser.previous = parser.current;

  for (;;) {
    parser.current = scanToken();
    if (parser.current.type != TOKEN_ERROR) {
      break;
    }

    errorAtCurrent(parser.current.start);
  }
}

static void consume(TokenType type, const char* message) {
  if (parser.current.type == type) {
    advance();
    return;
  }

  errorAtCurrent(message);
}

static void emitByte(uint8_t byte) {
  // note that the byte may be an opcode or an operand to an instruction
  writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
  emitByte(byte1);
  emitByte(byte2);
}

static void emitReturn() {
  // As of 17.3, we can only deal with expressions in the VM - we need to
  // return this op code to print the result out
  emitByte(OP_RETURN);
}

static uint8_t makeConstant(Value value) {
  int constant = addConstant(currentChunk(), value);
  if (constant > UINT8_MAX) {
     error("Too many constants in one chunk.");
     return 0;
  }

  return (uint8_t)constant;
}

static void emitConstant(Value value) {
  emitBytes(OP_CONSTANT, makeConstant(value));
}

static void endCompiler() {
  emitReturn();
}

/**
 * As far as the backend is concerned, there's nothing to a grouping, so
 * nothing gets emitted - it just allows a lower precedence expression to be
 * returned where a higher one is expected
 */
static void grouping() {
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void number() {
  double value = strtod(parser.previous.start, NULL);
  emitConstant(value);
}

static void unary() {
  TokenType operatorType = parser.previous.type;

  // compile the operand, allowing for nested unary expressions - e.g `!!false`
  parsePrecedence(PREC_UNARY);

  switch (operatorType) {
    case TOKEN_MINUS: {
      // note we emit this _after_ the operand, so that we pop it off the stack
      // first - has minor effect that this line error reporting my not look
      // right:
      // ```
      // print -
      //   true; // would show error on line 2, even though it's on line 1
      // ```
      // alternatively, could store the token's line number before compiling
      // the operand and passing that to emitByte()
      emitByte(OP_NEGATE);
      break;
    }
  }
}

static void parsePrecedence(Precedence precedence) {
  
}

static void expression() {
  parsePrecedence(PREC_ASSIGNMENT);
}

bool compile(const char* source, Chunk* chunk) {
  initScanner(source);
  compilingChunk = chunk;

  parser.hadError = false;
  parser.panicMode = false;

  // 'primes the pump' on the scanner by loading a token into `previous` token
  advance();
  expression();
  consume(TOKEN_EOF, "Expect end of expression.");
  endCompiler();
  return !parser.hadError;
}