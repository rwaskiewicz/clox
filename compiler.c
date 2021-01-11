#include <stdio.h>
#include <stdlib.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

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

typedef void (*ParseFn)();

typedef struct {
  ParseFn prefix;
  ParseFn infix;
  Precedence precedence;
} ParseRule;

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

static bool check(TokenType type) {
  return parser.current.type == type;
}

/**
 * If the current token has the given type, consume it and return true.
 * Otherwise do not consume it, return false, do not pass go
 */
static bool match(TokenType type) {
  if (!check(type)) {
    return false;
  }

  advance();
  return true;
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

/**
 * Helper method for adding a Value to a chunk's constant table. Returns the
 * index if it was placed there. May throw if there are too many constants
 */
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
#ifdef DEBUG_PRINT_CODE
  if (!parser.hadError) {
    disassembleChunk(currentChunk(), "code");
  }
#endif
}

static void expression();
static void statement();
static void declaration();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);

/**
 * A string is too big to put in the bytecode stream as an operand to an opcode
 * so instead we'll put it in the chunk's constant table and store the index to
 * look it up next to the opcode instead
 *
 * This function does the actual string allocation and creation of the
 * ObjString under the hood
 *
 * Returns the location (index) in the chunk's constants table
 */
static uint8_t identifierConstant(Token* name) {
  return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}

/**
 * Parse the variable name - returns the location (index) of the constant in
 * the chunk's constants table
 */
static uint8_t parseVariable(const char* errorMessage) {
  consume(TOKEN_IDENTIFIER, errorMessage);
  return identifierConstant(&parser.previous);
}

static void binary() {
  // Remember the operator. It's already been consumed (and the LHS has been
  // compiled already too)
  TokenType operatorType = parser.previous.type;

  // Compile the right operand
  ParseRule* rule = getRule(operatorType);
  parsePrecedence((Precedence)rule->precedence + 1);

  // Emit the operator instruction. Note the LHS and RHS are on the stack
  // already
  switch (operatorType) {
    case TOKEN_BANG_EQUAL: {
      emitBytes(OP_EQUAL, OP_NOT);
      break;
    }
    case TOKEN_EQUAL_EQUAL: {
      emitByte(OP_EQUAL);
      break;
    }
    case TOKEN_GREATER: {
      emitByte(OP_GREATER);
      break;
    }
    case TOKEN_GREATER_EQUAL: {
      emitBytes(OP_LESS, OP_NOT);
      break;
    }
    case TOKEN_LESS: {
      emitByte(OP_LESS);
      break;
    }
    case TOKEN_LESS_EQUAL: {
      emitBytes(OP_GREATER, OP_NOT);
      break;
    }
    case TOKEN_PLUS: {
      emitByte(OP_ADD);
      break;
    }
    case TOKEN_MINUS: {
      emitByte(OP_SUBTRACT);
      break;
    }
    case TOKEN_STAR: {
      emitByte(OP_MULTIPLY);
      break;
    }
    case TOKEN_SLASH: {
      emitByte(OP_DIVIDE);
      break;
    }
    default:
      return; // unreachable
  }
}

static void literal() {
  switch (parser.previous.type) {
    case TOKEN_FALSE: {
      emitByte(OP_FALSE);
      break;
    }
    case TOKEN_NIL: {
      emitByte(OP_NIL);
      break;
    }
    case TOKEN_TRUE: {
      emitByte(OP_TRUE);
      break;
    }
    default: {
      return; // unreachable
    }
  }
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
  emitConstant(NUMBER_VAL(value));
}

static void string() {
  // Note: we need to trim the leading and trailing quotation marks, hence the
  // arithmetic in determining how much of the string to copy
  emitConstant(OBJ_VAL(copyString(parser.previous.start + 1, parser.previous.length - 2)));
}

static void namedVariable(Token name) {
  uint8_t arg = identifierConstant(&name);
  emitBytes(OP_GET_GLOBAL, arg);
}

static void variable() {
  namedVariable(parser.previous);
}

static void unary() {
  TokenType operatorType = parser.previous.type;

  // compile the operand, allowing for nested unary expressions - e.g `!!false`
  parsePrecedence(PREC_UNARY);

  switch (operatorType) {
    case TOKEN_BANG: {
      emitByte(OP_NOT);
      break;
    }
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

ParseRule rules[] = {
  [TOKEN_LEFT_PAREN]    = {grouping, NULL,   PREC_NONE},
  [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE},
  [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_DOT]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_MINUS]         = {unary,    binary, PREC_TERM},
  [TOKEN_PLUS]          = {NULL,     binary, PREC_TERM},
  [TOKEN_SEMICOLON]     = {NULL,     NULL,   PREC_NONE},
  [TOKEN_SLASH]         = {NULL,     binary, PREC_FACTOR},
  [TOKEN_STAR]          = {NULL,     binary, PREC_FACTOR},
  [TOKEN_BANG]          = {unary,    NULL,   PREC_NONE},
  [TOKEN_BANG_EQUAL]    = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_EQUAL]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_EQUAL_EQUAL]   = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_GREATER]       = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_LESS]          = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_LESS_EQUAL]    = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_IDENTIFIER]    = {variable, NULL,   PREC_NONE},
  [TOKEN_STRING]        = {string,   NULL,   PREC_NONE},
  [TOKEN_NUMBER]        = {number,   NULL,   PREC_NONE},
  [TOKEN_AND]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
  [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
  [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
  [TOKEN_OR]            = {NULL,     NULL,   PREC_NONE},
  [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
  [TOKEN_SUPER]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_THIS]          = {NULL,     NULL,   PREC_NONE},
  [TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE},
  [TOKEN_VAR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_WHILE]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ERROR]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},
};

static void parsePrecedence(Precedence precedence) {
  advance();
  ParseFn prefixRule = getRule(parser.previous.type)->prefix;
  if (prefixRule == NULL) {
    error("Expect expression.");
    return;
  }

  prefixRule();

  while (precedence < getRule(parser.current.type)->precedence) {
    advance();
    ParseFn infixRule = getRule(parser.previous.type)->infix;
    infixRule();
  }
}

/**
 * Emit the bytes related to a variable
 */
static void defineVariable(uint8_t global) {
  emitBytes(OP_DEFINE_GLOBAL, global);
}

static ParseRule* getRule(TokenType type) {
  return &rules[type];
}

static void expression() {
  parsePrecedence(PREC_ASSIGNMENT);
}

static void varDeclaration() {
  // get the variable name
  uint8_t global = parseVariable("Expect variable name.");

  if (match(TOKEN_EQUAL)) {
    expression();
  } else {
    // implicitly initialize the variable to nil if it wasn't set by the user
    emitByte(OP_NIL);
  }
  consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration");

  defineVariable(global);
}

static void expressionStatement() {
  expression();
  consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
  emitByte(OP_POP);
}

static void printStatement() {
  expression();
  consume(TOKEN_SEMICOLON, "Expect ';' after value.");
  emitByte(OP_PRINT);
}

/**
 * When we're entered panic mode while parsing the previous statement, we will
 * want to synchronize - aka stop at a statement boundary
 */
static void synchronize() {
  parser.panicMode = false;

  while (parser.current.type != TOKEN_EOF) {
    if (parser.previous.type == TOKEN_SEMICOLON) {
      return;
    }

    switch (parser.current.type) {
      case TOKEN_CLASS:
      case TOKEN_FUN:
      case TOKEN_VAR:
      case TOKEN_FOR:
      case TOKEN_IF:
      case TOKEN_WHILE:
      case TOKEN_PRINT:
      case TOKEN_RETURN:
        return;
      default: {
        // Do nothing.
        ;
      }

      advance();
    }
  }
}

static void declaration() {
  if (match(TOKEN_VAR)) {
    varDeclaration();
  } else {
    statement();
  }

  if (parser.panicMode) {
    synchronize();
  }
}

static void statement() {
  if (match(TOKEN_PRINT)) {
    printStatement();
  } else {
    expressionStatement();
  }
}

bool compile(const char* source, Chunk* chunk) {
  initScanner(source);
  compilingChunk = chunk;

  parser.hadError = false;
  parser.panicMode = false;

  // 'primes the pump' on the scanner by loading a token into `previous` token
  advance();

  while (!match(TOKEN_EOF)) {
    declaration();
  }

  endCompiler();
  return !parser.hadError;
}