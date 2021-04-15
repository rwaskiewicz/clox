#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "memory.h"
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

typedef void (*ParseFn)(bool canAssign);

typedef struct {
  ParseFn prefix;
  ParseFn infix;
  Precedence precedence;
} ParseRule;

typedef struct {
  // the name of the variable
  Token name;
  // the scope depth of the block where the local is declared
  int depth;
  // true if the local is captured by any later nested function declaration
  bool isCaptured;
} Local;

/*
 * Struct representing an Upcalue
 */
typedef struct {
  uint8_t index;
  bool isLocal;
} Upvalue;

/*
 * Enum to designate whether we're compiling top-level code vs a function body
 */
typedef enum {
  TYPE_FUNCTION,
  TYPE_INITIALIZER,
  TYPE_METHOD,
  TYPE_SCRIPT,
} FunctionType;

// need to name the struct since we can't reference the typedef in the struct
typedef struct Compiler {
  // Pointer back to the previous compiler, all the way back to the global one
  struct Compiler* enclosing;
  // reference to the function object being built
  ObjFunction* function;
  // the type of the function object being built
  FunctionType type;

  // flat array of all locals in scope, ordered in the array in the order they appear in the code
  // hard limit on the number of locals tied to the the fact that instruction operand is one byte
  Local locals[UINT8_COUNT];
  // how many locals are in scope - i.e. how many slots in `locals` are in used at a time
  int localCount;
  // array containing all upvalues
  Upvalue upvalues[UINT8_COUNT];
  // the number of blocks deep we are at a given time. zero is the global scope.
  int scopeDepth;
} Compiler;

typedef struct ClassCompiler {
  struct ClassCompiler* enclosing;
  Token name;
} ClassCompiler;

Parser parser;

Compiler* current = NULL;

ClassCompiler* currentClass = NULL;

/**
 * Return the chunk we're writing
 */
static Chunk* currentChunk() {
  return &current->function->chunk;
}

static void errorAt(Token* token, const char* message) {
  if (parser.panicMode) {
    return;
  }

  parser.panicMode = true;

  fprintf(stderr, "[line %d] Error", token->line);

  if (token->type == TOKEN_EOF) {
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

static void emitLoop(int loopStart) {
  emitByte(OP_LOOP);

  // + 2 for the 16 bit operand (2 bytes)
  int offset = currentChunk()->count - loopStart + 2;
  if (offset > UINT16_MAX) {
    error("Loop body too large.");
  }

  emitByte((offset >> 8) & 0xff);
  emitByte(offset & 0xff);
}

/**
 * emit the initial instruction for a jump with placeholder offsets, returning
 * the offset of the emitted instruction
 */
static int emitJump(uint8_t instruction) {
  emitByte(instruction);
  // 16 bit offset gives us 65K bytes of code to jump over
  emitByte(0xff);
  emitByte(0xff);
  return currentChunk()->count - 2;
}

static void emitReturn() {
  if(current->type == TYPE_INITIALIZER) {
    // load slot zero, which has the instance of a class
    emitBytes(OP_GET_LOCAL, 0);
  } else {
    // implicit nil
    emitByte(OP_NIL);
  }
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

static void patchJump(int offset) {
  // -2 to adjust for the bytecode for the jump offset itself
  int jump = currentChunk()->count - offset - 2;

  if (jump > UINT16_MAX) {
    error("Too much code to jump over.");
  }

  currentChunk()->code[offset] = (jump >> 8) & 0xff;
  currentChunk()->code[offset + 1] = jump & 0xff;
}

static void initCompiler(Compiler* compiler, FunctionType type) {
  compiler->enclosing = current;
  compiler->function = NULL;
  compiler->type = type;
  compiler->localCount = 0;
  compiler->scopeDepth = 0;
  // this is 'gc related paranoia` per 24.2.1 (we set this to NULL a few lines before)
  compiler->function = newFunction();
  current = compiler;

  if (type != TYPE_SCRIPT) {
    // we just parsed the functions name, so let's grab it
    current->function->name = copyString(parser.previous.start,
                                         parser.previous.length);
  }

  /*
   * The compiler's locals array keeps track of slots used for temporaries
   * (r-values), and local variables. Implicitly allocate slot 0 for the VM's
   * own use
   */
  Local* local = &current->locals[current->localCount++];
  local->depth = 0;
  local->isCaptured = false;
  if (type != TYPE_FUNCTION) {
    // for methods, repurpose this slot to store the receiver
    local->name.start = "this";
    local->name.length = 4;
  } else {
    // give it an empty name so it can't be overwritten
    local->name.start = "";
    local->name.length = 0;
  }
}

static ObjFunction* endCompiler() {
  emitReturn();
  ObjFunction* function = current->function;
#ifdef DEBUG_PRINT_CODE
  if (!parser.hadError) {
    disassembleChunk(currentChunk(), function->name != NULL ? function->name->chars : "<script>");
  }
#endif

  // this means we're limited in nature to the number of how nested functions
  // can be. Go too far and you cna overflow the stack.
  current = current->enclosing;
  return function;
}

static void beginScope() {
  current->scopeDepth++;
}

static void endScope() {
  current->scopeDepth--;

  // remove locals that occur in a scope that we just left
  while (current->localCount > 0 &&
         current->locals[current->localCount - 1].depth > current->scopeDepth) {
           if (current->locals[current->localCount - 1].isCaptured) {
             // remove the variable from the stack, and hoist it to the heap
             // the variable is right at the top of the stack, so no arg needed
             emitByte(OP_CLOSE_UPVALUE);
           } else {
             // remove the variable from the stack
             emitByte(OP_POP);
           }
           // decrement our pointer
           current->localCount--;
         }
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
 * Determine if two identifiers are the same
 */
static bool identifiersEqual(Token* a, Token* b) {
  if (a->length != b->length) {
    return false;
  }
  // tokens aren't Lox strings (yet), so we don't have their hashes and need to memcmp
  return memcmp(a->start, b->start, a->length) == 0;
}

/**
 * Attempt to resolve a variable as a local one
 */
static int resolveLocal(Compiler* compiler, Token* name) {
  for (int i = compiler->localCount - 1; i >= 0; i--) {
    Local* local = &compiler->locals[i];
    if (identifiersEqual(name, &local->name)) {
      if (local->depth == -1) {
        error("Can't read local variable in its own initializer.");
      }
      return i;
    }
  }
  return -1;
}

/*
 * Create an upvalue
 */
static int addUpvalue(Compiler* compiler, uint8_t index, bool isLocal){
  int upvalueCount = compiler->function->upvalueCount;

  for (int i = 0; i < upvalueCount; i++) {
    Upvalue* upvalue = &compiler->upvalues[i];
    if (upvalue->index == index && upvalue->isLocal == isLocal) {
      return i;
    }
  }

  if (upvalueCount == UINT8_COUNT) {
    error("Too many closure variables in function.");
    return 0;
  }

  compiler->upvalues[upvalueCount].isLocal = isLocal;
  compiler->upvalues[upvalueCount].index = index;
  return compiler->function->upvalueCount++;
}

/*
 * Attempt to resolve a variable declared in surrounding functions
 */
static int resolveUpvalue(Compiler* compiler, Token* name) {
    if (compiler->enclosing == NULL) {
      return -1;
    }

    // base case - we look for a matching local variable in the enclosing fn
    int local = resolveLocal(compiler->enclosing, name);
    if (local != -1) {
      compiler->enclosing->locals[local].isCaptured = true;
      // return the _upvalue_ index
      return addUpvalue(compiler, (uint8_t)local, true);
    }

    // look up the variable via upvalue on the enclosing compiler
    int upvalue = resolveUpvalue(compiler->enclosing, name);
    if (upvalue != -1) {
      // call this with 'isLocal' = false to denote this isn't a local variable
      return addUpvalue(compiler, (uint8_t)upvalue, false);
    }

    return -1;
}

/**
 * Store a local variable
 */
static void addLocal(Token name) {
  // because the index to the Locals array in the compiler is indexed by a
  // single byte, we have a limitation of 256 local variables per scope
  if (current->localCount == UINT8_COUNT) {
    error("Too many local variables in function.");
    return;
  }

  Local* local = &current->locals[current->localCount++];
  local->name = name;
  // use -1 as a sentinel value for marking the var as 'unintialized' to handle
  // cases like var a = a;
  local->depth = -1;
  // all locals start off not captured
  local->isCaptured = false;
}

/**
 * For local variables, record their existence
 */
static void declareVariable() {
  if (current->scopeDepth == 0) {
    return;
  }

  Token* name = &parser.previous;
  /**
   *  Check to ensure that two variables don't have the same name in the same scope:
   *  {
   *    var a = 1;
   *    var a = 2; // illegal
   *  }
   *  Note shadowing is allowed: { var a = 1; { var a = 2; } }
   */
  for (int i = current->localCount - 1; i >= 0; i--) {
    Local* local = &current->locals[i];
    if (local->depth != -1 && local->depth < current->scopeDepth) {
      break;
    }

    if (identifiersEqual(name, &local->name)) {
      error("Already variable with this name in this scope.");
    }
  }

  addLocal(*name);
}

/**
 * Parse the variable name - returns the location (index) of the constant in
 * the chunk's constants table
 */
static uint8_t parseVariable(const char* errorMessage) {
  consume(TOKEN_IDENTIFIER, errorMessage);

  declareVariable();
  // at runtime, locals aren't resolved by name, no need to add it to the constant's table
  if (current->scopeDepth > 0) {
    // return a dummy index
    return 0;
  }

  return identifierConstant(&parser.previous);
}

/**
 * Initialize a variable in the scope
 */
static void markInitialized() {
  if (current->scopeDepth == 0) {
    return;
  }
  current->locals[current->localCount-1].depth = current->scopeDepth;
}

/**
 * Emit the bytes related to a variable
 */
static void defineVariable(uint8_t global) {
  // don't emit if we're not in global scope
  if (current->scopeDepth > 0) {
    // Initialize the variable after declaring it
    // - declaring is making it available in the scope
    // - defining it (here) is giving it a value
    markInitialized();
    return;
  }

  emitBytes(OP_DEFINE_GLOBAL, global);
}

static uint8_t argumentList() {
  uint8_t argCount = 0;
  if(!check(TOKEN_RIGHT_PAREN)) {
    do {
      expression();

      // since the arg count fits in a singe byte operand, we have a 255 limit
      if (argCount == 255) {
        error("Can't have more than 255 arguments.");
      }
      argCount++;
    } while (match(TOKEN_COMMA));
  }

  consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
  return argCount;
}

/**
 * the LHS of the expression has been compiled, so its value is on the top of
 * the stack.
 */
static void and_(bool canAssign) {
  // if the RHS is false, well we know the and expression is falsey
  int endJump = emitJump(OP_JUMP_IF_FALSE);

  // discard the LHS, since we'll evaluate the RHS, which will become the LHS
  emitByte(OP_POP);
  parsePrecedence(PREC_AND);

  // if we jumped, the value is still at the top of the stack to be the result
  // of the entire expr (we never popped)
  patchJump(endJump);
}

static void binary(bool canAssign) {
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

/*
 * By the time we call this method, '(' has been consumed
 */
static void call(bool canAssign) {
  uint8_t argCount = argumentList();
  emitBytes(OP_CALL, argCount);
}

/*
 * The dot binds tightly, with precedence as high as the parentheses in a fn
 * call
 */
static void dot(bool canAssign) {
  consume(TOKEN_IDENTIFIER, "Expect property name after '.'.");
  uint8_t name = identifierConstant(&parser.previous);

  if (canAssign && match(TOKEN_EQUAL)) {
    expression();
    emitBytes(OP_SET_PROPERTY, name);
  } else if (match(TOKEN_LEFT_PAREN)) {
    uint8_t argCount = argumentList();
    emitBytes(OP_INVOKE, name);
    emitByte(argCount);
  } else {
    emitBytes(OP_GET_PROPERTY, name);
  }
}

static void literal(bool canAssign) {
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
static void grouping(bool canAssign) {
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void number(bool canAssign) {
  double value = strtod(parser.previous.start, NULL);
  emitConstant(NUMBER_VAL(value));
}

static void or_(bool canAssign) {
  // if the LHS is falsey, do a tiny lil jump to the next statement
  int elseJump = emitJump(OP_JUMP_IF_FALSE);
  int endJump = emitJump(OP_JUMP);

  patchJump(elseJump);
  emitByte(OP_POP);

  parsePrecedence(PREC_OR);
  patchJump(endJump);
}

static void string(bool canAssign) {
  // Note: we need to trim the leading and trailing quotation marks, hence the
  // arithmetic in determining how much of the string to copy
  emitConstant(OBJ_VAL(copyString(parser.previous.start + 1, parser.previous.length - 2)));
}

/**
 * Perform an action for a given variable name
 * - Assign or read a variable
 * - Act on a local or global variable
 */
static void namedVariable(Token name, bool canAssign) {
  uint8_t getOp, setOp;
  int arg = resolveLocal(current, &name);

  if (arg != -1) {
    getOp = OP_GET_LOCAL;
    setOp = OP_SET_LOCAL;
  } else if ((arg = resolveUpvalue(current, &name)) != -1) {
    getOp = OP_GET_UPVALUE;
    setOp = OP_SET_UPVALUE;
  } else {
    arg = identifierConstant(&name);
    getOp = OP_GET_GLOBAL;
    setOp = OP_SET_GLOBAL;
  }

  if (canAssign && match(TOKEN_EQUAL)) {
    expression();
    emitBytes(setOp, (uint8_t)arg);
  } else {
    emitBytes(getOp, (uint8_t)arg);
  }
}

static void variable(bool canAssign) {
  namedVariable(parser.previous, canAssign);
}

/*
 * Treat `this` as a lexical variable that _magically_ gets initialized. That
 * way, things like closures do the 'right thing' for free basically.
 */
static void this_(bool canAssign) {
  if (currentClass == NULL) {
    error("Can't use 'this' outside of a class.");
    return;
  }
  // don't allow assignment to `this`
  variable(false);
}

static void unary(bool canAssign) {
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
  [TOKEN_LEFT_PAREN]    = {grouping, call,   PREC_CALL},
  [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE},
  [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_DOT]           = {NULL,     dot,    PREC_CALL},
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
  [TOKEN_AND]           = {NULL,     and_,   PREC_AND},
  [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
  [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
  [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
  [TOKEN_OR]            = {NULL,     or_,    PREC_OR},
  [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
  [TOKEN_SUPER]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_THIS]          = {this_,    NULL,   PREC_NONE},
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

  bool canAssign = precedence <= PREC_ASSIGNMENT;
  prefixRule(canAssign);

  while (precedence <= getRule(parser.current.type)->precedence) {
    advance();
    ParseFn infixRule = getRule(parser.previous.type)->infix;
    infixRule(canAssign);
  }

  // if '=' is the current token, we should have consumed it somehow...report
  // the error
  if (canAssign && match(TOKEN_EQUAL)) {
    error("Invalid assignment target.");
  }
}

static ParseRule* getRule(TokenType type) {
  return &rules[type];
}

static void expression() {
  parsePrecedence(PREC_ASSIGNMENT);
}

static void block() {
  while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
    declaration();
  }

  consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void function(FunctionType type) {
  Compiler compiler;
  // create a new compiler for each function object
  initCompiler(&compiler, type);
  beginScope();

  // Compile the parameter list
  consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
  if (!check(TOKEN_RIGHT_PAREN)) {
    do {
      current->function->arity++;
      if (current->function->arity > 255) {
        errorAtCurrent("Can't have more than 255 parameters.");
      }

      uint8_t paramConstant = parseVariable("Expect parameter name.");
      defineVariable(paramConstant);
    } while (match(TOKEN_COMMA));
  }
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");

  // The body
  consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");
  block();

  // Create the function object
  ObjFunction* function = endCompiler();
  emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function)));

  /*
   * Each upvalue has 2 single byte operands
   */
  for (int i = 0; i < function->upvalueCount; i++) {
    emitByte(compiler.upvalues[i].isLocal ? 1 : 0);
    // local slot or upvalue index
    emitByte(compiler.upvalues[i].index);
  }
}

static void method() {
  consume(TOKEN_IDENTIFIER, "Expect method name.");
  uint8_t constant = identifierConstant(&parser.previous);

  FunctionType type = TYPE_METHOD;
  if (parser.previous.length == 4 &&
      memcmp(parser.previous.start, "init", 4) == 0) {
    type = TYPE_INITIALIZER;
  }
  function(type);
  emitBytes(OP_METHOD, constant);
}

static void classDeclaration() {
  consume(TOKEN_IDENTIFIER, "Expect class name.");
  Token className = parser.previous;
  uint8_t nameConstant = identifierConstant(&parser.previous);
  // bind the class name to a variable of the same name
  declareVariable();

  emitBytes(OP_CLASS, nameConstant);
  defineVariable(nameConstant);

  ClassCompiler classCompiler;
  classCompiler.name = parser.previous;
  classCompiler.enclosing = currentClass;
  currentClass = &classCompiler;

  // detect a superclass clause
  if (match(TOKEN_LESS)) {
    consume(TOKEN_IDENTIFIER, "Expect superclass name.");
    // take the identifer token and treat it as a variable reference, emitting
    // code to load it's value (so look up the class and push onto the stack)
    variable(false);

    if (identifiersEqual(&className, &parser.previous)) {
      error("A class can't inherit from itself.");
    }

    namedVariable(className, false);
    emitByte(OP_INHERIT);
  }

  // generate code to load the class name one the stack before compiling methods
  namedVariable(className, false);

  consume(TOKEN_LEFT_BRACE, "Expect '{' before class body.");
  // lox doesn't have field declarations, so anything before the closing brace
  // must be a method
  while(!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
    method();
  }
  consume(TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
  // pop the class off stack
  emitByte(OP_POP);

  currentClass = currentClass->enclosing;
}

/*
 * Function declarations differ from variables in that it is safe for a
 * function to refer to its own name in its own body, since you can't call it
 * until after its defined - and we need to allow that for a little thing
 * called recursion
 */
static void funDeclaration() {
  // note: a fn declaration at the top level will bind the fn to a global var
  // otherwise, we have a regular ol' local var
  uint8_t global = parseVariable("Expect function name.");
  markInitialized();
  function(TYPE_FUNCTION);
  defineVariable(global);
}

static void varDeclaration() {
  // get the variable name, returning the index to the identifier in chunk's constants table
  uint8_t global = parseVariable("Expect variable name.");

  if (match(TOKEN_EQUAL)) {
    // 22.3 has a really good image for this and how efficient temporaries are at becoming a local variable
    expression();
  } else {
    // implicitly initialize the variable to nil if it wasn't set by the user
    emitByte(OP_NIL);
  }
  consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration");

  // emit that we have a global variable at some index in the table
  defineVariable(global);
}

static void expressionStatement() {
  expression();
  consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
  emitByte(OP_POP);
}

static void forStatement() {
  // in the event we declare a variable in the initializer...
  beginScope();

  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
  if (match(TOKEN_SEMICOLON)) {
    // No initializer. This is fine, move along...
  } else if (match(TOKEN_VAR)) {
    varDeclaration();
  } else {
    // has a happy little side effect of finding a semicolon and performing a
    // POP so we don't have the initialzer leaving anything on the stack
    expressionStatement();
  }

  int loopStart = currentChunk()->count;

  int exitJump = -1;
  if (!match(TOKEN_SEMICOLON)) {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");

    // jump out of the loop if the condition is false
    exitJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP); // condition needs to be popped
  }

  if (!match(TOKEN_RIGHT_PAREN)) {
    // jump to go _to_ the body, over the increment clause
    int bodyJump = emitJump(OP_JUMP);

    int incrementStart = currentChunk()->count;
    expression();
    emitByte(OP_POP);
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after 'for' clauses.");

    // right before the condition expression
    // this happens right after an increment, since an increment happens at the
    // end of a loop (a little weird, I know)
    emitLoop(loopStart);
    loopStart = incrementStart;
    patchJump(bodyJump);
  }

  statement();

  emitLoop(loopStart);

  if (exitJump != -1) {
    patchJump(exitJump);
    emitByte(OP_POP); // condition needs to be popped
  }

  endScope();
}

/**
 * parse an if statement
 */
static void ifStatement() {
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

  int thenJump = emitJump(OP_JUMP_IF_FALSE);
  // when the condition is truthy, pop the condition right before the code
  // inside the 'then'
  emitByte(OP_POP);
  statement();

  // if the condition is true and we run the 'then' body, we can't be running
  // the else code
  int elseJump = emitJump(OP_JUMP);

  // 'backpatching' - we emit the jump instruction first with a placeholder offset
  // then we'll compile the 'then' body to know how far back to jump
  patchJump(thenJump);
  // if the condition is falsy, pop it at the beginning of else
  emitByte(OP_POP);
  // we've compiled the 'then' branch
  if (match(TOKEN_ELSE)) {
    statement();
  }
  patchJump(elseJump);
}

static void printStatement() {
  expression();
  consume(TOKEN_SEMICOLON, "Expect ';' after value.");
  emitByte(OP_PRINT);
}

static void returnStatement() {
  if (current->type == TYPE_SCRIPT) {
    error("Can't return from top-level code.");
  }

  if (match(TOKEN_SEMICOLON)) {
    emitReturn();
  } else {
    if (current->type == TYPE_INITIALIZER) {
      error("Can't return a value from an initializer.");
    }
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
    emitByte(OP_RETURN);
  }
}

static void whileStatement() {
  int loopStart = currentChunk()->count;

  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

  int exitJump = emitJump(OP_JUMP_IF_FALSE);

  emitByte(OP_POP); // popping the condition off the stack
  statement();

  emitLoop(loopStart);

  patchJump(exitJump);
  emitByte(OP_POP); // popping the condition off the stack
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
  if (match(TOKEN_CLASS)) {
    classDeclaration();
  } else if (match(TOKEN_FUN)) {
    funDeclaration();
  } else if (match(TOKEN_VAR)) {
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
  } else if (match(TOKEN_FOR)) {
    forStatement();
  } else if (match(TOKEN_IF)) {
    ifStatement();
  } else if (match(TOKEN_RETURN)) {
    returnStatement();
  } else if (match(TOKEN_WHILE)) {
    whileStatement();
  } else if (match(TOKEN_LEFT_BRACE)) {
    beginScope();
    block();
    endScope();
  } else {
    expressionStatement();
  }
}

ObjFunction* compile(const char* source) {
  initScanner(source);
  Compiler compiler;
  initCompiler(&compiler, TYPE_SCRIPT);

  parser.hadError = false;
  parser.panicMode = false;

  // 'primes the pump' on the scanner by loading a token into `previous` token
  advance();

  while (!match(TOKEN_EOF)) {
    declaration();
  }

  ObjFunction* function = endCompiler();
  return parser.hadError ? NULL : function;
}

void markCompilerRoots() {
  Compiler *compiler = current;
  while (compiler != NULL) {
    markObject((Obj*)compiler->function);
    compiler = compiler->enclosing;
  }
}