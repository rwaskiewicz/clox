#ifndef clox_scanner_h
#define clox_scanner_h

typedef enum {
  // Single character tokens
  TOKEN_LEFT_PAREN, TOKEN_RIGHT_PAREN,
  TOKEN_LEFT_BRACE, TOKEN_RIGHT_BRACE,
  TOKEN_COMMA, TOKEN_DOT, TOKEN_MINUS, TOKEN_PLUS,
  TOKEN_SEMICOLON, TOKEN_SLASH, TOKEN_STAR,

  // One or two character tokens
  TOKEN_BANG, TOKEN_BANG_EQUAL,
  TOKEN_EQUAL, TOKEN_EQUAL_EQUAL,
  TOKEN_GREATER, TOKEN_GREATER_EQUAL,
  TOKEN_LESS, TOKEN_LESS_EQUAL,

  // Literals
  TOKEN_IDENTIFIER, TOKEN_STRING, TOKEN_NUMBER,

  // Keywords
  TOKEN_AND, TOKEN_CLASS, TOKEN_ELSE, TOKEN_FALSE,
  TOKEN_FOR, TOKEN_FUN, TOKEN_IF, TOKEN_NIL, TOKEN_OR,
  TOKEN_PRINT, TOKEN_RETURN, TOKEN_SUPER, TOKEN_THIS,
  TOKEN_TRUE, TOKEN_VAR,

  /**
   * There are a couple of errors get detected during scanning - unterminated
   * strings and unrecognized characters. clox produces a sythnetic 'error'
   * token for that error and passes it over to the compiler. That way the
   * compiler knows an error occurred and can perform error recovery
   */
  TOKEN_ERROR,
  TOKEN_EOF
} TokenType;

typedef struct {
  TokenType type;
  const char* start;
  int length;
  int line;
} Token;

void initScanner(const char* source);
Token scanToken();

#endif