#include <stdio.h>
#include <string.h>

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

static bool isDigit(char c) {
  return c >= '0' && c <= '9';
}

static bool isAtEnd() {
  return *scanner.current == '\0';
}

/**
 * Consume the current character and return it
 */
static char advance() {
  scanner.current++;
  return scanner.current[-1];
}

/**
 * Return the current character without consuming it
 */
static char peek() {
  return *scanner.current;
}

/**
 * Look ahead without consuming the next token
 */
static char peekNext() {
  if (isAtEnd()) {
    return '\0';
  }
  return scanner.current[1];
}

/**
 * Check to see if the current character is the expected one.
 * If so, advance.
 * Otherwise, stay right where you are
 */
static bool match(char expected) {
  if (isAtEnd()) {
    return false;
  }
  if (*scanner.current != expected) {
    return false;
  }

  scanner.current++;
  return true;
}

static Token makeToken(TokenType type) {
  Token token;
  token.type = type;
  token.start = scanner.start;
  token.length = (int)(scanner.current - scanner.start);
  token.line = scanner.line;

  return token;
}

static Token errorToken(const char* message) {
  Token token;
  token.type = TOKEN_ERROR;
  token.start = message;
  token.length = (int)strlen(message);
  token.line = scanner.line;

  return token;
}

static void skipWhitespace() {
  for (;;) {
    char c = peek();
    switch (c) {
      case ' ':
      case '\r':
      case '\t':
        advance();
        break;
      case '\n':
        scanner.line++;
        advance();
        break;
      case '/': {
        // we don't want to consume the first '/' if the next character is not
        // another '/'
        if (peekNext() == '/') {
          // a comment goes until the end of the line, but don't consume the
          // newline - that way on the next loop we consume it properly with
          // it's own `case` statement
          while(peek() != '\n' && !isAtEnd()) {
            advance();
          }
        } else {
          return;
        }
        break;
      }
      default:
        return;
    }
  }
}

static Token number() {
  while (isDigit(peek())) {
    advance();
  }

  if (peek() == '.' && isDigit(peekNext())) {
    // Consume the '.'
    advance();

    while (isDigit(peek())) {
      advance();
    }
  }

  return makeToken(TOKEN_NUMBER);
}

static Token string() {
  while (peek() != '"' && !isAtEnd()) {
    if (peek() == '\n') {
      scanner.line++;
    }
    advance();
  }

  if (isAtEnd()) {
    return errorToken("Unterminated string.");
  }

  // The closing quote
  advance();
  return makeToken(TOKEN_STRING);
}

Token scanToken() {
  // We could check this in the switch function, but that gets tricky to
  // ensure the function finds the next token after the whitespace 
  skipWhitespace();

  scanner.start = scanner.current;

  if (isAtEnd()) {
    return makeToken(TOKEN_EOF);
  }

  char c = advance();
  if (isDigit(c)) {
    return number();
  }

  switch (c) {
    case '(': return makeToken(TOKEN_LEFT_PAREN);
    case ')': return makeToken(TOKEN_RIGHT_PAREN);
    case '{': return makeToken(TOKEN_LEFT_BRACE);
    case '}': return makeToken(TOKEN_RIGHT_BRACE);
    case ';': return makeToken(TOKEN_SEMICOLON);
    case ',': return makeToken(TOKEN_COMMA);
    case '.': return makeToken(TOKEN_DOT);
    case '-': return makeToken(TOKEN_MINUS);
    case '+': return makeToken(TOKEN_PLUS);
    case '/': return makeToken(TOKEN_SLASH);
    case '*': return makeToken(TOKEN_STAR);
    case '!': {
      return makeToken(
        match('=') ? TOKEN_BANG_EQUAL : TOKEN_BANG
      );
    }
    case '=': {
      return makeToken(
        match('=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL
      );
    }
    case '<': {
      return makeToken(
        match('=') ? TOKEN_LESS_EQUAL : TOKEN_LESS
      );
    }
    case '>': {
      return makeToken(
        match('=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER
      );
    }
    case '"': return string();
  }

  return errorToken("Unexpected character.");
}