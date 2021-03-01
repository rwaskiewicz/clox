#ifndef clox_object_h
#define clox_object_h

#include "chunk.h"
#include "common.h"
#include "value.h"

// gets the obj from the Value struct, then looks at `Obj#type`
#define OBJ_TYPE(value) (AS_OBJ(value)->type)

// Helpers for determining whether or not a Lox object is of a particular type
#define IS_FUNCTION(value) isObjType(vale, OBJ_FUNCTION)
#define IS_STRING(value)   isObjType(value, OBJ_STRING)

// gets the object as an ObjFunction
#define AS_FUNCTION(value) ((ObjFunction*)AS_OBJ(value))
// gets the obj from the Value struct
#define AS_STRING(value)   ((ObjString*)AS_OBJ(value))
// gets the obj from the Value struct, then looks at `Obj#chars`
#define AS_CSTRING(value)  (((ObjString*)AS_OBJ(value))->chars)

/*
 * Describes complex object types
 */
typedef enum {
  OBJ_FUNCTION,
  OBJ_STRING,
} ObjType;

struct Obj {
  ObjType type;
  struct Obj* next;
};

/*
 * A lox object representing a function
 */
typedef struct {
  // header associated with all Lox objects
  Obj obj;
  // the number of parameters that the function expects
  int arity;
  // chunk associated with the function
  Chunk chunk;
  // the name of the function
  ObjString* name;
} ObjFunction;

struct ObjString {
  Obj obj;
  // allows us to know how much memory we've allocated without having to walk
  // the entire string to find the null terminator
  int length;
  char* chars;
  // since strings are immutable, we can calculate this upfront & eagerly cache
  uint32_t hash;
};

ObjFunction* newFunction();
ObjString* takeString(char* chars, int length);
ObjString* copyString(const char* chars, int length);
void printObject(Value value);

// body uses `value` 2x - the argument expression is evaluated for each time
// its used, which isn't ideal if the arg expr has side effects. As a result,
// we don't inline this.
static inline bool isObjType(Value value, ObjType type) {
  return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif