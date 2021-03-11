#ifndef clox_object_h
#define clox_object_h

#include "chunk.h"
#include "common.h"
#include "value.h"

// gets the obj from the Value struct, then looks at `Obj#type`
#define OBJ_TYPE(value) (AS_OBJ(value)->type)

// Helpers for determining whether or not a Lox object is of a particular type
#define IS_CLOSURE(value)  isObjType(value, OBJ_CLOSURE)
#define IS_FUNCTION(value) isObjType(value, OBJ_FUNCTION)
#define IS_NATIVE(value)   isObjType(value, OBJ_NATIVE)
#define IS_STRING(value)   isObjType(value, OBJ_STRING)

// casts the obj as an ObjClosure
#define AS_CLOSURE(value) ((ObjClosure*)AS_OBJ(value))
// casts the object as an ObjFunction
#define AS_FUNCTION(value) ((ObjFunction*)AS_OBJ(value))
// casts the object as a native fn
#define AS_NATIVE(value) (((ObjNative*)AS_OBJ(value))->function)
// casts the obj from the Value struct
#define AS_STRING(value)   ((ObjString*)AS_OBJ(value))
// casts the obj from the Value struct, then looks at `Obj#chars`
#define AS_CSTRING(value)  (((ObjString*)AS_OBJ(value))->chars)

/*
 * Describes complex object types
 */
typedef enum {
  OBJ_CLOSURE,
  OBJ_FUNCTION,
  OBJ_NATIVE,
  OBJ_STRING,
  OBJ_UPVALUE
} ObjType;

struct Obj {
  ObjType type;
  bool isMarked;
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
  // the number of upvalues stored for the function
  int upvalueCount;
  // chunk associated with the function
  Chunk chunk;
  // the name of the function
  ObjString* name;
} ObjFunction;

typedef Value (*NativeFn)(int argCount, Value* args);

typedef struct {
  Obj obj;
  NativeFn function;
} ObjNative;

struct ObjString {
  Obj obj;
  // allows us to know how much memory we've allocated without having to walk
  // the entire string to find the null terminator
  int length;
  char* chars;
  // since strings are immutable, we can calculate this upfront & eagerly cache
  uint32_t hash;
};

typedef struct ObjUpvalue {
  Obj obj;
  // pointer to the closed over variable, not a value
  Value *location;
  Value closed;
  // pointer to the next open Upvalue
  struct ObjUpvalue* next;
} ObjUpvalue;

typedef struct {
  Obj obj;
  ObjFunction* function;
  ObjUpvalue** upvalues;
  int upvalueCount;
} ObjClosure;

ObjClosure* newClosure(ObjFunction* function);
ObjFunction* newFunction();
ObjNative* newNative();
ObjString* takeString(char* chars, int length);
ObjString* copyString(const char* chars, int length);
ObjUpvalue* newUpvalue(Value* slot);
void printObject(Value value);

// body uses `value` 2x - the argument expression is evaluated for each time
// its used, which isn't ideal if the arg expr has side effects. As a result,
// we don't inline this.
static inline bool isObjType(Value value, ObjType type) {
  return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif