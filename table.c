#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"

/**
 * this varies based on the hash fn, collision handling strategy, and keysets
 * - semi-arbitrarily picked for Lox
 */
#define TABLE_MAX_LOAD 0.75

void initTable(Table* table) {
  table->count = 0;
  table->capacity = 0;
  table->entries = NULL;
}

void freeTable(Table* table) {
  FREE_ARRAY(Entry, table->entries, table->capacity);
  initTable(table);
}

/**
 * This fn handles figuring out which bucket each Entry belongs in, as well as
 * linear probing. Returns the address of the Entry to set
 */
static Entry* findEntry(Entry* entries, int capacity, ObjString* key) {
  uint32_t index = key->hash & (capacity - 1);
  Entry* tombstone = NULL;

  // we won't loop forever thanks to the load factor
  for (;;) {
    Entry* entry = &entries[index];

    if (entry->key == NULL) {
      if (IS_NIL(entry->value)) {
        // Case: the key isn't in the table (NULL), the bucket is empty OR
        // there is a tombstone whose location we should return instead
        return tombstone != NULL ? tombstone : entry;
      } else {
        // Case: We found a tombstone
        if (tombstone == NULL) {
          tombstone = entry;
        }
      }
    } else if (entry->key == key) {
       // Case: the key is already in the table, we're going to overwrite it
       return entry;
    }

    // Case: We have a collision, start linear probing
    index = (index + 1) & (capacity - 1);
  }
}

bool tableGet(Table* table, ObjString* key, Value* value) {
  if (table->count == 0) {
    return false;
  }

  Entry* entry = findEntry(table->entries, table->capacity, key);
  if (entry->key == NULL) {
    return false;
  }

  // copy the found value to the output parameter
  *value = entry->value;
  return true;
}

static void adjustCapacity(Table* table, int capacity) {
  Entry *entries = ALLOCATE(Entry, capacity);
  for (int i = 0; i < capacity; i++) {
    entries[i].key = NULL;
    entries[i].value = NIL_VAL;
  }

  // reset the count - it won't stay the same as we're not going to copy over
  // tombstones - this is a little wasteful - we grow the array with _fewer_
  // entries in it
  table->count = 0;
  // we can't just call `realloc` here - when we choose a bucket, it's modulo'd
  // the array size, so we need to recalculate the buckets. this can create new
  // collisions, so we'll just rebuild the table from scratch
  for (int i = 0; i < table->capacity; i++) {
    Entry* entry = &table->entries[i];
    if (entry->key == NULL) {
      continue;
    }

    Entry* dest = findEntry(entries, capacity, entry->key);
    dest->key = entry->key;
    dest->value = entry->value;
    table->count++;
  }

  FREE_ARRAY(Entry, table->entries, table->capacity);
  table->entries = entries;
  table->capacity = capacity;
}

bool tableSet(Table* table, ObjString* key, Value value) {
  if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
    int capacity = GROW_CAPACITY(table->capacity);
    adjustCapacity(table, capacity);
  }

  Entry* entry = findEntry(table->entries, table->capacity, key);

  bool isNewKey = entry->key == NULL;
  // the count is incremented for new entries for an empty bucket.
  // note that implies that even when an entry is turned into a tombstone, the
  // count stays the same - it is better here to have 'full' buckets and
  // unnecessarily grow the array rather than treat tombstones like empty
  // buckets and infinite loop while trying to find an entry
  if (isNewKey && IS_NIL(entry->value)) {
    table->count++;
  }

  entry->key = key;
  entry->value = value;
  return isNewKey;
}

bool tableDelete(Table* table, ObjString* key) {
  if (table->count == 0) {
    return false;
  }

  // Find the entry
  Entry* entry = findEntry(table->entries, table->capacity, key);
  if (entry->key == NULL) {
    return false;
  }

  // Pace a tombstone entry
  entry->key = NULL;
  entry->value = BOOL_VAL(true);
}

void tableAddAll(Table* from, Table* to) {
  for (int i = 0; i < from->capacity; i++) {
    Entry* entry = &from->entries[i];
    if (entry->key != NULL) {
      tableSet(to, entry->key, entry-> value);
    }
  }
}

ObjString* tableFindString(Table* table, const char* chars, int length, uint32_t hash) {
  if (table->count == 0) {
    return NULL;
  }

  uint32_t index = hash & (table->capacity - 1);

  for (;;) {
    Entry* entry = &table->entries[index];

    if (entry->key == NULL) {
      // stop if we find an empty, non-tombstone entry
      if (IS_NIL(entry->value)) {
        return NULL;
      }
    } else if (entry->key->length == length &&
        entry->key->hash == hash &&
        memcmp(entry->key->chars, chars, length) == 0) {
      // found it!
      return entry->key;
    }
    index = (index + 1) & (table->capacity - 1);
  }
}

void tableRemoveWhite(Table* table) {
  for (int i = 0; i < table->capacity; i++) {
    Entry* entry = &table->entries[i];
    if (entry->key != NULL && !entry->key->obj.isMarked) {
      tableDelete(table, entry->key);
    }
  }
}

void markTable(Table* table) {
  for (int i = 0; i < table->capacity; i++) {
    Entry* entry = &table->entries[i];
    markObject((Obj*)entry->key);
    markValue(entry->value);
  }
}