# clox

My implmentation of Crafting Interpreter's lox language in C (Pt 3 of the book)

## Testing

To run tests against the test suite defined in the CI repo:

- `undef` debugging symbols in `common.h`
- compile your version
- pull down the latest CI repo changes
- use the names in `[test.dart]`(https://github.com/munificent/craftinginterpreters/blob/master/tool/bin/test.dart#L747)
- invoke from the CI repo
```
dart tool/bin/test.dart chap23_jumping --interpreter ../clox-2/clox
```