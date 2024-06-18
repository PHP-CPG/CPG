# Opcodes
The PHP bytecode CPG is based on PHP opcodes. Recent PHP versions are compiled from the PHP source code to an intermediare bytecode used by the PHP virtual machine. Check out [nikic's great blog](https://www.npopov.com) for a better introduction and deep-dive into the topic.

This documentation is a **work in progress**.

## Ressources and sources

- https://www.npopov.com/2017/04/14/PHP-7-Virtual-machine.html
- https://stackoverflow.com/questions/36031490/what-is-ext-nop-and-ext-stmt-php-opcode
- https://www.npopov.com/2022/05/22/The-opcache-optimizer.html
  - includes an explanation of the bitflags used below 
- https://3v4l.org/ also offers opcode dumps
  - If you want to find out when a particular opcodes is generated, Google it with `site:3v4l.org` to find usages.  
- PHP src
  - e.g. list of opcodes of PHP 7.4.28: https://github.com/php/php-src/blob/b3c6c35570283c457cd5ee6d56301a1bc7f2f8e6/Zend/zend_vm_opcodes.h 
- PHP doc
  - documents some opcodes, but tends to be inconclusive

## Dumping the opcodes

```shell
php -d zend_extension=opcache -d opcache.enable_cli=1 -d opcache.opt_debug_level=0x10000 --syntax-check <file.php>
```
Can be used to dump the opcodes. This is handy to get a feeling for PHP opcodes.

## CPG limitations
Since the CPG is built from bytecode it can not implement everything up-to-spec.

### Example:

```php
<?php
$x = $_GET["x"];
if($x > 10) {
    print("a");
}
```

converts to:

```
$_main: ; (lines=7, args=0, vars=1, tmps=4)
    ; (before optimizer)
    ; if.php:1-6
L0 (2):     T1 = FETCH_R (global) string("_GET")
L1 (2):     T2 = FETCH_DIM_R T1 string("x")
L2 (2):     ASSIGN CV0($x) T2
L3 (3):     T4 = IS_SMALLER int(10) CV0($x)
L4 (3):     JMPZ T4 L6
L5 (4):     ECHO string("a")
L6 (6):     RETURN int(1)
```

which is then converted to a cpg. The bytecode instructions are modeled as calls.
The CPG only implements basic functionality from the spec, e.g it doesn't support constructs like `cpg.assignment`.

```scala
joern> cpg.assignment.l 
res0: List[operatorextension.OpNodes.Assignment] = List()
```

But that functionality can be queried via calls:
```scala
joern> cpg.call("ASSIGN").l 
res1: List[Call] = List(
  Call(
    id -> 512409557603043114L,
    argumentIndex -> -1,
    argumentName -> None,
    code -> "ASSIGN CV($x) T2",
    columnNumber -> None,
    dispatchType -> "STATIC_DISPATCH",
    dynamicTypeHintFullName -> ArraySeq(),
    lineNumber -> Some(value = 1),
    methodFullName -> "<empty>",
    name -> "ASSIGN",
    order -> 2,
    signature -> "",
    typeFullName -> "<empty>"
  )
)
```

## Opcodes

This is a WIP list of documented bytecodes.

| opcode                     | argc | coflo | daflo | call | doc                                 | comment                                                                    |
|----------------------------|------|-------|-------|------|-------------------------------------|----------------------------------------------------------------------------|
| NEW                        | 2    | x     | x     | x    | [yes](./NEW.md)                     | init creates obeject                                                       |
| INIT_FCALL                 | 3    | x     | x     | x    | [yes](./INIT_FCALL.md)              | init call function                                                         |
| INIT_METHOD_CALL           | 3    | x     | x     | x    | [yes](./INIT_METHOD_CALL.md)        | init call method                                                           |
| INIT_NS_FCALL_BY_NAME      | 2    | x     | x     | x    | [yes](./INIT_FS_FCALL_BY_NAME.md)   | init call a function referenced by namespace                               |
| INIT_DYNAMIC_CALL          | 2    | x     | x     | x    | [yes](./INIT_DYNAMIC_CALL.md)       | init call a function saved in a variable                                   |
| INIT_FCALL_BY_NAME         | 2    | x     | x     | x    | [todo]                              | init call a function                                                       |
| INIT_STATIC_METHOD_CALL    | 3/4  | x     | x     | x    | [yes](./INIT_STATIC_METHOD_CALL.md) | init call a static method                                                  |
| DO_FCALL                   | 0    | x     | x     | y    | [todo]                              | perform latest call init                                                   |
| DO_ICALL                   | 0    | x     | x     | y    | [todo]                              | perform latest call init                                                   |
| DO_UCALL                   | 0    | x     | x     | y    | [todo]                              | perform latest call init                                                   |
| DO_FCALL_BY_NAME           | 0    | x     | x     | y    | [todo]                              | perform latest call for `INIT_FCALL_BY_NAME`                               |
| SEND_VAL                   | 2    | x     | y     | x    | [todo]                              | send value (const, tmp) to function call                                   |
| SEND_VAR                   | 2    | x     | y     | x    | [todo]                              | send variable (CV, var) to function call                                   |
| SEND_REF                   |      |       |       |      |                                     | send argument by reference                                                 |
| SEND_VAR_EX                | 2    | x     | y     | x    | [todo]                              | variant of SEND_VAR but by-value or by-reference is determined at runtime  |
| SEND_VAL_EX                | 2    | x     | y     | x    | [todo]                              | variant of SEND_VAL, but by-value or by-reference is determined statically |
| SEND_VAR_NO_REF_EX         | 2    | x     | y     | x    | [todo]                              | send pseudo variable, e.g. function call results                           |
| SEND_FUNC_ARG              | 2    | x     | y     | x    | [todo]                              | [todo]                                                                     |
| ISSET_ISEMPTY_DIM_OBJ      | 3    | x     | x     | x    | no                                  | checks if the dimension of obj is set                                      |
| ISSET_ISEMPTY_CV           | 2    | x     | x     | x    | [todo]                              | [todo]                                                                     |
| ISSET_ISEMPTY_PROP_OBJ     | 3    | x     | x     | x    | no                                  | checks if the property of the object is set                                |
| FETCH_OBJ_FUNC_ARG         | 2/3  | x     | y     | x    | [todo]                              | [todo]                                                                     |
| FETCH_STATIC_PROP_R        | 3    | x     | y     | x    | [todo]                              | [todo]                                                                     |
| FETCH_STATIC_PROP_W        | 4    | x     | y     | x    | [todo]                              | [todo]                                                                     |
| FETCH_STATIC_PROP_FUNC_ARG | 3    | x     | y     | x    | [todo]                              | [todo]                                                                     |
| FETCH_OBJ_R                | 2    | x     | y     | x    | [todo]                              | [todo]                                                                     |
| FETCH_OBJ_W                | 3    | x     | y     | x    | [todo]                              | [todo]                                                                     |
| FETCH_OBJ_IS               | 2    | x     | y     | x    | [todo]                              | [todo]                                                                     |
| FETCH_DIM_R                | 2    | x     | y     | x    | [todo]                              | get value in array x1 at index x2                                          |
| FETCH_DIM_FUNC_ARG         | 2    | x     | y     | x    | [todo]                              | [todo]                                                                     |
| FETCH_LIST_R               | 2    | x     | y     | x    | [todo]                              | [todo]                                                                     |
| FETCH_CLASS_CONSTANT       | 2/3  | x     | y     | x    | [todo]                              | [todo]                                                                     |
| FETCH_CLASS                | 2    | x     | y     | x    | [todo]                              | [todo]                                                                     |
| FETCH_CONSTANT             | 3    | x     | y     | x    | [todo]                              | [todo]                                                                     |
| FETCH_THIS                 | 0    | x     | y     | x    | [todo]                              | [todo]                                                                     |
| INIT_ARRAY                 | 3/4  | x     | x     | x    | no                                  | inits a new array                                                          |
| ADD_ARRAY_ELEMENT          | 2    | x     | y     | x    | [todo]                              | [todo]                                                                     |
| CHECK_FUNC_ARG             | 1    | x     | x     | x    | [todo]                              | [todo]                                                                     |
| RECV                       | 1    | x     | y     | x    | [todo]                              | callsite opcode to receive arguments                                       |
| RECV_INIT                  | 2    | x     | y     | x    | [todo]                              | [todo]                                                                     |
| JMPNZ                      | 2    | y     | x     | x    | [todo]                              | jump to on non zero                                                        |
| JMPNZ_EX                   | 2    | y     | x     | x    | [todo]                              | [todo]                                                                     |
| JMPZ_EX                    | 2    | y     | x     | x    | [todo]                              | [todo]                                                                     |
| JMPZ                       | 2    | y     | x     | x    | [todo]                              | jump to on zero                                                            |
| JMP                        | 1    | y     | x     | x    | [todo]                              | jump to                                                                    |
| JMPZNZ                     | 2    | y     | x     | x    | [todo]                              | [todo]                                                                     |
| FE_FETCH_R                 | 3    | n     | y     | x    | [todo]                              | [todo]                                                                     |
| FE_RESET_R                 | 2    | x     | y     | x    | [todo]                              | [todo]                                                                     |
| ASSIGN_DIM                 | 2    | x     | y     | x    | [todo]                              | [todo]                                                                     |
| ASSIGN                     | 2    | x     | y     | x    | [todo]                              | [todo]                                                                     |
| ASSIGN_OP                  | 3    | x     | y     | x    | [todo]                              | [todo]                                                                     |
| ASSIGN_STATIC_PROP         | 1    | x     | y     | x    | [todo]                              | [todo]                                                                     |
| ASSIGN_OBJ                 | 2    | x     | y     | x    | [todo]                              | [todo]                                                                     |
| ROPE_INIT                  | 2    | x     | x     | x    | [yes](./ROPE.md)                    | init rope string concatination                                             |
| ROPE_ADD                   | 3    | x     | y     | x    | [yes](./ROPE.md)                    | concat string to rope                                                      |
| ROPE_END                   | 3    | x     | y     | x    | [yes](./ROPE.md)                    | concat string to rope and end rope concationation                          |
| CAST                       | 2    | x     | x     | x    | no                                  | casts the second value to the provided type                                |
| NOP                        | 0    | x     | x     | x    | no                                  | does nothing                                                               |
| BEGIN_SILENCE              | 0    | x     | x     | x    | [todo]                              | [todo]                                                                     |
| EXT_STMT                   | 0    | x     | x     | x    | [todo]                              | hook point for debuggers                                                   |
| EXT_NOP                    | 0    | x     | x     | x    | [todo]                              | [todo]                                                                     |
| RETURN                     | 1    | y     | y     | x    | no                                  | returns the given value                                                    |
| ECHO                       | 1    | x     | x     | x    | no                                  | echoes the given value                                                     |
| BW_NOT                     | 1    | x     | x     | x    | [todo]                              | bitwise not                                                                |
| BOOL_NOT                   | 1    | x     | x     | x    | no                                  | inverts the given boolean value                                            |
| QM_ASSIGN                  | 1    | x     | y     | x    | [todo]                              | implements ternary operator (`? :`)                                        |
| PRE_INC                    | 1    | x     | y     | x    | no                                  | increments given value                                                     |
| POST_INC                   | 1    | x     | y     | x    | no                                  | increments given value                                                     |
| PRE_DEC                    | 1    | x     | y     | x    | no                                  | decrements given value                                                     |
| POST_DEC                   | 1    | x     | y     | x    | no                                  | decrements given value                                                     |
| FREE                       | 1    | x     | y     | x    | no                                  | frees the given reference                                                  |
| PRINT                      | 1    | x     | x     | x    | no                                  | prints the given value                                                     |
| FE_FREE                    | 1    | x     | y     | x    | [todo]                              | [todo]                                                                     |
| END_SILENCE                | 1    | x     | x     | x    | [todo]                              | [todo]                                                                     |
| BOOL                       | 1    | x     | x     | x    | no                                  | [todo]                                                                     |
| OP_DATA                    | 1    | x     | y     | x    | [todo]                              | carries additional param for previous bytecode                             |
| CATCH                      | 1    | x     | x     | x    | no                                  | catches the given type of exception                                        |
| THROW                      | 1    | y     | y     | x    | no                                  | throws the given error object                                              |
| CONCAT                     | 2    | x     | x     | x    | [todo]                              | [todo]                                                                     |
| FAST_CONCAT                | 2    | x     | x     | x    | [todo]                              | [todo]                                                                     |
| ADD                        | 2    | x     | x     | x    | no                                  | add the two given values                                                   |
| SUB                        | 2    | x     | x     | x    | no                                  | subtract the two given values                                              |
| MUL                        | 2    | x     | x     | x    | no                                  | multiply the two given values                                              |
| DIV                        | 2    | x     | x     | x    | no                                  | divide the two given values                                                |
| MOD                        | 2    | x     | x     | x    | no                                  | modulo the two given values                                                |
| SL                         | 2    | x     | x     | x    | no                                  | shift bits left value2 by value1 count                                     |
| SR                         | 2    | x     | x     | x    | no                                  | shift bits right value2 by value1 count                                    |
| BW_OR                      | 2    | x     | x     | x    | no                                  | bitwise or the two values                                                  |
| BW_AND                     | 2    | x     | x     | x    | no                                  | bitwise and the two given values                                           |
| BW_XOR                     | 2    | x     | x     | x    | no                                  | bitwise xor the two given values                                           |
| BOOL_OR                    | 2    | x     | x     | x    | no                                  | or the two given values                                                    |
| IS_EQUAL                   | 2    | x     | x     | x    | no                                  | check if the two given values are equal                                    |
| IS_NOT_EQUAL               | 2    | x     | x     | x    | no                                  | check if the two given values ar enot equal                                |
| IS_IDENTICAL               | 2    | x     | x     | x    | no                                  | check if the two given values are identical                                |
| IS_NOT_IDENTICAL           | 2    | x     | x     | x    | no                                  | check if the two given values are not identical                            |
| IS_SMALLER                 | 2    | x     | x     | x    | no                                  | check if the first value is smaller                                        |
| IS_SMALLER_OR_EQUAL        | 2    | x     | x     | x    | no                                  | check if the first value is not smaller                                    |
| BIND_GLOBAL                | 2    | x     | y     | x    | [todo]                              | create a global value                                                      |
| DECLARE_CLASS_DELAYED      | 2    | x     | x     | x    | [todo]                              | [todo]                                                                     |
| UNSET_DIM                  | 2    | x     | y     | x    | [todo]                              | unset the dimension value                                                  |
| INSTANCEOF                 | 2    | x     | x     | x    | no                                  | check if value2 is an instance of value1                                   |
| COALESCE                   |      |       |       |      |                                     | implements null coalescing operator (`??`)                                 |
| SPACESHIP                  |      |       |       |      |                                     | implements spaceship operator (`<=>`)                                      |

