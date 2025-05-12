# GMC (Game Machine Core) Language Documentation

GMC is a concise and expressive programming language designed for scripting on
the Game Machine platform. It features a C-like syntax with
influences from Rust and Python, blending clarity, safety, and power.

---

## 🧠 Syntax Overview

### Entry Example

```gmc
func execute(str line) bool {
    if (line == "help") {
        println("This is a GMC program, type 'exit' to exit.");
    } else if (line == "clear" || line == "cls") {
        clear();
    } else if (line == "version") {
        println("GMC version 1.0.0");
    } else if (line == "69") {
        println("FUCK YOU");
    } else {
        println("Unknown command:", line);
        return false;
    }
    return true;
}

println("Welcome to the GMC program, type 'help' for more information!");

while (true) {
    const str line = input("> ");
    if (line == "exit") {
        break;
    }
    const bool success = execute(line);
    if (!success) {
        println("Error executing command:", line);
    }
}

println("Goodbye!");
```

---

## 🧾 Types

### Primitives

* `u8`, `u16`, `u32`, `u64`: Unsigned integers
* `i8`, `i16`, `i32`, `i64`: Signed integers
* `int`: Alias for `i32`
* `float`: Floating-point number (f64)
* `bool`: Boolean
* `char`: Equivalent to `u8`, interpreted as a character

### Composite Types

* `str`: String
* `list<T>`: A generic list type.
  * If you don't supply a type aka `list`, it defaults to `list<any>`.
  * `any` allows heterogeneous types in a single list.
  * **Fields & Methods:**
    * `length`: The number of elements in the list.
    * `append(value)`: Adds a value to the end of the list.
    * `pop()`: Removes and returns the last element from the list.
    * `clear()`: Clears the list.
* `imlist<T>`: A generic immutable list type.
  * See `list<T>` for more information.

### Functions

`functions do not have a type yet but you can still define them list so`

```gmc
func add(int a, int b) int {
    return a + b;
}
```

## 🪙 Variables

```gmc
var int x = 32;
conat int x = 32;
```

variables can be defined with `var` or `const` and can be redefined later only
if they are `var`.

variables that are `const` cannot be redefined
but there inner value can be changed if not immutable.

### Defining a variable

In order to define a variable, you can use the `var` or `const` keyword followed by the type and the name of the variable with an optional initial value.

```gmc
var type name;
var type name = value;
```

### Variable Scoping

Variables are scoped to the block they are defined in.

```gmc
{
    var int x = 32;
    println(x);
}
println(x); # Error: x is not defined
```

### Field Access

Variables can be defined in fields (only in list types for now but will be structs).
In order to access a field, you can use the `.` operator followed by the name of the field.
Functions can also be defined and used the same way.

```gmc
var list<int> x = [1, 2, 3];
println(x.length); # 3
x.add(4);
println(x); # [1, 2, 3, 4]
```

### Indexed Access

Variables can also be indexed.
In order to access an index, you can use the `[]` operator followed by the index.
Indexes start at 0 and are i64.
Indexes can be negative to access the last element.

```gmc
var list<int> x = [1, 2, 3];
println(x[0]); # 1
println(x[1]); # 2
println(x[2]); # 3
println(x[-1]); # 3
println(x[-2]); # 2
println(x[-3]); # 1
```

---

## 🧩 Expressions & Blocks

### Block Expressions

GMC blocks are Rust-style. The value of the last expression in the block becomes
the block's value.

```gmc
var int x = {
    10;
    40;
}; # x == 40
```

### Control Flow as Expressions

`if`, `for`, and `while` can be used as expressions.

```gmc
var int x = if (a > b) a else b;
```

`break`, `continue`, and `return` can only be usesd as statements.
both `break` and `return` can take in an optional expression.

```gmc
const int x = for (var int i = 0; i < 10; i += 1;) {
    if (check(i)) {
        break 10;
    }
} else 0
```

---

## 🔁 Loops

### While Loop

```gmc
while (condition) {
    # body
}
```

### For Loop

C-style with an extra semicolon:

```gmc
for (init; cond; step;) {
    # body
}
```

### For with Else

```gmc
for (var int i = 0; i < 10; i += 1;) {
    if (check(i)) {
        break;
    }
} else {
    println("No match found");
}
```

---

## ✅ Conditionals

```gmc
if (cond) {
    # then
} else if (other) {
    # else if
} else {
    # else
}
```

---

## 🗨️ Comments

Python-style comments are used:

```gmc
# This is a comment
```

## ❌ Errors

Errors can happen at any time (Levi's fault).

Errors can not be handled at the moment.

Errors can be thrown with the `throw` functions.

### Lexical Errors

Can happen at the lexer stage.
Example: Invalid Token.

### Parse Errors

Can happen at the parser stage.
Example: Missing semicolon.

### Runtime Errors

Can happen at the interpreter stage.
Example: Division by zero;

---

## 📚 Built-in Functions

GMC includes a small set of powerful built-in functions for interacting with the user, managing runtime behavior, and evaluating code dynamically. These functions are always available globally and do not require import.

### `print(...)`

* **Usage**: `print("Hello", 123, some_var)`
* **Description**: Prints all arguments to standard output on the same line, separated by spaces.
* **Returns**: `none`
* **Note**: No newline is printed at the end.

---

### `println(...)`

* **Usage**: `println("Hello", "world!")`
* **Description**: Similar to `print`, but adds a newline at the end.
* **Returns**: `none`

---

### `input(str prompt) str`

* **Usage**: `const str name = input("Enter name: ")`
* **Description**: Displays a prompt and waits for user input from standard input. Returns the entered line as a string.
* **Returns**: `str`

---

### `throw(str name, str msg)`

* **Usage**: `throw("MyError", "Something went wrong!")`
* **Description**: Raises a runtime error with a custom name and message. Execution is interrupted unless handled internally.
* **Returns**: `error`

---

### `exit(u8 code = 0)`

* **Usage**: `exit(1)`
* **Description**: Terminates the program immediately. If a code is given, it will be used as the exit code (0–255).
* **Returns**: Does not return

---

### `len(lengtable value) int`

* **Usage**: `const int size = len(my_list)`
* **Description**: Returns the number of elements in a list or characters in a string.
* **Returns**: `int`
* **Errors**: Fails if the argument is not a sizeable type.

---

### `eval(str code) any`

* **Usage**: `eval("1 + 2 * 3")`
* **Description**: Dynamically parses and evaluates a GMC expression or statement.
* **Returns**: The result of the evaluated code.
* **Errors**:

  * Lexing or parsing errors with detailed messages.
  * Runtime errors if evaluation fails.

---

These functions form the backbone of user interaction and error handling in GMC.
They integrate tightly with the interpreter and type system, including runtime checks and memory safety.

---

## 📤 Casting Functions

Casting functions allow you to explicitly convert a value from one type to another.
Unlike `toX` functions, `asX` functions do not parse strings
and only attempt safe conversion between existing runtime values.

### 🔀 `asInt(value)`

Casts a value to an `int`.

* ✅ Works with: `int`, `float`, `bool`, `char`
* ❌ Fails on: `string`, `list`, unsupported types
* Example:

  ```lang
  asInt(3.5)  // => 3
  ```

---

### 🔀 `asFloat(value)`

Casts a value to a `float`.

* ✅ Works with: `float`, `int`, `bool`, `char`
* ❌ Fails on: `string`, `list`, unsupported types
* Example:

  ```lang
  asFloat(3)  // => 3.0
  ```

---

### 🔀 `asChar(value)`

Casts a value to a `char` (u8).

* ✅ Works with: small `int` values
* ❌ Fails on: out-of-range `int`, `string`, `list`, `bool`, etc.
* Example:

  ```lang
  asChar(65)  // => 'A'
  ```

---

### 🔀 `asString(value)`

Casts a value to a string.

* ✅ Works with: most types (via internal safe conversion)
* ❌ May fail on complex or unsupported types
* Example:

  ```lang
  asString(42)  // => "42"
  ```

---

### 🔀 `asBool(value)`

Casts a value to a boolean.

* ✅ Works with: `int`, `float`, `char`
* ❌ Fails on: `string`, `list`, etc.
* Example:

  ```lang
  asBool(0)  // => false
  asBool(1)  // => true
  ```

---

### 🔀 `asList(value)`

Casts a value to a list.

* ✅ Works with: values that are already lists or can be wrapped safely
* ❌ Fails on unsupported types
* Example:

  ```lang
  asList("list")  // => ["l", "i", "s", "t"]
  ```

---

## 🔡 Conversion Functions

Conversion functions operate specifically on **strings** and attempt to **parse** them into other types.

### 🔁 `toInt(str)`

Parses a string into an `int`.

* ✅ Works with: `string`
* ❌ Fails on: non-numeric strings
* Example:

  ```lang
  toInt("42")  // => 42
  ```

---

### 🔁 `toFloat(str)`

Parses a string into a `float`.

* ✅ Works with: `string`
* ❌ Fails on: non-numeric strings
* Example:

  ```lang
  toFloat("3.14")  // => 3.14
  ```

---

### 🔁 `toString(value)`

Converts a value to a string representation.

* ✅ Works with: all types
* ❌ Never fails
* Example:

  ```lang
  toString(5)  // => "5"
  ```

---

## 🔮 Macros

Macros are a powerful way to extend GMC's syntax and functionality.
They can be used to define custom syntax, functions, or other code blocks.

Macros are defined using the `$` character followed by the command (e.g., `define` or `include`).

Macros can be expanded by using the `@` character followed by the macro name.
When defining a macro the `;;` syntax is used to define the end of the macro.

```gmc
$include "include.gmc"
$define call sayHello("Levi"); println("Hello World") ;;

@call;
```

## 🔮 Future Plans

* User-defined types (structs, enums)
* Modules and imports
* Pattern matching
* Compiler optimizations

---

## 🛠️ Tools

* REPL support
* File compilation (WIP)

GMC aims to be compact, expressive, and efficient for scripting Game Machine logic.
