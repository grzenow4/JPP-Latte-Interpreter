# JPP-Latte-Interpreter

### Language description
Slightly modified version of [Latte](https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2022/Latte/) language. Detailed description below.

### Types
Latte supports 3 types of values: `int`, `bool` and `string`. During declaration you can provide an initial value, but it is not necessary:
```c
int x = 42, zero;
bool t = true, f;
string empty_str;
```

### Tuples
Values can be packed into tuples and unpacked. Tuples can be nested as well. The syntax is similar to Python, but square brackets are used to create a tuple:
```c
[int, bool] k = [42, true];
int n;
bool b;
n, b = k;

[string, [int, int]] data = ["point", [4, 2]];
string s;
[int, int] p;
int x, y;
s, p = data;
x, y = p;
```
Tuples cannot be used in any arithmetic operations, but they can be compared:
```c
[int, int] p1 = [4, 2], p2 = [6, 9], p3 = [4, 0];
assert(p1 < p2);
assert(p1 >= p3);
assert(p2 == [6, 9]);
```

### Arithemtic
All arithmetic operations and logical comparisions can be used on expressions:
```c
int x = 42;
int y = x * 2 - 15;
bool b = y == 69 && x + y > 100;
```
There is also syntactic sugar available for some operations:
```c
x = x + 1;   <=> x++;
x = x - 1;   <=> x--;
x = x <*> y; <=> x <*>= y; // for any arithemtic operation <*>
```

### Printing
To print the value of an expression you can use `print` or `println` statements:
```c
int x = 42, y = 69;
print("x + y = ");
println(x + y);
```

### Assertions
The `assert` statement is used to verify the truth of a logical expression:
```
int x = 17, y = 25;
assert(x + y == 42);
```

### Control statements
Latte provides control statements `while`, `if` and `if-else`:
```c
int x = 10, y, z;
while (x > 0) {
    if (x % 2 == 0) {
        y++;
    } else {
        z++;
    }
}
```
You can exit a loop using the `break` or `continue` statements:
```c
int x;
while (true) {
    if (x == 42) {
        break;
    } else {
        x++;
        continue;
    }
    x = 100;
}
```

### Functions
You can define functions in Latte. A function takes a list of arguments and a block of instructions to execute. Arguments can be passed by reference or by value:
```c
int fun(int x, int &y, bool flag) {
    if (flag) {
        y = y + x;
    }
    return x + y;
}
```
Latte also allows ffor recursive function calls. Here is an example program that calculates the factorial of a number:
```c
int factorial(int n) {
    if (n < 1) {
        return 1;
    }
    return n * factorial(n - 1);
}
```

### Nested functions
Functions in Latte are statically scoped and can also be define inside other functions:
```c
int foo(int x) {
    int bar(int y) {
        return x + y;
    }
    println(bar(17));
}
foo(25);
```

### Static typing
Latte is a statically typed language, which means that before any instructions are executed, the [TypeChecker](TypeChecker.hs) is run to catch any typing errors, incorrect number of arguments passed to a function, etc.
```c
string n = 42;     // error

while (42) {       // error
    print("so many errors here");
}

break;             // error

int foo(int &x, bool b, string s) {
    return x;
}

int x;
foo(42, true, ""); // error
foo(x, true);      // error

assert(2 + 2);     // error
print(x > false);  // error

[int, [int, int]] p;
[int, string] q;
int a, b, c;
a, b, c = p;       // error
a, b = q;          // error

bool bar() {
    return 42;     // error
}
```
### How to use
To compile and run the interpreter, use the following commands:
```bash
make
./interpreter program
```
where `program` is either a file or a list of files. If no arguments are passed, the interpreter will read the program from the standard input.
