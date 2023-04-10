int foo(bool f, int &x, [string, int] &k) {
    if (f) {
        x = 42;
    } else {
        x = 69;
    }
    k = ["my_number_is_x", x];
}

int x;
[string, int] p;

foo(true, &x, &p);
assert(x == 42);
assert(p == ["my_number_is_x", 42]);

foo(false, &x, &p);
assert(x == 69);
assert(p == ["my_number_is_x", 69]);
