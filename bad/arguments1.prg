int foo(int x, bool y) {
    if (y) {
        return x + 1;
    } else {
        return 42;
    }
}

foo(42, true, false); // error
