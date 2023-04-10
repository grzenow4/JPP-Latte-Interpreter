int foo(int &x, int y) {
    x = y;
}

foo(42, 10); // error
