int factorial(int n) {
    if (n < 1) {
        return 1;
    }
    return n * factorial(n - 1);
}

int sum(int n) {
    if (n > 0) {
        return n + sum(n - 1);
    }
    return 0;
}

assert(factorial(5) == 120);
assert(sum(100) == 5050);
