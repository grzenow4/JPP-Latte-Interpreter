int s = 1, i = 1, n = 5;

while (i < n) {
    int k = 1, l = 0, p = 1;
    while (p < i) {
        k++;
        if (l < p) {
            l++;
            p = 1;
        } else {
            p++;
        }
    }
    s += 6 * k + p - l;
    i++;
}

assert(s == n * n * n);
println(s); // s = 125
