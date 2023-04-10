int max(int x, int y) {
    if (x > y) {
        return x;
    }
    return y;
}

bool magic(int x) {
    if (x == 42) {
        return true;
   }
   return false;
}

string print_num(int x) {
    if (x > 100) {
        return "It's huge!";
    }
    if (x > 10) {
        return "It's big!";
    }
    if (x == 0) {
        return "It's zero!";
    }
    return "It's small :c";
}

[int, int] add_points([int, int] p, [int, int] q) {
    int x, y, z, t;
    x, y = p;
    z, t = q;
    return [x + z, y + t];
}

int x = 17, y = 25;
[int, int] p = [17, 22], q = [25, 47];

assert(max(x, y) == 25);
assert(magic(x + y));
assert(add_points(p, q) == [42, 69]);

println(print_num(x * 10)); // "It's huge!"
println(print_num(x - y));  // "It's small :c"
println(print_num(0));      // "It's zero!"
