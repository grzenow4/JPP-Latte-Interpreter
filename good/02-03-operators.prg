int x = 17, y = 25;
int z = x + y;
bool a = x <= y, b = z != x + y;

println(z);      // 42
println(z % x);  // 8
println(a || b); // true

x += y;
z *= 10;

assert(x == 42);
assert(z == 420);

[int, int] p1 = [4, 2], p2 = [6, 9], p3 = [4, 0];

assert(p1 < p2);
assert(p1 >= p3);
assert(p2 == [6, 9]);
