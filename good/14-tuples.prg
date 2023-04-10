[int, string] p = [25, "Latte"], r;
[int, [int, string]] q = [17, p];

println(q); // [17, [25, "Latte"]]
println(r); // [0, ""]

int x, y;
string z;

x, r = q;
y, z = r;

println(r); // [25, "Latte"]
println(x); // 17
println(y); // 25
println(z); // "Latte"
