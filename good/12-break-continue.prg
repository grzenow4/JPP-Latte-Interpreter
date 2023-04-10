int x, y;

while (x < 100) {
    x++;
    if (x == 42) {
        break;
    }
}

assert(x == 42);

while (y < 10) {
    y++;
    if (y % 2 == 0) {
        continue;
    }
    print(y); // 1 3 5 7 9
    print(" ");
}

println("");
