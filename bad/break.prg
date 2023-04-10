int x = 42;

if (x < 69) {
    while (true) {
        x++;
        while (x == 43) {
            break;
        }
        break;
    }
    break; // error
}
