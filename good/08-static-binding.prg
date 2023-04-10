int x = 500;

int glob() {
    x++;
    println(x); // x = 501
}

int loc() {
    int x = 10;
    glob();
    println(x); // x = 10
}

int main() {
    loc();
    println(x); // x = 501
}

main();

/* --------------------------------- */

int foo() {
    int x = 500;

    int glob2() {
        x++;
        println(x); // x = 501
    }

    int loc2() {
        int x = 10;
        glob2();
        println(x); // x = 10
    }

    loc2();
    println(x); // x = 501
}

foo();
