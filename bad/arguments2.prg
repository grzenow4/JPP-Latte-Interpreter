int inc(int &x) {
    x++;
}

string y = "not a number";

inc(&y); // error
