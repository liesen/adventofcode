#include <algorithm>
#include <iostream>
#include <string>
#include <vector>

int step(int step, int curr, int min, int max, std::vector<int> &next, bool debug = false) {
    if (debug) std::cerr << "-- move " << step << " --" << std::endl;
    int a = next[curr];
    int b = next[a];
    int c = next[b];

    int dest = curr - 1;

    while (dest == a || dest == b || dest == c) {
        dest--;
    }

    if (dest < min) {
        dest = max;

        while (dest == a || dest == b || dest == c) {
            dest--;
        }
    }

    if (debug) std::cerr << "pick up: " << a << ", " << b << ", " << c << std::endl;
    if (debug) std::cerr << "destination: " << dest << std::endl;

    next[curr] = next[c];
    int tmp = next[dest];
    next[dest] = a;
    next[c] = tmp;
    return next[curr];
}

int main() {
    std::string input("389125467");
    std::vector<int> next(10 + 1);

    for (int i = 0; i < input.length(); i++) {
        next[input[i] - '0'] = input[(i + 1) % 10] - '0';
    }

    next[input[input.length() - 1] - '0'] = input[0] - '0';
    int curr = input[0] - '0';
    int min = 1, max = 9;

    for (int i = 0; i < 100; i++) {
        curr = step(i + 1, curr, min, max, next);
    }

    int x = next[1];

    while (x != 1) {
        std::cout << x;
        x = next[x];
    }

    std::cout << std::endl;

    // Part 2
    max = 1000000;
    next.resize(max + 1);

    // Reset labels
    for (int i = 0; i < input.length(); i++) {
        next[input[i] - '0'] = input[(i + 1) % 10] - '0';
    }

    // Extend with a million elements
    next[input[input.length() - 1] - '0'] = 10;

    for (int i = 10; i < max; i++) {
        next[i] = i + 1;
    }

    next[max] = input[0] - '0';
    curr = input[0] - '0';

    // Do the D.A.N.C.E.
    for (int i = 0; i < 10000000; i++) {
        curr = step(i + 1, curr, min, max, next, false);
    }

    std::cout << ((int64_t) next[1]) * ((int64_t) next[next[1]]) << std::endl;
}