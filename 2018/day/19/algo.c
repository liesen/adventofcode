#include <assert.h>
#include <stdio.h>

int main() {
    // Goto statements
    int r0 = 0, r1 = 0, r2 = 0, r3 = 0, r4 = 0, r5 = 0;

    r0 = 0, r1 = 1, r2 = 1, r3 = 1, r4 = 60, r5 = 896;

    // Begin
    r2 = 1;

    ip_1:
    r3 = 1;

    ip_2:
    r4 = r2 * r3;

    if (r4 == r5) {
        r0 += r2;
    }

    r3 += 1;

    if (!(r3 > r5)) {
        goto ip_2;
    }

    r2 += 1;

    if (!(r2 > r5)) {
        goto ip_1;
    }

    // Done
    assert(r0 == 2040);
    printf("%d\n", r0);

    // Same algorithm but with do-while
    r0 = 0, r1 = 1, r2 = 1, r3 = 1, r4 = 60, r5 = 896;

    // Begin
    r2 = 1;

    do {
        r3 = 1;

        do {
            if (r2 * r3 == r5) {
                // printf("r0=%d,r1=%d,r2=%d,r3=%d,r4=%d,r5=%d,r2*r3=%d\n", r0, r1, r2, r3, r4, r5, r2*r3);
                r0 += r2;
            }
            r3 += 1;
        } while (r3 <= r5);

        r2++;
    } while (r2 <= r5);

    // Done
    printf("%d\n", r0);
    assert(r0 == 2040);


    // Now for-loops
    r0 = 0, r1 = 1, r2 = 1, r3 = 1, r4 = 60, r5 = 896;

    for (r2 = 1; r2 <= r5; r2++) {
        for (r3 = 1; r3 <= r5; r3++) {
            if (r2 * r3 == r5) r0 += r2;
        }
    }

    // Done
    printf("%d\n", r0);
    assert(r0 == 2040);
}