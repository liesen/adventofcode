#include <stdio.h>

int main(int argc, char **argv) {
    int n = 0;
    int r0 = 0, r1 = 0, r2 = 0, r3 = 0, r4 = 0, r5 = 0;
    r0 = 1;
    int *ip = &r1;
    
    while (*ip < 36) {
        printf("n=%d,ip=%d,r0=%d,r1=%d,r2=%d,r3=%d,r4=%d,r5=%d\n", n, *ip, r0, r1, r2, r3, r4, r5);

        switch (*ip) {
            case 0: r1 = r1 + 16; break;
            case 1: r2 = 1; break;
            case 2: r3 = 1; break;
            case 3: r4 = r2 * r3; break;
            case 4: r4 = r4 == r5; break;
            case 5: r1 = r4 + r1; break;
            case 6: r1 = r1 + 1; break;
            case 7: r0 = r2 + r0; break;
            case 8: r3 = r3 + 1; break;
            case 9: r4 = r3 > r5; break;
            case 10: r1 = r1 + r4; break;
            case 11: r1 = 2; break;
            case 12: r2 = r2 + 1; break;
            case 13: r4 = r2 > r5; break;
            case 14: r1 = r4 + r1; break;
            case 15: r1 = 1; break;
            case 16: r1 = r1 * r1; break;
            // init
            case 17: r5 = r5 + 2; break; // r5 = 2
            case 18: r5 = r5 * r5; break; // r5 = 4
            case 19: r5 = r1 * r5; break; // r5 = 76
            case 20: r5 = r5 * 11; break; // r5 = 836
            case 21: r4 = r4 + 2; break; // r4 = 2
            case 22: r4 = r4 * r1; break; // r4 = 44
            case 23: r4 = r4 + 16; break; // r4 = 60
            case 24: r5 = r5 + r4; break; // r5 = 896
            case 25: r1 = r1 + r0; break;
            case 26: r1 = 0; break; // goto 1 if r0 = 0
            case 27: r4 = r1; break; // here r1 = 1, r4=27
            case 28: r4 = r4 * r1; break; // r4 *= 28
            case 29: r4 = r1 + r4; break; // r4 += 29
            case 30: r4 = r1 * r4; break; // r4 *= 30
            case 31: r4 = r4 * 14; break; // r4 *= 14
            case 32: r4 = r4 * r1; break; // r4 *= 32
            case 33: r5 = r5 + r4; break; // r5 = ((27*28)+29)*30*14*32+896
            case 34: r0 = 0; break;
            case 35: r1 = 0; break;
        }

        *ip += 1;
        n++;
        printf("n=%d,ip=%d,r0=%d,r1=%d,r2=%d,r3=%d,r4=%d,r5=%d\n", n, *ip, r0, r1, r2, r3, r4, r5);
        getchar();
    }

    printf("r0 = %d\n", r0);
}