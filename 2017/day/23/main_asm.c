#include <stdio.h>
int main() {
int a, b, c, d, e, f, g, h, mulcnt;
a = b = c = d = e = f = g = h = mulcnt = 0;
__1: b = 93;  // set b 93
__2: c = b;  // set c b
__3: if (a != 0) goto __5;  // jnz a 2
__4: if (1 != 0) goto __9;  // jnz 1 5
__5: b *= 100;  mulcnt += 1; // mul b 100
__6: b -= -100000;  // sub b -100000
__7: c = b;  // set c b
__8: c -= -17000;  // sub c -17000
__9: f = 1;  // set f 1
__10: d = 2;  // set d 2
__11: e = 2;  // set e 2
__12: g = d;  // set g d
__13: g *= e;  mulcnt += 1; // mul g e
__14: g -= b;  // sub g b
__15: if (g != 0) goto __17;  // jnz g 2
__16: f = 0;  // set f 0
__17: e -= -1;  // sub e -1
__18: g = e;  // set g e
__19: g -= b;  // sub g b
__20: if (g != 0) goto __12;  // jnz g -8
__21: d -= -1;  // sub d -1
__22: g = d;  // set g d
__23: g -= b;  // sub g b
__24: if (g != 0) goto __11;  // jnz g -13
__25: if (f != 0) goto __27;  // jnz f 2
__26: h -= -1;  // sub h -1
__27: g = b;  // set g b
__28: g -= c;  // sub g c
__29: if (g != 0) goto __31;  // jnz g 2
__30: if (1 != 0) goto __33;  // jnz 1 3
__31: b -= -17;  // sub b -17
__32: if (1 != 0) goto __9;  // jnz 1 -23
__33:
printf("a=%d b=%d c=%d d=%d e=%d f=%d g=%d h=%d, mulcnt=%d\n", a, b, c, d, e, f, g, h, mulcnt);
return 0;
}
