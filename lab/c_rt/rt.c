#include <stdio.h>
#include <stdlib.h>

// Function declarations and definitions
void art_panic() { printf("panic\n"); exit(-1); }
void art_print(const void *s) { printf("%s", (const char *)s); }
int art_iadd(int a, int b) { return a + b; }
int art_isub(int a, int b) { return a - b; }
int art_imul(int a, int b) { return a * b; }
int art_idiv(int a, int b) { return a / b; }
int art_irem(int a, int b) { return a % b; }
void art_exit_program(int a) { exit(a); }
int art_lnot(int a) { return !a; }
int art_land(int a, int b) { return a && b; }
int art_lor(int a, int b) { return a || b; }
int art_ineg(int a) { return -a; }
int art_ishl(int a, int b) { return a << b; }
int art_ishr(int a, int b) { return a >> b; }
int art_ilt(int a, int b) { return a < b; }
int art_ile(int a, int b) { return a <= b; }
int art_igt(int a, int b) { return a > b; }
int art_ige(int a, int b) { return a >= b; }
int art_iinv(int a) { return ~a; }
int art_iand(int a, int b) { return a & b; }
int art_ior(int a, int b) { return a | b; }
int art_ixor(int a, int b) { return a ^ b; }
float art_fneg(float a) { return -a; }
float art_fadd(float a, float b) { return a + b; }
float art_fsub(float a, float b) { return a - b; }
float art_fmul(float a, float b) { return a * b; }
float art_fdiv(float a, float b) { return a / b; }
int art_flt(float a, float b) { return a < b; }
int art_fle(float a, float b) { return a <= b; }
int art_fgt(float a, float b) { return a > b; }
int art_fge(float a, float b) { return a >= b; }