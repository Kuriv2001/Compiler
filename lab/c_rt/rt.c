#include <stdio.h>
#include <stdlib.h>


void panic() {
    printf("panic\n"); 
    exit(-1);
}

void print(const void *s) {printf("%s", (const char *)s);}

int iadd(int a, int b) {return a + b; }
int isub(int a, int b) {return a - b; }
int imul(int a, int b) {return a * b; }
int idiv(int a, int b) {return a / b; }
int irem(int a, int b) {return a % b; }

void exit_program(int a) {exit(a);}
int lnot(int a) {return !a;}
int land(int a, int b) {return a && b;}
int lor(int a, int b) {return a || b;}
int ineg(int a) {return -a;}
int ishl(int a, int b) {return a << b;}
int ishr(int a, int b) {return a >> b;}
int ilt(int a, int b) {return a < b;}
int ile(int a, int b) {return a <= b;}
int igt(int a, int b) {return a > b;}
int ige(int a, int b) {return a >= b;}
int iinv(int a) {return ~a;}
int iand(int a, int b) {return a & b;}
int ior(int a, int b) {return a | b;}
int ixor(int a, int b) {return a ^ b;}
float fneg(float a) {return -a;}
float fadd(float a, float b) {return a + b;}
float fsub(float a, float b) {return a - b;}
float fmul(float a, float b) {return a * b;}
float fdiv(float a, float b) {return a / b;}
int flt(float a, float b) {return a < b;}
int fle(float a, float b) {return a <= b;}
int fgt(float a, float b) {return a > b;}
int fge(float a, float b) {return a >= b;}

  
// T is the type in which we want to narrow
// V is the returned type (option)
// def narrow[T: ClassTag,V](a: Any, someConstructor: T => V, none: V): V =
//   a match
//     case t: T => someConstructor(t)
//     case _ => none

// def narrowUnconditionally[T: ClassTag](a: Any): T =
//   a match
//     case t: T => t
//     case _ => panic