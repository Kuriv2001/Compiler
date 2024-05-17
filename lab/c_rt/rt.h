#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
    INT,
    FLOAT,
    STRING,
    BOOL,
    RECORD
} ArtType;

typedef struct ArtVariant ArtVariant;

typedef union {
    int i;
    float f;
    char s[100];
    int b;
    ArtVariant* recordFields;
} ArtValue;

struct ArtVariant {
    char label[100];
    ArtType type;
    ArtValue value;
};

// Function declarations
void art_panic(void);
void add_field_to_record(ArtVariant *record, size_t index, ArtVariant value);
void init_record(ArtVariant *record, char * label);
void free_record(ArtVariant *record);
void art_print(ArtVariant v);

// Integer arithmetic operations
int art_iadd(int a, int b);
int art_isub(int a, int b);
int art_imul(int a, int b);
int art_idiv(int a, int b);
int art_irem(int a, int b);

// Program control
void art_exit_program(int a);

// Logical operations
int art_lnot(int a);
int art_land(int a, int b);
int art_lor(int a, int b);

// Integer bitwise and comparison operations
int art_ineg(int a);
int art_ishl(int a, int b);
int art_ishr(int a, int b);
int art_ilt(int a, int b);
int art_ile(int a, int b);
int art_igt(int a, int b);
int art_ige(int a, int b);
int art_iinv(int a);
int art_iand(int a, int b);
int art_ior(int a, int b);
int art_ixor(int a, int b);

// Floating-point operations
float art_fneg(float a);
float art_fadd(float a, float b);
float art_fsub(float a, float b);
float art_fmul(float a, float b);
float art_fdiv(float a, float b);
int art_flt(float a, float b);
int art_fle(float a, float b);
int art_fgt(float a, float b);
int art_fge(float a, float b);