#include <stdio.h>
#include <stdlib.h>

// For Literal
typedef enum {
    INT,
    FLOAT,
    STRING,
    BOOL
} ArtType;

typedef union {
    int i;
    float f;
    char* s;
    int b; // Represent boolean as int
    void * record;
} ArtValue;

typedef struct {
    ArtType type;
    ArtValue value;
} ArtVariant;

// For Records
typedef struct {
    ArtType type;
    char* label;
    ArtValue value;
} ArtRecordField;

typedef struct {
    char* name;
    size_t field_count;
    ArtRecordField* fields;
} ArtRecord;

ArtRecordField create_field(const char* label, ArtType type, ArtValue value);
ArtRecord create_record(const char* name, size_t field_count);
void add_field_to_record(ArtRecord* record, size_t index, ArtRecordField field);

void art_panic();
void art_print(ArtVariant v);

int art_iadd(int a, int b);
int art_isub(int a, int b);
int art_imul(int a, int b);
int art_idiv(int a, int b);
int art_irem(int a, int b);
void art_exit_program(int a);
int art_lnot(int a);
int art_land(int a, int b);
int art_lor(int a, int b);
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

float art_fneg(float a);
float art_fadd(float a, float b);
float art_fsub(float a, float b);
float art_fmul(float a, float b);
float art_fdiv(float a, float b);
int art_flt(float a, float b);
int art_fle(float a, float b);
int art_fgt(float a, float b);
int art_fge(float a, float b);
