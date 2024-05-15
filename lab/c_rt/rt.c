#include <stdio.h>
#include <stdlib.h>

//For Literal
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

//For Records

typedef struct {
    ArtType type;
    char* label;
    ArtValue value;
} ArtRecordField;

typedef struct {
    char* name;
    size_t field_count;
    ArtRecordField * fields;
} ArtRecord;

ArtRecordField create_field(const char* label, ArtType type, ArtValue value) {
    ArtRecordField field;
    field.label = (char*)label;
    field.type = type;
    field.value = value;
    return field;
}

ArtRecord create_record(const char* name, size_t field_count) {
    ArtRecord record;
    record.name = (char*)name; 
    record.field_count = field_count;
    return record;
}

void add_field_to_record(ArtRecord* record, size_t index, ArtRecordField field) {
    record->fields[index] = field;
}

void art_panic() { printf("panic\n"); exit(-1); }
void art_print(ArtVariant v) {
    switch (v.type) {
        case INT:
            printf("%d", v.value.i);
            break;
        case FLOAT:
            printf("%f", v.value.f);
            break;
        case STRING:
            printf("%s", v.value.s);
            break;
        case BOOL:
            printf("%s", v.value.b ? "true" : "false");
            break;
        default:
            printf("Unknown type");
    }
}
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