#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
    ArtRecordField *fields;
} ArtRecord;

ArtRecordField create_field(const char* label, ArtVariant variant) {
    ArtRecordField field;
    field.label = strdup(label);
    field.type = variant.type;
    field.value = variant.value;
    return field;
}

ArtRecord* create_record(const char* name, size_t field_count) {
    ArtRecord* record = (ArtRecord*)malloc(sizeof(ArtRecord));
    if (!record) {
        perror("Failed to allocate memory for record");
        exit(EXIT_FAILURE);
    }
    record->name = strdup(name);
    record->field_count = field_count;
    record->fields = (ArtRecordField*)malloc(field_count * sizeof(ArtRecordField));
    if (!record->fields) {
        perror("Failed to allocate memory for fields");
        free(record->name);
        free(record);
        exit(EXIT_FAILURE);
    }
    return record;
}

void add_field_to_record(ArtRecord* record, size_t index, ArtRecordField field) {
    if (index >= record->field_count) {
        fprintf(stderr, "Index out of bounds when adding field to record\n");
        return;
    }
    record->fields[index] = field;
}

void free_field(ArtRecordField* field) {
    free(field->label);
    if (field->type == STRING) {
        free(field->value.s);
    }
}

void free_record(ArtRecord* record) {
    for (size_t i = 0; i < record->field_count; ++i) {
        free_field(&record->fields[i]);
    }
    free(record->fields);
    free(record->name);
    free(record);
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