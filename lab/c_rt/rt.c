#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lib.h"

void art_panic() { 
    printf("panic\n"); 
    exit(-1); 
}

void init_record(ArtVariant *record, char * label) {
    record->type = RECORD;
    strncpy(record->label, label, 100);
    record->value.recordFields = (ArtVariant*)malloc(20 * sizeof(ArtVariant));
    if (record->value.recordFields == NULL) {
        fprintf(stderr, "Failed to allocate memory for record fields.\n");
        exit(EXIT_FAILURE);
    }
}

// Function to add a field to a record
void add_field_to_record(ArtVariant *record, size_t index, ArtVariant value) {
    if (record->type != RECORD) {
        fprintf(stderr, "Cannot add field to a non-record type.\n");
        exit(EXIT_FAILURE);
    }
    strcpy(record->value.recordFields[index].label, value.label);
    record->value.recordFields[index].type = value.type;
    record->value.recordFields[index].value = value.value;
}

void free_record(ArtVariant *record) {
    if (record->type == RECORD) {
        free(record->value.recordFields);
        record->value.recordFields = NULL;
    }
}

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
        case RECORD:
            printf("Record %s:\n", v.label);
            for (size_t i = 0; i < 20; ++i) {
                if (strlen(v.value.recordFields[i].label) > 0) {
                    printf("  %s: ", v.value.recordFields[i].label);
                    art_print(v.value.recordFields[i]);
                    printf("\n");
                }
            }
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