#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lib.h"

void art_panic() { 
    printf("panic\n"); 
    exit(-1); 
}

// returns true if both ArtVariants can be matched else false
int art_compare(ArtVariant a, ArtVariant b) {
    if (b.type == WILDCARD) {
        return 1;
    }

    if (a.type != b.type) {
        return 0;
    }

    switch (a.type) {
        // case INT:
        //     return (b.num_fields <= 0) || (a.value.i == b.value.i);

        // case FLOAT:
        //     return (b.num_fields <= 0) || (a.value.f == b.value.f);

        // case STRING:
        //     return (b.num_fields <= 0) || (strcmp(a.value.s, b.value.s) == 0);

        // case BOOL:
        //     return (b.num_fields <= 0) || (a.value.b == b.value.b);
         case INT:
            return (a.value.i == b.value.i);

        case FLOAT:
            return (a.value.f == b.value.f);

        case STRING:
            return (strcmp(a.value.s, b.value.s) == 0);

        case BOOL:
            return (a.value.b == b.value.b);

        case RECORD: {
            // Compare the labels
            int label_cmp = strcmp(a.label, b.label);
            if (label_cmp != 0 || a.num_fields != b.num_fields) return 0;

            

            // Compare the fields (assumes same number of fields)
            for (size_t i = 0; i < a.num_fields; ++i) {
                int field_cmp = art_compare(a.value.recordFields[i], b.value.recordFields[i]);
                if (field_cmp == 0) return 0;
            }
            return 1;
        }

        default:
            printf("Unknown type in art_compare\n");
            art_panic();
            return 0;  // Return a value to ensure consistency
    }
}

void init_record(ArtVariant *record, char * label, int num_fields) {
    record->type = RECORD;
    strncpy(record->label, label, 100);
    if (num_fields > 20) {
        fprintf(stderr, "Cannot have more than 20 fields in a record.\n");
        exit(EXIT_FAILURE);
    }
    record->num_fields = num_fields;
    record->value.recordFields = (ArtVariant*)malloc(num_fields * sizeof(ArtVariant));
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
    record->value.recordFields[index].num_fields = value.num_fields;
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
//TODO all have to be updated for fitting ArtVariant (when used)
ArtVariant art_iadd(ArtVariant a, ArtVariant b) { 
    ArtVariant result;
    result.type = INT;
    result.value.i = a.value.i + b.value.i;
    return result;
}
int art_isub(int a, int b) { return a - b; }

ArtVariant art_imul(ArtVariant a, ArtVariant b) {
    ArtVariant result;
    result.type = INT;
    result.value.i = a.value.i * b.value.i;
    return result;
}

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