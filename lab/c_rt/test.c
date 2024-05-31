#include <stdio.h>
#include <stdlib.h>
#include "rt.h"

ArtVariant create_record_RaI(ArtVariant  no_label0){
    ArtVariant tempRecord;
    init_record(&tempRecord, "RaI", 1);
    add_field_to_record(&tempRecord, 0, no_label0);
    return tempRecord;
}


int main(int argc, char *argv[]) {
ArtVariant  _statusI;
if (art_compare(create_record_RaI((ArtVariant){.type = INT, .value.i = 1, .label = "", .num_fields = 1}), (create_record_RaI((ArtVariant){.type = INT, .value.i = 2, .label = "", .num_fields = 1})))) {
  _statusI = (ArtVariant){.type = INT, .value.i = 2, .label = "", .num_fields = 1};
}
else if (art_compare(create_record_RaI((ArtVariant){.type = INT, .value.i = 1, .label = "", .num_fields = 1}), (create_record_RaI((ArtVariant){.type = INT, .value.i = 1, .label = "", .num_fields = 1})))) {
  _statusI = (ArtVariant){.type = INT, .value.i = 1, .label = "", .num_fields = 1};
}
else if (art_compare(create_record_RaI((ArtVariant){.type = INT, .value.i = 1, .label = "", .num_fields = 1}), (ArtVariant) {.label = "", .num_fields = 0, .type = WILDCARD, .value.i = 0 })) {
  _statusI = (ArtVariant){.type = INT, .value.i = 0, .label = "", .num_fields = 1};
}
else art_panic();


//Start of main:
art_print(_statusI);

}