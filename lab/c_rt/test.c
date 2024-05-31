#include <stdio.h>
#include <stdlib.h>
#include "rt.h"

ArtVariant create_record_RrecordI(ArtVariant  no_label0){
    ArtVariant tempRecord;
    init_record(&tempRecord, "RrecordI", 1);
    add_field_to_record(&tempRecord, 0, no_label0);
    return tempRecord;
}


int main(int argc, char *argv[]) {
ArtVariant  _xI = (ArtVariant){.type = INT, .value.i = 3, .label = "", .num_fields = 1};

ArtVariant  _recordRrecordI = create_record_RrecordI(_xI);

ArtVariant  _statusRunit;
if (art_compare(_recordRrecordI, create_record_RrecordI(_xI))) {
  _statusRunit = art_print((ArtVariant){.type = STRING, .value.s = "Congrats", .label = "", .num_fields = 1});
}
else if (art_compare(_recordRrecordI, (ArtVariant) {.label = "", .num_fields = 0, .type = WILDCARD, .value.i = 0 })) {
  _statusRunit = art_print((ArtVariant){.type = STRING, .value.s = "Sorry :(", .label = "", .num_fields = 1});
}
else art_panic();

//Start of main:
_statusRunit;

free_record(&_statusRunit);
free_record(&_recordRrecordI);
}