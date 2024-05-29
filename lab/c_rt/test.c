#include <stdio.h>
#include <stdlib.h>
#include "rt.h"

ArtVariant create_record_RaI(ArtVariant  no_label0){
    ArtVariant tempRecord;
    init_record(&tempRecord, "RaI", 1);
    add_field_to_record(&tempRecord, 0, no_label0);
    return tempRecord;
}


ArtVariant create_record_RxRaII(ArtVariant  no_label0, ArtVariant  no_label1){
    ArtVariant tempRecord;
    init_record(&tempRecord, "RxRaII", 2);
    add_field_to_record(&tempRecord, 0, no_label0);
    add_field_to_record(&tempRecord, 1, no_label1);
    return tempRecord;
}


int main(int argc, char *argv[]) {
ArtVariant  _xRxRaII = create_record_RxRaII(create_record_RaI((ArtVariant){.type = INT, .value.i = 1, .label = "", .num_fields = 1}), (ArtVariant){.type = INT, .value.i = 2, .label = "", .num_fields = 1});

ArtVariant  _statusI;
if (art_compare(_xRxRaII, create_record_RxRaII(create_record_RaI(_yI: ArtVariant), _zI: ArtVariant))) {
  _statusI = art_iadd(art_iadd(_yI, _zI), art_imul((ArtVariant){.type = INT, .value.i = 1, .label = "", .num_fields = 1}, (ArtVariant){.type = INT, .value.i = 2, .label = "", .num_fields = 1}));
}
else art_panic();


//Start of main:
art_print(_statusI);

free_record(&_xRxRaII);
}