module MAPL_FieldEntryCollectionMap
   use MAPL_FieldEntryCollection

#include "types/key_deferredLengthString.inc"
#define _value class(FieldEntryCollection)
#define _value_allocatable class(FieldEntryCollection)

#define _map FieldEntryCollectionMap
#define _iterator FieldEntryCollectionMapIterator
#define _pair FieldEntryCollectionPair
#define _alt
#include "templates/map.inc"
end module MAPL_FieldEntryCollectionMap
