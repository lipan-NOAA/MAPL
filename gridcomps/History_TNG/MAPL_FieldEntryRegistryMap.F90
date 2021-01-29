module MAPL_FieldEntryRegistryMap
   use MAPL_FieldEntryRegistry

#include "types/key_deferredLengthString.inc"
#define _value class(FieldEntryRegistry)
#define _value_allocatable class(FieldEntryRegistry)

#define _map FieldEntryRegistryMap
#define _iterator FieldEntryRegistryMapIterator
#define _pair FieldEntryRegistryPair
#define _alt
#include "templates/map.inc"
end module MAPL_FieldEntryRegistryMap
