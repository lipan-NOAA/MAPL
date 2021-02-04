module FieldRegistryEntryMapMod
   use FieldRegistryEntryMod

#include "types/key_deferredLengthString.inc"
#define _value class(FieldRegistryEntry)
#define _value_allocatable class(FieldRegistryEntry)

#define _map FieldRegistryEntryMap
#define _iterator FieldRegistryEntryMapIterator
#define _pair FieldRegistryEntryPair
#define _alt
#include "templates/map.inc"
end module FieldRegistryEntryMapMod
