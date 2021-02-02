module MAPL_FieldBundleEntryMap
   use MAPL_FieldBundleEntry

#include "types/key_deferredLengthString.inc"
#define _value class(FieldBundleEntry)
#define _value_allocatable class(FieldBundleEntry)

#define _map FieldBundleEntryMap
#define _iterator FieldBundleEntryMapIterator
#define _pair FieldBundleEntryPair
#define _alt
#include "templates/map.inc"
end module MAPL_FieldBundleEntryMap
