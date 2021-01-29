#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"
#include "unused_dummy.H"

module MAPL_FieldEntryRegistry
   use ESMF
   use NUOPC
   use MAPL_ExceptionHandling
   use MAPL_AbstractFieldEntry

   implicit none
   private

   public FieldEntryRegistry

   type, extends(AbstractFieldEntry) :: FieldEntryRegistry
   contains
      procedure :: initialize
      procedure :: name
   end type FieldEntryRegistry
contains
   subroutine initialize(this, short_name, component_name, units, alias_name)
      class(FieldEntryRegistry), intent(  out) :: this
      character(*),              intent(in   ) :: short_name
      character(*),              intent(in   ) :: component_name
      character(*), optional,    intent(in   ) :: units
      character(*), optional,    intent(in   ) :: alias_name

      _UNUSED_DUMMY(alias_name)

      call this%base_initialize(short_name, component_name, units=units)
   end subroutine initialize

   function name(this) result(field_name)
      character(:), allocatable :: field_name
      class(FieldEntryRegistry), intent(inout) :: this

      field_name = this%standard_name()
   end function name
end module MAPL_FieldEntryRegistry

module MAPL_FieldEntryRegistryMap
   use MAPL_FieldEntryRegistry

#include "types/key_deferredLengthString.inc"
#define _value type(FieldEntryRegistry)

#define _map FieldEntryRegistryMap
#define _iterator FieldEntryRegistryMapIterator
#define _alt
#include "templates/map.inc"
end module MAPL_FieldEntryRegistryMap
