#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module MAPL_FieldEntryRegistry
   use ESMF
   use NUOPC
   use MAPL_ExceptionHandling
   use MAPL_AbstractFieldEntry
   use MAPL_KeywordEnforcerMod

   implicit none
   private

   public FieldEntryRegistry

   type, extends(AbstractFieldEntry) :: FieldEntryRegistry
   contains
      procedure :: initialize
      procedure :: name
   end type FieldEntryRegistry
contains
   subroutine initialize(this, short_name, component_name, unusable, units, alias_name)
      class(FieldEntryRegistry),        intent(  out) :: this
      character(*),                     intent(in   ) :: short_name
      character(*),                     intent(in   ) :: component_name
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      character(*),           optional, intent(in   ) :: units
      character(*),           optional, intent(in   ) :: alias_name

      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(alias_name)

      call this%base_initialize(short_name, component_name, units=units)
   end subroutine initialize

   function name(this) result(field_name)
      character(:), allocatable :: field_name
      class(FieldEntryRegistry), intent(inout) :: this

      field_name = this%standard_name()
   end function name
end module MAPL_FieldEntryRegistry
