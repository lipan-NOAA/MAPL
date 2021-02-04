#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module FieldGroupEntryMod
   use ESMF
   use NUOPC
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod

   use AbstractFieldEntryMod
   use FieldRegistryEntryMod

   implicit none
   private

   public FieldGroupEntry
   public default_alias

   character(*), parameter :: default_alias = "no_alias"

   type, extends(AbstractFieldEntry) :: FieldGroupEntry
      private
      character(:), allocatable :: alias_name
   contains
      procedure :: initialize
      procedure :: get_alias_name

      procedure :: set_alias_name

      procedure :: name

      procedure :: registry_entry
   end type FieldGroupEntry
contains
   subroutine initialize(this, short_name, component_name, unusable, units, alias_name)
      class(FieldGroupEntry),           intent(  out) :: this
      character(*),                     intent(in   ) :: short_name
      character(*),                     intent(in   ) :: component_name
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      character(*),           optional, intent(in   ) :: units
      character(*),           optional, intent(in   ) :: alias_name

      _UNUSED_DUMMY(unusable)

      call this%base_initialize(short_name, component_name, units=units)

      if (present(alias_name)) then
         this%alias_name = alias_name
      else
         this%alias_name = default_alias
      end if
   end subroutine initialize

   function get_alias_name(this) result(alias)
      character(:), allocatable :: alias
      class(FieldGroupEntry), intent(in) :: this

      alias = this%alias_name
   end function get_alias_name

   subroutine set_alias_name(this, alias_name, rc)
      class(FieldGroupEntry), intent(inout) :: this
      character(*),           intent(in   ) :: alias_name
      integer, optional,      intent(  out) :: rc

      integer :: status

      status = 0

      if (this%alias_name == default_alias) then
         this%alias_name = alias_name
      else
         status = 1
      end if

      if (present(rc)) rc = status
   end subroutine set_alias_name

   function name(this) result(field_name)
      character(:), allocatable :: field_name
      class(FieldGroupEntry), intent(inout) :: this

      character(:), allocatable :: standard_name

      standard_name = this%standard_name()

      if (this%alias_name == default_alias) then
         field_name = standard_name
      else
         field_name = this%alias_name // '.' // standard_name
      end if
   end function name

   function registry_entry(this) result(field_entry)
      type(FieldRegistryEntry) :: field_entry
      class(FieldGroupEntry), intent(in) :: this

      call field_entry%initialize(this%get_short_name(), &
         this%get_component_name(), units=this%get_units())
   end function registry_entry
end module FieldGroupEntryMod
