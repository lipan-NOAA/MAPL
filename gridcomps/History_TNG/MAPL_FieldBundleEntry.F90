#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module MAPL_FieldBundleEntry
   use ESMF
   use NUOPC
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod

   use MAPL_AbstractFieldEntry
   use MAPL_FieldRegistryEntry

   implicit none
   private

   public FieldBundleEntry

   type, extends(AbstractFieldEntry) :: FieldBundleEntry
      private
      character(:), allocatable :: alias_name
   contains
      procedure :: initialize
      procedure :: get_alias_name

      procedure :: set_alias_name

      procedure :: name

      procedure :: registry_entry
   end type FieldBundleEntry
contains
   subroutine initialize(this, short_name, component_name, unusable, units, alias_name)
      class(FieldBundleEntry),          intent(  out) :: this
      character(*),                     intent(in   ) :: short_name
      character(*),                     intent(in   ) :: component_name
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      character(*),           optional, intent(in   ) :: units
      character(*),           optional, intent(in   ) :: alias_name

      _UNUSED_DUMMY(unusable)

      call this%base_initialize(short_name, component_name, units=units)

      if (present(alias_name)) this%alias_name = alias_name
   end subroutine initialize

   function get_alias_name(this) result(alias)
      character(:), allocatable :: alias
      class(FieldBundleEntry), intent(in) :: this

      alias = this%alias_name
   end function get_alias_name

   subroutine set_alias_name(this, alias_name, rc)
      class(FieldBundleEntry), intent(inout) :: this
      character(*),                intent(in   ) :: alias_name
      integer, optional,           intent(  out) :: rc

      integer :: status = 0

      if (allocated(this%alias_name)) then
         status = 1
      else
         this%alias_name = alias_name
      end if

      _RETURN(status)
   end subroutine set_alias_name

   function name(this) result(field_name)
      character(:), allocatable :: field_name
      class(FieldBundleEntry), intent(inout) :: this

      character(:), allocatable :: standard_name

      standard_name = this%standard_name()

      if (allocated(this%alias_name)) then
         field_name = this%alias_name // '.' // standard_name
      else
         field_name = standard_name
      end if
   end function name

   function registry_entry(this) result(field_entry)
      type(FieldRegistryEntry) :: field_entry
      class(FieldBundleEntry), intent(in) :: this

      call field_entry%initialize(this%get_short_name(), &
         this%get_component_name(), units=this%get_units())
   end function registry_entry
end module MAPL_FieldBundleEntry
