#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module FieldRegistryMod
   use, intrinsic :: iso_fortran_env, only: INT64
   use ESMF
   use NUOPC
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod

   use FieldEntryMod
   use FieldEntryMapMod

   implicit none
   private

   public FieldRegistry

   type :: FieldRegistry
      private
      type(FieldEntryMap) :: map
   contains
      procedure :: size
      procedure :: count
      procedure :: at
      procedure :: insert

      procedure :: advertise
   end type FieldRegistry
contains
   integer(kind=INT64) function size(this)
      class(FieldRegistry), intent(in) :: this

      size = this%map%size()
   end function size

   integer(kind=INT64) function count(this, key)
      class(FieldRegistry), intent(in) :: this
      character(*),         intent(in) :: key

      count = this%map%count(key)
   end function count

   function at(this, key) result(field_entry)
      class(FieldEntry), pointer :: field_entry
      class(FieldRegistry), intent(in) :: this
      character(*),         intent(in) :: key

      field_entry => this%map%at(key)
   end function at

   subroutine insert(this, field_entry)
      class(FieldRegistry), intent(inout) :: this
      class(FieldEntry),    intent(inout) :: field_entry

      call this%map%insert(field_entry%standard_name(), field_entry)
   end subroutine insert

   subroutine advertise(this, state, unusable, rc)
      class(FieldRegistry),             intent(inout) :: this
      type(ESMF_State),                 intent(inout) :: state
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      integer,                optional, intent(  out) :: rc

      class(FieldEntry), pointer  :: field_entry
      type(FieldEntryMapIterator) :: iter

      integer :: status

      _UNUSED_DUMMY(unusable)

      iter = this%map%begin()
      do while(iter /= this%map%end())
         field_entry => iter%value()
         call field_entry%advertise(state, __RC__)

         call iter%next()
      end do

      _RETURN(_SUCCESS)
   end subroutine advertise
end module FieldRegistryMod
