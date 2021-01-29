#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"
#include "unused_dummy.H"

module MAPL_FieldCollection
   use, intrinsic :: iso_fortran_env, only: INT64
   use ESMF
   use NUOPC
   use MAPL_ExceptionHandling

   use MAPL_FieldEntryCollection
   use MAPL_FieldEntryCollectionMap

   implicit none
   private

   public FieldCollection

   type :: FieldCollection
      private
      type(FieldEntryCollectionMap) :: map
   contains
      procedure :: size
      procedure :: count
      procedure :: at
      procedure :: insert
   end type FieldCollection
contains
   integer(kind=INT64) function size(this)
      class(FieldCollection), intent(in) :: this

      size = this%map%size()
   end function size

   integer(kind=INT64) function count(this, key)
      class(FieldCollection), intent(in) :: this
      character(*),           intent(in) :: key

      count = this%map%count(key)
   end function count

   function at(this, key) result(field_entry)
      class(FieldEntryCollection), pointer :: field_entry
      class(FieldCollection), intent(in) :: this
      character(*),           intent(in) :: key

      field_entry => this%map%at(key)
   end function at

   subroutine insert(this, field_entry)
      class(FieldCollection),      intent(inout) :: this
      class(FieldEntryCollection), intent(inout) :: field_entry

      character(:), allocatable :: name

      call this%map%insert(field_entry%name(), field_entry)
   end subroutine insert
end module MAPL_FieldCollection
