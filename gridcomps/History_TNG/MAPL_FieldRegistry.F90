#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"
#include "unused_dummy.H"

module MAPL_FieldRegistry
   use, intrinsic :: iso_fortran_env, only: INT64
   use ESMF
   use NUOPC
   use MAPL_ExceptionHandling

   use MAPL_FieldEntryRegistry
   use MAPL_FieldEntryRegistryMap

   implicit none
   private

   public FieldRegistry

   type :: FieldRegistry
      private
      type(FieldEntryRegistryMap) :: map
   contains
      procedure :: size
      procedure :: count
      procedure :: at
      procedure :: insert
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
      class(FieldEntryRegistry), pointer :: field_entry
      class(FieldRegistry), intent(in) :: this
      character(*),         intent(in) :: key

      field_entry => this%map%at(key)
   end function at

   subroutine insert(this, field_entry)
      class(FieldRegistry),      intent(inout) :: this
      class(FieldEntryRegistry), intent(inout) :: field_entry

      character(:), allocatable :: name

      call this%map%insert(field_entry%name(), field_entry)
   end subroutine insert
end module MAPL_FieldRegistry
