#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module GroupRegistryMod
   use, intrinsic :: iso_fortran_env, only: INT64
   use ESMF
   use NUOPC
   use yaFyaml
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod

   use GroupMod
   use GroupMapMod

   implicit none
   private

   public GroupRegistry

   type :: GroupRegistry
      private
      type(GroupMap) :: map
   contains
      procedure :: size
      procedure :: count
      procedure :: at
      procedure :: insert
   end type GroupRegistry
contains
   integer(kind=INT64) function size(this)
      class(GroupRegistry), intent(in) :: this

      size = this%map%size()
   end function size

   integer(kind=INT64) function count(this, key)
      class(GroupRegistry), intent(in) :: this
      character(*),         intent(in) :: key

      count = this%map%count(key)
   end function count

   function at(this, key) result(group_entry)
      class(Group), pointer :: group_entry
      class(GroupRegistry), intent(in) :: this
      character(*),         intent(in) :: key

      group_entry => this%map%at(key)
   end function at

   subroutine insert(this, key, group_entry, unusable, rc)
      class(GroupRegistry),             intent(inout) :: this
      character(*),                     intent(in   ) :: key
      type(Group),                      intent(inout) :: group_entry
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      status = 0
      if (this%count(key) > 0) then
         status = 1
      else
         call this%map%insert(key, group_entry)
      end if

      if (present(rc)) rc = status
   end subroutine insert
end module GroupRegistryMod
