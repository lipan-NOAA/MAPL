#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module CollectionRegistryMod
   use, intrinsic :: iso_fortran_env, only: INT64
   use ESMF
   use NUOPC
   use yaFyaml
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod

   use CollectionMod
   use CollectionMapMod

   implicit none
   private

   public CollectionRegistry

   type :: CollectionRegistry
      private
      type(CollectionMap) :: map
   contains
      procedure :: size
      procedure :: count
      procedure :: at
      procedure :: insert
   end type CollectionRegistry
contains
   integer(kind=INT64) function size(this)
      class(CollectionRegistry), intent(in) :: this

      size = this%map%size()
   end function size

   integer(kind=INT64) function count(this, key)
      class(CollectionRegistry), intent(in) :: this
      character(*),              intent(in) :: key

      count = this%map%count(key)
   end function count

   function at(this, key) result(collection_entry)
      class(Collection), pointer :: collection_entry
      class(CollectionRegistry), intent(in) :: this
      character(*),              intent(in) :: key

      collection_entry => this%map%at(key)
   end function at

   subroutine insert(this, key, collection_entry, unusable, rc)
      class(CollectionRegistry),        intent(inout) :: this
      character(*),                     intent(in   ) :: key
      class(Collection),                intent(inout) :: collection_entry
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      status = 0
      if (this%count(key) > 0) then
         status = 1
      else
         call this%map%insert(key, collection_entry)
      end if

      if (present(rc)) rc = status
   end subroutine insert
end module CollectionRegistryMod
