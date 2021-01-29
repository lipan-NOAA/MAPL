#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module MAPL_FieldCollection
   use, intrinsic :: iso_fortran_env, only: INT64
   use ESMF
   use NUOPC
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod

   use MAPL_FieldEntryCollection
   use MAPL_FieldEntryCollectionMap

   use MAPL_FieldEntryRegistry
   use MAPL_FieldRegistry

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

      procedure :: advertise
      procedure :: register
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
      class(FieldCollection),     intent(inout) :: this
      type(FieldEntryCollection), intent(inout) :: field_entry

      character(:), allocatable :: name

      call this%map%insert(field_entry%name(), field_entry)
   end subroutine insert

   subroutine advertise(this, state, unusable,&
         TransferOfferGeomObject, SharePolicyField, SharePolicyGeomObject, rc)
      class(FieldCollection),           intent(inout) :: this
      type(ESMF_State),                 intent(inout) :: state
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      character(*),           optional, intent(in   ) :: TransferOfferGeomObject
      character(*),           optional, intent(in   ) :: SharePolicyField
      character(*),           optional, intent(in   ) :: SharePolicyGeomObject
      integer,                optional, intent(  out) :: rc

      class(FieldEntryCollection), allocatable :: field_entry
      type(FieldEntryCollectionMapIterator)    :: iter

      integer :: status

      _UNUSED_DUMMY(unusable)

      iter = this%map%begin()
      do while(iter /= this%map%end())
         field_entry = iter%value()
         call field_entry%advertise(state, &
            TransferOfferGeomObject=TransferOfferGeomObject, &
            SharePolicyField=SharePolicyField, &
            SharePolicyGeomObject=SharePolicyGeomObject, &
            __RC__)

         call iter%next()
      end do

      _RETURN(_SUCCESS)
   end subroutine advertise

   subroutine register(this, field_registry)
      class(FieldCollection), intent(inout) :: this
      type(FieldRegistry),    intent(inout) :: field_registry

      class(FieldEntryCollection), allocatable :: field_entry
      type(FieldEntryRegistry)                 :: registry_entry
      type(FieldEntryCollectionMapIterator)    :: iter

      iter = this%map%begin()
      do while(iter /= this%map%end())
         field_entry    = iter%value()
         registry_entry = field_entry%registry_entry()
         call field_registry%insert(registry_entry)

         call iter%next()
      end do
   end subroutine register
end module MAPL_FieldCollection
