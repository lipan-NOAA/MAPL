#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module FieldEntryMod
   use ESMF
   use NUOPC
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod

   implicit none
   private

   public :: FieldEntry
   public :: default_units

   character(*), parameter :: default_units = '1'

   type :: FieldEntry
      private
      character(:), allocatable :: short_name
      character(:), allocatable :: component_name
      character(:), allocatable :: units
   contains
      procedure :: initialize

      procedure :: get_short_name
      procedure :: get_component_name
      procedure :: get_units

      procedure :: set_units

      procedure :: equal_to_entry
      generic   :: operator(==) => equal_to_entry
      procedure :: not_equal_to_entry
      generic   :: operator(/=) => not_equal_to_entry

      procedure :: standard_name

      procedure :: NUOPC_has_entry
      procedure :: NUOPC_add_entry
      procedure :: register

      procedure :: NUOPC_advert
      procedure :: advertise
   end type FieldEntry
contains
   subroutine initialize(this, short_name, component_name, unusable, units)
      class(FieldEntry),                intent(  out) :: this
      character(*),                     intent(in   ) :: short_name
      character(*),                     intent(in   ) :: component_name
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      character(*),           optional, intent(in   ) :: units

      _UNUSED_DUMMY(unusable)

      this%short_name     = short_name
      this%component_name = component_name

      if (present(units)) then
         this%units = units
      else
         this%units = default_units
      end if
   end subroutine initialize

   function get_short_name(this) result(short_name)
      character(:), allocatable :: short_name
      class(FieldEntry), intent(in) :: this

      short_name = this%short_name
   end function get_short_name

   function get_component_name(this) result(component_name)
      character(:), allocatable :: component_name
      class(FieldEntry), intent(in) :: this

      component_name = this%component_name
   end function get_component_name

   function get_units(this) result(units)
      character(:), allocatable :: units
      class(FieldEntry), intent(in) :: this

      units = this%units
   end function get_units

   subroutine set_units(this, units, unusable, rc)
      class(FieldEntry),                intent(inout) :: this
      character(*),                     intent(in   ) :: units
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      status = 0

      if (this%units /= default_units) then
         status = 1
      else
         this%units = units
      end if

      if(present(rc)) rc = status
   end subroutine set_units

   logical function equal_to_entry(this, field_entry)
      class(FieldEntry), intent(in) :: this
      class(FieldEntry), intent(in) :: field_entry

      logical :: equiv

      equiv = same_type_as(this, field_entry)

      if (this%short_name     /= field_entry%get_short_name())     equiv = .false.
      if (this%component_name /= field_entry%get_component_name()) equiv = .false.
      if (this%units          /= field_entry%get_units())          equiv = .false.

      equal_to_entry = equiv
   end function equal_to_entry

   logical function not_equal_to_entry(a, b)
      class(FieldEntry), intent(in) :: a
      class(FieldEntry), intent(in) :: b

      not_equal_to_entry = .not. (a == b)
   end function not_equal_to_entry

   function standard_name(this) result(std_name)
      character(:), allocatable :: std_name
      class(FieldEntry), intent(inout) :: this

      std_name = this%short_name // '.' // this%component_name
   end function standard_name

   subroutine NUOPC_has_entry(this, name, has_entry, unusable, rc)
      class(FieldEntry),        intent(inout) :: this
      character(*),                     intent(in   ) :: name
      logical,                          intent(  out) :: has_entry
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      has_entry = NUOPC_FieldDictionaryHasEntry(name, rc=status)
      VERIFY_NUOPC_(status)

      _RETURN(_SUCCESS)
   end subroutine NUOPC_has_entry

   subroutine NUOPC_add_entry(this, name, units, unusable, rc)
      class(FieldEntry),                intent(inout) :: this
      character(*),                     intent(in   ) :: name
      character(*),                     intent(in   ) :: units
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      call NUOPC_FieldDictionaryAddEntry(name, units, rc=status)
      VERIFY_NUOPC_(status)

      _RETURN(_SUCCESS)
   end subroutine NUOPC_add_entry

   subroutine register(this, unusable, rc)
      class(FieldEntry),                intent(inout) :: this
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      logical                   :: has_entry
      character(:), allocatable :: standard_name

      integer :: status

      _UNUSED_DUMMY(unusable)

      standard_name = this%standard_name()

      call this%NUOPC_has_entry(standard_name, has_entry, __RC__)

      if (.not. has_entry) then
         call this%NUOPC_add_entry(standard_name, this%units, __RC__)
      end if

      _RETURN(_SUCCESS)
   end subroutine register

   subroutine NUOPC_advert(this, state, standard_name, unusable,&
         TransferOfferGeomObject, SharePolicyField, SharePolicyGeomObject, rc)
      class(FieldEntry),                intent(inout) :: this
      type(ESMF_State),                 intent(inout) :: state
      character(*),                     intent(in   ) :: standard_name
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      character(*),           optional, intent(in   ) :: TransferOfferGeomObject
      character(*),           optional, intent(in   ) :: SharePolicyField
      character(*),           optional, intent(in   ) :: SharePolicyGeomObject
      integer,                optional, intent(  out) :: rc

      integer :: status

      call NUOPC_Advertise(state, standard_name, &
         TransferOfferGeomObject=TransferOfferGeomObject, &
         SharePolicyField=SharePolicyField, &
         SharePolicyGeomObject=SharePolicyGeomObject, &
         __RC__)
      VERIFY_NUOPC_(status)

      _RETURN(_SUCCESS)
   end subroutine NUOPC_advert

   subroutine advertise(this, state, unusable,&
         TransferOfferGeomObject, SharePolicyField, SharePolicyGeomObject, rc)
      class(FieldEntry),                intent(inout) :: this
      type(ESMF_State),                 intent(inout) :: state
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      character(*),           optional, intent(in   ) :: TransferOfferGeomObject
      character(*),           optional, intent(in   ) :: SharePolicyField
      character(*),           optional, intent(in   ) :: SharePolicyGeomObject
      integer,                optional, intent(  out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      call this%register(__RC__)

      call this%NUOPC_advert(state, this%standard_name(),&
         TransferOfferGeomObject=TransferOfferGeomObject, &
         SharePolicyField=SharePolicyField, &
         SharePolicyGeomObject=SharePolicyGeomObject, &
         __RC__)

      _RETURN(_SUCCESS)
   end subroutine advertise
end module FieldEntryMod
