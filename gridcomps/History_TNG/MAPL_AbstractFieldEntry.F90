#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module MAPL_AbstractFieldEntry
   use ESMF
   use NUOPC
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod

   implicit none
   private

   public :: AbstractFieldEntry
   public :: default_units

   character(*), parameter :: default_units = '1'

   type, abstract :: AbstractFieldEntry
      private
      character(:), allocatable :: short_name
      character(:), allocatable :: component_name
      character(:), allocatable :: units
   contains
      procedure, non_overridable :: base_initialize
      procedure(i_initialize), deferred :: initialize

      procedure :: get_short_name
      procedure :: get_component_name
      procedure :: get_units

      procedure :: set_units

      procedure :: standard_name
      procedure(i_name), deferred :: name

      procedure :: NUOPC_has_entry
      procedure :: NUOPC_add_entry
      procedure :: register_name

      procedure :: NUOPC_are_syno
      procedure :: NUOPC_add_syno
      procedure :: register_syno

      procedure :: register

      procedure :: NUOPC_advert
      procedure :: advertise
   end type

   abstract interface
      function i_name(this) result(field_name)
         import AbstractFieldEntry
            character(:), allocatable :: field_name
            class(AbstractFieldEntry), intent(inout) :: this
      end function

      subroutine i_initialize(this, short_name, component_name, unusable, units, alias_name)
         use MAPL_KeywordEnforcerMod
         import AbstractFieldEntry
            class(AbstractFieldEntry),        intent(  out) :: this
            character(*),                     intent(in   ) :: short_name
            character(*),                     intent(in   ) :: component_name
            class(KeywordEnforcer), optional, intent(in   ) :: unusable
            character(*),           optional, intent(in   ) :: units
            character(*),           optional, intent(in   ) :: alias_name
      end subroutine i_initialize
   end interface

contains
   subroutine base_initialize(this, short_name, component_name, unusable, units)
      class(AbstractFieldEntry),        intent(  out) :: this
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
   end subroutine base_initialize

   function get_short_name(this) result(short_name)
      character(:), allocatable :: short_name
      class(AbstractFieldEntry), intent(in) :: this

      short_name = this%short_name
   end function get_short_name

   function get_component_name(this) result(component_name)
      character(:), allocatable :: component_name
      class(AbstractFieldEntry), intent(in) :: this

      component_name = this%component_name
   end function get_component_name

   function get_units(this) result(units)
      character(:), allocatable :: units
      class(AbstractFieldEntry), intent(in) :: this

      units = this%units
   end function get_units

   subroutine set_units(this, units, rc)
      class(AbstractFieldEntry), intent(inout) :: this
      character(*),              intent(in   ) :: units
      integer, optional,         intent(  out) :: rc

      integer :: status = 0

      if (this%units /= default_units) then
         status = 1
      else
         this%units = units
      end if

      _RETURN(status)
   end subroutine set_units

   function standard_name(this) result(std_name)
      character(:), allocatable :: std_name
      class(AbstractFieldEntry), intent(inout) :: this

      std_name = this%short_name // '.' // this%component_name
   end function standard_name

   subroutine NUOPC_has_entry(this, name, has_entry, rc)
      class(AbstractFieldEntry), intent(inout) :: this
      character(*),              intent(in   ) :: name
      logical,                   intent(  out) :: has_entry
      integer, optional,         intent(  out) :: rc

      integer :: status

      has_entry = NUOPC_FieldDictionaryHasEntry(name, rc=status)
      VERIFY_NUOPC_(status)

      _RETURN(_SUCCESS)
   end subroutine NUOPC_has_entry

   subroutine NUOPC_add_entry(this, name, units, rc)
      class(AbstractFieldEntry), intent(inout) :: this
      character(*),              intent(in   ) :: name
      character(*),              intent(in   ) :: units
      integer, optional,         intent(  out) :: rc

      integer :: status

      call NUOPC_FieldDictionaryAddEntry(name, units, rc=status)
      VERIFY_NUOPC_(status)

      _RETURN(_SUCCESS)
   end subroutine NUOPC_add_entry

   subroutine register_name(this, name, units, rc)
      class(AbstractFieldEntry), intent(inout) :: this
      character(*),              intent(in   ) :: name
      character(*),              intent(in   ) :: units
      integer, optional,         intent(  out) :: rc

      logical :: has_entry
      integer :: status

      call this%NUOPC_has_entry(name, has_entry, __RC__)

      if (.not. has_entry) then
         call this%NUOPC_add_entry(name, units, __RC__)
      end if

      _RETURN(_SUCCESS)
   end subroutine register_name

   subroutine NUOPC_are_syno(this, name1, name2, are_syno, rc)
      class(AbstractFieldEntry), intent(inout) :: this
      character(*),              intent(in   ) :: name1
      character(*),              intent(in   ) :: name2
      logical,                   intent(  out) :: are_syno
      integer, optional,         intent(  out) :: rc

      integer :: status

      are_syno = NUOPC_FieldDictionaryMatchSyno(name1, name2, rc=status)
      VERIFY_NUOPC_(status)

      _RETURN(_SUCCESS)
   end subroutine NUOPC_are_syno

   subroutine NUOPC_add_syno(this, name1, name2, rc)
      class(AbstractFieldEntry), intent(inout) :: this
      character(*),              intent(in   ) :: name1
      character(*),              intent(in   ) :: name2
      integer, optional,         intent(  out) :: rc

      integer :: status

      call NUOPC_FieldDictionarySetSyno([name1, name2], rc=status)
      VERIFY_NUOPC_(status)

      _RETURN(_SUCCESS)
   end subroutine NUOPC_add_syno

   subroutine register_syno(this, name1, name2, rc)
      class(AbstractFieldEntry), intent(inout) :: this
      character(*),              intent(in   ) :: name1
      character(*),              intent(in   ) :: name2
      integer, optional,         intent(  out) :: rc

      logical :: are_syno
      integer :: status

      call this%NUOPC_are_syno(name1, name2, are_syno, __RC__)

      if (.not. are_syno) then
         call this%NUOPC_add_syno(name1, name2, __RC__)
      end if

      _RETURN(_SUCCESS)
   end subroutine register_syno

   subroutine register(this, rc)
      class(AbstractFieldEntry), intent(inout) :: this
      integer, optional,         intent(  out) :: rc

      character(:), allocatable :: standard_name
      character(:), allocatable :: name
      character(:), allocatable :: units

      integer :: status

      units = this%get_units()

      standard_name = this%standard_name()
      call this%register_name(standard_name, units, __RC__)

      name = this%name()
      if (standard_name /= name) then
         call this%register_name(name, units, __RC__)
         call this%register_syno(standard_name, name, __RC__)
      end if

      _RETURN(_SUCCESS)
   end subroutine register

   subroutine NUOPC_advert(this, state, standard_name,&
         TransferOfferGeomObject, SharePolicyField, SharePolicyGeomObject, rc)
      class(AbstractFieldEntry), intent(inout) :: this
      type(ESMF_State),          intent(inout) :: state
      character(*),              intent(in   ) :: standard_name
      character(*), optional,    intent(in   ) :: TransferOfferGeomObject
      character(*), optional,    intent(in   ) :: SharePolicyField
      character(*), optional,    intent(in   ) :: SharePolicyGeomObject
      integer,      optional,    intent(  out) :: rc

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
      class(AbstractFieldEntry),        intent(inout) :: this
      type(ESMF_State),                 intent(inout) :: state
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      character(*),           optional, intent(in   ) :: TransferOfferGeomObject
      character(*),           optional, intent(in   ) :: SharePolicyField
      character(*),           optional, intent(in   ) :: SharePolicyGeomObject
      integer,                optional, intent(  out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      call this%register(__RC__)

      call this%NUOPC_advert(state, this%name(),&
         TransferOfferGeomObject=TransferOfferGeomObject, &
         SharePolicyField=SharePolicyField, &
         SharePolicyGeomObject=SharePolicyGeomObject, &
         __RC__)

      _RETURN(_SUCCESS)
   end subroutine advertise
end module MAPL_AbstractFieldEntry
