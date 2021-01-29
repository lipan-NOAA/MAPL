#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"
#include "unused_dummy.H"

module MAPL_AbstractFieldEntry
   use ESMF
   use NUOPC
   use MAPL_ExceptionHandling

   implicit none
   private

   public :: AbstractFieldEntry

   type, abstract :: AbstractFieldEntry
      private
      character(:), allocatable :: short_name
      character(:), allocatable :: component_name
   contains
      procedure, non_overridable :: base_initialize
      procedure :: initialize

      procedure :: get_short_name
      procedure :: get_component_name

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
   end interface

contains
   subroutine base_initialize(this, short_name, component_name)
      class(AbstractFieldEntry), intent(  out) :: this
      character(*),              intent(in   ) :: short_name
      character(*),              intent(in   ) :: component_name

      this%short_name = short_name
      this%component_name = component_name
   end subroutine base_initialize

   subroutine initialize(this, short_name, component_name, alias_name)
      class(AbstractFieldEntry), intent(  out) :: this
      character(*),              intent(in   ) :: short_name
      character(*),              intent(in   ) :: component_name
      character(*), optional,    intent(in   ) :: alias_name

      call this%base_initialize(short_name, component_name)

      _UNUSED_DUMMY(alias_name)
   end subroutine initialize

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

   subroutine NUOPC_add_entry(this, name, rc)
      class(AbstractFieldEntry), intent(inout) :: this
      character(*),              intent(in   ) :: name
      integer, optional,         intent(  out) :: rc

      integer :: status

      call NUOPC_FieldDictionaryAddEntry(name, "1", rc=status)
      VERIFY_NUOPC_(status)

      _RETURN(_SUCCESS)
   end subroutine NUOPC_add_entry

   subroutine register_name(this, name, rc)
      class(AbstractFieldEntry), intent(inout) :: this
      character(*),              intent(in   ) :: name
      integer, optional,         intent(  out) :: rc

      logical :: has_entry
      integer :: status

      call this%NUOPC_has_entry(name, has_entry, __RC__)

      if (.not. has_entry) then
         call this%NUOPC_add_entry(name, __RC__)
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

      integer :: status

      standard_name = this%standard_name()
      call this%register_name(standard_name, __RC__)

      name = this%name()
      if (standard_name /= name) then
         call this%register_name(name, __RC__)
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

   subroutine advertise(this, state, &
         TransferOfferGeomObject, SharePolicyField, SharePolicyGeomObject, rc)
      class(AbstractFieldEntry), intent(inout) :: this
      type(ESMF_State),          intent(inout) :: state
      character(*), optional,    intent(in   ) :: TransferOfferGeomObject
      character(*), optional,    intent(in   ) :: SharePolicyField
      character(*), optional,    intent(in   ) :: SharePolicyGeomObject
      integer,      optional,    intent(  out) :: rc

      integer :: status

      call this%register(__RC__)

      call this%NUOPC_advert(state, this%name(),&
         TransferOfferGeomObject=TransferOfferGeomObject, &
         SharePolicyField=SharePolicyField, &
         SharePolicyGeomObject=SharePolicyGeomObject, &
         __RC__)

      _RETURN(_SUCCESS)
   end subroutine advertise
end module MAPL_AbstractFieldEntry
