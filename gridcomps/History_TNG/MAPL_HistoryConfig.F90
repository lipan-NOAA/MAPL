module StringAltSetMod
#include "types/deferredLengthString.inc"
#define _set StringAltSet
#define _iterator StringAltSetIterator

#include "templates/altSet.inc"

#undef _iterator
#undef _set
#undef _type
end module StringAltSetMod

module MAPL_HistoryFieldConfigMod
#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"
#include "unused_dummy.H"

   use ESMF
   use NUOPC
   use MAPL_ExceptionHandling

   use StringAltSetMod
   use yaFyaml

   implicit None
   private

   public HistoryFieldConfig

   character(*), parameter :: field_key = 'field'
   character(*), parameter :: component_key = 'component'
   character(*), parameter :: name_key = 'name'

   type :: HistoryFieldConfig
      private
      character(:), allocatable :: short_name
      character(:), allocatable :: component_name
      type(StringAltSet)        :: alternate_names

   contains
      procedure :: standard_name

      procedure :: has_alternate_name
      procedure :: add_alternate_name
      procedure :: create_alternate_name
      procedure :: alternate_name

      procedure :: name

      procedure :: register
   end type HistoryFieldConfig

contains
   function standard_name(this) result(std_name)
      character(:), allocatable :: std_name
      class(HistoryFieldConfig), intent(in) :: this

      std_name = this%short_name // '.' // this%component_name
   end function standard_name

   logical function has_alternate_name(this)
      class(HistoryFieldConfig), intent(in) :: this

      has_alternate_name = (this%alternate_names%size() == 1)
   end function has_alternate_name

   subroutine add_alternate_name(this, name)
      class(HistoryFieldConfig), intent(inout) :: this
      character(*),              intent(in   ) :: name

      call this%alternate_names%insert(name)
   end subroutine add_alternate_name

   function create_alternate_name(this, name, rc) result(alt_name)
      character(:), allocatable :: alt_name
      class(HistoryFieldConfig), intent(in   ) :: this
      character(*),              intent(in   ) :: name
      integer, optional,         intent(  out) :: rc

      character(:), allocatable :: std_name
      integer :: status

      if (this%alternate_names%count(name) > 0) then
         std_name = this%standard_name()

         alt_name = name // '.' // std_name
         _RETURN(_SUCCESS)
      else
         _RETURN(_FAILURE)
      end if

   end function create_alternate_name

   function alternate_name(this, rc) result(alt_name)
      character(:), allocatable :: alt_name
      class(HistoryFieldConfig), intent(in   ) :: this
      integer, optional,         intent(  out) :: rc

      type(StringAltSetIterator) :: iter
      character(:), allocatable  :: name
      integer :: status

      iter = this%alternate_names%begin()
      name = iter%value()

      alt_name = this%create_alternate_name(name, __RC__)

      _RETURN(_SUCCESS)
   end function alternate_name

   function name(this, rc) result(field_name)
      character(:), allocatable :: field_name
      class(HistoryFieldConfig), intent(in   ) :: this
      integer, optional,         intent(  out) :: rc

      integer :: status

      if (this%has_alternate_name()) then
         field_name = this%alternate_name(__RC__)
      else
         field_name = this%standard_name()
      end if

      _RETURN(_SUCCESS)
   end function name

   subroutine register_name(name, rc)
      character(*),      intent(in   ) :: name
      integer, optional, intent(  out) :: rc

      logical :: has_entry
      integer :: status

      has_entry = NUOPC_FieldDictionaryHasEntry(name, rc=status)
      VERIFY_NUOPC_(status)

      if (.not. has_entry) then
         call NUOPC_FieldDictionaryAddEntry(name, "1", rc=status)
         VERIFY_NUOPC_(status)
      end if

      _RETURN(_SUCCESS)
   end subroutine register_name

   subroutine register_syno(name1, name2, rc)
      character(*),      intent(in   ) :: name1
      character(*),      intent(in   ) :: name2
      integer, optional, intent(  out) :: rc

      logical :: are_syno
      integer :: status

      are_syno = NUOPC_FieldDictionaryMatchSyno(name1, name2, rc=status)
      VERIFY_NUOPC_(status)

      if (.not. are_syno) then
         call NUOPC_FieldDictionarySetSyno([name1, name2], rc=status)
         VERIFY_NUOPC_(status)
      end if

      _RETURN(_SUCCESS)
   end subroutine register_syno

   subroutine register(this, rc)
      class(HistoryFieldConfig), intent(in   ) :: this
      integer, optional,         intent(  out) :: rc

      character(:), allocatable  :: std_name
      character(:), allocatable  :: alt_name
      type(StringAltSetIterator) :: iter
      integer :: status

      std_name = this%standard_name()
      call register_name(std_name, __RC__)

      iter = this%alternate_names%begin()

      do while (iter /= this%alternate_names%end())
         alt_name = this%create_alternate_name(iter%value(), __RC__)

         call register_name(alt_name, __RC__)
         call register_syno(std_name, alt_name, __RC__)

         call iter%next()
      end do

      _RETURN(_SUCCESS)
   end subroutine register

   subroutine read_config(this, config)
      class(HistoryFieldConfig), intent(inout) :: this
      type(Configuration),       intent(in   ) :: config

      type(ConfigurationIterator) :: iter
      character(:), pointer       :: key
      character(:), allocatable   :: name

      iter = config%begin()
      do while(iter /= config%end())
         key => iter%key()

         select case (key)
         case (field_key)
            this%short_name = iter%value()
         case (component_key)
            this%component_name = iter%value()
         case (name_key)
            name = iter%value()
            call this%add_alternate_name(name)
         end select

         call iter%next()
      end do
   end subroutine read_config
end module MAPL_HistoryFieldConfigMod

module MAPL_HistoryConfigMod
   use ESMF
   use NUOPC

   implicit None
   private
end module MAPL_HistoryConfigMod
