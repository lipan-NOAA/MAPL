#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"
#include "unused_dummy.H"

module MAPL_HistoryFieldConfigBase
   use ESMF
   use NUOPC
   use MAPL_ExceptionHandling

   implicit none
   private

   public :: HistoryFieldConfigBase

   type :: HistoryFieldConfigBase
      private
      character(:), allocatable :: short_name
      character(:), allocatable :: component_name
   contains
      procedure :: initialize
      procedure :: standard_name

      procedure :: NUOPC_has_entry
      procedure :: NUOPC_add_entry
      procedure :: register_name
   end type
contains
   subroutine initialize(this, short_name, component_name)
      class(HistoryFieldConfigBase), intent(  out) :: this
      character(*),                  intent(in   ) :: short_name
      character(*),                  intent(in   ) :: component_name

      this%short_name = short_name
      this%component_name = component_name
   end subroutine initialize

   function standard_name(this) result(std_name)
      character(:), allocatable :: std_name
      class(HistoryFieldConfigBase), intent(in) :: this

      std_name = this%short_name // '.' // this%component_name
   end function standard_name

   subroutine NUOPC_has_entry(this, name, has_entry, rc)
      class(HistoryFieldConfigBase), intent(inout) :: this
      character(*),                  intent(in   ) :: name
      logical,                       intent(  out) :: has_entry
      integer, optional,             intent(  out) :: rc

      integer :: status

      has_entry = NUOPC_FieldDictionaryHasEntry(name, rc=status)
      VERIFY_NUOPC_(status)

      _RETURN(_SUCCESS)
   end subroutine NUOPC_has_entry

   subroutine NUOPC_add_entry(this, name, rc)
      class(HistoryFieldConfigBase), intent(inout) :: this
      character(*),                  intent(in   ) :: name
      integer, optional,             intent(  out) :: rc

      integer :: status

      call NUOPC_FieldDictionaryAddEntry(name, "1", rc=status)
      VERIFY_NUOPC_(status)

      _RETURN(_SUCCESS)
   end subroutine NUOPC_add_entry

   subroutine register_name(this, name, rc)
      class(HistoryFieldConfigBase), intent(inout) :: this
      character(*),                  intent(in   ) :: name
      integer, optional,             intent(  out) :: rc

      logical :: has_entry
      integer :: status

      call this%NUOPC_has_entry(name, has_entry, __RC__)

      if (.not. has_entry) then
         call this%NUOPC_add_entry(name, __RC__)
      end if

      _RETURN(_SUCCESS)
   end subroutine register_name
end module MAPL_HistoryFieldConfigBase
