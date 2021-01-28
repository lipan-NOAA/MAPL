#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"
#include "unused_dummy.H"

module MAPL_HistoryFieldConfigCollection
   use ESMF
   use NUOPC
   use MAPL_ExceptionHandling
   use MAPL_HistoryFieldConfigBase

   implicit none
   private

   public HistoryFieldConfigCollection

   type, extends(HistoryFieldConfigBase) :: HistoryFieldConfigCollection
      private
      character(:), allocatable :: alias_name
   contains
      procedure :: initialize
      procedure :: get_alias_name

      procedure :: name
   end type HistoryFieldConfigCollection
contains
   subroutine initialize(this, short_name, component_name, alias_name)
      class(HistoryFieldConfigCollection), intent(  out) :: this
      character(*),                        intent(in   ) :: short_name
      character(*),                        intent(in   ) :: component_name
      character(*), optional,              intent(in   ) :: alias_name

      call this%base_initialize(short_name, component_name)

      if (present(alias_name)) this%alias_name = alias_name
   end subroutine initialize

   function get_alias_name(this) result(alias)
      character(:), allocatable :: alias
      class(HistoryFieldConfigCollection), intent(in) :: this

      alias = this%alias_name
   end function get_alias_name

   function name(this) result(field_name)
      character(:), allocatable :: field_name
      class(HistoryFieldConfigCollection), intent(inout) :: this

      character(:), allocatable :: standard_name

      standard_name = this%standard_name()

      if (allocated(this%alias_name)) then
         field_name = this%alias_name // '.' // standard_name
      else
         field_name = standard_name
      end if
   end function name
end module MAPL_HistoryFieldConfigCollection
