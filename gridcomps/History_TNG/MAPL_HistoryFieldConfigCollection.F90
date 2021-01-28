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

   type, extends(HistoryFieldConfigBase) :: HistoryFieldConfigCollection
      private
      character(:), allocatable :: alias_name
   contains
      procedure :: name
   end type HistoryFieldConfigCollection
contains
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
