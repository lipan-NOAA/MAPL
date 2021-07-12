#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"

module mapl_new_OpenMP_Support
   use ESMF
   use MAPL_BaseMod
   use MAPL_ExceptionHandling
   use mapl_KeywordEnforcerMod
   
   implicit none
   private
   
   public :: Interval
   public :: SubGridBuilder
   
   type :: Interval
      integer :: min
      integer :: max
   end type Interval

   type :: SubGridBuilder
      private
      integer :: num_subgrids
   contains
      procedure :: get_subgrid
      procedure :: get_subgrids
   end type SubGridBuilder

   interface SubGridBuilder
      module procedure new_SubGridBuilder
   end interface SubGridBuilder
   
contains

   function new_SubGridBuilder(num_subgrids) result(builder)
      type(SubGridBuilder) :: builder
      integer, intent(in) :: num_subgrids
      builder%num_subgrids = num_subgrids
   end function new_SubGridBuilder
   
   function get_subgrid(this, grid, bounds, unusable, rc) result(subgrid)
      type(ESMF_Grid) :: subgrid
      class(SubGridBuilder), intent(in) :: this
      type(ESMF_Grid), intent(in) :: grid
      type(Interval), intent(in) :: bounds
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _RETURN(0)
   end function get_subgrid
   
   function get_subgrids(this, grid, unusable, rc) result(subgrids)
      type(ESMF_Grid), allocatable :: subgrids(:)
      class(SubGridBuilder), intent(in) :: this
      type(ESMF_Grid), intent(in) :: grid
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _RETURN(0)
   end function get_subgrids
   
end module mapl_new_OpenMP_Support
