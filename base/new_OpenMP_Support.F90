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
      integer :: local_count(3)
      integer :: status
      integer :: petMap(1,1,1)
      integer :: myPet, section
      type(ESMF_VM) :: vm
      real(kind=ESMF_KIND_R8), pointer :: new_lats(:,:), new_lons(:,:)
      real(kind=ESMF_KIND_R8), pointer :: lats(:,:), lons(:,:)

      call MAPL_GridGet(grid,localcellcountPerDim=local_count, __RC__)
      call ESMF_VMGetCurrent(vm, __RC__)
      call ESMF_VMGet(vm, localPET=myPET, __RC__)

      petMap(1,1,1) = myPet
      section = bounds%max - bounds%min + 1
      subgrid  = ESMF_GridCreateNoPeriDim( &
           countsPerDEDim1 = [local_count(1)], &
           countsPerDEDim2 = [section], &
           indexFlag=ESMF_INDEX_DELOCAL, &
           coordDep1=[1,2], &
           coordDep2=[1,2], &
           coordSys=ESMF_COORDSYS_SPH_RAD, &
           petMap = petMap, &
           __RC__)
      call ESMF_GridAddCoord(grid=subgrid, staggerloc=ESMF_STAGGERLOC_CORNER, __RC__)

      call ESMF_GridGetCoord(grid=grid, coordDim=1, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CORNER, farrayPtr=lons, __RC__)
      call ESMF_GridGetCoord(grid=grid, coordDim=2, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CORNER, farrayPtr=lats, __RC__)
      
      
      call ESMF_GridGetCoord(grid=subgrid, coordDim=1, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CORNER, &
           farrayPtr=new_lons, __RC__)
      new_lons = subset_array(lons, bounds)

      call ESMF_GridGetCoord(grid=subgrid, coordDim=2, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CORNER, &
           farrayPtr=new_lats, __RC__)
      new_lats = subset_array(lats, bounds)
      

      _RETURN(0)
   end function get_subgrid
   
   function get_subgrids(this, grid, unusable, rc) result(subgrids)
      type(ESMF_Grid), allocatable :: subgrids(:)
      class(SubGridBuilder), intent(in) :: this
      type(ESMF_Grid), intent(in) :: grid
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      type(Interval), allocatable :: bounds(:)
      integer :: local_count(3)
      integer :: status
      integer :: petMap(1,1,1)
      integer :: myPet, i, section
      type(ESMF_VM) :: vm
      real(kind=ESMF_KIND_R8), pointer :: new_lats(:,:), new_lons(:,:)
      real(kind=ESMF_KIND_R8), pointer :: lats(:,:), lons(:,:)

      call MAPL_GridGet(grid,localcellcountPerDim=local_count, __RC__)
      bounds = find_bounds(local_count(2), this%num_subgrids)   

      allocate(subgrids(size(bounds)))
      call ESMF_VMGetCurrent(vm, __RC__)
      call ESMF_VMGet(vm, localPET=myPET, __RC__)

      petMap(1,1,1) = myPet
      do i = 1, size(bounds)
         section = bounds(i)%max - bounds(i)%min + 1
         subgrids(i) = ESMF_GridCreateNoPeriDim( &
              countsPerDEDim1 = [local_count(1)], &
              countsPerDEDim2 = [section], &
              indexFlag=ESMF_INDEX_DELOCAL, &
              coordDep1=[1,2], &
              coordDep2=[1,2], &
              coordSys=ESMF_COORDSYS_SPH_RAD, &
              petMap = petMap, &
              __RC__)
         call ESMF_GridAddCoord(grid=subgrids(i), staggerloc=ESMF_STAGGERLOC_CENTER, __RC__)
      end do

      call ESMF_GridGetCoord(grid=grid, coordDim=1, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=lons, __RC__)
      call ESMF_GridGetCoord(grid=grid, coordDim=2, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=lats, __RC__)

      do i = 1, size(bounds)
         call ESMF_GridGetCoord(grid=subgrids(i), coordDim=1, localDE=0, &
              staggerloc=ESMF_STAGGERLOC_CENTER, &
              farrayPtr=new_lons, __RC__)
         new_lons = subset_array(lons, bounds(i))
      end do

      do i = 1, size(bounds) 
         call ESMF_GridGetCoord(grid=subgrids(i), coordDim=2, localDE=0, &
              staggerloc=ESMF_STAGGERLOC_CENTER, &
              farrayPtr=new_lats, __RC__)
         new_lats = subset_array(lats, bounds(i))
      end do
   
      _RETURN(0)
   end function get_subgrids

   function find_bounds(yDim, num_grids) result(bounds)
        integer, intent(in) :: yDim
        integer, intent(in) :: num_grids
        type(Interval), allocatable :: bounds(:) 
        integer :: i, step
        integer :: count, numOfFirstSize, numOfSecondSize, firstSize, secondSize
        allocate(bounds(num_grids))

        ! if the size of each grid is the same
        if (modulo(yDim, num_grids) == 0) then
            step = yDim/num_grids
            count = 1
            ! go from 1-yDim incrementing by step size
            do i = 1, yDim, step
            bounds(count)%min = i
            bounds(count)%max = i + step - 1
            count = count + 1
            end do
        ! if at least one grid is a different size
        else 
            firstSize = yDim/num_grids 
            numOfSecondSize = modulo(yDim, num_grids)
            numOfFirstSize = num_grids - numOfSecondSize
            secondSize = (yDim - firstSize * numOfFirstSize) / numOfSecondSize
            
            count = 1
            do i = 1, numOfFirstSize * firstSize, firstSize 
            bounds(count)%min = i
            bounds(count)%max = i + firstSize - 1
            count = count + 1
            end do

            do i = numOfFirstSize * firstSize + 1, yDim, secondSize
            bounds(count)%min = i   
            bounds(count)%max = i + secondSize - 1
            count = count + 1
            end do
        end if
    end function find_bounds

    function subset_array(input_array, bounds) result(output_array)
        real(kind=ESMF_KIND_R8), pointer, intent(in) :: input_array(:,:)
        type(Interval), intent(in) :: bounds
        real(kind=ESMF_KIND_R8), pointer :: output_array(:,:)

        allocate(output_array(size(input_array,1), bounds%max - bounds%min + 1))
        output_array(:,:) = input_array(:,bounds%min:bounds%max) 

    end function subset_array
   
end module mapl_new_OpenMP_Support
