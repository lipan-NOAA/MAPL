#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"

module MAPL_OpenMP_Support
    use ESMF
    use MAPL_BaseMod
    use MAPL_ExceptionHandling

    implicit NONE

    type :: Interval
        integer :: min
        integer :: max
    end type Interval

    interface  make_mini_grids
        module procedure make_mini_grids_from_num_grids
        module procedure make_mini_grids_from_bounds
    end interface make_mini_grids
    
    CONTAINS 

    function make_mini_grids_from_num_grids(primary_grid, num_grids, rc) result(mini_grids)
        type(ESMF_Grid), allocatable :: mini_grids(:)
        type(ESMF_Grid), intent(in) :: primary_grid
        integer, intent(in) :: num_grids
        integer, optional, intent(out) :: rc
        integer :: local_count(3)
        integer :: status
        type(Interval) :: bounds(:)
        
        call MAPL_GridGet(primary_grid,localcellcountPerDim=local_count, __RC__)
        bounds = FindBounds(local_count(2), num_grids)
        mini_grids = make_mini_grids(primary_grid, bounds, __RC__)
    end function make_mini_grids_from_num_grids

    function make_mini_grids_from_bounds(primary_grid, bounds, rc) result(mini_grids)
        type(ESMF_Grid), allocatable :: mini_grids(:)
        type(ESMF_Grid), intent(in) :: primary_grid
        type(Interval), intent(in) :: bounds(:)
        integer, optional, intent(out) :: rc
        integer :: local_count(3)
        integer :: status
        integer :: petMap(1,1,1)
        integer :: myPet
        type(ESMF_VM) :: vm

        ! allocate(minigrids(something))
        call MAPL_GridGet(primary_grid,localcellcountPerDim=local_count, __RC__)
        call ESMF_VMGetCurrent(vm, __RC__)
        call ESMF_VMGet(vm, localPET=myPET, __RC__)

        petMap(1,1,1) = myPet
        do i = 1, numGrids
            section = bounds(i)%max - bounds(i)%min + 1
            xy_grid(i) = ESMF_GridCreateNoPeriDim( &
                  countsPerDEDim1 = [local_count(1)], &
                  countsPerDEDim2 = [section], &
                  indexFlag=ESMF_INDEX_DELOCAL, &
                  coordDep1=[1,2], &
                  coordDep2=[1,2], &
                  coordSys=ESMF_COORDSYS_SPH_RAD, &
                  petMap = petMap, &
                  __RC__)
            call ESMF_GridAddCoord(grid=xy_grid(i), staggerloc=ESMF_STAGGERLOC_CENTER, __RC__)
         end do
        
    end function make_mini_grids_from_bounds

    subroutine FindBounds(yDim, numGrids, bounds)
        implicit NONE
        integer :: yDim, numGrids, i, step, count, numOfFirstSize, numOfSecondSize, firstSize, secondSize
        integer, allocatable :: bounds(:,:)
        allocate(bounds(numGrids,2)) 

        ! if the size of each grid is the same
        if (modulo(yDim, numGrids) == 0) then
            step = yDim/numGrids
            count = 1
            ! go from 1-yDim incrementing by step size
            do i = 1, yDim, step
            bounds(count,1) = i
            bounds(count,2) = i + step - 1
            count = count + 1
            end do
        ! if at least one grid is a different size
        else 
            firstSize = yDim/numGrids 
            numOfSecondSize = modulo(yDim, numGrids)
            numOfFirstSize = numGrids - numOfSecondSize
            secondSize = (yDim - firstSize * numOfFirstSize) / numOfSecondSize
            
            count = 1
            do i = 1, numOfFirstSize * firstSize, firstSize 
            bounds(count,1) = i
            bounds(count,2) = i + firstSize - 1
            count = count + 1
            end do

            do i = numOfFirstSize * firstSize + 1, yDim, secondSize
            bounds(count,1) = i   
            bounds(count,2) = i + secondSize - 1
            count = count + 1
            end do
        end if
    end subroutine

    subroutine SubsetArray(input_array, output_array, bounds)
        implicit NONE
        real(kind=ESMF_KIND_R8), pointer :: input_array(:,:)
        real(kind=ESMF_KIND_R8), pointer :: output_array(:,:)
        integer :: bounds(2)
        
        allocate(output_array(size(input_array,1), bounds(2)-bounds(1)+1))
        output_array(:, :) = input_array(:,bounds(1):bounds(2)) 
     end subroutine

end module MAPL_OpenMP_Support 
