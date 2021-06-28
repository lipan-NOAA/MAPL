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

    interface  make_subgrids
        module procedure make_subgrids_from_num_grids
        module procedure make_subgrids_from_bounds
    end interface make_subgrids

    interface  make_subfields
        module procedure make_subfields_from_num_grids
        module procedure make_subfields_from_bounds
    end interface make_subfields
    
    CONTAINS 

    function make_subgrids_from_num_grids(primary_grid, num_grids, rc) result(subgrids)
        type(ESMF_Grid), allocatable :: subgrids(:)
        type(ESMF_Grid), intent(in) :: primary_grid
        integer, intent(in) :: num_grids
        integer, optional, intent(out) :: rc
        integer :: local_count(3)
        integer :: status
        type(Interval), allocatable :: bounds(:)
        
        call MAPL_GridGet(primary_grid,localcellcountPerDim=local_count, __RC__)
        bounds = find_bounds(local_count(2), num_grids)
        subgrids = make_subgrids(primary_grid, bounds, __RC__)
    end function make_subgrids_from_num_grids

    function make_subgrids_from_bounds(primary_grid, bounds, rc) result(subgrids)
        type(ESMF_Grid), allocatable :: subgrids(:)
        type(ESMF_Grid), intent(in) :: primary_grid
        type(Interval), intent(in) :: bounds(:)
        integer, optional, intent(out) :: rc
        integer :: local_count(3)
        integer :: status
        integer :: petMap(1,1,1)
        integer :: myPet, i, section
        type(ESMF_VM) :: vm
        real(kind=ESMF_KIND_R8), pointer :: new_lats(:,:), new_lons(:,:)
        real(kind=ESMF_KIND_R8), pointer :: lats(:,:), lons(:,:)

        allocate(subgrids(size(bounds)))
        call MAPL_GridGet(primary_grid,localcellcountPerDim=local_count, __RC__)
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

         call ESMF_GridGetCoord(grid=primary_grid, coordDim=1, localDE=0, &
            staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=lons, __RC__)
         call ESMF_GridGetCoord(grid=primary_grid, coordDim=2, localDE=0, &
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
        
    end function make_subgrids_from_bounds
        

    function make_subfields_from_num_grids(primary_field, primary_grid, subgrids, num_grids, rc) result(subfields)
        type(ESMF_Field), allocatable :: subfields(:)
        type(ESMF_Field), intent(in) :: primary_field
        type(ESMF_Grid), intent(in) :: primary_grid
        type(ESMF_Grid), intent(in) :: subgrids(:)
        integer, intent(in) :: num_grids
        integer, optional, intent(out) :: rc
        integer :: status
        integer :: local_count(3)
        type(Interval), allocatable :: bounds(:)
    
        call MAPL_GridGet(primary_grid,localcellcountPerDim=local_count, __RC__)
        bounds = find_bounds(local_count(2), num_grids)
        subfields = make_subfields(primary_field, subgrids, bounds, __RC__)
    end function make_subfields_from_num_grids

    function make_subfields_from_bounds(primary_field, subgrids, bounds, rc) result(subfields)
        type(ESMF_Field), allocatable :: subfields(:)
        type(ESMF_Field), intent(in) :: primary_field
        type(ESMF_Grid), intent(in) :: subgrids(:)
        type(Interval), intent(in) :: bounds(:)
        integer, optional, intent(out) :: rc
        integer :: status, i
        real(kind=ESMF_KIND_R4), pointer:: old_ptr(:,:)
        real(kind=ESMF_KIND_R4), pointer :: new_ptr(:,:)
        character(len=20) :: name

        call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr, rc=status)

        allocate(subfields(size(bounds)))
        name = 'new field'
        do i = 1, size(bounds)
            subfields(i) = ESMF_FieldEmptyCreate(name=name, __RC__)
            call ESMF_FieldEmptySet(field=subfields(i), &
                grid=subgrids(i), &
                staggerloc=ESMF_STAGGERLOC_CENTER, &
                __RC__) 
            call ESMF_FieldEmptyComplete(field=subfields(i), &
                typekind=ESMF_TYPEKIND_R4, __RC__)
            call ESMF_FieldGet(field=subfields(i), localDe=0, farrayPtr=new_ptr, __RC__)
            new_ptr => old_ptr(:,bounds(i)%min:bounds(i)%max) 
        end do

    end function make_subfields_from_bounds

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
    end function

    function subset_array(input_array, bounds) result(output_array)
        real(kind=ESMF_KIND_R8), pointer, intent(in) :: input_array(:,:)
        type(Interval), intent(in) :: bounds
        real(kind=ESMF_KIND_R8), pointer :: output_array(:,:)

        allocate(output_array(size(input_array,1), bounds%max - bounds%min + 1))
        output_array(:,:) = input_array(:,bounds%min:bounds%max) 

    end function

end module MAPL_OpenMP_Support 
