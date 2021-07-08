#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"

module MAPL_OpenMP_Support
    use ESMF
    use MAPL_BaseMod
    use MAPL_ExceptionHandling
    use mapl_KeywordEnforcerMod

    implicit none
    private

    public :: Interval
    public :: make_subgrids
    public :: make_subfields
    public :: make_subFieldBundles
    public :: find_bounds
    public :: subset_array

    
    type :: Interval
        integer :: min
        integer :: max
    end type Interval

    interface  make_subgrids
        module procedure make_subgrids_from_num_grids
        module procedure make_subgrids_from_bounds
    end interface make_subgrids

    interface
       function i_make_subfields(primary_field, num_subgrids, rc) result(subfields)
           use ESMF
           implicit none
           type(ESMF_Field), allocatable :: subfields(:)
           type(ESMF_Field), intent(in) :: primary_field
           integer, intent(in) :: num_subgrids
           integer, optional, intent(out) :: rc
       end function i_make_subfields
    end interface

    interface  make_subfields
       module procedure make_subfields_from_num_grids
    end interface make_subfields

    interface make_subfieldBundles
       module procedure make_subfieldBundles_base
       module procedure make_subfieldBundles_ordinary
    end interface

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
        _RETURN(ESMF_SUCCESS)
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
        
        _RETURN(ESMF_SUCCESS)
    end function make_subgrids_from_bounds
        

   ! function make_subfields_from_num_grids(primary_field, primary_grid, subgrids, num_grids, rc) result(subfields)
   !     type(ESMF_Field), allocatable :: subfields(:)
   !     type(ESMF_Field), intent(in) :: primary_field
   !     type(ESMF_Grid), intent(in) :: primary_grid
   !     type(ESMF_Grid), intent(in) :: subgrids(:)
   !     integer, intent(in) :: num_grids
   !     integer, optional, intent(out) :: rc
   !     integer :: status
   !     integer :: local_count(3)
   !     type(Interval), allocatable :: bounds(:)

   !     call MAPL_GridGet(primary_grid,localcellcountPerDim=local_count, __RC__)
   !     bounds = find_bounds(local_count(2), num_grids)
   !     subfields = make_subfields(primary_field, subgrids, bounds, __RC__)

   !     _RETURN(ESMF_SUCCESS)
   ! end function make_subfields_from_num_grids

    function make_subfields_from_num_grids(primary_field, num_subgrids, rc) result(subfields)
        type(ESMF_Field), allocatable :: subfields(:)
        type(ESMF_Field), intent(in) :: primary_field
        integer, intent(in) :: num_subgrids
        integer, optional, intent(out) :: rc
        integer :: status, i
        real(kind=ESMF_KIND_R4), pointer :: old_ptr_2d_r4(:,:)
        real(kind=ESMF_KIND_R4), pointer :: new_ptr_2d_r4(:,:)
        real(kind=ESMF_KIND_R8), pointer :: old_ptr_2d_r8(:,:)
        real(kind=ESMF_KIND_R8), pointer :: new_ptr_2d_r8(:,:)
        real(kind=ESMF_KIND_R4), pointer :: old_ptr_3d_r4(:,:,:)
        real(kind=ESMF_KIND_R4), pointer :: new_ptr_3d_r4(:,:,:)
        real(kind=ESMF_KIND_R8), pointer :: old_ptr_3d_r8(:,:,:)
        real(kind=ESMF_KIND_R8), pointer :: new_ptr_3d_r8(:,:,:)
        integer(kind=ESMF_KIND_I4), pointer :: old_ptr_2d_i4(:,:)
        integer(kind=ESMF_KIND_I4), pointer :: new_ptr_2d_i4(:,:)
        integer(kind=ESMF_KIND_I4), pointer :: old_ptr_3d_i4(:,:,:)
        integer(kind=ESMF_KIND_I4), pointer :: new_ptr_3d_i4(:,:,:)
        integer(kind=ESMF_KIND_I8), pointer :: old_ptr_2d_i8(:,:)
        integer(kind=ESMF_KIND_I8), pointer :: new_ptr_2d_i8(:,:)
        integer(kind=ESMF_KIND_I8), pointer :: old_ptr_3d_i8(:,:,:)
        integer(kind=ESMF_KIND_I8), pointer :: new_ptr_3d_i8(:,:,:)
        type(ESMF_TypeKind_Flag) :: typekind
        integer :: rank
        integer :: local_count(3)
        character(len=ESMF_MAXPATHLEN) :: name 
        type(ESMF_Grid), allocatable :: subgrids(:)
        type(Interval), allocatable :: bounds(:)
        type(ESMF_Grid) :: primary_grid

        call ESMF_FieldGet(primary_field, grid=primary_grid, typekind=typekind, rank=rank, name=name,  __RC__)
        call MAPL_GridGet(primary_grid,localcellcountPerDim=local_count, __RC__)
       
        bounds = find_bounds(local_count(2), num_subgrids)
        subgrids = make_subgrids(primary_grid, num_subgrids)
        allocate(subfields(size(bounds)))
        
        ! 2d, r4
        if (typekind == ESMF_TYPEKIND_R4 .AND. rank == 2) then
           call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr_2d_r4,  __RC__)
           do i = 1, size(bounds)
              new_ptr_2d_r4 => old_ptr_2d_r4(:,bounds(i)%min:bounds(i)%max)
              subfields(i) = ESMF_FieldCreate(subgrids(i), new_ptr_2d_r4, name=name,  __RC__)
           end do
       
        ! 2d, r8
        else if (typekind == ESMF_TYPEKIND_R8 .AND. rank == 2) then
           call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr_2d_r8,  __RC__)
           do i = 1, size(bounds)
              new_ptr_2d_r8 => old_ptr_2d_r8(:,bounds(i)%min:bounds(i)%max)
              subfields(i) = ESMF_FieldCreate(subgrids(i), new_ptr_2d_r8, name=name,  __RC__)
           end do
        
        ! 3d, r4
        else if (typekind == ESMF_TYPEKIND_R4 .AND. rank == 3) then
           call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr_3d_r4,  __RC__)
           do i = 1, size(bounds)
              new_ptr_3d_r4 => old_ptr_3d_r4(:,bounds(i)%min:bounds(i)%max,:) 
              subfields(i) = ESMF_FieldCreate(subgrids(i), new_ptr_3d_r4, name=name, __RC__)
           end do
       
        ! 3d, r8
        else if (typekind == ESMF_TYPEKIND_R8 .AND. rank == 3) then
           call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr_3d_r8,  __RC__)
           do i = 1, size(bounds)
              new_ptr_3d_r8 => old_ptr_3d_r8(:,bounds(i)%min:bounds(i)%max,:)
              subfields(i) = ESMF_FieldCreate(subgrids(i), new_ptr_3d_r8, name=name,  __RC__) 
           end do

        ! 2d, i4
        else if (typekind == ESMF_TYPEKIND_I4 .AND. rank == 2) then
           call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr_2d_i4, __RC__)
           do i = 1, size(bounds)
              new_ptr_2d_i4 => old_ptr_2d_i4(:,bounds(i)%min:bounds(i)%max)
              subfields(i) = ESMF_FieldCreate(subgrids(i), new_ptr_2d_i4, name=name, __RC__)
           end do

        ! 3d, i4
        else if (typekind == ESMF_TYPEKIND_I4 .AND. rank == 3) then
           call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr_3d_i4,  __RC__)
           do i = 1, size(bounds)
              new_ptr_3d_i4 => old_ptr_3d_i4(:,bounds(i)%min:bounds(i)%max,:) 
              subfields(i) = ESMF_FieldCreate(subgrids(i), new_ptr_3d_i4, name=name, __RC__)
           end do

        ! 2d, i8
        else if (typekind == ESMF_TYPEKIND_I8 .AND. rank == 2) then
            call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr_2d_i8,  __RC__)
           do i = 1, size(bounds)
              new_ptr_2d_i8 => old_ptr_2d_i8(:,bounds(i)%min:bounds(i)%max) 
              subfields(i) = ESMF_FieldCreate(subgrids(i), new_ptr_2d_i8, name=name, __RC__)
           end do
           
        ! 3d, i8
        else if (typekind == ESMF_TYPEKIND_I8 .AND. rank == 3) then
            call ESMF_FieldGet(field=primary_field, localDe=0, farrayPtr=old_ptr_3d_i8,  __RC__)
           do i = 1, size(bounds)
              new_ptr_3d_i8 => old_ptr_3d_i8(:,bounds(i)%min:bounds(i)%max,:) 
              subfields(i) = ESMF_FieldCreate(subgrids(i), new_ptr_3d_i8, name=name, __RC__)
           end do
        
        end if

        _RETURN(ESMF_SUCCESS)
    end function make_subfields_from_num_grids


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

    function make_subFieldBundles_base(bundle, num_grids, make, unusable, rc) result(sub_bundles)
       type(ESMF_FieldBundle), allocatable :: sub_bundles(:)
       type(ESMF_FieldBundle), intent(in) :: bundle
       integer, intent(in) :: num_grids
       procedure(i_make_subFields) :: make
       class(KeywordEnforcer), optional, intent(in) :: unusable
       integer, optional, intent(out) :: rc
       integer :: i, j, num_fields, status
       type(ESMF_Field), allocatable :: field_list(:)
       type(ESMF_Field), allocatable :: subfields(:)
       character(len=ESMF_MAXPATHLEN) :: name

       allocate(sub_bundles(num_grids))
      
       ! get number of fields and field list from field bundle
       call ESMF_FieldBundleGet(bundle, fieldCount=num_fields, name=name, __RC__)
       allocate(field_list(num_fields))
       call ESMF_FieldBundleGet(bundle, fieldList=field_list, __RC__)

       ! make subfields for each field and add each subfield to corresponding field bundle
       do i = 1, num_grids
          sub_bundles(i) = ESMF_FieldBundleCreate(name=name, __RC__)
       end do
       do i = 1, size(field_list)
          subfields = make(field_list(i), num_grids, __RC__)
          do j = 1, size(subfields)
             call ESMF_FieldBundleAdd(sub_bundles(j), subfields(j:j), __RC__)
          end do
       end do

       _RETURN(ESMF_SUCCESS)
    end function make_subFieldBundles_base

    function  make_subfieldBundles_ordinary(bundle,num_grids,unusable, rc) result(sub_bundles)
       type(ESMF_FieldBundle), allocatable :: sub_bundles(:)
       type(ESMF_FieldBundle), intent(in) :: bundle
       integer, intent(in) :: num_grids
       class(KeywordEnforcer), optional, intent(in) :: unusable
       integer, optional, intent(out) :: rc
       integer :: status
       sub_bundles = make_subFieldBundles_base(bundle,num_grids,make_subFields_from_num_grids, __RC__)
       _RETURN(ESMF_SUCCESS)
    end function make_subfieldBundles_ordinary



end module MAPL_OpenMP_Support 
