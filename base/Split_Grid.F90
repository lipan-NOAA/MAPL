#include "MAPL_Generic.h"

   Program ut_ReGridding

   use MPI
   use ESMF
   use MAPL_ConstantsMod
   use ESMFL_Mod
   use MAPL_Profiler
   use MAPL_BaseMod
   use MAPL_CommsMod
   use MAPL_GridManagerMod
   use MAPL_LatLonGridFactoryMod, only: LatLonGridFactory
   use MAPL_CubedSphereGridFactoryMod, only: CubedSphereGridFactory
   use MAPL_ExceptionHandling
   use omp_lib, only: omp_get_max_threads
   use, intrinsic :: iso_fortran_env, only: REAL64,REAL32
   use MAPL_OpenMP_Support

   implicit NONE

   call main()

CONTAINS

    subroutine main()

!CONTAINS

!  Basic ESMF objects being used in this example
!  ---------------------------------------------
   type(ESMF_Grid) :: cs_grid
   type(ESMF_Grid), allocatable :: xy_grid(:)
   type(ESMF_VM) :: vm

!  ------------------------------------------------
   integer :: myPET   ! The local PET number
   integer :: nPET    ! The total number of PETs you are running on

   integer :: status, rc
   integer :: nx,ny,nargs
   integer :: im,jm,numGrids
   integer :: i,section
   character(len=ESMF_MAXPATHLEN) :: astr,str
   integer :: local_count(3),global_count(3)
   type(CubedSphereGridFactory) :: cs_factory
   !type(LatLonGridFactory) :: ll_factory
   integer :: mpi_comm,rank,comm_size
   real(kind=ESMF_KIND_R8), pointer :: lats(:,:),lons(:,:)
   real(kind=ESMF_KIND_R8), pointer :: xy_lats(:,:), xy_lons(:,:)
   integer, allocatable :: lower_upper_index(:,:)
   integer :: petMap(1,1,1)

   type(ESMF_Field) :: old_field
   type(ESMF_Field), allocatable :: new_field(:)
   real(kind=ESMF_KIND_R4), pointer:: old_ptr(:,:)
   real(kind=ESMF_KIND_R4), pointer :: new_ptr(:,:)
   character(len=20) :: name

    ! initialize ESMF, check get mpi comm to show it matches PET
    call ESMF_Initialize (LogKindFlag=ESMF_LOGKIND_MULTI, vm=vm, rc=status)
    _VERIFY(STATUS)
    call ESMF_VMGet(vm, localPET=myPET, petCount=nPet,mpiCommunicator=mpi_comm,rc=status)
    _VERIFY(STATUS)
    call MPI_Comm_Rank(mpi_comm,rank,status)
    _VERIFY(STATUS)
    call MPI_Comm_Size(mpi_comm,comm_size,status)
    _VERIFY(STATUS)
    write(*,'(A,i4,A,i4)')"Hello on mpirank : ",rank," and PET number: ",myPet

    !! examples of how to run
    !! lat-lon grid
    !   mpirun -np 6 Split_Grid.x -im 180 -jm 90 -nx 3 -ny 2 -n 2
    !   mpirun -np 16 Split_Grid.x -im 180 -jm 90 -nx 4 -ny 4 -n 4
    !! for cubed-sphere grid (each face decomposed)
    !   mpirun -np 6 Split_Grid.x -im 90 -nx 1 -n 3
    !   mpirun -np 24 Split_Grid.x -im 90 -nx 2 -n 4

    ! get values for im, jm, nx, ny from command arguments
    nargs = command_argument_count()
    do i=1,nargs
      call get_command_argument(i,str)
      select case(trim(str))
      case('-im')
         call get_command_argument(i+1,astr)
         read(astr,*)im
      case('-jm')
         call get_command_argument(i+1,astr)
         read(astr,*)jm
      case('-nx')
         call get_command_argument(i+1,astr)
         read(astr,*)nx
      case('-ny')
         call get_command_argument(i+1,astr)
         read(astr,*)ny
      ! n is the number of mini-grids
      case('-n')
         call get_command_argument(i+1, astr)
         read(astr,*)numGrids
      end select
    enddo

    ! make a lat-lon grid

   !  ll_factory = LatLonGridFactory(im_world=im,jm_world=jm,nx=nx,ny=ny,lm=1,pole='PE', &
   !     dateline='DE',rc=status)
   !  _VERIFY(status)
   !  grid=ll_factory%make_grid(rc=status)
   !  _VERIFY(status)
 
    ! make a cubed-sphere grid

    cs_factory = CubedSphereGridFactory(im_world=im,nx=nx,ny=nx,lm=1,rc=status)
    _VERIFY(status)

    cs_grid=cs_factory%make_grid(rc=status)
    _VERIFY(status)

    call MAPL_GridGet(cs_grid,localcellcountPerDim=local_count, globalCellCountPerDim=global_count,rc=status)
    _VERIFY(status)
   !  write(*,'(A,i4,A,i4,A,i4,i4,A,i4,i4)')"My pet: ",myPet, " out of ",nPet, & 
   ! "Local sizes: ",local_count(1),local_count(2), &
   !  " Global sizes: ",global_count(1),global_count(2)

    ! now get coordinates from the grid

    call ESMF_GridGetCoord(cs_grid, coordDim=1, localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CENTER, &
      farrayPtr=lons, rc=status)
    _VERIFY(status)

    call ESMF_GridGetCoord(cs_grid, coordDim=2, localDE=0, &
    staggerloc=ESMF_STAGGERLOC_CENTER, &
    farrayPtr=lats, rc=status)
    _VERIFY(status)
    
   write(*,'(A,i3,A,f10.5,f10.5)')'My pet: ',myPet,' min/max lons ',minval(lons),maxval(lons)
   write(*,'(A,i3,A,f10.5,f10.5)')'My pet: ',myPet,' min/max lats ',minval(lats),maxval(lats)

   ! now make mini-grid from full grid
   call FindBounds(local_count(2), numGrids, lower_upper_index)

   allocate(xy_grid(numGrids))
   petMap(1,1,1) = myPet
   do i = 1, numGrids
      section = lower_upper_index(i,2) - lower_upper_index(i,1) + 1
      xy_grid(i) = ESMF_GridCreateNoPeriDim( &
            countsPerDEDim1 = [local_count(1)], &
            countsPerDEDim2 = [section], &
            indexFlag=ESMF_INDEX_DELOCAL, &
            coordDep1=[1,2], &
            coordDep2=[1,2], &
            coordSys=ESMF_COORDSYS_SPH_RAD, &
            petMap = petMap, &
            rc=status)
      _VERIFY(status)
      call ESMF_GridAddCoord(grid=xy_grid(i), staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
      _VERIFY(status)
   end do
 
   do i = 1, numGrids
      call ESMF_GridGetCoord(grid=xy_grid(i), coordDim=1, localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CENTER, &
      farrayPtr=xy_lons, rc=status)
      _VERIFY(status)
      call SubsetArray(lons, xy_lons, lower_upper_index(i,:))
   end do
   
   do i = 1, numGrids
      call ESMF_GridGetCoord(grid=xy_grid(i), coordDim=2, localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CENTER, &
      farrayPtr=xy_lats, rc=status)
      _VERIFY(status)
      call SubsetArray(lats, xy_lats, lower_upper_index(i,:))
   end do
   deallocate(lower_upper_index)

   ! now for the fields...
  
   old_field = ESMF_FieldCreate(grid=cs_grid, typekind=ESMF_TYPEKIND_R4, &
      indexflag=ESMF_INDEX_DELOCAL, &
      staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
   _VERIFY(status)  ! use old cs_grid

   call ESMF_FieldGet(field=old_field, localDe=0, farrayPtr=old_ptr, rc=status)
   _VERIFY(status)

   call FindBounds(local_count(2), numGrids, lower_upper_index) 
   allocate(new_field(numGrids))
   name = 'new field'
   do i = 1, numGrids
      new_field(i) = ESMF_FieldEmptyCreate(name=name, rc=status)
      _VERIFY(STATUS)
      call ESMF_FieldEmptySet(field=new_field(i), &
           grid=xy_grid(i), &
           staggerloc=ESMF_STAGGERLOC_CENTER, &
           rc=status)
      _VERIFY(STATUS)  
      call ESMF_FieldEmptyComplete(field=new_field(i), &
      typekind=ESMF_TYPEKIND_R4, rc=status) !ungriddedLBound=(/1/), ungriddedUBound=(/5/)
      _VERIFY(status)
      call ESMF_FieldGet(field=new_field(i), localDe=0, farrayPtr=new_ptr, rc=status)
      _VERIFY(status)
      new_ptr => old_ptr(:,lower_upper_index(i,1):lower_upper_index(i,2)) 
   end do

   ! finish
   call ESMF_Finalize()
   end subroutine main

!   ! find the lower and upper bounds (2 y-coodinates) of each mini-grid
!    subroutine FindBounds(yDim, numGrids, lower_upper_index)
!       implicit NONE
!       integer :: yDim, numGrids, i, step, count, numOfFirstSize, numOfSecondSize, firstSize, secondSize
!       integer, allocatable :: lower_upper_index(:,:)
!       allocate(lower_upper_index(numGrids,2))

!       ! if the size of each grid is the same
!       if (modulo(yDim, numGrids) == 0) then
!          step = yDim/numGrids
!          count = 1
!          ! go from 1-yDim incrementing by step size
!          do i = 1, yDim, step
!             lower_upper_index(count,1) = i
!             lower_upper_index(count,2) = i + step - 1
!             count = count + 1
!          end do
!       ! if at least one grid is a different size
!       else 
!          firstSize = yDim/numGrids 
!          numOfSecondSize = modulo(yDim, numGrids)
!          numOfFirstSize = numGrids - numOfSecondSize
!          secondSize = (yDim - firstSize * numOfFirstSize) / numOfSecondSize
         
!          count = 1
!          do i = 1, numOfFirstSize * firstSize, firstSize 
!             lower_upper_index(count,1) = i
!             lower_upper_index(count,2) = i + firstSize - 1
!             count = count + 1
!          end do

!          do i = numOfFirstSize * firstSize + 1, yDim, secondSize
!             lower_upper_index(count,1) = i   
!             lower_upper_index(count,2) = i + secondSize - 1
!             count = count + 1
!          end do
!       end if

!       ! ! test the number of boxes/grids and the indices
!       ! print*, 'number of boxes: ', size(lower_upper_index, 1)
!       ! do i = 1,size(lower_upper_index,1)
!       !    print*, lower_upper_index(i,1), lower_upper_index(i,2)
!       ! end do

!    end subroutine

   ! subroutine SubsetArray(input_array, output_array, lower_upper_index)
   !    implicit NONE
   !    integer :: i, j
   !    real(kind=ESMF_KIND_R8), pointer :: input_array(:,:)
   !    real(kind=ESMF_KIND_R8), pointer :: output_array(:,:)
   !    integer :: lower_upper_index(2)
      
   !    allocate(output_array(size(input_array,1), lower_upper_index(2)-lower_upper_index(1)+1))
   !    output_array(:, :) = input_array(:,lower_upper_index(1):lower_upper_index(2)) 
   ! end subroutine

end program ut_ReGridding  
