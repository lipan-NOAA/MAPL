#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"

module mapl_MaplGenericComponent
   use ESMF
   use mapl_AbstractFrameworkComponent
   use mapl_BaseFrameworkComponent
   use mapl_keywordenforcermod
   use mapl_AbstractComponent
   use mapl_CompositeComponent
   use mapl_ConcreteComposite
   use mapl_MaplComponent
   use mapl_ErrorHandlingMod
   use pFlogger
   use mapl_OpenMP_Support
   use mapl_MaplGrid
   implicit none
   private

   public :: MaplGenericComponent
   public :: get_grid

   type, extends(BaseFrameworkComponent) :: MaplGenericComponent
!!$      private
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: import_state
      type(ESMF_State) :: export_state
      type(ESMF_State) :: internal_state
      logical :: threading_active = .FALSE.
      type(ESMF_State), allocatable :: import_substates(:)
      type(ESMF_State), allocatable :: export_substates(:)
      type(ESMF_State), allocatable :: internal_substates(:)
      type(MaplGrid) :: grid
      type(MaplGrid), allocatable :: subgrids(:)
      

   contains
      procedure :: initialize => stub
      procedure :: run => stub
      procedure :: finalize => stub

      procedure :: initialize_child => stub_child
      procedure :: run_child => stub_child
      procedure :: finalize_child => stub_child

      procedure :: add_child_component
      
      procedure :: activate_threading
      procedure :: deactivate_threading

      procedure :: create_subobjects

      ! accessors

      procedure :: get_logger
      procedure :: set_logger
      procedure :: is_threading_active
      procedure :: get_internal_state
      procedure :: get_import_state
      procedure :: get_export_state
      procedure :: get_grid
   end type MaplGenericComponent

contains



   subroutine stub(this, clock, phase, unusable, rc)
      class(MaplGenericComponent), intent(inout) :: this
      type(ESMF_Clock), intent(inout) :: clock
      character(*), intent(in) :: phase
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      class(AbstractComponent), pointer :: user_component

      _UNUSED_DUMMY(unusable)

      user_component => this%get_component()
      call user_component%run(this%import_state, this%export_state, clock, phase, __RC__)

      _RETURN(_SUCCESS)
   end subroutine stub
   
   subroutine stub_child(this, name, clock, phase, unusable, rc)
      class(MaplGenericComponent), intent(inout) :: this
      character(*), intent(in) :: name
      type(ESMF_Clock), intent(inout) :: clock
      character(*), intent(in) :: phase
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      class(AbstractFrameworkComponent), pointer :: child
      integer :: status

      _UNUSED_DUMMY(unusable)

      child => this%get_child(name)
      call child%run(clock, phase, __RC__)

      _RETURN(_SUCCESS)
   end subroutine stub_child


   function add_child_component(this, name, user_component) result(child)
      class(AbstractFrameworkComponent), pointer :: child
      class(MaplGenericComponent), target, intent(inout) :: this
      character(*), intent(in) :: name
      class(AbstractComponent), intent(in) :: user_component

      type(MaplGenericComponent) :: tmp

      child => this%add_child(name, tmp)
      call child%set_component(user_component)

   end function add_child_component


   subroutine generic_entry_point(gridcomp, import, export, clock, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      type(ESMF_State), intent(inout) :: import
      type(ESMF_State), intent(inout) :: export
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc

      character(len=ESMF_MAXSTR) :: name
      integer :: phase
      type(ESMF_Method_Flag) :: method
      integer :: status

      class(MaplComponent), pointer :: component
      type :: ComponentWrapper
         class(MaplComponent), pointer :: component
      end type ComponentWrapper

      type(ComponentWrapper) :: wrapper
      character(:), pointer :: phase_name
      
      call ESMF_UserCompGetInternalState(gridcomp, "MaplComponent", wrapper, status)
      _VERIFY(status)
      component => wrapper%component

      call ESMF_GridCompGet( gridcomp, name=name, currentPhase=phase, currentMethod=method, __RC__)

      if (method == ESMF_METHOD_INITIALIZE) then

!!$         phase_name => component%init_phase_map%at(phase)
!!$         _ASSERT(associated(phase_name),'no such phase')
         call component%initialize(import, export, clock, phase_name, __RC__)

      else if (method == ESMF_METHOD_RUN) then

!!$         phase_name = component%run_phase_map%at(phase)
!!$         _ASSERT(associated(phase_name),'no such phase')
         call component%run(import, export, clock, phase_name, __RC__)

      else if (method == ESMF_METHOD_FINALIZE) then
         
!!$         phase_name = component%finalize_phase_map%at(phase)
!!$         _ASSERT(associated(phase_name),'no such phase')
         call component%finalize(import, export, clock, phase_name, __RC__)

      else
         _FAIL('unknown value for ESMF_METHOD_FLAG')
      end if
         
      _RETURN(_SUCCESS)
   end subroutine generic_entry_point

   function get_logger(this) result(lgr)
      class(Logger), pointer :: lgr
      class(MaplGenericComponent), intent(in) :: this
      class(AbstractComponent), pointer :: component

      component => this%get_component()
      lgr => component%get_logger()
      
   end function get_logger

   subroutine set_logger(this, lgr)
      class(MaplGenericComponent), intent(inout) :: this
      class(Logger), target :: lgr
      class(AbstractComponent), pointer :: component

      component => this%get_component()
      call component%set_logger(lgr)
      
   end subroutine set_logger

   function is_threading_active(this) result(threading_active)
     class(MaplGenericComponent), intent(in) :: this
     logical :: threading_active
     
     threading_active = this%threading_active
   end function is_threading_active

   function get_internal_state(this) result(internal_state)
      class(MaplGenericComponent), target, intent(in) :: this
      type(ESMF_State), pointer :: internal_state
      integer :: thread 

      if (.NOT. this%is_threading_active()) then
        internal_state => this%internal_state
     else
        thread = 0
        !$ thread = omp_get_thread_num()
        internal_state => this%internal_substates(thread+1)
     end if
   end function get_internal_state

   function get_import_state(this) result(import_state)
     class(MaplGenericComponent), target, intent(in) :: this
     type(ESMF_State), pointer :: import_state
     integer :: thread

     if (.NOT. this%is_threading_active()) then
        import_state => this%import_state
     else
        thread = 0
        !$ thread = omp_get_thread_num()
        import_state => this%import_substates(thread+1)
     end if
   end function get_import_state

   function get_export_state(this) result(export_state)
     class(MaplGenericComponent), target, intent(in) :: this
     type(ESMF_State), pointer :: export_state
     integer :: thread

     if (.NOT. this%is_threading_active()) then
        export_state => this%export_state
     else
        thread = 0
        !$ thread = omp_get_thread_num()
        export_state => this%export_substates(thread+1)
     end if
   end function get_export_state

   function get_grid(this) result(grid)
     class(MaplGenericComponent), target, intent(in) :: this
     type(MaplGrid), pointer :: grid
     integer :: thread

     if (.NOT. this%is_threading_active()) then
        grid => this%grid
     else
        thread = 0
        !$ thread = omp_get_thread_num()
        grid => this%subgrids(thread+1) ! subgrids is of type ESMF_Grid because of the return type of make_subgrids
     end if
   end function get_grid


   recursive subroutine activate_threading(this) 
     class(MaplGenericComponent), intent(inout) :: this
     class(AbstractFrameworkComponent), pointer :: child
     integer :: num_children, i, rc, num_threads
      
     !$ if(.NOT. omp_in_parallel()) then 
        !$omp parallel
     !$ end if

     !$omp serial
     this%threading_active = .TRUE.
     num_children = this%get_num_children()

     num_threads = 1
     !$ num_threads = omp_get_num_threads()

     if (.NOT. allocated(this%import_substates)) then
        call this%create_subobjects(num_threads)
     end if

     do i = 1, num_children
        child => this%get_child(i) 
        SELECT TYPE (child)
        TYPE IS (MaplGenericComponent)
           call child%activate_threading()
        CLASS DEFAULT
           _FAIL('illegal type for child')
        END SELECT
     end do
     !$omp end serial

   end subroutine activate_threading

   subroutine create_subobjects(this, num_threads)
     class(MaplGenericComponent), intent(inout) :: this
     integer, intent(in) :: num_threads
     integer :: i, rc, status
     type(ESMF_Grid), allocatable :: subgrids(:)

     this%import_substates = make_substates(this%import_state, num_threads) ! number for num_grids argument?
     this%export_substates = make_substates(this%export_state, num_threads)
     this%internal_substates = make_substates(this%internal_state, num_threads)
     subgrids = make_subgrids(this%grid%ESMFGrid, num_threads) ! make_subgrids requires grid of type ESMF_Grid

     allocate(this%subgrids(size(subgrids)))
     do i = 1, size(subgrids)
        call this%subgrids(i)%set(subgrids(i), __RC__)
     end do
     deallocate(subgrids)
   end subroutine create_subobjects

   subroutine deactivate_threading(this)
     class(MaplGenericComponent), intent(inout) :: this
     class(AbstractFrameworkComponent), pointer :: child
     integer :: num_children, i, rc

     !$ if(omp_in_parallel()) then 
        !$omp end parallel
     !$ end if
     
     num_children = this%get_num_children()
     do i = 1, num_children
        child => this%get_child(i) 
        SELECT TYPE (child)
        TYPE IS (MaplGenericComponent)
           call child%deactivate_threading()
        CLASS DEFAULT
           _FAIL('illegal type for child')
        END SELECT
     end do
     
     this%threading_active = .FALSE.
   end subroutine deactivate_threading

end module mapl_MaplGenericComponent
