#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module HistoryCapMod
   use, intrinsic :: iso_fortran_env, only: INT64
   use ESMF
   use NUOPC
   use MAPL_CapMod

   use FieldRegistryMod
   use NUOPCmapMod

   implicit none
   private

   public HistoryCap

   type :: HistoryCap
      private

      character(len=:), allocatable              :: name
      character(len=:), allocatable              :: rc_file
      procedure(i_set_services), nopass, pointer :: set_services
      type(MAPL_Cap), pointer                    :: cap => null()
      type(NUOPCmap), pointer                    :: phase_map => null()

      type(FieldRegistry) :: registry
   contains
      procedure :: init_p0
      procedure :: generic_init
      procedure :: data_init
      procedure :: advance
      procedure :: check_import
      procedure :: set_clock
      procedure :: finalize
   end type HistoryCap

   abstract interface
      subroutine i_set_services(gc, rc)
         import ESMF_GridComp
         type(ESMF_GridComp), intent(inout) :: gc
         integer,             intent(  out) :: rc
      end subroutine i_set_services
   end interface
contains
   subroutine init_p0(this, model, import_state, export_state, clock, rc)
      class(HistoryCap),  intent(inout) :: this
      type(ESMF_GridComp)               :: model
      type(ESMF_State)                  :: import_state
      type(ESMF_State)                  :: export_state
      type(ESMF_Clock)                  :: clock
      integer,            intent(  out) :: rc

      integer :: status

      rc = ESMF_SUCCESS

      call NUOPC_CompFilterPhaseMap(model, ESMF_METHOD_INITIALIZE, &
         acceptStringList=["IPDv05p"], rc=rc)
      VERIFY_NUOPC_(rc)

      ! TODO: Implement rest
   end subroutine init_p0

   subroutine generic_init(this, model, import_state, export_state, clock, rc)
      class(HistoryCap),  intent(inout) :: this
      type(ESMF_GridComp)               :: model
      type(ESMF_State)                  :: import_state
      type(ESMF_State)                  :: export_state
      type(ESMF_Clock)                  :: clock
      integer,            intent(  out) :: rc

      character(len=:), pointer :: phase_label
      integer                   :: current_phase_index

      rc = ESMF_SUCCESS

      call ESMF_GridCompGet(model, currentPhase=current_phase_index, rc=rc)
      VERIFY_NUOPC_(rc)

      call this%phase_map%get_phase(current_phase_index, phase_label, rc)
      VERIFY_NUOPC_(rc)

      ! TODO: Implement these
      ! select case(phase_label)
      ! case (phase_label_list(1))
      !    call this%init_p1(import_state, export_state, clock, rc)
      !    VERIFY_NUOPC_(rc)

      ! case (phase_label_list(2))
      !    call this%init_p2(import_state, export_state, clock, rc)
      !    VERIFY_NUOPC_(rc)

      ! case (phase_label_list(3))
      !    call this%init_p3(import_state, export_state, clock, rc)
      !    VERIFY_NUOPC_(rc)

      ! case (phase_label_list(4))
      !    call this%init_p4(import_state, export_state, clock, rc)
      !    VERIFY_NUOPC_(rc)

      ! case (phase_label_list(5))
      !    call this%init_p5(import_state, export_state, clock, rc)
      !    VERIFY_NUOPC_(rc)

      ! case (phase_label_list(6))
      !    call this%init_p6(import_state, export_state, clock, rc)
      !    VERIFY_NUOPC_(rc)
      ! end select
   end subroutine generic_init

   subroutine data_init(this, model, rc)
      class(HistoryCap),  intent(inout) :: this
      type(ESMF_GridComp)               :: model
      integer,            intent(  out) :: rc

      rc = ESMF_SUCCESS

      call NUOPC_CompAttributeSet(model, &
         name="InitializeDataComplete", value="true", rc=rc)
      VERIFY_NUOPC_(rc)
   end subroutine data_init

   subroutine advance(this, rc)
      class(HistoryCap), intent(inout) :: this
      integer,           intent(  out) :: rc

      rc = ESMF_SUCCESS

      call this%cap%step_model(rc=rc)
      VERIFY_NUOPC_(rc)
   end subroutine advance

   subroutine check_import(this, rc)
      class(HistoryCap), intent(inout) :: this
      integer,           intent(  out) :: rc

      rc = ESMF_SUCCESS
   end subroutine check_import

   subroutine set_clock(this, model, rc)
      class(HistoryCap),  intent(inout) :: this
      type(ESMF_GridComp)               :: model
      integer,            intent(  out) :: rc

      type(ESMF_TimeInterval) :: time_step
      type(ESMF_Clock)        :: model_clock
      integer                 :: heartbeat_dt

      rc = ESMF_SUCCESS

      ! set time interval
      heartbeat_dt = this%cap%cap_gc%get_heartbeat_dt()
      call ESMF_TimeIntervalSet(time_step, s=heartbeat_dt, rc=rc)
      VERIFY_NUOPC_(rc)

      ! TODO: investigate build error
      ! set clock with time interval
      ! call NUOPC_ModelGet(model, modelClock=model_clock, rc=rc)
      ! VERIFY_NUOPC_(rc)
      ! call ESMF_ClockSet(model_clock, timeStep=time_step, rc=rc)
      ! VERIFY_NUOPC_(rc)
   end subroutine set_clock

   subroutine finalize(this, rc)
      class(HistoryCap), intent(inout) :: this
      integer,           intent(  out) :: rc


      rc = ESMF_SUCCESS
   end subroutine finalize
end module HistoryCapMod
