#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module NUOPC_HistoryCapMod
   use ESMF
   use NUOPC
   use NUOPC_Model, &
       model_SetServices    => SetServices, &
       model_Advance        => label_advance, &
       model_CheckImport    => label_CheckImport, &
       model_DataInitialize => label_DataInitialize, &
       model_SetClock       => label_SetClock, &
       model_Finalize       => label_finalize

   use HistoryCapMod
   use NUOPCmapMod

   implicit none
   private

   public SetServices

   character(*), parameter :: internal_name = 'NUOPC_HistoryCap'

   type :: HistoryCap_wrapper
      type(HistoryCap), pointer :: ptr
   end type HistoryCap_wrapper
contains
   subroutine SetServices(model, rc)
      type(ESMF_GridComp)  :: model
      integer, intent(out) :: rc

      integer :: i

      rc = ESMF_SUCCESS

      call NUOPC_CompDerive(model, model_SetServices, rc=rc)
      VERIFY_NUOPC_(rc)

      call ESMF_GridCompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
         userRoutine=initialize_p0, phase=0, rc=rc)
      VERIFY_NUOPC_(rc)

      ! TODO: eliminate entry point once we can use ESMF 8.1.0 specialize labels
      ! set entry point for methods that require specific implementation
      do i=1, num_phases
         call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
            phaseLabelList=[phase_label_list(i)], userRoutine=generic_initialize, rc=rc)
         VERIFY_NUOPC_(rc)
      end do

      ! attach specializing method(s)
      call NUOPC_CompSpecialize(model, specLabel=model_DataInitialize, &
         specRoutine=initialize_data, rc=rc)
      VERIFY_NUOPC_(rc)
      call NUOPC_CompSpecialize(model, specLabel=model_Advance, &
         specRoutine=advance, rc=rc)
      VERIFY_NUOPC_(rc)
      call ESMF_MethodRemove(model, label=model_CheckImport, rc=rc)
      VERIFY_NUOPC_(rc)
      call NUOPC_CompSpecialize(model, specLabel=model_CheckImport, &
         specRoutine=check_import, rc=rc)
      VERIFY_NUOPC_(rc)

      call NUOPC_CompSpecialize(model, specLabel=model_SetClock, &
         specRoutine=set_clock, rc=rc)
      VERIFY_NUOPC_(rc)
      call NUOPC_CompSpecialize(model, specLabel=model_Finalize, &
         specRoutine=finalize, rc=rc)
      VERIFY_NUOPC_(rc)
   end subroutine SetServices

   subroutine initialize_p0(model, import_state, export_state, clock, rc)
      type(ESMF_GridComp)  :: model
      type(ESMF_State)     :: import_state
      type(ESMF_State)     :: export_state
      type(ESMF_Clock)     :: clock
      integer, intent(out) :: rc

      type(HistoryCap), pointer :: cap

      rc = ESMF_SUCCESS

      call NUOPC_CompFilterPhaseMap(model, ESMF_METHOD_INITIALIZE, &
          acceptStringList=["IPDv05p"], rc=rc)
      VERIFY_NUOPC_(rc)

      cap => get_HistoryCap(model, rc)
      VERIFY_NUOPC_(rc)

      call cap%init_p0(model, import_state, export_state, clock, rc)
      VERIFY_NUOPC_(rc)
   end subroutine initialize_p0

   subroutine generic_initialize(model, import_state, export_state, clock, rc)
      type(ESMF_GridComp)  :: model
      type(ESMF_State)     :: import_state
      type(ESMF_State)     :: export_state
      type(ESMF_Clock)     :: clock
      integer, intent(out) :: rc

      type(HistoryCap), pointer :: cap

      rc = ESMF_SUCCESS

      cap => get_HistoryCap(model, rc)
      VERIFY_NUOPC_(rc)

      call cap%generic_init(model, import_state, export_state, clock, rc)
      VERIFY_NUOPC_(rc)
   end subroutine generic_initialize

   subroutine advertise(model, rc)
      type(ESMF_GridComp)  :: model
      integer, intent(out) :: rc

      type(HistoryCap), pointer :: cap

      rc = ESMF_SUCCESS

      cap => get_HistoryCap(model, rc)
      VERIFY_NUOPC_(rc)

      call cap%advertise(model, rc)
      VERIFY_NUOPC_(rc)
   end subroutine advertise

   subroutine realize(model, rc)
      type(ESMF_GridComp)  :: model
      integer, intent(out) :: rc

      type(HistoryCap), pointer :: cap

      rc = ESMF_SUCCESS

      cap => get_HistoryCap(model, rc)
      VERIFY_NUOPC_(rc)

      call cap%realize(model, rc)
      VERIFY_NUOPC_(rc)
   end subroutine realize

   subroutine initialize_data(model, rc)
      type(ESMF_GridComp)  :: model
      integer, intent(out) :: rc

      type(HistoryCap), pointer :: cap

      rc = ESMF_SUCCESS

      cap => get_HistoryCap(model, rc)
      VERIFY_NUOPC_(rc)

      call cap%data_init(model, rc)
      VERIFY_NUOPC_(rc)
   end subroutine initialize_data

   subroutine advance(model, rc)
      type(ESMF_GridComp)  :: model
      integer, intent(out) :: rc

      type(HistoryCap), pointer :: cap

      rc = ESMF_SUCCESS

      cap => get_HistoryCap(model, rc)
      VERIFY_NUOPC_(rc)

      call cap%advance(rc)
      VERIFY_NUOPC_(rc)
   end subroutine advance

   subroutine check_import(model, rc)
      type(ESMF_GridComp)  :: model
      integer, intent(out) :: rc

      type(HistoryCap), pointer :: cap

      rc = ESMF_SUCCESS

      cap => get_HistoryCap(model, rc)
      VERIFY_NUOPC_(rc)

      call cap%check_import(rc)
      VERIFY_NUOPC_(rc)
   end subroutine check_import

   subroutine set_clock(model, rc)
      type(ESMF_GridComp)  :: model
      integer, intent(out) :: rc

      type(HistoryCap), pointer :: cap

      rc = ESMF_SUCCESS

      cap => get_HistoryCap(model, rc)
      VERIFY_NUOPC_(rc)

      call cap%set_clock(model, rc)
      VERIFY_NUOPC_(rc)
   end subroutine set_clock

   subroutine finalize(model, rc)
      type(ESMF_GridComp)  :: model
      integer, intent(out) :: rc

      type(HistoryCap), pointer :: cap

      rc = ESMF_SUCCESS

      cap => get_HistoryCap(model, rc)
      VERIFY_NUOPC_(rc)

      call cap%finalize(rc)
      VERIFY_NUOPC_(rc)
   end subroutine finalize

   function get_HistoryCap(gc, rc) result(cap)
      type(ESMF_GridComp), intent(inout) :: gc
      integer,             intent(  out) :: rc
      type(HistoryCap),    pointer       :: cap

      type(HistoryCap_wrapper) :: wrapper

      rc = ESMF_SUCCESS

      call ESMF_UserCompGetInternalState(gc, internal_name, wrapper, rc)
      VERIFY_NUOPC_(rc)

      cap => wrapper%ptr
   end function get_HistoryCap
end module NUOPC_HistoryCapMod
