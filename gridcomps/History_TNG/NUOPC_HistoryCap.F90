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

   implicit none
   private

   character(*), parameter :: internal_name = 'NUOPC_HistoryCap'

   type :: HistoryCap_wrapper
      type(HistoryCap), pointer :: ptr
   end type HistoryCap_wrapper
end module NUOPC_HistoryCapMod
