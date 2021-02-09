#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module HistoryCapMod
   use, intrinsic :: iso_fortran_env, only: INT64
   use ESMF
   use NUOPC

   use FieldRegistryMod

   implicit none
   private

   public HistoryCap

   type :: HistoryCap
      private
      type(FieldRegistry) :: registry
   end type HistoryCap
end module HistoryCapMod
