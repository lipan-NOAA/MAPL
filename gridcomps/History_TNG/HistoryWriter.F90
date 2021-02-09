#include "MAPL_Generic.h"
#include "unused_dummy.H"

module HistoryWriterMod
   use, intrinsic :: iso_fortran_env, only: INT64
   use ESMF
   use NUOPC

   use CollectionMod

   implicit None
   private

   public HistoryWriter

   type :: HistoryWriter
      private
      type(Collection) :: collection
   end type HistoryWriter
end module HistoryWriterMod
