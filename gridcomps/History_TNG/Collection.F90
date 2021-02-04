#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module CollectionMod
   use, intrinsic :: iso_fortran_env, only: INT64
   use ESMF
   use NUOPC
   use yaFyaml
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod

   use FieldGroupMod

   implicit none
   private

   public Collection

   type :: Collection
      character(:), allocatable :: name
      character(:), allocatable :: template
      character(:), allocatable :: frequency
      type(FieldGroup)          :: fields
   contains
   end type Collection
contains
end module CollectionMod

