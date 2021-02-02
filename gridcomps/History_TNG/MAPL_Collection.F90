#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module MAPL_Collection
   use, intrinsic :: iso_fortran_env, only: INT64
   use ESMF
   use NUOPC
   use yaFyaml
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod

   use MAPL_FieldCollection

   implicit none
   private

   public Collection

   type :: Collection
      character(:), allocatable :: name
      character(:), allocatable :: template
      character(:), allocatable :: frequency
      type(FieldCollection)     :: fields
   contains
      procedure :: import_fields
      procedure :: import_bundle
   end type Collection
contains
   subroutine import_fields(this, config)
      class(Collection),   intent(inout) :: this
      type(Configuration), intent(inout) :: config

      character(:), pointer       :: component_name
      type(ConfigurationIterator) :: iter
      type(Configuration)         :: sub_config

      iter = config%begin()
      do while(iter /= config%end())
         component_name => iter%key()
         sub_config     =  iter%value()

         call this%import_bundle(component_name, sub_config)
         call iter%next()
      end do
   end subroutine import_fields

   subroutine import_bundle(this, component_name, config)
      class(Collection),   intent(inout) :: this
      character(*),        intent(in   ) :: component_name
      type(Configuration), intent(inout) :: config

      type(ConfigurationIterator) :: iter
      type(Configuration)         :: sub_config

      iter = config%begin()
      do while(iter /= config%end())
         sub_config = iter%value()

         call this%fields%insert_config(component_name, sub_config)
         call iter%next()
      end do
   end subroutine import_bundle
end module MAPL_Collection

