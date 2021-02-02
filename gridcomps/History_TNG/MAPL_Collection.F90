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
   subroutine import_fields(this, config, rc)
      class(Collection),   intent(inout) :: this
      type(Configuration), intent(inout) :: config
      integer, optional,   intent(  out) :: rc

      character(:), pointer       :: component_name
      type(ConfigurationIterator) :: iter
      type(Configuration)         :: sub_config

      integer :: status

      iter = config%begin()
      do while(iter /= config%end())
         component_name => iter%key()
         sub_config     =  iter%value()

         call this%import_bundle(component_name, sub_config, __RC__)
         call iter%next()
      end do

      _RETURN(_SUCCESS)
   end subroutine import_fields

   subroutine import_bundle(this, component_name, config, rc)
      class(Collection),   intent(inout) :: this
      character(*),        intent(in   ) :: component_name
      type(Configuration), intent(inout) :: config
      integer, optional,   intent(  out) :: rc

      character(:), pointer       :: short_name
      type(ConfigurationIterator) :: iter
      type(Configuration)         :: sub_config

      integer :: status

      iter = config%begin()
      do while(iter /= config%end())
         short_name => iter%key()
         sub_config =  iter%value()

         call this%fields%import_field(short_name, component_name, sub_config, __RC__)
         call iter%next()
      end do

      _RETURN(_SUCCESS)
   end subroutine import_bundle
end module MAPL_Collection

