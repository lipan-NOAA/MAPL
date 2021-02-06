#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module CollectionMod
   use, intrinsic :: iso_fortran_env, only: INT64
   use ESMF
   use NUOPC
   use yaFyaml
   use gFTL_StringVector
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod

   use GroupMod

   implicit none
   private

   public Collection

   character(*), parameter :: template_key  = 'template'
   character(*), parameter :: frequency_key = 'frequency'
   character(*), parameter :: groups_key    = 'groups'

   type :: Collection
      private
      character(:), allocatable :: name

      ! TODO: these should be their own types
      character(:), allocatable :: template
      character(:), allocatable :: frequency

      type(StringVector) :: groups
      type(Group)        :: fields
   contains
      procedure :: import_collection
      procedure :: import_groups
   end type Collection
contains
   subroutine import_collection(this, name, config, rc)
      class(Collection),   intent(inout) :: this
      character(*),        intent(in   ) :: name
      type(Configuration), intent(inout) :: config
      integer, optional,   intent(  out) :: rc

      character(:), pointer       :: key
      type(ConfigurationIterator) :: iter
      type(Configuration)         :: sub_config

      integer :: status

      this%name = name

      ! Import the fields
      call this%fields%import_group(config, __RC__)

      ! Import everything else
      iter = config%begin()
      do while(iter /= config%end())
         key => iter%key()

         select case (key)
         case (template_key)
            this%template = iter%value()
         case (frequency_key)
            this%frequency = iter%value()
         case (groups_key)
            sub_config = iter%value()
            call this%import_groups(sub_config)
         end select

         call iter%next()
      end do

      _RETURN(_SUCCESS)
   end subroutine import_collection

   subroutine import_groups(this, config, rc)
      class(Collection),   intent(inout) :: this
      type(Configuration), intent(inout) :: config
      integer, optional,   intent(  out) :: rc

      character(:), allocatable   :: group_name
      type(ConfigurationIterator) :: iter

      integer :: status

      if (config%is_sequence()) then
         iter = config%begin()
         do while(iter /= config%end())
            group_name = iter%value()
            call this%groups%push_back(group_name)

            call iter%next()
         end do

         status = 0
      else
         status = 1
      end if

      _RETURN(status)
   end subroutine import_groups
end module CollectionMod
