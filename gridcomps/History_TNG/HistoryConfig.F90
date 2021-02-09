#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module HistoryConfigMod
   use, intrinsic :: iso_fortran_env, only: INT64
   use ESMF
   use NUOPC
   use yaFyaml
   use gFTL_StringVector
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod

   Use FieldRegistryMod
   use GroupRegistryMod
   use CollectionMod
   use CollectionRegistryMod

   implicit none
   private

   public HistoryConfig

   character(*), parameter :: enabled_key     = 'Enabled'
   character(*), parameter :: groups_key      = 'Groups'
   character(*), parameter :: collections_key = 'Collections'

   type :: HistoryConfig
      private
      type(StringVector) :: enabled

      type(FieldRegistry)      :: fields
      type(GroupRegistry)      :: groups
      type(CollectionRegistry) :: collections
   contains
      procedure :: import_yaml
      procedure :: import_enabled
      procedure :: finish_import

      procedure :: fill_collection_groups
      procedure :: fill_field_registry
   end type HistoryConfig
contains
   subroutine import_yaml(this, config, rc)
      class(HistoryConfig), intent(inout) :: this
      type(Configuration),  intent(inout) :: config
      integer, optional,    intent(  out) :: rc

      character(:), pointer       :: key
      type(ConfigurationIterator) :: iter
      type(Configuration)         :: sub_config

      integer :: status

      iter = config%begin()
      do while(iter /= config%end())
         key => iter%key()

         select case (key)
         case (enabled_key)
            sub_config = iter%value()
            call this%import_enabled(sub_config, __RC__)
         case (groups_key)
            sub_config = iter%value()
            call this%groups%import_groups(sub_config, __RC__)
         case (collections_key)
            sub_config = iter%value()
            call this%collections%import_collections(sub_config, __RC__)
         end select

         call iter%next()
      end do

      _RETURN(_SUCCESS)
   end subroutine import_yaml

   subroutine import_enabled(this, config, rc)
      class(HistoryConfig), intent(inout) :: this
      type(Configuration),  intent(inout) :: config
      integer, optional,    intent(  out) :: rc

      character(:), allocatable   :: collection_name
      type(ConfigurationIterator) :: iter

      integer :: status

      if (config%is_sequence()) then
         iter = config%begin()
         do while(iter /= config%end())
            collection_name = iter%value()
            call this%enabled%push_back(collection_name)

            call iter%next()
         end do

         status = 0
      else
         status = 1
      end if

      _RETURN(status)
   end subroutine import_enabled

   subroutine finish_import(this, rc)
      class(HistoryConfig), intent(inout) :: this
      integer, optional,    intent(  out) :: rc

      integer :: status

      call this%fill_collection_groups(__RC__)
      call this%fill_field_registry(__RC__)

      _RETURN(_SUCCESS)
   end subroutine finish_import

   subroutine fill_collection_groups(this, rc)
      class(HistoryConfig), intent(inout) :: this
      integer, optional,    intent(  out) :: rc

      character(:), pointer      :: collection_name
      type(StringVectorIterator) :: iter
      type(Collection), pointer  :: collection_entry

      integer :: status

      status = 0
      iter = this%enabled%begin()
      do while(iter /= this%enabled%end())
         collection_name => iter%get()

         if (this%collections%count(collection_name) > 0) then
            collection_entry => this%collections%at(collection_name)

            call collection_entry%fill_groups(this%groups, __RC__)
         else
            status = 1
            exit
         end if

         call iter%next()
      end do

      _RETURN(status)
   end subroutine fill_collection_groups

   subroutine fill_field_registry(this, rc)
      class(HistoryConfig), intent(inout) :: this
      integer, optional,    intent(  out) :: rc

      character(:), pointer      :: collection_name
      type(StringVectorIterator) :: iter
      type(Collection), pointer  :: collection_entry

      integer :: status

      status = 0
      iter = this%enabled%begin()
      do while(iter /= this%enabled%end())
         collection_name => iter%get()

         if (this%collections%count(collection_name) > 0) then
            collection_entry => this%collections%at(collection_name)

            call collection_entry%register(this%fields)
         else
            status = 1
            exit
         end if

         call iter%next()
      end do

      _RETURN(status)
   end subroutine fill_field_registry
end module HistoryConfigMod
