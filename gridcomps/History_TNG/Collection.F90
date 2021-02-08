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

   use FieldRegistryMod
   use GroupMod
   use GroupRegistryMod
   use TemplateMod
   use FrequencyMod

   implicit none
   private

   public Collection

   character(*), parameter :: template_key  = 'template'
   character(*), parameter :: frequency_key = 'frequency'
   character(*), parameter :: groups_key    = 'groups'

   type :: Collection
      private
      character(:), allocatable :: name

      type(Template)  :: template
      type(Frequency) :: frequency
      ! TODO: these should be their own types

      type(StringVector) :: groups
      type(Group)        :: fields
   contains
      procedure :: get_name

      procedure :: advertise

      procedure :: import_collection
      procedure :: import_groups
      procedure :: get_groups
   end type Collection
contains
   function get_name(this) result(collection_name)
      character(:), allocatable :: collection_name
      class(Collection), intent(in) :: this

      collection_name = this%name
   end function get_name

   subroutine advertise(this, state, unusable,&
         TransferOfferGeomObject, SharePolicyField, SharePolicyGeomObject, rc)
      class(Collection),                intent(inout) :: this
      type(ESMF_State),                 intent(inout) :: state
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      character(*),           optional, intent(in   ) :: TransferOfferGeomObject
      character(*),           optional, intent(in   ) :: SharePolicyField
      character(*),           optional, intent(in   ) :: SharePolicyGeomObject
      integer,                optional, intent(  out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      call this%fields%advertise(state, &
         TransferOfferGeomObject=TransferOfferGeomObject, &
         SharePolicyField=SharePolicyField, &
         SharePolicyGeomObject=SharePolicyGeomObject, &
         __RC__)

      _RETURN(_SUCCESS)
   end subroutine advertise

   subroutine register(this, field_registry)
      class(Collection),   intent(inout) :: this
      type(FieldRegistry), intent(inout) :: field_registry

      call this%fields%register(field_registry)
   end subroutine register

   subroutine import_collection(this, name, config, rc)
      class(Collection),   intent(inout) :: this
      character(*),        intent(in   ) :: name
      type(Configuration), intent(inout) :: config
      integer, optional,   intent(  out) :: rc

      character(:), allocatable   :: config_value
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
            config_value = iter%value()
            call this%template%initialize(config_value)
         case (frequency_key)
            config_value = iter%value()
            call this%frequency%initialize(config_value)
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

   subroutine get_groups(this, group_registry, rc)
      class(Collection),   intent(inout) :: this
      type(GroupRegistry), intent(in   ) :: group_registry
      integer, optional,   intent(  out) :: rc

      character(:), pointer      :: group_name
      type(Group),  pointer      :: group_entry
      type(StringVectorIterator) :: iter

      integer :: status

      iter = this%groups%begin()
      do while(iter /= this%groups%end())
         group_name => iter%get()

         if (group_registry%count(group_name) > 0) then
            group_entry => group_registry%at(group_name)

            call this%fields%union(group_entry, __RC__)
         end if
         call iter%next()
      end do

      _RETURN(_SUCCESS)
   end subroutine get_groups
end module CollectionMod
