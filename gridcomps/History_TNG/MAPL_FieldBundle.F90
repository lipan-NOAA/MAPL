#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module MAPL_FieldBundle
   use, intrinsic :: iso_fortran_env, only: INT64
   use ESMF
   use NUOPC
   use yaFyaml
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod

   use MAPL_FieldBundleEntry
   use MAPL_FieldBundleEntryMap

   use MAPL_FieldRegistryEntry
   use MAPL_FieldRegistry

   implicit none
   private

   public FieldBundle

   character(*), parameter :: alias_key = 'alias'
   character(*), parameter :: units_key = 'units'

   type :: FieldBundle
      private
      type(FieldBundleEntryMap) :: map
   contains
      procedure :: size
      procedure :: count
      procedure :: at
      procedure :: insert

      procedure :: advertise
      procedure :: register

      procedure :: import_bundle
      procedure :: import_component
      procedure :: import_field
   end type FieldBundle
contains
   integer(kind=INT64) function size(this)
      class(FieldBundle), intent(in) :: this

      size = this%map%size()
   end function size

   integer(kind=INT64) function count(this, key)
      class(FieldBundle), intent(in) :: this
      character(*),       intent(in) :: key

      count = this%map%count(key)
   end function count

   function at(this, key) result(field_entry)
      class(FieldBundleEntry), pointer :: field_entry
      class(FieldBundle), intent(in) :: this
      character(*),       intent(in) :: key

      field_entry => this%map%at(key)
   end function at

   subroutine insert(this, field_entry)
      class(FieldBundle),      intent(inout) :: this
      class(FieldBundleEntry), intent(inout) :: field_entry

      character(:), allocatable :: name

      call this%map%insert(field_entry%name(), field_entry)
   end subroutine insert

   subroutine advertise(this, state, unusable,&
         TransferOfferGeomObject, SharePolicyField, SharePolicyGeomObject, rc)
      class(FieldBundle),               intent(inout) :: this
      type(ESMF_State),                 intent(inout) :: state
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      character(*),           optional, intent(in   ) :: TransferOfferGeomObject
      character(*),           optional, intent(in   ) :: SharePolicyField
      character(*),           optional, intent(in   ) :: SharePolicyGeomObject
      integer,                optional, intent(  out) :: rc

      class(FieldBundleEntry), pointer  :: field_entry
      type(FieldBundleEntryMapIterator) :: iter

      integer :: status

      _UNUSED_DUMMY(unusable)

      iter = this%map%begin()
      do while(iter /= this%map%end())
         field_entry => iter%value()
         call field_entry%advertise(state, &
            TransferOfferGeomObject=TransferOfferGeomObject, &
            SharePolicyField=SharePolicyField, &
            SharePolicyGeomObject=SharePolicyGeomObject, &
            __RC__)

         call iter%next()
      end do

      _RETURN(_SUCCESS)
   end subroutine advertise

   subroutine register(this, field_registry)
      class(FieldBundle),  intent(inout) :: this
      type(FieldRegistry), intent(inout) :: field_registry

      class(FieldBundleEntry), pointer  :: field_entry
      type(FieldRegistryEntry)          :: registry_entry
      type(FieldBundleEntryMapIterator) :: iter

      iter = this%map%begin()
      do while(iter /= this%map%end())
         field_entry    => iter%value()
         registry_entry =  field_entry%registry_entry()
         call field_registry%insert(registry_entry)

         call iter%next()
      end do
   end subroutine register

   subroutine import_bundle(this, config, rc)
      class(FieldBundle),  intent(inout) :: this
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

         call this%import_component(component_name, sub_config, __RC__)
         call iter%next()
      end do

      _RETURN(_SUCCESS)
   end subroutine import_bundle

   subroutine import_component(this, component_name, config, rc)
      class(FieldBundle),  intent(inout) :: this
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

         call this%import_field(short_name, component_name, sub_config, __RC__)
         call iter%next()
      end do

      _RETURN(_SUCCESS)
   end subroutine import_component

   subroutine import_field(this, short_name, component_name, config, unusable, rc)
      class(FieldBundle),               intent(inout) :: this
      character(*),                     intent(in   ) :: short_name
      character(*),                     intent(in   ) :: component_name
      type(Configuration),              intent(inout) :: config
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      character(:), allocatable  :: alias
      character(:), allocatable  :: units
      type(FieldBundleEntry) :: field_entry

      character(:), pointer       :: key
      type(ConfigurationIterator) :: iter
      type(Configuration)         :: sub_config

      integer :: status

      _UNUSED_DUMMY(unusable)

      call field_entry%initialize(short_name, component_name)

      iter =  config%begin()
      do while(iter /= config%end())
         key => iter%key()

         select case (key)
         case (alias_key)
            alias = iter%value()
            call field_entry%set_alias_name(alias, __RC__)
         case (units_key)
            units = iter%value()
            call field_entry%set_units(units, __RC__)
         end select

         call iter%next()
      end do

      call this%insert(field_entry)
      _RETURN(_SUCCESS)
   end subroutine import_field
end module MAPL_FieldBundle
