#include "MAPL_ErrLog.h"
module MAPL_ExtDataTimeSample
   use yaFyaml
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_TimeStringConversion
   implicit none
   private

   type, public :: ExtDataTimeSample
      logical :: time_interpolation
      type(ESMF_Time), allocatable :: source_time(:)
      character(:), allocatable :: extrap_outside
      character(:), allocatable :: refresh_time
      character(:), allocatable :: refresh_frequency
      character(:), allocatable :: refresh_offset
      contains
         procedure :: set_defaults
         procedure :: append_from_yaml
   end type

contains

   subroutine set_defaults(this,unusable,rc)
      class(ExtDataTimeSample), intent(inout), target :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status 
      _UNUSED_DUMMY(unusable)
      this%time_interpolation=.true.
      this%extrap_outside='none'
      this%refresh_time="00"
      this%refresh_frequency="PT0S"
      this%refresh_offset="PT0S"
      if (allocated(this%source_time)) then 
         deallocate(this%source_time,stat=status)
         _VERIFY(status)
      end if
      allocate(this%source_time(0),stat=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
   end subroutine set_defaults

   subroutine append_from_yaml(TimeSample,config,unusable,rc)
      class(ExtDataTimeSample), intent(inout), target :: TimeSample
      type(Configuration), intent(in) :: config
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      logical :: is_present
      integer :: status
      character(len=:), allocatable :: source_str
      integer :: idx
      character(len=:), allocatable :: tempc
      logical :: templ
      _UNUSED_DUMMY(unusable)

      if (allocated(tempc)) deallocate(tempc)
      call config%get(tempc,"extrapolation",is_present=is_present,rc=status)
      _VERIFY(status)
      if (is_present) TimeSample%extrap_outside=tempc

      call config%get(templ,"time_interpolation",is_present=is_present,rc=status)
      _VERIFY(status)
      if (is_present) TimeSample%time_interpolation=templ

      if (allocated(tempc)) deallocate(tempc)
      call config%get(tempc,"update_reference_time",is_present=is_present,rc=status)
      _VERIFY(status)
      if (is_present) TimeSample%refresh_time=tempc

      if (allocated(tempc)) deallocate(tempc)
      call config%get(tempc,"update_frequency",is_present=is_present,rc=status)
      _VERIFY(status)
      if (is_present) TimeSample%refresh_frequency=tempc

      if (allocated(tempc)) deallocate(tempc)
      call config%get(tempc,"update_offset",is_present=is_present,rc=status)
      _VERIFY(status)
      if (is_present) TimeSample%refresh_offset=tempc

      call config%get(source_str,"source_time",is_present=is_present,rc=status)
      _VERIFY(status)
      if (is_present) then
         if (allocated(TimeSample%source_time)) deallocate(TimeSample%source_time)
         idx = index(source_str,',')
         _ASSERT(idx/=0,'invalid specification of source_time')
         allocate(TimeSample%source_time(2))
         TimeSample%source_time(1)=string_to_esmf_time(source_str(:idx-1))
         TimeSample%source_time(2)=string_to_esmf_time(source_str(idx+1:))
      else 
         if (.not.allocated(TimeSample%source_time)) allocate(TimeSample%source_time(0))
      end if
     
      _RETURN(_SUCCESS)
   end subroutine append_from_yaml

end module MAPL_ExtDataTimeSample

module MAPL_ExtDataTimeSampleMap
   use MAPL_ExtDataTimeSample

#include "types/key_deferredLengthString.inc"
#define _value type(ExtDataTimeSample)
#define _alt

#define _map ExtDataTimeSampleMap
#define _iterator ExtDataTimeSampleMapIterator

#include "templates/map.inc"

#undef _iterator
#undef _map

#undef _alt
#undef _value

end module MAPL_ExtDataTimeSampleMap
