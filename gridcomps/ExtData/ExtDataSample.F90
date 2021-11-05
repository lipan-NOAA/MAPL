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

      integer :: status
      character(len=:), allocatable :: source_str
      integer :: idx
      character(len=:), allocatable :: tempc
      _UNUSED_DUMMY(unusable)

      if (allocated(tempc)) deallocate(tempc)
      tempc = get_string_with_default(config,"extrapolation") 
      TimeSample%extrap_outside=tempc

      if (config%has("time_interpolation")) then
         TimeSample%time_interpolation = config%of("time_interpolation")
      else
         TimeSample%time_interpolation = .true.
      end if

      if (allocated(tempc)) deallocate(tempc)
      tempc = get_string_with_default(config,"update_reference_time") 
      TimeSample%refresh_time=tempc

      if (allocated(tempc)) deallocate(tempc)
      tempc = get_string_with_default(config,"update_frequency") 
      TimeSample%refresh_frequency=tempc

      if (allocated(tempc)) deallocate(tempc)
      tempc = get_string_with_default(config,"update_offset") 
      TimeSample%refresh_offset=tempc

      if (config%has("source_time")) then
         call config%get(source_str,"source_time",rc=status)
         _VERIFY(status)
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

      contains

         function get_string_with_default(config,selector) result(string)
            type(configuration), intent(in) :: config
            character(len=*),intent(in) :: selector
            character(len=:), allocatable :: string
            if (config%has(selector)) then
               string=config%of(selector)
            else
               string=''
            end if
         end function
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
