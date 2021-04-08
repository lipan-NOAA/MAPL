#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module MAPL_ExtDataDerived
   use yaFyaml
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   implicit none
   private

   type, public :: ExtDataDerived
      character(:), allocatable :: expression
      character(:), allocatable :: sample_key
      contains
         procedure :: display
         procedure :: append_from_yaml
         procedure :: set_defaults
   end type

contains

   subroutine set_defaults(this,unusable,rc)
      class(ExtDataDerived), intent(inout), target :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)
      this%expression=''
      _RETURN(_SUCCESS)
   end subroutine set_defaults

   recursive  subroutine append_from_yaml(rule,config,key,unusable,rc)
      class(ExtDataDerived), intent(inout), target :: rule
      type(Configuration), intent(in) :: config
      character(len=*), intent(in) :: key
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      logical :: is_present
      integer :: status
      character(len=:), allocatable :: tempc
      _UNUSED_DUMMY(unusable)


      if (allocated(tempc)) deallocate(tempc)
      call config%get(tempc,"function",is_present=is_present,rc=status)
      _VERIFY(status)
      _ASSERT(is_present,"no expression found in derived entry") 
      if (is_present) rule%expression=tempc

      if (allocated(tempc)) deallocate(tempc)
      call config%get(tempc,"sample",is_present=is_present,rc=status)
      _VERIFY(status)
      if (is_present) rule%sample_key=tempc

      _RETURN(_SUCCESS)
   end subroutine append_from_yaml

   subroutine display(this)
      class(ExtDataDerived) :: this
      write(*,*)"function: ",trim(this%expression)
   end subroutine display
 
end module MAPL_ExtDataDerived

module MAPL_ExtDataDerivedMap
   use MAPL_ExtDataDerived

#include "types/key_deferredLengthString.inc"
#define _value type(ExtDataDerived)
#define _alt

#define _map ExtDataDerivedMap
#define _iterator ExtDataDerivedMapIterator

#include "templates/map.inc"

#undef _iterator
#undef _map

#undef _alt
#undef _value

end module MAPL_ExtDataDerivedMap
