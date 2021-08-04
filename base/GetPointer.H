#ifdef NAME_
#undef NAME_
#endif

#ifdef NAMESTR_
#undef NAMESTR_
#endif

#define NAME_ ESMFL_StateGetPtrToData
#define NAMESTR_ 'ESMFL_StateGetPtrToData'

#include "overload.macro"

 subroutine SUB_(STATE, PTR, NAME, alloc, notFoundOK, RC)
   type(ESMF_State),  intent(INOUT) :: STATE
   real(KIND=EKIND_), pointer :: PTR DIMENSIONS_
   character(len=*),  intent(IN   ) :: NAME
   logical, optional, intent(IN   ) :: alloc
   logical, optional, intent(IN   ) :: notFoundOK
   integer, optional, intent(  OUT) :: RC
   
   integer                         :: STATUS

   type (ESMF_FieldBundle)         :: bundle
   type (ESMF_Field)               :: field
   logical                         :: IsOk
   integer                         :: loc
   type(ESMF_FieldStatus_Flag)     :: fieldStatus
   type(ESMF_StateItem_Flag)       :: itemType

#define IDENTITY(x)x
#define concat(x,y) IDENTITY(x)IDENTITY(y)
   !$omp critical (concat(GetPointer_)concat(RANK_))
   NULLIFY(ptr)
   if (present(notFoundOK)) then
      IsOk = notFoundOK
   else
      IsOk = .false.
   end if

! Get Field from state

   loc = index(name,';;')

   do
   if(loc/=0) then
      call ESMF_StateGet(state, name(:loc-1), itemType=itemType, rc=status)
      if (status /= 0) exit
      if (itemType /= ESMF_STATEITEM_FIELDBUNDLE .and. IsOk) then
         exit
      else
         call ESMF_StateGet(state, name(:loc-1), Bundle, rc=status)
         _VERIFY(STATUS)
      end if

      call ESMF_StateGet(state, name(loc+2:), itemType=itemType, rc=status)
      if (status /= 0) exit
      if (itemType /= ESMF_STATEITEM_FIELD .and. IsOk) then
         exit
      else
         call ESMF_StateGet(state, name(loc+2:), Field, rc=status)
         if (status /= 0) exit
      end if
   else
      call ESMF_StateGet(state, name, itemType=itemType, rc=status)
      if (status /= 0) exit
      if (itemType /= ESMF_STATEITEM_FIELD .and. IsOk) then
         exit
      else
         call ESMF_StateGet(state, name, Field, rc=status)
         if (status /= 0) exit
      end if
   end if

   call ESMF_FieldGet(field, status=fieldStatus, rc=status)
   if (status /= 0) exit
     if (fieldStatus == ESMF_FIELDSTATUS_COMPLETE) then
      call ESMF_FieldGet(field, 0, Ptr, rc=status)
      if (status /= 0) exit
   end if

   if (present(alloc)) then
      if (alloc .and. (.not. associated(PTR))) then
         call MAPL_AllocateCoupling(Field, rc=STATUS)
         if (status /= 0) exit
         call ESMF_FieldGet(field, 0, Ptr, rc=status)
         if (status /= 0) exit
      endif
   endif

!ALT I dont think the next lines are needed anymore
!#if 0
   block
     integer :: DIMS
     call ESMF_AttributeGet(field, name='VLOCATION', value=DIMS, rc=status)
     if (STATUS==ESMF_SUCCESS) then
        if (DIMS == MAPL_VLocationEdge .and. associated(ptr)) then
#if RANK_ == 3
           call AdjustPtrBounds(ptr, ptr, 1, size(ptr,1), 1, size(ptr,2), 0, size(ptr,3)-1)
#elif RANK_ == 1
           call AdjustPtrBounds(ptr, ptr, 0, size(ptr,1)-1)
#endif
        end if
     end if
   end block
!#endif
     exit
   end do
   !$omp end critical (concat(GetPointer_)concat(RANK_))

   
   _RETURN(ESMF_SUCCESS)
   
 end subroutine SUB_

#undef DIMENSIONS_
#undef RANK_
#undef RANKSTR_
   
