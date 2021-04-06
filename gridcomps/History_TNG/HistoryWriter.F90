#include "MAPL_Generic.h"
#include "unused_dummy.H"
#include "NUOPC_ErrLog.h"

module HistoryWriterMod
   use ESMF
   use NUOPC
   use NUOPC_Model
   use CollectionMod
   use MAPL_BaseMod

   implicit None
   private

   public HistoryWriter

   type :: HistoryWriter
      private
      type(collection) :: hist_collection
   contains
      procedure :: initialize
      procedure :: advertise
      procedure :: acceptTransfer
      procedure :: realizeAccepted
      procedure :: write_collection
   end type HistoryWriter

contains

   subroutine initialize(this, mycollection)
      class(HistoryWriter), intent(  out) :: this
      type(collection),     intent(in   ) :: mycollection

      this%hist_collection = mycollection

   end subroutine initialize

   subroutine advertise(this, model, rc)
      class(HistoryWriter),  intent(inout) :: this
      type(ESMF_GridComp)               :: model
      integer,            intent(  out) :: rc

      type(ESMF_State) :: import_state
      type(ESMF_State) :: export_state

      rc = ESMF_SUCCESS

      call NUOPC_ModelGet(model, importState=import_state, exportState=export_state, rc=rc)
      VERIFY_NUOPC_(rc)

      ! Advertise the GEOS fields as exports for History to receive
      call this%hist_collection%advertise(import_state, TransferOfferGeomObject="cannot provide",rc=rc)
      VERIFY_NUOPC_(rc)

   end subroutine advertise

   subroutine realizeAccepted(this, model, rc)
      class(HistoryWriter),  intent(inout) :: this
      type(ESMF_GridComp)               :: model
      integer,            intent(  out) :: rc

      type(ESMF_State) :: import_state
      type(ESMF_State) :: export_state

      rc = ESMF_SUCCESS

      call NUOPC_ModelGet(model, importState=import_state, exportState=export_state, rc=rc)
      VERIFY_NUOPC_(rc)

      ! Advertise the GEOS fields as exports for History to receive
      call this%hist_collection%realize(import_state, rc=rc)
      VERIFY_NUOPC_(rc)

   end subroutine realizeAccepted

   subroutine acceptTransfer(this, model, rc)
      class(HistoryWriter),  intent(inout) :: this
      type(ESMF_GridComp)               :: model
      integer,            intent(  out) :: rc

      type(ESMF_State) :: import_state
      type(ESMF_State) :: export_state

      rc = ESMF_SUCCESS

      call NUOPC_ModelGet(model, importState=import_state, rc=rc)
      VERIFY_NUOPC_(rc)

      call adjustAcceptedGeom(import_State, rc=rc)
      VERIFY_NUOPC_(rc)

    contains ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine adjustAcceptedGeom(state, rc)
      ! Look at all of the fields in state, including in nested states. Adjust
      ! the distribution of the accepted geom object to a 1 DE/PET distribution.
      type(ESMF_State)  :: state
      integer, optional :: rc
      ! local variables
      integer                                 :: itemCount, item
      type(ESMF_Field)                        :: field
      character(len=80)                       :: transferAction
      character(len=80), allocatable          :: itemNameList(:)
      type(ESMF_StateItem_Flag), allocatable  :: itemTypeList(:)
      type(ESMF_GeomType_Flag)                :: geomtype
      type(ESMF_Grid)                         :: grid
      type(ESMF_Mesh)                         :: mesh
      character(160)                          :: msgString
      type(ESMF_DistGrid)                     :: distgrid
      integer                                 :: dimCount, tileCount
      integer, allocatable                    :: minIndexPTile(:,:), maxIndexPTile(:,:)
      type(ESMF_StateIntent_Flag)             :: stateIntent
      character(len=80)                       :: transferActionAttr

      if (present(rc)) rc = ESMF_SUCCESS

      call ESMF_StateGet(state, nestedFlag=.true., itemCount=itemCount, &
        stateIntent=stateIntent, rc=rc)
      VERIFY_NUOPC_(rc)

      if (stateIntent==ESMF_STATEINTENT_EXPORT) then
        transferActionAttr="ProducerTransferAction"
      elseif (stateIntent==ESMF_STATEINTENT_IMPORT) then
        transferActionAttr="ConsumerTransferAction"
      else
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="The stateIntent must either be IMPORT or EXPORT here.", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return  ! bail out
      endif

      allocate(itemNameList(itemCount), itemTypeList(itemCount))

      call ESMF_StateGet(state, nestedFlag=.true., &
        itemNameList=itemNameList, itemTypeList=itemTypeList, rc=rc)
      VERIFY_NUOPC_(rc)

      do item=1, itemCount
        if (itemTypeList(item)==ESMF_STATEITEM_FIELD) then
          ! this is a field -> get more info
          call ESMF_StateGet(state, field=field, itemName=itemNameList(item), &
            rc=rc)
          VERIFY_NUOPC_(rc)
          call NUOPC_GetAttribute(field, name=transferActionAttr, &
            value=transferAction, rc=rc)
          VERIFY_NUOPC_(rc)
          if (trim(transferAction)=="accept") then
            ! the Connector instructed the model to accept geom object
            ! -> find out which type geom object the field holds
            call ESMF_FieldGet(field, geomtype=geomtype, rc=rc)
            VERIFY_NUOPC_(rc)
            if (geomtype==ESMF_GEOMTYPE_GRID) then
              ! empty field holds a Grid with DistGrid
              call ESMF_FieldGet(field, grid=grid, rc=rc)
              VERIFY_NUOPC_(rc)
              ! access the DistGrid
              call ESMF_GridGet(grid, distgrid=distgrid, rc=rc)
              VERIFY_NUOPC_(rc)
              ! Create a custom DistGrid, based on the minIndex, maxIndex of the
              ! accepted DistGrid, but with a default regDecomp for the current VM
              ! that leads to 1DE/PET.
              ! get dimCount and tileCount
              call ESMF_DistGridGet(distgrid, dimCount=dimCount, &
                tileCount=tileCount, rc=rc)
              VERIFY_NUOPC_(rc)
              ! allocate minIndexPTile and maxIndexPTile accord. to dimCount
              ! and tileCount
              allocate(minIndexPTile(dimCount, tileCount), &
                maxIndexPTile(dimCount, tileCount))
              ! get minIndex and maxIndex arrays
              call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, &
                maxIndexPTile=maxIndexPTile, rc=rc)
              VERIFY_NUOPC_(rc)
              ! create the new DistGrid with the same minIndexPTile and
              ! maxIndexPTile, but with a default regDecompPTile
              distgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile, &
                maxIndexPTile=maxIndexPTile, rc=rc)
              VERIFY_NUOPC_(rc)
              ! Create a new Grid on the new DistGrid and swap it in the Field
              grid = ESMF_GridCreate(distgrid, rc=rc)
              VERIFY_NUOPC_(rc)
              call ESMF_FieldEmptySet(field, grid=grid, rc=rc)
              VERIFY_NUOPC_(rc)
              deallocate(minIndexPTile, maxIndexPTile)
            else
              call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
                msg="Unsupported geom object found in "// &
                trim(itemNameList(item)), &
                line=__LINE__, &
                file=__FILE__, &
                rcToReturn=rc)
              return ! bail out
            endif
          endif
        endif
      enddo

      deallocate(itemNameList, itemTypeList)

    end subroutine

  end subroutine acceptTransfer
   
   subroutine write_collection(this, model, rc)
      class(HistoryWriter),  intent(inout) :: this
      type(ESMF_GridComp)               :: model
      integer,            intent(  out) :: rc

      type(ESMF_State) :: import_state
      type(ESMF_State) :: export_state

    type(ESMF_Field) :: field
    real, pointer :: ptr2d(:,:),ptr3d(:,:,:)
    integer :: itemCount
    character(len=ESMF_MAXSTR),allocatable :: vname(:)
    character(len=50) :: units
    integer :: i,mypet,rank
    type(ESMF_VM) :: vm
    type(ESMF_Clock) :: clock
    type(ESMF_Time) :: time
    character(len=ESMF_MAXSTR) :: cname

    rc = ESMF_SUCCESS
    call ESMF_VMGetCurrent(vm)
    call ESMF_VMGet(vm,rc=rc)
    VERIFY_NUOPC_(rc)
    call ESMF_VMGet(vm,localPet=mypet,rc=rc)
    VERIFY_NUOPC_(rc)


    call NUOPC_ModelGet(model, importState=import_state, exportState=export_state, &
        modelClock=clock, rc=rc)
    VERIFY_NUOPC_(rc)
    call NUOPC_CompAttributeGet(model,name="CompLabel",value=cname,rc=rc)
    VERIFY_NUOPC_(rc)
    
    call ESMF_StateGet(import_state,itemCount=itemCount,rc=rc)
    VERIFY_NUOPC_(rc)
    allocate(vname(itemCount))
    call ESMF_StateGet(import_State,itemNameList=vname)

    do i=1,itemCount
     
       if (mypet==0) write(*,*)trim(cname),' found: ',trim(vname(i))
       call ESMF_StateGet(import_State,trim(vname(i)),field)
       call ESMF_FieldGet(field,rank=rank)
       if (rank==2) then
          call ESMF_FieldGet(field,0,ptr2d,rc=rc)
          if (mypet==0) write(*,*)'writer size: ',shape(ptr2d)
          if (mypet==0) write(*,*)'writer writing: ',maxval(ptr2d)
       else
          call ESMF_FieldGet(field,0,ptr3d,rc=rc)
          if (mypet==0) write(*,*)'writer size: ',shape(ptr3d)
          if (mypet==0) write(*,*)'writer writing: ',maxval(ptr3d)
       end if

    enddo

   end subroutine write_collection

end module HistoryWriterMod
