#include "error_handling.h"
!==============================================================================
! Earth System Modeling Framework
! Copyright 2002-2022, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, GeoMOISTsical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module MOIST

  !-----------------------------------------------------------------------------
  ! MOIST Component.
  !-----------------------------------------------------------------------------

  use my_error_handling
  use ESMF
  use NUOPC
  use NUOPC_Model, &
    modelSS      => SetServices

  implicit none

  private

  public SetServices

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    ! derive from NUOPC_Model
    call NUOPC_CompDerive(model, modelSS, _RC)

    ! specialize model
    call NUOPC_CompSpecialize(model, specLabel=label_Advertise, &
      specRoutine=Advertise, _RC)
    call NUOPC_CompSpecialize(model, specLabel=label_RealizeProvided, &
      specRoutine=Realize, _RC)
    call NUOPC_CompSpecialize(model, specLabel=label_RealizeAccepted, &
      specRoutine=RealizeAccepted, _RC)
    call NUOPC_CompSpecialize(model, specLabel=label_Advance, &
      specRoutine=Advance, _RC)
    call NUOPC_CompSpecialize(model, specLabel=label_CheckImport, &
      specRoutine=NUOPC_NoOp, _RC)
    call NUOPC_CompSpecialize(model, specLabel=label_TimestampExport, &
      specRoutine=NUOPC_NoOp, _RC)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Advertise(model, rc) 
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)        :: importState, exportState

    rc = ESMF_SUCCESS

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, _RC)

    call NUOPC_Advertise(exportState, StandardName="MOISTEX", &
       TransferOfferGeomObject="will provide", &
       SharePolicyField="share", &
       SharePolicyGeomObject="not share", &
       _RC)
    call NUOPC_Advertise(importState, StandardName="RADEX", &
       TransferOfferGeomObject="cannot provide", &
       SharePolicyField="share", &
       SharePolicyGeomObject="not share", &
       _RC)

    call NUOPC_Advertise(exportState, StandardName="BOBO", &
       TransferOfferGeomObject="will provide", &
       SharePolicyField="share", &
       SharePolicyGeomObject="not share", &
       _RC)
    call print_message("Advertise MOIST")

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Realize(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)          :: importState, exportState
    type(ESMF_Grid)           :: gridIn
    type(ESMF_Grid)           :: gridOut
    type(ESMF_Field)          :: field, bobo
    type(ESMF_StateItem_Flag) :: itemtype

    call print_message("Realize MOIST start")
    rc = ESMF_SUCCESS

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, _RC)

    ! create a Grid object for Fields
    gridIn = ESMF_GridCreateNoPeriDimUfrm(maxIndex=(/100, 100/), &
      minCornerCoord=(/0._ESMF_KIND_R8, -50._ESMF_KIND_R8/), &
      maxCornerCoord=(/360._ESMF_KIND_R8, 90._ESMF_KIND_R8/), &
      coordSys=ESMF_COORDSYS_CART, staggerLocList=(/ESMF_STAGGERLOC_CENTER/), &
      _RC)
    gridOut = gridIn ! for now out same as in

    ! exportable field: MOISTEX
    call NUOPC_Realize(exportState, grid=gridOut, &
      fieldName="MOISTEX", &
      selection="realize_connected_remove_others", _RC)

    bobo = ESMF_FieldCreate(gridOut, ESMF_TYPEKIND_R4,name="BOBO",ungriddedLBound=[1],ungriddedUBound=[72],_RC)
    call NUOPC_Realize(exportState, bobo, _RC)


    call print_message("Realize MOISTs end")

   !call NUOPC_SetAttribute(exportState, "FieldTransferPolicy", "transferAll", _RC)

  end subroutine

  subroutine RealizeAccepted(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)          :: importState, exportState
    type(ESMF_Grid)           :: gridIn
    type(ESMF_Grid)           :: gridOut
    type(ESMF_Field)          :: field, bobo
    type(ESMF_StateItem_Flag) :: itemtype

    call print_message("Realize MOIST start")
    rc = ESMF_SUCCESS

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, _RC)

    ! create a Grid object for Fields
    gridIn = ESMF_GridCreateNoPeriDimUfrm(maxIndex=(/100, 100/), &
      minCornerCoord=(/0._ESMF_KIND_R8, -50._ESMF_KIND_R8/), &
      maxCornerCoord=(/360._ESMF_KIND_R8, 90._ESMF_KIND_R8/), &
      coordSys=ESMF_COORDSYS_CART, staggerLocList=(/ESMF_STAGGERLOC_CENTER/), &
      _RC)
    gridOut = gridIn ! for now out same as in

    call NUOPC_Realize(importState,  &
      fieldName="RADEX",_RC)

    call print_message("Realize MOIST end")

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Advance(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    integer, save               :: step=1
    type(ESMF_Field)            :: field
    type(ESMF_FileStatus_Flag)  :: status
    type(ESMF_StateItem_Flag)   :: itemType
    character(len=160)          :: msgString
    real(kind=ESMF_KIND_R4), pointer :: ptr3d(:,:,:)
    real(kind=ESMF_KIND_R8), pointer :: ptr2d(:,:)

    rc = ESMF_SUCCESS

    ! query for clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=clock, importState=importState, &
      exportState=exportState, _RC)

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep

    ! Because of the way that the internal Clock was set by default,
    ! its timeStep is equal to the parent timeStep. As a consequence the
    ! currTime + timeStep is equal to the stopTime of the internal Clock
    ! for this call of the Advance() routine.

    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing MOIST from: ", unit=msgString, _RC)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, _RC)

    call ESMF_ClockPrint(clock, options="stopTime", &
      preString="---------------------> to: ", unit=msgString, _RC)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, _RC)

    call print_pointer_address(exportState,"MOIST exp",_RC)
    call print_pointer_address(importState,"MOIST imp",_RC)
    call ESMF_StateGet(exportState, itemName="BOBO",field=field, _RC)
    call ESMF_FieldGet(field,farrayPtr=ptr3d,_RC)
    ptr3d=step
    step=step+1
    call print_next_time(clock,"Advanced MOIST to: ")

  end subroutine

  !-----------------------------------------------------------------------------

end module
