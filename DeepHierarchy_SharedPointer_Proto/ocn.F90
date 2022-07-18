#include "error_handling.h"
!==============================================================================
! Earth System Modeling Framework
! Copyright 2002-2022, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module OCN

  !-----------------------------------------------------------------------------
  ! OCN Component.
  !-----------------------------------------------------------------------------

  use my_error_handling
  use ESMF
  use NUOPC
  use NUOPC_Model, &
    modelSS    => SetServices

  implicit none

  private

  public SetVM, SetServices

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
    !call NUOPC_CompSpecialize(model, specLabel=label_SetClock, &
      !specRoutine=SetClock, _RC)
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

    !! importable field: precipitation_flux
    !call NUOPC_Advertise(importState, &
      !StandardName="precipitation_flux", &
       !TransferOfferGeomObject="will provide", &
       !SharePolicyField="not share", &
       !SharePolicyGeomObject="not share", &
       !_RC)
    !!! importable field: air_pressure_at_sea_level
    call NUOPC_Advertise(importState, &
      StandardName="air_pressure_at_sea_level", name="pmsl", &
       TransferOfferGeomObject="will provide", &
       SharePolicyField="not share", &
       SharePolicyGeomObject="not share", &
       _RC)
    !!! importable field: surface_net_downward_shortwave_flux
    !call NUOPC_Advertise(importState, &
      !StandardName="surface_net_downward_shortwave_flux", name="rsns", &
       !TransferOfferGeomObject="will provide", &
       !SharePolicyField="not share", &
       !SharePolicyGeomObject="not share", &
       !_RC)

    ! exportable field: sea_surface_temperature
    call NUOPC_Advertise(exportState, &
      StandardName="sea_surface_temperature", name="sst", &
       TransferOfferGeomObject="will provide", &
       SharePolicyField="not share", &
       SharePolicyGeomObject="not share", &
       _RC)

    call print_message("Advertise Ocean")

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Realize(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)          :: importState, exportState
    type(ESMF_Grid)           :: gridIn
    type(ESMF_Grid)           :: gridOut
    type(ESMF_Field)          :: field
    type(ESMF_StateItem_Flag) :: itemType

    rc = ESMF_SUCCESS

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, _RC)

    ! create a Grid object for Fields
    gridIn = ESMF_GridCreateNoPeriDimUfrm(maxIndex=(/100, 50/), &
      minCornerCoord=(/0._ESMF_KIND_R8, -80._ESMF_KIND_R8/), &
      maxCornerCoord=(/360._ESMF_KIND_R8, 80._ESMF_KIND_R8/), &
      coordSys=ESMF_COORDSYS_CART, staggerLocList=(/ESMF_STAGGERLOC_CENTER/), &
      _RC)
    gridOut = gridIn ! for now out same as in

    ! importable field: precipitation_flux
    !call NUOPC_Realize(importState, grid=gridIn, &
      !fieldName="precipitation_flux", &
      !selection="realize_connected_remove_others", _RC)
    !!! importable field: air_pressure_at_sea_level
    call NUOPC_Realize(importState, grid=gridIn, &
      fieldName="pmsl", &
      selection="realize_connected_remove_others", _RC)
    !!! importable field: surface_net_downward_shortwave_flux
    !call NUOPC_Realize(importState, grid=gridIn, &
      !fieldName="rsns", &
      !selection="realize_connected_remove_others", _RC)

    ! exportable field: sea_surface_temperature
    call NUOPC_Realize(exportState, grid=gridOut, &
      fieldName="sst", &
      selection="realize_connected_remove_others", _RC)

    !call ESMF_StateGet(exportState, itemName="sst", itemType=itemType, _RC)
    !if (itemType==ESMF_STATEITEM_FIELD) then
      !call ESMF_StateGet(exportState, field=field, itemName="sst", _RC)
      !call ESMF_FieldFill(field, dataFillScheme="sincos", &
        !param1I4=0, param2I4=1, _RC)
    !endif

    call print_message("Realize Ocean")

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetClock(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_TimeInterval)       :: stabilityTimeStep

    rc = ESMF_SUCCESS

    ! query for clock
    call NUOPC_ModelGet(model, modelClock=clock, _RC)

    ! initialize internal clock
    ! here: parent Clock and stability timeStep determine actual model timeStep
    !TODO: stabilityTimeStep should be read in from configuation
    !TODO: or computed from internal Grid information
    call ESMF_TimeIntervalSet(stabilityTimeStep, m=15, _RC) ! 15 minute steps
    call NUOPC_CompSetClock(model, clock, stabilityTimeStep, _RC)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Advance(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Time)             :: currTime
    type(ESMF_TimeInterval)     :: timeStep
    integer, save               :: step=1
    type(ESMF_Field)            :: field
    type(ESMF_FileStatus_Flag)  :: status
    character(len=160)          :: msgString

    rc = ESMF_SUCCESS

    ! query for clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=clock, importState=importState, &
      exportState=exportState, _RC)

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep

    ! Because of the way that the internal Clock was set in SetClock(),
    ! its timeStep is likely smaller than the parent timeStep. As a consequence
    ! the time interval covered by a single parent timeStep will result in
    ! multiple calls to the Advance() routine. Every time the currTime
    ! will come in by one internal timeStep advanced. This goes until the
    ! stopTime of the internal Clock has been reached.

    call print_pointer_address(exportState,"ocn exp",_RC)
    call print_pointer_address(importState,"ocn imp",_RC)

    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing OCN from: ", unit=msgString, _RC)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, _RC)

    call ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep, _RC)

    call ESMF_TimePrint(currTime + timeStep, &
      preString="---------------------> to: ", unit=msgString, _RC)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, _RC)

    step=step+1
    call print_next_time(clock,"Advanced OCN to: ")

  end subroutine

  !-----------------------------------------------------------------------------

end module
