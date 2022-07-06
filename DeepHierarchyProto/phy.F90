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

module PHY

  !-----------------------------------------------------------------------------
  ! PHY Component.
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
    call NUOPC_CompSpecialize(model, specLabel=label_Advance, &
      specRoutine=Advance, _RC)

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

    ! Disabling the following macro, e.g. renaming to WITHIMPORTFIELDS_disable,
    ! will result in a model component that does not advertise any importable
    ! Fields. Use this if you want to drive the model independently.
#define WITHIMPORTFIELDS
#ifdef WITHIMPORTFIELDS
    ! importable field: sea_surface_temperature
    call NUOPC_Advertise(importState, StandardName="sea_surface_temperature", name="sst", _RC)
#endif

#define WITHEXPORTFIELDS
#ifdef WITHEXPORTFIELDS

    call NUOPC_Advertise(exportState, StandardName="precipitation_flux", _RC)

    call NUOPC_Advertise(exportState, StandardName="PHYEX", _RC)

    call NUOPC_Advertise(exportState, StandardName="BOBO", _RC)
#endif
    call print_message("Advertise Phys")

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

    call print_message("Realize Phys start")
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

#define WITHIMPORTFIELDS
#ifdef WITHIMPORTFIELDS
    ! importable field: sea_surface_temperature
    call NUOPC_Realize(importState, grid=gridIn, &
      fieldName="sst", &
      selection="realize_connected_remove_others", _RC)
#endif

#define WITHEXPORTFIELDS
#ifdef WITHEXPORTFIELDS
    ! exportable field: precipitation_flux
    call NUOPC_Realize(exportState, grid=gridOut, &
      fieldName="precipitation_flux", &
      selection="realize_connected_remove_others", _RC)
    ! exportable field: PHYEX
    call NUOPC_Realize(exportState, grid=gridOut, &
      fieldName="PHYEX", &
      selection="realize_connected_remove_others", _RC)

    bobo = ESMF_FieldCreate(gridOut, ESMF_TYPEKIND_R4,name="BOBO",ungriddedLBound=[1],ungriddedUBound=[72],_RC)
    call NUOPC_Realize(exportState, bobo, _RC)


    call ESMF_StateGet(exportState, itemName="precipitation_flux", itemType=itemType, _RC)
    if (itemType==ESMF_STATEITEM_FIELD) then
      call ESMF_StateGet(exportState, field=field, itemName="precipitation_flux", _RC)
      call ESMF_FieldFill(field, dataFillScheme="sincos",  param1I4=0, param2I4=4, _RC)
    endif

    call ESMF_StateGet(exportState, itemName="PHYEX", itemType=itemType, _RC)
    if (itemType==ESMF_STATEITEM_FIELD) then
      call ESMF_StateGet(exportState, field=field, itemName="PHYEX", _RC)
      call ESMF_FieldFill(field, dataFillScheme="sincos", param1I4=0, param2I4=5, _RC)
    endif

    ! write out the Fields in the exportState
    !call NUOPC_Write(exportState, fileNamePrefix="field_phy_export_datainit_", &
      !status=ESMF_FILESTATUS_REPLACE, relaxedFlag=.true., _RC)

#endif
    call print_message("Realize Phys end")

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
    real, pointer :: ptr3d(:,:,:)

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
      preString="------>Advancing PHY from: ", unit=msgString, _RC)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, _RC)

    call ESMF_ClockPrint(clock, options="stopTime", &
      preString="---------------------> to: ", unit=msgString, _RC)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, _RC)

    ! update the export fields with data
    call ESMF_StateGet(exportState, itemName="precipitation_flux", &
      itemType=itemType, _RC)
    if (itemType==ESMF_STATEITEM_FIELD) then
      call ESMF_StateGet(exportState, field=field, itemName="precipitation_flux", _RC)
      call ESMF_FieldFill(field, dataFillScheme="sincos", param1I4=step, param2I4=4, _RC)
    endif
    call ESMF_StateGet(exportState, itemName="PHYEX", &
      itemType=itemType, _RC)
    if (itemType==ESMF_STATEITEM_FIELD) then
      call ESMF_StateGet(exportState, field=field, itemName="PHYEX", _RC)
      call ESMF_FieldFill(field, dataFillScheme="sincos", param1I4=step, param2I4=5, _RC)
    endif
    call ESMF_StateGet(exportState, itemName="BOBO",field=field, _RC)
    call ESMF_FieldGet(field,farrayPtr=ptr3d,_RC)
    ptr3d=step
    ! write out the Fields in the importState
    !status=ESMF_FILESTATUS_OLD
    !if (step==1) status=ESMF_FILESTATUS_REPLACE
    !call NUOPC_Write(importState, fileNamePrefix="field_phy_import_adv_", &
      !timeslice=step, status=status, relaxedFlag=.true., _RC)
    ! write out the Fields in the exportState
    !call NUOPC_Write(exportState, fileNamePrefix="field_phy_export_adv_", &
      !timeslice=step, status=status, relaxedFlag=.true., _RC)
    ! increment step counter
    step=step+1
    call print_message("Advance Phys")

  end subroutine

  !-----------------------------------------------------------------------------

end module
