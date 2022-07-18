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

module RAD

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
    !call NUOPC_CompAttributeSet(model, name="HierarchyProtocol", value="PushUpAllExportsAndUnsatisfiedImports", _RC)
    !call NUOPC_CompAttributeSet(model, name="HierarchyProtocol", value="ConnectProvidedFields", _RC)

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
    call NUOPC_Advertise(importState, StandardName="sea_surface_temperature", name="sst", _RC)
#endif

#define WITHEXPORTFIELDS
#ifdef WITHEXPORTFIELDS
    !call NUOPC_Advertise(exportState, StandardName="RADEX", _RC)
#endif
    call print_message("Advertise Rad")

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
    type(ESMF_StateItem_Flag) :: itemtype

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
    call NUOPC_Realize(importState, grid=gridIn, &
      fieldName="sst", &
      selection="realize_connected_remove_others", _RC)
#endif

#define WITHEXPORTFIELDS
#ifdef WITHEXPORTFIELDS
    !call NUOPC_Realize(exportState, grid=gridIn, &
      !fieldName="RADEX", &
      !selection="realize_connected_remove_others", _RC)

#endif
    call print_message("Realize Rad")

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

    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing RAD from: ", unit=msgString, _RC)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, _RC)

    call ESMF_ClockPrint(clock, options="stopTime", &
      preString="---------------------> to: ", unit=msgString, _RC)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, _RC)

    step=step+1
    call print_next_time(clock,"Advanced Rad to: ")

  end subroutine

  !-----------------------------------------------------------------------------

end module
