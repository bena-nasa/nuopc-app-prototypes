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
  use MAPL_redu
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
      specRoutine=RealizeProvided, _RC)
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
    type(ESMF_Config) :: config
    character(len=ESMF_MAXSTR) :: share,provide

    rc = ESMF_SUCCESS

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, _RC)

    config=ESMF_ConfigCreate()
    call ESMF_ConfigLoadFile(config,filename="rad_input.rc",_RC)
    call ESMF_ConfigGetAttribute(config,share,Label="share:",default="share",_RC)
    call ESMF_ConfigGetAttribute(config,provide,Label="provide:",default="can provide",_RC)

    call NUOPC_Advertise(importState, StandardName="MOISTEX", &
       TransferOfferGeomObject=provide, &
       SharePolicyField=share, &
       SharePolicyGeomObject=share, & 
       _RC)
    call NUOPC_Advertise(importState, StandardName="DYNEX", &
       TransferOfferGeomObject=provide, &
       SharePolicyField=share, &
       SharePolicyGeomObject=share, & 
       _RC)
    call NUOPC_Advertise(exportState, StandardName="RADEX", &
       TransferOfferGeomObject=provide, &
       SharePolicyField=share, &
       SharePolicyGeomObject=share, &
       _RC)

    call print_message("Advertise Rad")

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine RealizeProvided(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)          :: importState, exportState
    type(ESMF_Grid)           :: grid

    call print_message("RealizeProvided Rad Start")

    rc = ESMF_SUCCESS

    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, _RC)

    ! create a Grid object for Fields
    grid = make_a_grid(config_file="rad_input.rc",_RC)

    call MAPL_realize_provided_field(importState,grid,"MOISTEX",_RC)

    call MAPL_realize_provided_field(importState,grid,"DYNEX",_RC)

    call MAPL_realize_provided_field(exportState,grid,"RADEX",_RC)

    call print_message("RealizeProvided Rad End")

  end subroutine

  subroutine RealizeAccepted(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)          :: importState, exportState

    call print_message("RealizeAccepted Rad Start")
    rc = ESMF_SUCCESS

    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, _RC)

    call MAPL_realize_accepted(importState,exportState,_RC)

    call print_message("RealizeAccepted Rad End")

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
    real(KIND=ESMF_KIND_R8), pointer :: ptr2d(:,:)

    rc = ESMF_SUCCESS

    ! query for clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=clock, importState=importState, &
      exportState=exportState, _RC)
    call print_pointer_address(exportState,"rad exp",_RC)
    call print_pointer_address(importState,"rad imp",_RC)
    
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
