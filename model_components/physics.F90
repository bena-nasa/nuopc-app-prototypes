#include "error_handling.h"
!==============================================================================
! Earth System Modeling Framework
! Copyright 2002-2022, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, GeoPHYSICSsical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module PHYSICS

  !-----------------------------------------------------------------------------
  ! PHYSICS Component.
  !-----------------------------------------------------------------------------

  use my_error_handling
  use MAPL_redu
  use ESMF
  use NUOPC
  use NUOPC_Model, modelSS      => SetServices

  implicit none

  private

  public SetServices
  public physics_internal
  public physics_internal_wrapper

  type physics_internal
     type(ESMF_GridComp) :: moist
     type(ESMF_GridComp) :: rad
  end type
  type physics_internal_wrapper
     type(physics_internal), pointer :: ptr
  end type

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    type(physics_internal), pointer :: my_internal
    type(physics_internal_wrapper) :: wrap

    rc = ESMF_SUCCESS

    ! derive from NUOPC_Model
    call NUOPC_CompDerive(model, modelSS, _RC)

    ! hook to my private state
    allocate(my_internal)
    wrap%ptr => my_internal
    call ESMF_UserCompSetInternalState(model,'PHYSICS_INTERNAL',wrap,_RC)

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

    rc = ESMF_SUCCESS

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, _RC)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine RealizeProvided(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)          :: importState, exportState
    type(ESMF_Grid)           :: grid

    call print_message("RealizeProvided PHYSICS start")
    rc = ESMF_SUCCESS

    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, _RC)

    grid = make_a_grid(_RC)

    call print_message("RealizeProvided PHYSICS end")

  end subroutine

  subroutine RealizeAccepted(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)          :: importState, exportState

    call print_message("RealizeAccepted PHYSICS start")
    rc = ESMF_SUCCESS

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, _RC)

    call MAPL_realize_accepted(importState,exportState,_RC)

    call print_message("RealizeAccpted PHYSICS end")

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

    type(physics_internal_wrapper) :: wrap
    type(physics_internal), pointer :: physics_int
    type(ESMF_State) :: moist_import,moist_export,rad_import,rad_export

    rc = ESMF_SUCCESS

    ! query for clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=clock, importState=importState, &
      exportState=exportState, _RC)

    call ESMF_USerCompGetInternalState(model,'PHYSICS_INTERNAL',wrap,_RC)
    physics_int => wrap%ptr

    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing PHYSICS from: ", unit=msgString, _RC)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, _RC)

    call ESMF_ClockPrint(clock, options="stopTime", &
      preString="---------------------> to: ", unit=msgString, _RC)
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, _RC)

    call NUOPC_ModelGet(physics_int%moist,importState=moist_import,exportState=moist_export,_RC)
    call NUOPC_ModelGet(physics_int%rad,importState=rad_import,exportState=rad_export,_RC)
    call ESMF_GridCompRun(physics_int%moist,importState=moist_import,exportState=moist_export,_RC)
    call ESMF_GridCompRun(physics_int%rad,importState=rad_import,exportState=rad_export,_RC)

    call print_next_time(clock,"Advanced PHYSICS to: ")

  end subroutine

  !-----------------------------------------------------------------------------

end module
