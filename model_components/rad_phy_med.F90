#include "error_handling.h"
!==============================================================================
! Earth System Mediatoring Framework
! Copyright 2002-2022, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, GeoMOISTsical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module RAD_TO_PHYS_MED

  !-----------------------------------------------------------------------------
  ! MOIST Component.
  !-----------------------------------------------------------------------------

  use my_error_handling
  use MAPL_redu
  use ESMF
  use NUOPC
  use NUOPC_Mediator, &
    mediatorSS      => SetServices

  implicit none

  private

  public SetServices

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(Mediator, rc)
    type(ESMF_GridComp)  :: Mediator
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    ! derive from NUOPC_Mediator
    call NUOPC_CompDerive(Mediator, mediatorSS, _RC)

    ! specialize Mediator
    call NUOPC_CompSpecialize(Mediator, specLabel=label_Advertise, &
      specRoutine=Advertise, _RC)
    call NUOPC_CompSpecialize(Mediator, specLabel=label_RealizeProvided, &
      specRoutine=RealizeProvided, _RC)
    call NUOPC_CompSpecialize(Mediator, specLabel=label_RealizeAccepted, &
      specRoutine=RealizeAccepted, _RC)
    call NUOPC_CompSpecialize(Mediator, specLabel=label_Advance, &
      specRoutine=Advance, _RC)
    call NUOPC_CompSpecialize(Mediator, specLabel=label_CheckImport, &
      specRoutine=NUOPC_NoOp, _RC)
    call NUOPC_CompSpecialize(Mediator, specLabel=label_TimestampExport, &
      specRoutine=NUOPC_NoOp, _RC)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Advertise(Mediator, rc) 
    type(ESMF_GridComp)  :: Mediator
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Config) :: config
    character(len=ESMF_MAXSTR) :: share, provide

    rc = ESMF_SUCCESS

    config=ESMF_ConfigCreate()
    call ESMF_ConfigLoadFile(config,filename="rad_phys_med_input.rc",_RC)
    call ESMF_ConfigGetAttribute(config,share,Label="share:",default="share",_RC)
    call ESMF_ConfigGetAttribute(config,provide,Label="provide:",default="can provide",_RC)

    ! query for importState and exportState
    call NUOPC_MediatorGet(Mediator, importState=importState, &
      exportState=exportState, _RC)

    call NUOPC_Advertise(importState, StandardName="DYNEX", &
       TransferOfferGeomObject=provide, &
       SharePolicyField=share, &
       SharePolicyGeomObject=share, &
       _RC)
    call NUOPC_Advertise(exportState, StandardName="DYNEX", &
       TransferOfferGeomObject=provide, &
       SharePolicyField=share, &
       SharePolicyGeomObject=share, &
       _RC)

    call NUOPC_SetAttribute(importState,"FieldTransferPolicy","transferAll",_RC)

    call print_message("Advertise MOIST")

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine RealizeProvided(Mediator, rc)
    type(ESMF_GridComp)  :: Mediator
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)          :: importState, exportState
    type(ESMF_Grid)           :: grid

    call print_message("RealizeProvided MOIST start")
    rc = ESMF_SUCCESS

    call NUOPC_MediatorGet(Mediator, importState=importState, &
      exportState=exportState, _RC)

    grid = make_a_grid(config_file="rad_phy_med_input.rc",_RC)

    call MAPL_realize_provided_field(importState,grid,"DYNEX",_RC)
    call print_message("RealizeProvided MOIST end")

  end subroutine

  subroutine RealizeAccepted(Mediator, rc)
    type(ESMF_GridComp)  :: Mediator
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)          :: importState, exportState

    call print_message("RealizeAccepted MOIST start")
    rc = ESMF_SUCCESS

    ! query for importState and exportState
    call NUOPC_MediatorGet(Mediator, importState=importState, &
      exportState=exportState, _RC)

    call MAPL_realize_accepted(importState,exportState,_RC)

    call print_message("RealizeAccpted MOIST end")

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Advance(Mediator, rc)
    type(ESMF_GridComp)  :: Mediator
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
    real(kind=ESMF_KIND_R4), pointer :: ptr2d(:,:)

    rc = ESMF_SUCCESS

    ! query for clock, importState and exportState
    call NUOPC_MediatorGet(Mediator, MediatorClock=clock, importState=importState, &
      exportState=exportState, _RC)

    ! HERE THE Mediator ADVANCES: currTime -> currTime + timeStep

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

    call ESMF_StateGet(exportState, itemName="BOBO",field=field, _RC)
    call ESMF_FieldGet(field,farrayPtr=ptr3d,_RC)
    ptr3d=step
    step=step+1
    write(*,*)"Mr Burns bear BOBO turned this old in moist: ",maxval(ptr3d)
    write(*,*)"Mr Burns bear BOBO has this shape in moist: ",shape(ptr3d)
    call print_next_time(clock,"Advanced MOIST to: ")
    call ESMF_StateGet(exportState, itemName="MOISTEX",field=field, _RC)
    call ESMF_FieldGet(field,farrayPtr=ptr2d,_RC)
    ptr2d = step*step

    call print_pointer_address(exportState,"MOIST exp",_RC)
    call print_pointer_address(importState,"MOIST imp",_RC)

  end subroutine

  !-----------------------------------------------------------------------------

end module
