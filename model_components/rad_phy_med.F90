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

  type(ESMF_State), save :: radState
  type(ESMF_State), save :: phyState

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
    call ESMF_ConfigLoadFile(config,filename="rad_phy_med_input.rc",_RC)
    call ESMF_ConfigGetAttribute(config,share,Label="share:",default="share",_RC)
    call ESMF_ConfigGetAttribute(config,provide,Label="provide:",default="can provide",_RC)

    ! query for importState and exportState
    call NUOPC_MediatorGet(Mediator, importState=importState, &
      exportState=exportState, _RC)

    call NUOPC_AddNamespace(exportState,namespace="RAD",nestedState=radState,_RC)
    call NUOPC_AddNamespace(importState,namespace="PHY",nestedState=phyState,_RC)

    call NUOPC_Advertise(radState, StandardName="DYNEX", &
       TransferOfferGeomObject=provide, &
       SharePolicyField=share, &
       SharePolicyGeomObject=share, &
       _RC)
    call NUOPC_Advertise(phyState, StandardName="DYNEX", &
       TransferOfferGeomObject=provide, &
       SharePolicyField=share, &
       SharePolicyGeomObject=share, &
       _RC)

    !call NUOPC_SetAttribute(importState,"FieldTransferPolicy","transferAll",_RC)

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

    call MAPL_realize_provided_field(phyState,grid,"DYNEX",_RC)
    call MAPL_realize_provided_field(radState,grid,"DYNEX",_RC)
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

    call MAPL_realize_accepted(phyState,radState,_RC)

    call print_message("RealizeAccpted MOIST end")

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Advance(Mediator, rc)
    type(ESMF_GridComp)  :: Mediator
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(ESMF_FileStatus_Flag)  :: status
    type(ESMF_Field)            :: field
    character(len=160)          :: msgString
    real(kind=ESMF_KIND_R4), pointer :: ptr2d_ex(:,:),ptr2d_im(:,:)

    rc = ESMF_SUCCESS

    ! query for clock, importState and exportState
    call NUOPC_MediatorGet(Mediator, MediatorClock=clock, importState=importState, &
      exportState=exportState, _RC)

    call ESMF_StateGet(phyState,itemname="DYNEX",field=field,_RC)
    call ESMF_FieldGet(field,farrayPtr=ptr2d_im,_RC)
    call ESMF_StateGet(radState,itemname="DYNEX",field=field,_RC)
    call ESMF_FieldGet(field,farrayPtr=ptr2d_ex,_RC)
    ptr2d_ex=ptr2d_im

    call print_pointer_address(exportState,"RAD_PHYS_MED exp",_RC)
    call print_pointer_address(importState,"RAD_PHYS_MED imp",_RC)

  end subroutine

  !-----------------------------------------------------------------------------

end module
