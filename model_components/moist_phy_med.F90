#include "error_handling.h"
!==============================================================================
! Earth System mediatoring Framework
! Copyright 2002-2022, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, GeoMOISTsical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module MOIST_TO_PHYS_MED

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
  type(ESMF_State), save :: moistState
  type(ESMF_State), save :: phyState

  public SetServices

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    ! derive from NUOPC_mediator
    call NUOPC_CompDerive(mediator, mediatorSS, _RC)

    ! specialize mediator
    call NUOPC_CompSpecialize(mediator, specLabel=label_Advertise, &
      specRoutine=Advertise, _RC)
    call NUOPC_CompSpecialize(mediator, specLabel=label_RealizeProvided, &
      specRoutine=RealizeProvided, _RC)
    call NUOPC_CompSpecialize(mediator, specLabel=label_RealizeAccepted, &
      specRoutine=RealizeAccepted, _RC)
    call NUOPC_CompSpecialize(mediator, specLabel=label_Advance, &
      specRoutine=Advance, _RC)
    call NUOPC_CompSpecialize(mediator, specLabel=label_CheckImport, &
      specRoutine=NUOPC_NoOp, _RC)
    call NUOPC_CompSpecialize(mediator, specLabel=label_TimestampExport, &
      specRoutine=NUOPC_NoOp, _RC)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Advertise(mediator, rc) 
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Config) :: config
    character(len=ESMF_MAXSTR) :: share, provide

    rc = ESMF_SUCCESS

    config=ESMF_ConfigCreate()
    call ESMF_ConfigLoadFile(config,filename="moist_phy_med_input.rc",_RC)
    call ESMF_ConfigGetAttribute(config,share,Label="share:",default="share",_RC)
    call ESMF_ConfigGetAttribute(config,provide,Label="provide:",default="can provide",_RC)

    ! query for importState and exportState
    call NUOPC_mediatorGet(mediator, importState=importState, &
      exportState=exportState, _RC)

    call NUOPC_AddNamespace(importState,namespace="MOIST",nestedState=moistState,_RC)
    call NUOPC_AddNamespace(exportState,namespace="PHY",nestedState=phyState,_RC)

    call NUOPC_Advertise(phyState, StandardName="MOISTEX", &
       TransferOfferGeomObject=provide, &
       SharePolicyField=share, &
       SharePolicyGeomObject=share, &
       _RC)

    call NUOPC_Advertise(phyState, StandardName="BOBO", &
       TransferOfferGeomObject=provide, &
       SharePolicyField=share, &
       SharePolicyGeomObject=share, &
       _RC)

    call NUOPC_Advertise(moistState, StandardName="MOISTEX", &
       TransferOfferGeomObject=provide, &
       SharePolicyField=share, &
       SharePolicyGeomObject=share, &
       _RC)

    call NUOPC_Advertise(moistState, StandardName="BOBO", &
       TransferOfferGeomObject=provide, &
       SharePolicyField=share, &
       SharePolicyGeomObject=share, &
       _RC)

    !call NUOPC_SetAttribute(exportState,"FieldTransferPolicy","transferall",_RC)
    call print_message("Advertise MOIST")

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine RealizeProvided(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)          :: importState, exportState
    type(ESMF_Grid)           :: grid

    call print_message("RealizeProvided MOIST start")
    rc = ESMF_SUCCESS

    call NUOPC_mediatorGet(mediator, importState=importState, &
      exportState=exportState, _RC)

    grid = make_a_grid(config_file="moist_phy_med_input.rc",_RC)

    call MAPL_realize_provided_field(phyState,grid,"BOBO",lm=72,_RC)
    call MAPL_realize_provided_field(phyState,grid,"MOISTEX",_RC)
    call MAPL_realize_provided_field(moistState,grid,"BOBO",lm=72,_RC)
    call MAPL_realize_provided_field(moistState,grid,"MOISTEX",_RC)

    call print_message("RealizeProvided MOIST end")

  end subroutine

  subroutine RealizeAccepted(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)          :: importState, exportState

    call print_message("RealizeAccepted MOIST start")
    rc = ESMF_SUCCESS

    ! query for importState and exportState
    call NUOPC_mediatorGet(mediator, importState=importState, &
      exportState=exportState, _RC)

    call MAPL_realize_accepted(moistState,phyState,_RC)

    call print_message("RealizeAccpted MOIST end")

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Advance(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Field)            :: field
    type(ESMF_FileStatus_Flag)  :: status
    character(len=160)          :: msgString
    real(kind=ESMF_KIND_R4), pointer :: ptr3d_ex(:,:,:),ptr3d_im(:,:,:)
    real(kind=ESMF_KIND_R4), pointer :: ptr2d_ex(:,:),ptr2d_im(:,:)

    rc = ESMF_SUCCESS

    ! query for clock, importState and exportState
    call NUOPC_mediatorGet(mediator, mediatorClock=clock, importState=importState, &
      exportState=exportState, _RC)

    call ESMF_StateGet(moistState,itemname="MOISTEX",field=field,_RC)
    call ESMF_FieldGet(field,farrayPtr=ptr2d_im,_RC)
    call ESMF_StateGet(phyState,itemname="MOISTEX",field=field,_RC)
    call ESMF_FieldGet(field,farrayPtr=ptr2d_ex,_RC)
    call ESMF_StateGet(moistState,itemname="BOBO",field=field,_RC)
    call ESMF_FieldGet(field,farrayPtr=ptr3d_im,_RC)
    call ESMF_StateGet(phyState,itemname="BOBO",field=field,_RC)
    call ESMF_FieldGet(field,farrayPtr=ptr3d_ex,_RC)
    ptr2d_im = ptr2d_ex
    ptr3d_im = ptr3d_ex

    call print_pointer_address(exportState,"MOIST_PHY_MED exp",_RC)
    call print_pointer_address(importState,"MOIST_PHY_MED imp",_RC)

  end subroutine

  !-----------------------------------------------------------------------------

end module
