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

#define CUSTOMRUNSEQUENCE_on

module ATM

  !-----------------------------------------------------------------------------
  ! Code specializing generic NUOPC_Driver as ATM model with DYN+PHY children
  !-----------------------------------------------------------------------------

  use my_error_handling
  use ESMF
  use NUOPC
  use NUOPC_Driver, &
    driverSS             => SetServices

  use DYN, only: dynSS => SetServices
  use PHY, only: phySS => SetServices

  use NUOPC_Connector, only: cplSS => SetServices

  implicit none

  private

  public SetServices

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    ! derive from NUOPC_Driver
    call NUOPC_CompDerive(driver, driverSS, _RC)
    ! specialize driver
    call NUOPC_CompSpecialize(driver, specLabel=label_SetModelServices, &
      specRoutine=SetModelServices, _RC)
#ifdef CUSTOMRUNSEQUENCE_on
    call NUOPC_CompSpecialize(driver, specLabel=label_SetRunSequence, &
      specRoutine=SetRunSequence, _RC)
#endif

    ! set driver verbosity
    call NUOPC_CompAttributeSet(driver, name="Verbosity", value="high", _RC)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_GridComp)           :: child
    type(ESMF_CplComp)            :: conn
    integer                       :: verbosity
    character(len=10)             :: vString

    rc = ESMF_SUCCESS

    ! SetServices for DYN
    call NUOPC_DriverAddComp(driver, "DYN", dynSS, petList=(/0/), &
      comp=child, _RC)
    verbosity = 0 ! reset
    verbosity = ibset(verbosity,0)  ! log basic intro/extro and indentation
    verbosity = ibset(verbosity,11) ! log info about data dependency loop
    verbosity = ibset(verbosity,12) ! log info about run time-loop
    write(vString,"(I10)") verbosity
!    call NUOPC_CompAttributeSet(child, name="Verbosity", value=vString, _RC)
    call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", _RC)

    ! SetServices for PHY
    call NUOPC_DriverAddComp(driver, "PHY", phySS, petList=(/1/), &
      comp=child, _RC)
    verbosity = 0 ! reset
    verbosity = ibset(verbosity,0)  ! log basic intro/extro and indentation
    verbosity = ibset(verbosity,11) ! log info about data dependency loop
    verbosity = ibset(verbosity,12) ! log info about run time-loop
!    call NUOPC_CompAttributeSet(child, name="Verbosity", value=vString, _RC)
    call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", _RC)

#ifndef CUSTOMRUNSEQUENCE_on
    ! SetServices for PHY2DYN
    call NUOPC_DriverAddComp(driver, srcCompLabel="PHY", dstCompLabel="DYN", &
      compSetServicesRoutine=cplSS, comp=conn, _RC)
    call NUOPC_CompAttributeSet(conn, name="Verbosity", value="high", _RC)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetRunSequence(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)              :: name
    type(NUOPC_FreeFormat)              :: runSeqFF

    rc = ESMF_SUCCESS

    ! query the driver for its name
    call ESMF_GridCompGet(driver, name=name, _RC)

    ! set up free format run sequence
    runSeqFF = NUOPC_FreeFormatCreate(stringList=(/ &
      " @*            ",    &
      "   PHY -> DYN  ",    &
      "   DYN         ",    &
      "   PHY         ",    &
      " @             " /), &
      _RC)

    ! ingest FreeFormat run sequence
    call NUOPC_DriverIngestRunSequence(driver, runSeqFF, autoAddConnectors=.true., _RC)

    ! clean-up
    call NUOPC_FreeFormatDestroy(runSeqFF, _RC)

  end subroutine

  !-----------------------------------------------------------------------------

end module
