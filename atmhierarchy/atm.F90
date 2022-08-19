#include "error_handling.h"
!==============================================================================
! Earth System Modeling Framework
! Copyright 2002-2022, University Corporation for DYNospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module ATM

  !-----------------------------------------------------------------------------
  ! Code that specializes generic ESM Component code.
  !-----------------------------------------------------------------------------

  use my_error_handling
  use ESMF
  use NUOPC
  use NUOPC_Driver, &
    driverSS             => SetServices

  use DYN, only: DYNSS => SetServices
  use PHY, only: PHYSS => SetServices

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
    call NUOPC_CompSpecialize(driver, specLabel=label_SetRunSequence, &
      specRoutine=SetRunSequence, _RC)

    ! set driver verbosity
    call NUOPC_CompAttributeSet(driver, name="Verbosity", value="high", _RC)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Time)               :: startTime
    type(ESMF_Time)               :: stopTime
    type(ESMF_TimeInterval)       :: timeStep
    type(ESMF_Clock)              :: internalClock
    integer                       :: petCount, i
    type(ESMF_GridComp)           :: comp
    type(ESMF_CplComp)            :: conn
    integer                       :: verbosity
    character(len=10)             :: vString
    type(ESMF_Info)               :: info

    rc = ESMF_SUCCESS

    ! Create and set the info object that is used to pass hints into methods
    info = ESMF_InfoCreate(_RC)

    ! get the petCount
    call ESMF_GridCompGet(driver, petCount=petCount, _RC)

    call NUOPC_DriverAddComp(driver, "DYN", DYNSS,  comp=comp, _RC)
    verbosity = 0 ! reset
    verbosity = ibset(verbosity,0)  ! log basic intro/extro and indentation
    verbosity = ibset(verbosity,9)  ! log info run phase
    verbosity = ibset(verbosity,11) ! log info about data dependency loop
    verbosity = ibset(verbosity,12) ! log info about run time-loop
    write(vString,"(I10)") verbosity
    call NUOPC_CompAttributeSet(comp, name="Verbosity", value="high", _RC)

    call NUOPC_DriverAddComp(driver, "PHY", PHYSS, comp=comp, _RC)
    verbosity = 0 ! reset
    verbosity = ibset(verbosity,0)  ! log basic intro/extro and indentation
    verbosity = ibset(verbosity,9)  ! log info run phase
    write(vString,"(I10)") verbosity
    call NUOPC_CompAttributeSet(comp, name="Verbosity", value="high", _RC)

    ! SetServices for DYN2PHY
    call NUOPC_DriverAddComp(driver, srcCompLabel="DYN", dstCompLabel="PHY", &
      compSetServicesRoutine=cplSS, comp=conn, _RC)
    call NUOPC_CompAttributeSet(conn, name="Verbosity", value="high", _RC)
    ! SetServices for PHY2DYN
    call NUOPC_DriverAddComp(driver, srcCompLabel="PHY", dstCompLabel="DYN", &
      compSetServicesRoutine=cplSS, comp=conn, _RC)
    call NUOPC_CompAttributeSet(conn, name="Verbosity", value="high", _RC)

    ! set the driver clock
    call ESMF_TimeIntervalSet(timeStep, m=15, _RC) ! 15 minute steps

    call ESMF_TimeSet(startTime, yy=2010, mm=6, dd=1, h=0, m=0, _RC)
    call ESMF_TimeSet(stopTime, yy=2010, mm=6, dd=1, h=2, m=0, _RC)

    internalClock = ESMF_ClockCreate(name="Application Clock", &
      timeStep=timeStep, startTime=startTime, stopTime=stopTime, _RC)

    call ESMF_GridCompSet(driver, clock=internalClock, _RC)

  end subroutine

  subroutine SetRunSequence(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    character(ESMF_MAXSTR)              :: name
    type(NUOPC_FreeFormat)              :: runSeqFF

    rc = ESMF_SUCCESS

    ! query the driver for its name
    call ESMF_GridCompGet(driver, name=name, _RC)


    runSeqFF = NUOPC_FreeFormatCreate(stringList=(/ &
      " @*            ",    &
      "   DYN         ",    &
      "   DYN -> PHY  ",    &
      "   PHY         ",    &
      "   PHY -> DYN  ",    &
      " @             " /), &
      _RC)

    ! ingest FreeFormat run sequence
    call NUOPC_DriverIngestRunSequence(driver, runSeqFF, _RC)

    ! clean-up
    call NUOPC_FreeFormatDestroy(runSeqFF, _RC)

  end subroutine

end module
