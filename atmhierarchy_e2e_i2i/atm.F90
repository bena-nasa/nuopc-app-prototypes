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
  use RAD, only: RADSS => SetServices
  use MOIST, only: MOISTSS => SetServices
  USE GEOSLIKE_PHYSICS, only: physics_internal, physics_internal_wrapper, PHYSICSSS => SetServices
  use RAD_TO_PHYS_MED, only: rad_to_phy_ss => SetServices
  use MOIST_TO_PHYS_MED, only: moist_to_phy_ss => SetServices

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
    !call NUOPC_CompSpecialize(driver, specLabel=label_ExecuteRunSequence, &
      !specRoutine=ExecuteRunSequence, _RC)
    call NUOPC_CompSpecialize(driver, specLabel=label_ModifyInitializePhaseMap, &
      specRoutine=ModifyInitializePhaseMap, _RC)

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
    integer                       :: petCount
    type(ESMF_GridComp)           :: comp 
    type(ESMF_CplComp)            :: coupler
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

    call NUOPC_DriverAddComp(driver, "RAD", RADSS, comp=comp, _RC)
    verbosity = 0 ! reset
    verbosity = ibset(verbosity,0)  ! log basic intro/extro and indentation
    verbosity = ibset(verbosity,9)  ! log info run phase
    write(vString,"(I10)") verbosity
    call NUOPC_CompAttributeSet(comp, name="Verbosity", value="high", _RC)

    call nuopc_driveraddcomp(driver, "MOIST", moistss, comp=comp, _RC)
    verbosity = 0 ! reset
    verbosity = ibset(verbosity,0)  ! log basic intro/extro and indentation
    verbosity = ibset(verbosity,9)  ! log info run phase
    write(vstring,"(i10)") verbosity
    call nuopc_compattributeset(comp, name="verbosity", value="high", _RC)

    call nuopc_driveraddcomp(driver, "PHYSICS", physicsss, comp=comp, _RC)
    verbosity = 0 ! reset
    verbosity = ibset(verbosity,0)  ! log basic intro/extro and indentation
    verbosity = ibset(verbosity,9)  ! log info run phase
    write(vstring,"(i10)") verbosity
    call nuopc_compattributeset(comp, name="verbosity", value="high", _RC)

    call nuopc_driveraddcomp(driver, "RAD_TO_PHY", rad_to_phy_ss, comp=comp, _RC)
    verbosity = 0 ! reset
    verbosity = ibset(verbosity,0)  ! log basic intro/extro and indentation
    verbosity = ibset(verbosity,9)  ! log info run phase
    write(vstring,"(i10)") verbosity
    call nuopc_compattributeset(comp, name="verbosity", value="high", _RC)

    call nuopc_driveraddcomp(driver, "MOIST_TO_PHY", moist_to_phy_ss, comp=comp, _RC)
    verbosity = 0 ! reset
    verbosity = ibset(verbosity,0)  ! log basic intro/extro and indentation
    verbosity = ibset(verbosity,9)  ! log info run phase
    write(vstring,"(i10)") verbosity
    call nuopc_compattributeset(comp, name="verbosity", value="high", _RC)

    call NUOPC_DriverAddComp(driver,srcCOmpLabel="RAD",dstCompLabel="MOIST",compsetServicesRoutine=cplSS,comp=coupler,_RC)
    call NUOPC_DriverAddComp(driver,srcCOmpLabel="MOIST",dstCompLabel="RAD",compsetServicesRoutine=cplSS,comp=coupler,_RC)
    call NUOPC_DriverAddComp(driver,srcCOmpLabel="PHYSICS",dstCompLabel="DYN",compsetServicesRoutine=cplSS,comp=coupler,_RC)
    call NUOPC_DriverAddComp(driver,srcCOmpLabel="DYN",dstCompLabel="PHYSICS",compsetServicesRoutine=cplSS,comp=coupler,_RC)

    call NUOPC_DriverAddComp(driver,srcCOmpLabel="MOIST_TO_PHY",dstCompLabel="PHYSICS",compsetServicesRoutine=cplSS,comp=coupler,_RC)
    call NUOPC_DriverAddComp(driver,srcCOmpLabel="RAD_TO_PHY",dstCompLabel="PHYSICS",compsetServicesRoutine=cplSS,comp=coupler,_RC)
    call NUOPC_DriverAddComp(driver,srcCOmpLabel="PHYSICS",dstCompLabel="MOIST_TO_PHY",compsetServicesRoutine=cplSS,comp=coupler,_RC)
    call NUOPC_DriverAddComp(driver,srcCOmpLabel="PHYSICS",dstCompLabel="RAD_TO_PHY",compsetServicesRoutine=cplSS,comp=coupler,_RC)
   
    call NUOPC_DriverAddComp(driver,srcCOmpLabel="MOIST_TO_PHY",dstCompLabel="MOIST",compsetServicesRoutine=cplSS,comp=coupler,_RC)
    call NUOPC_DriverAddComp(driver,srcCOmpLabel="RAD_TO_PHY",dstCompLabel="RAD",compsetServicesRoutine=cplSS,comp=coupler,_RC)
    call NUOPC_DriverAddComp(driver,srcCOmpLabel="MOIST",dstCompLabel="MOIST_TO_PHY",compsetServicesRoutine=cplSS,comp=coupler,_RC)
    call NUOPC_DriverAddComp(driver,srcCOmpLabel="RAD",dstCompLabel="RAD_TO_PHY",compsetServicesRoutine=cplSS,comp=coupler,_RC)
   

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
      " @*                        ", &
      "   PHYSICS -> DYN          ", &
      "   DYN                     ", &
      "   DYN -> PHYSICS          ", &
      "   PHYSICS                 ", &
      "   PHYSICS -> MOIST_TO_PHY ", &
      "   PHYSICS -> RAD_TO_PHY   ", &
      "   MOIST_TO_PHY            ", &
      "   RAD_TO_PHY              ", &
      "   MOIST_TO_PHY -> PHYSICS ", &
      "   RAD_TO_PHY -> PHYSICS   ", &
      "   MOIST -> MOIST_TO_PHY   ", &
      "   MOIST_TO_PHY -> MOIST   ", &
      "   RAD -> RAD_TO_PHY       ", &
      "   RAD_TO_PHY -> RAD       ", &
      " @                         " /), &
      _RC)

    ! ingest FreeFormat run sequence
    call NUOPC_DriverIngestRunSequence(driver, runSeqFF, AutoAddConnectors=.false., _RC)

    ! clean-up
    call NUOPC_FreeFormatDestroy(runSeqFF, _RC)

  end subroutine

  !subroutine ExecuteRunSequence(driver,rc)
    !type(ESMF_GridComp)  :: driver
    !integer, intent(out) :: rc

    !rc = ESMF_SUCCESS


  !end subroutine ExecuteRunSequence

  subroutine ModifyInitializePhaseMap(driver,rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    type(ESMF_GridComp) :: physics_gc, moist_gc, rad_gc
    type(ESMF_CplComp) :: moist_to_rad, rad_to_moist
    type(physics_internal_wrapper) :: wrap
    type(physics_internal), pointer :: physics_int

    rc = ESMF_SUCCESS

    call NUOPC_DriverGetComp(driver,"PHYSICS",comp=physics_gc,_RC)
    call NUOPC_DriverGetComp(driver,"MOIST",comp=moist_gc,_RC)
    call NUOPC_DriverGetComp(driver,"RAD",comp=rad_gc,_RC)

    call NUOPC_DriverGetComp(driver,"RAD","MOIST",comp=rad_to_moist,_RC)
    call NUOPC_DriverGetComp(driver,"MOIST","RAD",comp=moist_to_rad,_RC)

    call ESMF_UserCompGetInternalState(physics_gc,'PHYSICS_INTERNAL',wrap,_RC)
    write(*,*)"Stuffing into physics internal!"
    physics_int => wrap%ptr
    physics_int%moist=moist_gc
    physics_int%rad=rad_gc

    physics_int%rad_to_moist = rad_to_moist
    physics_int%moist_to_rad = moist_to_rad

  end subroutine ModifyInitializePhaseMap


end module
