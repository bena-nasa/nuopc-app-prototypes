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

module ESM

  !-----------------------------------------------------------------------------
  ! Code that specializes generic ESM Component code.
  !-----------------------------------------------------------------------------

  use my_error_handling
  use ESMF
  use NUOPC
  use NUOPC_Driver, &
    driverSS             => SetServices

  use ATM, only: atmSS => SetServices
  use OCN, only: ocnSVM => SetVM, ocnSS => SetServices

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
    integer                       :: petCountATM, petCountOCN
    integer, allocatable          :: petList(:)
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

    ! split up the PETs between ATM and OCN
    petCountOCN = min(2,petCount/2) ! don't give OCN more than 2 PETs
    petCountATM = petCount - petCountOCN

     ! SetServices for ATM with petList on first section of PETs
    allocate(petList(petCountATM))
    do i=1, petCountATM
      petList(i) = i-1 ! PET labeling goes from 0 to petCount-1
    enddo
    call NUOPC_DriverAddComp(driver, "ATM", atmSS, petList=petList, &
      comp=comp, _RC)
    deallocate(petList)
    verbosity = 0 ! reset
    verbosity = ibset(verbosity,0)  ! log basic intro/extro and indentation
    verbosity = ibset(verbosity,9)  ! log info run phase
    verbosity = ibset(verbosity,11) ! log info about data dependency loop
    verbosity = ibset(verbosity,12) ! log info about run time-loop
    write(vString,"(I10)") verbosity
!    call NUOPC_CompAttributeSet(comp, name="Verbosity", value=vString, _RC)
    call NUOPC_CompAttributeSet(comp, name="Verbosity", value="high", _RC)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! SetServices for OCN with petList on second section of PETs
    allocate(petList(petCountOCN))
    do i=1, petCountOCN
      petList(i) = petCountATM + i-1 ! PET labeling goes from 0 to petCount-1
    enddo
    call ESMF_InfoSet(info, key="/NUOPC/Hint/PePerPet/MaxCount", value=2, &
      _RC)
    call NUOPC_DriverAddComp(driver, "OCN", ocnSS, ocnSVM, info=info, &
      petList=petList, comp=comp, _RC)
    deallocate(petList)
    verbosity = 0 ! reset
    verbosity = ibset(verbosity,0)  ! log basic intro/extro and indentation
    verbosity = ibset(verbosity,9)  ! log info run phase
    write(vString,"(I10)") verbosity
!    call NUOPC_CompAttributeSet(comp, name="Verbosity", value=vString, _RC)
    call NUOPC_CompAttributeSet(comp, name="Verbosity", value="high", _RC)

    ! SetServices for atm2ocn
    call NUOPC_DriverAddComp(driver, srcCompLabel="ATM", dstCompLabel="OCN", &
      compSetServicesRoutine=cplSS, comp=conn, _RC)
    call NUOPC_CompAttributeSet(conn, name="Verbosity", value="high", _RC)
    ! SetServices for ocn2atm
    call NUOPC_DriverAddComp(driver, srcCompLabel="OCN", dstCompLabel="ATM", &
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

  !-----------------------------------------------------------------------------

end module
