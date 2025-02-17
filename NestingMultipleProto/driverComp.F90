!==============================================================================
! Earth System Modeling Framework
! Copyright 2002-2022, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module driverComp

  !-----------------------------------------------------------------------------
  ! Code that specializes generic NUOPC_Driver
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Driver, &
    driverSS             => SetServices

  use advectDiffComp, only: &
    advectDiff_SS => SetServices

  use NUOPC_Connector, only: cplSS => SetServices

  implicit none

  private

  ! private module data --> ONLY PARAMETERS
  integer, parameter            :: stepCount = 1000
  real(ESMF_KIND_R8), parameter :: stepTime  = 30.D0  ! step time [s]
                                                      ! should be parent step

  public SetServices

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    ! derive from NUOPC_Driver
    call NUOPC_CompDerive(driver, driverSS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! specialize driver
    call NUOPC_CompSpecialize(driver, specLabel=label_SetModelServices, &
      specRoutine=SetModelServices, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! set driver verbosity
    call NUOPC_CompAttributeSet(driver, name="Verbosity", value="high", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetModelServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Grid)               :: grid
    type(ESMF_Field)              :: field
    type(ESMF_Time)               :: startTime
    type(ESMF_Time)               :: stopTime
    type(ESMF_TimeInterval)       :: timeStep
    type(ESMF_Clock)              :: internalClock
    integer                       :: petCount, i
    integer, allocatable          :: petList(:)
    type(ESMF_GridComp)           :: model
    type(ESMF_CplComp)            :: connector

    rc = ESMF_SUCCESS

    ! get petCount
    call ESMF_GridCompGet(driver, petCount=petCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! SetServices for PARENT DOMAIN advectDiffComp
    call NUOPC_DriverAddComp(driver, "advectDiff_Parent", advectDiff_SS, &
      comp=model, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! Set verbosity
    call NUOPC_CompAttributeSet(model, name="Verbosity", value="1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! Set parent domain's Nesting Attributes
    call NUOPC_CompAttributeSet(model, name="NestingGeneration", &
      value=0, rc=rc)   ! Parent generation
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(model, name="Nestling", &
      value=0, rc=rc)   ! First (and only) nestling in this generation
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! SetServices for NEST DOMAIN advectDiffComp
    allocate(petList(petCount/2))
    do i=1, petCount/2
      petList(i) = i-1 ! PET labeling goes from 0 to petCount-1
    enddo
    call NUOPC_DriverAddComp(driver, "advectDiff_Nestling0", advectDiff_SS, &
      petList=petList, comp=model, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    deallocate(petList)
    ! Set verbosity
    call NUOPC_CompAttributeSet(model, name="Verbosity", value="1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! Set nestling domain's Nesting Attributes
    call NUOPC_CompAttributeSet(model, name="NestingGeneration", &
      value=1, rc=rc)   ! First child generation
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(model, name="Nestling", &
      value=0, rc=rc)   ! First (and only) nestling in this generation
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! SetServices for NEST DOMAIN advectDiffComp
    allocate(petList(petCount/2))
    do i=petCount/2+1, petCount
      petList(i-petCount/2) = i-1 ! PET labeling goes from 0 to petCount-1
    enddo
    call NUOPC_DriverAddComp(driver, "advectDiff_Nestling1", advectDiff_SS, &
      petList=petList, comp=model, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    deallocate(petList)
    ! Set verbosity
    call NUOPC_CompAttributeSet(model, name="Verbosity", value="1", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! Set nestling domain's Nesting Attributes
    call NUOPC_CompAttributeSet(model, name="NestingGeneration", &
      value=1, rc=rc)   ! First child generation
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompAttributeSet(model, name="Nestling", &
      value=1, rc=rc)   ! First (and only) nestling in this generation
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! SetServices for connector: parent->nestling0
    call NUOPC_DriverAddComp(driver, srcCompLabel="advectDiff_Parent", &
      dstCompLabel="advectDiff_Nestling0", compSetServicesRoutine=cplSS, &
      comp=connector, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! Set verbosity
    call NUOPC_CompAttributeSet(connector, name="Verbosity", value="1", rc=rc)

    ! SetServices for connector: parent->nestling1
    call NUOPC_DriverAddComp(driver, srcCompLabel="advectDiff_Parent", &
      dstCompLabel="advectDiff_Nestling1", compSetServicesRoutine=cplSS, &
      comp=connector, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! Set verbosity
    call NUOPC_CompAttributeSet(connector, name="Verbosity", value="1", rc=rc)

    ! set the model clock
    call ESMF_TimeSet(startTime, s = 0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeSet(stopTime, s_r8 = stepTime * stepCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_TimeIntervalSet(timeStep, s_r8 = stepTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    internalClock = ESMF_ClockCreate(name="Driver Clock", &
      timeStep=timeStep, startTime=startTime, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

    call ESMF_GridCompSet(driver, clock=internalClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

end module
