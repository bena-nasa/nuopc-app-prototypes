!==============================================================================
! Earth System Modeling Framework
! Copyright 2002-2022, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================

module MED

  !-----------------------------------------------------------------------------
  ! Mediator Component.
  ! This mediator operates on two timescales and keeps two internal Clocks to
  ! do so.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Mediator, &
    mediatorSS             => SetServices

  implicit none

  private

  ! private internal state to keep instance data
  type InternalStateStruct
    type(ESMF_Clock)      :: clockSlow
    type(ESMF_Clock)      :: clockFast
  end type

  type InternalState
    type(InternalStateStruct), pointer :: wrap
  end type

  public SetServices

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    ! derive from NUOPC_Mediator
    call NUOPC_CompDerive(mediator, mediatorSS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! specialize mediator
    call NUOPC_CompSpecialize(mediator, specLabel=label_Advertise, &
      specRoutine=Advertise, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(mediator, specLabel=label_RealizeProvided, &
      specRoutine=Realize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! slow Mediation phase with OCN (use the default "RunPhase1" for slow)
    call NUOPC_CompSpecialize(mediator, specLabel=label_SetRunClock, &
      specPhaseLabel="RunPhase1", specRoutine=SetRunClock_slow, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(mediator, specLabel=label_Advance, &
      specPhaseLabel="RunPhase1", specRoutine=Advance_slow, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! fast Mediation phase with ATM: before
    call NUOPC_CompSetEntryPoint(mediator, ESMF_METHOD_RUN, &
      phaseLabelList=(/"RunPhaseFastBefore"/), &
      userRoutine=routine_Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(mediator, specLabel=label_SetRunClock, &
      specPhaseLabel="RunPhaseFastBefore", &
      specRoutine=SetRunClock_fast_before, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(mediator, specLabel=label_CheckImport, &
      specPhaseLabel="RunPhaseFastBefore", &
      specRoutine=CheckImport_fast_before, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(mediator, specLabel=label_TimestampExport, &
      specPhaseLabel="RunPhaseFastBefore", &
      specRoutine=TimestampExport_fast_before, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(mediator, specLabel=label_Advance, &
      specPhaseLabel="RunPhaseFastBefore", &
      specRoutine=Advance_fast_before, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! fast Mediation phase with ATM: after
    call NUOPC_CompSetEntryPoint(mediator, ESMF_METHOD_RUN, &
      phaseLabelList=(/"RunPhaseFastAfter"/), &
      userRoutine=routine_Run, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(mediator, specLabel=label_SetRunClock, &
      specPhaseLabel="RunPhaseFastAfter", &
      specRoutine=SetRunClock_fast_after, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(mediator, specLabel=label_CheckImport, &
      specPhaseLabel="RunPhaseFastAfter", &
      specRoutine=CheckImport_fast_after, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(mediator, specLabel=label_TimestampExport, &
      specPhaseLabel="RunPhaseFastAfter", &
      specRoutine=TimestampExport_fast_after, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(mediator, specLabel=label_Advance, &
      specPhaseLabel="RunPhaseFastAfter", &
      specRoutine=Advance_fast_after, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Advertise(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)        :: importState, exportState

    rc = ESMF_SUCCESS

    ! query for importState and exportState
    call NUOPC_MediatorGet(mediator, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: sea_surface_temperature
    call NUOPC_Advertise(importState, &
      StandardName="sea_surface_temperature", name="sst", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: air_pressure_at_sea_level
    call NUOPC_Advertise(importState, &
      StandardName="air_pressure_at_sea_level", name="pmsl", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: surface_net_downward_shortwave_flux
    call NUOPC_Advertise(importState, &
      StandardName="surface_net_downward_shortwave_flux", name="rsns", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: sea_surface_temperature
    call NUOPC_Advertise(exportState, &
      StandardName="sea_surface_temperature", name="sst", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: air_pressure_at_sea_level
    call NUOPC_Advertise(exportState, &
      StandardName="air_pressure_at_sea_level", name="pmsl", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: surface_net_downward_shortwave_flux
    call NUOPC_Advertise(exportState, &
      StandardName="surface_net_downward_shortwave_flux", name="rsns", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Realize(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Clock)        :: clock
    type(ESMF_Field)        :: field
    type(ESMF_Grid)         :: gridIn
    type(ESMF_Grid)         :: gridOut
    type(InternalState)     :: is
    integer                 :: stat
    type(ESMF_TimeInterval) :: timeStep

    rc = ESMF_SUCCESS

    ! query for driverClock, importState and exportState
    call NUOPC_MediatorGet(mediator, driverClock=clock, &
      importState=importState, exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! create a Grid object for Fields
   gridIn = ESMF_GridCreateNoPeriDimUfrm(maxIndex=(/20, 100/), &
      minCornerCoord=(/10._ESMF_KIND_R8, 20._ESMF_KIND_R8/), &
      maxCornerCoord=(/100._ESMF_KIND_R8, 200._ESMF_KIND_R8/), &
      coordSys=ESMF_COORDSYS_CART, staggerLocList=(/ESMF_STAGGERLOC_CENTER/), &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    gridOut = gridIn ! for now out same as in

    ! importable field: sea_surface_temperature
    field = ESMF_FieldCreate(name="sst", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: air_pressure_at_sea_level
    field = ESMF_FieldCreate(name="pmsl", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: surface_net_downward_shortwave_flux
    field = ESMF_FieldCreate(name="rsns", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: sea_surface_temperature
    field = ESMF_FieldCreate(name="sst", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: air_pressure_at_sea_level
    field = ESMF_FieldCreate(name="pmsl", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: surface_net_downward_shortwave_flux
    field = ESMF_FieldCreate(name="rsns", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! The Fields in the exportState are going to be correctly timestamped by
    ! the generic code. However, the Fields in the importState are not
    ! automatically time stamped. Instead it is expected that the customized
    ! initialization takes care of this aspect. Here just "blindly" time stamp
    ! the Fields in the importState, indicating to the first Run method that
    ! all is good.

    call NUOPC_SetTimestamp(importState, clock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Allocate memory for the internal state and set it in the Component.
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of the internal state memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_GridCompSetInternalState(mediator, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Initialize the internal clocks: both slow and fast start as copies of in
    is%wrap%clockSlow = ESMF_ClockCreate(clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    is%wrap%clockFast = ESMF_ClockCreate(clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! set the slow clock to 60min steps
    call ESMF_TimeIntervalSet(timeStep, m=60, rc=rc) ! 60 min steps
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_ClockSet(is%wrap%clockSlow, timestep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    ! set the fast clock to 30min steps
    call ESMF_TimeIntervalSet(timeStep, m=30, rc=rc) ! 30 min steps
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)
    call ESMF_ClockSet(is%wrap%clockFast, timestep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      call ESMF_Finalize(endflag=ESMF_END_ABORT)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetRunClock_slow(mediator, rc)
    type(ESMF_GridComp)   :: mediator
    integer, intent(out)  :: rc

    ! local variables
    type(InternalState)     :: is
    type(ESMF_Clock)        :: driverClock

    rc = ESMF_SUCCESS

    ! query component for its internal state
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(mediator, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! set clockSlow to be the component clock
    call ESMF_GridCompSet(mediator, clock=is%wrap%clockSlow, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! query component for the driver Clock
    call NUOPC_MediatorGet(mediator, driverClock=driverClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! check and set the component clock against the driver clock
    call NUOPC_CompCheckSetClock(mediator, driverClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
      msg="NUOPC INCOMPATIBILITY DETECTED: between model and driver clocks", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine SetRunClock_slow

  !-----------------------------------------------------------------------------

  subroutine SetRunClock_fast_before(mediator, rc)
    type(ESMF_GridComp)   :: mediator
    integer, intent(out)  :: rc

    ! local variables
    type(InternalState)     :: is
    type(ESMF_Clock)        :: driverClock

    rc = ESMF_SUCCESS

    ! query component for its internal state
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(mediator, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! set clockFast to be the component clock
    call ESMF_GridCompSet(mediator, clock=is%wrap%clockFast, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! query component for the driver Clock
    call NUOPC_MediatorGet(mediator, driverClock=driverClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! check and set the component clock against the driver clock
    call NUOPC_CompCheckSetClock(mediator, driverClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
      msg="NUOPC INCOMPATIBILITY DETECTED: between model and driver clocks", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine SetRunClock_fast_before

  !-----------------------------------------------------------------------------

  subroutine SetRunClock_fast_after(mediator, rc)
    type(ESMF_GridComp)   :: mediator
    integer, intent(out)  :: rc

    ! local variables
    type(InternalState)     :: is
    type(ESMF_Clock)        :: driverClock

    rc = ESMF_SUCCESS

    ! query component for its internal state
    nullify(is%wrap)
    call ESMF_GridCompGetInternalState(mediator, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! set clockFast to be the component clock
    call ESMF_GridCompSet(mediator, clock=is%wrap%clockFast, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! query component for the driver Clock
    call NUOPC_MediatorGet(mediator, driverClock=driverClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! check and set the component clock against the driver clock
    call NUOPC_CompCheckSetClock(mediator, driverClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
      msg="NUOPC INCOMPATIBILITY DETECTED: between model and driver clocks", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine SetRunClock_fast_after

  !-----------------------------------------------------------------------------

  subroutine CheckImport_fast_before(mediator, rc)
    type(ESMF_GridComp)   :: mediator
    integer, intent(out)  :: rc

    ! This is the routine that ensures that the import Fields come in with
    ! the correct time stamps during the "fast" cycle:
    ! -> Fields from the ATM are not used by the "before" phase and need not
    !    be checked.
    ! -> Fields from the OCN must be at the startTime of the parent driver
    !    Clock

    ! local variables
    type(ESMF_Clock)        :: clock
    type(ESMF_Time)         :: startTime
    type(ESMF_State)        :: importState
    type(ESMF_Field)        :: field
    logical                 :: atCorrectTime

    rc = ESMF_SUCCESS

    ! query the Component for its importState
    call ESMF_GridCompGet(mediator, importState=importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! query the Component for its driverClock
    call NUOPC_MediatorGet(mediator, driverClock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! get the start time out of the driver Clock
    call ESMF_ClockGet(clock, startTime=startTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! check fields from OCN to be at startTime of the driver Clock
    if (NUOPC_IsConnected(importState, fieldName="sst")) then
      call ESMF_StateGet(importState, itemName="sst", field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      atCorrectTime = NUOPC_IsAtTime(field, startTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if (.not.atCorrectTime) then
        !TODO: introduce and use INCOMPATIBILITY return codes!!!!
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="NUOPC INCOMPATIBILITY DETECTED: Import Fields not at correct time", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return  ! bail out
      endif
    endif

  end subroutine CheckImport_fast_before

  !-----------------------------------------------------------------------------

  subroutine CheckImport_fast_after(mediator, rc)
    type(ESMF_GridComp)   :: mediator
    integer, intent(out)  :: rc

    ! This is the routine that ensures that the import Fields come in with
    ! the correct time stamps during the "fast" cycle:
    ! -> Fields from the ATM must be at stopTime because this mediator phase
    !    runs _after_ the ATM runs.
    ! -> Fields from the OCN are not used by the "after" phase and need not
    !    be checked.

    ! local variables
    type(ESMF_Clock)        :: clock
    type(ESMF_Time)         :: stopTime
    type(ESMF_State)        :: importState
    type(ESMF_Field)        :: field
    logical                 :: atCorrectTime

    rc = ESMF_SUCCESS

    ! query the Component for its Clock and importState
    call ESMF_GridCompGet(mediator, clock=clock, importState=importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! get the stop time out of the Clock
    call ESMF_ClockGet(clock, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! check fields from ATM to be at stopTime
    if (NUOPC_IsConnected(importState, fieldName="pmsl")) then
      call ESMF_StateGet(importState, itemName="pmsl", field=field, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      atCorrectTime = NUOPC_IsAtTime(field, stopTime, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if (.not.atCorrectTime) then
        !TODO: introduce and use INCOMPATIBILITY return codes!!!!
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="NUOPC INCOMPATIBILITY DETECTED: Import Fields not at correct time", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return  ! bail out
      endif
    endif

  end subroutine CheckImport_fast_after

  !-----------------------------------------------------------------------------

  subroutine TimestampExport_fast_before(mediator, rc)
    type(ESMF_GridComp)   :: mediator
    integer, intent(out)  :: rc

    ! This is the routine that executes _after_ the "fast_before" mediator
    ! phase has been run. Timestamping does not need to be adjusted here,
    ! but the Clock needs to be stepped back because the "fast_after" phase
    ! will be updating the same Clock during the same driver cylce.

    ! local variables
    type(ESMF_Clock)        :: clock
    type(ESMF_TimeInterval) :: timeStep

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(mediator, clock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! get the timeStep out of Clock
    call ESMF_ClockGet(clock, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! step the Clock back one timestep
    call ESMF_ClockAdvance(clock, timeStep= -timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine TimestampExport_fast_before

  !-----------------------------------------------------------------------------

  subroutine TimestampExport_fast_after(mediator, rc)
    type(ESMF_GridComp)   :: mediator
    integer, intent(out)  :: rc

    ! This is the routine that applies the time stamp on the export Fields
    ! during the "fast" cycle:
    ! -> By default the MED Run method time stamps the export Fields with the
    !    current time at the beginning of the advance step, however here,
    !    because the "fast" cycle runs after the ATM model, the correct time
    !    stamp is the currTime _after_ the MED advance step.

    ! local variables
    type(ESMF_Clock)      :: clock
    type(ESMF_State)      :: exportState

    rc = ESMF_SUCCESS

    ! query the Component for info
    call ESMF_GridCompGet(mediator, clock=clock, exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! update timestamp on export Fields
    call NUOPC_SetTimestamp(exportState, clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine TimestampExport_fast_after

  !-----------------------------------------------------------------------------

  subroutine Advance_slow(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    character(len=160)          :: msgString

    rc = ESMF_SUCCESS

    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(mediator, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! HERE THE MEDIATOR does the mediation of Fields that come in on the
    ! importState with a timestamp consistent to the currTime of the
    ! mediators Clock.

    ! The Mediator uses the data on the import Fields to update the data
    ! held by Fields in the exportState.

    ! After this routine returns the generic Mediator will correctly
    ! timestamp the export Fields at currTime, and update the Mediator Clock to:
    !
    !       currTime -> currTime + timeStep
    !
    ! Where the timeStep is equal to the parent timeStep.

    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing MED slow() from: ", unit=msgString, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_ClockPrint(clock, options="stopTime", &
      preString="----------------------------> to: ", unit=msgString, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Advance_fast_before(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    character(len=160)          :: msgString

    rc = ESMF_SUCCESS

    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(mediator, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! HERE THE MEDIATOR does the mediation of Fields that come in on the
    ! importState with a timestamp consistent to the currTime of the
    ! mediators Clock.

    ! The Mediator uses the data on the import Fields to update the data
    ! held by Fields in the exportState.

    ! After this routine returns the generic Mediator will correctly
    ! timestamp the export Fields at currTime, and update the Mediator Clock to:
    !
    !       currTime -> currTime + timeStep
    !
    ! Where the timeStep is equal to the parent timeStep.

    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing MED fast_before() from: ", unit=msgString, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_ClockPrint(clock, options="stopTime", &
      preString="-----------------------------------> to: ", unit=msgString, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Advance_fast_after(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    character(len=160)          :: msgString

    rc = ESMF_SUCCESS

    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(mediator, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! HERE THE MEDIATOR does the mediation of Fields that come in on the
    ! importState with a timestamp consistent to the currTime of the
    ! mediators Clock.

    ! The Mediator uses the data on the import Fields to update the data
    ! held by Fields in the exportState.

    ! After this routine returns the generic Mediator will correctly
    ! timestamp the export Fields at currTime, and update the Mediator Clock to:
    !
    !       currTime -> currTime + timeStep
    !
    ! Where the timeStep is equal to the parent timeStep.

    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing MED fast_after() from: ", unit=msgString, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_ClockPrint(clock, options="stopTime", &
      preString="----------------------------------> to: ", unit=msgString, &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

end module
