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
  use PHYDRV, only: phyDRVSS => SetServices

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
    type(ESMF_GridComp)           :: child
    type(ESMF_CplComp)            :: conn
    integer                       :: verbosity
    character(len=10)             :: vString

    rc = ESMF_SUCCESS

    ! SetServices for DYN
    call NUOPC_DriverAddComp(driver, "DYN", dynSS, comp=child, _RC)
    verbosity = 0 ! reset
    verbosity = ibset(verbosity,0)  ! log basic intro/extro and indentation
    verbosity = ibset(verbosity,11) ! log info about data dependency loop
    verbosity = ibset(verbosity,12) ! log info about run time-loop
    write(vString,"(I10)") verbosity
!    call NUOPC_CompAttributeSet(child, name="Verbosity", value=vString, _RC)
    call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", _RC)

    ! SetServices for PHY
    call NUOPC_DriverAddComp(driver, "PHYDRV", phyDRVSS, comp=child, _RC)
    verbosity = 0 ! reset
    verbosity = ibset(verbosity,0)  ! log basic intro/extro and indentation
    verbosity = ibset(verbosity,11) ! log info about data dependency loop
    verbosity = ibset(verbosity,12) ! log info about run time-loop
!    call NUOPC_CompAttributeSet(child, name="Verbosity", value=vString, _RC)
    call NUOPC_CompAttributeSet(child, name="Verbosity", value="high", _RC)
    !call NUOPC_DriverAddComp(driver, srcCompLabel="PHYDRV", dstCompLabel="DYN", &
      !compSetServicesRoutine=cplSS, comp=conn, _RC)
    call NUOPC_DriverAddComp(driver, srcCompLabel="DYN", dstCompLabel="PHYDRV", &
      compSetServicesRoutine=cplSS, comp=conn, _RC)
    call NUOPC_DriverAddComp(driver, srcCompLabel="PHYDRV", dstCompLabel="DYN", &
      compSetServicesRoutine=cplSS, comp=conn, _RC)

  end subroutine

  !-----------------------------------------------------------------------------

end module
