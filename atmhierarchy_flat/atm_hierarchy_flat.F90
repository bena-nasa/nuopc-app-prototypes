#define I_AM_MAIN
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

program esmApp

  !-----------------------------------------------------------------------------
  ! Generic ESM application driver
  !-----------------------------------------------------------------------------

  use my_error_handling
  use ESMF
  use NUOPC
  use atm, only: atmSS => SetServices

  implicit none

  integer                 :: rc, urc
  type(ESMF_GridComp)     :: esmComp

  ! Initialize ESMF
  call ESMF_Initialize(logkindflag=ESMF_LOGKIND_MULTI, logAppendFlag=.false., &
    defaultCalkind=ESMF_CALKIND_GREGORIAN, _RC)

  call ESMF_LogSet(flush=.true., _RC)

  call ESMF_LogWrite("esmApp STARTING", ESMF_LOGMSG_INFO, _RC)

  !-----------------------------------------------------------------------------

  ! need to add "PHYEX" to the NUOPC Field Dictionary
  call NUOPC_FieldDictionaryAddEntry("MOISTEX", canonicalUnits="1", _RC)
  call NUOPC_FieldDictionaryAddEntry("RADEX", canonicalUnits="1", _RC)
  call NUOPC_FieldDictionaryAddEntry("BOBO", canonicalUnits="1", _RC)
  call NUOPC_FieldDictionaryAddEntry("DYNEX", canonicalUnits="1", _RC)

  !-----------------------------------------------------------------------------

  ! Create the earth system Component
  esmComp = ESMF_GridCompCreate(name="esm", _RC)

  ! SetServices for the earth system Component
  call ESMF_GridCompSetServices(esmComp, atmSS, userRc=urc, _RC)
  _VERIFY(urc)

  ! Call Initialize for the earth system Component
  call ESMF_GridCompInitialize(esmComp, userRc=urc, _RC)
  _VERIFY(urc)

  ! Call Run  for earth the system Component
  call ESMF_GridCompRun(esmComp, userRc=urc, _RC)
  _VERIFY(urc)

  ! Call Finalize for the earth system Component
  call ESMF_GridCompFinalize(esmComp, userRc=urc, _RC)
  _VERIFY(urc)

  ! Destroy the earth system Component
  call ESMF_GridCompDestroy(esmComp, _RC)

  call ESMF_LogWrite("esmApp FINISHED", ESMF_LOGMSG_INFO, _RC)

  ! Finalize ESMF
  call ESMF_Finalize()

end program
