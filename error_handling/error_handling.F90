module my_error_handling
   use esmf
   implicit none
   private

   public :: verify_abort
   public :: verify_return
   public :: print_message

   contains

   function verify_return(status,file,line) result(res)
      logical :: res
      integer :: status
      character(len=*) :: file
      integer :: line
      if (status/=0) write(*,*)"Error in ",trim(file)," on line ",line
      res =ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, line=line, file=file)
      
   end function

   subroutine verify_abort(status,file,line)
      !logical :: res
      integer :: status
      character(len=*) :: file
      integer :: line
      logical :: res
      if (status/=0) write(*,*)"Error in ",trim(file)," on line ",line
      res =ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, line=line, file=file)
      if (res) call ESMF_Finalize(endflag=ESMF_END_ABORT)
      
   end subroutine

   subroutine print_message(A)
      character(len=*), intent(in) :: A
      type(ESMF_VM) :: vm
      integer :: localPet
      call ESMF_VMGetGlobal(vm)
      call ESMF_VMGet(vm,localPet=localPet)
      write(*,'(I3,1x,A)')localPet,A
   end subroutine

end module my_error_handling
