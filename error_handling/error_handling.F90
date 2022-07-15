#include "error_handling.h"
module my_error_handling
   use esmf
   implicit none
   private

   public :: verify_abort
   public :: verify_return
   public :: print_message
   public :: print_pointer_address

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

   subroutine print_pointer_address(state,message,rc)
      type(ESMF_State) :: state
      character(len=*), intent(in) :: message
      integer, optional, intent(out) :: rc

      integer :: status

      integer :: i, rank, nfields
      character(len=ESMF_MAXSTR), allocatable :: fnames(:)
      type(ESMF_TypeKind_Flag) :: typekind
      type(c_ptr) :: base_address
      type(ESMF_Field) :: field
      real(kind=ESMF_KIND_R4), pointer :: ptr2d_r4(:,:),ptr3d_r4(:,:,:)
      real(kind=ESMF_KIND_R8), pointer :: ptr2d_r8(:,:),ptr3d_r8(:,:,:)

      call ESMF_StateGet(state,itemCount=nfields,_RC)
      allocate(fnames(nfields))
      call ESMF_StateGet(state,itemNameList=fnames,_RC)
      write(*,*)"Printing addresses from: ",trim(message)
      do i=1,nfields
         call ESMF_StateGet(state,trim(fnames(i)),field,_RC)
         call ESMF_FieldGet(field,rank=rank,typekind=typekind,_RC)
         if (typekind==ESMF_TYPEKIND_R4 .and. rank ==2) then
            call ESMF_FieldGet(field,0,farrayptr=ptr2d_r4,_RC)
            base_address = c_loc(ptr2d_r4)
         else if (typekind==ESMF_TYPEKIND_R8 .and. rank ==2) then
            call ESMF_FieldGet(field,0,farrayptr=ptr2d_r8,_RC)
            base_address = c_loc(ptr2d_r8)
         else if (typekind==ESMF_TYPEKIND_R4 .and. rank ==3) then
            call ESMF_FieldGet(field,0,farrayptr=ptr3d_r4,_RC)
            base_address = c_loc(ptr3d_r4)
         else if (typekind==ESMF_TYPEKIND_R8 .and. rank ==3) then
            call ESMF_FieldGet(field,0,farrayptr=ptr3d_r8,_RC)
            base_address = c_loc(ptr3d_r8)
         end if
         write(*,'(A,A10,A,I)')"Address of field: ",trim(fnames(i))," is ",transfer(base_address,0_C_INTPTR_T)
      enddo
   end subroutine

end module my_error_handling
