#include "error_handling.h"
module MAPL_redu
   use esmf
   use NUOPC
   use my_error_handling
   implicit none
   private

   public :: MAPL_Realize_Accepted
   public :: MAPL_realize_provided_field
   public :: make_a_grid

   contains

!  since we do not need to know about the nature of the field
!  here I can make this generic, realize provided, forgetaboutit
   subroutine MAPL_Realize_Accepted(import,export,rc)
      type(ESMF_State), intent(inout) :: import
      type(ESMF_State), intent(inout) :: export
      integer, intent(out), optional :: rc

      integer :: item_count,i
      type(ESMF_Field) :: field
      type (ESMF_StateItem_Flag), allocatable   :: item_types(:)
      character(len=ESMF_MAXSTR), allocatable   :: item_names(:)
      character(len=ESMF_MAXSTR) :: transfer_action

      ! first we do imports
      call ESMF_StateGet(import, itemCount=item_count, _RC)
      allocate(item_names(item_count))
      allocate(item_types(item_count))
      call ESMF_StateGet(import, ITEMNAMELIST=item_names, ITEMTYPELIST=item_types, _RC)
      do i=1,item_count
         if (item_types(i) == ESMF_StateItem_Field) then
            write(*,*)"trying to realize from import: ",trim(item_names(i))
            call ESMF_StateGet(import,field=field,itemname=item_names(i),_RC)
            call NUOPC_GetAttribute(field,name="ConsumerTransferOffer",value=transfer_action,_RC)
            if (transfer_action =="accept") then
               call NUOPC_Realize(import,fieldname=item_names(i),_RC)
               write(*,*)"actually realized from import: ",trim(item_names(i))
            end if
         end if
      enddo
      deallocate(item_names,item_types)

      ! first we do exports
      call ESMF_StateGet(export, itemCount=item_count, _RC)
      allocate(item_names(item_count))
      allocate(item_types(item_count))
      call ESMF_StateGet(export, ITEMNAMELIST=item_names, ITEMTYPELIST=item_types, _RC)
      do i=1,item_count
         if (item_types(i) == ESMF_StateItem_Field) then
            write(*,*)"trying to realize from export: ",trim(item_names(i))
            call ESMF_StateGet(export,field=field,itemname=item_names(i),_RC)
            call NUOPC_GetAttribute(field,name="ProducerTransferOffer",value=transfer_action,_RC)
            if (transfer_action =="accept") then
               call NUOPC_Realize(export,fieldname=item_names(i),_RC)
               write(*,*)"actually realized from export: ",trim(item_names(i))
            end if
         end if
      enddo

   end

   subroutine MAPL_Realize_Provided_Field(state,grid,name,lm,rc)
      type(ESMF_State), intent(inout) :: state
      type(ESMF_Grid), intent(inout) :: grid
      character(len=*), intent(in) :: name
      integer, optional, intent(in) :: lm
      integer, optional, intent(out) :: rc

      character(len=ESMF_MAXSTR) :: transfer_action,search_for
      type(ESMF_Field) :: old_field,new_field
      type(ESMF_StateIntent_Flag) :: state_intent

      call ESMF_StateGet(state,stateIntent=state_intent,_RC)
      if (state_intent == ESMF_STATEINTENT_EXPORT) then
         search_for = "ProducerTransferOffer" 
      else if (state_intent == ESMF_STATEINTENT_IMPORT) then
         search_for = "ConsumerTransferOffer"
      end if 
      call ESMF_StateGet(state,field=old_field,itemName=trim(name),_RC)
      call NUOPC_GetAttribute(old_field,name=trim(search_for), value=transfer_action,_RC)
      if (trim(transfer_action) == "provide") then
         write(*,*)"Found provided so realize: ",trim(name)
         if (present(lm)) then
            new_field = ESMF_FieldCreate(grid,ESMF_TYPEKIND_R4,name=trim(name),ungriddedLBound=[1],ungriddedUbound=[lm],_RC)
         else
            new_field = ESMF_FieldCreate(grid,ESMF_TYPEKIND_R4,name=trim(name),_RC)
         end if
         call NUOPC_Realize(state,field=new_field,_RC)
      end if
   end subroutine
          
   function make_a_grid(im,jm,rc) result(grid)
      type(ESMF_Grid) :: grid
      integer, intent(in), optional :: im
      integer, intent(in), optional :: jm
      integer, intent(out), optional :: rc 

      integer :: local_im, local_jm

      local_im = 100
      local_jm = 100
      if (present(im)) local_im=im
      if (present(jm)) local_jm=jm

      grid = ESMF_GridCreateNoPeriDimUfrm(maxIndex=[local_im,local_jm], &
      minCornerCoord=(/0._ESMF_KIND_R8, -50._ESMF_KIND_R8/), &
      maxCornerCoord=(/360._ESMF_KIND_R8, 90._ESMF_KIND_R8/), &
      coordSys=ESMF_COORDSYS_CART, staggerLocList=(/ESMF_STAGGERLOC_CENTER/), &
      _RC)

   end function

end module
