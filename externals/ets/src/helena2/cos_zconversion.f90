subroutine cos_zconversion(e,nbrho)
   use itm_types
   implicit none

   integer,intent(in)            :: nbrho
   real(R8),dimension(nbrho) :: e
  
   if (nbrho < 4) then
       print*, "error"
       return
   endif

   ! continuite au centre pour les NaN
   if ( (abs(e(1)).gt.huge(e(1))) .or. (abs(e(1)).eq.0)    ) then
       e(1) = (61.0_8/46.0_8) * e(2) - (9.0_8/23.0_8) * e(3) + (3.0_8/46.0_8) * e(4)
   endif

end subroutine cos_zconversion
