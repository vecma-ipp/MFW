         SUBROUTINE RUNTIM
!        -----------------
!
!  UPDATE CPU TIME (SECS) AND PRINT IT
!
         use prec_const
         use globals
         implicit none
!         
         real(rkind) :: cptime
!
         CPTIME = 0._rkind
!     
         call cpu_time(cptime)
!
         CPTIME = CPTIME - STIME
!
         WRITE (6,9900) CPTIME
!
         RETURN
!
 9900    FORMAT(/,1X,'CPU TIME USED SO FAR =',1PE14.6,' SECS')
!
         END
