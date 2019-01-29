!*DECK NUMREC03
!*CALL PROCESS
SUBROUTINE REALFT(DATA,N,ISIGN)
  !     -------------------------------
  !
  !     FROM NUMERICAL RECIPES
  !
  !     USES FOUR1
  !
  !     CALCULATES THE FOURIER TRANSFORM OF A SET OF N REAL-VALUED DATA POINTS.
  !     REPLACES THIS DATA (WHICH IS STORED IN ARRAY DATA(1:N)) BY THE POSITIVE 
  !     HALF OF ITS COMPLEX FOURIER TRANSFORM. THE REAL-VALUED FIRST AND LAST 
  !     COMPONENTS OF THE COMPLEX TRANSFORM ARE RETURNED AS ELEMENTS DATA(1) 
  !     AND DATA(2), RESPECTIVELY. N MUST BE A POWER OF 2. THIS ROUTINE ALSO 
  !     CALCULATES THE INVERSE TRANSFORM OF A
  !     COMPLEX DATA ARRAY IF IT IS THE TRANSFORM OF REAL DATA. 
  !     (RESULT IN THIS CASE MUST BE MULTIPLIED BY 2/N)
  !
  USE prec_const
  IMPLICIT NONE
  !%OS      REAL WR,WI,WPR,WPI,WTEMP,THETA
  REAL(RKIND)   	::  	WTEMP 	! <realft.f90>
  REAL(RKIND)   	::  	H2I 	! <realft.f90>
  REAL(RKIND)   	::  	H2R 	! <realft.f90>
  REAL(RKIND)   	::  	H1I 	! <realft.f90>
  REAL(RKIND)   	::  	H1R 	! <realft.f90>
  REAL(RKIND)   	::  	WIS 	! <realft.f90>
  REAL(RKIND)   	::  	WRS 	! <realft.f90>
  INTEGER       	::  	I4 	! <realft.f90>
  INTEGER       	::  	I3 	! <realft.f90>
  INTEGER       	::  	I2 	! <realft.f90>
  INTEGER       	::  	I1 	! <realft.f90>
  INTEGER       	::  	I 	! <realft.f90>
  INTEGER       	::  	N2P3 	! <realft.f90>
  REAL(RKIND)   	::  	WPI 	! <realft.f90>
  REAL(RKIND)   	::  	WPR 	! <realft.f90>
  REAL(RKIND)   	::  	DATA 	! <realft.f90>
  REAL(RKIND)   	::  	C2 	! <realft.f90>
  INTEGER       	::  	ISIGN 	! <realft.f90>
  REAL(RKIND)   	::  	C1 	! <realft.f90>
  REAL(RKIND)   	::  	WI 	! <realft.f90>
  REAL(RKIND)   	::  	WR 	! <realft.f90>
  INTEGER       	::  	N 	! <realft.f90>
  REAL(RKIND)   	::  	THETA 	! <realft.f90>
  DIMENSION DATA(*)
  !%OS      DIMENSION DATA(2*N+2)
  !-----------------------------------------------------------------------
  !     DOUBLE PRECISION FOR THE TRIGONOMETRIC RECURRENCES
  THETA=6.28318530717959_RKIND/2.0_RKIND/REAL(N,RKIND)
  WR=1.0_RKIND
  WI=0.0_RKIND
  C1=0.5_RKIND
  !     FORWARD TRANSFORM
  IF (ISIGN.EQ.1) THEN
     C2=-0.5_RKIND
     CALL FOUR1(DATA,N,+1)
     DATA(2*N+1)=DATA(1)
     DATA(2*N+2)=DATA(2)
  ELSE
     !     SET UP FOR AN INVERSE TRANSFORM
     C2=0.5_RKIND
     THETA=-THETA
     DATA(2*N+1)=DATA(2)
     DATA(2*N+2)=0.0_RKIND
     DATA(2)=0.0_RKIND
  ENDIF
  WPR=-2.0_RKIND* SIN(0.5_RKIND*THETA)**2
  WPI= SIN(THETA)
  N2P3=2*N+3
  DO I=1,N/2+1
     I1=2*I-1
     I2=I1+1
     I3=N2P3-I2
     I4=I3+1
     WRS=    (WR)
     WIS=    (WI)
     !     THE TWO SEPARATE TRANSFORMS ARE SEPARATED OUT OF DATA
     H1R=C1*(DATA(I1)+DATA(I3))
     H1I=C1*(DATA(I2)-DATA(I4))
     H2R=-C2*(DATA(I2)+DATA(I4))
     H2I=C2*(DATA(I1)-DATA(I3))
     !     AND RECOMBINED TO FORM TRUE TRANSFORM OF ORIGINAL REAL DATA
     DATA(I1)=H1R+WRS*H2R-WIS*H2I
     DATA(I2)=H1I+WRS*H2I+WIS*H2R
     DATA(I3)=H1R-WRS*H2R+WIS*H2I
     DATA(I4)=-H1I+WRS*H2I+WIS*H2R
     !     RECURRENCE
     WTEMP=WR
     WR=WR*WPR-WI*WPI+WR
     WI=WI*WPR+WTEMP*WPI+WI
  END DO
  IF (ISIGN.EQ.1) THEN
     DATA(2)=DATA(2*N+1)
  ELSE
     !     INVERSE TRANSFORM
     CALL FOUR1(DATA,N,-1)
  ENDIF
  RETURN
END SUBROUTINE REALFT
