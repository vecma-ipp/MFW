      SUBROUTINE DPCGRC(IDO,N,X,P,R,Z,RELERR,ITMAX)
*
      REAL*8 X(N),P(N)
      REAL*8 R(N),Z(N)
      REAL*8 RELERR
      INTEGER IDO,N,ITMAX
*
      COMMON/DPCGR1/ITER
      INTEGER ITER
      COMMON/DPCGR2/RSNORM
      REAL*8 RSNORM
      COMMON/DPCGR3/C
      REAL*8 C
      REAL*8 ALFA,BETA,ERROR,C1
*
      IF(IDO.EQ.0) THEN
*       P=X
        DO I=1,N
          P(I)=X(I)
        ENDDO
*       RSNORM=|Z|
        RSNORM=0.
        DO I=1,N
          RSNORM=RSNORM+R(I)*R(I)
        ENDDO
        RSNORM=DSQRT(RSNORM)
        IDO=1
        ITER=0
        RETURN
      ELSE IF(IDO.EQ.1 .AND. ITER.EQ.0) THEN
*<      Z=A*P
*       R=R-Z
        DO I=1,N
          R(I)=R(I)-Z(I)
        ENDDO
        ERROR = 0.
        DO I = 1,N
          ERROR = ERROR + R(I)*R(I)
        ENDDO
        ERROR=DSQRT(ERROR)/RSNORM
        IF(ERROR.LE.RELERR) THEN
          IDO=3
          RETURN
        ENDIF
        IDO=2
        ITER=0
        RETURN
      ELSE IF(IDO.EQ.2 .AND.ITER.EQ.0) THEN
*<      Z=M-1*R
*       P=Z
        DO I=1,N
          P(I)=Z(I)
        ENDDO
*       C=(Z,R)
        C=0.
        DO I=1,N
          C=C+Z(I)*R(I)
        ENDDO
        IDO=1
        ITER=1
        RETURN
      ELSE IF(IDO.EQ.1) THEN
*<      Z=A*P
*       ALFA=C/(Z,P)
        ALFA=0.
        DO I=1,N
          ALFA=ALFA+Z(I)*P(I)
        ENDDO
        ALFA=C/ALFA
*       X=X+ALFA*P
        DO I=1,N
          X(I)=X(I)+ALFA*P(I)
        ENDDO
*       R=R-ALFA*Z
        DO I=1,N
          R(I)=R(I)-ALFA*Z(I)
        ENDDO
        ERROR = 0.
        DO I = 1,N
          ERROR = ERROR + R(I)*R(I)
        ENDDO
        ERROR=DSQRT(ERROR)/RSNORM
        IF(ERROR.LE.RELERR .OR. ITER.GE.ITMAX) THEN
          IDO=3
          RETURN
        ENDIF
        IDO=2
        ITER=ITER+1
        RETURN
      ELSE IF(IDO.EQ.2) THEN
*<      Z=M-1*R
*       BETA=(Z,R)/C
        C1=0.
        DO I=1,N
          C1=C1+Z(I)*R(I)
        ENDDO
        BETA=C1/C
        C=C1
*       P=Z+BETA*P
        DO I=1,N
          P(I)=Z(I)+BETA*P(I)
        ENDDO
        IDO=1
        RETURN
      ENDIF
*
      RETURN
      END






