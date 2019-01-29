!*DECK C3SA01B
!*CALL PROCESS
SUBROUTINE OUTGLOAD(KOPT,KFLAG)
  !        ###############################
  !
  !                                        AUTHORS:
  !                                        O. SAUTER, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  !   PRINT OUTPUT RELATED TO DIAGNOSTICS COMPUTED MAINLY IN ROUTINE GLOADD *
  !   AND IN SURFADD
  !   KOPT: SAME AS KOPT IN GLOADD
  !   KFLAG: VALUE OF NFLGADDIA(KOPT)
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  USE interpol
  IMPLICIT NONE
  !
  INTEGER          ::     JJ
  INTEGER          ::     JVA
  INTEGER          ::     J
  INTEGER          ::     II
  INTEGER          ::     IQ
  INTEGER          ::     ITXTLN
  INTEGER          ::     IPRO
  INTEGER          ::     IDIAQI
  INTEGER          ::     IDIAPRO
  INTEGER          ::     I
  INTEGER          ::     KOPT
  INTEGER          ::     KFLAG
  !
  !---*----*----*----*----*----*----*----*----*----*----*----*----*----*
  !
  IF (KFLAG .EQ. 0) RETURN
  !
  GO TO (100,200,300,400) KOPT
  !.......................................................................
  !
  !     1. EQUILIBRIUM DIAGNOSTICS RELATED TO COMPARISON WITH PRETOR AND OTHERS
  !     .  RBDIAG, TITLE IN RBDIAGTX, NUMBER OF PROFILES IN KFLAG
  !
100 CONTINUE
  !
  DO I=1,KFLAG
     CALL RARRAY(RBDIAGTX(I),RBDIAG(1,I),NISO1EFF1)
  END DO
  !
  RETURN
  !.......................................................................
  !
  !     2. DIAGNOSTIC ARRAYS AND VALUES ON RATIONAL Q SURFACES.
  !     DIAGARS, TITLE IN DIAGARTX, NB OF PROFILES IN KFLAG=NDIAGLAS/100,
  !     NUMBER OF Q VALUES FOR DIAG ON RATIONAL SURFACES IN NDIAGLAS - 100*KFLAG
  !
200 CONTINUE
  !
  IDIAPRO = NDIAGLAS/100
  IF (KFLAG .NE. IDIAPRO) THEN
     WRITE(6,'(/" PROBLEM IN 200 OUTGLOAD:",/" KFLAG= ",I4, &
          &       " IDIAPRO= ",I4," NDIAGLAS= ",I5)') KFLAG,IDIAPRO,NDIAGLAS
  ENDIF
  IDIAQI = NDIAGLAS - IDIAPRO*100
  WRITE(6,'(/" PROFILES AND INTEGER Q SUMMARY IN DIAGARS:",/)')
  DO IPRO=1,IDIAPRO
     CALL RARRAY(TRIM(DIAGARTX(IPRO)),DIAGARS(1,IPRO),NISO1EFF1)
  ENDDO
  WRITE(6,'(/,A)') TRIM(DIAGARTX(IDIAPRO+1))
  DO IQ=1,IDIAQI
     WRITE(6,'(1P14E10.2)') (DIAGARS(II,IDIAPRO+IQ),II=1,NDIAPERQ)
  ENDDO
  !
  RETURN
  !.......................................................................
  !
  !     3. PARAMETERS RELATED TO NEOCLASSICAL TEARING MODE
  !
300 CONTINUE
  !
  !   EXCEPTIONNALY: COMPUTE QUANTITIES NOT COMPUTED IN SURFADD OR GLOADD
  !
  !   BALOONING CRITERIA
  VALSNEO(46,1) = NCBAL(1)
  VALSNEO(46,NEONBQS) = NCBAL(NISO1EFF1)
  DO I=2,NEONBQS-1
     DO J=1,NISO1EFF1-1
        IF (SMISOP1(J+1) .GE. VALSNEO(1,I)) THEN
           VALSNEO(46,I) = FLINEAR(SMISOP1(J),SMISOP1(J+1),NCBAL(J)*RC1P, &
                &           NCBAL(J+1)*RC1P,VALSNEO(1,I))
           !   PUT 999 IF ONE ADJACENT SURFACE IS STABLE AND THE OTHER ONE NOT => MARGINAL?
           IF (NCBAL(J)*NCBAL(J+1).EQ.0 .AND. &
                &           (NCBAL(J)+NCBAL(J+1)).NE.0) VALSNEO(46,I) = 999
           GO TO 310
        ENDIF
     END DO
310  CONTINUE
  END DO
  !
  WRITE(6,'(//," NEOCLASSICAL RELEVANT DATA ON RATIONAL", &
       &     " Q SURFACES",//,"  GLOBAL DATA",/,"  ===========",/)')
  DO I=1,NEONBGLO
     WRITE(6,'(10X,A," = ",1PE13.5)') TITGLNEO(I),GLOBNEO(I)
  END DO
  !     VALUES ON Q SURFACES, 10 VALUES AT A TIME
  WRITE(6,'(/,"  LOCAL DATA IN CHEASE UNITS", &
       &     /,        "  ==========================",/)')
  WRITE(6,'(/11A11)') "QS  ", &
       &     (TITSNEO(JVA),JVA=1,MIN(10,NEONBVAL))
  DO I=1,NEONBQS
     IF (VALSNEO(1,I) .GE. 0._RKIND) THEN
        !   too confusing to change some in MKSA
        !%OS             VALSNEO(9,I) = VALSNEO(9,I) * GLOBNEO(1)
        !%OS             VALSNEO(13,I) = VALSNEO(13,I) * GLOBNEO(1)
        WRITE(6,'(1P18E11.3)') QVALNEO(I), &
             &         (VALSNEO(JVA,I),JVA=1,MIN(10,NEONBVAL))
     END IF
  END DO
  !
  DO JJ=11,NEONBVAL,10
     WRITE(6,'(/11A11)') "QS  ", &
          &       (TITSNEO(JVA),JVA=JJ,MIN(JJ+9,NEONBVAL))
     DO I=1,NEONBQS
        IF (VALSNEO(1,I) .GE. 0._RKIND) THEN
           WRITE(6,'(1P11E11.3)') QVALNEO(I), &
                &           (VALSNEO(JVA,I),JVA=JJ,MIN(JJ+9,NEONBVAL))
        END IF
     END DO
  END DO
  !
  RETURN
  !
400 CONTINUE
  !
  RETURN
END SUBROUTINE OUTGLOAD
