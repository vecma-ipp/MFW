!*DECK C2SA01
!*CALL PROCESS
SUBROUTINE MESH(K)
  !        ##################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !**********************************************************************
  !                                                                     *
  ! C2SA01 : K = 1 COMPUTE S-MESH FOR I* OR J-PARALLEL.                 *
  !                COMPUTE SIGMA & THETA MESHES FOR EQUILIBRIUM         *
  !                DISCRETIZATION. COMPUTE PLASMA RADII RHOS(THETA)     *
  !                AND D(RHOS(THETA)) / D(THETA).                       *
  !                                                                     *
  !          K = 2 COMPUTE ON EVERY GAUSSIAN INTEGRATION POINT FOR THE  *
  !                EQUILIBRIUM SOLVER:                                  *
  !                       - SIGMA & THETA                               *
  !                       - RHOS(THETA) & D(RHOS(THETA) / D(THETA)      *
  !                       - THE VALUES FB(I), I=1,...,16 OF THE BASIS   *
  !                         FUNCTIONS                                   *
  !                       - THE RELATION BETWEEN INVERSE CLOCKWISE AND  *
  !                         UP-DOWN NUMEROTATION OF THE THETA MESH      *
  !                                                                     *
  !          K = 3 COMPUTE THE S-MESH FOR ERATO, LION, MARS AND         *
  !                BALLOONING                                           *
  !                                                                     *
  !          K = 4 COMPUTE THE CHI-MESH FOR ERATO, LION, MARS AND       *
  !                BALLOONING                                           *
  !                                                                     *
  !          K = 5 COMPUTE THE VACUUM S-MESH FOR MARS.                  *
  !                                                                     *
  !          K = 6 COMPUTE THE S-MESH FOR THE BALLOONING OPTIMIZATION   *
  !                                                                     *
  !          K = 7 COMPUTE THE S-MESH AND THE THETA-MESH FOR PENN       *
  !                                                                     *
  !          K = 8 THETA MESH FOR XTOR                                  *
  !                                                                     *
  !          K = 9 EQDSK MESK: CONSTANT IN PSI                          *
  !                                                                     *
  !**********************************************************************
  !
  !
  USE globals
  IMPLICIT NONE
  !
  INTEGER          ::     J
  INTEGER          ::     J706
  INTEGER          ::     J707
  INTEGER          ::     J705
  INTEGER          ::     J704
  INTEGER          ::     J703
  INTEGER          ::     J701
  REAL(RKIND)      ::     ZDIF
  REAL(RKIND)      ::     ZADD
  INTEGER          ::     J702
  REAL(RKIND)      ::     ZWGT
  REAL(RKIND)      ::     ZRAC
  INTEGER          ::     J603
  INTEGER          ::     J601
  REAL(RKIND)      ::     ZDPS
  INTEGER          ::     J505
  INTEGER          ::     J504
  REAL(RKIND)      ::     ZEXP
  REAL(RKIND)      ::     ZD1
  REAL(RKIND)      ::     ZFPRIM
  REAL(RKIND)      ::     ZFA
  INTEGER          ::     JITER
  REAL(RKIND)      ::     ZALPHA
  REAL(RKIND)      ::     ZQ
  INTEGER          ::     J501
  INTEGER          ::     J405
  INTEGER          ::     J404
  REAL(RKIND)      ::     ZCSHFT
  INTEGER          ::     J403
  INTEGER          ::     J402
  INTEGER          ::     J401
  REAL(RKIND)      ::     ZDCHI
  INTEGER          ::     J302
  INTEGER          ::     J301
  REAL(RKIND)      ::     ZDP
  INTEGER          ::     I
  INTEGER          ::     J214
  INTEGER          ::     J215
  INTEGER          ::     J210
  INTEGER          ::     J211
  REAL(RKIND)      ::     ZF
  REAL(RKIND)      ::     ZS
  INTEGER          ::     J209
  INTEGER          ::     J212
  REAL(RKIND)      ::     ZS2
  REAL(RKIND)      ::     ZS1
  INTEGER          ::     J213
  INTEGER          ::     J208
  INTEGER          ::     J206
  INTEGER          ::     J205
  INTEGER          ::     J207
  INTEGER          ::     J202
  REAL(RKIND)      ::     ZRHOS
  INTEGER          ::     J201
  INTEGER          ::     J204
  INTEGER          ::     J109
  REAL(RKIND)      ::     ZRHOS4
  REAL(RKIND)      ::     ZRHOS3
  REAL(RKIND)      ::     ZRHOS2
  REAL(RKIND)      ::     ZRHOS1
  REAL(RKIND)      ::     ZT4
  REAL(RKIND)      ::     ZT3
  REAL(RKIND)      ::     ZT2
  REAL(RKIND)      ::     ZT1
  INTEGER          ::     J108
  REAL(RKIND)      ::     ZEPS
  INTEGER          ::     J107
  REAL(RKIND)      ::     ZDS
  INTEGER          ::     J106
  REAL(RKIND)      ::     ZT
  INTEGER          ::     J105
  INTEGER          ::     J104
  INTEGER          ::     ISHIFT
  INTEGER          ::     J103
  REAL(RKIND)      ::     ZDT
  INTEGER          ::     J102
  INTEGER          ::     J101
  REAL(RKIND)      ::     ZDPIPR
  INTEGER          ::     K
  DIMENSION &
       &   ZF(NPT,16),    ZRAC(NPMGS+1),ZRHOS(NTP1),    ZRHOS1(NTP1), &
       &   ZRHOS2(NTP1),  ZRHOS3(NTP1), ZRHOS4(NTP1),   ZS(NPT), &
       &   ZS1(NPT),      ZS2(NPT),     ZT(NTP1+NPCHI), ZT1(NPT), &
       &   ZT2(NPT),      ZT3(NPT),     ZT4(NPT),       ZWGT(NPMGS+1)
  !
  !---*----*----*----*----*----*----*----*----*----*----*----*----*----*
  !
  GOTO (100,200,300,400,500,600,700,800,900) K
  !
100 CONTINUE 
  !
  ZDPIPR = 1._RKIND / REAL(NISO - 1,RKIND)
  !
  DO J101=1,NISO
     CSIPRI(J101) = (J101 - 1._RKIND) * ZDPIPR
  END DO
  !
  !**********************************************************************
  !                                                                     *
  !        NMESHB = 0 ===> EQUIDISTANT S-MESH                           *
  !        NMESHB = 1 ===> WEIGHTED S-MESH                              *
  !        NPOIDB = 0 ===> NO WEIGHTING IS POSSIBLE FOR S-MESH          *
  !        SOLPDB = 1 ===> NO WEIGHTING IS POSSIBLE FOR S-MESH          *
  !                                                                     *
  !**********************************************************************
  !
  IF (NMESHB .NE. 0 .AND. NPOIDB .NE. 0 .AND. &
       &       SOLPDB .NE. 1._RKIND) THEN
     CALL PACKME(NISO,NPOIDB,CSIPRI,BPLACE,BWIDTH,SOLPDB)
  ENDIF
  !
  DO J102=1,NISO-1
     CSIPR(J102) = .5_RKIND * (CSIPRI(J102+1) + CSIPRI(J102))
  END DO
  !
  CSIPR(NISO) = 1._RKIND
  !
  !**********************************************************************
  !                                                                     *
  !        FILL IN THETA VALUES : CONSTANT STEP = ZDT                   *
  !                                                                     *
  !**********************************************************************
  !
  ZDT = 2._RKIND * CPI / REAL(NT,RKIND)
  !
  DO J103=1,NT1
     CT(J103) = .5_RKIND * (2._RKIND * J103 - 3._RKIND) * ZDT
  END DO
  !
  !**********************************************************************
  !                                                                     *
  !        NMESHD = 0 ===> EQUIDISTANT THETA-MESH                       *
  !        NMESHD = 1 ===> WEIGHTED THETA-MESH                          *
  !        NPOIDD = 0 ===> NO WEIGHTING IS POSSIBLE FOR THETA-MESH      *
  !        SOLPDD = 1 ===> NO WEIGHTING IS POSSIBLE FOR THETA-MESH      *
  !     ASSUME DPLACE IN [-PI,+PI] TO EASE SYMMETRIC PACKING            *
  !                                                                     *
  !**********************************************************************
  !    
  ISHIFT = 0
  !
  IF (NMESHD .NE. 0 .AND. NPOIDD .NE. 0 .AND. &
       &        SOLPDD .NE. 1._RKIND) THEN
     !
     DO J104=1,NPOIDD
        IF (DPLACE(J104) .GT. CPI) THEN
           PRINT *,' DPLACE(',J104,')=',DPLACE(J104), &
                &             ' SHOULD BE IN [-PI,PI]'
           STOP
        ENDIF
        IF (DPLACE(J104) .LT. 0._RKIND) &
             &                DPLACE(J104) = DPLACE(J104) + 2._RKIND*CPI
     END DO
     !
     CALL PACKMEP(NT1,NPOIDD,CT,DPLACE,DWIDTH,SOLPDD,NMESHD,NMESHPOLEXP)
     !
     DO J105=1,NPOIDD
        IF (DPLACE(J105) .GT. CPI) &
             &          DPLACE(J105) = DPLACE(J105) - 2._RKIND*CPI
     END DO
     !    
     ISHIFT = 1
  ENDIF
  !
  !**********************************************************************
  !                                                                     *
  !    THETA MESH WITH CONSTANT AREA BETWEEN CONSTANT THETA SURFACES    *
  !                                                                     *
  !**********************************************************************
  !
  IF (NDIFT .NE. 0) THEN
     CALL TETARE(CT,NT)
     ISHIFT = 1
  ENDIF
  !    
  !**********************************************************************
  !     *
  !     IF THETA MESH HAS BEEN PACKED OR NDIFT = 1 SHIFT THETA MESH FROM *
  !     [0 ; 2*PI] TO  [(CT(NT)-2*PI)/2 ; 2*PI-(CT(NT)-2*PI)/2]          *
  !     *
  !**********************************************************************
  !     
  IF (ISHIFT .EQ. 1) THEN
     ZT(1) = .5_RKIND * (CT(1) + CT(NT) - 2._RKIND*CPI)
     !     
     DO J106=2,NT1
        ZT(J106) = .5_RKIND * (CT(J106) + CT(J106-1))
     END DO
     !     
     CALL DCOPY(NT1,ZT,1,CT,1)
  ENDIF
  !
  !**********************************************************************
  !                                                                     *
  !        FILL IN SIGMA VALUES : CONSTANT STEP = ZDS                   *
  !                                                                     *
  !**********************************************************************
  !
  ZDS = 1._RKIND / REAL(NS,RKIND)
  !
  DO J107=1,NS1
     CSIG(J107) = (J107 - 1) * ZDS
  END DO
  !
  !**********************************************************************
  !                                                                     *
  !        NMESHC = 0 ===> EQUIDISTANT SIGMA-MESH                       *
  !        NMESHC = 1 ===> WEIGHTED SIGMA-MESH                          *
  !        NPOIDC = 0 ===> NO WEIGHTING IS POSSIBLE FOR SIGMA-MESH      *
  !        SOLPDC = 1 ===> NO WEIGHTING IS POSSIBLE FOR SIGMA-MESH      *
  !                                                                     *
  !**********************************************************************
  !
  IF (NMESHC .NE. 0 .AND. NPOIDC .NE. 0 .AND. &
       &       SOLPDC .NE. 1._RKIND) THEN
     CALL PACKME(NS1,NPOIDC,CSIG,CPLACE,CWIDTH,SOLPDC)
  ENDIF
  !
  !**********************************************************************
  !                                                                     *
  !        COMPUTE PLASMA SURFACE PARAMETERS:                           *
  !                                                                     *
  !        1) COMPUTE BOUNDARY VECTOR RADIUS : RHOS(CT(J))              *
  !                                                                     *
  !**********************************************************************
  !
  CALL BOUND(NT1,CT,RHOS)
  !
  ZEPS = 1.E-3_RKIND
  !
  DO J108=1,NT
     ZT1(J108) = CT(J108) - 2._RKIND * ZEPS
     ZT2(J108) = CT(J108) -      ZEPS
     ZT3(J108) = CT(J108) +      ZEPS
     ZT4(J108) = CT(J108) + 2._RKIND * ZEPS
  END DO
  !
  CALL BOUND(NT,ZT1,ZRHOS1)
  CALL BOUND(NT,ZT2,ZRHOS2)
  CALL BOUND(NT,ZT3,ZRHOS3)
  CALL BOUND(NT,ZT4,ZRHOS4)
  !
  DO J109=1,NT
     DRSDT(J109) = (ZRHOS1(J109)+8*(ZRHOS3(J109)-ZRHOS2(J109))- &
          &        ZRHOS4(J109))/(12._RKIND*ZEPS)
  END DO
  !
  DRSDT(NT1) = DRSDT(1)
  !
  RETURN
  !
200 CONTINUE 
  !
  !**********************************************************************
  !                                                                     *
  ! 4.5.2  COMPUTE RHO-SURFACE AND D(RHO-SURFACE)/DTHETA FOR EACH       *
  !        THETA GIVEN BY THE POSITION OF THE 16 GAUSSIAN INTEGRATION   *
  !        POINTS IN THE CELL.                                          *
  !                                                                     *
  !**********************************************************************
  !
  ZEPS = 1.E-3_RKIND
  !
  DO J204=1,NWGAUS
     DO J201=1,NT
        ZT(J201)  = CT(J201)+(CT(J201+1)-CT(J201))*DZETA(J204,2)
        ZT1(J201) = ZT(J201) - 2._RKIND * ZEPS
        ZT2(J201) = ZT(J201) -      ZEPS
        ZT3(J201) = ZT(J201) +      ZEPS
        ZT4(J201) = ZT(J201) + 2._RKIND * ZEPS
     END DO
     !
     CALL BOUND(NT,ZT,ZRHOS)
     CALL BOUND(NT,ZT1,ZRHOS1)
     CALL BOUND(NT,ZT2,ZRHOS2)
     CALL BOUND(NT,ZT3,ZRHOS3)
     CALL BOUND(NT,ZT4,ZRHOS4)
     !
     DO J202=1,NT
        YRST(J202,J204)   = ZRHOS(J202)
        YDRSDT(J202,J204) = (ZRHOS1(J202) + 8 * (ZRHOS3(J202) - &
             &           ZRHOS2(J202)) - ZRHOS4(J202)) / &
             &           (12._RKIND * ZEPS)
     END DO
  END DO
  !
  !**********************************************************************
  !                                                                     *
  ! 4.5.4. COMPUTE SIGMA AND THETA VALUES ON THE 16 INTEGRATION POINTS  *
  !                                                                     *
  !**********************************************************************
  !
  DO J207=1,NWGAUS
     DO J205=1,NS
        RSINT(J205,J207) = CSIG(J205)+(CSIG(J205+1)-CSIG(J205))* &
             &                            DZETA(J207,1)
     END DO
     DO J206=1,NT
        RTINT(J206,J207) = CT(J206) + (CT(J206+1) - CT(J206)) * &
             &           DZETA(J207,2)
     END DO
  END DO
  !
  DO J208=1,NT
     ZT1(J208) = CT(J208)
     ZT2(J208) = CT(J208+1)
  END DO
  !
  DO J213=1,NS
     CALL RESETR(ZS1,NT,CSIG(J213))
     CALL RESETR(ZS2,NT,CSIG(J213+1))
     !
     DO J212=1,NWGAUS
        DO J209=1,NT
           ZS(J209) = RSINT(J213,J212)
           ZT(J209) = RTINT(J209,J212)
        END DO
        !
        CALL BASIS1(NT,NPT,ZS1,ZS2,ZT1,ZT2,ZS,ZT,ZF)
        !
        DO J211=1,16
           DO J210=1,NT
              FB(J210,J212,J211,J213) = ZF(J210,J211)
           END DO
        END DO
     END DO
  END DO
  !
  !**********************************************************************
  !                                                                     *
  ! 4.5.6. COMPUTE UP AND DOWN NUMEROTATION STARTING FROM INVERSE       *
  !        CLOCKWISE NUMEROTATION                                       *
  !                                                                     *
  !**********************************************************************
  !
  DO J215=1,NS1
     DO J214=1,NT
        I = (J215 - 1) * NT + J214
        IF (J214 .GE. 2 .AND. J214 .LE. NT/2+1) THEN
           NUPDWN(I) = I + J214 - 2
        ELSE IF (J214 .GE. NT/2+2 .AND. J214 .LE. NT) THEN
           NUPDWN(I) = (J215 + 1) * NT - 2 * J214 + 3
        ELSE IF (J214 .EQ. 1) THEN
           NUPDWN(I) = I
        ENDIF
     END DO
  END DO
  !
  RETURN
  !
300 CONTINUE 
  !
  !**********************************************************************
  !                                                                     *
  ! 1.     INITIALIZATION  AND  COMPUTING S-MESH                        *
  !                                                                     *
  ! 1.1.   EQUIDISTANT S-MESH : ZDS = CONSTANT STEP IN S-DIRECTION      *
  !                                                                     *
  !**********************************************************************
  !
  ZDP = 1._RKIND / REAL(NPSI,RKIND)
  !
  DO J301=1,NPSI1
     CS(J301) = (J301 - 1) * ZDP
  END DO
  !
  !**********************************************************************
  !                                                                     *
  !        NMESHA = 0 ===> EQUIDISTANT S-MESH                           *
  !        NMESHA = 1 ===> WEIGHTED S-MESH ON SPECIFIED S VALUES        *
  !        NMESHA = 2 ===> WEIGHTED S-MESH ON SPECIFIED Q VALUES        *
  !        NMESHA = 3 ===> s-mesh with smoothed constant by step density*
  !        NMESHA = 4 ===> same as 1 but points exactly on APLACE
  !                                                                     *
  !        NPOIDA = 0 AND NPOIDQ = 0                                    *
  !                   ===> NO WEIGHTING IS POSSIBLE FOR S-MESH          *
  !        SOLPDA = 1 ===> NO WEIGHTING IS POSSIBLE FOR S-MESH          *
  !                                                                     *
  !**********************************************************************
  !
  IF ((NMESHA .EQ. 1 .OR. NMESHA .EQ. 2) .AND. &
       &       (NPOIDA .NE. 0 .OR. NPOIDQ .NE. 0) &
       & .AND. SOLPDA .NE. 1._RKIND) THEN
     !
     IF (NMESHA .EQ. 2) CALL QPLACS
     !
     IF (NPOIDA .NE. 0 .AND. SOLPDA .NE. 1._RKIND) THEN
        CALL PACKME(NPSI1,NPOIDA,CS,APLACE,AWIDTH,SOLPDA)
     ENDIF
  ELSE IF (NMESHA .EQ. 3 .AND. NPOIDA .GE. 2) THEN
     CALL PACKME1(NPSI1,NPOIDA,CS,APLACE,AWIDTH)
  ELSE IF (NMESHA .EQ. 4 .AND. NPOIDA .NE. 0) THEN
     IF (ANY(APLACE .GT. 0.) .AND. ANY(APLACE .LT. 1.)) THEN
        CALL PACKME2(NPSI1,NPOIDA,CS,APLACE,AWIDTH,SOLPDA)
     ELSE
        PRINT *,' WARNING: APLACE>0 AND APLACE<1 required for NMESHA=4'
        PRINT *,' NMESHA=1 USED INSTEAD'
        CALL PACKME(NPSI1,NPOIDA,CS,APLACE,AWIDTH,SOLPDA)
     END IF
  ENDIF
  !
  DO J302=1,NPSI
     CSM(J302) = .5_RKIND * (CS(J302+1) + CS(J302))
  END DO
  !
  CSM(NPSI1) = 1._RKIND
  !
  !**********************************************************************
  !                                                                     *
  !    S-MESH WITH CONSTANT FLUX VOLUME BETWEEN SURFACES                *
  !                                                                     *
  !**********************************************************************
  !
  IF (NDIFPS .EQ. 1) CALL PSVOL
  !
  RETURN
  !
400 CONTINUE 
  !
  !**********************************************************************
  !                                                                     *
  ! 4. FILL IN EQUIDISTANT CHI-MESH                                     *
  !                                                                     *
  !**********************************************************************
  !
  ZDCHI = 2._RKIND * CPI / REAL(NCHI,RKIND)
  !
  DO J401=1,NCHI1
     CHI(J401) = (J401 - 1.5_RKIND) * ZDCHI
  END DO
  !
  !**********************************************************************
  !                                                                     *
  !        NMESHE = 0 ===> EQUIDISTANT CHI-MESH                         *
  !        NMESHE = 1 ===> WEIGHTED CHI-MESH                            *
  !        NPOIDE = 0 ===> NO WEIGHTING IS POSSIBLE FOR CHI-MESH        *
  !        SOLPDE = 1 ===> NO WEIGHTING IS POSSIBLE FOR CHI-MESH        *
  !                                                                     *
  !**********************************************************************
  !
  IF (NMESHE .NE. 0 .AND. NPOIDE .NE. 0 .AND. &
       &           SOLPDE .NE. 1._RKIND) THEN
     !     
     DO J402=1,NPOIDE
        IF (EPLACE(J402) .GT. CPI) THEN
           PRINT *,' EPLACE(',J402,')=',EPLACE(J402), &
                &             ' SHOULD BE IN [-PI,PI]'
           STOP
        ENDIF
        IF (EPLACE(J402) .LT. 0.0_RKIND) &
             &                EPLACE(J402) = EPLACE(J402) + 2._RKIND*CPI
     END DO

     CALL PACKMEP(NCHI1,NPOIDE,CHI,EPLACE,EWIDTH,SOLPDE,NMESHE,NMESHPOLEXP)
     !     
     DO J403=1,NPOIDE
        IF (EPLACE(J403) .GT. CPI) &
             &                EPLACE(J403) = EPLACE(J403) - 2._RKIND*CPI
     END DO
     !
     ZCSHFT = .5_RKIND * CHI(2)
     !
     DO J404=1,NCHI1
        CHI(J404) = CHI(J404) - ZCSHFT
     END DO
  ENDIF
  !
  DO J405=1,NCHI
     CHIM(J405) = .5_RKIND * (CHI(J405) + CHI(J405+1))
  END DO
  !
  RETURN
  !
500 CONTINUE 
  !
  IF (NVEXP .EQ. 0) THEN
     !
     !**********************************************************************
     !                                                                     *
     !    EQUIDISTANT VACUUM S-MESH FOR MARS                               *
     !                                                                     *
     !**********************************************************************
     !
     DO J501 = 1, NV
        CSV(J501) = 1._RKIND + (REXT - 1._RKIND) * REAL(J501-1,RKIND) / REAL(NV,RKIND)
     END DO
     !
     CSV(NV+1) = REXT
     !
     !**********************************************************************
     !                                                                     *
     !    EXPONENTIAL VACUUM S-MESH FOR MARS                               *
     !                                                                     *
     !    IF NVEXP = 1 EXPONENTIAL MESH, OTHERWISE EQUIDISTANT             *
     !    CELL SIZES CSV(I+1) - CS(I) = ZALPHA**(I-1)*(CSV(2) - CSV(1))    *
     !    AND CSV(2) - CSV(1) = CS(NPSI1) - CS(NPSI)                       *
     !                                                                     *
     !**********************************************************************
     !
  ELSE IF (NV .EQ. 1) THEN
     !
     CSV(1) = 1.0_RKIND
     CSV(NV+1) = REXT
     !
  ELSE
     !
     !     NVEXP.NE.0 .AND. NV.NE.1
     !
     ZEPS = RC1M13
     !
     !.. FIRST DETERMINE ZALPHA TO SATISFY ABOVE RELATIONS
     !
     ZQ     = (REXT - 1._RKIND) / (CS(NPSI1) - CS(NPSI))
     ZALPHA = 2._RKIND
     !
     IF (ZQ .LT. NV) ZALPHA = 0.5_RKIND
     !
     !...START VALUE FOR ZALPHA
     !
     JITER = 0
     !
502  CONTINUE 
     !
     ZFA    = (ZALPHA**NV - 1._RKIND) / (ZALPHA - 1._RKIND) - ZQ
     JITER  = JITER + 1
     ZFPRIM = (((NV - 1._RKIND) * ZALPHA - NV) * ZALPHA**(NV-1) + 1._RKIND) / &
          &               (ZALPHA - 1._RKIND)**2
     ZFA    = ZFA / ZFPRIM
     ZALPHA = ZALPHA - ZFA
     !      
     IF (ABS(ZFA) .LT. RC1M12) GOTO 503
     !
     IF (JITER .LT. 500) GOTO 502
     !
     WRITE(*,*) ' ITERATION FOR ALPHA DOES NOT CONVERGE'
     STOP 'NO_CONV'
     !
503  CONTINUE 
     !
     ZD1    = CS(NPSI1) - CS(NPSI)
     CSV(1) = 1._RKIND
     ZEXP   = 1._RKIND
     !      
     DO J504=1,NV
        ZEXP        = ZEXP * ZALPHA
        CSV(J504+1) = 1._RKIND + ZD1 * (ZEXP - 1._RKIND) / (ZALPHA - 1._RKIND)
     END DO
  ENDIF
  !
  DO J505=1,NV
     CSMV(J505) = .5_RKIND * (CSV(J505+1) + CSV(J505))
  END DO
  !
  RETURN
  !
600 CONTINUE 
  !
  !**********************************************************************
  !                                                                     *
  ! 5. S-MESH FOR BALLOONING OPTIMIZATION                               *
  !                                                                     *
  !**********************************************************************
  !
  ZDPS = 1._RKIND / REAL(NPPR,RKIND)
  !
  DO J601=1,NPPR+1
     PCS(J601) = (J601 - 1)  * ZDPS
  END DO
  !
  PCS(NPPR+1) = 1._RKIND
  !
  !**********************************************************************
  !                                                                     *
  !        NMESHB = 0 ===> EQUIDISTANT S-MESH                           *
  !        NMESHB = 1 ===> WEIGHTED S-MESH                              *
  !        NPOIDB = 0 ===> NO WEIGHTING IS POSSIBLE FOR S-MESH          *
  !        SOLPDB = 1 ===> NO WEIGHTING IS POSSIBLE FOR S-MESH          *
  !                                                                     *
  !**********************************************************************
  !
  IF (NMESHB .NE. 0 .AND. NPOIDB .NE. 0 .AND. &
       &       SOLPDB .NE. 1._RKIND) THEN
     CALL PACKME(NPPR+1,NPOIDB,PCS,BPLACE,BWIDTH,SOLPDB)
  ENDIF
  !
  DO J603=1,NPPR
     PCSM(J603) = .5_RKIND * (PCS(J603+1) + PCS(J603))
  END DO
  !
  PCSM(NPPR+1) = 1._RKIND
  !
  RETURN
  !
  !**********************************************************************
  !                                                                     *
  ! 7.1. CSPEN-MESH                                                     *
  !                                                                     *
  !**********************************************************************
  !
700 CONTINUE 
  !
  CALL DCOPY(NPSI,CS(2),1,CSPEN(5),5)
  CALL GAUSS(NMGAUS,ZRAC,ZWGT)
  !
  DO J702=1,NPSI
     ZADD = CS(J702+1) + CS(J702)
     ZDIF = CS(J702+1) - CS(J702)
     !
     I = (J702 - 1) * (NMGAUS + 1)
     !
     DO J701=1,NMGAUS
        CSPEN(I+J701) = .5_RKIND * (ZADD + ZDIF * ZRAC(J701))
     END DO
  END DO
  !
  !**********************************************************************
  !                                                                     *
  ! 7.2. FILL IN EQUIDISTANT CTPEN-MESH                                 *
  !                                                                     *
  !**********************************************************************
  !
  ZDT = 2._RKIND * CPI / REAL(NCHI,RKIND)
  !
  DO J703=1,NCHI1
     ZT(J703) = (J703 - 1._RKIND) * ZDT
  END DO
  !
  !**********************************************************************
  !                                                                     *
  !        NMESHE = 0 ===> EQUIDISTANT CTPEN-MESH                       *
  !        NMESHE = 1 ===> WEIGHTED CTPEN-MESH                          *
  !        NPOIDE = 0 ===> NO WEIGHTING IS POSSIBLE FOR CTPEN-MESH      *
  !        SOLPDE = 1 ===> NO WEIGHTING IS POSSIBLE FOR CTPEN-MESH      *
  !                                                                     *
  !**********************************************************************
  !
  IF (NMESHE .NE. 0 .AND. NPOIDE .NE. 0 .AND. &
       &           SOLPDE .NE. 1._RKIND) THEN
     !     
     DO J704=1,NPOIDE
        IF (EPLACE(J704) .GT. CPI) THEN
           PRINT *,' EPLACE(',J704,')=',EPLACE(J704), &
                &             ' SHOULD BE IN [-PI,PI]'
           STOP
        ENDIF
        IF (EPLACE(J704) .LT. 0.0_RKIND) &
             &                EPLACE(J704) = EPLACE(J704) + 2._RKIND*CPI

     END DO
     !     
     CALL PACKMEP(NCHI1,NPOIDE,ZT,EPLACE,EWIDTH,SOLPDE,NMESHE,NMESHPOLEXP)
     !     
     DO J705=1,NPOIDE
        IF (EPLACE(J705) .GT. CPI) &
             &                EPLACE(J705) = EPLACE(J705) - 2._RKIND*CPI
     END DO
     !
  ENDIF
  !
  !**********************************************************************
  !                                                                     *
  ! 7.3. CTPEN MESH WITH CONSTANT AREA BETWEEN CONSTANT THETA SURFACES  *
  !                                                                     *
  !**********************************************************************
  !
  IF (NDIFT .NE. 0) THEN
     CALL TETARE(ZT,NCHI)
  ENDIF
  !
  CALL DCOPY(NCHI1,ZT,1,CTPEN,5)
  !
  DO J707=1,NCHI
     ZADD = ZT(J707+1) + ZT(J707)
     ZDIF = ZT(J707+1) - ZT(J707)
     !
     I = (J707 - 1) * (NMGAUS + 1)
     !
     DO J706=1,NMGAUS
        CTPEN(I+J706+1) = .5_RKIND * (ZADD + ZDIF * ZRAC(J706))
     END DO
  END DO
  !
  RETURN
  !
800 CONTINUE 
  !
  !**********************************************************************
  !                                                                     *
  ! 8. FILL IN EQUIDISTANT CTXT-MESH                                    *
  !                                                                     *
  !**********************************************************************
  !
  ZDT = 2._RKIND * CPI / REAL(NTNOVA,RKIND)
  !
  DO J=1,NTNOVA
     CTXT(J) = REAL(J-1,RKIND) * ZDT
  END DO
  !
  ZDT = 2._RKIND * CPI / REAL(NTNOVA*MDT,RKIND)
  !
  DO J=1,NTNOVA*MDT
     CTXT_REFINED(J) = REAL(J-1,RKIND) * ZDT
  END DO
  !
  RETURN
  !
900 CONTINUE 
  !
  !**********************************************************************
  !                                                                     *
  ! 9.   EQDSK-MESH: CONSTANT IN PSI                                    *
  !                                                                     *
  !**********************************************************************
  !
  ZDP = 1._RKIND / REAL(NPSI,RKIND)
  !
  DO J=1,NPSI1
     CS(J) = SQRT(REAL(J-1,RKIND)*ZDP)
  END DO
  DO J=1,NPSI1
     CSM(J) = CS(J)
  END DO
  CSM(1) = 0.25_RKIND * CS(2)
  !
  RETURN
END SUBROUTINE MESH
