!*DECK C1S01
!*CALL PROCESS
SUBROUTINE LABRUN
  !        #################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !**********************************************************************
  !                                                                     *
  ! C1S01 LABEL THE RUN                                                 *
  !                                                                     *
  !**********************************************************************
  !
  !
  USE globals
  IMPLICIT NONE
  !
  !---*----*----*----*----*----*----*----*----*----*----*----*----*----*
  !
  !**********************************************************************
  !                                                                     *
  ! 1. READ THE LABELS                                                  *
  !                                                                     *
  !**********************************************************************
  !
  READ (5,1000) LABEL1
  READ (5,1000) LABEL2
  READ (5,1000) LABEL3
  READ (5,1000) LABEL4
  !
  !**********************************************************************
  !                                                                     *
  ! 2. WRITE THE HEADING                                                *
  !                                                                     *
  !**********************************************************************
  !
  CALL PAGE
  CALL BLINES(30)
  WRITE(*,'(16x,a)') 'C  CCCCCCCC      H        HH      E  EEEEEEEE      A  AAAAAAAA      S  SSSSSSSS      E  EEEEEEEE'
  WRITE(*,'(16x,a)') 'CC  CCCCCCCC     HH       HHH     EE  EEEEEEEE     AA  AAAAAAAA     SS  SSSSSSSS     EE  EEEEEEEE'
  WRITE(*,'(16x,a)') 'CCC  CCCCCCC     HHH      HHH     EEE  EEEEEEE     AAA  AAAAAAA     SSS  SSSSSSS     EEE  EEEEEEE'
  WRITE(*,'(16x,a)') 'CCC      CCC     HHH      HHH     EEE              AAA      AAA     SSS              EEE'
  WRITE(*,'(16x,a)') 'CCC              HHH      HHH     EEE              AAA      AAA     SSS              EEE'
  WRITE(*,'(16x,a)') 'CCC              HHHHHHH  HHH     EEEEEEE          AAAAAAA  AAA     SSSSSSSSSSS      EEEEEEE'
  WRITE(*,'(16x,a)') 'CCC              HHHHHHHH HHH     EEEEEEEE         AAAAAAAA AAA     SSSSSSSSSSSS     EEEEEEEE'
  WRITE(*,'(16x,a)') 'CCC              HHHHHHHHHHHH     EEEEEEEEE        AAAAAAAAAAAA     SSSSSSSSSSSS     EEEEEEEEE'
  WRITE(*,'(16x,a)') 'CCC              HHH      HHH     EEE              AAA      AAA              SSS     EEE'
  WRITE(*,'(16x,a)') 'CCC      CCC     HHH      HHH     EEE              AAA      AAA              SSS     EEE'
  WRITE(*,'(16x,a)') 'CCCCCCCCCCCC     HHH      HHH     EEEEEEEEEEEE     AAA      AAA     SSSSSSSSSSSS     EEEEEEEEEEEE'
  WRITE(*,'(16x,a)') 'CCCCCCCCCCCC     HHH      HHH     EEEEEEEEEEEE     AAA      AAA     SSSSSSSSSSSS     EEEEEEEEEEEE'
  WRITE(*,'(16x,a)') ' CCCCCCCCCC       HH       HH      EEEEEEEEEE       AA       AA      SSSSSSSSSS       EEEEEEEEEE'
  CALL BLINES(10)
  WRITE (6,1030)
  !
  !**********************************************************************
  !                                                                     *
  ! 3. WRITE THE LABELS                                                 *
  !                                                                     *
  !**********************************************************************
  !
  CALL BLINES(10)
  CALL MESAGE(LABEL1)
  CALL BLINES(1)
  CALL MESAGE(LABEL2)
  CALL BLINES(1)
  CALL MESAGE(LABEL3)
  CALL BLINES(1)
  CALL MESAGE(LABEL4)
  CALL PAGE
  !
  RETURN
1000 FORMAT(A80)
1030 FORMAT(40X, &
       &          'CUBIC HERMIT ELEMENT AXISYMETRIC STATIC EQUILIBRIUM')
  !
END SUBROUTINE LABRUN
