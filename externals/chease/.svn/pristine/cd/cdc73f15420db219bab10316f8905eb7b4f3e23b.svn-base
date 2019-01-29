MODULE counter
  IMPLICIT NONE
  INTEGER, PARAMETER :: nfmax=32
  LOGICAL :: used(nfmax) = .FALSE.
  INTEGER :: current_fid=0
CONTAINS

  INTEGER FUNCTION next_fid
    IMPLICIT NONE
    INTEGER :: i
    DO i=1,nfmax
       IF( .NOT. used(i) ) THEN
          used(i) = .TRUE.
          next_fid = i
          RETURN
       END IF
    END DO
    PRINT*, "Number of created files exceeds NFMAX =", nfmax
    STOP
  END FUNCTION next_fid

  SUBROUTINE closef(fid)
    IMPLICIT NONE
    INTEGER, intent(in) :: fid
    IF( fid .GT. nfmax) PRINT*, 'Invalid fid', fid
    used(fid) = .FALSE.
  END SUBROUTINE closef
END MODULE counter

PROGRAM main
  USE counter
  IMPLICIT NONE
  INTEGER :: fid1, fid2, fid3, fid4
!
  fid1 = next_fid(); PRINT*, 'Get fid =', fid1
  fid2 = next_fid(); PRINT*, 'Get fid =', fid2
  fid3 = next_fid(); PRINT*, 'Get fid =', fid3
  
  CALL closef(fid2); PRINT*, 'Close fid =', fid2
  fid4 = next_fid(); PRINT*, 'Get fid =', fid4
END PROGRAM main
