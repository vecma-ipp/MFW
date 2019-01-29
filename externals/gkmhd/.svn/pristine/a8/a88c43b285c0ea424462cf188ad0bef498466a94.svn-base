MODULE Dim

#ifdef ITMCPOs
  USE Phys_Constants
#endif

  IMPLICIT NONE

!...  npesx is decomposition over x, must be 1 for all but MPP
!...  npes ends up as product of npes?
!...  npes?0 remembers the values for I/O

  INTEGER :: npesx=1,npes=1
  INTEGER :: mype=0,mypex=0
  INTEGER :: mypexdir=0
  INTEGER :: npesx0=1

!...  the 00 are global dimensions

  INTEGER :: nx00=32,ny00=128

!...  the 0 are local dimensions for each PE
!...  n_neighbors is zero for pure plotting w/o FD operations

  INTEGER :: nx0,ny0
  INTEGER :: ngdx=1,nx,nxm,nxm2,nxm3,nxm4,nx2
  INTEGER :: ngdy=1,ny,nym,nym2,nym3,nym4,ny2
  INTEGER :: n_neighbors = 0
  INTEGER :: nk,nl,nvars=9,neqs=2,nadvect=11,nvsnap=9

!...  var indices 

  INTEGER :: mupsi=1,mutheta=2,muj=3,mupressure=4,muphi=5,muphige=6

!...  miscellany

#ifdef ITMCPOs
#else
  REAL :: pi=3.1415927,tpi=6.2831854
#endif

END MODULE Dim

