      subroutine pSplineEval(nr,nz,nCoeff,rpkg,zpkg,fspl,
     &     np,r,z,f,ierr)
   
      implicit none

C/*--------------------------------------------------------------------
C * pSplineEval.f
C *
C * wrapper to the fortran pSpline evaluation routines from NTCC
C * 
C * Modification history
C * --------------------
C * 05-June 2008 1.4.3 Created
C *
C *
C *
C *-------------------------------------------------------------------*/
      
      integer ilinx,iliny,ierr,i,j,k,l,m,iselect(6)
C Input requested data
      integer nr,nz,nCoeff,inf3,np
      real*8 r(np),z(np)
C The input array for the spline fit
      real*8 fspl(4,4,nr,nz), rpkg(nr,4), zpkg(nz,4), ztol
      integer iper,imsg,itol,ialg,iwarn
C Return data
      real*8 coeff(nCoeff),rKnot(nr),zKnot(nz)
C Return data
      real*8 f(np,6)

      do i=1,6
         iselect(i) = 0
      enddo
      iselect(1) = 1
      ilinx = 1
      iliny = 1
      inf3 = nr
      iper = 0
      imsg = 0
      itol = 0
      ialg = -3
      ierr = 0
      iwarn = 0

C Call the spline evaluation routine
      call r8bcspvec(iselect,np,r,z,np,f,nr,rpkg,nz,zpkg,fspl,nr,
     &     iwarn,ierr)



      end
  
