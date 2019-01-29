      subroutine pSpline(nr,nz,nCoeff,r,z,rawdata,rpkg,zpkg,fspl,
     &     nwrk,ierr)
      
      implicit none

C/*--------------------------------------------------------------------
C * pSpline.f
C *
C * wrapper to the fortran pSpline routines from NTCC
C * 
C * Modification history
C * --------------------
C * 04-June 2008 1.4.3 Created
C *
C *
C *
C *-------------------------------------------------------------------*/

      
      integer nwrk,ilinx,ilinth,ierr,i,j,k,l,m
C Input raw data
      integer nr,nz,nCoeff
      real*8 rawdata(nCoeff),r(nr),z(nz)
C The input array for the spline fit
      real*8 fspl(4,4,nr,nz)     
C Boundary Conditions
      integer ibcxmin,ibcxmax,ibcthmin,ibcthmax
      real*8 bcxmin(nz),bcxmax(nz),bcthmin(nr),bcthmax(nr)
C Work array for the spline fit
      real*8 work(nwrk)
C Return data
      real*8 coeff(nCoeff),rpkg(nr,4),zpkg(nz,4),ztol
C Package data
      integer iper,imsg,itol,ialg,iwarn

C Fill the input array
      do i=1,nr
         do j=1,nz
            fspl(1,1,i,j) = rawdata(((i-1)*nz)+j)
         enddo
      enddo

C Set the boundary conditions - Set to "not a knot"
      ibcxmin = 0
      ibcxmax = 0
      ibcthmin = 0
      ibcthmax = 0

C And call the spline fitting routine
      call r8bcspline(r,nr,z,nz,fspl,nr,ibcxmin,bcxmin,ibcxmax,
     &     bcxmax,ibcthmin,bcthmin,ibcthmax,bcthmax,work,nwrk,ilinx, 
     &     ilinth,ierr)

C Calculate r and z packages to speed up evaluation later 
      iper = 0
      imsg = 0
      itol = 0
      ialg = -3
      call r8genxpkg(nr,r,rpkg,iper,imsg,itol,ztol,ialg,ierr)
      if(ierr .gt. 0) return
      call r8genxpkg(nz,z,zpkg,iper,imsg,itol,ztol,ialg,ierr)
      if(ierr .gt. 0) return

      nr = nr * 4
      nz = nz * 4

      end
  
