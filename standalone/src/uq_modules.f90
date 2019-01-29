!> @brief 
!> Modules for the implemention of the Bayesian UQ using 
!  Poylynomial Chaos Expansion method 
!> 
! .........................................................................

! .........................................................................
module mod_numrec

  implicit none
  
  contains
 
  ! This routine returns arrays x[0..n-1] and w[0..n-1] containing the abscissas and weights of
  ! the n-point Gauss-Hermite quadrature formula. The largest abscissa is returned in x[0], the
  ! most negative in x[n-1].
  subroutine gauher(x, w, n)
    integer n
    double precision, dimension(n) :: w,x
    double precision, parameter    :: eps   = 3.d-14,            &
                                      pim4  = .7511255444649425d0
    integer, parameter             :: maxit = 10
    
    double precision p1, p2, p3, pp, z, z1
    integer i, its, j, m
    
    m = (n+1)/2
    do i=1,m
      ! ...
      if(i.eq.1) then
        z = sqrt(float(2*n+1))-1.85575*(2*n+1)**(-.16667)
      else if(i.eq.2)then
        z = z-1.14d0*n**.426d0/z
      else if (i.eq.3)then
        z = 1.86d0*z-.86d0*x(1)
      else if (i.eq.4)then
        z = 1.91d0*z-.91d0*x(2)
      else
        z = 2.*z-x(i-2)
      endif
      ! ...
      
      ! ...    
      do its = 1, maxit
        p1 = PIM4
        p2 = 0.d0
        
        do j = 1,n
          p3 = p2
          p2 = p1
          p1 = z*sqrt(2.d0/j)*p2-sqrt(dble(j-1)/dble(j))*p3
        enddo
        
        pp = sqrt(2.d0*n)*p2
        z1 = z
        z  = z1-p1/pp
        if (abs(z-z1).le.eps) exit
      enddo
      
      if (its.eq.maxit+1) then
        write(6,*) '>>> too many iterations in gauher.'
        stop
      endif
      ! ...
      
      ! ...
      x(i)     = z
      x(n+1-i) = -z
      w(i) = 2.d0/(pp*pp)
      w(n+1-i) = w(i)
      ! ...
    enddo
    
    return
  end subroutine gauher
  ! ...

  ! ...
  subroutine gauherprob(x, w, n)
    double precision, parameter  :: pi=3.141592653589793238462643d0
    integer n
    double precision, dimension(n) :: w,x

    call gauher(x,w,n)
    w = w/sqrt(pi)
    x = sqrt(2.d0)*x

    return
  end subroutine gauherprob
  ! ...

  ! ...
  subroutine polher(x, hpol, np)
    integer np
    double precision                  :: x
    double precision, dimension(np)   :: hpol

    double precision, dimension(np+2) :: hhelp
    double precision, parameter       :: pim4 = .7511255444649425d0
    double precision                  :: a1, a2

    integer i, j

    hhelp(1) = 0.d0
    hhelp(2) = 1

    do i = 3, np+1
      j = i-3 
      !a1=2.d0
      a1 = 1.d0
      !a2=2.d0*j
      a2 = j
      hhelp(i )= x*a1*hhelp(i-1) - a2*hhelp(i-2)
    enddo

    do i = 1, np
      hpol(i) = hhelp(i+1)
    enddo
  end subroutine polher
  ! ...

  ! ....
  subroutine polnorm64(hnorm64,np)
    integer np

    integer(kind=selected_int_kind(16)), dimension(np) :: hnorm64
    integer(kind=selected_int_kind(16)) :: i64

    hnorm64(1) = 1
    do i64 = 2, np
      hnorm64(i64) = hnorm64(i64-1)*(i64-1)
    enddo
  end subroutine polnorm64
  ! ...

  ! ...
  subroutine pictures(ig, gstr)
    integer    :: ig
    character*4  gstr

    integer    :: igsub, ig1, ig2, ig3, ig4
    character*10 number

    number = '0123456789'

    if (ig.ge.10000) then
      write(6,*)'>>> gif-Ausgabe erweitern.'
      stop
    endif

    !      Basteln der vierstelligen Ausgabezahl
    ig4   = ig/1000
    igsub = ig4*1000
    ig3   = (ig - igsub)/100
    igsub = igsub + ig3*100
    ig2   = (ig-igsub)/10
    igsub = igsub + ig2*10
    ig1   = ig - igsub

    gstr = number(ig4+1:ig4+1) // number(ig3+1:ig3+1) &
       // number(ig2+1:ig2+1) // number(ig1+1:ig1+1)
  end subroutine pictures
  ! ....

end module mod_numrec
! .........................................................................

! .........................................................................
module mod_index

  use iso_fortran_env
  use mod_numrec

  implicit none

  contains
  
  ! ...
  subroutine number_of_terms(nfterme,npterme,np,mmax,jterme)
    integer, dimension(0:np)     :: jterme

    integer nfterme,npterme,np,mmax
    integer i,j,isum

    ! (N) Dimension: mmax (Zahl der Zufallsvariablen)
    ! (P) Polynomordnung: np

    jterme = 1
    do j = 1, np
      isum = 1
      do i = max(mmax,j)+1, j+mmax
        jterme(j) = jterme(j)*i
        isum = isum*(i-max(mmax,j))
      enddo

      jterme(j) = jterme(j)/isum
    enddo

    nfterme = jterme(np)
    npterme = jterme(np-1)

!    write(6,*) '>>> Insgesamt #Funktionsterme: ', nfterme
!    write(6,*) '>>> Insgesamt #Hermiteterme: ', npterme
    
    do j = np, 0, -1
      if (j.ne.0) then
        jterme(j) = jterme(j) - jterme(j-1)
      else
        jterme(0) = 1
      endif
    enddo
  
  end subroutine number_of_terms
  ! ...

  ! ...
  subroutine index_gen_nall(npterme, np, mmax, nloc, ibeg, ifunc, ihe)
    
    integer :: npterme, np, mmax, nloc, ibeg
    integer, dimension(:,:)         :: ifunc,ihe
    integer :: i,j,k,l,m,jtest,isum,iflag,ihelp
    
    ifunc(1,:)=1
    do j=0,nloc-1
      jtest=j+ibeg
      do k=mmax,1,-1
        ihelp=jtest/((np+1)**(k-1))
        jtest=jtest-ihelp*((np+1)**(k-1))
        ifunc(j+1,k)=ihelp+1
      enddo
    enddo
    
    l=2
    do i=1,np-1
      do j=0,(i+1)**mmax-1         !  0, 6**2=35
        jtest=j
        isum=0
        do k=mmax,1,-1
          ihelp=jtest/((i+1)**(k-1))
          jtest=jtest-ihelp*((i+1)**(k-1))
          isum=isum+ihelp
          if (isum.gt.i) exit
          ihe(l,k)=ihelp
        enddo
        if (isum.eq.i) then
          l=l+1
          if (l.gt.npterme) exit
        endif
      enddo
    enddo
    
  end subroutine index_gen_nall
  ! ...

  ! ...
  ! TODO: use local
  subroutine index_gen_ihe64(npterme, np, mmax, ihe)
    integer :: npterme, np, mmax
    integer, dimension(:,:) :: ihe
    integer :: l
    integer(int64)  :: i64, j64, k64, ihelp64, np64, mmax64, jtest64, isum64

    np64   = np
    mmax64 = mmax

    l = 2
    do i64 = 1, np64-1
      do j64 = 0, (i64+1)**mmax64-1         !  0, 6**2=35
        jtest64 = j64
        isum64  = 0
        do k64 = mmax64, 1, -1
          ihelp64 = jtest64/((i64+1)**(k64-1))
          jtest64 = jtest64 - ihelp64*((i64+1)**(k64-1))
          isum64  =  isum64 + ihelp64
          if (isum64.gt.i64) exit
          ihe(l,k64) = ihelp64
        enddo

        if (isum64.eq.i64) then
          l = l+1
          if (l.gt.npterme) exit
        endif
      enddo
    enddo

  end subroutine index_gen_ihe64
  ! ...

end module mod_index
! .........................................................................
