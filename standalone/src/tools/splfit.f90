! Fortran module for curves approximation by non periodic splines.
! Extracted from FITpack distribution (http://www.netlib.org/dierckx).

module fitpack
contains

  !***************************************************************************
  subroutine curfit(iopt,m,x,y,w,xb,xe,k,s,nest,n,t,c,fp,&
       wrk,lwrk,iwrk,ier)

  implicit none

    !  ..scalar arguments..
      REAL(8) xb,xe,s,fp
      INTEGER iopt,m,k,nest,n,lwrk,ier
    !  ..array arguments..
      REAL(8) x(m),y(m),w(m),t(nest),c(nest),wrk(lwrk)
      INTEGER iwrk(nest)
    !  ..local scalars..
      REAL(8) tol
      INTEGER i,ia,ib,ifp,ig,iq,iz,j,k1,k2,lwest,maxit,nmin
    !  ..
    !  we set up the parameters tol and maxit
      maxit = 20
      tol = 1.0e-3 
    !  before starting computations a data check is made. if the input data
    !  are invalid, control is immediately repassed to the calling program.
      ier = 10
      if(k.le.0 .or. k.gt.5) go to 50
      k1 = k+1
      k2 = k1+1
      if(iopt.lt.(-1) .or. iopt.gt.1) go to 50
      nmin = 2*k1
      if(m.lt.k1 .or. nest.lt.nmin) go to 50
      lwest = m*k1+nest*(7+3*k)
      if(lwrk.lt.lwest) go to 50
      if(xb.gt.x(1) .or. xe.lt.x(m) .or. w(1).le.0.) go to 50
      do 10 i=2,m
         if(x(i-1).ge.x(i) .or. w(i).le.0.) go to 50
  10  continue
      if(iopt.ge.0) go to 30
      if(n.lt.nmin .or. n.gt.nest) go to 50
      j = n
      do 20 i=1,k1
         t(i) = xb
         t(j) = xe
         j = j-1
  20  continue
      call fpchec(x,m,t,n,k,ier)
      if(ier) 50,40,50
  30  if(s.lt.0.) go to 50
      if(s.eq.0. .and. nest.lt.(m+k1)) go to 50
      ier = 0
    ! we partition the working space and determine the spline approximation.
  40  ifp = 1
      iz = ifp+nest
      ia = iz+nest
      ib = ia+nest*k1
      ig = ib+nest*k2
      iq = ig+nest*k2
      call fpcurf(iopt,x,y,w,m,xb,xe,k,s,nest,tol,maxit,k1,k2,n,t,c,fp,&
       wrk(ifp),wrk(iz),wrk(ia),wrk(ib),wrk(ig),wrk(iq),iwrk,ier)
  50  return
  end subroutine curfit
!***************************************************************************

!***************************************************************************
  subroutine fpback(a,z,n,k,c,nest)
    !  subroutine fpback calculates the solution of the system of
    !  equations a*c = z with a a n x n upper triangular matrix
    !  of bandwidth k.

      implicit none

    !  ..scalar arguments..
        INTEGER n,k,nest
    !  ..array arguments..
        REAL(8) a(nest,k),z(n),c(n)
    !  ..local scalars..
        REAL(8) store
        INTEGER i,i1,j,k1,l,m
    !  ..
        k1 = k-1
        c(n) = z(n)/a(n,1)
        i = n-1
        if(i.eq.0) go to 30
        do 20 j=2,n
          store = z(i)
          i1 = k1
          if(j.le.k1) i1 = j-1
          m = i
          do 10 l=1,i1
            m = m+1
            store = store-c(m)*a(i,l+1)
    10    continue
          c(i) = store/a(i,1)
          i = i-1
    20  continue
    30  return
  end subroutine fpback
!***************************************************************************

!***************************************************************************
      subroutine fpbspl(t,n,k,x,l,h)
!  subroutine fpbspl evaluates the (k+1) non-zero b-splines of
!  degree k at t(l) <= x < t(l+1) using the stable recurrence
!  relation of de boor and cox.
!  ..

      IMPLICIT NONE

!  ..scalar arguments..
      REAL(8) x
      INTEGER n,k,l
!  ..array arguments..
      REAL(8) t(n),h(6)
!  ..local scalars..
      REAL(8) f,one
      INTEGER i,j,li,lj
!  ..local arrays..
      REAL(8) hh(5)
!  ..
      one = 1.0
      h(1) = one
      do 20 j=1,k
        do 10 i=1,j
          hh(i) = h(i)
  10    continue
        h(1) = 0.0
        do 20 i=1,j
          li = l+i
          lj = li-j
          f = hh(i)/(t(li)-t(lj))
          h(i) = h(i)+f*(t(li)-x)
          h(i+1) = f*(x-t(lj))
  20  continue
      return
      end subroutine fpbspl
!***************************************************************************

!***************************************************************************
      subroutine fpchec(x,m,t,n,k,ier)
      IMPLICIT NONE

!  ..scalar arguments..
      INTEGER m,n,k,ier
!  ..array arguments..
      REAL(8) x(m),t(n)
!  ..local scalars..
      INTEGER i,j,k1,k2,l,nk1,nk2,nk3
      REAL(8) tj,tl
!  ..
      k1 = k+1
      k2 = k1+1
      nk1 = n-k1
      nk2 = nk1+1
      ier = 10
!  check condition no 1
      if(nk1.lt.k1 .or. nk1.gt.m) go to 80
!  check condition no 2
      j = n
      do 20 i=1,k
        if(t(i).gt.t(i+1)) go to 80
        if(t(j).lt.t(j-1)) go to 80
        j = j-1
  20  continue
!  check condition no 3
      do 30 i=k2,nk2
        if(t(i).le.t(i-1)) go to 80
  30  continue
!  check condition no 4
      if(x(1).lt.t(k1) .or. x(m).gt.t(nk2)) go to 80
!  check condition no 5
      if(x(1).ge.t(k2) .or. x(m).le.t(nk1)) go to 80
      i = 1
      l = k2
      nk3 = nk1-1
      if(nk3.lt.2) go to 70
      do 60 j=2,nk3
        tj = t(j)
        l = l+1
        tl = t(l)
  40    i = i+1
        if(i.ge.m) go to 80
        if(x(i).le.tj) go to 40
        if(x(i).ge.tl) go to 80
  60  continue
  70  ier = 0
  80  return
      end subroutine fpchec
!***************************************************************************

!***************************************************************************
      subroutine fpdisc(t,n,k2,b,nest)
!  subroutine fpdisc calculates the discontinuity jumps of the kth
!  derivative of the b-splines of degree k at the knots t(k+2)..t(n-k-1)

      IMPLICIT NONE

!  ..scalar arguments..
      INTEGER n,k2,nest
!  ..array arguments..
      REAL(8) t(n),b(nest,k2)
!  ..local scalars..
      REAL(8) an,fac,prod
      INTEGER i,ik,j,jk,k,k1,l,lj,lk,lmk,lp,nk1,nrint
!  ..local array..
      REAL(8) h(12)
!  ..
      k1 = k2-1
      k = k1-1
      nk1 = n-k1
      nrint = nk1-k
      an = nrint
      fac = an/(t(nk1+1)-t(k1))
      do 40 l=k2,nk1
        lmk = l-k1
        do 10 j=1,k1
          ik = j+k1
          lj = l+j
          lk = lj-k2
          h(j) = t(l)-t(lk)
          h(ik) = t(l)-t(lj)
  10    continue
        lp = lmk
        do 30 j=1,k2
          jk = j
          prod = h(j)
          do 20 i=1,k
            jk = jk+1
            prod = prod*h(jk)*fac
  20      continue
          lk = lp+k1
          b(lmk,j) = (t(lk)-t(lp))/prod
          lp = lp+1
  30    continue
  40  continue
      return
      end subroutine fpdisc
!***************************************************************************

!***************************************************************************
    function fprati(p1,f1,p2,f2,p3,f3) result(rfprati)
!  given three points (p1,f1),(p2,f2) and (p3,f3), function fprati
!  gives the value of p such that the rational interpolating function
!  of the form r(p) = (u*p+v)/(p+w) equals zero at p.
!  ..

      IMPLICIT NONE

!  ..scalar arguments..
      REAL(8) p1,f1,p2,f2,p3,f3
!  ..local scalars..
      REAL(8) h1,h2,h3,p, rfprati
!  ..
      if(p3.gt.0.) go to 10
!  value of p in case p3 = infinity.
      p = (p1*(f1-f3)*f2-p2*(f2-f3)*f1)/((f1-f2)*f3)
      go to 20
!  value of p in case p3 ^= infinity.
  10  h1 = f1*(f2-f3)
      h2 = f2*(f3-f1)
      h3 = f3*(f1-f2)
      p = -(p1*p2*h3+p2*p3*h1+p3*p1*h2)/(p1*h1+p2*h2+p3*h3)
!  adjust the value of p1,f1,p3 and f3 such that f1 > 0 and f3 < 0.
  20  if(f2.lt.0.) go to 30
      p1 = p2
      f1 = f2
      go to 40
  30  p3 = p2
      f3 = f2
  40  rfprati = p
      return
      end function fprati
!***************************************************************************

!***************************************************************************
      subroutine fpcurf(iopt,x,y,w,m,xb,xe,k,s,nest,tol,maxit,k1,k2,&
       n,t,c,fp,fpint,z,a,b,g,q,nrdata,ier)
      !use utils
      IMPLICIT NONE

!  ..
!  ..scalar arguments..
      REAL(8) xb,xe,s,tol,fp
      INTEGER iopt,m,k,nest,maxit,k1,k2,n,ier
!  ..array arguments..
      REAL(8) x(m),y(m),w(m),t(nest),c(nest),fpint(nest),&
       z(nest),a(nest,k1),b(nest,k2),g(nest,k2),q(m,k1)
      INTEGER nrdata(nest)
!  ..local scalars..
      REAL(8) acc,con1,con4,con9,cos,half,fpart,fpms,fpold,fp0,f1,f2,f3,&
       one,p,pinv,piv,p1,p2,p3,rn,sin,store,term,wi,xi,yi
      INTEGER i,ich1,ich3,it,iter,i1,i2,i3,j,k3,l,l0,&
       mk1,new,nk1,nmax,nmin,nplus,npl1,nrint,n8
!  ..local arrays..
      REAL(8) h(7)
!  ..function references
      REAL(8) abs
      INTEGER max0,min0
!  ..subroutine references..
!fpback,fpbspl,fpgivs,fpdisc,fpknot,fprota
!  ..
!  set constants
      one = 1.0
      con1 = 0.1
      con9 = 0.9e0
      con4 = 0.4e-01
      half = 0.5
!  part 1: determination of the number of knots and their position     
!  **************************************************************      
!  determine nmin, the number of knots for polynomial approximation.
      nmin = 2*k1
      if(iopt.lt.0) go to 60
!  calculation of acc, the absolute tolerance for the root of f(p)=s.
      acc = tol*s
!  determine nmax, the number of knots for spline interpolation.
      nmax = m+k1
      if(s.gt.0.) go to 45
!  if s=0, s(x) is an interpolating spline.
!  test whether the required storage space exceeds the available one.
      n = nmax
      if(nmax.gt.nest) go to 420
!  find the position of the interior knots in case of interpolation.
  10  mk1 = m-k1
      if(mk1.eq.0) go to 60
      k3 = k/2
      i = k2
      j = k3+2
      if(k3*2.eq.k) go to 30
      do 20 l=1,mk1
        t(i) = x(j)
        i = i+1
        j = j+1
  20  continue
      go to 60
  30  do 40 l=1,mk1
        t(i) = (x(j)+x(j-1))*half
        i = i+1
        j = j+1
  40  continue
      go to 60
!  if s>0 our initial choice of knots depends on the value of iopt.
!  if iopt=0 or iopt=1 and s>=fp0, we start computing the least-squares
!  polynomial of degree k which is a spline without interior knots.
!  if iopt=1 and fp0>s we start computing the least squares spline
!  according to the set of knots found at the last call of the routine.
  45  if(iopt.eq.0) go to 50
      if(n.eq.nmin) go to 50
      fp0 = fpint(n)
      fpold = fpint(n-1)
      nplus = nrdata(n)
      if(fp0.gt.s) go to 60
  50  n = nmin
      fpold = 0.
      nplus = 0
      nrdata(1) = m-2
!  main loop for the different sets of knots. m is a save upper bound
!  for the number of trials.
  60  do 200 iter = 1,m
        if(n.eq.nmin) ier = -2
!  find nrint, tne number of knot intervals.
        nrint = n-nmin+1
!  find the position of the additional knots which are needed for
!  the b-spline representation of s(x).
        nk1 = n-k1
        i = n
        do 70 j=1,k1
          t(j) = xb
          t(i) = xe
          i = i-1
  70    continue
!  compute the b-spline coefficients of the least-squares spline
!  sinf(x). the observation matrix a is built up row by row and
!  reduced to upper triangular form by givens transformations.
!  at the same time fp=f(p=inf) is computed.
        fp = 0.
!  initialize the observation matrix a.
        do 80 i=1,nk1
          z(i) = 0.
          do 80 j=1,k1
            a(i,j) = 0.
  80    continue
        l = k1
        do 130 it=1,m
!  fetch the current data point x(it),y(it).
          xi = x(it)
          wi = w(it)
          yi = y(it)*wi
!  search for knot interval t(l) <= xi < t(l+1).
  85      if(xi.lt.t(l+1) .or. l.eq.nk1) go to 90
          l = l+1
          go to 85
!  evaluate the (k+1) non-zero b-splines at xi and store them in q.
  90      call fpbspl(t,n,k,xi,l,h)
          do 95 i=1,k1
            q(it,i) = h(i)
            h(i) = h(i)*wi
  95      continue
!  rotate the new row of the observation matrix into triangle.
          j = l-k1
          do 110 i=1,k1
            j = j+1
            piv = h(i)
            if(piv.eq.0.) go to 110
!  calculate the parameters of the givens transformation.
            call fpgivs(piv,a(j,1),cos,sin)
!  transformations to right hand side.
            call fprota(cos,sin,yi,z(j))
            if(i.eq.k1) go to 120
            i2 = 1
            i3 = i+1
            do 100 i1 = i3,k1
              i2 = i2+1
!  transformations to left hand side.
              call fprota(cos,sin,h(i1),a(j,i2))
 100        continue
 110      continue
!  add contribution of this row to the sum of squares of residual
!  right hand sides.
 120      fp = fp+yi**2
 130    continue
        if(ier.eq.(-2)) fp0 = fp
        fpint(n) = fp0
        fpint(n-1) = fpold
        nrdata(n) = nplus
!  backward substitution to obtain the b-spline coefficients.
        call fpback(a,z,nk1,k1,c,nest)
!  test whether the approximation sinf(x) is an acceptable solution.
        if(iopt.lt.0) go to 440
        fpms = fp-s
        if(abs(fpms).lt.acc) go to 440
!  if f(p=inf) < s accept the choice of knots.
        if(fpms.lt.0.) go to 250
!  if n = nmax, sinf(x) is an interpolating spline.
        if(n.eq.nmax) go to 430
!  increase the number of knots.
!  if n=nest we cannot increase the number of knots because of
!  the storage capacity limitation.
        if(n.eq.nest) go to 420
!  determine the number of knots nplus we are going to add.
        if(ier.eq.0) go to 140
        nplus = 1
        ier = 0
        go to 150
 140    npl1 = nplus*2
        rn = nplus
        if(fpold-fp.gt.acc) npl1 = rn*fpms/(fpold-fp)
        nplus = min0(nplus*2,max0(npl1,nplus/2,1))
 150    fpold = fp
!  compute the sum((w(i)*(y(i)-s(x(i))))**2) for each knot interval
!  t(j+k) <= x(i) <= t(j+k+1) and store it in fpint(j),j=1,2,...nrint.
        fpart = 0.
        i = 1
        l = k2
        new = 0
        do 180 it=1,m
          if(x(it).lt.t(l) .or. l.gt.nk1) go to 160
          new = 1
          l = l+1
 160      term = 0.
          l0 = l-k2
          do 170 j=1,k1
            l0 = l0+1
            term = term+c(l0)*q(it,j)
 170      continue
          term = (w(it)*(term-y(it)))**2
          fpart = fpart+term
          if(new.eq.0) go to 180
          store = term*half
          fpint(i) = fpart-store
          i = i+1
          fpart = store
          new = 0
 180    continue
        fpint(nrint) = fpart
        do 190 l=1,nplus
!  add a new knot.
          call fpknot(x,m,t,n,fpint,nrdata,nrint,nest,1)
!  if n=nmax we locate the knots as for interpolation.
          if(n.eq.nmax) go to 10
!  test whether we cannot further increase the number of knots.
          if(n.eq.nest) go to 200
 190    continue
!  restart the computations with the new set of knots.
 200  continue
!  test whether the least-squares kth degree polynomial is a solution
!  of our approximation problem.
 250  if(ier.eq.(-2)) go to 440
!  part 2: determination of the smoothing spline sp(x).                
!  ***************************************************                 
!  evaluate the discontinuity jump of the kth derivative of the
!  b-splines at the knots t(l),l=k+2,...n-k-1 and store in b.
      call fpdisc(t,n,k2,b,nest)
!  initial value for p.
      p1 = 0.
      f1 = fp0-s
      p3 = -one
      f3 = fpms
      p = 0.
      do 255 i=1,nk1
         p = p+a(i,1)
 255  continue
      rn = nk1
      p = rn/p
      ich1 = 0
      ich3 = 0
      n8 = n-nmin
!  iteration process to find the root of f(p) = s.
      do 360 iter=1,maxit
!  the rows of matrix b with weight 1/p are rotated into the
!  triangularised observation matrix a which is stored in g.
        pinv = one/p
        do 260 i=1,nk1
          c(i) = z(i)
          g(i,k2) = 0.
          do 260 j=1,k1
            g(i,j) = a(i,j)
 260    continue
        do 300 it=1,n8
!  the row of matrix b is rotated into triangle by givens transformation
          do 270 i=1,k2
            h(i) = b(it,i)*pinv
 270      continue
          yi = 0.
          do 290 j=it,nk1
            piv = h(1)
!  calculate the parameters of the givens transformation.
            call fpgivs(piv,g(j,1),cos,sin)
!  transformations to right hand side.
            call fprota(cos,sin,yi,c(j))
            if(j.eq.nk1) go to 300
            i2 = k1
            if(j.gt.n8) i2 = nk1-j
            do 280 i=1,i2
!  transformations to left hand side.
              i1 = i+1
              call fprota(cos,sin,h(i1),g(j,i1))
              h(i) = h(i1)
 280        continue
            h(i2+1) = 0.
 290      continue
 300    continue
!  backward substitution to obtain the b-spline coefficients.
        call fpback(g,c,nk1,k2,c,nest)
!  computation of f(p).
        fp = 0.
        l = k2
        do 330 it=1,m
          if(x(it).lt.t(l) .or. l.gt.nk1) go to 310
          l = l+1
 310      l0 = l-k2
          term = 0.
          do 320 j=1,k1
            l0 = l0+1
            term = term+c(l0)*q(it,j)
 320      continue
          fp = fp+(w(it)*(term-y(it)))**2
 330    continue
!  test whether the approximation sp(x) is an acceptable solution.
        fpms = fp-s
        if(abs(fpms).lt.acc) go to 440
!  test whether the maximal number of iterations is reached.
        if(iter.eq.maxit) go to 400
!  carry out one more step of the iteration process.
        p2 = p
        f2 = fpms
        if(ich3.ne.0) go to 340
        if((f2-f3).gt.acc) go to 335
!  our initial choice of p is too large.
        p3 = p2
        f3 = f2
        p = p*con4
        if(p.le.p1) p=p1*con9 + p2*con1
        go to 360
 335    if(f2.lt.0.) ich3=1
 340    if(ich1.ne.0) go to 350
        if((f1-f2).gt.acc) go to 345
!  our initial choice of p is too small
        p1 = p2
        f1 = f2
        p = p/con4
        if(p3.lt.0.) go to 360
        if(p.ge.p3) p = p2*con1 + p3*con9
        go to 360
 345    if(f2.gt.0.) ich1=1
!  test whether the iteration process proceeds as theoretically
!  expected.
 350    if(f2.ge.f1 .or. f2.le.f3) go to 410
!  find the new value for p.
        p = fprati(p1,f1,p2,f2,p3,f3)
 360  continue
!  error codes and messages.
 400  ier = 3
      go to 440
 410  ier = 2
      go to 440
 420  ier = 1
      go to 440
 430  ier = -1
 440  return
      end subroutine fpcurf
!***************************************************************************

!***************************************************************************
      subroutine fpgivs(piv,ww,cos,sin)
!  subroutine fpgivs calculates the parameters of a givens
!  transformation .
!  ..

      IMPLICIT NONE


!  ..scalar arguments..
      REAL(8) piv,ww,cos,sin
!  ..local scalars..
      REAL(8) dd,one,store
!  ..function references..
!  ..
      one = 1.0
      store = abs(piv)
      if(store.ge.ww) dd = store*sqrt(one+(ww/piv)**2)
      if(store.lt.ww) dd = ww*sqrt(one+(piv/ww)**2)
      cos = ww/dd
      sin = piv/dd
      ww = dd
      return
      end
!***************************************************************************

!***************************************************************************
      subroutine fpknot(x,m,t,n,fpint,nrdata,nrint,nest,istart)
!  subroutine fpknot locates an additional knot for a spline of degree
!  k and adjusts the corresponding parameters,i.e.
!t     : the position of the knots.
!n     : the number of knots.
!nrint : the number of knotintervals.
!fpint : the sum of squares of residual right hand sides
!    for each knot interval.
!nrdata: the number of data points inside each knot interval.
!  istart indicates that the smallest data point at which the new knot
!  may be added is x(istart+1)
!  ..

      IMPLICIT NONE

!  ..scalar arguments..
      INTEGER m,n,nrint,nest,istart
!  ..array arguments..
      REAL(8) x(m),t(nest),fpint(nest)
      INTEGER nrdata(nest)
!  ..local scalars..
      REAL(8) an,am,fpmax
      INTEGER ihalf,j,jbegin,jj,jk,jpoint,k,maxbeg,maxpt,&
       next,nrx,number
!  ..
      k = (n-nrint-1)/2
!  search for knot interval t(number+k) <= x <= t(number+k+1) where
!  fpint(number) is maximal on the condition that nrdata(number)
!  not equals zero.
      fpmax = 0.0
      jbegin = istart
      do 20 j=1,nrint
        jpoint = nrdata(j)
        if(fpmax.ge.fpint(j) .or. jpoint.eq.0) go to 10
        fpmax = fpint(j)
        number = j
        maxpt = jpoint
        maxbeg = jbegin
  10    jbegin = jbegin+jpoint+1
  20  continue
!  let coincide the new knot t(number+k+1) with a data point x(nrx)
!  inside the old knot interval t(number+k) <= x <= t(number+k+1).
      ihalf = maxpt/2+1
      nrx = maxbeg+ihalf
      next = number+1
      if(next.gt.nrint) go to 40
!  adjust the different parameters.
      do 30 j=next,nrint
        jj = next+nrint-j
        fpint(jj+1) = fpint(jj)
        nrdata(jj+1) = nrdata(jj)
        jk = jj+k
        t(jk+1) = t(jk)
  30  continue
  40  nrdata(number) = ihalf-1
      nrdata(next) = maxpt-ihalf
      am = maxpt
      an = nrdata(number)
      fpint(number) = fpmax*an/am
      an = nrdata(next)
      fpint(next) = fpmax*an/am
      jk = next+k
      t(jk) = x(nrx)
      n = n+1
      nrint = nrint+1
      return
      end subroutine fpknot
!***************************************************************************

!***************************************************************************
      subroutine fprota(cos,sin,a,b)
!  subroutine fprota applies a givens rotation to a and b.
!  ..

      IMPLICIT NONE

!  ..scalar arguments..
      REAL(8) cos,sin,a,b
! ..local scalars..
      REAL(8) stor1,stor2
!  ..
      stor1 = a
      stor2 = b
      b = cos*stor2+sin*stor1
      a = cos*stor1-sin*stor2
      return
      end subroutine fprota
!***************************************************************************

!***************************************************************************
      subroutine splev(t,n,c,k,x,y,m,ier)

      IMPLICIT NONE


!  subroutine splev evaluates in a number of points x(i),i=1,2,...,m
!  a spline s(x) of degree k, given in its b-spline representation.
!
!  calling sequence:
! call splev(t,n,c,k,x,y,m,ier)
!
!  input parameters:
!t    : array,length n, which contains the position of the knots.
!n    : integer, giving the total number of knots of s(x).
!c    : array,length n, which contains the b-spline coefficients.
!k    : integer, giving the degree of s(x).
!x    : array,length m, which contains the points where s(x) must
!   be evaluated.
!m    : integer, giving the number of points where s(x) must be
!   evaluated.
!
!  output parameter:
!y    : array,length m, giving the value of s(x) at the different
!   points.
!ier  : error flag
!  ier = 0 : normal return
!  ier =10 : invalid input data (see restrictions)
!
!  restrictions:
!m >= 1
!t(k+1) <= x(i) <= x(i+1) <= t(n-k) , i=1,2,...,m-1.
!
!  other subroutines required: fpbspl.
!
!  references :
!de boor c  : on calculating with b-splines, j. approximation theory
!         6 (1972) 50-62.
!cox m.g.   : the numerical evaluation of b-splines, j. inst. maths
!         applics 10 (1972) 134-149.
!dierckx p. : curve and surface fitting with splines, monographs on
!         numerical analysis, oxford university press, 1993.
!
!  author :
!p.dierckx
!dept. computer science, k.u.leuven
!celestijnenlaan 200a, b-3001 heverlee, belgium.
!e-mail : Paul.Dierckx@cs.kuleuven.ac.be
!
!  latest update : march 1987
!
!  ..scalar arguments..
      INTEGER n,k,m,ier
!  ..array arguments..
      REAL(8) t(n),c(n),x(m),y(m)
!  ..local scalars..
      INTEGER i,j,k1,l,ll,l1,nk1
      REAL(8) arg,sp,tb,te
!  ..local array..
      REAL(8) h(6)
!  ..
!  before starting computations a data check is made. if the input data
!  are invalid control is immediately repassed to the calling program.
      ier = 10
      if(m-1) 100,30,10
  10  do 20 i=2,m
        if(x(i).lt.x(i-1)) then
           ier = 101
           go to 100
        endif
  20  continue
  30  ier = 0
!  fetch tb and te, the boundaries of the approximation interval.
      k1 = k+1
      nk1 = n-k1
      tb = t(k1)
      te = t(nk1+1)
      l = k1
      l1 = l+1
!  main loop for the different points.
      do 80 i=1,m
!  fetch a new x-value arg.
        arg = x(i)
        if(arg.lt.tb) arg = tb
        if(arg.gt.te) arg = te
!  search for knot interval t(l) <= arg < t(l+1)
  40    if(arg.lt.t(l1) .or. l.eq.nk1) go to 50
        l = l1
        l1 = l+1
        go to 40
!  evaluate the non-zero b-splines at arg.
  50    call fpbspl(t,n,k,arg,l,h)
!  find the value of s(x) at x=arg.
        sp = 0.
        ll = l-k1
        do 60 j=1,k1
          ll = ll+1
          sp = sp+c(ll)*h(j)
  60    continue
        y(i) = sp
  80  continue
 100  return
      end subroutine splev
!***************************************************************************

end module fitpack

!*****************************************************************************************
module splfit
  use fitpack
contains

  ! given the set of data points (x(i),y(i)) and the set of positive
  ! numbers w(i), subroutine splrep determines a smooth spline
  ! approximation of degree k on the interval xb <= x <= xe.
  subroutine splrep(x, y, w, t, xb, xe, k, c)

    implicit none

    real(8), dimension(:), intent(in) :: x
    real(8), dimension(:), intent(in) :: y
    real(8), dimension(:), intent(in) :: w
    real(8), dimension(:), intent(in) :: t

    real(8), optional, intent(inout)  :: xb
    real(8), optional, intent(inout)  :: xe
    integer, optional,intent(inout)   :: k

    real(8), dimension(:), intent(out):: c

    real(8), dimension(:), allocatable:: wrk
    integer,  dimension(:), allocatable:: iwrk

    integer  :: iopt, m, nest, n, lwrk, ier
    real(8) :: s, fp
    
    m = size(x, 1)  
    if (size(y) /= m) then
      write(*,*) "ERROR: x and y should have the same size."
      stop
    end if

    ! weights 
    if (size(w) /= m) then
        write(*,*) "ERROR: w should have the same size than x and y."
      stop
    end if

    ! Endpoints 
    if (.not.present(xb)) then
      xb = x(1)
    end if
    if (.not.present(xe)) then
      xe = x(m)
    end if
    
    ! Spline degree
    if (present(k)) then
      ! For Fitpack call, check if 1<=k<=5
      if (k<1 .or. k>5) then
        write(*,*) "ERROR: the spline degree should be between 1 and 5."
        stop
      end if
    else
      ! Cubic spline by default
      k = 3
    end if

    ! Knots
    iopt = -1
    nest = size(t, 1)
    lwrk =  m*(k + 1) + nest*(7 + 3*k)

    allocate(wrk(lwrk))
    allocate(iwrk(nest))
   
    ! Approximation: s should be not nil
    s = m - sqrt(2.0*m)

    call curfit(iopt, m, x, y, w, xb, xe, k, s, nest, n, t, c, fp, wrk, lwrk, iwrk, ier)
    
    print*, 'n  = ', n
    print*, 'fp = ', fp
    
    deallocate(iwrk) 
    deallocate(wrk) 
    
    print *,"return from splrep"

  end subroutine splrep

end module splfit
