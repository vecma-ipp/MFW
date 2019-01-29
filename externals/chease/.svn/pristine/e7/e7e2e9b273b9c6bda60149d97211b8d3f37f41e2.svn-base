MODULE prec_rkind
  !
  !   Precision for real and complex
  !
  INTEGER, PARAMETER :: RKIND = SELECTED_REAL_KIND(15,300)
  INTEGER, PARAMETER :: CKIND = RKIND
  INTEGER, PARAMETER :: ITM_I4 = SELECTED_INT_KIND (9)        ! Integer*4
  INTEGER, PARAMETER :: ITM_I8 = SELECTED_INT_KIND (18)       ! Integer*8
  !
END MODULE prec_rkind
MODULE interpos_module
  !
  ! Examples of calls to this generic routine:
  !
  ! call interpos(xin,yin,nin,nout,xout=xout,yout=yout)
  ! call interpos(xin,yin,nin,nout,tension,xout,yout)
  ! call interpos(xin,yin,nin,nout,tension,xout,yout,youtp)
  ! call interpos(xin,yin,nin,nout,tension,xout=xout,yout=yout,nbc=bc_type,ybc=bc_values)
  ! call interpos(xin,yin,nin,nout,tension,xout,yout,nbc=bc_type,ybc=bc_values,option=32)
  !
  ! The full possibilities can be expressed as follows:
  !
  ! call interpos(xin,yin[[,yinnew],yinpp,]nin[,nout,tension],xout,yout,[youtp,youtpp,youtint,nbc,ybc,sigma,option,info])
  ! with nout optional if equals one (xout, yout, etc are scalars)
  !
  ! These are grouped into 3 main routines:
  !
  ! main:
  ! call interpos(xin,yin,nin,[xout,yout,nout,tension,youtp,youtpp,youtint,nbc,ybc,sigma,option])
  ! These two to compute 1st yinpp and then to just evaluate for different xout values with second calls
  ! call interpos(xin,yin,yinnew,yinpp,nin,[tension,nbc,ybc,sigma])                 ! only computes yinpp
  ! call interpos(xin,yinnew,yinpp,nin,nout,xout,yout,youtp,youtpp,youtint,option])    ! only computes yout's
  !
  ! Each of these allow for scalar xout, yout, etc which means that nout is also optional in this case
  !
  !
  implicit none
  public :: interpos
  interface interpos
     module procedure interpos_def, interpos_defper, interpos_yinpp, interpos_interp &
       & ,interpos_defxscal, interpos_defperxscal, interpos_interpxscal
  end interface

contains

  SUBROUTINE interpos_def(XIN,YIN,NIN,NOUT,tension,xout,yout,youtp,youtpp,youtint,nbc,ybc,sigma,option,info)
    !
    USE prec_rkind
    implicit none
    !
    INTERFACE
       SUBROUTINE INTLINEAR(PXIN,PYIN,KNIN,PXOUT,PYOUT,PYOUTP,PYOUTPP,PYOUTINT,KNOUT,KOPTDER,KEXTRAPO)
         USE PREC_RKIND
         IMPLICIT NONE
         ! arguments
         INTEGER :: KNIN, KNOUT, KOPTDER, KEXTRAPO
         REAL(RKIND) :: PXIN(KNIN), PYIN(KNIN), PXOUT(KNOUT)
         REAL(RKIND) :: PYOUT(KNOUT), PYOUTP(KNOUT), PYOUTPP(KNOUT), PYOUTINT(KNOUT)
       END SUBROUTINE INTLINEAR
       SUBROUTINE INTQUADRATIC(PXIN,PYIN,KNIN,PXOUT,PYOUT,PYOUTP,PYOUTPP,PYOUTINT,KNOUT,KOPTDER,KOPTXPOL,NBC)
         USE PREC_RKIND
         IMPLICIT NONE
         ! arguments
         INTEGER :: KNIN, KNOUT, KOPTDER, KOPTXPOL
         INTEGER, OPTIONAL ::  NBC
         REAL(RKIND) :: PXIN(KNIN), PYIN(KNIN), PXOUT(KNOUT)
         REAL(RKIND):: PYOUT(KNOUT), PYOUTP(KNOUT), PYOUTPP(KNOUT), PYOUTINT(KNOUT)
         !
       END SUBROUTINE INTQUADRATIC
       SUBROUTINE CBSPLGEN(PXIN,PYIN,PYINNEW,PYINPP,KNIN,PXOUT,PYOUT, &
            &  PYOUTP,PYOUTPP,PYOUTINT,KNOUT,KOPT,PSIG,NBCLFT &
            &  ,NBCRGT,XBCLFT,XBCRGT,YBCLFT,YBCRGT,KEXTRPOL,PXEXP0,PXEXPDL,KFLAG)
         USE prec_rkind
         implicit none
         REAL(RKIND) :: EPSILON
         PARAMETER(EPSILON = 1.0E-10_RKIND)
         ! arguments
         integer :: knin, knout, kopt, nbclft, nbcrgt, kextrpol
         integer:: kflag
         real(rkind) :: PXIN(KNIN), PYIN(KNIN), PXOUT(KNOUT), PSIG(KNIN), &
              & xbclft, xbcrgt, ybclft, ybcrgt, pxexp0, pxexpdl
         real(rkind):: PYINNEW(KNIN), PYINPP(KNIN), PYOUT(KNOUT), PYOUTP(KNOUT), &
              & PYOUTPP(KNOUT), PYOUTINT(KNOUT)
       END SUBROUTINE CBSPLGEN
    END INTERFACE
    !
    !
    INTEGER :: NIN
    INTEGER,optional :: NOUT
    REAL(RKIND) ::  XIN(NIN), YIN(NIN)
    REAL(RKIND), optional ::  tension
    REAL(RKIND), optional ::  xout(:)
    REAL(RKIND), optional ::  yout(:), youtp(:), youtpp(:), youtint(:)
    REAL(RKIND), optional ::  ybc(:), sigma(:)
    INTEGER, optional :: nbc(2), option
    INTEGER, optional :: info
    ! need variables computed even if not given as argument
    REAL(RKIND), allocatable ::  xout2(:), yout2(:), youtp2(:), youtpp2(:), youtint2(:), xin_eff(:), yin_eff(:)
    REAL(RKIND) ::  sigma2(NIN), zdx
    integer :: info2, nout2, nin_eff, i, j, nverbose=1
    !
    INTEGER ioptder, NBCLFT,NBCRGT, iextrapo, inttype, optabs
    REAL(RKIND) :: XBCLFT,XBCRGT, YBCLFT,YBCRGT
    REAL(RKIND) ::  ynew(NIN), yinpp(NIN), sigmamin
    REAL(RKIND) :: PXEXP0,PXEXPDL, zz
    !
    info2 = -1
    !
    ! 1. Deals with various optional arguments
    !
    ! check that there are not 2 points too close  in rho: now 1e-5 from average dx
    zdx=(xin(nin)-xin(1)) / real(nin,rkind)
    nin_eff = nin
    allocate(xin_eff(nin_eff))
    allocate(yin_eff(nin_eff))
    i=1
    xin_eff(1)=xin(1)
    yin_eff(1)=yin(1)
    do j=2,NIN_eff
      if (abs(xin(j)-xin_eff(i)) .gt. 1e-5_rkind*zdx) then
        i=i+1;
        xin_eff(i) = xin(j)
        yin_eff(i) = yin(j)
      end if
    end do
    nin_eff = i;
    if (nverbose .ge. 3) then
      if (nin_eff .ne. nin) write(0,*) 'There were multiple input x points, changed nin=',nin, &
           & ' to nin_eff=',nin_eff,' in interpos_def to avoid this'
    end if
    !
    ! xout
    if (present(xout)) then
      if (present(nout)) then
        nout2=nout
      else
        ! print *,'% nout not present, use size(xout) instead'
        nout2 = size(xout)
      end if
      allocate(xout2(nout2))
      xout2 = xout(1:nout2)
    else
      nout2 = nin
!      if (present(nout)) nout = nin
      allocate(xout2(nout2))
      xout2 = xin_eff(1:nout2)
    end if
    allocate(yout2(nout2))
    allocate(youtp2(nout2))
    allocate(youtpp2(nout2))
    allocate(youtint2(nout2))
    ! sigma
    if (present(sigma)) then
      ! normalize to minimum value, so that taus=tension at this position
      sigmamin = minval(sigma)
      if (sigmamin .le. 0._rkind) then
        print *,' ERROR: min(sigma)=',sigmamin,' is .le. 0.'
        if (present(info)) info = 1
        return
      end if
      sigma2 = sigma/sigmamin
    else
      sigma2 = 1._rkind
    end if
    ! tension
    if (present(tension)) then
       if (tension .ge. 0._rkind) then
          sigma2=tension*sigma2
       else
          ! tension=-1 or negative, uses default value |tension| * mean(Delta_x)**3
         ! zz=minval(xin_eff(2:nin_eff)-xin_eff(1:nin_eff-1)) (minval not good if there is one very small dx like rho mesh from (R,Z))
          zz=sum(xin_eff(2:nin_eff)-xin_eff(1:nin_eff-1)) / real(nin_eff-1,rkind)
          ! tension=abs(tension)*zz**3 ! if intent(inout) but does not allow call: tension=-1.
          sigma2=abs(tension)*zz**3*sigma2
       end if
    else
       ! zz=minval(xin_eff(2:nin_eff)-xin_eff(1:nin_eff-1))
       zz=sum(xin_eff(2:nin_eff)-xin_eff(1:nin_eff-1)) / real(nin_eff-1,rkind)
       sigma2=zz**3*sigma2
    end if
    !
    ! boundary conditions (if nbc given, ybc should also be given)
    if (present(nbc)) then
      if (present(ybc)) then
        NBCLFT = nbc(1)
        NBCRGT = nbc(2)
        YBCLFT = ybc(1)
        YBCRGT = ybc(2)
        if (size(ybc) .GE. 4) then
          if (NBCLFT .ge. 10) XBCLFT = ybc(3)
          if (NBCRGT .ge. 10) XBCRGT = ybc(4)
          if (size(ybc) .GE. 6) then
            PXEXP0 = ybc(5)
            PXEXPDL= ybc(6)
          end if
        end if
      else
        print *,' problems in interpos, nbc is given but not ybc'
        if (present(info)) info = 2
        return
      end if
    else
      ! default is 2nd derivative=0 at each end
      ! For quadratic interpolation, default is NBCLFT=0 is used for quadratic defined with points and discontinuous 1st derivative
      NBCLFT = 0
      NBCRGT = 0
      YBCLFT = 0._rkind
      YBCRGT = 0._rkind
      ! not used but sets value to make sure:
      XBCLFT = xin_eff(1)
      XBCRGT = xin_eff(nin_eff)
      PXEXP0 = xin_eff(1)-xin_eff(nin_eff)
      PXEXPDL= 1._rkind
    end if
    ! compute y (0), also yp (1), also ypp (2) or also yint(3)
    ioptder = 0
    if (present(youtp)) ioptder=1
    if (present(youtpp)) ioptder=2
    if (present(youtint)) ioptder=3
    ! option: interpolation and extrapolation choice
    if (.NOT. present(option)) then
      inttype=3 ! cubic
      iextrapo = 32 !extrapolation cubic on 1st dx outside and then quadratic
    else
      optabs = abs(option)
      select case (optabs)
      case(1, 11, 21)
        ! linear interpolation
        inttype = 1
        ! extrapolation
        if (optabs .eq. 1)  iextrapo = 0 ! no extrapolation
        if (optabs .eq. 11) iextrapo = sign(1,option) ! linear extrapolation with edge-deriv for 11; edge-val for -11
        if (optabs .eq. 21) iextrapo = sign(10,option) ! constant outside: y=yedge for option=21, y=0. for -21
      case(2,12,22,32,42)
        ! quadratic interpolation
        inttype = 2
        ! extrapolation
        if (optabs .eq. 2)  iextrapo = 0 ! no extrapolation
        if (optabs .eq. 12) iextrapo = sign(21,option) ! quadratic on dx then linear extrapol. with edge-deriv for 12; edge-val for -12
        if (optabs .eq. 22) iextrapo = sign(1,option) ! linear extrapol. with edge-deriv for 22; edge-val for -22
        if (optabs .eq. 32) iextrapo = sign(2,option) ! quadratic extrapol. with edge-deriv for 32; edge-val for -32
        if (optabs .eq. 42) iextrapo = sign(10,option) ! constant outside: y=yedge for option=42, y=0. for -42
      case(3,13,23,33,43,53,63)
        ! cubic interpolation
        inttype = 3
        ! extrapolation
        if (optabs .eq. 3)  iextrapo = 0 ! no extrapolation
        if (optabs .eq. 13) iextrapo = sign(32,option) ! cubic on dx then quadratic extrapol. with edge-deriv for 13; edge-val for -13
        if (optabs .eq. 23) iextrapo = sign(1,option) ! linear extrapol. with edge-deriv for 22; edge-val for -22
        if (optabs .eq. 33) iextrapo = sign(2,option) ! quadratic extrapol. with edge-deriv for 33; edge-val for -33
        if (optabs .eq. 43) iextrapo = sign(3,option) ! cubic extrapol. with edge-deriv for 43; edge-val for -43
        if (optabs .eq. 53) iextrapo = sign(31,option) ! cubic on dx then linear extrapol. with edge-deriv for 53; edge-val for -53
        if (optabs .eq. 63) iextrapo = sign(10,option) ! constant outside: y=yedge for option=63, y=0. for -63
      case default
        inttype=mod(optabs,10)
        select case (inttype)
        case (1)
          iextrapo = 1
        case (2)
          iextrapo = 21
        case (3)
          iextrapo = 32
        case default
          print *,'option= ',option, &
            & ' is not valid. The last digit should be 1, 2 or 3 for linear, quadratic or cubic interpolation'
          if (present(info)) info = 3
          return
        end select
      end select
    end if
    !
    ! 2. call relevant interpolation routine
    !
    select case (inttype)
    case (1)
      ! linear
      call intlinear(XIN_EFF,YIN_EFF,NIN_EFF,xout2,yout2,youtp2,youtpp2,youtint2,NOUT2,ioptder,iextrapo)
    case (2)
      ! quadratic
      call intquadratic(XIN_EFF,YIN_EFF,NIN_EFF,xout2,yout2,youtp2,youtpp2,youtint2,NOUT2,ioptder,iextrapo,NBCLFT)
    case (3)
      ! cubic
      ! general cubic spline routine with non-periodic boundary conditions

      CALL CBSPLGEN(XIN_EFF,YIN_EFF,YNEW,YINPP,Nin_eff,XOUT2,YOUT2,YOUTP2,YOUTPP2,YOUTINT2, &
        &    nout2,ioptder,sigma2,NBCLFT, &
        &    NBCRGT,XBCLFT,XBCRGT,YBCLFT,YBCRGT,iextrapo,PXEXP0,PXEXPDL,info2)
      !
    case default
      print *,' inttype=',inttype,' ; error in this routine, this should not be possible'
      if (present(info)) info = 4
      return
    end select
    !
    ! 3. Fill in output values
    !
    ! yout, p, pp, int
    if (present(yout)) yout(1:nout2) = yout2
    if (present(youtp)) youtp(1:nout2) = youtp2
    if (present(youtpp)) youtpp(1:nout2) = youtpp2
    if (present(youtint)) youtint(1:nout2) = youtint2
    !
    if (present(info)) info = 0
    !
    deallocate(xout2)
    deallocate(yout2)
    deallocate(youtp2)
    deallocate(youtpp2)
    deallocate(youtint2)
    return
  END SUBROUTINE interpos_def

  SUBROUTINE interpos_defper(XIN,YIN,NIN,NOUT,tension,xout,yout,youtp,youtpp,youtint,NBC,YBC,sigma,info)
    ! for periodic boundary conditions, nbc=-1 and ybc=period should be given
    ! option not needed since no extrapolation and only valid for cubic spline here
    USE prec_rkind
    implicit none
    interface
       SUBROUTINE CBSPLGNP(PXIN,PYIN,PYINNEW,PYINPP,KNIN,PXOUT,PYOUT, &
            &  PYOUTP,PYOUTPP,PYOUTINT,KNOUT,KOPT,PSIG,PERIOD,PXEXP0,PXEXPDL,KFLAG)
         USE prec_rkind
         implicit none
         !
         REAL(RKIND) :: PXIN(KNIN), PYIN(KNIN) &
              &  ,PXOUT(KNOUT), PSIG(KNIN)
         REAL(RKIND) :: PYINNEW(KNIN), PYINPP(KNIN) &
              &  ,PYOUT(KNOUT), PYOUTP(KNOUT), PYOUTPP(KNOUT), PYOUTINT(KNOUT)
         !
         REAL(RKIND) :: PERIOD, PXEXP0, PXEXPDL
         INTEGER :: KOPT, KNIN, KNOUT
         INTEGER :: KFLAG
       END SUBROUTINE CBSPLGNP
    end interface
    INTEGER :: NIN, NBC
    INTEGER,optional :: NOUT
    REAL(RKIND) ::  XIN(NIN), YIN(NIN)
    REAL(RKIND), optional ::  tension
    REAL(RKIND), optional ::  xout(:)
    REAL(RKIND), optional ::  yout(:), youtp(:), youtpp(:), youtint(:)
    REAL(RKIND), optional ::   ybc, sigma(:)
    INTEGER, optional :: info
    ! need variables computed even if not given as argument
    REAL(RKIND), allocatable ::  xout2(:), yout2(:), youtp2(:), youtpp2(:), youtint2(:), xin_eff(:), yin_eff(:)
    REAL(RKIND) ::  sigma2(NIN), zdx, tension_eff
    integer :: info2, nout2, nin_eff, i, j, nverbose=1
    !
    INTEGER ioptder
    REAL(RKIND) :: period
    REAL(RKIND) ::  ynew(NIN), yinpp(NIN), sigmamin
    REAL(RKIND) :: PXEXP0,PXEXPDL, zz
    !
    info2 = -1
    !
    ! 1. Deals with various optional arguments
    !
    ! period
    nin_eff = nin
    if (present(ybc)) then
      period = ybc
      ! check if extra point is given, if yes skip it with warning:
      if (abs(xin(nin_eff)-xin(1)-period) .lt. 1.0e-10_rkind*(xin(2)-xin(1))) then
        if (nverbose .ge. 3) then
          print *,'% it seems periodic point x(n)=x(1)+period is included in input, use nin-1 points'
        end if
        nin_eff = nin_eff-1
      end if
    else
      ! Assumes last x point corresponds to x(1)+period and is redundant
      period=xin(nin_eff)-xin(1)
      nin_eff = nin_eff-1
    end if
    !
    ! check that there are not 2 points too close  in rho: now 1e-5 from average dx
    zdx=period / real(nin_eff,rkind)
    allocate(xin_eff(nin_eff))
    allocate(yin_eff(nin_eff))
    i=1
    xin_eff(1)=xin(1)
    yin_eff(1)=yin(1)
    do j=2,NIN_eff
      if (abs(xin(j)-xin_eff(i)) .gt. 1e-5_rkind*zdx) then
        i=i+1;
        xin_eff(i) = xin(j)
        yin_eff(i) = yin(j)
      end if
    end do
    nin_eff = i;
    if (nverbose .ge. 3) then
      if (nin_eff .ne. nin) write(0,*) 'There were multiple input x points, changed nin=',nin, &
           & ' to nin_eff=',nin_eff,' in interpos_defper to avoid this'
    end if
    !
    ! xout
    if (present(xout)) then
      if (present(nout)) then
        nout2=nout
      else
        ! print *,'% nout not present, use size(xout) instead'
        nout2 = size(xout)
      end if
      allocate(xout2(nout2))
      xout2 = xout(1:nout2)
    else
      nout2 = nin_eff
!      if (present(nout)) nout = nin_eff
      allocate(xout2(nout2))
      xout2 = xin(1:nout2)
    end if
    allocate(yout2(nout2))
    allocate(youtp2(nout2))
    allocate(youtpp2(nout2))
    allocate(youtint2(nout2))
    ! sigma
    if (present(sigma)) then
      ! normalize to minimum value, so that taus=tension at this position
      sigmamin = minval(sigma)
      if (sigmamin .le. 0._rkind) then
        print *,' ERROR: min(sigma)=',sigmamin,' is .le. 0.'
        if (present(info)) info = 1
        return
      end if
      sigma2 = sigma/sigmamin
    else
      sigma2 = 1._rkind
    end if
    ! tension
    if (present(tension)) then
      ! For imposing periodic boundary conditions, one needs a finite tension. 
      ! If tension=0, issue a warning and use 1e-3*default as default
      tension_eff = tension
      if (tension_eff .eq. 0._rkind) then
        if (nverbose .ge. 1) then
          write(0,*) 'Warning, tension=0 with periodic interpos. Needs a finite tension, uses -1e-3'
        end if
        tension_eff = -0.001_RKIND
      end if
      if (tension_eff .gt. 0._rkind) then
        sigma2=tension_eff*sigma2
      else
        ! tension_eff=-1 or negative, uses default value |tension_eff| * Delta_x**3
        !zz=minval(xin_eff(2:nin_eff)-xin_eff(1:nin_eff-1)) (minval not good if there is a very small dx as rho mesh from (R,Z))
        zz=sum(xin_eff(2:nin_eff)-xin_eff(1:nin_eff-1)) / real(nin_eff-1,rkind)
        ! tension_eff=abs(tension_eff)*zz**3 ! if intent(inout) but does not allow call: tension_eff=-1.
        sigma2=abs(tension_eff)*zz**3*sigma2
      end if
    else
      ! zz=minval(xin_eff(2:nin_eff)-xin_eff(1:nin_eff-1))
      zz=sum(xin_eff(2:nin_eff)-xin_eff(1:nin_eff-1)) / real(nin_eff-1,rkind)
      sigma2=zz**3*sigma2
    end if
    !
    ! boundary conditions
    if (nbc .ne. -1) then
      if (nverbose .ge. 1) then
        print *,'nbc when scalar should be -1 for periodic boundary conditions, otherwise vector of size 2'
      end if
      if (present(info)) info = 2
      return
    end if

    ! compute y (0), also yp (1), also ypp (2) or also yint(3)
    ioptder = 0
    if (present(youtp)) ioptder=1
    if (present(youtpp)) ioptder=2
    if (present(youtint)) ioptder=3
    !
    ! 2. call relevant interpolation routine
    !
    ! cubic
    ! general cubic spline routine with periodic boundary conditions
    CALL CBSPLGNP(XIN_EFF,YIN_EFF,YNEW,YINPP,Nin_eff,XOUT2,YOUT2,YOUTP2,YOUTPP2,YOUTINT2, &
      &    nout2,ioptder,sigma2,period,PXEXP0,PXEXPDL,info2)
      !
    !
    ! 3. Fill in output values
    !
    ! yout, p, pp, int
    ! if xout = xin, could add extra periodic point if nin_eff was set to nin-1
    if (present(yout)) yout(1:nout2) = yout2
    if (present(youtp)) youtp(1:nout2) = youtp2
    if (present(youtpp)) youtpp(1:nout2) = youtpp2
    if (present(youtint)) youtint(1:nout2) = youtint2
    !
    if (present(info)) info = 0
    !
    deallocate(xin_eff)
    deallocate(yin_eff)
    deallocate(xout2)
    deallocate(yout2)
    deallocate(youtp2)
    deallocate(youtpp2)
    deallocate(youtint2)
    return
  END SUBROUTINE interpos_defper

  SUBROUTINE interpos_yinpp(XIN,YIN,YINNEW,YINPP,NIN,tension,nbc,ybc,sigma,info)
    !
    USE prec_rkind
    implicit none
    INTEGER :: NIN
    REAL(RKIND) ::  XIN(NIN), YIN(NIN)
    REAL(RKIND) ::   YINNEW(NIN), YINPP(NIN)
    REAL(RKIND), optional ::  tension
    REAL(RKIND), optional ::  ybc(:), sigma(:)
    INTEGER, optional :: nbc(2)
    INTEGER, optional :: info
    ! need variables computed even if not given as argument
    REAL(RKIND) ::  sigma2(NIN)
    integer :: info2, nverbose=1
    !
    INTEGER NBCLFT,NBCRGT
    REAL(RKIND) :: XBCLFT,XBCRGT, YBCLFT,YBCRGT
    REAL(RKIND) ::  sigmamin
    REAL(RKIND) :: PXEXP0,PXEXPDL, zz
    !
    ! 1. Deals with various optional arguments
    !
    if (nverbose .ge. 3) write(0,*) ' in interpos_yinpp'
    !
    ! sigma
    if (present(sigma)) then
      ! normalize to minimum value, so that taus=tension at this position
      sigmamin = minval(sigma)
      if (sigmamin .le. 0._rkind) then
        print *,' ERROR: min(sigma)=',sigmamin,' is .le. 0.'
        if (present(info)) info = 1
        return
      end if
      sigma2 = sigma/sigmamin
    else
      sigma2 = 1._rkind
    end if
    ! tension
    if (present(tension)) then
       if (tension .ge. 0._rkind) then
          sigma2=tension*sigma2
       else
          ! tension=-1 or negative, uses default value |tension| * Delta_x**3
         ! zz=minval(xin(2:nin)-xin(1:nin-1)) (minval not good if there is a very small dx as rho mesh from (R,Z))
          zz=sum(xin(2:nin)-xin(1:nin-1)) / real(nin-1,rkind)
          ! tension=abs(tension)*zz**3 ! if intent(inout) but does not allow call: tension=-1.
          sigma2=abs(tension)*zz**3*sigma2
       end if
    else
       ! zz=minval(xin(2:nin)-xin(1:nin-1))
       zz=sum(xin(2:nin)-xin(1:nin-1)) / real(nin-1,rkind)
       sigma2=zz**3*sigma2
    end if
    !
    ! boundary conditions (if nbc given, ybc should also be given)
    if (present(nbc)) then
      if (present(ybc)) then
        NBCLFT = nbc(1)
        NBCRGT = nbc(2)
        YBCLFT = ybc(1)
        YBCRGT = ybc(2)
        if (size(ybc) .GE. 4) then
          if (NBCLFT .ge. 10) XBCLFT = ybc(3)
          if (NBCRGT .ge. 10) XBCRGT = ybc(4)
          if (size(ybc) .GE. 6) then
            PXEXP0 = ybc(5)
            PXEXPDL= ybc(6)
          end if
        end if
      else
        print *,' problems in interpos, nbc is given but not ybc'
        if (present(info)) info = 2
        return
      end if
    else
      ! default is 2nd derivative=0 at each end
      ! For quadratic interpolation, default is NBCLFT=0 is used for quadratic defined with points and discontinuous 1st derivative
      NBCLFT = 0
      NBCRGT = 0
      YBCLFT = 0._rkind
      YBCRGT = 0._rkind
      ! not used but sets value to make sure:
      XBCLFT = xin(1)
      XBCRGT = xin(nin)
      PXEXP0 = xin(1)-xin(nin)
      PXEXPDL= 1._rkind
    end if
    !
    ! 2. call relevant interpolation routine
    !
    CALL CBFITBND(XIN,YIN,YINNEW,NIN,YINPP,SIGMA2,NBCLFT,NBCRGT, &
      &  XBCLFT,XBCRGT,YBCLFT,YBCRGT,PXEXP0,PXEXPDL)
    !
    if (present(info)) info = 0
    !
    return
  END SUBROUTINE interpos_yinpp

  SUBROUTINE interpos_interp(XIN,YIN,YPP,NIN,NOUT,xout,yout,youtp,youtpp,youtint,option,info)
    ! Note: calls arg ypp instead of yinpp otherwise there is ambiguity with interpos_yinpp
    USE prec_rkind
    implicit none
    INTEGER :: NIN
    INTEGER,optional :: NOUT
    REAL(RKIND) ::  XIN(NIN), YIN(NIN), YPP(NIN)
    REAL(RKIND), optional ::  xout(:)
    REAL(RKIND), optional ::  yout(:), youtp(:), youtpp(:), youtint(:)
    INTEGER, optional :: option
    INTEGER, optional :: info
    ! need variables computed even if not given as argument
    REAL(RKIND), allocatable ::  xout2(:), yout2(:), youtp2(:), youtpp2(:), youtint2(:)
    integer :: info2, nout2, optabs
    !
    INTEGER :: ioptder, iextrapo, inttype
    !
    ! 1. Deals with various optional arguments
    !
    ! xout
    if (present(xout)) then
      if (present(nout)) then
        nout2=nout
      else
        ! print *,'% nout not present, use size(xout) instead'
        nout2 = size(xout)
      end if
      allocate(xout2(nout2))
      xout2 = xout(1:nout2)
    else
      nout2 = nin
!      if (present(nout)) nout = nin
      allocate(xout2(nout2))
      xout2 = xin(1:nout2)
    end if
    allocate(yout2(nout2))
    allocate(youtp2(nout2))
    allocate(youtpp2(nout2))
    allocate(youtint2(nout2))
    ! compute y (0), also yp (1), also ypp (2) or also yint(3)
    ioptder = 0
    if (present(youtp)) ioptder=1
    if (present(youtpp)) ioptder=2
    if (present(youtint)) ioptder=3
    ! option: interpolation and extrapolation choice
    if (.NOT. present(option)) then
      iextrapo = 32 !extrapolation cubic on 1st dx outside and then quadratic
    else
      optabs = abs(option)
      select case (optabs)
      case(1, 11, 21)
        ! linear interpolation
        print *,' should call it only for cubic since uses ypp already calculated by cubic interpolation assumption'
        if (present(info)) info = 3
        return
      case(2,12,22,32,42)
        ! quadratic interpolation
        print *,' should call it only for cubic since uses ypp already calculated by cubic interpolation assumption'
        if (present(info)) info = 4
        return
      case(3,13,23,33,43,53,63)
        ! cubic interpolation
        inttype = 3
        ! extrapolation
        if (optabs .eq. 3)  iextrapo = 0 ! no extrapolation
        if (optabs .eq. 13) iextrapo = sign(32,option) ! cubic on dx then quadratic extrapol. with edge-deriv for 13; edge-val for -13
        if (optabs .eq. 23) iextrapo = sign(1,option) ! linear extrapol. with edge-deriv for 22; edge-val for -22
        if (optabs .eq. 33) iextrapo = sign(2,option) ! quadratic extrapol. with edge-deriv for 33; edge-val for -33
        if (optabs .eq. 43) iextrapo = sign(3,option) ! cubic extrapol. with edge-deriv for 43; edge-val for -43
        if (optabs .eq. 53) iextrapo = sign(31,option) ! cubic on dx then linear extrapol. with edge-deriv for 53; edge-val for -53
        if (optabs .eq. 63) iextrapo = sign(10,option) ! constant outside: y=yedge for option=63, y=0. for -63
      case default
        inttype=mod(optabs,10)
        select case (inttype)
        case (1)
          print *,' should call it only for cubic since uses ypp already calculated by cubic interpolation assumption'
          if (present(info)) info = 3
          return
        case (2)
          print *,' should call it only for cubic since uses ypp already calculated by cubic interpolation assumption'
          if (present(info)) info = 4
          return
        case (3)
          iextrapo = 32
        case default
          print *,'option= ',option, &
            & ' is not valid. The last digit should be 1, 2 or 3 for linear, quadratic or cubic interpolation'
          if (present(info)) info = 3
          return
        end select
      end select
    end if
    !
    ! 2. call relevant interpolation routine
    !
    ! cubic interpolation
    CALL SPLIBNDA(XIN,YIN,YPP,NIN,XOUT2,YOUT2,YOUTP2,YOUTPP2,YOUTINT2,NOUT2, &
      & iextrapo, ioptder)
    !
    ! 3. Fill in output values
    !
    ! yout, p, pp, int
    if (present(yout)) yout(1:nout2) = yout2
    if (present(youtp)) youtp(1:nout2) = youtp2
    if (present(youtpp)) youtpp(1:nout2) = youtpp2
    if (present(youtint)) youtint(1:nout2) = youtint2
    !
    if (present(info)) info = 0
    !
    deallocate(xout2)
    deallocate(yout2)
    deallocate(youtp2)
    deallocate(youtpp2)
    deallocate(youtint2)
    return
  END SUBROUTINE interpos_interp

  SUBROUTINE interpos_defxscal(X,Y,N,xscal,tension,yscal,yscalp,yscalpp,yscalint,nbcscal,ybcscal,sigma,option,info)
    !
    USE prec_rkind
    implicit none
    !
    INTERFACE
       SUBROUTINE INTLINEAR(PXIN,PYIN,KNIN,PXOUT,PYOUT,PYOUTP,PYOUTPP,PYOUTINT,KNOUT,KOPTDER,KEXTRAPO)
         USE PREC_RKIND
         IMPLICIT NONE
         ! arguments
         INTEGER :: KNIN, KNOUT, KOPTDER, KEXTRAPO
         REAL(RKIND) :: PXIN(KNIN), PYIN(KNIN), PXOUT(KNOUT)
         REAL(RKIND) :: PYOUT(KNOUT), PYOUTP(KNOUT), PYOUTPP(KNOUT), PYOUTINT(KNOUT)
       END SUBROUTINE INTLINEAR
       SUBROUTINE INTQUADRATIC(PXIN,PYIN,KNIN,PXOUT,PYOUT,PYOUTP,PYOUTPP,PYOUTINT,KNOUT,KOPTDER,KOPTXPOL,NBC)
         USE PREC_RKIND
         IMPLICIT NONE
         ! arguments
         INTEGER :: KNIN, KNOUT, KOPTDER, KOPTXPOL
         INTEGER, OPTIONAL ::  NBC
         REAL(RKIND) :: PXIN(KNIN), PYIN(KNIN), PXOUT(KNOUT)
         REAL(RKIND):: PYOUT(KNOUT), PYOUTP(KNOUT), PYOUTPP(KNOUT), PYOUTINT(KNOUT)
         !
       END SUBROUTINE INTQUADRATIC
       SUBROUTINE CBSPLGEN(PXIN,PYIN,PYINNEW,PYINPP,KNIN,PXOUT,PYOUT, &
            &  PYOUTP,PYOUTPP,PYOUTINT,KNOUT,KOPT,PSIG,NBCLFT &
            &  ,NBCRGT,XBCLFT,XBCRGT,YBCLFT,YBCRGT,KEXTRPOL,PXEXP0,PXEXPDL,KFLAG)
         USE prec_rkind
         implicit none
         REAL(RKIND) :: EPSILON
         PARAMETER(EPSILON = 1.0E-10_RKIND)
         ! arguments
         integer :: knin, knout, kopt, nbclft, nbcrgt, kextrpol
         integer:: kflag
         real(rkind) :: PXIN(KNIN), PYIN(KNIN), PXOUT(KNOUT), PSIG(KNIN), &
              & xbclft, xbcrgt, ybclft, ybcrgt, pxexp0, pxexpdl
         real(rkind):: PYINNEW(KNIN), PYINPP(KNIN), PYOUT(KNOUT), PYOUTP(KNOUT), &
              & PYOUTPP(KNOUT), PYOUTINT(KNOUT)
       END SUBROUTINE CBSPLGEN
    END INTERFACE
    !
    INTEGER :: N
    REAL(RKIND) ::  X(N), Y(N)
    REAL(RKIND), optional ::  tension
    REAL(RKIND) ::  xscal
    REAL(RKIND), optional ::  yscal, yscalp, yscalpp, yscalint
    REAL(RKIND), optional ::  ybcscal(:), sigma(:)
    INTEGER, optional :: nbcscal(:), option
    INTEGER, optional :: info
    ! need variables computed even if not given as argument
    REAL(RKIND) ::  xout2(1), yout2(1), youtp2(1), youtpp2(1), youtint2(1)
    REAL(RKIND) ::  sigma2(N), xin_eff(N), yin_eff(N)
    integer :: info2, nout2, i, j, nverbose=1
    !
    INTEGER :: ioptder, NBCLFT,NBCRGT, iextrapo, inttype, optabs, nin_eff
    REAL(RKIND) :: XBCLFT,XBCRGT, YBCLFT,YBCRGT
    REAL(RKIND) ::  ynew(N), ypp(N), sigmamin
    REAL(RKIND) :: PXEXP0,PXEXPDL, zz, zdx
    !
    info2 = -1
    !
    ! 1. Deals with various optional arguments
    !
    ! check that there are not 2 points too close  in rho: now 1e-5 from average dx
    zdx=(x(n)-x(1)) / real(n,rkind)
    nin_eff = n
    i=1
    xin_eff(1)=x(1)
    yin_eff(1)=y(1)
    do j=2,NIN_eff
      if (abs(x(j)-xin_eff(i)) .gt. 1e-5_rkind*zdx) then
        i=i+1;
        xin_eff(i) = x(j)
        yin_eff(i) = y(j)
      end if
    end do
    nin_eff = i;
    if (nverbose .ge. 3) then
      if (nin_eff .ne. N) write(0,*) 'There were multiple input x points, changed N=',N, &
           & ' to nin_eff=',nin_eff,' in interpos_defxscal to avoid this'
    end if
    !
    ! xscal
    nout2 = 1
    xout2(1) = xscal
    ! sigma
    if (present(sigma)) then
      ! normalize to minimum value, so that tension=tension at this position
      sigmamin = minval(sigma)
      if (sigmamin .le. 0._rkind) then
        print *,' ERROR: min(sigma)=',sigmamin,' is .le. 0.'
        if (present(info)) info = 1
        return
      end if
      sigma2 = sigma/sigmamin
    else
      sigma2 = 1._rkind
    end if
    ! tension
    if (present(tension)) then
      if (tension .ge. 0._rkind) then
        sigma2=tension*sigma2
      else
        ! tension=-1 or negative, uses default value |tension| * Delta_x**3
        ! zz=minval(xin_eff(2:n)-xin_eff(1:nin_eff-1)) (minval not good if there is a very small dx as rho mesh from (R,Z))
        zz=sum(xin_eff(2:nin_eff)-xin_eff(1:nin_eff-1)) / real(nin_eff-1,rkind)
        ! tension=abs(tension)*zz**3 ! if intent(inout) but does not allow call: tension=-1.
        sigma2=abs(tension)*zz**3*sigma2
      end if
    else
      ! zz=minval(xin_eff(2:nin_eff)-xin_eff(1:nin_eff-1))
      zz=sum(xin_eff(2:nin_eff)-xin_eff(1:nin_eff-1)) / real(nin_eff-1,rkind)
      sigma2=zz**3*sigma2
    end if
    !
    ! boundary conditions (if nbcscal given, ybcscal should also be given)
    if (present(nbcscal)) then
      if (present(ybcscal)) then
        NBCLFT = nbcscal(1)
        NBCRGT = nbcscal(2)
        YBCLFT = ybcscal(1)
        YBCRGT = ybcscal(2)
        if (size(ybcscal) .GE. 4) then
          if (NBCLFT .ge. 10) XBCLFT = ybcscal(3)
          if (NBCRGT .ge. 10) XBCRGT = ybcscal(4)
          if (size(ybcscal) .GE. 6) then
            PXEXP0 = ybcscal(5)
            PXEXPDL= ybcscal(6)
          end if
        end if
      else
        print *,' problems in interpos, nbcscal is given but not ybcscal'
        if (present(info)) info = 2
        return
      end if
    else
      ! default is 2nd derivative=0 at each end
      ! For quadratic interpolation, default is NBCLFT=0 is used for quadratic defined with points and discontinuous 1st derivative
      NBCLFT = 0
      NBCRGT = 0
      YBCLFT = 0._rkind
      YBCRGT = 0._rkind
      ! not used but sets value to make sure:
      XBCLFT = xin_eff(1)
      XBCRGT = xin_eff(nin_eff)
      PXEXP0 = xin_eff(1)-xin_eff(nin_eff)
      PXEXPDL= 1._rkind
    end if
    ! compute y (0), also yp (1), also ypp (2) or also yint(3)
    ioptder = 0
    if (present(yscalp)) ioptder=1
    if (present(yscalpp)) ioptder=2
    if (present(yscalint)) ioptder=3
    ! option: interpolation and extrapolation choice
    if (.NOT. present(option)) then
      inttype=3 ! cubic
      iextrapo = 32 !extrapolation cubic on 1st dx outside and then quadratic
    else
      optabs = abs(option)
      select case (optabs)
      case(1, 11, 21)
        ! linear interpolation
        inttype = 1
        ! extrapolation
        if (optabs .eq. 1)  iextrapo = 0 ! no extrapolation
        if (optabs .eq. 11) iextrapo = sign(1,option) ! linear extrapolation with edge-deriv for 11; edge-val for -11
        if (optabs .eq. 21) iextrapo = sign(10,option) ! constant outside: y=yedge for option=21, y=0. for -21
      case(2,12,22,32,42)
        ! quadratic interpolation
        inttype = 2
        ! extrapolation
        if (optabs .eq. 2)  iextrapo = 0 ! no extrapolation
        if (optabs .eq. 12) iextrapo = sign(21,option) ! quadratic on dx then linear extrapol. with edge-deriv for 12; edge-val for -12
        if (optabs .eq. 22) iextrapo = sign(1,option) ! linear extrapol. with edge-deriv for 22; edge-val for -22
        if (optabs .eq. 32) iextrapo = sign(2,option) ! quadratic extrapol. with edge-deriv for 32; edge-val for -32
        if (optabs .eq. 42) iextrapo = sign(10,option) ! constant outside: y=yedge for option=42, y=0. for -42
      case(3,13,23,33,43,53,63)
        ! cubic interpolation
        inttype = 3
        ! extrapolation
        if (optabs .eq. 3)  iextrapo = 0 ! no extrapolation
        if (optabs .eq. 13) iextrapo = sign(32,option) ! cubic on dx then quadratic extrapol. with edge-deriv for 13; edge-val for -13
        if (optabs .eq. 23) iextrapo = sign(1,option) ! linear extrapol. with edge-deriv for 22; edge-val for -22
        if (optabs .eq. 33) iextrapo = sign(2,option) ! quadratic extrapol. with edge-deriv for 33; edge-val for -33
        if (optabs .eq. 43) iextrapo = sign(3,option) ! cubic extrapol. with edge-deriv for 43; edge-val for -43
        if (optabs .eq. 53) iextrapo = sign(31,option) ! cubic on dx then linear extrapol. with edge-deriv for 53; edge-val for -53
        if (optabs .eq. 63) iextrapo = sign(10,option) ! constant outside: y=yedge for option=63, y=0. for -63
      case default
        inttype=mod(optabs,10)
        select case (inttype)
        case (1)
          iextrapo = 1
        case (2)
          iextrapo = 21
        case (3)
          iextrapo = 32
        case default
          print *,'option= ',option, &
            & ' is not valid. The last digit should be 1, 2 or 3 for linear, quadratic or cubic interpolation'
          if (present(info)) info = 3
          return
        end select
      end select
    end if
    !
    ! 2. call relevant interpolation routine
    !
    select case (inttype)
    case (1)
      ! linear
      call intlinear(Xin_eff,Yin_eff,Nin_eff,xout2,yout2,youtp2,youtpp2,youtint2,NOUT2,ioptder,iextrapo)
    case (2)
      ! quadratic
      call intquadratic(Xin_eff,Yin_eff,Nin_eff,xout2,yout2,youtp2,youtpp2,youtint2,NOUT2,ioptder,iextrapo,NBCLFT)
    case (3)
      ! cubic
      ! general cubic spline routine with non-periodic boundary conditions
      CALL CBSPLGEN(Xin_eff,Yin_eff,YNEW,YPP,Nin_eff,XOUT2,YOUT2,YOUTP2,YOUTPP2,YOUTINT2, &
        &    nout2,ioptder,sigma2,NBCLFT, &
        &    NBCRGT,XBCLFT,XBCRGT,YBCLFT,YBCRGT,iextrapo,PXEXP0,PXEXPDL,info2)
      !
    case default
      print *,' inttype=',inttype,' ; error in this routine, this should not be possible'
      if (present(info)) info = 4
      return
    end select
    !
    ! 3. Fill in output values
    !
    ! yout, p, pp, int
    if (present(yscal)) yscal = yout2(1)
    if (present(yscalp)) yscalp = youtp2(1)
    if (present(yscalpp)) yscalpp = youtpp2(1)
    if (present(yscalint)) yscalint = youtint2(1)
    !
    if (present(info)) info = 0
    !
    return
  END SUBROUTINE interpos_defxscal

  SUBROUTINE interpos_defperxscal(X,Y,N,tension,xscal,yscal,yscalp,yscalpp,yscalint,NBCSCAL,YBCSCAL,sigma,info)
    ! for periodic boundary conditions, nbcscal=-1 and ybcscal=period should be given
    ! option not needed since no extrapolation and only valid for cubic spline here
    USE prec_rkind
    implicit none
    interface
       SUBROUTINE CBSPLGNP(PXIN,PYIN,PYINNEW,PYINPP,KNIN,PXOUT,PYOUT, &
            &  PYOUTP,PYOUTPP,PYOUTINT,KNOUT,KOPT,PSIG,PERIOD,PXEXP0,PXEXPDL,KFLAG)
         USE prec_rkind
         implicit none
         !
         REAL(RKIND) :: PXIN(KNIN), PYIN(KNIN) &
              &  ,PXOUT(KNOUT), PSIG(KNIN)
         REAL(RKIND) :: PYINNEW(KNIN), PYINPP(KNIN) &
              &  ,PYOUT(KNOUT), PYOUTP(KNOUT), PYOUTPP(KNOUT), PYOUTINT(KNOUT)
         !
         REAL(RKIND) :: PERIOD, PXEXP0, PXEXPDL
         INTEGER :: KOPT, KNIN, KNOUT
         INTEGER :: KFLAG
       END SUBROUTINE CBSPLGNP
    end interface
    INTEGER :: N, NBCSCAL
    REAL(RKIND) ::  X(N), Y(N)
    REAL(RKIND), optional ::  tension
    REAL(RKIND) ::  xscal
    REAL(RKIND), optional ::  yscal, yscalp, yscalpp, yscalint
    REAL(RKIND), optional ::   ybcscal, sigma(:)
    INTEGER, optional :: info
    ! need variables computed even if not given as argument
    REAL(RKIND) ::  xout2(1), yout2(1), youtp2(1), youtpp2(1), youtint2(1)
    REAL(RKIND) ::  sigma2(N), zdx, tension_eff
    REAL(RKIND), allocatable ::  xin_eff(:), yin_eff(:)
    integer :: info2, nout2, nin_eff, i, j, nverbose=1
    !
    INTEGER ioptder
    REAL(RKIND) :: period
    REAL(RKIND) ::  ynew(N), ypp(N), sigmamin
    REAL(RKIND) :: PXEXP0,PXEXPDL, zz
    !
    info2 = -1
    !
    ! 1. Deals with various optional arguments
    !
    ! period
    nin_eff = n
    if (present(ybcscal)) then
      period = ybcscal
      ! check if extra point is given, if yes skip it with warning:
      if (abs(x(nin_eff)-x(1)-period) .lt. 1.0e-10_rkind*(x(2)-x(1))) then
        if (nverbose .ge. 3) then
          print *,'% it seems periodic point x(n)=x(1)+period is included in input, use n-1 points'
        end if
        nin_eff = nin_eff-1
      end if
    else
      ! Assumes last x point corresponds to x(1)+period and is redundant
      period=x(nin_eff)-x(1)
      nin_eff = nin_eff-1
    end if
    !
    ! check that there are not 2 points too close  in rho: now 1e-5 from average dx
    zdx=period / real(nin_eff,rkind)
    allocate(xin_eff(nin_eff))
    allocate(yin_eff(nin_eff))
    i=1
    xin_eff(1)=x(1)
    yin_eff(1)=y(1)
    do j=2,NIN_eff
      if (abs(x(j)-xin_eff(i)) .gt. 1e-5_rkind*zdx) then
        i=i+1;
        xin_eff(i) = x(j)
        yin_eff(i) = y(j)
      end if
    end do
    nin_eff = i;
    if (nverbose .ge. 3) then
      if (nin_eff .ne. N) write(0,*) 'There were multiple input x points, changed N=',N, &
           & ' to nin_eff=',nin_eff,' in interpos_defperxscal to avoid this'
    end if
    !
    ! xscal
    nout2 = 1
    xout2(1) = xscal
    ! sigma
    if (present(sigma)) then
      ! normalize to minimum value, so that tension=tension at this position
      sigmamin = minval(sigma)
      if (sigmamin .le. 0._rkind) then
        print *,' ERROR: min(sigma)=',sigmamin,' is .le. 0.'
        if (present(info)) info = 1
        return
      end if
      sigma2 = sigma/sigmamin
    else
      sigma2 = 1._rkind
    end if
    ! tension
    if (present(tension)) then
      ! For imposing periodic boundary conditions, one needs a finite tension. 
      ! If tension=0, issue a warning and use 1e-3*default as default
      tension_eff = tension
      if (tension_eff .eq. 0._rkind) then
        if (nverbose .ge. 1) then
          write(0,*) 'Warning, tension=0 with periodic interpos. Needs a finite tension, uses -1e-3'
        end if
        tension_eff = -0.001_RKIND
      end if
      if (tension_eff .ge. 0._rkind) then
        sigma2=tension_eff*sigma2
      else
        ! tension_eff=-1 or negative, uses default value |tension_eff| * Delta_x**3
        ! zz=minval(xin_eff(2:nin_eff)-xin_eff(1:nin_eff-1)) (minval not good if there is a very small dx as rho mesh from (R,Z)
        zz=sum(xin_eff(2:nin_eff)-xin_eff(1:nin_eff-1)) / real(nin_eff-1,rkind)
        ! tension_eff=abs(tension_eff)*zz**3 ! if intent(inout) but does not allow call: tension_eff=-1.
        sigma2=abs(tension_eff)*zz**3*sigma2
      end if
    else
      ! zz=minval(xin_eff(2:nin_eff)-xin_eff(1:nin_eff-1))
      zz=sum(xin_eff(2:nin_eff)-xin_eff(1:nin_eff-1)) / real(nin_eff-1,rkind)
      sigma2=zz**3*sigma2
    end if
    !
    ! boundary conditions
    if (nbcscal .ne. -1) then
      print *,'nbcscal when scalar should be -1 for periodic boundary conditions, otherwise vector of size 2'
      if (present(info)) info = 2
      return
    end if

    ! compute y (0), also yp (1), also ypp (2) or also yint(3)
    ioptder = 0
    if (present(yscalp)) ioptder=1
    if (present(yscalpp)) ioptder=2
    if (present(yscalint)) ioptder=3
    !
    ! 2. call relevant interpolation routine
    !
    ! cubic
    ! general cubic spline routine with periodic boundary conditions
    CALL CBSPLGNP(XIN_EFF,YIN_EFF,YNEW,YPP,Nin_eff,XOUT2,YOUT2,YOUTP2,YOUTPP2,YOUTINT2, &
      &    nout2,ioptder,sigma2,period,PXEXP0,PXEXPDL,info2)
      !
    !
    ! 3. Fill in output values
    !
    ! yscal, p, pp, int
    if (present(yscal)) yscal = yout2(1)
    if (present(yscalp)) yscalp = youtp2(1)
    if (present(yscalpp)) yscalpp = youtpp2(1)
    if (present(yscalint)) yscalint = youtint2(1)
    !
    if (present(info)) info = 0
    deallocate(xin_eff)
    deallocate(yin_eff)
    !
    return
  END SUBROUTINE interpos_defperxscal

  SUBROUTINE interpos_interpxscal(X,Y,YPP,N,xscal,yscal,yscalp,yscalpp,yscalint,option,info)
    ! Note: calls arg ypp instead of yinpp otherwise there is ambiguity with interpos_yinpp
    USE prec_rkind
    implicit none
    INTEGER :: N
    REAL(RKIND) ::  X(N), Y(N), YPP(N)
    REAL(RKIND) ::  XSCAL
    REAL(RKIND), optional ::  yscal, yscalp, yscalpp, yscalint
    INTEGER, optional :: option
    INTEGER, optional :: info
    ! need variables computed even if not given as argument
    REAL(RKIND) ::  xout2, yout2, youtp2, youtpp2, youtint2
    integer :: info2, nout2, optabs
    !
    INTEGER :: ioptder, iextrapo, inttype
    !
    ! 1. Deals with various optional arguments
    !
    ! xscal
    nout2 = 1
    xout2 = xscal
    ! compute y (0), also yp (1), also ypp (2) or also yint(3)
    ioptder = 0
    if (present(yscalp)) ioptder=1
    if (present(yscalpp)) ioptder=2
    if (present(yscalint)) ioptder=3
    ! option: interpolation and extrapolation choice
    if (.NOT. present(option)) then
      iextrapo = 32 !extrapolation cubic on 1st dx outside and then quadratic
    else
      optabs = abs(option)
      select case (optabs)
      case(1, 11, 21)
        ! linear interpolation
        print *,' should call it only for cubic since uses ypp already calculated by cubic interpolation assumption'
        if (present(info)) info = 3
        return
      case(2,12,22,32,42)
        ! quadratic interpolation
        print *,' should call it only for cubic since uses ypp already calculated by cubic interpolation assumption'
        if (present(info)) info = 4
        return
      case(3,13,23,33,43,53,63)
        ! cubic interpolation
        inttype = 3
        ! extrapolation
        if (optabs .eq. 3)  iextrapo = 0 ! no extrapolation
        if (optabs .eq. 13) iextrapo = sign(32,option) ! cubic on dx then quadratic extrapol. with edge-deriv for 13; edge-val for -13
        if (optabs .eq. 23) iextrapo = sign(1,option) ! linear extrapol. with edge-deriv for 22; edge-val for -22
        if (optabs .eq. 33) iextrapo = sign(2,option) ! quadratic extrapol. with edge-deriv for 33; edge-val for -33
        if (optabs .eq. 43) iextrapo = sign(3,option) ! cubic extrapol. with edge-deriv for 43; edge-val for -43
        if (optabs .eq. 53) iextrapo = sign(31,option) ! cubic on dx then linear extrapol. with edge-deriv for 53; edge-val for -53
        if (optabs .eq. 63) iextrapo = sign(10,option) ! constant outside: y=yedge for option=63, y=0. for -63
      case default
        inttype=mod(optabs,10)
        select case (inttype)
        case (1)
          print *,' should call it only for cubic since uses ypp already calculated by cubic interpolation assumption'
          if (present(info)) info = 3
          return
        case (2)
          print *,' should call it only for cubic since uses ypp already calculated by cubic interpolation assumption'
          if (present(info)) info = 4
          return
        case (3)
          iextrapo = 32
        case default
          print *,'option= ',option, &
            & ' is not valid. The last digit should be 1, 2 or 3 for linear, quadratic or cubic interpolation'
          if (present(info)) info = 3
          return
        end select
      end select
    end if
    !
    ! 2. call relevant interpolation routine
    !
    ! cubic interpolation
    CALL SPLIBNDA(X,Y,YPP,N,XOUT2,YOUT2,YOUTP2,YOUTPP2,YOUTINT2,NOUT2, &
      & iextrapo, ioptder)
    !
    ! 3. Fill in output values
    !
    ! yout, p, pp, int
    if (present(yscal)) yscal = yout2
    if (present(yscalp)) yscalp = youtp2
    if (present(yscalpp)) yscalpp = youtpp2
    if (present(yscalint)) yscalint = youtint2
    !
    if (present(info)) info = 0
    !
    return
  END SUBROUTINE interpos_interpxscal

end MODULE interpos_module
SUBROUTINE CBSPLGEN(PXIN,PYIN,PYINNEW,PYINPP,KNIN,PXOUT,PYOUT, &
  &  PYOUTP,PYOUTPP,PYOUTINT,KNOUT,KOPT,PSIG,NBCLFT &
  &  ,NBCRGT,XBCLFT,XBCRGT,YBCLFT,YBCRGT,KEXTRPOL,PXEXP0,PXEXPDL,KFLAG)
  !     =================================================================
  !
  !     NOTE: THIS ROUTINE INCLUDES THE STANDARD CUBIC SPLINE IF PTAUS=0 (i.e. psig=0):
  !           THEN PYINNEW IS NOT USED AND MDAMAT=3 IS SUFFICIENT
  !           (=> PYINNEW(1) OR PYINNEW=PYIN IS OK)
  !
  !     Interpolate (pxin,pyin) on (pxout,pyout) using
  !     Hirshman fitted cubic spline with ptaus value or
  !     standard cubic spline if PTAUS=0 (psig=ptaus*sigma/sigmamin)
  !
  !     KOPT = 0: ONLY INTERPOLATE FUNCTION INTO PYOUT
  !     KOPT = 1: INTERPOLATE FUNCTION INTO PYOUT AND 1ST DER. INTO PYOUTP
  !     KOPT = 2: AS KOPT=1 PLUS 2ND DER. INTO PYOUTPP
  !     KOPT = 3: AS KOPT=2 PLUS INTEGRAL FROM PXIN(1) UP TO PXOUT(J)
  !
  !     SEE COMMENTS FOR ROUTINE CBFITBND FOR MORE INFORMATION
  !
  !     IF LAPACK ROUTINES NOT AVAILABLE, USE spgbtrf_s.f
  !     (LAPACK SOURCE COPIED FROM NETLIB.ORG)
  !
  !     PXIN    : INPUT ABSCISSA (GIVEN DATA)
  !     PYIN    : INPUT VALUES OF FUNCTION AT PXIN(I),I=1,KNIN
  !     PYINNEW : IF PTAUS.NE.0, THEN PYNEW CONTAINS ON OUTPUT THE NEW VALUES
  !     .         OF THE FUNCTION AT PXIN(I) FOR THE CUBIC SPLINE FIT
  !     PYINPP  : SECOND DER. OF THE CUBIC SPLINE FIT FOR (PXIN,PYIN) IF PTAUS=0 OR
  !     .         ON (PXIN,PYNEW) OTHERWISE
  !     KNIN    : NUMBER OF INPUT POINTS
  !     PXOUT   : X VALUES AT WHICH THE FUNCTION HAS TO BE INTERPOLATED (INPUT)
  !     PYOUT   : INTERPOLATED VALUES AT PXOUT(I),I=1,KNOUT (OUTPUT)
  !     PYOUTP  : INTERPOLATED VALUES OF 1ST DER. OF FUNCTIONS AT PXOUT(I) (OUTPUT, IF KOPT.GE.1)
  !     PYOUTPP : INTERPOLATED VALUES OF 2ND DER. OF FUNCTIONS AT PXOUT(I) (OUTPUT, IF KOPT.EQ.2)
  !     PYOUTINT: INTEGRAL OF Y FROM PXIN(1) TO PXOUT(J)
  !     KNOUT   : NUMBER OF POINTS FOR OUTPUT
  !     KOPT    : SEE ABOVE
  !     PSIG    : SIGMA at each point, normalized by minimum sigma, times PTAUS
  !     PTAUS   : WEIGHT OF SECOND DERIVATIVE IN THE CHI**2 TO BE MINIMIZED. PTAUS=0 GIVES THE
  !     .         STANDARD CUBIC SPLINE. LARGER VALUES OF PTAUS WILL SMOOTH MORE THE 2ND DER.
  !     NBCLFT  : FOR LEFT B.C., VALUE SHOULD BE 0,1,2,10,11 OR 12. (SEE ROUTINE CBFITBND BELOW)
  !     NBCRGT  : FOR RIGHT B.C. (SEE ROUTINE CBFITBND BELOW)
  !     XBCLFT  : FOR LEFT B.C., USED ONLY IF NBCLFT.GE.10 (SEE ROUTINE CBFITBND BELOW)
  !     XBCRGT  : FOR RIGHT B.C., USED ONLY IF NBCRGT.GE.10 (SEE ROUTINE CBFITBND BELOW)
  !     YBCLFT  : VALUE OF LEFT B.C.
  !     YBCRGT  : VALUE OF RIGHT B.C.
  !     .         STANDARD B.C. (SECOND DER. = 0) IS OBTAINED WITH:
  !     .         NBCLFT = NBCRGT = 0 AND YBCLFT = YBCRGT = 0.
  !     KEXTRPOL: OPTION ON HOW TO EXTRAPOLATE THE FUNCTION IF PXOUT(I) IS OUTSIDE [PXIN(1),PXIN(KNIN)]
  !     .       = 0: STOP WITH ERROR MESSAGE IF OUT OF BOUND
  !     .       = 1: LINEAR EXTRAPOLATION
  !     .       = 2: USE QUADRATIC INTERPOLATION IF X OUT OF BOUND
  !     .       = 3: USE CUBIC INTERPOLATION IF X OUT OF BOUND
  !     .       = 21: USE QUADRATIC WITHIN ALFA*DELTA_X AND LINEAR FURTHER
  !     .       = 31: USE CUBIC WITHIN ALFA*DELTA_X AND LINEAR    FURTHER
  !     .       = 32: USE CUBIC WITHIN ALFA*DELTA_X AND QUADRATIC FURTHER
  !     PXEXP0  : PTAUS IS WEIGHTED BY AN EXP(-((X-PXEXP0)/PXEXPDL)**2)
  !     PXEXPDL : IF PXEXP0 NOT IN [PXIN(1),PXIN(KNIN)], EXP() IGNORED AND PTAUS=CST
  !     .         (SEE ROUTINE CBFITBND BELOW)
  !     .         GIVE PXEXP0=PXIN(1)-1. AND PXEXPDL=1. TO GET CONSTANT PTAUS
  !     KFLAG   : ERROR FLAG: IF NOT 0, THERE IS A PROBLEM
  !
  !-----------------------------------------------------------------------
  USE prec_rkind
  implicit none
  REAL(RKIND) :: EPSILON
  PARAMETER(EPSILON = 1.0E-10_RKIND)
  ! arguments
  integer :: knin, knout, kopt, nbclft, nbcrgt, kextrpol
  integer:: kflag
  real(rkind) :: PXIN(KNIN), PYIN(KNIN), PXOUT(KNOUT), PSIG(KNIN), &
    & xbclft, xbcrgt, ybclft, ybcrgt, pxexp0, pxexpdl
  real(rkind):: PYINNEW(KNIN), PYINPP(KNIN), PYOUT(KNOUT), PYOUTP(KNOUT), &
    & PYOUTPP(KNOUT), PYOUTINT(KNOUT)
  !
  integer :: mdmatot, i, ioptmono
  REAL(RKIND) :: zy, zyp, zypp
  !
  !-----------------------------------------------------------------------
  !     0. CHECK INPUT CONSISTENCY
  !
  KFLAG = 0
  IF (PSIG(1) .EQ. 0._RKIND) THEN
    IF (NBCLFT.GE.10 .OR. NBCRGT.GE.10 .OR. NBCLFT.EQ.2 &
      &  .OR. NBCRGT.EQ.2) THEN
      PRINT *,' PTAUS=0, BUT NEED SMOOTHING, WHEN'
      PRINT *,'     NBCLFT.GE.10 .OR. NBCRGT.GE.10 .OR.', &
        &      ' NBCLFT.EQ.2 .OR. NBCRGT.EQ.2'
      PRINT *,' NBCLFT = ',NBCLFT
      PRINT *,' NBCRGT = ',NBCRGT
      !%OS          STOP 'TAU=0'
      KFLAG = 1
      return
    ENDIF
  ENDIF
  !
  !   PXIN in ASCENDING ORDER
  !
  DO i=1,KNIN-1
    if (PXIN(i) .GE. PXIN(i+1)) then
      print *,' xin not in ascending order:'
      print *,' xin(',i,')= ',PXIN(i),'   >=   xin(',i+1,')= ', &
        &      PXIN(i+1)
      KFLAG = 2
      RETURN
    endif
  END DO
  !
  CALL CBFITBND(PXIN,PYIN,PYINNEW,KNIN,PYINPP,PSIG,NBCLFT,NBCRGT, &
    &  XBCLFT,XBCRGT,YBCLFT,YBCRGT,PXEXP0,PXEXPDL)
  !
  !L    2. COMPUTE INTERPOLATED VALUE AT EACH PXOUT
  !
  IF (PSIG(1) .EQ. 0.0_RKIND) THEN
    CALL SPLIBNDA(PXIN,PYIN   ,PYINPP,KNIN,PXOUT,PYOUT,PYOUTP,PYOUTPP,PYOUTINT,KNOUT, &
      &      KEXTRPOL,KOPT)
  ELSE
    CALL SPLIBNDA(PXIN,PYINNEW,PYINPP,KNIN,PXOUT,PYOUT,PYOUTP,PYOUTPP,PYOUTINT,KNOUT, &
      &      KEXTRPOL,KOPT)
  ENDIF
  !
  RETURN
END SUBROUTINE CBSPLGEN
SUBROUTINE CBSPLGNP(PXIN,PYIN,PYINNEW,PYINPP,KNIN,PXOUT,PYOUT, &
  &  PYOUTP,PYOUTPP,PYOUTINT,KNOUT,KOPT,PSIG,PERIOD,PXEXP0,PXEXPDL,KFLAG)
  !     =================================================================
  !
  !   ASSUME PERIODIC BOUNDARY CONDITION WITH Y(PXIN(1)+PERIOD) = PYIN(1)
  !   BUT PYIN(KNIN).NE. PYIN(1), SO DON-T INCLUDE EXTRA PERIODIC POINT
  !
  !     NOTE: THIS ROUTINE INCLUDES THE STANDARD CUBIC SPLINE IF PSIG=0:
  !           THEN PYINNEW IS NOT USED AND MDAMAT=3 IS SUFFICIENT
  !           (=> PYINNEW(1) OR PYINNEW=PYIN IS OK)
  !
  !     Interpolate (pxin,pyin) on (pxout,pyout) using
  !     Hirshman fitted cubic spline with ptaus value or
  !     standard cubic spline if PSIG=0
  !
  !     KOPT = 0: ONLY INTERPOLATE FUNCTION INTO PYOUT
  !     KOPT = 1: INTERPOLATE FUNCTION INTO PYOUT AND 1ST DER. INTO PYOUTP
  !     KOPT = 2: AS KOPT=1 PLUS 2ND DER. INTO PYOUTPP
  !     KOPT = 3: AS KOPT=2 PLUS PRIMITIVE(Y SPLINE) INTO PYOUTINT
  !
  !     SEE COMMENTS FOR ROUTINE CBFITBND FOR MORE INFORMATION
  !
  !     IF LAPACK ROUTINES NOT AVAILABLE, USE spgbtrf_s.f
  !     (LAPACK SOURCE COPIED FROM NETLIB.ORG)
  !
  !     PXIN(KNIN)    : INPUT ABSCISSA (GIVEN DATA)
  !     PYIN(KNIN)    : INPUT VALUES OF FUNCTION AT PXIN(I),I=1,KNIN
  !     PYINNEW(KNIN)   : IF PSIG.NE.0, THEN PYINNEW CONTAINS ON OUTPUT THE NEW VALUES
  !     .         OF THE FUNCTION AT PXIN(I) FOR THE CUBIC SPLINE FIT
  !     PYINPP(KNIN+1)  : SECOND DER. OF THE CUBIC SPLINE FIT FOR (PXIN,PYIN) IF PSIG=0 OR
  !     .         ON (PXIN,PYINNEW) OTHERWISE
  !     KNIN    : NUMBER OF INPUT POINTS
  !     PXOUT(KNOUT)   : X VALUES AT WHICH THE FUNCTION HAS TO BE INTERPOLATED (INPUT)
  !     PYOUT(KNOUT)   : INTERPOLATED VALUES AT PXOUT(I),I=1,KNOUT (OUTPUT)
  !     PYOUTP(KNOUT)  : INTERPOLATED VALUES OF 1ST DER. OF FUNCTIONS AT PXOUT(I) (OUTPUT, IF KOPT.GE.1)
  !     PYOUTPP(KNOUT) : INTERPOLATED VALUES OF 2ND DER. OF FUNCTIONS AT PXOUT(I) (OUTPUT, IF KOPT.GE.2)
  !     PYOUTINT(KNOUT): INTEGRAL OF YOUT AT PXOUT(I) (OUTPUT, IF KOPT.EQ.3)
  !     KNOUT   : NUMBER OF POINTS FOR OUTPUT
  !     KOPT    : SEE ABOVE
  !     PSIG   : WEIGHT OF SECOND DERIVATIVE IN THE CHI**2 TO BE MINIMIZED. PSIG=0 GIVES THE
  !     .         STANDARD CUBIC SPLINE. LARGER VALUES OF PSIG WILL SMOOTH MORE THE 2ND DER.
  !     PXEXP0  : PSIG IS WEIGHTED BY AN EXP(-((X-PXEXP0)/PXEXPDL)**2)
  !     PXEXPDL : IF PXEXP0 NOT IN [PXIN(1),PXIN(1)+PERIOD], EXP() IGNORED AND PSIG=CST
  !     .         (SEE ROUTINE CBFITBND BELOW)
  !     KFLAG   : ERROR FLAG: IF NOT 0, THERE IS A PROBLEM
  !
  !-----------------------------------------------------------------------
  USE prec_rkind
  implicit none
  REAL(RKIND) :: EPSILON
  PARAMETER(EPSILON = 1.0E-10)
  !
  REAL(RKIND) :: PXIN(KNIN), PYIN(KNIN) &
       &  ,PXOUT(KNOUT), PSIG(KNIN)
  REAL(RKIND) :: PYINNEW(KNIN), PYINPP(KNIN) &
       &  ,PYOUT(KNOUT), PYOUTP(KNOUT), PYOUTPP(KNOUT), PYOUTINT(KNOUT)
  !
  REAL(RKIND) :: PERIOD, PXEXP0, PXEXPDL
  INTEGER :: KOPT, KNIN, KNOUT
  INTEGER :: KFLAG
  !
  REAL(RKIND) :: ZY, ZYP, ZYPP, ZYINT, ZDXMIN, ZTAUS
  INTEGER :: I
  !-----------------------------------------------------------------------
  !
  !     0. CHECK INPUT CONSISTENCY
  !
  KFLAG=1
  ZTAUS = PSIG(1)
!!$  IF (ZTAUS .LT. 0) THEN
!!$    ZDXMIN = MINVAL(PXIN(2:KNIN)-PXIN(1:KNIN-1))
!!$    ZTAUS = ABS(ZTAUS) * ZDXMIN**3
!!$    PRINT *,'% TAUS CHANGED TO DEFAULT VALUE = ',ZTAUS
!!$  END IF
  !
  !   PXIN in ASCENDING ORDER
  !
  DO i=1,KNIN-1
    if (PXIN(i) .GE. PXIN(i+1)) then
      print *,' xin not in ascending order:'
      print *,' xin(',i,')= ',PXIN(i),'   >=   xin(',i+1,')= ', &
           &      PXIN(i+1)
      KFLAG = 2
      RETURN
    endif
  END DO
  !
  CALL CBFITPER(PXIN,PYIN,PYINNEW,KNIN,PYINPP,PSIG,PERIOD,PXEXP0,PXEXPDL)
  !
  !L    2. COMPUTE INTERPOLATED VALUE AT EACH PXOUT
  !
  KFLAG=2
  IF (ZTAUS .EQ. 0.0) THEN
    CALL SPLIPERA(PXIN,PYIN   ,PYINPP,KNIN,PXOUT,KNOUT,PYOUT,PYOUTP,PYOUTPP,PYOUTINT, &
      &      PERIOD,KOPT)
  ELSE
    CALL SPLIPERA(PXIN,PYINNEW,PYINPP,KNIN,PXOUT,KNOUT,PYOUT,PYOUTP,PYOUTPP,PYOUTINT, &
      &      PERIOD,KOPT)
  ENDIF
  !
  KFLAG=0
  RETURN
END SUBROUTINE CBSPLGNP
!-----------------------------------------------------------------------
SUBROUTINE CBFITBND(PXIN,PYIN,PYINNEW,KNIN,PYINPP,PSIG, &
  &  NBCLFT,NBCRGT,XBCLFT,XBCRGT,YBCLFT, &
  &  YBCRGT,PXEXP0,PXEXPDL)
  !
  !     NON-PERIODIC B.C
  !
  !     PREPARE SECOND DERIVATIVE OF CUBIC SPLINE INTERPOLATION AND NEW
  !     VALUES OF Y AT NODES YNEW FITTED SUCH THAT CHI**2 + TAUS*F''**2
  !     IS MINIMIZED ACCORDING TO HIRSHMAN ET AL, PHYS. PLASMAS 1 (1994) 2280.
  !     SIG = TAU*SIGMA_K/min(SIGMA_K) OF PAPER.
  !
  !     SETTING TAUS=0., ONE FINDS THE USUAL CUBIC SPLINE INT. WITH CHI**2=0
  !     TAUS LARGE => FIT CLOSER TO STRAIGHT LINE (SECOND DERIV.=0)
  !
  !     IF LAPACK ROUTINES NOT AVAILABLE, USE spgbtrf_s.f
  !     (LAPACK SOURCE COPIED FROM NETLIB.ORG)
  !
  !     IF TAUS=0, PYINNEW NOT USED => PYINNEW(1) OR PYINNEW=PYIN IS OK
  !
  !     BOUNDARY CONDITIONS, 3 TYPES DETERMINED BY THE VALUE OF (NBCLFT/RGT):
  !
  !     0) VALUE OF SECOND DERIVATIVE AT XBCLFT OR RGT IS GIVEN (0 OR 10)
  !     1) VALUE OF 1ST        "       "   "     "  "   "   "   (1 OR 11)
  !     2) VALUE OF FUNCTION AT XBCLFT OR RGT IS GIVEN          (2 OR 12)
  !
  !     THE VALUE IS GIVEN BY YBCLFT OR YBCRGT RESPECTIVELY.
  !
  !     FOR TYPE 1: IF (YBCLFT OR YBCRGT > 1E31 THEN DER. FROM LAGRANGIAN INTERP.
  !     FOR TYPE 1: IF (YBCLFT OR YBCRGT <-1E31 THEN DER. FROM LINEAR     INTERP.
  !
  !     IF NBCLFT OR NBCRGT IS < 10, PXIN(1) OR PXIN(KNIN) IS USED INSTEAD
  !     OF XBCLFT OR XBCRGT, RESPECTIVELY => XBCLFT OR XBCRGT NOT USED
  !
  !     IF END POINTS ARE USED FOR THE B.C. AND TYPE 0 OR 1, THEN USE SYMMETRY
  !     OF MATRIX
  !
  !     IF XBCLFT OR XBCRGT ARE USED, IMPOSE B.C. ON NODE CLOSEST TO XBCLFT OR XBCRGT
  !
  !     TENSION TAUS(K) IS GIVEN WITH AN EXPONENTIAL FORM TO BE ABLE TO LOCALIZE
  !     IT:
  !     .     TAU_K = PTAUS * EXP( -COF * ((X-X0)/DX)**2)
  !
  !     WHERE X0 = PXEXP0 AND DX = PXEXPDL, AND:
  !     COF = 1. IF PXEXP0 IN [PXIN(1),PXIN(KNIN)], 0 OTHERWISE
  !     THUS SETTING PXEXP0 OUTSIDE DOMAIN GIVES A CST TAU_K VALUE
  !
  !     TOTAL DIMENSION OF PAMAT. THE REQUIRED SPACE DEPENDS IF
  !     .         PTAUS IS ZERO OR NOT AND ON THE B.C. (SYMMETRIC OR NOT)
  !     THUS, MDMATOT CAN VARY BETWEEN 2*KNIN AND 10*KNIN (MAX. VALUE NEEDED)
  !
  !-----------------------------------------------------------------------
  !
  USE prec_rkind
  implicit none
  ! arguments
  integer :: KNIN, NBCLFT, NBCRGT
  real(rkind)  :: PXIN(KNIN), PYIN(KNIN), PSIG(KNIN), &
    & xbclft, xbcrgt, ybclft, ybcrgt, pxexp0, pxexpdl
  real(rkind) :: PYINNEW(KNIN), PYINPP(KNIN)
  !
  integer :: idamat, kpm2, ixbc, ibctyp, iik, &
    &  itauval, isym, n, i, j, k, iup, idown, idiag, ishift, ieff, ikp2 &
    &  ,ikp1, ikm1, ikm2, jk, jkp1, jkp2, jeff, iii, iupsofar, idwnsofa &
    &  , jbc, ik, idiamik, idiapik, iklft, irhs, info, &
    &  info2, jkm1, jkm2
  DIMENSION :: KPM2(KNIN,-2:+2), IXBC(2), IBCTYP(2)
  !OS      pointer(iptr_ftauk,ftauk)
  REAL(RKIND), DIMENSION(KNIN) :: ftauk
  !
  REAL(RKIND) :: WHK(KNIN), WOHK(KNIN), ZYBC(2)
  REAL(RKIND) :: xtkm1, xohkm1, xohkm2, xtk, xhkm1, &
    &  xohk, xtkp1, xhk, xohkp1, xykp1, xyk, &
    &  xykm1, ztaueff, zcofexp, zxexp0, zxexpdl, a1, a2, a3, a4, b1, &
    &  b2, b3, b4, px, fakk, fakkp1, fakkp2, frhs, zdelx, zero, zvalue, &
    &  zypeff, ztohkk1, zsign, fa2, fa3, fa0, fa1, &
    &  fakkm1, fakkm2, fun_ftauk, fcccc0, fcccc1, fcccc2, fcccc3
  integer nel
  REAL(RKIND), ALLOCATABLE :: PAMAT(:,:)
  !
  !
  !     FUNCTIONS FOR MATRIX COEFFICIENTS
  !
  REAL(RKIND) :: zsix, zthree, ztwo, zone
  PARAMETER(zsix=6._RKIND, zthree=3._RKIND, ztwo=2._RKIND, zone=1._RKIND)
  FAKKM2(XTKM1,XOHKM1,XOHKM2) = XTKM1*XOHKM1*XOHKM2
  FAKKM1(XTK,XTKM1,XHKM1,XOHK,XOHKM1,XOHKM2) = XHKM1/zsix &
    &  - XOHKM1*(XTK*XOHK+(XTK+XTKM1)*XOHKM1 + XTKM1*XOHKM2)
  FAKK(XTKP1,XTK,XTKM1,XHK,XHKM1,XOHK,XOHKM1) = (XHK+XHKM1)/ZTHREE &
    &  + XOHK*XOHK*(XTKP1+XTK) &
    &  + XOHKM1*(ZTWO*XTK*XOHK+(XTK+XTKM1)*XOHKM1)
  FAKKP1(XTKP1,XTK,XHK,XOHKP1,XOHK,XOHKM1) = XHK/zsix &
    &  - XOHK*(XTKP1*XOHKP1+(XTK+XTKP1)*XOHK + XTK*XOHKM1)
  FAKKP2(XTKP1,XOHKP1,XOHK) = XTKP1*XOHKP1*XOHK
  !
  FRHS(XYKP1,XYK,XYKM1,XOHK,XOHKM1) = (XYKP1-XYK)*XOHK &
    &  - (XYK-XYKM1)*XOHKM1
  !
  !     WHEN ONE WANTS AN ARRAY FOR TAU*SIGMA_K**2, THEN ONE SHOULD REPLACE
  !     THE FUNCTION FTAU BY AN ARRAY
  !
  fun_FTAUK(IIK)= ZTAUEFF*EXP(-ZCOFEXP*(PXIN(IIK)-ZXEXP0)**2 &
    &  /ZXEXPDL**2)
  !%OS      FTAUK(IIK) = ZTAUEFF
  !
  !.......................................................................
  !*COMDECK CUCCCC
  ! ----------------------------------------------------------------------
  ! --     STATEMENT FUNCTION FOR CUBIC INTERPOLATION                   --
  ! --                         23.04.88            AR        CRPP       --
  ! --                                                                  --
  ! -- CUBIC INTERPOLATION OF A FUNCTION F(X)                           --
  ! -- THE EIGHT ARGUMENTS A1,A2,A3,A4,B1,B2,B3,B4 ARE DEFINED BY:      --
  ! -- F(B1) = A1 , F(B2) = A2 , F(B3) = A3 , F(B4) = A4                --
  ! ----------------------------------------------------------------------
  !
  FA3(A1,A2,A3,A4,B1,B2,B3,B4) = &
    &        (A1-A2) / ((B1-B2)*(B2-B4)*(B2-B3)) + &
    &        (A1-A3) / ((B4-B3)*(B3-B1)*(B3-B2)) + &
    &        (A1-A4) / ((B1-B4)*(B2-B4)*(B3-B4))
  FA2(A1,A2,A3,A4,B1,B2,B3,B4) = &
    &        (A1-A2) / ((B2-B1)*(B3-B2)) + &
    &        (A3-A1) / ((B3-B1)*(B3-B2)) - &
    &        (B1+B2+B3) * FA3(A1,A2,A3,A4,B1,B2,B3,B4)
  FA1(A1,A2,A3,A4,B1,B2,B3,B4) = &
    &        (A1-A2) / (B1-B2) - &
    &        (B1+B2) * FA2(A1,A2,A3,A4,B1,B2,B3,B4) - &
    &        (B1*B1+B1*B2+B2*B2) * FA3(A1,A2,A3,A4,B1,B2,B3,B4)
  FA0(A1,A2,A3,A4,B1,B2,B3,B4) = &
    &        A1 - &
    &        B1 * (FA1(A1,A2,A3,A4,B1,B2,B3,B4) + &
    &              B1 * (FA2(A1,A2,A3,A4,B1,B2,B3,B4) + &
    &                    B1 * FA3(A1,A2,A3,A4,B1,B2,B3,B4)))
  ! ----------------------------------------------------------------------
  ! -- FCCCC0 GIVES THE VALUE OF THE FUNCTION AT POINT PX:              --
  ! -- FCCCC0(......,PX) = F(PX)                                        --
  ! ----------------------------------------------------------------------
  FCCCC0(A1,A2,A3,A4,B1,B2,B3,B4,PX) = &
    &              FA0(A1,A2,A3,A4,B1,B2,B3,B4) + &
    &              PX * (FA1(A1,A2,A3,A4,B1,B2,B3,B4) + &
    &                    PX * (FA2(A1,A2,A3,A4,B1,B2,B3,B4) + &
    &                          PX * FA3(A1,A2,A3,A4,B1,B2,B3,B4)))
  ! ----------------------------------------------------------------------
  ! -- FCCCC1 GIVES THE VALUE OF THE FIRST DERIVATIVE OF F(X) AT PX:    --
  ! -- FCCCC1(......,PX) = DF/DX (PX)                                   --
  ! ----------------------------------------------------------------------
  FCCCC1(A1,A2,A3,A4,B1,B2,B3,B4,PX) = &
    &              FA1(A1,A2,A3,A4,B1,B2,B3,B4) + &
    &              PX * (ZTWO * FA2(A1,A2,A3,A4,B1,B2,B3,B4) + &
    &                    ZTHREE * PX * FA3(A1,A2,A3,A4,B1,B2,B3,B4))
  ! ----------------------------------------------------------------------
  ! -- FCCCC2 GIVES THE VALUE OF THE SECOND DERIVATIVE OF F(X) AT PX:   --
  ! -- FCCCC2(......,PX) = D2F/DX2 (PX)                                 --
  ! ----------------------------------------------------------------------
  FCCCC2(A1,A2,A3,A4,B1,B2,B3,B4,PX) = &
    &             ZTWO * FA2(A1,A2,A3,A4,B1,B2,B3,B4) + &
    &             zsix * FA3(A1,A2,A3,A4,B1,B2,B3,B4) * PX
  ! ----------------------------------------------------------------------
  ! -- FCCCC3 GIVES THE VALUE OF THE THIRD DERIVATIVE OF F(X) AT PX:     -
  ! -- FCCCC3(......,PX) = D3F/DX3 (PX)                                  -
  ! ----------------------------------------------------------------------
  FCCCC3(A1,A2,A3,A4,B1,B2,B3,B4,PX) = &
    &                      zsix * FA3(A1,A2,A3,A4,B1,B2,B3,B4)
  !.......................................................................
  !
  !-----------------------------------------------------------------------
  !
  !     0. INITIALIZATION
  !
  ITAUVAL = 1
  IF (PSIG(1) .EQ. 0._RKIND) ITAUVAL = 0
  ZTAUEFF = abs(PSIG(1))
  ZXEXP0 = PXEXP0
  ZXEXPDL = PXEXPDL
  ZCOFEXP = 1.0_RKIND
  IF (ZXEXP0.LT.PXIN(1) .OR. ZXEXP0.GT.PXIN(KNIN)) ZCOFEXP=0.0_RKIND
  !
  ISYM = 1
  IF (NBCLFT.GE.10 .OR. NBCRGT.GE.10 .OR. NBCLFT.EQ.2 &
    &  .OR. NBCRGT.EQ.2) ISYM = 0
  !
  !
  !     0.3 PREPARE BAND WIDTH
  !
  !     PREPARE MATRIX DIMENSION.
  !     MINIMUM REQUIRED:
  !     IUP = 1 = IDOWN; IF PTAUS.NE.0 => IUP = IDOWN = 2
  !     IF SYMMETRIC, USE ONLY UPPER BAND AND DIAGONAL =>IDAMAT=IUP+1
  !     IF ASYMMETRIC => IDAMAT = 2*IDOWN + IUP + 1
  !     IF B.C. NOT AT END OF INTERVAL => IUP = IUP + 1, AND/OR IDOWN=IDOWN+1
  !
  !     => ALTOGETHER, MINIMUM VALUE: IDOWN=1, IUP=1, SYM. =>IDAMAT_MAX = 2
  !     => ALTOGETHER, MAXIMUM VALUE: IDOWN=3, IUP=3 =>IDAMAT_MAX = 10
  !
  !
  IF (ITAUVAL .EQ. 0) THEN
    IUP   = 1
    IDOWN = 1
    IDAMAT = 2
  ELSE
    IUP   = 2
    IDOWN = 2
    IDAMAT = 3
  END IF
  IDIAG = IUP + 1
  IF (ISYM .EQ. 0) THEN
    IDAMAT = 3*(IDAMAT-1)+1
    IF (NBCLFT .GE. 10) THEN
      IUP = IUP + 1
      IDAMAT = IDAMAT + 1
    END IF
    IF (NBCRGT .GE. 10) THEN
      IDOWN = IDOWN + 1
      IDAMAT = IDAMAT + 2
    END IF
    IDIAG = IUP + IDOWN + 1
  ENDIF
  !
  ALLOCATE(PAMAT(IDAMAT,KNIN))
  !
  PYINPP = 0._RKIND
  PAMAT = 0._RKIND
  !
  !     0.2 PRE-COMPUTE H_K AND 1./H_K, and zftauk
  !
  DO K=1,KNIN-1
    WHK(K)  = (PXIN(K+1) - PXIN(K))
  enddo
  DO K=1,KNIN-1
    WHK(K)  = (PXIN(K+1) - PXIN(K))
    WOHK(K) = zone / WHK(K)
    ftauk(k)=psig(k)
    !%OS        ftauk(k)=fun_ftauk(k)
  END DO
  WHK(KNIN) = 0.0_RKIND
  WOHK(KNIN) = 0.0_RKIND
  ftauk(knin)=psig(KNIN)
  !%OS      ftauk(knin)=fun_ftauk(KNIN)
  if (PSIG(1).lt.0._RKIND) ftauk(1)=-10._RKIND*ftauk(1)
  !
  !     0.4 DETERMINE NEIGHBOURS: K-2, K-1, .., K+2
  !     WHEN OUT OF BOUNDS, POINT TO INDEX KNIN, AS WHK, WOHK(KNIN)=0.0
  !
  DO ISHIFT=-2,+2
    DO K=1,KNIN
      KPM2(K,ISHIFT) = K + ISHIFT
    END DO
  END DO
  !     OUT OF INTERVAL: SEND TO KNIN
  KPM2(1,-2)   = KNIN
  KPM2(1,-1)   = KNIN
  KPM2(2,-2)   = KNIN
  KPM2(KNIN-1,+2) = KNIN
  KPM2(KNIN  ,+1) = KNIN
  KPM2(KNIN  ,+2) = KNIN
  !
  !     1. CONSTRUCT MATRIX AND R.H.S
  !     LAPACK SET-UP OF MATRIX:    A(I,J) -> PAMAT(I-J+IDIAG, J)
  !
  !.......................................................................
  !     AS MATRIX SYMMETRIC, COMPUTE ONLY UPPER PART, THAT IS IF J.GE.I
  !
  DO K=1,KNIN-1
    IEFF = K + IDIAG
    IKP2 = KPM2(K,+2)
    IKP1 = KPM2(K,+1)
    IKM1 = KPM2(K,-1)
    IKM2 = KPM2(K,-2)
    !     A(K,K)
    JK = K
    PAMAT(IEFF-JK,JK) = FAKK(FTAUK(IKP1),FTAUK(K),FTAUK(IKM1),WHK(K) &
      &    ,WHK(IKM1),WOHK(K),WOHK(IKM1))
    !
    !     A(K,K+1)
    JKP1 = K + 1
    PAMAT(IEFF-JKP1,JKP1) = FAKKP1(FTAUK(IKP1),FTAUK(K),WHK(K), &
      &    WOHK(IKP1),WOHK(K),WOHK(IKM1))
    !     A(K,K-1)
    !%OS        JKM1 = K - 1
    !%OS        PAMAT(IEFF-JKM1,JKM1) = FAKKM1(FTAUK(K),FTAUK(IKM1),WHK(IKM1), &
    !%OS     &    WOHK(K),WOHK(IKM1),WOHK(IKM2))
    !
    IF (ITAUVAL .EQ. 1) THEN
      !     A(K,K+2)
      JKP2 = K + 2
      IF (JKP2 .LE. KNIN) &
        &      PAMAT(IEFF-JKP2,JKP2) = FAKKP2(FTAUK(IKP1),WOHK(IKP1), &
        &      WOHK(K))
      !     A(K,K-2)
      !%OS          JKM2 = K - 2
      !%OS          PAMAT(IEFF-JKM2,JKM2) = FAKKM2(FTAUK(IKM1),WOHK(IKM1), &
      !%OS     &      WOHK(IKM2))
    ENDIF
    !     B(K)
    PYINPP(K) = FRHS(PYIN(IKP1),PYIN(K),PYIN(IKM1),WOHK(K), &
      &    WOHK(IKM1))
    !
  END DO
  !
  !     2. BOUNDARY CONDITIONS
  !
  !     2.1 IF NON-SYMMETRIC, COPY TOP PART TO BOTTOM BEFORE APPLYING
  !     B.C.
  !
  IF (ISYM .EQ. 0) THEN
    DO I=1,KNIN
      IEFF = I + IDIAG
      DO J=I+1,MIN(I+MIN(IUP,IDOWN),KNIN)
        JEFF = J + IDIAG
        !     A(J,I) = A(I,J)
        PAMAT(JEFF-I,I) = PAMAT(IEFF-J,J)
      END DO
    END DO
  ENDIF
  !%OS
  !     debug, print matrix and rhs
  !%OS      write(6,'(3a4,a)') 'i ','j1 ','j2 ',' i,j1  i,j1+1,..... i,j2'
  !%OS      do i=1,knin
  !%OS        ieff = idiag + i
  !%OS        j1 = i-idown
  !%OS        j2 = i+iup
  !%OSc%OS        j1 = max(i-idown,1)
  !%OSc%OS        j2 = min(i+iup,knin)
  !%OS        write(6,'(3i4,1p10e13.4)') i,j1,j2,(pamat(ieff-j,j),j=j1,j2)
  !%OS      end do
  !%OS      write(6,'(a4,a12)') 'i','RHS'
  !%OS      write(6,'(i4,1pe13.4)') (i,pyinpp(i),i=1,knin)
  !
  !%OS
  !
  !     2.2 B.C. AT TWO LOCATIONS PXIN(IXBC(JBC)), JBC=1,2
  !     IBCTYP(JBC) = 0, 1 OR 2 (TYPE OF B.C, SEE ABOVE).
  !     SO FAR USES NODE CLOSEST TO XBCLFT/RGT FOR LOCATION
  !     OF B.C., INSTEAD OF ACTUAL VALUE OF XBCLFT/RGT
  !
  IXBC(1) = 1
  IXBC(2) = KNIN
  IF (NBCLFT .GE. 10) THEN
    DO I=1,KNIN
      IF (PXIN(I) .GE. XBCLFT) GO TO 220
    END DO
220 CONTINUE
    ZDELX = ABS(PXIN(I)-XBCLFT)
    IXBC(1) = I
    IF (I .GE. KNIN) THEN
      IXBC(1) = KNIN
      PRINT *,' WARNING: LEFT B.C. AT I=KNIN: XBCLFT=',XBCLFT, &
        &      '  PXIN(KNIN)= ',PXIN(KNIN)
    ELSE IF (ABS(PXIN(I-1)-XBCLFT).LE.ZDELX .AND. I.NE.1) THEN
      IXBC(1) = I-1
    ENDIF
  ENDIF
  !
  IF (NBCRGT .GE. 10) THEN
    DO I=1,KNIN
      IF (PXIN(I) .GE. XBCRGT) GO TO 221
    END DO
221 CONTINUE
    ZDELX = ABS(PXIN(I)-XBCRGT)
    IXBC(2) = I
    IF (I .LE. 1) THEN
      IXBC(2) = 1
      PRINT *,' WARNING: RIGHT B.C. AT I=1: XBCRGT=',XBCRGT, &
        &      '  PXIN(1)= ',PXIN(1)
    ELSE IF (I .GT. KNIN) THEN
      IXBC(2) = KNIN
    ELSE IF (ABS(PXIN(I-1)-XBCRGT) .LE. ZDELX) THEN
      IXBC(2) = I-1
    ENDIF
  ENDIF
  !
  ZYBC(1) = YBCLFT
  ZYBC(2) = YBCRGT
  nel=10
  IBCTYP(1) = MOD(NBCLFT,nel)
  IBCTYP(2) = MOD(NBCRGT,nel)
  IF (IXBC(1) .EQ. IXBC(2)) THEN
    PRINT *,' ERROR, B.C. AT SAME LOCATIONS: IXBC(1)=IXBC(2)= ', &
      &    IXBC(1)
    !%OS        STOP '1=2'
    RETURN
  ELSE IF (IXBC(1) .GT. IXBC(2)) THEN
    PRINT *,' WARNING, NEEDED TO SWITCH B.C. POINTS AS IXBC(1)= ', &
      &    IXBC(1),' > IXBC(2)= ',IXBC(2)
    III = IXBC(1)
    IXBC(1) = IXBC(2)
    IXBC(2) = III
    ZYBC(1) = YBCRGT
    ZYBC(2) = YBCLFT
    nel=10
    IBCTYP(1) = MOD(NBCRGT,nel)
    IBCTYP(2) = MOD(NBCLFT,nel)
  ENDIF
  !
  !     2.3 MOVE EQUATIONS UP OR DOWN IF B.C. IS NOT AN END POINT
  !
  IF (IXBC(1) .NE. 1) THEN
    !
    !     MOVE ROW EQ. K=2,..,IXBC(1) UP BY ONE
    IUPSOFAR = IUP - 1
    DO K=2,IXBC(1)
      DO J=MAX(1,K-IDOWN),MIN(KNIN,K+IUPSOFAR)
        PAMAT(IDIAG+(K-1)-J,J) = PAMAT(IDIAG+K-J,J)
      END DO
      PYINPP(K-1) = PYINPP(K)
      !     ZERO A((K-1),(K-1)-IDOWN)
      IF (K-1-IDOWN .GE. 1) PAMAT(IDIAG+IDOWN,K-1-IDOWN) = 0.0_RKIND
    END DO
    !     ZERO ROW IXBC(1) AND RHS
    K = IXBC(1)
    DO J=MAX(1,K-IDOWN),MIN(KNIN,K+IUP)
      PAMAT(IDIAG+K-J,J) = 0.0_RKIND
    END DO
    PYINPP(K) = 0.0_RKIND
  ENDIF
  !
  IF (IXBC(2) .NE. KNIN) THEN
    !     
    !     MOVE EQ. K=IXBC(2),..,KNIN-1 DOWN BY ONE
    IDWNSOFA = IDOWN - 1
    DO K=KNIN-1,IXBC(2),-1
      DO J=MAX(1,K-IDWNSOFA),MIN(KNIN,K+IUP)
        PAMAT(IDIAG+(K+1)-J,J) = PAMAT(IDIAG+K-J,J)
      END DO
      PYINPP(K+1) = PYINPP(K)
      !     ZERO A((K+1),(K+1)+IUP)
      IF (K+1+IUP .LE. KNIN) PAMAT(IDIAG-IUP,K+1+IUP) = 0.0_RKIND
    END DO
    !     ZERO ROW IXBC(2) AND RHS
    K = IXBC(2)
    DO J=MAX(1,K-IDOWN),MIN(KNIN,K+IUP)
      PAMAT(IDIAG+K-J,J) = 0.0_RKIND
    END DO
    PYINPP(K) = 0.0_RKIND
  ENDIF
  !
  !     2.4 FOR ROW=IXBC(), MODIFY MATRIX AND RHS ACCORDING TO B.C. TYPE
  !
  ZERO = 0.0_RKIND
  DO JBC=1,2
    IK = IXBC(JBC)
    ZVALUE = ZYBC(JBC)
    IEFF = IK + IDIAG
    IKP2 = KPM2(IK,+2)
    IKP1 = KPM2(IK,+1)
    IKM1 = KPM2(IK,-1)
    IKM2 = KPM2(IK,-2)
    IF (IBCTYP(JBC) .EQ. 0) THEN
      !
      !     SYMMETRIZE => COL IK GOES TO RIGHT-HAND SIDE AND THEN ZEROED
      !
      IF (ISYM .EQ. 1) THEN
        IDIAMIK = IDIAG - IK
        DO I=MAX(1,IK-IUP),IK-1
          PYINPP(I) = PYINPP(I) - ZVALUE * PAMAT(I+IDIAMIK,IK)
          PAMAT(I+IDIAMIK,IK) = 0.0_RKIND
        END DO
        IDIAPIK = IDIAG + IK
        DO I=IK+1,MIN(KNIN,IK+IUP)
          PYINPP(I) = PYINPP(I) - ZVALUE * PAMAT(IDIAPIK-I,I)
          PAMAT(IDIAPIK-I,I) = 0.0_RKIND
        END DO
      ELSE
        IDIAMIK = IDIAG - IK
        DO I=MAX(1,IK-IUP),MIN(KNIN,IK+IDOWN)
          PYINPP(I) = PYINPP(I) - ZVALUE * PAMAT(I+IDIAMIK,IK)
          PAMAT(I+IDIAMIK,IK) = 0.0_RKIND
        END DO
        !     ZERO ROW IK
        DO J=MAX(1,IK-IDOWN),MIN(KNIN,IK+IUP)
          PAMAT(IEFF-J,J) = 0.0_RKIND
        END DO
      ENDIF
      !
      !     REPLACE ROW IK BY EQUATION: G_K = ZVALUE
      PAMAT(IDIAG,IK) = 1.0_RKIND
      PYINPP(IK) = ZVALUE
      !
    ELSE IF (IBCTYP(JBC) .EQ. 1) THEN
      !
      !     1ST DERIVATIVE GIVEN
      !
      ZYPEFF = ZVALUE
      IF (ZVALUE .GT. 1.E+31_RKIND) THEN
        !     FROM LGRANGIAN INTERPOLATION
        IKLFT = IK - 1
        IF (IK .EQ. 1) IKLFT = IK
        IF (IKLFT+3 .GT. KNIN) IKLFT = KNIN - 3
        ZYPEFF = FCCCC1(PYIN(IKLFT),PYIN(IKLFT+1),PYIN(IKLFT+2), &
          &        PYIN(IKLFT+3),PXIN(IKLFT),PXIN(IKLFT+1),PXIN(IKLFT+2), &
          &        PXIN(IKLFT+3),PXIN(IK))
      ELSE IF (ZVALUE .LT. -1.E+31_RKIND) THEN
        IKLFT = IK
        IF (IK .EQ. KNIN) IKLFT = IK - 1
        ZYPEFF = (PYIN(IKLFT+1)-PYIN(IKLFT)) &
          &        / (PXIN(IKLFT+1)-PXIN(IKLFT))
      ENDIF
      ZTOHKK1 = FTAUK(IK)*WOHK(IK)*WOHK(IKM1)
      !     A(IK,IK)
      IF (IK .NE. KNIN) PAMAT(IEFF-IK,IK) = FAKK(FTAUK(IKP1),FTAUK(IK), &
        &      ZERO,WHK(IK),ZERO,WOHK(IK),ZERO) + ZTOHKK1
      IF (IK .EQ. KNIN) PAMAT(IEFF-IK,IK) = FAKK(ZERO,FTAUK(IK), &
        &      FTAUK(IKM1),ZERO,WHK(IKM1),ZERO,WOHK(IKM1)) + ZTOHKK1
      !     A(IK,IK-1)
      JKM1 = IK - 1
      IF (ISYM.EQ.0 .AND. JKM1.GE.1) THEN
        IF (IK .NE. KNIN) PAMAT(IEFF-JKM1,JKM1) = - ZTOHKK1
        IF (IK .EQ. KNIN) PAMAT(IEFF-JKM1,JKM1) = FAKKM1(FTAUK(IK), &
          &        FTAUK(IKM1),WHK(IKM1),ZERO,WOHK(IKM1),WOHK(IKM2))
      ENDIF
      !     A(IK,IK+1)
      JKP1 = IK + 1
      IF (JKP1 .LE. KNIN) &
        &      PAMAT(IEFF-JKP1,JKP1) = FAKKP1(FTAUK(IKP1), &
        &      FTAUK(IK),WHK(IK),WOHK(IKP1),WOHK(IK),ZERO)
      !
      IF (ITAUVAL .EQ. 1) THEN
        !     A(IK,IK+2)
        JKP2 = IK + 2
        IF (JKP2 .LE. KNIN) &
          &        PAMAT(IEFF-JKP2,JKP2) = FAKKP2(FTAUK(IKP1),WOHK(IKP1), &
          &        WOHK(IK))
        !     A(IK,IK-2)
        JKM2 = IK - 2
        IF (ISYM.EQ.0 .AND. JKM2.GE.1) THEN
          IF (IK .NE. KNIN) PAMAT(IEFF-JKM2,JKM2) = 0.0_RKIND
          IF (IK .EQ. KNIN) PAMAT(IEFF-JKM2,JKM2) = FAKKM2(FTAUK(IKM1), &
            &          WOHK(IKM1),WOHK(IKM2))
        ENDIF
      ENDIF
      !     RHS
      ZSIGN = -1._RKIND
      IF (IK .EQ. KNIN) ZSIGN = +1._RKIND
      IF (IK .NE. KNIN) PYINPP(IK) = FRHS(PYIN(IKP1),PYIN(IK),ZERO, &
        &      WOHK(IK),ZERO) - ZYPEFF
      IF (IK .EQ. KNIN) PYINPP(IK) = FRHS(ZERO,PYIN(IK),PYIN(IKM1), &
        &      ZERO,WOHK(IKM1)) + ZYPEFF
      !
    ELSE IF (IBCTYP(JBC) .EQ. 2) THEN
      !
      !     FUNCTION IS GIVEN
      !
      !     A(IK,IK)
      PAMAT(IEFF-IK,IK) = - FTAUK(IK) * (WOHK(IK) + WOHK(IKM1))
      !     A(IK,IK+1)
      JKP1 = IK + 1
      IF (JKP1 .LE. KNIN) &
        &      PAMAT(IEFF-JKP1,JKP1) = FTAUK(IK) * WOHK(IK)
      !     A(IK,IK-1)
      JKM1 = IK - 1
      IF (ISYM.EQ.0 .AND. JKM1.GE.1) &
        &      PAMAT(IEFF-JKM1,JKM1) = FTAUK(IK) * WOHK(IKM1)
      !
      IF (ITAUVAL .EQ. 1) THEN
        !     A(IK,IK+2)
        JKP2 = IK + 2
        IF (JKP2 .LE. KNIN) PAMAT(IEFF-JKP2,JKP2) = 0.0_RKIND
        !     A(IK,IK-2)
        JKM2 = IK - 2
        IF (ISYM.EQ.0 .AND. JKM2.GE.1) PAMAT(IEFF-JKM2,JKM2) = 0.0_RKIND
      ENDIF
      !     RHS
      PYINPP(IK) = PYIN(IK) - ZVALUE
      !
    ENDIF
    !
  END DO
  !
  !     3. SOLVE SYSTEM
  !
  !     USE INTEGER WORK SPACE FROM KPM2(0) ARRAY FOR IPIVOT,
  !     AS KPM2(K,0) NOT NEEDED NOR USED
  !
  !%OS
  !     debug, print matrix and rhs
  !%OS      write(6,'(3a4,a)') 'i ','j1 ','j2 ',' i,j1  i,j1+1,..... i,j2'
  !%OS      do i=1,knin
  !%OS        ieff = idiag + i
  !%OS        j1 = i-idown
  !%OS        j2 = i+iup
  !%OSc%OS        j1 = max(i-idown,1)
  !%OSc%OS        j2 = min(i+iup,knin)
  !%OS        write(6,'(3i4,1p10e13.4)') i,j1,j2,(pamat(ieff-j,j),j=j1,j2)
  !%OS      end do
  !%OS      write(6,'(a4,a12)') 'i','RHS'
  !%OS      write(6,'(i4,1pe13.4)') (i,pyinpp(i),i=1,knin)
  !
  !%OS
  !%OS      print *,'isym= ',isym
  IF (ISYM .EQ. 1) THEN
    !   Single precision (using routines in spgbtrf_s.f compiled with -r8 for example)
    !   Double precision (using the lapack libraries from the compiler)
    CALL DPBTRF('U',KNIN,IUP,PAMAT,IDAMAT,INFO)
  ELSE
    CALL DGBTRF(KNIN,KNIN,IDOWN,IUP,PAMAT,IDAMAT,KPM2(1,0),INFO)
  ENDIF
  IRHS = 1
  IF (INFO .EQ. 0) THEN
    IF (ISYM .EQ. 1) THEN
      CALL DPBTRS('U',KNIN,IUP,IRHS,PAMAT,IDAMAT,PYINPP,KNIN,INFO2)
    ELSE
      CALL DGBTRS('N',KNIN,IDOWN,IUP,IRHS,PAMAT,IDAMAT,KPM2(1,0),PYINPP, &
        &      KNIN,INFO2)
    ENDIF
  ELSE
    PRINT *,' ERROR IN SP/GBTRF: INFO = ',INFO
    !%OS        STOP 'INFO'
    RETURN
  ENDIF
  !
  !     4. COMPUTE NEW VALUES OF Y_K (NON-STANDARD CUBIC SPLINE ONLY)
  !
  IF (ITAUVAL .EQ. 1) THEN
    DO K=1,KNIN
      IKP1 = KPM2(K,+1)
      IKM1 = KPM2(K,-1)
      PYINNEW(K) = PYIN(K) - FTAUK(K) * &
        &      ((PYINPP(IKP1)-PYINPP(K))*WOHK(K) &
        &      - (PYINPP(K)-PYINPP(IKM1))*WOHK(IKM1))
    END DO
    !
  ENDIF

  IF (INFO2 .LT. 0) THEN
    PRINT *,' ERROR IN SP/GBTRS: INFO2 = ',INFO2
    !%OS        STOP 'INFO2'
    RETURN
  ENDIF
  !
  DEALLOCATE(PAMAT)
  !
  RETURN
END SUBROUTINE CBFITBND
SUBROUTINE CBFITPER(PXIN,PYIN,PYINNEW,KNIN,PYINPP,PSIG,PERIOD,PXEXP0,PXEXPDL)
  !
  !     PERIODIC B.C WITH Y(PXIN(1)+PERIOD) = PYIN(1)
  !
  !     PREPARE SECOND DERIVATIVE OF CUBIC SPLINE INTERPOLATION AND NEW
  !     VALUES OF Y AT NODES YNEW FITTED SUCH THAT CHI**2 + TAUS*F''**2
  !     IS MINIMIZED ACCORDING TO HIRSHMAN ET AL, PHYS. PLASMAS 1 (1994) 2280.
  !     TAUS = TAU*SIGMA_K OF PAPER ASSUMING SIGMA_K CONSTANT.
  !
  !     SETTING TAUS=0., ONE FINDS THE USUAL CUBIC SPLINE INT. WITH CHI**2=0
  !     TAUS LARGE => FIT CLOSER TO STRAIGHT LINE (SECOND DERIV.=0)
  !
  !     IF LAPACK ROUTINES NOT AVAILABLE, USE NONSYM.F AND REMOVE "c%nonsym"
  !     (COPIED SOURCE FROM NETLIB.COM)
  !
  !     IF TAUS=0, PYINNEW NOT USED => PYINNEW(1) OR PYINNEW=PYIN IS OK
  !
  !
  USE prec_rkind
  implicit none
  !
  REAL(RKIND), DIMENSION(KNIN) :: PXIN, PYIN, PYINNEW, PYINPP &
       &  ,WHK, WOHK, PSIG
  REAL(RKIND), ALLOCATABLE :: PAMAT(:,:)
  REAL(RKIND), DIMENSION(KNIN) :: FTAUK

  INTEGER :: KTONUM(KNIN), KPM2(KNIN,-2:+2)
  INTEGER INFO2, N, IDOWN, IUP, IRHS
  !
  REAL(RKIND) :: FAKKM2, FAKKM1, FAKK, FAKKP1, FAKKP2, FRHS, fun_FTAUK, ZTAUEFF
  REAL(RKIND) :: PXEXP0, PXEXPDL, ZXEXP0, ZXEXPDL, ZCOFEXP
  REAL(RKIND) :: XTKM1, XTK, XTKP1, XOHKP1, XOHKM1, XHKM1, XHK, XOHKM2, XOHK, XYK, XYKP1, XYKM1, PERIOD
  !
  INTEGER :: ICUBSTD, KNIN, I, J, K, IBAND, IDIAG, &
    &  IDAMAT, IHALF, ISHIFT, IEFF, IKP2, IKP1, IKM1, IKM2, JKM2, JKM1, JK, &
    &  JKP1, JKP2, II
  !
  !%nonsym
!!$      REAL(RKIND) :: ANONSYM(501*9)
!!$      INTEGER :: IPIVOT(501)
  !%nonsym
  CHARACTER*1 ZCHAR
  !
  !     FUNCTIONS FOR MATRIX COEFFICIENTS
  !
  FAKKM2(XTKM1,XOHKM1,XOHKM2) = XTKM1*XOHKM1*XOHKM2
  FAKKM1(XTK,XTKM1,XHKM1,XOHK,XOHKM1,XOHKM2) = XHKM1/6. &
    &  - XOHKM1*(XTK*XOHK+(XTK+XTKM1)*XOHKM1 + XTKM1*XOHKM2)
  FAKK(XTKP1,XTK,XTKM1,XHK,XHKM1,XOHK,XOHKM1) = (XHK+XHKM1)/3. &
    &  +XOHK*XOHK*(XTKP1+XTK) + XOHKM1*(2.*XTK*XOHK+(XTK+XTKM1)*XOHKM1)
  FAKKP1(XTKP1,XTK,XHK,XOHKP1,XOHK,XOHKM1) = XHK/6. &
    &  - XOHK*(XTKP1*XOHKP1+(XTK+XTKP1)*XOHK + XTK*XOHKM1)
  FAKKP2(XTKP1,XOHKP1,XOHK) = XTKP1*XOHKP1*XOHK
  !
  FRHS(XYKP1,XYK,XYKM1,XOHK,XOHKM1) = (XYKP1-XYK)*XOHK &
    &  - (XYK-XYKM1)*XOHKM1
  !
  !     WHEN ONE WANTS AN ARRAY FOR TAU*SIGMA_K**2, THEN ONE SHOULD REPLACE
  !     THE FUNCTION fun_FTAU BY AN ARRAY
  !
  fun_FTAUK(II)= ZTAUEFF*EXP(-ZCOFEXP*(PXIN(II)-ZXEXP0)**2/ZXEXPDL**2)
  ! fun_FTAUK(II) = ZTAUEFF
  !
  !-----------------------------------------------------------------------
  !
  !     0. INITIALIZATION
  !
  ICUBSTD = 0
  IF (PSIG(1) .EQ. 0._RKIND) ICUBSTD = 1
  !
  ZTAUEFF = abs(PSIG(1))
  ZXEXP0 = PXEXP0
  ZXEXPDL = PXEXPDL
  ZCOFEXP = 1.0_RKIND
  IF (ZXEXP0.LT.PXIN(1) .OR. ZXEXP0.GT.PXIN(1)+PERIOD) ZCOFEXP=0.0_RKIND
  !
  !.......................................................................
  !
  !     0.3 PREPARE BAND WIDTH
  !
  IUP   = 4
  IDOWN = 4
  IDAMAT = 13
  IF (ICUBSTD .EQ. 1) THEN
    IUP   = 2
    IDOWN = 2
    IDAMAT = 7
  END IF
  IBAND = IDOWN + IUP + 1
  IDIAG = IBAND
  ALLOCATE(PAMAT(IDAMAT,KNIN))
  !%nonsym
  IDIAG = IBAND
  !%nonsym
  !
  PYINPP = 0.0_RKIND
  PAMAT = 0.0_RKIND
  !
  !     0.2 PRE-COMPUTE H_K AND 1./H_K
  !
  DO K=1,KNIN-1
    WHK(K)  = (PXIN(K+1) - PXIN(K))
    WOHK(K) = 1. / WHK(K)
  END DO
  WHK(KNIN) = PXIN(1) + PERIOD - PXIN(KNIN)
  IF (WHK(KNIN) .EQ. 0.0_RKIND) THEN
    PRINT *,' PXIN(1) + PERIOD - PXIN(KNIN) = 0. IN CBFITPER'
    PRINT *,' DO NOT INCLUDE EXTRA PERIODIC POINT: KNIN .NE. 1', &
      &    ' => CHANGE KNIN TO KNIN-1 ?'
!!$        STOP 'CBFITPER'
    return
  ELSE
    WOHK(KNIN) = 1. / WHK(KNIN)
  ENDIF
  !
  !   0.4 INDEXING OF UNKNOWNS IN ALTERNATIVE UP,DOWN,UP,... WAY IN ORDER
  !   TO KEEP A BAND MATRIX, EVEN IF IT DOUBLES THE BAND-WIDTH:
  !   K -> UNKNOWN_NUMBER:
  !   . KTONUM(K) = INUM WITH INUM=1 FOR K=1 ; =2*(K-1) FOR K IN [2,IHALF];
  !   . INUM=2*(KNIN-K)+3 FOR K IN [IHALF+1,KNIN]; WITH IHALF=KNIN/2 + 1
  !   WARNING: CARE IF KNIN IS ODD
  !
  IHALF = KNIN/2 + 1
  KTONUM(1) = 1
  DO K=2,IHALF
    KTONUM(K) = 2*K - 2
  END DO
  DO K=IHALF+1,KNIN
    KTONUM(K) = 2*(KNIN-K) + 3
  END DO
  !
  !     0.5 DETERMINE NEIGHBOURS: K-2, K-1, .., K+2
  !
  DO ISHIFT=-2,+2
    DO K=1,KNIN
      KPM2(K,ISHIFT) = K + ISHIFT
    END DO
  END DO
  !     PERIODIC ENDS
  KPM2(1,-2)   = KNIN - 1
  KPM2(1,-1)   = KNIN
  KPM2(2,-2)   = KNIN
  KPM2(KNIN-1,+2) = 1
  KPM2(KNIN  ,+1) = 1
  KPM2(KNIN  ,+2) = 2
  !
  !     1. CONSTRUCT MATRIX AND R.H.S
  !     LAPACK SET-UP OF MATRIX:    A(I,J) -> AMAT(I-J+IDIAG, J)
  !
  !.......................................................................
  !     (AS MATRIX SYMMETRIC, COMPUTE ONLY UPPER PART) NOT YET
  !
  DO K=1,KNIN
    IEFF = KTONUM(K) + IDIAG
    IKP2 = KPM2(K,+2)
    IKP1 = KPM2(K,+1)
    IKM1 = KPM2(K,-1)
    IKM2 = KPM2(K,-2)
    !     A(K,K-2)
    JKM2 = KTONUM(IKM2)
    ! PAMAT(IEFF-JKM2,JKM2) =FAKKM2(fun_FTAUK(IKM1),WOHK(IKM1),WOHK(IKM2))
    PAMAT(IEFF-JKM2,JKM2) =FAKKM2(PSIG(IKM1),WOHK(IKM1),WOHK(IKM2))
    !     A(K,K-1)
    JKM1 = KTONUM(IKM1)
    PAMAT(IEFF-JKM1,JKM1) = FAKKM1(PSIG(K),PSIG(IKM1),WHK(IKM1), &
      &    WOHK(K),WOHK(IKM1),WOHK(IKM2))
    !     A(K,K)
    JK = KTONUM(K)
    PAMAT(IEFF-JK,JK) =FAKK(PSIG(IKP1),PSIG(K),PSIG(IKM1),WHK(K), &
      &    WHK(IKM1),WOHK(K),WOHK(IKM1))
    !     A(K,K+1)
    JKP1 = KTONUM(IKP1)
    PAMAT(IEFF-JKP1,JKP1) = FAKKP1(PSIG(IKP1),PSIG(K),WHK(K), &
      &    WOHK(IKP1),WOHK(K),WOHK(IKM1))
    !     A(K,K+2)
    JKP2 = KTONUM(IKP2)
    PAMAT(IEFF-JKP2,JKP2) = FAKKP2(PSIG(IKP1),WOHK(IKP1),WOHK(K))
    !     B(K)
    PYINPP(KTONUM(K)) = FRHS(PYIN(IKP1),PYIN(K),PYIN(IKM1),WOHK(K) &
      &    ,WOHK(IKM1))
    !
  END DO
  !
  !     2. SOLVE SYSTEM
  !
  !     USE INTEGER WORK SPACE FROM KPM2(0) ARRAY FOR IPIVOT,
  !     AS KPM2)K,0) NOT NEEDED NOR USED
  !
  IRHS = 1
  INFO2 = 0
  !   Single precision (using routines in spgbtrf_s.f compiled with -r8 for example)
  !%single      CALL SGBTRF(N,KNIN,IDOWN,IUP,PAMAT,IDAMAT,KPM2(1,0),INFO2)
  !   Double precision (using the lapack libraries from the compiler)
  CALL DGBTRF(KNIN,KNIN,IDOWN,IUP,PAMAT,IDAMAT,KPM2(1,0),INFO2)
  IF (INFO2 .EQ. 0) THEN
    !   single precision with S as first character
    !   double precision with D as first character
    CALL DGBTRS('N',KNIN,IDOWN,IUP,IRHS,PAMAT,IDAMAT,KPM2(1,0),PYINPP, &
      &    KNIN,INFO2)
  ELSE
    PRINT *,' ERROR IN SP/GBTRF: INFO = ',INFO2
    STOP 'INFO'
  ENDIF
  !
  !%nonsym
!!$      DO I=1,IBAND*KNIN
!!$        ANONSYM(I) = 0.0_RKIND
!!$      ENDDO
!!$      DO I=1,KNIN
!!$!     UPPER PART
!!$        DO J=max(1,i),min(i+IUP,n)
!!$          ANONSYM((I-1)*IBAND+J-I+IUP+1) = PAMAT(IDIAG+I-J,J)
!!$        ENDDO
!!$!     LOWER PART
!!$        DO J=max(1,i-IUP),min(i-1,n-1)
!!$          ANONSYM((I-1)*IBAND+J-I+IUP+1) = PAMAT(IDIAG+J-I,I)
!!$        ENDDO
!!$      ENDDO
!!$      CALL NONSYM(ANONSYM,PAMAT,PYINPP,KNIN,IUP,IUP,1.0E-06_RKIND,INFO2)
  !%nonsym
  !
  !     2.2 COPY SOLUTION BACK TO STANDARD NUMBERING, USE PAMAT AS WORK SPACE
  !
  DO K=1,KNIN
    PAMAT(1,K) = PYINPP(KTONUM(K))
  END DO
  DO K=1,KNIN
    PYINPP(K) = PAMAT(1,K)
  END DO
  !
  !     3. COMPUTE NEW VALUES OF Y_K (NON-STANDARD CUBIC SPLINE ONLY)
  !
  IF (ICUBSTD .EQ. 0) THEN
    !
    DO K=1,KNIN
      IKP1 = KPM2(K,+1)
      IKM1 = KPM2(K,-1)
      PYINNEW(K) = PYIN(K) &
        &      - PSIG(K) * ((PYINPP(IKP1)-PYINPP(K))*WOHK(K) &
        &      - (PYINPP(K)-PYINPP(IKM1))*WOHK(IKM1))
    END DO
    !
  ENDIF

  IF (INFO2 .LT. 0) THEN
    PRINT *,' ERROR IN SP/GBTRS: INFO2 = ',INFO2
  ENDIF
  !
  DEALLOCATE(PAMAT)
  !
  RETURN
END SUBROUTINE CBFITPER
SUBROUTINE SPLIPERA(PXIN,PYIN,PYINPP,KNIN,PXOUT,KNOUT,PY,PYP,PYPP,PYINT,PERIOD,KOPT)
  USE prec_rkind
  implicit none
  REAL(RKIND) :: PXIN(KNIN),PYIN(KNIN),PYINPP(KNIN)
  REAL(RKIND) :: XA(KNIN+1),YA(KNIN+1),Y2A(KNIN+1), YINT(KNIN+1)
  REAL(RKIND) :: PXOUT(KNOUT), PY(KNOUT), PYP(KNOUT), PYPP(KNOUT), PYINT(KNOUT), PERIOD
  INTEGER :: KNIN, KNOUT, IIN_XOUT(KNOUT), IPER(KNOUT), KOPT
  !
  REAL(RKIND) :: ZXSHIFT(KNOUT), XAKLO, XAKHI, YAKLO, YAKHI, Y2AKLO, Y2AKHI, H, A, B, A2, B2
  INTEGER :: KLO, KHI, K, J, I, IPREVIOUS
  !
  !   with periodic boundary conditions, SET
  !   XA(N+1) = PXIN(1) + PERIOD
  !   YA(N+1) = PYIN(1)
  !   Y2A(N+1) = PYINPP(1)
  !
  !   Calculates Y, Yprime, Yprimeprime, int(y)
  !
  !-----------------------------------------------------------------------
  ! COMPUTE IIN_XOUT,IPER,ZXSHIFT TO LOCATE PXOUT WITHIN PXIN
  !
  !  CALL FINDINDICESPER(PXIN,KNIN,PXOUT,KNOUT,IIN_XOUT,IPER,ZXSHIFT,PERIOD)
  XA(1:KNIN) = PXIN
  XA(KNIN+1) = PXIN(1) + PERIOD
  YA(1:KNIN) = PYIN
  YA(KNIN+1) = PYIN(1)
  Y2A(1:KNIN) = PYINPP
  Y2A(KNIN+1) = PYINPP(1)
  IF (KOPT .GE. 3) THEN
    ! PREPARE INTEGRAL UP TO XIN(I) TO ADD UP TO LOCAL VALUE
    ! SINCE IT TAKES SOME EXTRA TIME DO IT ONLY IF REQUIRED
    YINT(1) = 0._RKIND
    DO I=2,KNIN+1
      H = XA(I) - XA(I-1)
      YINT(I) = YINT(I-1) +  H/2._RKIND*(YA(I-1) + YA(I) &
        & - H**2/12._RKIND*(Y2A(I-1)+Y2A(I)))
    END DO
  END IF
  !
  IPREVIOUS = 1
  DO J=1,KNOUT
    !     SHIFT PXOUT POINTS WITHIN [PXIN(1),PXIN(KNIN)] WITH IPER * PERIOD
    IF (PXOUT(J) .LT. PXIN(1)) THEN
      IPER(J) = INT((PXIN(1)-PXOUT(J))/PERIOD) + 1
      ZXSHIFT(J) = PXOUT(J) + IPER(J)*PERIOD
    ELSE IF (PXOUT(J) .GT. PXIN(1)+PERIOD) THEN
      IPER(J) = - (INT((PXOUT(J)-PXIN(1)-PERIOD)/PERIOD) + 1)
      ZXSHIFT(J) = PXOUT(J) + IPER(J)*PERIOD
    ELSE
      IPER(J) = 0
      ZXSHIFT(J) = PXOUT(J)
    ENDIF
    ! FIND INTERVAL IN PXIN WHERE PXOUT+-IPER(J)*PERIOD BELONGS, START FROM PREVIOUS INTERVAL, EXCEPT IF THERE WAS A JUMP BACK
    IF (ZXSHIFT(J) .LT. PXIN(IPREVIOUS)) IPREVIOUS = 1
    DO I=IPREVIOUS,KNIN
      IF ((ZXSHIFT(J) .GE. XA(I)) .AND. (ZXSHIFT(J) .LE. XA(I+1))) THEN
        IIN_XOUT(J) = I
        IPREVIOUS = I
        EXIT
      END IF
    END DO
  END DO
  !
  !
  DO J=1,KNOUT
    H = XA(IIN_XOUT(J)+1) - XA(IIN_XOUT(J))
    YAKLO = YA(IIN_XOUT(J))
    YAKHI = YA(IIN_XOUT(J)+1)
    Y2AKLO = Y2A(IIN_XOUT(J))
    Y2AKHI = Y2A(IIN_XOUT(J)+1)
    IF (H.EQ.0._RKIND) STOP 'BAD XA INPUT.'
    A=(XA(IIN_XOUT(J)+1) - ZXSHIFT(J)) / H
    B=1._RKIND - A
    A2=A**2
    B2=B**2
    PY(J)=A*YAKLO+B*YAKHI+ &
      &      (A*(A2-1._RKIND)*Y2AKLO+B*(B2-1._RKIND)*Y2AKHI)*(H**2)/6._RKIND
    IF (KOPT .GE. 1) PYP(J)=(YAKHI-YAKLO)/H - &
      &  ( (3._RKIND*A2-1._RKIND)*Y2AKLO - (3._RKIND*B2-1._RKIND)*Y2AKHI )*H/6._RKIND
    IF (KOPT .GE. 2) PYPP(J)=A*Y2AKLO+B*Y2AKHI
    IF (KOPT .GE. 3) PYINT(J)= YINT(IIN_XOUT(J)) + H/2._RKIND*(YAKLO*(1._RKIND-A2) + YAKHI*B2 &
      &  - (Y2AKLO*(1._RKIND-A2)**2 + Y2AKHI*B2*(2._RKIND-B2))  * H**2/12._RKIND) - IPER(J)*YINT(KNIN+1)
  END DO
  !
  RETURN
END SUBROUTINE SPLIPERA
!-----------------------------------------------------------------------
SUBROUTINE SPLIBNDA(PXIN,PYIN,PYINPP,KNIN,PXOUT,PY,PYP,PYPP,PYINT,KNOUT,KOPTXPOL,KOPTDER)
  USE prec_rkind
  implicit none
  REAL(RKIND) :: zone, ztwo, zthree, zfour, zsix
  PARAMETER(ZONE=1._RKIND, ZTWO=2._RKIND, ZTHREE=3._RKIND, ZFOUR=4._RKIND, ZSIX=6._RKIND)
  REAL(RKIND) :: PXIN(KNIN),PYIN(KNIN),PYINPP(KNIN)
  REAL(RKIND) :: PXOUT(KNOUT), PY(KNOUT), PYP(KNOUT), PYPP(KNOUT), PYINT(KNOUT)
  integer KNIN, KNOUT, KOPTXPOL, KOPTDER
  !
  !     KOPTDER = 0: COMPUTE ONLY FUNCTION PY POINTS PXOUT
  !     KOPTDER = 1: COMPUTE ALSO 1ST DERIVATIVE PYP
  !     KOPTDER = 2: COMPUTE ALSO 2ND DERIVATIVE PYPP
  !     KOPTDER = 3: COMPUTE ALSO INTEGRAL OF Y FROM XIN(1) TO XOUT(J) IN PYINT(J)
  !
  !   ABS(KOPTXPOL):
  !     KOPTXPOL = 0: STOP WITH ERROR MESSAGE IF OUT OF BOUND
  !     KOPTXPOL = 1: LINEAR EXTRAPOLATION: +1 FROM YEDGE,YPEDGE ; -1: FROM Y(LAST TWO POINTS)
  !     KOPTXPOL = 10: CONSTANT EXTRAPOLATION: +10: Y=YEDGE, -10: Y=0. OUTSIDE PXIN INTERVAL
  !     KOPTXPOL = 2: USE QUADRATIC EXTRAPOLATION IF X OUT OF BOUND
  !     KOPTXPOL = 3: USE CUBIC EXTRAPOLATION IF X OUT OF BOUND
  !     KOPTXPOL = 21: USE QUADRATIC WITHIN ALFA*DELTA_X AND LINEAR FURTHER
  !     KOPTXPOL = 31: USE CUBIC WITHIN ALFA*DELTA_X AND LINEAR    FURTHER
  !     KOPTXPOL = 32: USE CUBIC WITHIN ALFA*DELTA_X AND QUADRATIC FURTHER
  !
  !     KOPTXPOL > 0: VALUE AND 1ST DER. CONTINUOUS AT END OF INTERVAL, THUS
  !     .             USES CUBIC SPLINE OF LAST INTERVAL TO CONTINUE
  !     KOPTXPOL < 0: ONLY Y VALUE CONTINUOUS AND USES VALUES AT LAST BUT ONE,
  !     .             TWO, THREE POINTS TO EXTRAPOLATE (BETTER IF DER. AT EDGE
  !     .             IS WILD)
  !
  !-----------------------------------------------------------------------
  ! LOCAL VARIABLES:
  REAL(RKIND) :: ALFA
  PARAMETER(ALFA = 1._RKIND)
  REAL(RKIND) :: H, H2, A, B,  &
    & ZX1, ZY1, ZY1P, zx2, zy2,  &
    & ZXN, ZYN, ZYNP, ZXNN, ZYNN, &
    & ZXLFTDEL, ZYLFTDEL, ZYPLFTDEL, ZYINTLFTDL, &
    & ZXRGTDEL, ZYRGTDEL, ZYPRGTDEL, ZYINTRGTDL, &
    & ZYINT, ZYINT_XIN(KNIN), ZYIN_XPOL
  INTEGER ICONTDER
  INTEGER :: J1ST_XIN(KNIN),JLAST_XIN(KNIN), JLEFT(KNOUT), JRIGHT(KNOUT), &
    & JLEFT_DEL(KNOUT), JRIGHT_DEL(KNOUT), IOPTXPOL, I, J, K, KLO, KHI
  !
  ! VARIABLES RELATED TO FUNCTIONS:
  REAL(RKIND) :: FC3, X1, F1, P1, X2, PP1, PP2, HH, HPX1, &
       &  F2, P2, FC2, FC1, FC0, FQQQ0, FQQQ1, FQQQ2, &
       &  FLINEAR, FLINEARP, FCCCC0, FCCCC1, FCCCC2, FCCCC3, FQDQ0, FQDQ1, &
       &  FQDQ2, FCDCD0, FCDCD1, FCDCD2, FCDCD3, FB1, &
       &  FB2, FA2, FA3, FD2, FD1, &
       &  FCCCCM1, FCDCDM1, FQQQM1, FQDQM1, FLINEARM1, FLINXYP, FLINXYPM1, &
       &  FC0D, FC1C, FC2B, FC3A, &
       &  F2D3H, F2D2H, F2D1H, F2D0H,FC2DC2DH0,  FC2DC2DH1, FC2DC2DH2, FC2DC2DH3, FC2DC2DHM1, &
       &  FD0H, FD1H, FD2H, FD3H, FCDCDH0, FCDCDH1, FCDCDH2, FCDCDH3, FCDCDHM1, &
       &  FCH0, FCH1, FCH2, FCH3, FCCCCH0, FCCCCH1, X3, X4, F3, F4, H21, H31, H41, &
       &  FCCCCH2, FCCCCH3, FCCCCHM1, &
       &  FQH0, FQH1, FQH2, FQH3, FQQQH0, FQQQH1, FQQQH2, FQQQHM1, &
       &  FQDH0, FQDH1, FQDH2, FQDQH0, FQDQH1, FQDQH2, FQDQHM1, &
       &  FQ2DH0, FQ2DH1, FQ2DH2, FQ2DQH0, FQ2DQH1, FQ2DQH2, FQ2DQHM1
  REAL(RKIND) :: A1, A2, A3, A4, B1, B2, B3, B4, PX
  REAL(RKIND) :: FB0, FD0, FA0, FA1
  !
  ! REQUIRES VARIABLES ZTWO, ZTHREE, ZFOUR AND ZSIX
  ! (DEFINED AS 2._RKIND ETC TYPICALLY BUT IS USER DEPENDENT FOR PRECISION)
  !
  !.......................................................................
  !*COMDECK CUCCCC
  ! ----------------------------------------------------------------------
  ! --     STATEMENT FUNCTION FOR CUBIC INTERPOLATION                   --
  ! --                         23.04.88            AR        CRPP       --
  ! --                                                                  --
  ! -- CUBIC INTERPOLATION OF A FUNCTION F(X)                           --
  ! -- THE EIGHT ARGUMENTS A1,A2,A3,A4,B1,B2,B3,B4 ARE DEFINED BY:      --
  ! -- F(B1) = A1 , F(B2) = A2 , F(B3) = A3 , F(B4) = A4                --
  ! ----------------------------------------------------------------------
  !
  FA3(A1,A2,A3,A4,B1,B2,B3,B4) = &
    &        (A1-A2) / ((B1-B2)*(B2-B4)*(B2-B3)) + &
    &        (A1-A3) / ((B4-B3)*(B3-B1)*(B3-B2)) + &
    &        (A1-A4) / ((B1-B4)*(B2-B4)*(B3-B4))
  FA2(A1,A2,A3,A4,B1,B2,B3,B4) = &
    &        (A1-A2) / ((B2-B1)*(B3-B2)) + &
    &        (A3-A1) / ((B3-B1)*(B3-B2)) - &
    &        (B1+B2+B3) * FA3(A1,A2,A3,A4,B1,B2,B3,B4)
  FA1(A1,A2,A3,A4,B1,B2,B3,B4) = &
    &        (A1-A2) / (B1-B2) - &
    &        (B1+B2) * FA2(A1,A2,A3,A4,B1,B2,B3,B4) - &
    &        (B1*B1+B1*B2+B2*B2) * FA3(A1,A2,A3,A4,B1,B2,B3,B4)
  FA0(A1,A2,A3,A4,B1,B2,B3,B4) = &
    &        A1 - &
    &        B1 * (FA1(A1,A2,A3,A4,B1,B2,B3,B4) + &
    &              B1 * (FA2(A1,A2,A3,A4,B1,B2,B3,B4) + &
    &                    B1 * FA3(A1,A2,A3,A4,B1,B2,B3,B4)))
  ! ----------------------------------------------------------------------
  ! -- FCCCC0 GIVES THE VALUE OF THE FUNCTION AT POINT PX:              --
  ! -- FCCCC0(......,PX) = F(PX)                                        --
  ! ----------------------------------------------------------------------
  FCCCC0(A1,A2,A3,A4,B1,B2,B3,B4,PX) = &
    &              FA0(A1,A2,A3,A4,B1,B2,B3,B4) + &
    &              PX * (FA1(A1,A2,A3,A4,B1,B2,B3,B4) + &
    &                    PX * (FA2(A1,A2,A3,A4,B1,B2,B3,B4) + &
    &                          PX * FA3(A1,A2,A3,A4,B1,B2,B3,B4)))
  ! ----------------------------------------------------------------------
  ! -- FCCCC1 GIVES THE VALUE OF THE FIRST DERIVATIVE OF F(X) AT PX:    --
  ! -- FCCCC1(......,PX) = DF/DX (PX)                                   --
  ! ----------------------------------------------------------------------
  FCCCC1(A1,A2,A3,A4,B1,B2,B3,B4,PX) = &
    &              FA1(A1,A2,A3,A4,B1,B2,B3,B4) + &
    &              PX * (ZTWO * FA2(A1,A2,A3,A4,B1,B2,B3,B4) + &
    &                    ZTHREE * PX * FA3(A1,A2,A3,A4,B1,B2,B3,B4))
  ! ----------------------------------------------------------------------
  ! -- FCCCC2 GIVES THE VALUE OF THE SECOND DERIVATIVE OF F(X) AT PX:   --
  ! -- FCCCC2(......,PX) = D2F/DX2 (PX)                                 --
  ! ----------------------------------------------------------------------
  FCCCC2(A1,A2,A3,A4,B1,B2,B3,B4,PX) = &
    &             ZTWO * FA2(A1,A2,A3,A4,B1,B2,B3,B4) + &
    &             ZSIX * FA3(A1,A2,A3,A4,B1,B2,B3,B4) * PX
  ! ----------------------------------------------------------------------
  ! -- FCCCC3 GIVES THE VALUE OF THE THIRD DERIVATIVE OF F(X) AT PX:     -
  ! -- FCCCC3(......,PX) = D3F/DX3 (PX)                                  -
  ! ----------------------------------------------------------------------
  FCCCC3(A1,A2,A3,A4,B1,B2,B3,B4,PX) = &
    &                      ZSIX * FA3(A1,A2,A3,A4,B1,B2,B3,B4)
  ! ----------------------------------------------------------------------
  ! -- FCCCCM1 GIVES THE VALUE OF THE INTEGRAL OF F(X) FROM B1 TO PX:     -
  FCCCCM1(A1,A2,A3,A4,B1,B2,B3,B4,PX) = &
    &  (PX-B1)*(FA0(A1,A2,A3,A4,B1,B2,B3,B4) + &
    &  (PX+B1)*FA1(A1,A2,A3,A4,B1,B2,B3,B4)/ZTWO + &
    &  FA2(A1,A2,A3,A4,B1,B2,B3,B4)/ZTHREE*(PX*(PX+B1)+B1*B1) + &
    &  (PX+B1)*(PX*PX+B1*B1)*FA3(A1,A2,A3,A4,B1,B2,B3,B4)/ZFOUR)
  !-----------------------------------------------------------------------
  !.......................................................................
  !*COMDECK CUCDCD
  ! ----------------------------------------------------------------------
  ! --     STATEMENT FUNCTION FOR CUBIC INTERPOLATION                   --
  ! --                         19.01.87            AR        CRPP       --
  ! --                                                                  --
  ! -- CUBIC INTERPOLATION OF A FUNCTION F(X)                           --
  ! -- THE SIX ARGUMENTS X1,F1,P1,X2,F2,P2 ARE DEFINED AS FOLLOWS:      --
  ! -- F(X1) = F1 , F(X2) = F2 , DF/DX(X1) = P1 , DF/DX(X2) = P2        --
  ! ----------------------------------------------------------------------
  !
  FC3(X1,F1,P1,X2,F2,P2) = &
    &      (ZTWO * (F2 - F1) / (X1 - X2) + (P1 + P2)) / &
    &      ((X1 - X2) * (X1 - X2))
  FC2(X1,F1,P1,X2,F2,P2) = &
    &      (ZTHREE * (X1 + X2) * (F1 - F2) / (X1 - X2) - &
    &       P1 * (X1 + ZTWO * X2) - P2 * (X2 + ZTWO * X1)) / &
    &      ((X1 - X2) * (X1 - X2))
  FC1(X1,F1,P1,X2,F2,P2) = &
    &      (P1 + P2 - ZTWO*FC2(X1,F1,P1,X2,F2,P2)*(X1+X2) - &
    & ZTHREE*FC3(X1,F1,P1,X2,F2,P2)*(X1*X1+X2*X2))/ZTWO
  FC0(X1,F1,P1,X2,F2,P2) = &
    &      (F1 + F2 - FC1(X1,F1,P1,X2,F2,P2)*(X1+X2) - &
    & FC2(X1,F1,P1,X2,F2,P2)*(X1*X1+X2*X2) - &
    & FC3(X1,F1,P1,X2,F2,P2)*(X1**3+X2**3))/ZTWO
  ! ----------------------------------------------------------------------
  ! -- FCDCD0 GIVES THE VALUE OF THE FUNCTION AT POINT PX               --
  ! -- FCDCD0(......,PX) = F(PX)                                        --
  ! ----------------------------------------------------------------------
  FCDCD0(X1,F1,P1,X2,F2,P2,PX) = &
    &              FC0(X1,F1,P1,X2,F2,P2) + &
    &              PX * (FC1(X1,F1,P1,X2,F2,P2) + &
    &                    PX * (FC2(X1,F1,P1,X2,F2,P2) + &
    &                          PX * FC3(X1,F1,P1,X2,F2,P2)))
  ! ----------------------------------------------------------------------
  ! -- FCDCD1 GIVES THE VALUE OF THE FIRST DERIVATIVE OF F(X) AT PX:    --
  ! -- FCDCD1(......,PX) = DF/DX (PX)                                   --
  ! ----------------------------------------------------------------------
  FCDCD1(X1,F1,P1,X2,F2,P2,PX) = &
    &              FC1(X1,F1,P1,X2,F2,P2) + &
    &              PX * (ZTWO * FC2(X1,F1,P1,X2,F2,P2) + &
    &                    ZTHREE * PX * FC3(X1,F1,P1,X2,F2,P2))
  ! ----------------------------------------------------------------------
  ! -- FCDCD2 GIVES THE VALUE OF THE SECOND DERIVATIVE OF F(X) AT PX:   --
  ! -- FCDCD2(......,PX) = D2F/DX2 (PX)                                 --
  ! ----------------------------------------------------------------------
  FCDCD2(X1,F1,P1,X2,F2,P2,PX) = &
    &             ZTWO * FC2(X1,F1,P1,X2,F2,P2) + &
    &             ZSIX * FC3(X1,F1,P1,X2,F2,P2) * PX
  ! ----------------------------------------------------------------------
  ! -- FCDCD3 GIVES THE VALUE OF THE THIRD DERIVATIVE OF F(X) AT PX:    --
  ! -- FCDCD3(......,PX) = D3F/DX3 (PX)                                 --
  ! ----------------------------------------------------------------------
  FCDCD3(X1,F1,P1,X2,F2,P2,PX) = &
    &                      ZSIX * FC3(X1,F1,P1,X2,F2,P2)
  ! ----------------------------------------------------------------------
  ! -- FCDCDM1 GIVES THE VALUE OF THE INTEGRAL OF F(X) FROM X1 TO PX:
  FCDCDM1(X1,F1,P1,X2,F2,P2,PX) = &
    &  (PX-X1)*(FC0(X1,F1,P1,X2,F2,P2) + &
    &    (PX+X1) * FC1(X1,F1,P1,X2,F2,P2)/ZTWO + &
    &    (PX*(PX+X1)+X1*X1) * FC2(X1,F1,P1,X2,F2,P2)/ZTHREE + &
    &    (PX+X1)*(PX*PX+X1*X1) * FC3(X1,F1,P1,X2,F2,P2)/ZFOUR)
  !-----------------------------------------------------------------------
  !.......................................................................
  !*COMDECK CUC2DC2DH
  ! ----------------------------------------------------------------------
  ! --     STATEMENT FUNCTION FOR CUBIC INTERPOLATION                   --
  ! --                         10.10.2014  OS CRPP                      --
  ! --                                                                  --
  ! -- CUBIC INTERPOLATION OF A FUNCTION F(X-X(i))                           --
  ! -- THE SEVEN ARGUMENTS X1,F1,PP1,X2,F2,PP2,HH ARE DEFINED AS FOLLOWS: 
  ! -- F(X1) = F1 , F(X2) = F2 , D2F/DX2(X1) = PP1 , DF2/D2X(X2) = PP2, HH = X2-X1
  ! -- WRITTEN FROM F(X)=A (X-XI)^3 + B (X-XI)^2 + C (X-XI)^2 + D
  ! -- (BETTER WHEN X(I+1)-X(I) SMALL)
  ! ----------------------------------------------------------------------
  ! ADD "H" FOR "DELTA X" INSTEAD OF F(X)
  F2D3H(X1,F1,PP1,X2,F2,PP2,HH) = (PP2-PP1) / HH / ZSIX
  F2D2H(X1,F1,PP1,X2,F2,PP2,HH) = PP1 / ZTWO
  F2D1H(X1,F1,PP1,X2,F2,PP2,HH) = (F2-F1)/HH - HH/ZSIX*(ZTWO*PP1+PP2)
  F2D0H(X1,F1,PP1,X2,F2,PP2,HH) = F1
  ! ----------------------------------------------------------------------
  ! -- FC2DC2DH0 GIVES THE VALUE OF THE FUNCTION AT POINT PX with HPX1=PX-X1 --
  ! -- FC2DC2DH0(......,HPX1) = F(PX)                                        --
  ! ----------------------------------------------------------------------
  FC2DC2DH0(X1,F1,PP1,X2,F2,PP2,HH,HPX1) = &
       &           F2D0H(X1,F1,PP1,X2,F2,PP2,HH) + &
       &             HPX1 * (F2D1H(X1,F1,PP1,X2,F2,PP2,HH) + &
       &               HPX1 * (F2D2H(X1,F1,PP1,X2,F2,PP2,HH) + &
       &                 HPX1 * F2D3H(X1,F1,PP1,X2,F2,PP2,HH)))
  ! ----------------------------------------------------------------------
  ! -- FC2DC2DH1 GIVES THE VALUE OF THE 1st derivative AT POINT PX with HPX1=PX-X1
  ! -- FC2DC2DH1(......,HPX1) = dF(PX)/dx , similar for 2nd, 3RD derivative
  ! ----------------------------------------------------------------------
  FC2DC2DH1(X1,F1,PP1,X2,F2,PP2,HH,HPX1) = &
       &           (F2D1H(X1,F1,PP1,X2,F2,PP2,HH) + &
       &              HPX1 * (ZTWO*F2D2H(X1,F1,PP1,X2,F2,PP2,HH) + &
       &                HPX1 * ZTHREE*F2D3H(X1,F1,PP1,X2,F2,PP2,HH)))
  FC2DC2DH2(X1,F1,PP1,X2,F2,PP2,HH,HPX1) = &
       &           ZTWO * F2D2H(X1,F1,PP1,X2,F2,PP2,HH) + &
       &             ZSIX * HPX1 * F2D3H(X1,F1,PP1,X2,F2,PP2,HH)
  FC2DC2DH3(X1,F1,PP1,X2,F2,PP2,HH,HPX1) = &
       &             ZSIX * F2D3H(X1,F1,PP1,X2,F2,PP2,HH)
  ! Integral from X1 to PX
  FC2DC2DHM1(X1,F1,PP1,X2,F2,PP2,HH,HPX1) = &
       &           HPX1 * (F2D0H(X1,F1,PP1,X2,F2,PP2,HH) + &
       &             HPX1 * (F2D1H(X1,F1,PP1,X2,F2,PP2,HH)/ZTWO + &
       &               HPX1 * (F2D2H(X1,F1,PP1,X2,F2,PP2,HH)/ZTHREE + &
       &                 HPX1 * F2D3H(X1,F1,PP1,X2,F2,PP2,HH)/ZFOUR)))
  !-----------------------------------------------------------------------
  !.......................................................................
  !*COMDECK CUCDCDH
  ! ----------------------------------------------------------------------
  ! --     STATEMENT FUNCTION FOR CUBIC INTERPOLATION                   --
  ! --                         10.10.2014  OS CRPP                      --
  ! --                                                                  --
  ! -- CUBIC INTERPOLATION OF A FUNCTION F(X-X(i))                           --
  ! -- THE ARGUMENTS X1,F1,P1,X2,F2,P2,HH ARE DEFINED AS FOLLOWS: 
  ! -- F(X1) = F1 , F(X2) = F2 , DF/DX(X1) = P1 , DF/DX(X2) = P2, HH = X2-X1
  ! -- WRITTEN FROM F(X)=A (X-XI)^3 + B (X-XI)^2 + C (X-XI)^2 + D
  ! -- (BETTER WHEN X(I+1)-X(I) SMALL)
  ! ----------------------------------------------------------------------
  FD0H(X1,F1,P1,X2,F2,P2,HH) = F1
  FD1H(X1,F1,P1,X2,F2,P2,HH) = P1
  FD2H(X1,F1,P1,X2,F2,P2,HH) = (ZTHREE*(F2-F1) - HH*(P2+ZTWO*P1))/HH**2
  ! FD3H(X1,F1,P1,X2,F2,P2,HH) = (ZTWO*(F1-F2) + HH*(P1+P2))/HH**3
  FD3H(X1,F1,P1,X2,F2,P2,HH) = (P2-P1-ZTWO*FD2H(X1,F1,P1,X2,F2,P2,HH)*HH)/(HH*HH*ZTHREE)
  ! ----------------------------------------------------------------------
  ! -- FCDCDH0 GIVES THE VALUE OF THE FUNCTION AT POINT PX with HPX1=PX-X1 --
  ! -- FCDCDH0(......,HPX1) = F(PX)                                        --
  ! ----------------------------------------------------------------------
  FCDCDH0(X1,F1,P1,X2,F2,P2,HH,HPX1) = &
       &           FD0H(X1,F1,P1,X2,F2,P2,HH) + &
       &             HPX1 * (FD1H(X1,F1,P1,X2,F2,P2,HH) + &
       &               HPX1 * (FD2H(X1,F1,P1,X2,F2,P2,HH) + &
       &                 HPX1 * FD3H(X1,F1,P1,X2,F2,P2,HH)))
  ! ----------------------------------------------------------------------
  ! -- FCDCDH1 GIVES THE VALUE OF THE 1st derivative AT POINT PX with HPX1=PX-X1
  ! -- FCDCDH1(......,HPX1) = dF(PX)/dx , similar for 2nd, 3rd derivatives
  ! ----------------------------------------------------------------------
  FCDCDH1(X1,F1,P1,X2,F2,P2,HH,HPX1) = &
       &           (FD1H(X1,F1,P1,X2,F2,P2,HH) + &
       &              HPX1 * (ZTWO * FD2H(X1,F1,P1,X2,F2,P2,HH) + &
       &                HPX1 * ZTHREE * FD3H(X1,F1,P1,X2,F2,P2,HH)))
  FCDCDH2(X1,F1,P1,X2,F2,P2,HH,HPX1) = &
       &           ZTWO * FD2H(X1,F1,P1,X2,F2,P2,HH) + &
       &             ZSIX * HPX1 * FD3H(X1,F1,P1,X2,F2,P2,HH)
  FCDCDH3(X1,F1,P1,X2,F2,P2,HH,HPX1) = &
       &             ZSIX * FD3H(X1,F1,P1,X2,F2,P2,HH)
  ! Integral from X1 to PX
  FCDCDHM1(X1,F1,P1,X2,F2,P2,HH,HPX1) = &
       &           HPX1 * (FD0H(X1,F1,P1,X2,F2,P2,HH) + &
       &             HPX1 * (FD1H(X1,F1,P1,X2,F2,P2,HH)/ZTWO + &
       &               HPX1 * (FD2H(X1,F1,P1,X2,F2,P2,HH)/ZTHREE + &
       &                 HPX1 * FD3H(X1,F1,P1,X2,F2,P2,HH)/ZFOUR)))
  !-----------------------------------------------------------------------
  !.......................................................................
  !*COMDECK CUCCCCH
  ! ----------------------------------------------------------------------
  ! --     STATEMENT FUNCTION FOR CUBIC INTERPOLATION                   --
  ! --                         10.10.2014  OS CRPP                      --
  ! --                                                                  --
  ! -- CUBIC INTERPOLATION OF A FUNCTION F(PX)                           --
  ! -- THE ARGUMENTS X1,F1,X2,F2,X3,F3,X4,F4,H21,H31,H41 ARE DEFINED AS FOLLOWS: 
  ! -- F(X1) = F1 , F(X2) = F2 , F(X3) = F3 , F(X4) = F4, 
  ! -- H21 = X2-X1, H31 = X3-X1, H41 = X4-X1, HPX1=PX-X1
  ! -- Written from f(x)=a (x-x1)^3 + b (x-x1)^2 + c (x-x1)^2 + d
  ! -- (better when x(i+1)-x(i) small)
  ! ----------------------------------------------------------------------
  !
  FCH0(X1,F1,X2,F2,X3,F3,X4,F4,H21,H31,H41) = F1
  FCH1(X1,F1,X2,F2,X3,F3,X4,F4,H21,H31,H41) = &
       & (H41**2*(H41-H21)*(H31**3*(F2-F1)-H21**3*(F3-F1))-H31**2*(H31-H21)*(H41**3*(F2-F1)-H21**3*(F4-F1))) / &
       & (H41*(H41-H21)*(H31**2-H21**2)-H31*(H41**2-H21**2)*(H31-H21))/H21/H31/H41
!!$  FCH2(X1,F1,X2,F2,X3,F3,X4,F4,H21,H31,H41) = (H31**3*(F2-F1)-H21**3*(F3-F1)-(H31**3-H21**2*H31)* &
!!$       & FCH1(X1,F1,X2,F2,X3,F3,X4,F4,H21,H31,H41)*H21) / H21**2 / (H31-H21) / H31**2
  FCH2(X1,F1,X2,F2,X3,F3,X4,F4,H21,H31,H41) = &
       & (H31**3/H21*(F2-F1)-H21**2*(F3-F1) + H41**3/H21*(F2-F1)-H21**2*(F4-F1) &
       & - (H31*(H31**2-H21**2)+H41*(H41**2-H21**2))*FCH1(X1,F1,X2,F2,X3,F3,X4,F4,H21,H31,H41)) &
       & / H21 / (H31**2*(H31-H21)+H41**2*(H41-H21))
  FCH3(X1,F1,X2,F2,X3,F3,X4,F4,H21,H31,H41) = &
       & (F2+F3+F4-ZTHREE*F1-(H21**2+H31**2+H41**2)*FCH2(X1,F1,X2,F2,X3,F3,X4,F4,H21,H31,H41) &
       & - (H21+H31+H41)*FCH1(X1,F1,X2,F2,X3,F3,X4,F4,H21,H31,H41))/(H21**3+H31**3+H41**3)
  ! ----------------------------------------------------------------------
  ! -- FCCCCH0 GIVES THE VALUE OF THE FUNCTION AT POINT PX with HPX1=PX-X1 --
  ! -- FCCCCH0(......,HPX1) = F(PX)                                        --
  ! ----------------------------------------------------------------------
  FCCCCH0(X1,F1,X2,F2,X3,F3,X4,F4,H21,H31,H41,HPX1) = &
       &       FCH0(X1,F1,X2,F2,X3,F3,X4,F4,H21,H31,H41) + &
       &         HPX1 * (FCH1(X1,F1,X2,F2,X3,F3,X4,F4,H21,H31,H41) + &
       &           HPX1 * (FCH2(X1,F1,X2,F2,X3,F3,X4,F4,H21,H31,H41) + &
       &             HPX1 * FCH3(X1,F1,X2,F2,X3,F3,X4,F4,H21,H31,H41)))
  ! -- FCCCCH1(......,HPX1) = dF(PX)/dx
  FCCCCH1(X1,F1,X2,F2,X3,F3,X4,F4,H21,H31,H41,HPX1) = &
       &       FCH1(X1,F1,X2,F2,X3,F3,X4,F4,H21,H31,H41) + &
       &         HPX1 * (ZTWO*FCH2(X1,F1,X2,F2,X3,F3,X4,F4,H21,H31,H41) + &
       &           HPX1 * ZTHREE*FCH3(X1,F1,X2,F2,X3,F3,X4,F4,H21,H31,H41))
  ! -- FCCCCH2(......,HPX1) = d2F(PX)/dx^2
  FCCCCH2(X1,F1,X2,F2,X3,F3,X4,F4,H21,H31,H41,HPX1) = &
       &         ZTWO * FCH2(X1,F1,X2,F2,X3,F3,X4,F4,H21,H31,H41) + &
       &           ZSIX * HPX1 * FCH3(X1,F1,X2,F2,X3,F3,X4,F4,H21,H31,H41)
  ! -- FCCCCH3(......,HPX1) = d3F(PX)/dx^3
  FCCCCH3(X1,F1,X2,F2,X3,F3,X4,F4,H21,H31,H41,HPX1) = &
       &           ZSIX * FCH3(X1,F1,X2,F2,X3,F3,X4,F4,H21,H31,H41)
  ! -- FCCCCHM1(......,HPX1) = Integral of F(X) from X1 to PX
  FCCCCHM1(X1,F1,X2,F2,X3,F3,X4,F4,H21,H31,H41,HPX1) = &
       &       HPX1 * (FCH0(X1,F1,X2,F2,X3,F3,X4,F4,H21,H31,H41) + &
       &         HPX1 * (FCH1(X1,F1,X2,F2,X3,F3,X4,F4,H21,H31,H41)/ZTWO + &
       &           HPX1 * (FCH2(X1,F1,X2,F2,X3,F3,X4,F4,H21,H31,H41)/ZTHREE + &
       &             HPX1 * FCH3(X1,F1,X2,F2,X3,F3,X4,F4,H21,H31,H41)/ZFOUR)))
  !
  !.......................................................................
  !*COMDECK QUAQQQ
  ! ----------------------------------------------------------------------
  ! --     STATEMENT FUNCTION FOR QUADRATIC INTERPOLATION               --
  ! --                         19.01.87            AR        CRPP       --
  ! --                                                                  --
  ! -- QUADRATIC INTERPOLATION OF A FUNCTION F(X)                       --
  ! -- THE SIX PARAMETERS A1,A2,A3,B1,B2,B3 ARE DEFINED AS FOLLOWS:     --
  ! -- F(B1) = A1 , F(B2) = A2 , F(B3) = A3                             --
  ! ----------------------------------------------------------------------
  !
  FB2(A1,A2,A3,B1,B2,B3) = &
    &               ((A1-A2)/(B1-B2)-(A1-A3)/(B1-B3))/(B2-B3)
  FB1(A1,A2,A3,B1,B2,B3) = ((A1-A2)/(B1-B2))- &
    &         FB2(A1,A2,A3,B1,B2,B3)*(B1+B2)
  FB0(A1,A2,A3,B1,B2,B3) = A1-FB1(A1,A2,A3,B1,B2,B3)*B1 &
    &         -FB2(A1,A2,A3,B1,B2,B3)*B1*B1
  ! ----------------------------------------------------------------------
  ! -- FQQQ0 GIVES THE VALUE OF THE FUNCTION AT THE POINT PX            --
  ! -- FQQQ0(......,PX) = F(PX)                                         --
  ! ----------------------------------------------------------------------
  FQQQ0(A1,A2,A3,B1,B2,B3,PX) = FB0(A1,A2,A3,B1,B2,B3) + &
    &                                 PX * (FB1(A1,A2,A3,B1,B2,B3) + &
    &                                 PX * FB2(A1,A2,A3,B1,B2,B3))
  ! ----------------------------------------------------------------------
  ! -- FQQQ1 GIVES THE VALUE OF THE FIRST DERIVATIVE OF F(X) AT PX      --
  ! -- FQQQ1(......,PX) = DF/DX (PX)                                    --
  ! ----------------------------------------------------------------------
  FQQQ1(A1,A2,A3,B1,B2,B3,PX) = FB1(A1,A2,A3,B1,B2,B3) + &
    &     ZTWO * PX * FB2(A1,A2,A3,B1,B2,B3)
  ! ----------------------------------------------------------------------
  ! -- FQQQ2 GIVES THE VALUE OF THE SECOND DERIVATIVE OF F(X) AT PX     --
  ! -- FQQQ2(......,PX) = D2F/DX2 (PX)                                  --
  ! ----------------------------------------------------------------------
  FQQQ2(A1,A2,A3,B1,B2,B3) = ZTWO * FB2(A1,A2,A3,B1,B2,B3)
  ! ----------------------------------------------------------------------
  ! -- FQQQM1 GIVES THE VALUE OF THE INTEGRAL OF F(X) FROM B1 TO PX:
  !
  FQQQM1(A1,A2,A3,B1,B2,B3,PX) = &
    & (PX-B1)*(FB0(A1,A2,A3,B1,B2,B3) + &
    &  (PX+B1)*FB1(A1,A2,A3,B1,B2,B3)/ZTWO + &
    &  FB2(A1,A2,A3,B1,B2,B3)/ZTHREE*(PX*(PX+B1)+B1*B1))
  !.......................................................................
  !*COMDECK QUAQDQ
  ! ----------------------------------------------------------------------
  ! --     STATEMENT FUNCTION FOR QUADRATIC INTERPOLATION               --
  ! --                         19.01.87            AR        CRPP       --
  ! --                                                                  --
  ! -- QUADRATIC INTERPOLATION OF A FUNCTION F(X)                       --
  ! -- THE FIVE PARAMETERS X1,F1,P1,X2,F2    ARE DEFINED AS FOLLOWS:    --
  ! -- F(X1) = F1 , DF/DX(X1) = P1 , F(X2) = F2                         --
  ! ----------------------------------------------------------------------
  !
  FD2(X1,F1,P1,X2,F2) = ((F2-F1)/(X2-X1) - P1) / (X2-X1)
  FD1(X1,F1,P1,X2,F2) = P1 - ZTWO*X1*FD2(X1,F1,P1,X2,F2)
  FD0(X1,F1,P1,X2,F2) = F1 - X1*(X1*FD2(X1,F1,P1,X2,F2) + &
    &                                     FD1(X1,F1,P1,X2,F2))
  ! ----------------------------------------------------------------------
  ! -- FQDQ0 GIVES THE VALUE OF THE FUNCTION AT POINT PX                --
  ! -- FQDQ0(......,PX) = F(PX)                                         --
  ! ----------------------------------------------------------------------
  FQDQ0(X1,F1,P1,X2,F2,PX) = FD0(X1,F1,P1,X2,F2) + &
    &                              PX * (FD1(X1,F1,P1,X2,F2) + &
    &                                    PX * FD2(X1,F1,P1,X2,F2))
  ! ----------------------------------------------------------------------
  ! -- FQDQ1 GIVES THE VALUE OF THE FIRST DERIVATIVE OF F(X) AT PX      --
  ! -- FQDQ1(......,PX) = DF/DX (PX)                                    --
  ! ----------------------------------------------------------------------
  FQDQ1(X1,F1,P1,X2,F2,PX) = FD1(X1,F1,P1,X2,F2) + &
    &                              ZTWO* PX * FD2(X1,F1,P1,X2,F2)
  ! ----------------------------------------------------------------------
  ! -- FQDQ2 GIVES THE VALUE OF THE SECOND DERIVATIVE OF F(X) AT PX     --
  ! -- FQDQ2(......,PX) = D2F/DX2 (PX)                                  --
  ! ----------------------------------------------------------------------
  FQDQ2(X1,F1,P1,X2,F2) = ZTWO * FD2(X1,F1,P1,X2,F2)
  ! ----------------------------------------------------------------------
  ! -- FQDQM1 GIVES THE VALUE OF THE INTEGRAL OF F(X) FROM X1 TO PX:
  !
  FQDQM1(X1,F1,P1,X2,F2,PX) = &
    & (PX-X1)*(FD0(X1,F1,P1,X2,F2) + &
    &  (PX+X1)*FD1(X1,F1,P1,X2,F2)/ZTWO + &
    &  FD2(X1,F1,P1,X2,F2)/ZTHREE*(PX*(PX+X1)+X1*X1))
  !
  !.......................................................................
  !*COMDECK QUAQQQH
  ! ----------------------------------------------------------------------
  ! --     STATEMENT FUNCTION FOR QUADRATIC INTERPOLATION               --
  ! --                         19.01.87            AR        CRPP       --
  ! --                                                                  --
  ! -- QUADRATIC INTERPOLATION OF A FUNCTION F(X)                       --
  ! -- THE PARAMETERS X1,F1,X2,F2,X3,F3,X4,F4,H21,H31 ARE DEFINED AS FOLLOWS:     --
  ! -- F(X1) = F1 , F(X2) = F2 , F(X3) = F3 
  ! -- H21 = X2-X1, H31 = X3-X1, HPX1=PX-X1
  ! -- Written from f(x)=a (x-x1)^2 + b (x-x1) + c 
  ! ----------------------------------------------------------------------
  !
  FQH0(F1,F2,F3,H21,H31) = F1
  FQH1(F1,F2,F3,H21,H31) = &
       & ((F2-F1)*H31*H31 - (F3-F1)*H21*H21) / (H21*H31*(H31-H21))
  FQH2(F1,F2,F3,H21,H31) = &
       & (F3+F2-2*F1 - FQH1(F1,F2,F3,H21,H31)*(H21+H31)) &
       & / (H21*H21+H31*H31)
  ! -- FQQQH0(......,HPX1) = F(HPX1)
  FQQQH0(F1,F2,F3,H21,H31,HPX1) = &
       & FQH0(F1,F2,F3,H21,H31) + &
       &   HPX1 * (FQH1(F1,F2,F3,H21,H31) + &
       &     HPX1 * FQH2(F1,F2,F3,H21,H31))
  ! -- FQQQH1(......,HPX1) = DF/DX (HPX1)
  FQQQH1(F1,F2,F3,H21,H31,HPX1) = &
       &   FQH1(F1,F2,F3,H21,H31) + &
       &     ZTWO * HPX1 * FQH2(F1,F2,F3,H21,H31)
  ! -- FQQQH2(......,HPX1) = D2F/DX2 (HPX1)
  FQQQH2(F1,F2,F3,H21,H31,HPX1) = &
       & ZTWO * FQH2(F1,F2,F3,H21,H31)
  ! -- FQQQHM1 GIVES THE VALUE OF THE INTEGRAL OF F(X) FROM X1 TO PX:
  FQQQHM1(F1,F2,F3,H21,H31,HPX1) = &
       & HPX1 * (FQH0(F1,F2,F3,H21,H31) + &
       &   HPX1 * (FQH1(F1,F2,F3,H21,H31)/ZTWO + &
       &     HPX1 * FQH2(F1,F2,F3,H21,H31)/ZTHREE))
  !
  !.......................................................................
  !*COMDECK QUAQDQH
  ! ----------------------------------------------------------------------
  ! --     STATEMENT FUNCTION FOR QUADRATIC INTERPOLATION               --
  ! --                         19.01.87            AR        CRPP       --
  ! --                                                                  --
  ! -- QUADRATIC INTERPOLATION OF A FUNCTION F(X)                       --
  ! -- THE PARAMETERS X1,F1,X2,F2,X3,F3,X4,F4,H21,H31 ARE DEFINED AS FOLLOWS:     --
  ! -- F(X1) = F1 , DF(X1)/DX = P1 , F(X2) = F2 
  ! -- H21 = X2-X1, HPX1=PX-X1
  ! -- Written from f(x)=a (x-x1)^2 + b (x-x1) + c 
  ! ----------------------------------------------------------------------
  !
  FQDH0(F1,P1,F2,H21) = F1
  FQDH1(F1,P1,F2,H21) = P1
  FQDH2(F1,P1,F2,H21) = (F2 - P1*H21 - F1)/(H21*H21)
  ! -- FQDQH0(......,HPX1) = F(HPX1)
  FQDQH0(F1,P1,F2,H21,HPX1) = &
       & FQDH0(F1,P1,F2,H21) + &
       &   HPX1 * (FQDH1(F1,P1,F2,H21) + &
       &     HPX1 * FQDH2(F1,P1,F2,H21))
  ! -- FQQQH1(......,HPX1) = DF/DX (HPX1)
  FQDQH1(F1,P1,F2,H21,HPX1) = &
       &   FQDH1(F1,P1,F2,H21) + &
       &     ZTWO * HPX1 * FQDH2(F1,P1,F2,H21)
  ! -- FQDQH2(......,HPX1) = D2F/DX2 (HPX1)
  FQDQH2(F1,P1,F2,H21,HPX1) = ZTWO * FQDH2(F1,P1,F2,H21)
  ! -- FQQQHM1 GIVES THE VALUE OF THE INTEGRAL OF F(X) FROM X1 TO PX:
  FQDQHM1(F1,P1,F2,H21,HPX1) = &
       & HPX1 * (FQDH0(F1,P1,F2,H21) + &
       &   HPX1 * (FQDH1(F1,P1,F2,H21)/ZTWO + &
       &     HPX1 * FQDH2(F1,P1,F2,H21)/ZTHREE))
  !
  !.......................................................................
  !*COMDECK QUAQ2DQH
  ! ----------------------------------------------------------------------
  ! --     STATEMENT FUNCTION FOR QUADRATIC INTERPOLATION               --
  ! --                         19.01.87            AR        CRPP       --
  ! --                                                                  --
  ! -- QUADRATIC INTERPOLATION OF A FUNCTION F(X)                       --
  ! -- THE PARAMETERS X1,F1,X2,F2,X3,F3,X4,F4,H21,H31 ARE DEFINED AS FOLLOWS:     --
  ! -- F(X1) = F1 , D2F(X1)/DX^2 = PP1 , F(X2) = F2 
  ! -- H21 = X2-X1, HPX1=PX-X1
  ! -- Written from f(x)=a (x-x1)^2 + b (x-x1) + c 
  ! ----------------------------------------------------------------------
  !
  FQ2DH0(F1,PP1,F2,H21) = F1
  FQ2DH1(F1,PP1,F2,H21) = (F2-F1)/H21 - PP1*H21/ZTWO
  FQ2DH2(F1,PP1,F2,H21) = PP1 / ZTWO
  ! -- FQ2DQH0(......,HPX1) = F(HPX1)
  FQ2DQH0(F1,PP1,F2,H21,HPX1) = &
       & FQ2DH0(F1,PP1,F2,H21) + &
       &   HPX1 * (FQ2DH1(F1,PP1,F2,H21) + &
       &     HPX1 * FQ2DH2(F1,PP1,F2,H21))
  ! -- FQQQH1(......,HPX1) = DF/DX (HPX1)
  FQ2DQH1(F1,PP1,F2,H21,HPX1) = &
       &   FQ2DH1(F1,PP1,F2,H21) + &
       &     ZTWO * HPX1 * FQ2DH2(F1,PP1,F2,H21)
  ! -- FQ2DQH2(......,HPX1) = D2F/DX2 (HPX1)
  FQ2DQH2(F1,PP1,F2,H21,HPX1) = ZTWO * FQ2DH2(F1,PP1,F2,H21)
  ! -- FQQQHM1 GIVES THE VALUE OF THE INTEGRAL OF F(X) FROM X1 TO PX:
  FQ2DQHM1(F1,PP1,F2,H21,HPX1) = &
       & HPX1 * (FQ2DH0(F1,PP1,F2,H21) + &
       &   HPX1 * (FQ2DH1(F1,PP1,F2,H21)/ZTWO + &
       &     HPX1 * FQ2DH2(F1,PP1,F2,H21)/ZTHREE))
  !-----------------------------------------------------------------------
  !.......................................................................
  !     LINEAR
  !
  FLINEAR(X1,F1,X2,F2,PX) = F1 + (PX-X1)/(X2-X1) * (F2-F1)
  FLINEARP(X1,F1,X2,F2) = (F2-F1) / (X2-X1)
  FLINEARM1(X1,F1,X2,F2,PX) = (PX-X1)*(F1+(PX-X1)*(F2-F1)/(X2-X1)/ZTWO)
  FLINXYP(X1,F1,P1,PX) = P1*(PX-X1) + F1
  FLINXYPM1(X1,F1,P1,PX) = (PX-X1)*(F1+P1*(PX-X1)/ZTWO)

  !-----------------------------------------------------------------------
  ! 0. DEFAULTS
  !
  ICONTDER = 1
  IF (KOPTXPOL .LT. 0) ICONTDER = 0
  IOPTXPOL=ABS(KOPTXPOL)
  !
  ! COMPUTE INT. UP TO EACH INPUT INTERVAL
  IF (KOPTDER .GE. 3) THEN
    ZYINT_XIN(1) = 0._RKIND
    DO I=2,KNIN
      H = PXIN(I) - PXIN(I-1)
      ZYINT_XIN(I) = ZYINT_XIN(I-1) + H/2._RKIND*(PYIN(I) + PYIN(I-1) &
        & - H*H/12._RKIND*(PYINPP(I)+PYINPP(I-1)))
    END DO
  END IF
  !
  ! LOOP OVER XOUT POINTS WHICH CAN BE IN RANDOM ORDER
  DO J=1,KNOUT
    IF ((PXOUT(J) .LT. PXIN(1)) .OR. (PXOUT(J) .GT. PXIN(KNIN))) GO TO 200
    !
    ! 1.1 POINTS INSIDE INTERVAL [XIN(1),XIN(KNIN)]
    ! FIND PXIN INTERVAL BY BI-SECTION
    KLO=1
    KHI=KNIN
10  CONTINUE
    IF (KHI-KLO.GT.1) THEN
      K=(KHI+KLO)/2
      IF(PXIN(K) .GT. PXOUT(J))THEN
        KHI=K
      ELSE
        KLO=K
      ENDIF
      GOTO 10
    ENDIF
    H=PXIN(KHI)-PXIN(KLO)
    H2 = H * H
    SELECT CASE (KOPTDER)
    CASE (0)
      A=(PXIN(KHI)-PXOUT(J))/H
      B=(PXOUT(J)-PXIN(KLO))/H
      PY(J)=A*PYIN(KLO)+B*PYIN(KHI)+ ((A**3-A)*PYINPP(KLO)+(B**3-B)*PYINPP(KHI))*H2/6._RKIND
    CASE (1)
      A=(PXIN(KHI)-PXOUT(J))/H
      B=(PXOUT(J)-PXIN(KLO))/H
      PY(J)=A*PYIN(KLO)+B*PYIN(KHI)+ ((A**3-A)*PYINPP(KLO)+(B**3-B)*PYINPP(KHI))*H2/6._RKIND
      PYP(J)=(PYIN(KHI)-PYIN(KLO))/H - &
        & ((3._RKIND*A*A-1._RKIND)*PYINPP(KLO)-(3._RKIND*B*B-1._RKIND)*PYINPP(KHI) )*H/6._RKIND
    CASE (2)
      A=(PXIN(KHI)-PXOUT(J))/H
      B=(PXOUT(J)-PXIN(KLO))/H
      PY(J)=A*PYIN(KLO)+B*PYIN(KHI)+ ((A**3-A)*PYINPP(KLO)+(B**3-B)*PYINPP(KHI))*H2/6._RKIND
      PYP(J)=(PYIN(KHI)-PYIN(KLO))/H - &
        & ((3._RKIND*A*A-1._RKIND)*PYINPP(KLO)-(3._RKIND*B*B-1._RKIND)*PYINPP(KHI) )*H/6._RKIND
      PYPP(J)=A*PYINPP(KLO)+B*PYINPP(KHI)
    CASE (3)
      A=(PXIN(KHI)-PXOUT(J))/H
      B=ZONE - A
      A2 = A*A
      B2 = B*B
      PY(J)=A*PYIN(KLO)+B*PYIN(KHI)+ ((A**3-A)*PYINPP(KLO)+(B**3-B)*PYINPP(KHI))*H2/6._RKIND
      PYP(J)=(PYIN(KHI)-PYIN(KLO))/H - &
        & ((3._RKIND*A*A-1._RKIND)*PYINPP(KLO)-(3._RKIND*B*B-1._RKIND)*PYINPP(KHI) )*H/6._RKIND
      PYPP(J)=A*PYINPP(KLO)+B*PYINPP(KHI)
      PYINT(J) = ZYINT_XIN(KLO) + H/2._RKIND*(PYIN(KLO)*(1._RKIND-A2) + PYIN(KHI)*B2 &
        &  - (PYINPP(KLO)*(1._RKIND-A2)**2 + PYINPP(KHI)*B2*(2._RKIND-B2)) * H2/12._RKIND)
    END SELECT
    !
    GO TO 100
    !
    !     2 POINT OUTSIDE INTERVAL
    !
200 CONTINUE
    !
    !     2.1 IF KOPTXPOL=0, PRINT WARNING AND RETURN OR STOP
    !
    IF (IOPTXPOL .EQ. 0) THEN
      PRINT *,' POINT PXOUT(',J,')=',PXOUT(J),' OUTSIDE INTERVAL [',PXIN(1),',',PXIN(KNIN),']'
      RETURN
      !        STOP 'IOPTXPOL=0'
    ENDIF
    !
    !     2.2 COMPUTE VALUES FOR POINTS ON THE LEFT OF PXIN(1)
    !           EXTRAPOLATION DEPENDS ON VALUE OF KOPTXPOL
    !
    IF (PXOUT(J) .LT. PXIN(1)) THEN
      H = PXIN(2) - PXIN(1)
      ! YP(PXIN(1)) FROM SPLINE
      ! ZY1 REFERS TO FIRST KNOWN POINTS AND ZY2 TO SECOND KNOWN POINT, 
      ! TYPICALLY AT PXIN(1) AND PXIN(2) OR PXIN(1)-ALFA*H AND PXIN(1)
      ZY1P = (PYIN(2)-PYIN(1))/H - (2._RKIND*PYINPP(1)+PYINPP(2))*H/6._RKIND
      ZXLFTDEL = PXIN(1) - ALFA*H
      !
      !   2.2.1 SPECIAL PART [XIN(1)-ALFA*H,XIN(1)] IF IOPTXPOL>20
      IF ((PXOUT(J) .GE. ZXLFTDEL) .AND. (IOPTXPOL.GE.21)) THEN
        IF (IOPTXPOL .EQ. 21) THEN
          ! QUADRATIC
          IF (ICONTDER .EQ. 1) THEN
            ! KOPTXPOL = +21
            PY(J) = FQDQ0(PXIN(1),PYIN(1),ZY1P,PXIN(2),PYIN(2),PXOUT(J))
            IF (KOPTDER .GE. 1) PYP(J) = FQDQ1(PXIN(1),PYIN(1),ZY1P,PXIN(2),PYIN(2),PXOUT(J))
            IF (KOPTDER .GE. 2) PYPP(J) = FQDQ2(PXIN(1),PYIN(1),ZY1P,PXIN(2),PYIN(2))
            ! INTEGRATES DIRECTLY FROM PXIN(1)
            IF (KOPTDER .GE. 3) PYINT(J) = FQDQM1(PXIN(1),PYIN(1),ZY1P,PXIN(2),PYIN(2),PXOUT(J))
          ELSE
            ! KOPTXPOL = -21
            PY(J) = FQQQ0(PYIN(1),PYIN(2),PYIN(3),PXIN(1),PXIN(2),PXIN(3),PXOUT(J))
            IF (KOPTDER .GE. 1) PYP(J) = FQQQ1(PYIN(1),PYIN(2),PYIN(3),PXIN(1),PXIN(2),PXIN(3),PXOUT(J))
            IF (KOPTDER .GE. 2) PYPP(J) = FQQQ2(PYIN(1),PYIN(2),PYIN(3),PXIN(1),PXIN(2),PXIN(3))
            IF (KOPTDER .GE. 3) PYINT(J) = FQQQM1(PYIN(1),PYIN(2),PYIN(3),PXIN(1),PXIN(2),PXIN(3),PXOUT(J))
          END IF
        ELSEIF (IOPTXPOL .GE. 31) THEN
          ! CUBIC PART OF 31, 32
          IF (ICONTDER .EQ. 1) THEN
            ! KOPTXPOL = +31 OR +32
            PY(J) = FC2DC2DH0(PXIN(1),PYIN(1),PYINPP(1),PXIN(2),PYIN(2),PYINPP(2),H,PXOUT(J)-PXIN(1))
            IF (KOPTDER .GE. 1) PYP(J) = FC2DC2DH1(PXIN(1),PYIN(1),PYINPP(1),PXIN(2),PYIN(2),PYINPP(2), &
                 & H,PXOUT(J)-PXIN(1))
            IF (KOPTDER .GE. 2) PYPP(J) = FC2DC2DH2(PXIN(1),PYIN(1),PYINPP(1),PXIN(2),PYIN(2),PYINPP(2), &
                 & H,PXOUT(J)-PXIN(1))
            IF (KOPTDER .GE. 3) PYINT(J) = FC2DC2DHM1(PXIN(1),PYIN(1),PYINPP(2),PXIN(2),PYIN(2),PYINPP(2), &
                 & H,PXOUT(J)-PXIN(1))
          ELSE
            ! KOPTXPOL = -31 OR -32
            PY(J) = FCCCC0(PYIN(1),PYIN(2),PYIN(3),PYIN(4),PXIN(1),PXIN(2),PXIN(3),PXIN(4),PXOUT(J))
            IF (KOPTDER .GE. 1) PYP(J) = FCCCC1(PYIN(1),PYIN(2),PYIN(3),PYIN(4),PXIN(1),PXIN(2),PXIN(3),PXIN(4),PXOUT(J))
            IF (KOPTDER .GE. 2) PYPP(J) = FCCCC2(PYIN(1),PYIN(2),PYIN(3),PYIN(4),PXIN(1),PXIN(2),PXIN(3),PXIN(4),PXOUT(J))
            IF (KOPTDER .GE. 3) PYINT(J) = FCCCCM1(PYIN(1),PYIN(2),PYIN(3),PYIN(4),PXIN(1),PXIN(2),PXIN(3),PXIN(4),PXOUT(J))
          END IF
        ELSE
          PRINT *,'OPTION  KOPTXPOL= ',KOPTXPOL,' NOT YET DEFINED'
          PRINT *,'KOPTXPOL 1'
          RETURN ! AVOID STOPS
        END IF
        !
      ELSE
        !
        ! 2.2.2 EXTRAPOLATION FAR LEFT: X<XIN(1)-ALFA*H OR X<XIN(1) IF NO ALFA PART CONSIDERED
        !
        IF ((IOPTXPOL .EQ. 1) .OR. (IOPTXPOL .EQ. 21) .OR. (IOPTXPOL .EQ. 31)) THEN
          ! LINEAR EXTRAPOLATION
          SELECT CASE (KOPTXPOL)
          CASE (1)
            ZX1 = PXIN(1)
            ZY1 = PYIN(1)
            ZY1P = ZY1P
            ZYINT = 0._RKIND
          CASE(-1)
            ZX1 = PXIN(1)
            ZY1 = PYIN(1)
            ZY1P = FLINEARP(PXIN(1),PYIN(1),PXIN(2),PYIN(2))
            ZYINT = 0._RKIND
          CASE (21)
            ZX1 = ZXLFTDEL
            ZY1 = FQDQ0(PXIN(1),PYIN(1),ZY1P,PXIN(2),PYIN(2),ZXLFTDEL)
            ZYINT = FQDQM1(PXIN(1),PYIN(1),ZY1P,PXIN(2),PYIN(2),ZXLFTDEL)
            ZY1P = FQDQ1(PXIN(1),PYIN(1),ZY1P,PXIN(2),PYIN(2),ZXLFTDEL)
          CASE (-21)
            ZX1 = ZXLFTDEL
            ZY1 = FQQQ0(PYIN(1),PYIN(2),PYIN(3),PXIN(1),PXIN(2),PXIN(3),ZXLFTDEL)
            ZY1P = FQQQ1(PYIN(1),PYIN(2),PYIN(3),PXIN(1),PXIN(2),PXIN(3),ZXLFTDEL)
            ZYINT = FQQQM1(PYIN(1),PYIN(2),PYIN(3),PXIN(1),PXIN(2),PXIN(3),ZXLFTDEL)
          CASE (31)
            ZYINT = FC2DC2DHM1(PXIN(1),PYIN(1),PYINPP(1),PXIN(2),PYIN(2),PYINPP(2),H,ZXLFTDEL-PXIN(1))
            ZX1 = ZXLFTDEL
            ZY1 = FC2DC2DH0(PXIN(1),PYIN(1),PYINPP(1),PXIN(2),PYIN(2),PYINPP(2),H,ZXLFTDEL-PXIN(1))
            ZY1P = FC2DC2DH1(PXIN(1),PYIN(1),PYINPP(1),PXIN(2),PYIN(2),PYINPP(2),H,ZXLFTDEL-PXIN(1))
          CASE (-31)
            ZX1 = ZXLFTDEL
            ZY1 = FCCCC0(PYIN(1),PYIN(2),PYIN(3),PYIN(4),PXIN(1),PXIN(2),PXIN(3),PXIN(4),ZXLFTDEL)
            ZY1P = FCCCC1(PYIN(1),PYIN(2),PYIN(3),PYIN(4),PXIN(1),PXIN(2),PXIN(3),PXIN(4),ZXLFTDEL)
            ZYINT = FCCCCM1(PYIN(1),PYIN(2),PYIN(3),PYIN(4),PXIN(1),PXIN(2),PXIN(3),PXIN(4),ZXLFTDEL)
          END SELECT
          PY(J) = FLINXYP(ZX1,ZY1,ZY1P,PXOUT(J))
          IF (KOPTDER .GE. 1) PYP(J) = ZY1P
          IF (KOPTDER .GE. 2) PYPP(J) = 0._RKIND
          IF (KOPTDER .GE. 3) PYINT(J) = ZYINT + FLINXYPM1(ZX1,ZY1,ZY1P,PXOUT(J))
        ELSE IF (IOPTXPOL .EQ. 10) THEN
          ! CONSTANT OUTSIDE PXIN
          IF (ICONTDER .EQ. 1) THEN
            ! KOPTXPOL = +10
            ZYIN_XPOL = PYIN(1)
          ELSE
            ! KOPTXPOL = -10
            ZYIN_XPOL = 0._RKIND
          END IF
          PY(J) = ZYIN_XPOL
          IF (KOPTDER .GE. 1) PYP(J) = 0._RKIND
          IF (KOPTDER .GE. 2) PYPP(J) = 0._RKIND
          ! INTEGRATES FROM PXIN(1) DIRECTLY
          IF (KOPTDER .GE. 3) PYINT(J) = (PXOUT(J)-PXIN(1))*ZYIN_XPOL
          !
        ELSE IF ((IOPTXPOL .EQ. 2) .OR. (IOPTXPOL .EQ. 32)) THEN
          ! QUADRATIC EXTRAPOLATION
          SELECT CASE (KOPTXPOL)
          CASE (2)
            ZX1 = PXIN(1)
            ZY1 = PYIN(1)
            ZX2 = PXIN(2)
            ZY2 = PYIN(2)
            ZYINT = 0._RKIND
          CASE (-2)
            ZX1 = PXIN(1)
            ZY1 = PYIN(1)
            ZY1P = FQQQ1(PYIN(1),PYIN(2),PYIN(3),PXIN(1),PXIN(2),PXIN(3),PXIN(1))
            ZX2 = PXIN(2)
            ZY2 = PYIN(2)
            ZYINT = 0._RKIND
          CASE (32)
            ZYINT = FC2DC2DHM1(PXIN(1),PYIN(1),PYINPP(1),PXIN(2),PYIN(2),PYINPP(2),H,ZXLFTDEL-PXIN(1))
            ZX1 = ZXLFTDEL
            ZY1 = FC2DC2DH0(PXIN(1),PYIN(1),PYINPP(1),PXIN(2),PYIN(2),PYINPP(2),H,ZXLFTDEL-PXIN(1))
            ZY1P = FC2DC2DH1(PXIN(1),PYIN(1),PYINPP(1),PXIN(2),PYIN(2),PYINPP(2),H,ZXLFTDEL-PXIN(1))
            ZX2 = PXIN(1)
            ZY2 = PYIN(1)
          CASE (-32)
            ZX1 = ZXLFTDEL
            ZY1 = FCCCC0(PYIN(1),PYIN(2),PYIN(3),PYIN(4),PXIN(1),PXIN(2),PXIN(3),PXIN(4),ZXLFTDEL)
            ZY1P = FCCCC1(PYIN(1),PYIN(2),PYIN(3),PYIN(4),PXIN(1),PXIN(2),PXIN(3),PXIN(4),ZXLFTDEL)
            ZX2 = PXIN(1)
            ZY2 = PYIN(1)
            ZYINT = FCCCCM1(PYIN(1),PYIN(2),PYIN(3),PYIN(4),PXIN(1),PXIN(2),PXIN(3),PXIN(4),ZXLFTDEL)
          END SELECT
          PY(J) = FQDQ0(ZX1,ZY1,ZY1P,ZX2,ZY2,PXOUT(J))
          IF (KOPTDER .GE. 1) PYP(J) = FQDQ1(ZX1,ZY1,ZY1P,ZX2,ZY2,PXOUT(J))
          IF (KOPTDER .GE. 2) PYPP(J) = FQDQ2(ZX1,ZY1,ZY1P,ZX2,ZY2)
          IF (KOPTDER .GE. 3) PYINT(J) = ZYINT + FQDQM1(ZX1,ZY1,ZY1P,ZX2,ZY2,PXOUT(J))
        ELSE IF (IOPTXPOL .EQ. 3) THEN
          ! CUBIC EXTRAPOLATION
          SELECT CASE (KOPTXPOL)
          CASE (3)
            PY(J) = FC2DC2DH0(PXIN(1),PYIN(1),PYINPP(1),PXIN(2),PYIN(2),PYINPP(2),H,PXOUT(J)-PXIN(1))
            IF (KOPTDER .GE. 1) PYP(J) = &
                 & FC2DC2DH1(PXIN(1),PYIN(1),PYINPP(1),PXIN(2),PYIN(2),PYINPP(2),H,PXOUT(J)-PXIN(1));
            IF (KOPTDER .GE. 2) PYPP(J) = &
                 & FC2DC2DH2(PXIN(1),PYIN(1),PYINPP(1),PXIN(2),PYIN(2),PYINPP(2),H,PXOUT(J)-PXIN(1))
            IF (KOPTDER .GE. 3) PYINT(J) = &
                 & FC2DC2DHM1(PXIN(1),PYIN(1),PYINPP(1),PXIN(2),PYIN(2),PYINPP(2),H,PXOUT(J)-PXIN(1))
          CASE (-3)
            PY(J) = FCCCC0(PYIN(1),PYIN(2),PYIN(3),PYIN(4),PXIN(1),PXIN(2),PXIN(3),PXIN(4),PXOUT(J))
            IF (KOPTDER .GE. 1) PYP(J) = FCCCC1(PYIN(1),PYIN(2),PYIN(3),PYIN(4),PXIN(1),PXIN(2),PXIN(3),PXIN(4),PXOUT(J))
            IF (KOPTDER .GE. 2) PYPP(J) = FCCCC2(PYIN(1),PYIN(2),PYIN(3),PYIN(4),PXIN(1),PXIN(2),PXIN(3),PXIN(4),PXOUT(J))
            IF (KOPTDER .GE. 3) PYINT(J) = FCCCCM1(PYIN(1),PYIN(2),PYIN(3),PYIN(4),PXIN(1),PXIN(2),PXIN(3),PXIN(4),PXOUT(J))
          END SELECT
        ELSE
          PRINT *,'OPTION  IOPTXPOL= ',IOPTXPOL,' NOT YET DEFINED'
          STOP 'KOPTXPOL 2'
        END IF
      END IF
      !
    ELSE ! PXOUT(J) .GT. PXIN(KNIN)
      !
      !     2.3 COMPUTE VALUES FOR POINTS ON THE RIGHT OF PXIN(1)
      !         EXTRAPOLATION DEPENDS ON VALUE OF KOPTXPOL
      !
      H = PXIN(KNIN) - PXIN(KNIN-1)
      ! ZYN REFERS TO FIRST KNOWN POINTS AND ZYNN TO SECOND KNOWN POINT, 
      ! TYPICALLY AT PXIN(KNIN) AND PXIN(KNIN-1) OR PXIN(KNIN)+ALFA*H AND PXIN(KNIN)
      ! YP(PXIN(KNIN)) FROM SPLINE
      ZYNP = (PYIN(KNIN)-PYIN(KNIN-1))/H + (PYINPP(KNIN-1)+2._RKIND*PYINPP(KNIN))*H/6._RKIND
      ZXRGTDEL = PXIN(KNIN) + ALFA*H
      !
      !   2.3.1 SPECIAL PART ]XIN(KNIN),XIN(KNIN)+ALFA*H] IF IOPTXPOL>20
      IF ((PXOUT(J) .LE. ZXRGTDEL) .AND. (IOPTXPOL.GE.21)) THEN
        ZYINT = ZYINT_XIN(KNIN)
        IF (IOPTXPOL .EQ. 21) THEN
          ! QUADRATIC
          IF (ICONTDER .EQ. 1) THEN
            ! KOPTXPOL = +21
            PY(J) = FQDQ0(PXIN(KNIN),PYIN(KNIN),ZYNP,PXIN(KNIN-1),PYIN(KNIN-1),PXOUT(J))
            IF (KOPTDER .GE. 1) PYP(J) = FQDQ1(PXIN(KNIN),PYIN(KNIN),ZYNP,PXIN(KNIN-1),PYIN(KNIN-1),PXOUT(J))
            IF (KOPTDER .GE. 2) PYPP(J) = FQDQ2(PXIN(KNIN),PYIN(KNIN),ZYNP,PXIN(KNIN-1),PYIN(KNIN-1))
            ! INTEGRATES FROM PXIN(KNIN) SO ADD INTEGRAL UP TO PXIN(KNIN)
            IF (KOPTDER .GE. 3) PYINT(J) = ZYINT + FQDQM1(PXIN(KNIN),PYIN(KNIN),ZYNP,PXIN(KNIN-1),PYIN(KNIN-1),PXOUT(J))
          ELSE
            ! KOPTXPOL = -21
            PY(J) = FQQQ0(PYIN(KNIN),PYIN(KNIN-1),PYIN(KNIN-2),PXIN(KNIN),PXIN(KNIN-1),PXIN(KNIN-2),PXOUT(J))
            IF (KOPTDER .GE. 1) PYP(J) = &
              & FQQQ1(PYIN(KNIN),PYIN(KNIN-1),PYIN(KNIN-2),PXIN(KNIN),PXIN(KNIN-1),PXIN(KNIN-2),PXOUT(J))
            IF (KOPTDER .GE. 2) PYPP(J) = &
              & FQQQ2(PYIN(KNIN),PYIN(KNIN-1),PYIN(KNIN-2),PXIN(KNIN),PXIN(KNIN-1),PXIN(KNIN-2))
            IF (KOPTDER .GE. 3) PYINT(J) = ZYINT + &
              & FQQQM1(PYIN(KNIN),PYIN(KNIN-1),PYIN(KNIN-2),PXIN(KNIN),PXIN(KNIN-1),PXIN(KNIN-2),PXOUT(J))
          END IF
        ELSEIF (IOPTXPOL .GE. 31) THEN
          ! CUBIC PART OF 31, 32
          IF (ICONTDER .EQ. 1) THEN
            ! KOPTXPOL = +31 OR +32
            PY(J) = FC2DC2DH0(PXIN(KNIN),PYIN(KNIN),PYINPP(KNIN), &
                 &      PXIN(KNIN-1),PYIN(KNIN-1),PYINPP(KNIN-1),-H,PXOUT(J)-PXIN(KNIN))
            IF (KOPTDER .GE. 1) PYP(J) = FC2DC2DH1(PXIN(KNIN),PYIN(KNIN),PYINPP(KNIN), &
                 & PXIN(KNIN-1),PYIN(KNIN-1),PYINPP(KNIN-1),-H,PXOUT(J)-PXIN(KNIN))
            IF (KOPTDER .GE. 2) PYPP(J) = FC2DC2DH2(PXIN(KNIN),PYIN(KNIN),PYINPP(KNIN), &
                 & PXIN(KNIN-1),PYIN(KNIN-1),PYINPP(KNIN-1),-H,PXOUT(J)-PXIN(KNIN))
            IF (KOPTDER .GE. 3) PYINT(J) = ZYINT + FC2DC2DHM1(PXIN(KNIN),PYIN(KNIN),PYINPP(KNIN), &
                 & PXIN(KNIN-1),PYIN(KNIN-1),PYINPP(KNIN-1),-H,PXOUT(J)-PXIN(KNIN))
          ELSE
            ! KOPTXPOL = -31 OR -32
            PY(J) = FCCCC0(PYIN(KNIN),PYIN(KNIN-1),PYIN(KNIN-2),PYIN(KNIN-3), &
              & PXIN(KNIN),PXIN(KNIN-1),PXIN(KNIN-2),PXIN(KNIN-3),PXOUT(J))
            IF (KOPTDER .GE. 1) PYP(J) = FCCCC1(PYIN(KNIN),PYIN(KNIN-1),PYIN(KNIN-2),PYIN(KNIN-3), &
              & PXIN(KNIN),PXIN(KNIN-1),PXIN(KNIN-2),PXIN(KNIN-3),PXOUT(J))
            IF (KOPTDER .GE. 2) PYPP(J) = FCCCC2(PYIN(KNIN),PYIN(KNIN-1),PYIN(KNIN-2),PYIN(KNIN-3), &
              & PXIN(KNIN),PXIN(KNIN-1),PXIN(KNIN-2),PXIN(KNIN-3),PXOUT(J))
            IF (KOPTDER .GE. 3) PYINT(J) = ZYINT + FCCCCM1(PYIN(KNIN),PYIN(KNIN-1),PYIN(KNIN-2),PYIN(KNIN-3), &
              & PXIN(KNIN),PXIN(KNIN-1),PXIN(KNIN-2),PXIN(KNIN-3),PXOUT(J))
          END IF
        ELSE
          PRINT *,'OPTION  KOPTXPOL= ',KOPTXPOL,' NOT YET DEFINED'
          STOP 'KOPTXPOL 3'
        END IF
        !
      ELSE
        !
        ! 2.3.2 EXTRAPOLATION FAR RIGHT: X>XIN(KNIN)+ALFA*H OR X>XIN(KNIN) IF NO ALFA PART CONSIDERED
        !
        IF ((IOPTXPOL .EQ. 1) .OR. (IOPTXPOL .EQ. 21) .OR. (IOPTXPOL .EQ. 31)) THEN
          ! LINEAR EXTRAPOLATION
          SELECT CASE (KOPTXPOL)
          CASE (1)
            ZXN = PXIN(KNIN)
            ZYN = PYIN(KNIN)
            ZYINT = ZYINT_XIN(KNIN)
          CASE(-1)
            ZXN = PXIN(KNIN)
            ZYN = PYIN(KNIN)
            ZYNP = FLINEARP(PXIN(KNIN),PYIN(KNIN),PXIN(KNIN-1),PYIN(KNIN-1))
            ZYINT = ZYINT_XIN(KNIN)
          CASE (21)
            ZYINT = ZYINT_XIN(KNIN) + FQDQM1(PXIN(KNIN),PYIN(KNIN),ZYNP,PXIN(KNIN-1),PYIN(KNIN-1),ZXRGTDEL)
            ZXN = ZXRGTDEL
            ZYN = FQDQ0(PXIN(KNIN),PYIN(KNIN),ZYNP,PXIN(KNIN-1),PYIN(KNIN-1),ZXRGTDEL)
            ZYNP = FQDQ1(PXIN(KNIN),PYIN(KNIN),ZYNP,PXIN(KNIN-1),PYIN(KNIN-1),ZXRGTDEL)
          CASE (-21)
            ZXN = ZXRGTDEL
            ZYN = FQQQ0(PYIN(KNIN),PYIN(KNIN-1),PYIN(KNIN-2),PXIN(KNIN),PXIN(KNIN-1),PXIN(KNIN-2),ZXRGTDEL)
            ZYNP = FQQQ1(PYIN(KNIN),PYIN(KNIN-1),PYIN(KNIN-2),PXIN(KNIN),PXIN(KNIN-1),PXIN(KNIN-2),ZXRGTDEL)
            ZYINT = ZYINT_XIN(KNIN) + &
              & FQQQM1(PYIN(KNIN),PYIN(KNIN-1),PYIN(KNIN-2),PXIN(KNIN),PXIN(KNIN-1),PXIN(KNIN-2),ZXRGTDEL)
          CASE (31)
            ZXN = ZXRGTDEL
            ZYINT = ZYINT_XIN(KNIN) + FC2DC2DHM1(PXIN(KNIN),PYIN(KNIN),PYINPP(KNIN), &
                 & PXIN(KNIN-1),PYIN(KNIN-1),PYINPP(KNIN-1),-H,ZXRGTDEL-PXIN(KNIN))
            ZYN = FC2DC2DH0(PXIN(KNIN),PYIN(KNIN),PYINPP(KNIN), &
                 & PXIN(KNIN-1),PYIN(KNIN-1),PYINPP(KNIN-1),-H,ZXRGTDEL-PXIN(KNIN))
            ZYNP = FC2DC2DH1(PXIN(KNIN),PYIN(KNIN),PYINPP(KNIN), &
                 & PXIN(KNIN-1),PYIN(KNIN-1),PYINPP(KNIN-1),-H,ZXRGTDEL-PXIN(KNIN))
          CASE (-31)
            ZXN = ZXRGTDEL
            ZYN = FCCCC0(PYIN(KNIN),PYIN(KNIN-1),PYIN(KNIN-2),PYIN(KNIN-3), &
              & PXIN(KNIN),PXIN(KNIN-1),PXIN(KNIN-2),PXIN(KNIN-3),ZXRGTDEL)
            ZYNP = FCCCC1(PYIN(KNIN),PYIN(KNIN-1),PYIN(KNIN-2),PYIN(KNIN-3), &
              & PXIN(KNIN),PXIN(KNIN-1),PXIN(KNIN-2),PXIN(KNIN-3),ZXRGTDEL)
            ZYINT = ZYINT_XIN(KNIN) + FCCCCM1(PYIN(KNIN),PYIN(KNIN-1),PYIN(KNIN-2),PYIN(KNIN-3), &
              & PXIN(KNIN),PXIN(KNIN-1),PXIN(KNIN-2),PXIN(KNIN-3),ZXRGTDEL)
          END SELECT
          PY(J) = FLINXYP(ZXN,ZYN,ZYNP,PXOUT(J))
          IF (KOPTDER .GE. 1) PYP(J) = ZYNP
          IF (KOPTDER .GE. 2) PYPP(J) = 0._RKIND
          IF (KOPTDER .GE. 3) PYINT(J) = ZYINT + FLINXYPM1(ZXN,ZYN,ZYNP,PXOUT(J))
        ELSE IF (IOPTXPOL .EQ. 10) THEN
          ! CONSTANT OUTSIDE PXIN
          IF (ICONTDER .EQ. 1) THEN
            ! KOPTXPOL = +10
            ZYIN_XPOL = PYIN(KNIN)
          ELSE
            ! KOPTXPOL = -10
            ZYIN_XPOL = 0._RKIND
          END IF
          PY(J) = ZYIN_XPOL
          IF (KOPTDER .GE. 1) PYP(J) = 0._RKIND
          IF (KOPTDER .GE. 2) PYPP(J) = 0._RKIND
          IF (KOPTDER .GE. 3) PYINT(J) = ZYINT_XIN(KNIN) + (PXOUT(J)-PXIN(KNIN))*ZYIN_XPOL
          !
        ELSE IF ((IOPTXPOL .EQ. 2) .OR. (IOPTXPOL .EQ. 32)) THEN
          ! QUADRATIC EXTRAPOLATION
          SELECT CASE (KOPTXPOL)
          CASE (2)
            ZXN = PXIN(KNIN)
            ZYN = PYIN(KNIN)
            ZXNN = PXIN(KNIN-1)
            ZYNN = PYIN(KNIN-1)
            ZYINT = ZYINT_XIN(KNIN)
          CASE (-2)
            ZXN = PXIN(KNIN)
            ZYN = PYIN(KNIN)
            ZYNP = FQQQ1(PYIN(KNIN),PYIN(KNIN-1),PYIN(KNIN-2),PXIN(KNIN),PXIN(KNIN-1),PXIN(KNIN-2),PXIN(KNIN))
            ZXNN = PXIN(KNIN-1)
            ZYNN = PYIN(KNIN-1)
            ZYINT = ZYINT_XIN(KNIN)
          CASE (32)
            ZXN = ZXRGTDEL
            ZYINT = ZYINT_XIN(KNIN) + FC2DC2DHM1(PXIN(KNIN),PYIN(KNIN),PYINPP(KNIN), &
                 & PXIN(KNIN-1),PYIN(KNIN-1),PYINPP(KNIN-1),-H,ZXRGTDEL-PXIN(KNIN))
            ZXNN = PXIN(KNIN)
            ZYNN = PYIN(KNIN)
            ZYN = FC2DC2DH0(PXIN(KNIN),PYIN(KNIN),PYINPP(KNIN), &
                 & PXIN(KNIN-1),PYIN(KNIN-1),PYINPP(KNIN-1),-H,ZXRGTDEL-PXIN(KNIN))
            ZYNP = FC2DC2DH1(PXIN(KNIN),PYIN(KNIN),PYINPP(KNIN), &
                 & PXIN(KNIN-1),PYIN(KNIN-1),PYINPP(KNIN-1),-H,ZXRGTDEL-PXIN(KNIN))
          CASE (-32)
            ZXN = ZXRGTDEL
            ZYN = FCCCC0(PYIN(KNIN),PYIN(KNIN-1),PYIN(KNIN-2),PYIN(KNIN-3), &
              & PXIN(KNIN),PXIN(KNIN-1),PXIN(KNIN-2),PXIN(KNIN-3),ZXRGTDEL)
            ZYNP = FCCCC1(PYIN(KNIN),PYIN(KNIN-1),PYIN(KNIN-2),PYIN(KNIN-3), &
              & PXIN(KNIN),PXIN(KNIN-1),PXIN(KNIN-2),PXIN(KNIN-3),ZXRGTDEL)
            ZXNN = PXIN(KNIN)
            ZYNN = PYIN(KNIN)
            ZYINT = ZYINT_XIN(KNIN) + FCCCCM1(PYIN(KNIN),PYIN(KNIN-1),PYIN(KNIN-2),PYIN(KNIN-3), &
              & PXIN(KNIN),PXIN(KNIN-1),PXIN(KNIN-2),PXIN(KNIN-3),ZXRGTDEL)
          END SELECT
          PY(J) = FQDQ0(ZXN,ZYN,ZYNP,ZXNN,ZYNN,PXOUT(J))
          IF (KOPTDER .GE. 1) PYP(J) = FQDQ1(ZXN,ZYN,ZYNP,ZXNN,ZYNN,PXOUT(J))
          IF (KOPTDER .GE. 2) PYPP(J) = FQDQ2(ZXN,ZYN,ZYNP,ZXNN,ZYNN)
          IF (KOPTDER .GE. 3) PYINT(J) = ZYINT + FQDQM1(ZXN,ZYN,ZYNP,ZXNN,ZYNN,PXOUT(J))
        ELSE IF (IOPTXPOL .EQ. 3) THEN
          ! CUBIC EXTRAPOLATION
          SELECT CASE (KOPTXPOL)
          CASE (3)
            PY(J) = FC2DC2DH0(PXIN(KNIN),PYIN(KNIN),PYINPP(KNIN), &
                 & PXIN(KNIN-1),PYIN(KNIN-1),PYINPP(KNIN-1),-H,PXOUT(J)-PXIN(KNIN))
            IF (KOPTDER .GE. 1) PYP(J) = FC2DC2DH1(PXIN(KNIN),PYIN(KNIN),PYINPP(KNIN), &
                 & PXIN(KNIN-1),PYIN(KNIN-1),PYINPP(KNIN-1),-H,PXOUT(J)-PXIN(KNIN))
            IF (KOPTDER .GE. 2) PYPP(J) = FC2DC2DH2(PXIN(KNIN),PYIN(KNIN),PYINPP(KNIN), &
                 & PXIN(KNIN-1),PYIN(KNIN-1),PYINPP(KNIN-1),-H,PXOUT(J)-PXIN(KNIN))
            IF (KOPTDER .GE. 3) PYINT(J) = ZYINT_XIN(KNIN) + FC2DC2DHM1(PXIN(KNIN),PYIN(KNIN),PYINPP(KNIN), &
              & PXIN(KNIN-1),PYIN(KNIN-1),PYINPP(KNIN-1),-H,PXOUT(J)-PXIN(KNIN))
          CASE (-3)
            PY(J) = FCCCC0(PYIN(KNIN),PYIN(KNIN-1),PYIN(KNIN-2),PYIN(KNIN-3), &
              & PXIN(KNIN),PXIN(KNIN-1),PXIN(KNIN-2),PXIN(KNIN-3),PXOUT(J))
            IF (KOPTDER .GE. 1) PYP(J) = FCCCC1(PYIN(KNIN),PYIN(KNIN-1),PYIN(KNIN-2),PYIN(KNIN-3), &
              & PXIN(KNIN),PXIN(KNIN-1),PXIN(KNIN-2),PXIN(KNIN-3),PXOUT(J))
            IF (KOPTDER .GE. 2) PYPP(J) = FCCCC2(PYIN(KNIN),PYIN(KNIN-1),PYIN(KNIN-2),PYIN(KNIN-3), &
              & PXIN(KNIN),PXIN(KNIN-1),PXIN(KNIN-2),PXIN(KNIN-3),PXOUT(J))
            IF (KOPTDER .GE. 3) PYINT(J) = ZYINT_XIN(KNIN) + FCCCCM1(PYIN(KNIN),PYIN(KNIN-1),PYIN(KNIN-2),PYIN(KNIN-3), &
              & PXIN(KNIN),PXIN(KNIN-1),PXIN(KNIN-2),PXIN(KNIN-3),PXOUT(J))
          END SELECT
        ELSE
          PRINT *,'OPTION  KOPTXPOL= ',KOPTXPOL,' NOT YET DEFINED'
          STOP 'KOPTXPOL 4'
        END IF
      END IF
    END IF
    !
100 CONTINUE
  END DO
  RETURN
END SUBROUTINE SPLIBNDA
!.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
!
SUBROUTINE INTLINEAR(PXIN,PYIN,KNIN,PXOUT,PYOUT,PYOUTP,PYOUTPP,PYOUTINT,KNOUT,KOPTDER,KEXTRAPO)
  !
  !   COMPUTE LINEAR INTERPOLATION OF (PXIN,PYIN) ON (PXOUT,PYOUT).
  !
  !   KOPTDER = 0: COMPUTE ONLY PYOUT (PYOUTP, PYOUTPP NOT USED)
  !   KOPTDER = 1: COMPUTE ALSO 1ST DER. IN PYOUTP (PYOUTPP NOT USED)
  !   KOPTDER = 2: AS 1 AND 2ND DER. IN PYOUTPP
  !   KOPTDER = 3: AS 2 AND INTEGRAL FROM (PXIN(1) IN PYOUTPP
  !
  !   KEXTRAPO = 0: PRINT MESSAGE AND RETURN IF NEED TO EXTRAPOLATE
  !   KEXTRAPO =  1: LINEAR EXTRAPOLATION WITH LAST POINT AND DERIVATIVE
  !   KEXTRAPO = -1: LINEAR EXTRAPOLATION WITH LAST 2 POINTS (same as +1, thus use +1)
  !   KEXTRAPO =  10: Y=Y_EDGE FOR EXTRAPOLATION => CONSTANT EXTRAPOLATION
  !   KEXTRAPO = -10: Y=0 FOR EXTRAPOLATION
  !
  USE PREC_RKIND
  IMPLICIT NONE
  ! arguments
  INTEGER :: KNIN, KNOUT, KOPTDER, KEXTRAPO
  REAL(RKIND) :: PXIN(KNIN), PYIN(KNIN), PXOUT(KNOUT)
  REAL(RKIND) :: PYOUT(KNOUT), PYOUTP(KNOUT), PYOUTPP(KNOUT), PYOUTINT(KNOUT)
  !
  REAL(RKIND) :: ZYINT_XIN(KNIN)
  ! FOR FUNCTIONS
  REAL(RKIND) :: X1, X2, F1, F2, PX, FLINEAR, FLINEARP, FLINEARM1, &
       & ZYP, P1, FLINXYP, FLINXYPM1
  INTEGER :: K, KLO, KHI, I, J
  !
  FLINEAR(X1,F1,X2,F2,PX) = F1 + (PX-X1)/(X2-X1) * (F2-F1)
  FLINEARP(X1,F1,X2,F2) = (F2-F1) / (X2-X1)
  FLINEARM1(X1,F1,X2,F2,PX) = (PX-X1)*(F1+0.5_RKIND*(PX-X1)*(F2-F1)/(X2-X1))
!!$  FLINXYP(X1,F1,P1,PX) = P1*(PX-X1) + F1
!!$  FLINXYPM1(X1,F1,P1,PX) = (PX-X1)*(F1+0.5_RKIND*P1*(PX-X1))
  !.......................................................................
  !
  IF (KOPTDER .GE. 3) THEN
    ZYINT_XIN(1) = 0._RKIND
    DO I=2,KNIN
      ZYINT_XIN(I) = ZYINT_XIN(I-1) + 0.5_RKIND*(PXIN(I)-PXIN(I-1))*(PYIN(I)+PYIN(I-1))
    END DO
  END IF
  !
  DO J=1,KNOUT
    IF (PXOUT(J) .LT. PXIN(1)) THEN
      SELECT CASE (KEXTRAPO)
      CASE (-1, 1)
        PYOUT(J) = FLINEAR(PXIN(1),PYIN(1),PXIN(2),PYIN(2),PXOUT(J))
        IF (KOPTDER .GE. 1) PYOUTP(J) = FLINEARP(PXIN(1),PYIN(1),PXIN(2),PYIN(2))
        IF (KOPTDER .GE. 2) PYOUTPP(J) = 0._RKIND
        IF (KOPTDER .GE. 3) PYOUTINT(J) = FLINEARM1(PXIN(1),PYIN(1),PXIN(2),PYIN(2),PXOUT(J))
      CASE (0)
        PRINT *,' WARNING POINT PXOUT(',J,')=',PXOUT(J),' OUTSIDE INTERVAL ON LEFT'
        RETURN
      CASE (10)
        PYOUT(J) = PYIN(1)
        IF (KOPTDER .GE. 1) PYOUTP(J) = 0._RKIND
        IF (KOPTDER .GE. 2) PYOUTPP(J) = 0._RKIND
        IF (KOPTDER .GE. 3) PYOUTINT(J) = PYIN(1)*(PXOUT(J)-PXIN(1))
      CASE (-10)
        PYOUT(J) = 0._RKIND
        IF (KOPTDER .GE. 1) PYOUTP(J) = 0._RKIND
        IF (KOPTDER .GE. 2) PYOUTPP(J) = 0._RKIND
        IF (KOPTDER .GE. 3) PYOUTINT(J) = 0._RKIND
      CASE DEFAULT
        WRITE(0,*) 'DEFAULT IN INTLINEAR PXOUT(J) .LT. PXIN(1) SELECT CASE'
        WRITE(0,*) 'SHOULD NOT BE THERE, OPTION KEXTRAPO = ',KEXTRAPO,' NOT DEFINED'
        CALL FLUSH(0)
        RETURN
      END SELECT
      !
    ELSE IF (PXOUT(J) .GT. PXIN(KNIN)) THEN
      ! POINT ON RIGHT OF INTERVAL
      SELECT CASE (KEXTRAPO)
      CASE (-1, 1)
        PYOUT(J) = FLINEAR(PXIN(KNIN-1),PYIN(KNIN-1),PXIN(KNIN),PYIN(KNIN),PXOUT(J))
        IF (KOPTDER .GE. 1) PYOUTP(J) = FLINEARP(PXIN(KNIN-1),PYIN(KNIN-1),PXIN(KNIN),PYIN(KNIN))
        IF (KOPTDER .GE. 2) PYOUTPP(J) = 0._RKIND
        IF (KOPTDER .GE. 3) PYOUTINT(J) = ZYINT_XIN(KNIN) + &
          & FLINEARM1(PXIN(KNIN),PYIN(KNIN),PXIN(KNIN-1),PYIN(KNIN-1),PXOUT(J))
      CASE (0)
        PRINT *,' WARNING POINT PXOUT(',J,')=',PXOUT(J),' OUTSIDE INTERVAL ON RIGHT'
        RETURN
      CASE(10)
        PYOUT(J) = PYIN(KNIN)
        IF (KOPTDER .GE. 1) PYOUTP(J) = 0.0_RKIND
        IF (KOPTDER .GE. 2) PYOUTPP(J) = 0.0_RKIND
        IF (KOPTDER .GE. 3) PYOUTINT(J) = ZYINT_XIN(KNIN) + PYIN(KNIN)*(PXOUT(J)-PXIN(KNIN))
      CASE(-10)
        PYOUT(J) = 0.0_RKIND
        IF (KOPTDER .GE. 1) PYOUTP(J) = 0.0_RKIND
        IF (KOPTDER .GE. 2) PYOUTPP(J) = 0.0_RKIND
        IF (KOPTDER .GE. 3) PYOUTINT(J) = ZYINT_XIN(KNIN)
      CASE DEFAULT
        WRITE(0,*) 'DEFAULT IN INTLINEAR PXOUT(J) .GT. PXIN(1) SELECT CASE'
        WRITE(0,*) 'SHOULD NOT BE THERE, OPTION KEXTRAPO = ',KEXTRAPO,' NOT DEFINED'
        CALL FLUSH(0)
        RETURN
      END SELECT
      !
    ELSE
      ! FIND PXIN INTERVAL BY BI-SECTION
      KLO=1
      KHI=KNIN
10    CONTINUE
      IF (KHI-KLO.GT.1) THEN
        K=(KHI+KLO)/2
        IF(PXIN(K) .GT. PXOUT(J))THEN
          KHI=K
        ELSE
          KLO=K
        ENDIF
        GOTO 10
      ENDIF
      PYOUT(J) = FLINEAR(PXIN(KLO),PYIN(KLO),PXIN(KHI),PYIN(KHI),PXOUT(J))
      IF (KOPTDER .GE. 1) PYOUTP(J) = FLINEARP(PXIN(KLO),PYIN(KLO),PXIN(KHI),PYIN(KHI))
      IF (KOPTDER .GE. 2) PYOUTPP(J) = 0._RKIND
      IF (KOPTDER .GE. 3) PYOUTINT(J) = ZYINT_XIN(KLO) + &
        & FLINEARM1(PXIN(KLO),PYIN(KLO),PXIN(KHI),PYIN(KHI),PXOUT(J))
      !
    END IF
  END DO
  !
  RETURN
END SUBROUTINE INTLINEAR
!......................................................................
!     
SUBROUTINE INTQUADRATIC(PXIN,PYIN,KNIN,PXOUT,PYOUT,PYOUTP,PYOUTPP,PYOUTINT,KNOUT,KOPTDER,KOPTXPOL,NBC)
  !
  !   COMPUTE LINEAR INTERPOLATION OF (PXIN,PYIN) ON (PXOUT,PYOUT).
  !
  !   KOPTDER = 0: COMPUTE ONLY PYOUT (PYOUTP, PYOUTPP NOT USED)
  !   KOPTDER = 1: COMPUTE ALSO 1ST DER. IN PYOUTP (PYOUTPP NOT USED)
  !   KOPTDER = 2: AS 1 AND 2ND DER. IN PYOUTPP
  !   KOPTDER = 3: AS 2 AND INTEGRAL FROM (PXIN(1) IN PYOUTPP
  !
  !   KOPTXPOL = 0: SEND MESSAGE IF NEED TO EXTRAPOLATE
  !   KOPTXPOL = 1: LINEAR EXTRAPOLATION WITH CONTINUOUS DERIVATIVE
  !   KOPTXPOL = -1: LINEAR EXTRAPOLATION WITH LAST THREE Y VALUES
  !   KOPTXPOL = 2: QUADRATIC EXTRAPOLATION WITH CONTINUOUS DERIVATIVE
  !   KOPTXPOL = -2: QUADRATIC EXTRAPOLATION WITH LAST THREE Y VALUES
  !   KOPTXPOL = 21: QUADRATIC EXTRAPOLATION WITHIN ONE DELTA_X, THEN LINEAR
  !   KOPTXPOL = -21: SAME AS 21 BUT USES VALUES INSTEAD OF CLOSEST DERIVATIVE FOR EXTRAPOLATION
  !   KOPTXPOL = 10: Y=Y_EDGE FOR EXTRAPOLATION => CONSTANT EXTRAPOLATION
  !   KOPTXPOL = -10: Y=0 FOR EXTRAPOLATION
  !
  ! SOLVES FOR QUADRATICS WITH CONTINUOUS DERIVATIVE AND 
  !   NBC = 0:  2ND DERIVATIVE =0 AT LEFT (DEFAULT)
  !   NBC = 1:  2ND DERIVATIVE =0 AT RIGHT
  !
  USE PREC_RKIND
  IMPLICIT NONE
  REAL(RKIND) :: ZSIX, ZTHREE, ZTWO, ZONE
  PARAMETER(ZSIX=6._RKIND, ZTHREE=3._RKIND, ZTWO=2._RKIND, ZONE=1._RKIND)
  REAL(RKIND) :: ALFA
  PARAMETER(ALFA = 1._RKIND)
  ! arguments
  INTEGER :: KNIN, KNOUT, KOPTDER, KOPTXPOL
  INTEGER, OPTIONAL ::  NBC
  REAL(RKIND) :: PXIN(KNIN), PYIN(KNIN), PXOUT(KNOUT)
  REAL(RKIND) :: PYOUT(KNOUT), PYOUTP(KNOUT), PYOUTPP(KNOUT), PYOUTINT(KNOUT)
  !
  REAL(RKIND) :: BCOEF(KNIN-1), ZYINP(KNIN), ZYINT_XIN(KNIN), ZYIN_XPOL, ZYINT, &
       & ZXLFTDEL, ZXRGTDEL, ZX1, ZXN, ZY1, ZYN, ZY1P, ZX2, ZXNN, ZYNP, ZY2, ZYNN
  INTEGER I,J,IOPTXPOL, INBC, ICONTDER, K, KLO, KHI
  !
  !
  ! VARIABLES RELATED TO FUNCTIONS:
  REAL(RKIND) :: FQQQ0, FQQQ1, FQQQ2, &
    &  FLINEAR, FLINEARP,  &
    &  FQQQM1, FQDQM1, FLINEARM1, FLINXYP, FLINXYPM1, FPARABOLP
  REAL(RKIND) :: A1, A2, A3, A4, B1, B2, B3, B4, PX
  REAL(RKIND) :: FB0, FB1, FB2, FA0, FA1, FD2, FD1, FD0, FQDQ0, FQDQ1, FQDQ2
  REAL(RKIND) :: X1, F1, P1, X2, F2
  REAL(RKIND) :: X3, F3
  !
  !.......................................................................
  !*COMDECK QUAQQQ
  ! ----------------------------------------------------------------------
  ! --     STATEMENT FUNCTION FOR QUADRATIC INTERPOLATION               --
  ! --                         19.01.87            AR        CRPP       --
  ! --                                                                  --
  ! -- QUADRATIC INTERPOLATION OF A FUNCTION F(X)                       --
  ! -- THE SIX PARAMETERS A1,A2,A3,B1,B2,B3 ARE DEFINED AS FOLLOWS:     --
  ! -- F(B1) = A1 , F(B2) = A2 , F(B3) = A3                             --
  ! ----------------------------------------------------------------------
  !
  FB2(A1,A2,A3,B1,B2,B3) = &
    &               ((A1-A2)/(B1-B2)-(A1-A3)/(B1-B3))/(B2-B3)
  FB1(A1,A2,A3,B1,B2,B3) = ((A1-A2)/(B1-B2))- &
    &         FB2(A1,A2,A3,B1,B2,B3)*(B1+B2)
  FB0(A1,A2,A3,B1,B2,B3) = A1-FB1(A1,A2,A3,B1,B2,B3)*B1 &
    &         -FB2(A1,A2,A3,B1,B2,B3)*B1*B1
  ! ----------------------------------------------------------------------
  ! -- FQQQ0 GIVES THE VALUE OF THE FUNCTION AT THE POINT PX            --
  ! -- FQQQ0(......,PX) = F(PX)                                         --
  ! ----------------------------------------------------------------------
  FQQQ0(A1,A2,A3,B1,B2,B3,PX) = FB0(A1,A2,A3,B1,B2,B3) + &
    &                                 PX * (FB1(A1,A2,A3,B1,B2,B3) + &
    &                                 PX * FB2(A1,A2,A3,B1,B2,B3))
  ! ----------------------------------------------------------------------
  ! -- FQQQ1 GIVES THE VALUE OF THE FIRST DERIVATIVE OF F(X) AT PX      --
  ! -- FQQQ1(......,PX) = DF/DX (PX)                                    --
  ! ----------------------------------------------------------------------
  FQQQ1(A1,A2,A3,B1,B2,B3,PX) = FB1(A1,A2,A3,B1,B2,B3) + &
    &     ZTWO * PX * FB2(A1,A2,A3,B1,B2,B3)
  ! ----------------------------------------------------------------------
  ! -- FQQQ2 GIVES THE VALUE OF THE SECOND DERIVATIVE OF F(X) AT PX     --
  ! -- FQQQ2(......,PX) = D2F/DX2 (PX)                                  --
  ! ----------------------------------------------------------------------
  FQQQ2(A1,A2,A3,B1,B2,B3) = ZTWO * FB2(A1,A2,A3,B1,B2,B3)
  ! ----------------------------------------------------------------------
  ! -- FQQQM1 GIVES THE VALUE OF THE INTEGRAL OF F(X) FROM B1 TO PX:
  !
  FQQQM1(A1,A2,A3,B1,B2,B3,PX) = &
    & (PX-B1)*(FB0(A1,A2,A3,B1,B2,B3) + &
    &  0.5_RKIND*(PX+B1)*FB1(A1,A2,A3,B1,B2,B3) + &
    &  FB2(A1,A2,A3,B1,B2,B3)/3._RKIND*(PX*(PX+B1)+B1*B1))
  !.......................................................................
  !*COMDECK QUAQDQ
  ! ----------------------------------------------------------------------
  ! --     STATEMENT FUNCTION FOR QUADRATIC INTERPOLATION               --
  ! --                         19.01.87            AR        CRPP       --
  ! --                                                                  --
  ! -- QUADRATIC INTERPOLATION OF A FUNCTION F(X)                       --
  ! -- THE FIVE PARAMETERS X1,F1,P1,X2,F2    ARE DEFINED AS FOLLOWS:    --
  ! -- F(X1) = F1 , DF/DX(X1) = P1 , F(X2) = F2                         --
  ! ----------------------------------------------------------------------
  !
  FD2(X1,F1,P1,X2,F2) = ((F2-F1)/(X2-X1) - P1) / (X2-X1)
  FD1(X1,F1,P1,X2,F2) = P1 - ZTWO*X1*FD2(X1,F1,P1,X2,F2)
  FD0(X1,F1,P1,X2,F2) = F1 - X1*(X1*FD2(X1,F1,P1,X2,F2) + &
    &                                     FD1(X1,F1,P1,X2,F2))
  ! ----------------------------------------------------------------------
  ! -- FQDQ0 GIVES THE VALUE OF THE FUNCTION AT POINT PX                --
  ! -- FQDQ0(......,PX) = F(PX)                                         --
  ! ----------------------------------------------------------------------
  FQDQ0(X1,F1,P1,X2,F2,PX) = FD0(X1,F1,P1,X2,F2) + &
    &                              PX * (FD1(X1,F1,P1,X2,F2) + &
    &                                    PX * FD2(X1,F1,P1,X2,F2))
  ! ----------------------------------------------------------------------
  ! -- FQDQ1 GIVES THE VALUE OF THE FIRST DERIVATIVE OF F(X) AT PX      --
  ! -- FQDQ1(......,PX) = DF/DX (PX)                                    --
  ! ----------------------------------------------------------------------
  FQDQ1(X1,F1,P1,X2,F2,PX) = FD1(X1,F1,P1,X2,F2) + &
    &                              ZTWO* PX * FD2(X1,F1,P1,X2,F2)
  ! ----------------------------------------------------------------------
  ! -- FQDQ2 GIVES THE VALUE OF THE SECOND DERIVATIVE OF F(X) AT PX     --
  ! -- FQDQ2(......,PX) = D2F/DX2 (PX)                                  --
  ! ----------------------------------------------------------------------
  FQDQ2(X1,F1,P1,X2,F2) = ZTWO * FD2(X1,F1,P1,X2,F2)
  ! ----------------------------------------------------------------------
  ! -- FQDQM1 GIVES THE VALUE OF THE INTEGRAL OF F(X) FROM X1 TO PX:
  !
  FQDQM1(X1,F1,P1,X2,F2,PX) = &
    & (PX-X1)*(FD0(X1,F1,P1,X2,F2) + &
    &  0.5_RKIND*(PX+X1)*FD1(X1,F1,P1,X2,F2) + &
    &  FD2(X1,F1,P1,X2,F2)/3._RKIND*(PX*(PX+X1)+X1*X1))
  !-----------------------------------------------------------------------
  FPARABOLP(X1,X2,X3,F1,F2,F3,PX) = &
    &  ((PX-X1)+(PX-X2))*F3/((X3-X1)*(X3-X2))+ &
    &  ((PX-X1)+(PX-X3))*F2/((X2-X1)*(X2-X3))+ &
    &  ((PX-X2)+(PX-X3))*F1/((X1-X2)*(X1-X3))
  !.......................................................................
  !     LINEAR
  !
  FLINEAR(X1,F1,X2,F2,PX) = F1 + (PX-X1)/(X2-X1) * (F2-F1)
  FLINEARP(X1,F1,X2,F2) = (F2-F1) / (X2-X1)
  FLINEARM1(X1,F1,X2,F2,PX) = (PX-X1)*(F1+0.5_RKIND*(PX-X1)*(F2-F1)/(X2-X1))
  FLINXYP(X1,F1,P1,PX) = P1*(PX-X1) + F1
  FLINXYPM1(X1,F1,P1,PX) = (PX-X1)*(F1+0.5_RKIND*P1*(PX-X1))
  !
  !-----------------------------------------------------------------------
  ! 0. DEFAULTS
  !
  ICONTDER = 1
  IF (KOPTXPOL .LT. 0) ICONTDER = 0
  IOPTXPOL=ABS(KOPTXPOL)
  INBC = 0
  IF (PRESENT(NBC)) INBC = NBC
  !
  ! DETERMINE DERIVATIVE ZYINP(I) SUCH THAT QUADRATICS ARE THEN DEFINED BY XI, YI, YPI, XI+1, YI+1 WITHIN [XI,XI+1]
  ! VARIOUS OPTIONS ^CHOSEN THROUGH NBC
  !
!!$  ACOEF(1:KNIN-1) = PYIN(1:KNIN-1)
!!$  CCOEF(1:KNIN-1) = PYIN(2:KNIN)
  IF (INBC .EQ. 1) THEN
    ! FIND COEFFICIENTS: Y(X)=AI*(XI+1-X)^2 + BI*(XI+1-X)*(X-XI) + CI*(X-XI)^2 FOR [XI,XI+1], I=1,N-1
    ! AI = YI ; CI=YI+1
    ! BI/HI + BI+1/HI+1 = 2 YI+1 (1/HI+1/HI+1) FOR CONTINUITY OF DERIVATIVES
    ! NEED ONE BOUNDARY CONDITION
    ! d2y/dx2=0 at knin
    BCOEF(KNIN-1) = PYIN(KNIN-1) + PYIN(KNIN)
    DO I=KNIN-2,1,-1
      BCOEF(I) = (PXIN(I+1)-PXIN(I))*(-BCOEF(I+1)/(PXIN(I+2)-PXIN(I+1)) + & 
        & 2._RKIND*PYIN(I+1)*(1._RKIND/(PXIN(I+1)-PXIN(I))+1._RKIND/(PXIN(I+2)-PXIN(I+1))))
    END DO
    ! d2y/dx2=0 at 1
    !equiv    BCOEF(1) = PYIN(1) + PYIN(2)
    !equiv    DO I=2,KNIN-1
    !equiv      BCOEF(I) = (PXIN(I+1)-PXIN(I))*(-BCOEF(I-1)/(PXIN(I)-PXIN(I-1)) + & 
    !equiv        & 2._RKIND*PYIN(I)*(1._RKIND/(PXIN(I)-PXIN(I-1))+1._RKIND/(PXIN(I+1)-PXIN(I))))
    !equiv    END DO
    DO I=1,KNIN-1
      ZYINP(I) = (BCOEF(I)-2._RKIND*PYIN(I))/(PXIN(I+1)-PXIN(I))
    END DO
    ZYINP(KNIN) = (-BCOEF(KNIN-1)+2._RKIND*PYIN(KNIN))/(PXIN(KNIN)-PXIN(KNIN-1))
  ELSE
    ! USE 3 POINTS TO DEFINE QUADRATICS EXCEPT FOR LAST INTERVALS
    DO I=1,KNIN-2
      ZYINP(I) = FPARABOLP(PXIN(I),PXIN(I+1),PXIN(I+2),PYIN(I),PYIN(I+1),PYIN(I+2),PXIN(I))
    END DO
    ZYINP(KNIN-1) = FPARABOLP(PXIN(KNIN-2),PXIN(KNIN-1),PXIN(KNIN),PYIN(KNIN-2),PYIN(KNIN-1),PYIN(KNIN),PXIN(KNIN-1))
    ZYINP(KNIN) = FPARABOLP(PXIN(KNIN-2),PXIN(KNIN-1),PXIN(KNIN),PYIN(KNIN-2),PYIN(KNIN-1),PYIN(KNIN),PXIN(KNIN))
  END IF
  !write(*,'(1p2e14.5)') (pxin(i),zyinp(i),i=1,knin)
  !
  ! COMPUTE INT. UP TO EACH INPUT INTERVAL
  IF (KOPTDER .GE. 3) THEN
    ZYINT_XIN(1) = 0._RKIND
    DO I=1,KNIN-1
      ZYINT_XIN(I+1) = ZYINT_XIN(I) + FQDQM1(PXIN(I),PYIN(I),ZYINP(I),PXIN(I+1),PYIN(I+1),PXIN(I+1))
    END DO
  END IF
  !
  ! LOOP OVER PXOUT POINTS WHICH CAN BE IN RANDOM ORDER
  DO J=1,KNOUT
    IF ((PXOUT(J) .LT. PXIN(1)) .OR. (PXOUT(J) .GT. PXIN(KNIN))) GO TO 200
    !
    ! 1.1 POINTS INSIDE INTERVAL [XIN(1),XIN(KNIN)]
    ! FIND PXIN INTERVAL BY BI-SECTION
    KLO=1
    KHI=KNIN
10  CONTINUE
    IF (KHI-KLO.GT.1) THEN
      K=(KHI+KLO)/2
      IF(PXIN(K) .GT. PXOUT(J))THEN
        KHI=K
      ELSE
        KLO=K
      ENDIF
      GOTO 10
    ENDIF
    SELECT CASE (KOPTDER)
    CASE (0)
      PYOUT(J)=FQDQ0(PXIN(KLO),PYIN(KLO),ZYINP(KLO),PXIN(KHI),PYIN(KHI),PXOUT(J))
    CASE (1)
      PYOUT(J)=FQDQ0(PXIN(KLO),PYIN(KLO),ZYINP(KLO),PXIN(KHI),PYIN(KHI),PXOUT(J))
      PYOUTP(J)=FQDQ1(PXIN(KLO),PYIN(KLO),ZYINP(KLO),PXIN(KHI),PYIN(KHI),PXOUT(J))
    CASE (2)
      PYOUT(J)=FQDQ0(PXIN(KLO),PYIN(KLO),ZYINP(KLO),PXIN(KHI),PYIN(KHI),PXOUT(J))
      PYOUTP(J)=FQDQ1(PXIN(KLO),PYIN(KLO),ZYINP(KLO),PXIN(KHI),PYIN(KHI),PXOUT(J))
      PYOUTPP(J)=FQDQ2(PXIN(KLO),PYIN(KLO),ZYINP(KLO),PXIN(KHI),PYIN(KHI))
    CASE (3)
      PYOUT(J)=FQDQ0(PXIN(KLO),PYIN(KLO),ZYINP(KLO),PXIN(KHI),PYIN(KHI),PXOUT(J))
      PYOUTP(J)=FQDQ1(PXIN(KLO),PYIN(KLO),ZYINP(KLO),PXIN(KHI),PYIN(KHI),PXOUT(J))
      PYOUTPP(J)=FQDQ2(PXIN(KLO),PYIN(KLO),ZYINP(KLO),PXIN(KHI),PYIN(KHI))
      PYOUTINT(J) = ZYINT_XIN(KLO) + FQDQM1(PXIN(KLO),PYIN(KLO),ZYINP(KLO),PXIN(KHI),PYIN(KHI),PXOUT(J))
    END SELECT
    !
    GO TO 100
    !
    !     2 POINT OUTSIDE INTERVAL
    !
200 CONTINUE
    !
    !     2.1 IF KOPTXPOL=0, PRINT WARNING AND RETURN OR STOP
    !
    IF (IOPTXPOL .EQ. 0) THEN
      PRINT *,' PXOUT(',J,')=',PXOUT(J),' OUTSIDE INTERVAL [',PXIN(1),',',PXIN(KNIN),']'
      RETURN
      !        STOP 'IOPTXPOL=0'
    ENDIF
    !
    !     2.2 COMPUTE VALUES FOR POINTS ON THE LEFT OF PXIN(1)
    !           EXTRAPOLATION DEPENDS ON VALUE OF KOPTXPOL
    !
    IF (PXOUT(J) .LT. PXIN(1)) THEN
      ! ZY1 REFERS TO FIRST KNOWN POINTS AND ZY2 TO SECOND KNOWN POINT, 
      ! TYPICALLY AT PXIN(1) AND PXIN(2) OR PXIN(1)-ALFA*H AND PXIN(1)
      ZXLFTDEL = PXIN(1) - ALFA*(PXIN(2) - PXIN(1))
      !
      !   2.2.1 SPECIAL PART [XIN(1)-ALFA*H,XIN(1)] IF IOPTXPOL>20
      IF ((PXOUT(J) .GE. ZXLFTDEL) .AND. (IOPTXPOL.EQ.21)) THEN
        ! QUADRATIC PART
        IF (ICONTDER .EQ. 1) THEN
          PYOUT(J) = FQDQ0(PXIN(1),PYIN(1),ZYINP(1),PXIN(2),PYIN(2),PXOUT(J))
          IF (KOPTDER .GE. 1) PYOUTP(J) = FQDQ1(PXIN(1),PYIN(1),ZYINP(1),PXIN(2),PYIN(2),PXOUT(J))
          IF (KOPTDER .GE. 2) PYOUTPP(J) = FQDQ2(PXIN(1),PYIN(1),ZYINP(1),PXIN(2),PYIN(2))
          ! INTEGRATES DIRECTLY FROM PXIN(1)
          IF (KOPTDER .GE. 3) PYOUTINT(J) = FQDQM1(PXIN(1),PYIN(1),ZYINP(1),PXIN(2),PYIN(2),PXOUT(J))
        ELSE
          ! KOPTXPOL = -21
          PYOUT(J) = FQQQ0(PYIN(1),PYIN(2),PYIN(3),PXIN(1),PXIN(2),PXIN(3),PXOUT(J))
          IF (KOPTDER .GE. 1) PYOUTP(J) = FQQQ1(PYIN(1),PYIN(2),PYIN(3),PXIN(1),PXIN(2),PXIN(3),PXOUT(J))
          IF (KOPTDER .GE. 2) PYOUTPP(J) = FQQQ2(PYIN(1),PYIN(2),PYIN(3),PXIN(1),PXIN(2),PXIN(3))
          IF (KOPTDER .GE. 3) PYOUTINT(J) = FQQQM1(PYIN(1),PYIN(2),PYIN(3),PXIN(1),PXIN(2),PXIN(3),PXOUT(J))
        END IF
        !
      ELSE
        !
        ! 2.2.2 EXTRAPOLATION FAR LEFT: X<XIN(1)-ALFA*H OR X<XIN(1) IF NO ALFA PART CONSIDERED
        !
        IF ((IOPTXPOL .EQ. 1) .OR. (IOPTXPOL .EQ. 21)) THEN
          ! LINEAR EXTRAPOLATION
          SELECT CASE (KOPTXPOL)
          CASE (1)
            ZX1 = PXIN(1)
            ZY1 = PYIN(1)
            ZY1P = ZYINP(1)
            ZYINT = 0._RKIND
          CASE(-1)
            ZX1 = PXIN(1)
            ZY1 = PYIN(1)
            ZY1P = FLINEARP(PXIN(1),PYIN(1),PXIN(2),PYIN(2))
            ZYINT = 0._RKIND
          CASE (21)
            ZX1 = ZXLFTDEL
            ZY1 = FQDQ0(PXIN(1),PYIN(1),ZYINP(1),PXIN(2),PYIN(2),ZXLFTDEL)
            ZY1P = FQDQ1(PXIN(1),PYIN(1),ZYINP(1),PXIN(2),PYIN(2),ZXLFTDEL)
            ZYINT = FQDQM1(PXIN(1),PYIN(1),ZYINP(1),PXIN(2),PYIN(2),ZXLFTDEL)
          CASE (-21)
            ZX1 = ZXLFTDEL
            ZY1 = FQQQ0(PYIN(1),PYIN(2),PYIN(3),PXIN(1),PXIN(2),PXIN(3),ZXLFTDEL)
            ZY1P = FQQQ1(PYIN(1),PYIN(2),PYIN(3),PXIN(1),PXIN(2),PXIN(3),ZXLFTDEL)
            ZYINT = FQQQM1(PYIN(1),PYIN(2),PYIN(3),PXIN(1),PXIN(2),PXIN(3),ZXLFTDEL)
          END SELECT
          PYOUT(J) = FLINXYP(ZX1,ZY1,ZY1P,PXOUT(J))
          IF (KOPTDER .GE. 1) PYOUTP(J) = ZY1P
          IF (KOPTDER .GE. 2) PYOUTPP(J) = 0._RKIND
          IF (KOPTDER .GE. 3) PYOUTINT(J) = ZYINT + FLINXYPM1(ZX1,ZY1,ZY1P,PXOUT(J))
        ELSE IF (IOPTXPOL .EQ. 10) THEN
          ! CONSTANT OUTSIDE PXIN
          IF (ICONTDER .EQ. 1) THEN
            ! KOPTXPOL = +10
            ZYIN_XPOL = PYIN(1)
          ELSE
            ! KOPTXPOL = -10
            ZYIN_XPOL = 0._RKIND
          END IF
          PYOUT(J) = ZYIN_XPOL
          IF (KOPTDER .GE. 1) PYOUTP(J) = 0._RKIND
          IF (KOPTDER .GE. 2) PYOUTPP(J) = 0._RKIND
          ! INTEGRATES FROM PXIN(1) DIRECTLY
          IF (KOPTDER .GE. 3) PYOUTINT(J) = (PXOUT(J)-PXIN(1))*ZYIN_XPOL
          !
        ELSE
          ! QUADRATIC EXTRAPOLATION
          ZX1 = PXIN(1)
          ZY1 = PYIN(1)
          ZX2 = PXIN(2)
          ZY2 = PYIN(2)
          ZYINT = 0._RKIND
          IF (ICONTDER .EQ. 1) THEN
            ! KOPTXPOL = 2
            ZY1P = ZYINP(1)
          ELSE
            ! KOPTXPOL = -2
            ZY1P = FQQQ1(PYIN(1),PYIN(2),PYIN(3),PXIN(1),PXIN(2),PXIN(3),PXIN(1))
          ENDIF
          PYOUT(J) = FQDQ0(ZX1,ZY1,ZY1P,ZX2,ZY2,PXOUT(J))
          IF (KOPTDER .GE. 1) PYOUTP(J) = FQDQ1(ZX1,ZY1,ZY1P,ZX2,ZY2,PXOUT(J))
          IF (KOPTDER .GE. 2) PYOUTPP(J) = FQDQ2(ZX1,ZY1,ZY1P,ZX2,ZY2)
          IF (KOPTDER .GE. 3) PYOUTINT(J) = ZYINT + FQDQM1(ZX1,ZY1,ZY1P,ZX2,ZY2,PXOUT(J))
        END IF
      END IF
      !
    ELSE ! PXOUT(J) .GT. PXIN(KNIN)
      !
      !     2.3 COMPUTE VALUES FOR POINTS ON THE RIGHT OF PXIN(1)
      !         EXTRAPOLATION DEPENDS ON VALUE OF KOPTXPOL
      !
      ! ZYN REFERS TO FIRST KNOWN POINTS AND ZYNN TO SECOND KNOWN POINT, 
      ! TYPICALLY AT PXIN(KNIN) AND PXIN(KNIN-1) OR PXIN(KNIN)+ALFA*H AND PXIN(KNIN)
      ! YP(PXIN(KNIN)) FROM SPLINE
      ZXRGTDEL = PXIN(KNIN) + ALFA * (PXIN(KNIN) - PXIN(KNIN-1))
      !
      !   2.3.1 SPECIAL PART ]XIN(KNIN),XIN(KNIN)+ALFA*H] IF IOPTXPOL>20
      IF ((PXOUT(J) .LE. ZXRGTDEL) .AND. (IOPTXPOL.EQ.21)) THEN
        ZYINT = ZYINT_XIN(KNIN)
        ! QUADRATIC
        IF (ICONTDER .EQ. 1) THEN
          ! KOPTXPOL = +21
          PYOUT(J) = FQDQ0(PXIN(KNIN),PYIN(KNIN),ZYINP(KNIN),PXIN(KNIN-1),PYIN(KNIN-1),PXOUT(J))
          IF (KOPTDER .GE. 1) PYOUTP(J) = FQDQ1(PXIN(KNIN),PYIN(KNIN),ZYINP(KNIN),PXIN(KNIN-1),PYIN(KNIN-1),PXOUT(J))
          IF (KOPTDER .GE. 2) PYOUTPP(J) = FQDQ2(PXIN(KNIN),PYIN(KNIN),ZYINP(KNIN),PXIN(KNIN-1),PYIN(KNIN-1))
          ! INTEGRATES FROM PXIN(KNIN) SO ADD INTEGRAL UP TO PXIN(KNIN)
          IF (KOPTDER .GE. 3) PYOUTINT(J) = ZYINT + FQDQM1(PXIN(KNIN),PYIN(KNIN),ZYINP(KNIN),PXIN(KNIN-1),PYIN(KNIN-1),PXOUT(J))
        ELSE
          ! KOPTXPOL = -21
          PYOUT(J) = FQQQ0(PYIN(KNIN),PYIN(KNIN-1),PYIN(KNIN-2),PXIN(KNIN),PXIN(KNIN-1),PXIN(KNIN-2),PXOUT(J))
          IF (KOPTDER .GE. 1) PYOUTP(J) = &
               & FQQQ1(PYIN(KNIN),PYIN(KNIN-1),PYIN(KNIN-2),PXIN(KNIN),PXIN(KNIN-1),PXIN(KNIN-2),PXOUT(J))
          IF (KOPTDER .GE. 2) PYOUTPP(J) = &
               & FQQQ2(PYIN(KNIN),PYIN(KNIN-1),PYIN(KNIN-2),PXIN(KNIN),PXIN(KNIN-1),PXIN(KNIN-2))
          IF (KOPTDER .GE. 3) PYOUTINT(J) = ZYINT + &
               & FQQQM1(PYIN(KNIN),PYIN(KNIN-1),PYIN(KNIN-2),PXIN(KNIN),PXIN(KNIN-1),PXIN(KNIN-2),PXOUT(J))
        END IF
        !
      ELSE
        !
        ! 2.3.2 EXTRAPOLATION FAR RIGHT: X>XIN(KNIN)+ALFA*H OR X>XIN(KNIN) IF NO ALFA PART CONSIDERED
        !
        IF ((IOPTXPOL .EQ. 1) .OR. (IOPTXPOL .EQ. 21)) THEN
          ! LINEAR EXTRAPOLATION
          SELECT CASE (KOPTXPOL)
          CASE (1)
            ZXN = PXIN(KNIN)
            ZYN = PYIN(KNIN)
            ZYNP = ZYINP(KNIN)
            ZYINT = ZYINT_XIN(KNIN)
          CASE(-1)
            ZXN = PXIN(KNIN)
            ZYN = PYIN(KNIN)
            ZYNP = FLINEARP(PXIN(KNIN),PYIN(KNIN),PXIN(KNIN-1),PYIN(KNIN-1))
            ZYINT = ZYINT_XIN(KNIN)
          CASE (21)
            ZYINT = ZYINT_XIN(KNIN) + FQDQM1(PXIN(KNIN),PYIN(KNIN),ZYINP(KNIN),PXIN(KNIN-1),PYIN(KNIN-1),ZXRGTDEL)
            ZXN = ZXRGTDEL
            ZYN = FQDQ0(PXIN(KNIN),PYIN(KNIN),ZYINP(KNIN),PXIN(KNIN-1),PYIN(KNIN-1),ZXRGTDEL)
            ZYNP = FQDQ1(PXIN(KNIN),PYIN(KNIN),ZYINP(KNIN),PXIN(KNIN-1),PYIN(KNIN-1),ZXRGTDEL)
          CASE (-21)
            ZYINT = ZYINT_XIN(KNIN) + &
              & FQQQM1(PYIN(KNIN),PYIN(KNIN-1),PYIN(KNIN-2),PXIN(KNIN),PXIN(KNIN-1),PXIN(KNIN-2),ZXRGTDEL)
            ZXN = ZXRGTDEL
            ZYN = FQQQ0(PYIN(KNIN),PYIN(KNIN-1),PYIN(KNIN-2),PXIN(KNIN),PXIN(KNIN-1),PXIN(KNIN-2),ZXRGTDEL)
            ZYNP = FQQQ1(PYIN(KNIN),PYIN(KNIN-1),PYIN(KNIN-2),PXIN(KNIN),PXIN(KNIN-1),PXIN(KNIN-2),ZXRGTDEL)
          END SELECT
          PYOUT(J) = FLINXYP(ZXN,ZYN,ZYNP,PXOUT(J))
          IF (KOPTDER .GE. 1) PYOUTP(J) = ZYINP(KNIN)
          IF (KOPTDER .GE. 2) PYOUTPP(J) = 0._RKIND
          IF (KOPTDER .GE. 3) PYOUTINT(J) = ZYINT + FLINXYPM1(ZXN,ZYN,ZYNP,PXOUT(J))
        ELSE IF (IOPTXPOL .EQ. 10) THEN
          ! CONSTANT OUTSIDE PXIN
          IF (ICONTDER .EQ. 1) THEN
            ! KOPTXPOL = +10
            ZYIN_XPOL = PYIN(KNIN)
          ELSE
            ! KOPTXPOL = -10
            ZYIN_XPOL = 0._RKIND
          END IF
          PYOUT(J) = ZYIN_XPOL
          IF (KOPTDER .GE. 1) PYOUTP(J) = 0._RKIND
          IF (KOPTDER .GE. 2) PYOUTPP(J) = 0._RKIND
          IF (KOPTDER .GE. 3) PYOUTINT(J) = ZYINT_XIN(KNIN) + (PXOUT(J)-PXIN(KNIN))*ZYIN_XPOL
          !
        ELSE
          ! QUADRATIC EXTRAPOLATION
          ZXN = PXIN(KNIN)
          ZYN = PYIN(KNIN)
          ZXNN = PXIN(KNIN-1)
          ZYNN = PYIN(KNIN-1)
          ZYINT = ZYINT_XIN(KNIN)
          IF (KOPTXPOL .EQ. -2) ZYINP(KNIN) = &
               & FQQQ1(PYIN(KNIN),PYIN(KNIN-1),PYIN(KNIN-2),PXIN(KNIN),PXIN(KNIN-1),PXIN(KNIN-2),PXIN(KNIN))
          PYOUT(J) = FQDQ0(ZXN,ZYN,ZYINP(KNIN),ZXNN,ZYNN,PXOUT(J))
          IF (KOPTDER .GE. 1) PYOUTP(J) = FQDQ1(ZXN,ZYN,ZYINP(KNIN),ZXNN,ZYNN,PXOUT(J))
          IF (KOPTDER .GE. 2) PYOUTPP(J) = FQDQ2(ZXN,ZYN,ZYINP(KNIN),ZXNN,ZYNN)
          IF (KOPTDER .GE. 3) PYOUTINT(J) = ZYINT + FQDQM1(ZXN,ZYN,ZYINP(KNIN),ZXNN,ZYNN,PXOUT(J))
        END IF
      END IF
    END IF
    !
100 CONTINUE
  END DO
  RETURN 
END SUBROUTINE INTQUADRATIC
