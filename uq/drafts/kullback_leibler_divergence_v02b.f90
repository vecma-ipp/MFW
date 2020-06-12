module f90_kind

    integer, parameter :: rkind   = kind(1.d0)
    integer, parameter :: ikind   = kind(1)
    integer, parameter :: ckind   = kind('a')
    INTEGER, PARAMETER :: lkind   = KIND(.true.)
end module f90_kind

MODULE math_routines
  USE f90_kind
  IMPLICIT NONE
  REAL(rkind), PARAMETER :: pi=3.14159265358979_rkind
  REAL(rkind), PARAMETER :: ln2pi=1.837877066409_rkind
  REAL(rkind) :: xtest,x_integration_lb=-10.d0,x_integration_ub=17.5d0
  !INTEGER(ikind) :: divergence
  LOGICAL(lkind) :: xtest_new
  CONTAINS

    ELEMENTAL FUNCTION mean1(x)
        IMPLICIT NONE
        REAL(rkind), PARAMETER :: center=50.0
        REAL(rkind), INTENT(IN) :: x !expected to be in 0..100
        REAL(rkind) :: mean1
        !mean1=(x-center)**2/500.0 !mean in 0..5
        mean1=1.5
    END FUNCTION mean1

    ELEMENTAL FUNCTION mean2(x)
        IMPLICIT NONE
        REAL(rkind), PARAMETER :: center=50.0
        REAL(rkind), INTENT(IN) :: x !expected to be in 0..100
        REAL(rkind) :: mean2
        mean2=2.5 ! constant at 2.5
    END FUNCTION mean2

    ELEMENTAL FUNCTION sigma_a_1(x)
        IMPLICIT NONE
        !uncertainty 'above' mean -> right of mean
        REAL(rkind), INTENT(IN) :: x !expected to be in 0..100
        REAL(rkind) :: sigma_a_1
        sigma_a_1=0.2 ! constant at 0.2
    END FUNCTION sigma_a_1

    ELEMENTAL FUNCTION sigma_b_1(x)
        IMPLICIT NONE
        !uncertainty 'below' mean -> left of mean
        REAL(rkind), INTENT(IN) :: x !expected to be in 0..100
        REAL(rkind) :: sigma_b_1
        sigma_b_1=0.2 ! constant at 1
    END FUNCTION sigma_b_1

    ELEMENTAL FUNCTION sigma_a_2(x)
        IMPLICIT NONE
        REAL(rkind), INTENT(IN) :: x !expected to be in 0..100
        REAL(rkind) :: sigma_a_2 !right
        sigma_a_2=0.2 ! constant at 0.2
    END FUNCTION sigma_a_2

    ELEMENTAL FUNCTION sigma_b_2(x)
        IMPLICIT NONE
        REAL(rkind), INTENT(IN) :: x !expected to be in 0..100
        REAL(rkind) :: sigma_b_2 !left
        sigma_b_2=2.01-(50-x)**2/1250.0 
        !sigma_b_2=2.01-(50-x)**2/5000.0
    END FUNCTION sigma_b_2

    ELEMENTAL FUNCTION density1(x)
        IMPLICIT NONE
        REAL(rkind), INTENT(IN) :: x !expected to be in 0..100
        REAL(rkind) :: density1
        REAL(rkind) :: mu,sa,sb !mean, sa = variance to the right,sb = variance to the left
        mu=mean1(xtest)
        sa=sigma_a_1(xtest)
        sb=sigma_b_1(xtest)
        if (x>=mu) then
           density1=0.5/(sqrt(2*pi)*sa) * exp(-0.5*((x-mu)/sa)**2)
        else
           density1=0.5/(sqrt(2*pi)*sb) * exp(-0.5*((x-mu)/sb)**2)
        endif
    END FUNCTION density1

    FUNCTION density1_v(x,choice)
        !vector version of density1
        IMPLICIT NONE
        REAL(rkind), INTENT(IN) :: x(:) !expected to be in 0..100
        REAL(rkind),DIMENSION(size(x)) :: density1_v
        REAL(rkind) :: mu,sa,sb !mean, sa = variance to the right,sb = variance to the left
        INTEGER(ikind), INTENT(IN) :: choice
        mu=mean1(xtest)
        sa=sigma_a_1(xtest)
        sb=sigma_b_1(xtest)
        where (x>=mu) 
           density1_v=0.5/(sqrt(2*pi)*sa) * exp(-0.5*((x-mu)/sa)**2)
        elsewhere 
           density1_v=0.5/(sqrt(2*pi)*sb) * exp(-0.5*((x-mu)/sb)**2)
        endwhere
    END FUNCTION density1_v

    ELEMENTAL FUNCTION density2(x)
        IMPLICIT NONE
        REAL(rkind), INTENT(IN) :: x !expected to be in 0..100
        REAL(rkind) :: density2
        REAL(rkind) :: mu,sa,sb !mean, sa = variance to the right,sb = variance to the left
        mu=mean2(xtest)
        sa=sigma_a_2(xtest)
        sb=sigma_b_2(xtest)
        if (x>=mu) then
           density2=0.5/(sqrt(2*pi)*sa) * exp(-0.5*((x-mu)/sa)**2)
        else
           density2=0.5/(sqrt(2*pi)*sb) * exp(-0.5*((x-mu)/sb)**2)
        endif
    END FUNCTION density2

    FUNCTION density2_v(x,choice)
        !vector version of density2
        IMPLICIT NONE
        REAL(rkind), INTENT(IN) :: x(:) !expected to be in 0..100
        REAL(rkind),DIMENSION(size(x)) :: density2_v
        REAL(rkind) :: mu,sa,sb !mean, sa = variance to the right,sb = variance to the left
        INTEGER(ikind), INTENT(IN) :: choice
        mu=mean2(xtest)
        sa=sigma_a_2(xtest)
        sb=sigma_b_2(xtest)
        where (x>=mu)
           density2_v=0.5/(sqrt(2*pi)*sa) * exp(-0.5*((x-mu)/sa)**2)
        elsewhere
           density2_v=0.5/(sqrt(2*pi)*sb) * exp(-0.5*((x-mu)/sb)**2)
        endwhere
    END FUNCTION density2_v

    SUBROUTINE calc_cumulative_function(y_cumulative,x_support_points,func,x_integration_lb,x_integration_ub,choice)
        IMPLICIT NONE
        REAL(rkind), INTENT(IN) :: x_support_points(:)
        REAL(rkind), INTENT(IN) :: x_integration_lb,x_integration_ub
        REAL(rkind), INTENT(OUT):: y_cumulative(:)
        INTEGER(ikind), INTENT(IN) :: choice
        INTEGER(ikind) :: i,n
        INTERFACE
                FUNCTION func(x,choice)
                USE f90_kind
                REAL(rkind), DIMENSION(:), INTENT(IN) :: x
                REAL(rkind), DIMENSION(size(x)) :: func
                INTEGER(ikind), INTENT(IN) :: choice
                END FUNCTION func
        END INTERFACE
        n=assert_eq('ERROR: size-mismatch in calc_cumulative.',size(x_support_points),size(y_cumulative))
        DO i=1,n
           y_cumulative(i)=qsimp(func,x_integration_lb,x_support_points(i),choice)
        ENDDO
    END SUBROUTINE calc_cumulative_function

    FUNCTION divergence_func(x,choice)
        IMPLICIT NONE
        INTEGER(ikind), PARAMETER :: n_support_points=400
        REAL(rkind), INTENT(IN) :: x(:)
        INTEGER(ikind), INTENT(IN) :: choice
        REAL(rkind),DIMENSION(SIZE(x)) :: divergence_func
        REAL(rkind),DIMENSION(size(x)) :: tmp_m 
        REAL(rkind),DIMENSION(SIZE(x)) :: term1_cum,term2_cum
        REAL(rkind),DIMENSION(n_support_points),SAVE :: x_support_points,y_cum1,y_cum2
        INTEGER(ikind) :: i,idx        

        SELECT CASE (choice)
           CASE (1)          !Hellinger-Distance squared H^2(p1;p2) =: 1 - int_dx [sqrt(p1(x)*p2(x))]
                             ! 0<= H^2 <= 1
              divergence_func=sqrt(density1(x)*density2(x))
              where (divergence_func/=divergence_func) divergence_func=0.d0 !Avoid NaNs 
              !DO i=1,size(x)
              !   write(*,*)i,x(i),density1(x(i)),density2(x(i)),divergence_func(i)
              !   read(*,*)
              !ENDDO
           CASE (2)          !Jensen-Shannon-divergence := 1/2 (D(P|M) + D(Q|M)) with M=1/2(P+Q) and D(P|Q)=Kullback-Leibler 
                             ! 0<= JS <= ln(2) 
                             ! D(P|Q) = int_dx [p(x) * log (p(x)/q(x) ]                            
              tmp_m=0.5*(density1(x)+density2(x))
              divergence_func=0.5*( density1(x)*log(density1(x)/tmp_m) + density2(x)*log(density2(x)/tmp_m) )
              where (divergence_func/=divergence_func) divergence_func=0.d0
              !DO i=1,size(x)
              !   write(*,*)i,x(i),density1(x(i)),density1(x(i))*log(density1(x(i))/tmp_m(i)),density2(x(i)),&
              !                    divergence_func(i),tmp_m(i)
              !   read(*,*)
              !ENDDO

           CASE (3)        !Wasserstein-metric: int_dx abs (cum(p1(-inf..x))-cum(p2(-inf..x))) : Difference of cumulative functions
              if (xtest_new) then
                 x_support_points=arth_r(x_integration_lb,(x_integration_ub-x_integration_lb)/(size(x_support_points)-1),&
                                         size(x_support_points))
                 call calc_cumulative_function(y_cum1,x_support_points,density1_v,x_integration_lb,x_integration_ub,choice)
                 call calc_cumulative_function(y_cum2,x_support_points,density2_v,x_integration_lb,x_integration_ub,choice)
                 xtest_new=.FALSE.  !now we have updated cumulative distributions for present value of xtest
                 !DO i=1,n_support_points !output of cumulative functions
                 !   write(50,*)x_support_points(i),y_cum1(i),y_cum2(i)
                 !ENDDO
              endif
              DO i=1,size(x)
                 !compute value of cumulative distribution at location x(i) using linear interpolation
                 idx=locate(x_support_points,x(i)) !index idx such that x_support(idx) <= x(i) < x_support(idx+1)
                 term1_cum(i)=polint(x_support_points(idx:idx+1),y_cum1(idx:idx+1),x(i)) 
                 term2_cum(i)=polint(x_support_points(idx:idx+1),y_cum2(idx:idx+1),x(i))
              ENDDO 
              where (term1_cum/=term1_cum) term1_cum=0.0d0
              where (term2_cum/=term2_cum) term2_cum=0.0d0
              divergence_func=abs(term1_cum-term2_cum)
              !DO i=1,size(x)
              !   write(*,*)i,x(i),term1_cum(i),term2_cum(i),divergence_func(i)
              !   read(*,*)
              !ENDDO
           CASE (4) ! return mean1 ! H-dist approx: distance= (mean1-mean2)**2 / [2*(s1^2 + s2^2) + (mean1-mean2)^2]    
              divergence_func=x*density1(x)
           CASE (5) ! return mean2 
              divergence_func=x*density2(x)   
           CASE (6) ! return second moment (non-centered) of density1
              divergence_func=x*x*density1(x)
           CASE (7) ! return second moment (non-centered) of density2  
              divergence_func=x*x*density2(x)
           CASE (8) !KL(p2||p1)
              where ((density1(x)>1.d-40).AND.(density2(x)>1.d-40)) 
                 divergence_func=(density2(x)*log(density2(x)/density1(x)))
              elsewhere
                 divergence_func=0.d0
              endwhere   
              where (divergence_func/=divergence_func) divergence_func=0.d0
           CASE (9) !KL(p1||p2)
              where ((density1(x)>1.d-40).AND.(density2(x)>1.d-40))
                 divergence_func=(density1(x)*log(density1(x)/density2(x)))
              elsewhere
                 divergence_func=0.d0
              endwhere   
              where (divergence_func/=divergence_func) divergence_func=0.d0
           CASE(10) ! return third moment (non-centered) of density1
              divergence_func=x*x*x*density1(x)
           CASE(11) ! return third moment (non-centered) of density2
              divergence_func=x*x*x*density2(x)   
           CASE DEFAULT
                write(*,*)'ERROR: this divergence function is not implemented. choice=',choice
        END SELECT  
    END FUNCTION divergence_func

    FUNCTION gammln(xx)
        IMPLICIT NONE
        REAL(rkind), INTENT(IN) :: xx
        REAL(rkind) :: gammln
        REAL(rkind) :: tmp,x,y,ser
        REAL(rkind) :: stp = 2.5066282746310005_rkind
        REAL(rkind), DIMENSION(6) :: coef = (/76.18009172947146_rkind,&
                -86.50532032941677_rkind,24.01409824083091_rkind,&
                -1.231739572450155_rkind,0.1208650973866179e-2_rkind,&
                -0.5395239384953e-5_rkind/)
        INTEGER(ikind)::j

        x=xx
        y=x
        tmp=x+5.5_rkind
        tmp=(x+0.5_rkind)*log(tmp)-tmp       
        ser=1.000000000190015_rkind
        DO j=1,6
           y=y+1.0_rkind
           ser=ser+coef(j)/y
        ENDDO
        gammln=tmp+log(stp*ser/x)
    END FUNCTION gammln

    SUBROUTINE swap(a,b)
        REAL(rkind), INTENT(INOUT) :: a,b
        REAL(rkind) :: dum
        dum=a
        a=b
        b=dum
    END SUBROUTINE swap

    SUBROUTINE swap_v(a,b)
        REAL(rkind), DIMENSION(:), INTENT(INOUT) :: a,b
        REAL(rkind), DIMENSION(SIZE(a)) :: dum
        dum=a
        a=b
        b=dum
     END SUBROUTINE swap_v 

     SUBROUTINE masked_swap(a,b,mask)
        REAL(rkind), INTENT(INOUT) :: a,b
        LOGICAL(lkind), INTENT(IN) :: mask
        REAL(rkind) :: swp
        if (mask) then
                swp=a
                a=b
                b=swp
        end if
     END SUBROUTINE masked_swap

     SUBROUTINE sort_heap(arr)
        IMPLICIT NONE
        REAL(rkind), DIMENSION(:), INTENT(INOUT) :: arr
        INTEGER(ikind) :: i,n
        n=size(arr)
        do i=n/2,1,-1
                call sift_down(i,n)
        end do
        do i=n,2,-1
                call swap(arr(1),arr(i))
                call sift_down(1,i-1)
        end do
        CONTAINS
!BL
           SUBROUTINE sift_down(l,r)
              INTEGER(ikind), INTENT(IN) :: l,r
              INTEGER(ikind) :: j,jold
              REAL(rkind) :: a
              a=arr(l)
              jold=l
              j=l+l
              do
                if (j > r) exit
                if (j < r) then
                        if (arr(j) < arr(j+1)) j=j+1
                end if
                if (a >= arr(j)) exit
                arr(jold)=arr(j)
                jold=j
                j=j+j
              end do
              arr(jold)=a
           END SUBROUTINE sift_down
     END SUBROUTINE sort_heap

     FUNCTION pythag(a,b)
        IMPLICIT NONE
	REAL(rkind), INTENT(IN) :: a,b
	REAL(rkind) :: pythag
	REAL(rkind) :: absa,absb
        absa=abs(a)
        absb=abs(b)
        if (absa > absb) then
           pythag=absa*sqrt(1.0_rkind+(absb/absa)**2)
        else
           if (absb == 0.0) then
               pythag=0.0
           else
               pythag=absb*sqrt(1.0_rkind+(absa/absb)**2)
           end if
        end if
     END FUNCTION pythag

     FUNCTION locate(xx,x)
        !given an array xx(1:N) and a value of x, returns a integer value j such that x is between xx(j) and xx(j+1)
        !return values of 0 or N indicate that x is out of range; method: bisection + end-point-correction
        IMPLICIT NONE
        REAL(rkind), DIMENSION(:), INTENT(IN) :: xx
        REAL(rkind), INTENT(IN) :: x
        INTEGER(ikind) :: locate
        INTEGER(ikind) :: n,jl,jm,ju
        LOGICAL :: ascnd
        n=size(xx)
        ascnd = (xx(n) >= xx(1))
        jl=0
        ju=n+1
        do
                if (ju-jl <= 1) exit
                jm=(ju+jl)/2
                if (ascnd .eqv. (x >= xx(jm))) then
                        jl=jm
                else
                        ju=jm
                end if
        end do
        if (x == xx(1)) then
                locate=1
        else if (x == xx(n)) then
                locate=n-1
        else
                locate=jl
        end if
     END FUNCTION locate

     FUNCTION polint(xa,ya,x)
        !given array xa(1:N),ya(1:N) a polynom interpolation of degree (N-1) is calculated
        !for the point x: y(x) = polynom_interpolation(x;xa,ya,degree(N-1) )
        !error : dy 
        IMPLICIT NONE
        REAL(rkind), DIMENSION(:), INTENT(IN) :: xa,ya
        REAL(rkind), INTENT(IN) :: x
        !REAL(rkind), INTENT(OUT) :: y,dy
        REAL(rkind) :: polint
        REAL(rkind) :: y,dy
        INTEGER(ikind) :: m,n,ns
        REAL(rkind), DIMENSION(size(xa)) :: c,d,den,ho
        n=assert_eq('polint',size(xa),size(ya))
        c=ya
        d=ya
        ho=xa-x
        ns=iminloc(abs(x-xa))
        y=ya(ns)
        ns=ns-1
        do m=1,n-1
                den(1:n-m)=ho(1:n-m)-ho(1+m:n)
                if (any(den(1:n-m) == 0.0)) then
                   write(*,*)'ERROR: polint: calculation failure'
                   stop
                endif
                den(1:n-m)=(c(2:n-m+1)-d(1:n-m))/den(1:n-m)
                d(1:n-m)=ho(1+m:n)*den(1:n-m)
                c(1:n-m)=ho(1:n-m)*den(1:n-m)
                if (2*ns < n-m) then
                        dy=c(ns+1)
                else
                        dy=d(ns)
                        ns=ns-1
                end if
                y=y+dy
        end do
        polint=y !return value
     END FUNCTION polint

     FUNCTION assert_eq(string,n1,n2,n3)
        CHARACTER(LEN=*), INTENT(IN) :: string
        INTEGER, INTENT(IN) :: n1,n2
        INTEGER, INTENT(IN),OPTIONAL ::n3
        INTEGER(ikind) :: assert_eq,n_dummy
        if (present(n3)) then
           n_dummy=n3
        else 
           n_dummy=n1
        endif
        if (n1 == n2 .and. n2 == n_dummy) then
                assert_eq=n1
        else
                write (*,*) 'nrerror: an assert_eq failed with this tag:', &
                        string
                STOP 'program terminated by assert_eq'
        end if
     END FUNCTION assert_eq

     FUNCTION outerprod(a,b)
        REAL(rkind), DIMENSION(:), INTENT(IN) :: a,b
        REAL(rkind), DIMENSION(size(a),size(b)) :: outerprod
        outerprod = spread(a,dim=2,ncopies=size(b)) * &
                spread(b,dim=1,ncopies=size(a))
     END FUNCTION outerprod

     FUNCTION imaxloc(arr)
        REAL(rkind), DIMENSION(:), INTENT(IN) :: arr
        INTEGER(ikind) :: imaxloc
        INTEGER(ikind), DIMENSION(1) :: imax
        imax=maxloc(arr(:))
        imaxloc=imax(1)
     END FUNCTION imaxloc

     FUNCTION iminloc(arr)
        REAL(rkind), DIMENSION(:), INTENT(IN) :: arr
        INTEGER(ikind) :: iminloc
        INTEGER(ikind), DIMENSION(1) :: imin
        imin=minloc(arr(:))
        iminloc=imin(1)
     END FUNCTION iminloc 

     FUNCTION arth_r(first,increment,n)
        INTEGER(ikind), PARAMETER :: NPAR_ARTH=16,NPAR2_ARTH=8
        REAL(rkind), INTENT(IN) :: first,increment
        INTEGER(ikind), INTENT(IN) :: n
        REAL(rkind), DIMENSION(n) :: arth_r
        INTEGER(ikind) :: k,k2
        REAL(rkind) :: temp
        if (n > 0) arth_r(1)=first
        if (n <= NPAR_ARTH) then
                do k=2,n
                        arth_r(k)=arth_r(k-1)+increment
                end do
        else
                do k=2,NPAR2_ARTH
                        arth_r(k)=arth_r(k-1)+increment
                end do
                temp=increment*NPAR2_ARTH
                k=NPAR2_ARTH
                do
                        if (k >= n) exit
                        k2=k+k
                        arth_r(k+1:min(k2,n))=temp+arth_r(1:min(k,n-k))
                        temp=temp+temp
                        k=k2
                end do
        end if
     END FUNCTION arth_r

     RECURSIVE SUBROUTINE trapzd(func,a,b,s,n,choice)
        IMPLICIT NONE
        REAL(rkind), INTENT(IN) :: a,b
        REAL(rkind), INTENT(INOUT) :: s
        INTEGER(ikind), INTENT(IN) :: n
        INTEGER(ikind), INTENT(IN) :: choice
        INTERFACE
                FUNCTION func(x,choice)
                USE f90_kind
                REAL(rkind), DIMENSION(:), INTENT(IN) :: x
                INTEGER(ikind), INTENT(IN) :: choice
                REAL(rkind), DIMENSION(size(x)) :: func
                END FUNCTION func
        END INTERFACE
        REAL(rkind) :: del,fsum
        INTEGER(ikind) :: it
        if (n == 1) then
                s=0.5_rkind*(b-a)*sum(func( (/ a,b /),choice ))
        else
                it=2**(n-2)
                del=(b-a)/it
                fsum=sum(func(arth_r(a+0.5_rkind*del,del,it),choice))
                s=0.5_rkind*(s+del*fsum)
        end if
     END SUBROUTINE trapzd

     SUBROUTINE trapzd2(func,a,b,s,n)
        IMPLICIT NONE
        REAL(rkind), INTENT(IN) :: a,b
        REAL(rkind), INTENT(INOUT) :: s
        INTEGER(ikind), INTENT(IN) :: n
        INTERFACE
                FUNCTION func(x)
                USE f90_kind
                REAL(rkind), DIMENSION(:), INTENT(IN) :: x
                REAL(rkind), DIMENSION(size(x)) :: func
                END FUNCTION func
        END INTERFACE
        REAL(rkind) :: del,fsum
        INTEGER(ikind) :: it
        if (n == 1) then
                s=0.5_rkind*(b-a)*sum(func( (/ a,b /) ))
        else
                it=2**(n-2)
                del=(b-a)/it
                fsum=sum(func(arth_r(a+0.5_rkind*del,del,it)))
                s=0.5_rkind*(s+del*fsum)
        end if
     END SUBROUTINE trapzd2

     RECURSIVE FUNCTION qsimp(func,a,b,choice)
        IMPLICIT NONE
        REAL(rkind),PARAMETER :: eps_tiny=1.d-15
        REAL(rkind), INTENT(IN) :: a,b
        INTEGER(ikind), INTENT(IN) :: choice
        REAL(rkind) :: qsimp
        INTERFACE
                FUNCTION func(x,choice)
                USE f90_kind 
                REAL(rkind), DIMENSION(:), INTENT(IN) :: x
                INTEGER(ikind), INTENT(IN) :: choice
                REAL(rkind), DIMENSION(size(x)) :: func
                END FUNCTION func
        END INTERFACE
        INTEGER(ikind), PARAMETER :: JMAX=27
        REAL(rkind), PARAMETER :: EPS=1.0e-4_rkind
        INTEGER(ikind) :: j
        REAL(rkind) :: os,ost,st
        ost=0.0
        os= 0.0
        do j=1,JMAX
                call trapzd(func,a,b,st,j,choice)
                qsimp=(4.0_rkind*st-ost)/3.0_rkind
                if (j > 5) then
                        !write(*,*)'qsimp:',j,qsimp,os,abs(qsimp-os),'<?<',EPS*abs(os)
                        if (abs(qsimp-os) < EPS*abs(os) .or. &
                                (abs(qsimp)<eps_tiny  .and. abs(os) < eps_tiny)) RETURN
                                !(qsimp == 0.0 .and. os == 0.0)) RETURN
                end if
                os=qsimp
                ost=st
        end do
        write(*,*)'ERROR: qsimp: too many steps'
        stop
        END FUNCTION qsimp

        FUNCTION qsimp2(func,a,b)
        IMPLICIT NONE
        REAL(rkind),PARAMETER :: eps_tiny=1.d-99
        REAL(rkind), INTENT(IN) :: a,b
        REAL(rkind) :: qsimp2
        INTERFACE
                FUNCTION func(x)
                USE f90_kind
                REAL(rkind), DIMENSION(:), INTENT(IN) :: x
                REAL(rkind), DIMENSION(size(x)) :: func
                END FUNCTION func
        END INTERFACE
        INTEGER(ikind), PARAMETER :: JMAX=25
        REAL(rkind), PARAMETER :: EPS=1.0e-3_rkind
        INTEGER(ikind) :: j
        REAL(rkind) :: os,ost,st
        ost=0.0
        os= 0.0
        do j=1,JMAX
                call trapzd2(func,a,b,st,j)
                qsimp2=(4.0_rkind*st-ost)/3.0_rkind
                if (j > 5) then
                        !write(*,*)'qsimp2:',j,qsimp2,os,abs(qsimp2-os),'<?<',EPS*abs(os)
                        if (abs(qsimp2-os) < EPS*abs(os) .or. &
                                (abs(qsimp2)<eps_tiny  .and. abs(os) < eps_tiny)) RETURN
                end if
                os=qsimp2
                ost=st
        end do
        write(*,*)'ERROR: qsimp2: too many steps'
        stop
        END FUNCTION qsimp2

END MODULE math_routines

!********************************************************************
!**************************             *****************************
!**************************     MAIN    *****************************
!**************************             *****************************
!********************************************************************

program divergences_calculation
! computes various divergences (Hellinger, Wasserstein, Jensen-Shannon, Kullback-Leibler
USE f90_kind
USE math_routines,        ONLY: qsimp,xtest,divergence_func,mean1,mean2,sigma_a_1,sigma_a_2,sigma_b_1,sigma_b_2,&
                                density1,density2,xtest_new,x_integration_lb,x_integration_ub
implicit none
REAL(rkind),PARAMETER :: x_min =0.0, x_max=100.0, dx= 0.2, alpha=0.1
INTEGER(ikind) :: divergence_choice
REAL(rkind)    :: divergence_value,x,divergence_value_q
REAL(rkind)    :: mean_difference, mean_1, mean_2, moment2_1, moment2_2 !second moment
REAL(rkind)    :: variance1,variance2, term_a,term_d
REAL(rkind)    :: skewness1, moment3_1, skewness2, moment3_2 !third moment
  write(*,*)'Please enter desired divergence: (1) Hellinger  (2) Jensen-Shannon (3) Wasserstein-1 (4) Hell-approx (5) KL-approx'
  write(*,*)'Your choice :'
  read(*,*)divergence_choice
  xtest=x_min

  DO  !loop over positions x_test in [x_min, x_max], pdfs are from -inf..+inf at each position x_test
      xtest_new=.true. !cumulative distributions need to be recalculated upstream for Wasserstein-metric 
      write(21,*)xtest,mean1(xtest),mean1(xtest)-sigma_b_1(xtest),mean1(xtest)+sigma_a_1(xtest),&
                 mean2(xtest),mean2(xtest)-sigma_b_2(xtest),mean2(xtest)+sigma_a_2(xtest)
      !divergence_value=qsimp(divergence_func,x_integration_lb,x_integration_ub)
      SELECT CASE (divergence_choice)
      CASE (1)
         divergence_value=qsimp(divergence_func,x_integration_lb,x_integration_ub,divergence_choice) 
         write(*,*)xtest,sqrt(1-divergence_value) !by definition of Hellinger distance: sqrt(1- (int_dx sqrt(p1*p2)))
         write(40+divergence_choice,*)xtest,sqrt(1.0-divergence_value)
      CASE (2)
         divergence_value=qsimp(divergence_func,x_integration_lb,x_integration_ub,divergence_choice)
         write(*,*)xtest,divergence_value/log(2.d0)   
         write(40+divergence_choice,*)xtest,divergence_value/log(2.d0)  !division by ln(2) to map result into [0..1]-range
      CASE (3)
         divergence_value=qsimp(divergence_func,x_integration_lb,x_integration_ub,divergence_choice)
         write(*,*)xtest,divergence_value
         write(40+divergence_choice,*)xtest,divergence_value
      CASE (4)
         mean_1=qsimp(divergence_func,x_integration_lb,x_integration_ub,4)
         mean_2=qsimp(divergence_func,x_integration_lb,x_integration_ub,5)
         mean_difference=mean_2-mean_1
         moment2_1=qsimp(divergence_func,x_integration_lb,x_integration_ub,6) !second moment of first distribution
         moment2_2=qsimp(divergence_func,x_integration_lb,x_integration_ub,7)
         variance1=moment2_1-mean_1**2
         variance2=moment2_2-mean_2**2
         write(40+divergence_choice,*)xtest,mean_difference**2/(2*variance1+2*variance2+mean_difference**2),&
                                            mean_difference,variance1,variance2
      CASE (5) !KL-Approximation : arxiv 1907.00288 (mit Variance berücksichtigt)
         mean_1=qsimp(divergence_func,x_integration_lb,x_integration_ub,4)
         mean_2=qsimp(divergence_func,x_integration_lb,x_integration_ub,5)
         mean_difference=mean_2-mean_1
         moment2_1=qsimp(divergence_func,x_integration_lb,x_integration_ub,6) !second moment of first distribution
         moment2_2=qsimp(divergence_func,x_integration_lb,x_integration_ub,7)
         variance1=moment2_1-mean_1**2
         variance2=moment2_2-mean_2**2
         term_a=mean_difference**2+variance1+variance2
         term_d=sqrt(term_a**2-4*variance1*variance2)
         divergence_value=(term_a-2*variance2)/term_d*atanh(term_d/term_a)+0.5*log(variance2/variance1)
         divergence_value_q=(term_a-2*variance1)/term_d*atanh(term_d/term_a)+0.5*log(variance1/variance2)
         write(40+divergence_choice,*)xtest,divergence_value,divergence_value_q
      CASE (6)
         mean_1=qsimp(divergence_func,x_integration_lb,x_integration_ub,4)
         mean_2=qsimp(divergence_func,x_integration_lb,x_integration_ub,5)
         mean_difference=mean_2-mean_1
         moment2_1=qsimp(divergence_func,x_integration_lb,x_integration_ub,6) !second moment of first distribution
         moment2_2=qsimp(divergence_func,x_integration_lb,x_integration_ub,7)
         moment3_1=qsimp(divergence_func,x_integration_lb,x_integration_ub,10) !third moment of first distribution
         moment3_2=qsimp(divergence_func,x_integration_lb,x_integration_ub,11)
         variance1=moment2_1-mean_1**2
         variance2=moment2_2-mean_2**2
         skewness1=moment3_1-3*mean_1*variance1-mean_1**3/(sqrt(variance1)**3)
         skewness2=moment3_2-3*mean_2*variance2-mean_2**3/(sqrt(variance2)**3)
         term_a=mean_difference**2+variance1+variance2
         term_d=sqrt(term_a**2-4*variance1*variance2)
         !divergence_value=(mean_difference**2+(skewness1-skewness2)**2)/&
         !                 (2*variance1+2*variance2+mean_difference**2+(skewness1+skewness2)**2)
         divergence_value=(mean_difference**2)/(2*variance1+2*variance2+mean_difference**2) 
         divergence_value=(1-alpha)*divergence_value+alpha*(skewness1-skewness2)**2/&
                          (2*variance1+2*variance2+mean_difference**2+(abs(skewness1)+abs(skewness2))**2)
         write(40+divergence_choice,*)xtest,divergence_value,skewness1,skewness2
      CASE (8) !KL (q|p)
         divergence_value=qsimp(divergence_func,x_integration_lb,x_integration_ub,divergence_choice)
         write(*,*)xtest,divergence_value
         write(40+divergence_choice,*)xtest,divergence_value
      CASE (9) !KL (p|q)
         divergence_value=qsimp(divergence_func,x_integration_lb,x_integration_ub,divergence_choice)
         write(*,*)xtest,divergence_value   
         write(40+divergence_choice,*)xtest,divergence_value
      CASE DEFAULT
         write(*,*)'ERROR: this divergence measure is not (yet?) implemented...'
         stop
      END SELECT
      xtest=xtest+dx
      if (xtest>x_max) exit
  ENDDO    
  !output of density functions at a specific xtest-value 
  xtest=1.d0
  x=-2.0
  DO 
     write(19,*)x,density1(x),density2(x)
     x=x+0.01
     if (x>9.d0) exit 
  ENDDO
end program divergences_calculation
