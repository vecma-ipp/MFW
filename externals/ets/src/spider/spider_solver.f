         !subroutine solve_fft(isol,wdm)
         subroutine solve(isol,wdm)
         include 'double.inc'
         include 'param.inc'
         include 'comblc.inc'
          common /comffs/ axm(nip),ax0(nip),axp(nip)

         real time_beg,time_end
         real*8 wdm(nip,njp)
         real*8 wdmx(nip,njp)
       dimension fwrk(nip,njp),ffwrk(nip,njp)

         do i=1,ni
         do j=1,nj
          wdmx(i,j)=wdm(i,j)
         enddo
         enddo

         do i=1,ni
         do j=1,nj
          fwrk(i,j)=-curf(i,j)*r(i)
         enddo
         enddo

           call foursol(ni,nj,wdm,fwrk,ffwrk,r,z,axm,ax0,axp)

                            wght=0.5d0
	       if(itin.le.2 ) wght=1.0d0

          do i=2,ni1
          do j=2,nj1
           wdm(i,j)=wdm(i,j)*wght+(1.d0-wght)*wdmx(i,j)
         enddo
         enddo

         return
         end

!*******************************************************************

         !subroutine solve(isol,wdm)
         subroutine solve_spar(isol,wdm)

         include 'double.inc'
         include 'param.inc'
         include 'comblc.inc'

         common /comwrc/ rsp,p,ip

         real*8 zw(neqp),rsp(nspp),wdm(nip,njp)

         integer p(neqp),ip(neqp),isp(nspp),ipath,flag,esp

         equivalence (rsp(1),isp(1))

c        equivalence (right(1),zw(1))

           ipath=3

         if(isol.ne.0) go to 20

         call odrvd(neq,ia,ja,a,p,ip,nspp,isp,1,flag)

c          do 10 i=1,neqp
c            ip(i)=i
c             p(i)=i
c10        continue

           !write(6,*) 'odrv flag=',flag

           ipath=1
           isol=1

 20        continue


         call sdrvd(neq,p,ip,ia,ja,a,right,zw,nspp,
     *              isp,rsp,esp,ipath,flag)

c           do 860 i=1,neq
c          write(6,*) 'zw(i) i',i,zw(i)
c860       continue
c
           !write(6,*) 'sdrv: flag,esp',flag,esp
c
cccc     call nev(zw)

c raspakovka reshenia

                            wght=0.5d0
	       if(itin.le.2 ) wght=1.0d0

           do 500 i=2,ni1
           do 500 j=2,nj1

           ieq=nlin(i,j)

           wdm(i,j)=zw(ieq)*wght+(1.d0-wght)*wdm(i,j)

 500       continue

         return
         end

         subroutine nev(zw)

         include 'double.inc'
         include 'param.inc'
         include 'comblc.inc'

         real*8 zw(neqp)

         do 10 il=1,neq

         i1=ia(il)
         i2=ia(il+1)-1

         znev=0.d0
         znmx=0.d0

         do 100 im=i1,i2

         ic=ja(im)

         znev=znev+a(im)*zw(ic)

       !   if(il.eq.1) then
       !
       ! write(6,*) 'im a(im)',im,a(im)
       ! write(6,*) 'ic zw(ic)',ic,zw(ic)
       ! write(6,*) 'znev   '  ,znev
       !   endif

 100     continue

         znev=right(il)-znev
         znab=dabs(znev)
c        write(6,*) '***right znev ***',il,right(il),znev
         znmx=dmax1(znmx,znab)

 10      continue

C           write(6,*) 'nev:',znmx

         return
         end
C-----------------------------------------------------------------------
!***************************************************************

        subroutine foursol(ni,nj,u,f,ff,x,y,axm,ax0,axp)

        implicit real*8(a-h,o-z)
        include 'param.inc'

       dimension u(nip,njp),f(nip,njp),x(nip),y(njp)
       dimension ff(nip,njp)
       dimension ff1(njp),w(njp),slam(njp),a(njp)
       dimension ap(nip),bp(nip),cp(nip),fp(nip)
       dimension alf(nip),bet(nip),u1(nip)
       dimension axm(nip),axp(nip),ax0(nip)

c-------power nj=2*mm definition
          md=nj-1
         do m=1,100
          mm=m
         if(md/2*2.ne.md) then
          write(*,*) 'nj-1 must be power of 2.program is terminated '
          write(*,*) 'nj=', nj
	    stop
         endif							 
          md=md/2	 
         if(md.eq.1) go to 446
         enddo	   	 
 446     continue	  
         nj1=nj-1
         ni1=ni-1

         hy=y(2)-y(1)

         do i=2,ni1
         do j=2,nj1
          u(i,j)=f(i,j)
         enddo
         enddo

        do i=2,ni-1
         u(i,2)=u(i,2)-u(i,1)/hy**2
         u(i,nj-1)=u(i,nj-1)-u(i,nj)/hy**2
        enddo

        do j=2,nj-1
         u(2,j)=u(2,j)-u(1,j)*axm(2)
         u(ni-1,j)=u(ni-1,j)-u(ni,j)*axp(ni1)
        enddo
!*************************************************
        do i=2,ni1

         do j=2,nj1
          w(j)=u(i,j)
         enddo

        call rft1(mm,w,a,3)

         do k=2,nj1
          ff(i,k)=a(k)
         enddo      

        enddo

!*************************************************

        do k=2,nj-1

          slam(k)=4.d0*dsin(0.5d0*(k-1.d0)*pi/nj1)**2/hy**2

         do i=2,ni-1
          ap(i-1)=axp(i)
          bp(i-1)=axm(i)
          cp(i-1)=ax0(i)+slam(k)
          fp(i-1)=-ff(i,k)          
         enddo 

          ap(ni-2)=0.d0	   
          bp(1)=0.d0
		
          call pROG1D(ni-2,u1(2), Ap,Bp,Cp,Fp, ALF,BET)

         do i=2,ni-1
          ff(i,k)=u1(i)	 
         enddo 
			        
        enddo
!*************************************************
        do i=2,ni-1
         do k=2,nj-1
          a(k)=ff(i,k)	 
         enddo 

        call rft1(mm,a,w,6)

         do j=2,nj-1
           u(i,j)=w(j)
         enddo 
        enddo 

!            zerr=0.d0
!          do i=2,ni-1
!          do j=2,nj-1
!           znew=axm(i)*u(i-1,j)-ax0(i)*u(i,j)+axp(i)*u(i+1,j)
!     +         +(u(i,j-1)-2.d0*u(i,j)+u(i,j+1))/hy**2-f(i,j)
!           ernew=dabs(znew)
!           ernew=dmax1(zerr,ernew)
!          enddo
!          enddo
!
!          write(6,*) 'fursol:new',ernew

        return
        end
!**********************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      SUBROUTINE RFT1 (MM, X, Y, MODE)
C     FAST FOURIER TRANSFORM IN ONE DIMENSION.
C     ==================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(*), Y(*)
      COMMON /FDATA/F,RTTWO, N, N2, M, NC, NS, ND, NR
      COMMON /FWORK/ W(2048)
C     ==================================================================
C==== FIXED DATA PREPARATION.
      MD = MM
      IF(MODE .NE. 1 .AND. MODE .NE. 4) MD = MM + 1
      CALL SETFT (MD)
      N1 = 2 ** MM + 1
C==== COPY INPUT VECTOR INTO WORKING STORAGE.
      DO 10 I = 1, N1
   10 W(ND+I) = X(I) * F
      GO TO (20, 30, 40, 50, 30, 40), MODE
C==== GENERAL ANALYSIS.
   20 CALL RPA
      W(NR+N1) = 0.0d0
      GO TO 60
C==== DCODSINE ANALYSIS OR SYNTHESIS.
   30 CALL RCA
      GO TO 60
C==== DSINE ANALYSIS OR SYNTHESIS.
   40 CALL RSA
      W(NR+1) = X(1)
      W(NR+N1) = X(N1)
      GO TO 60
C==== GENERAL SYNTHESIS.
   50 CALL RPS
      W(NR+N1) = W(NR+1)
C==== COPY INTO OUTPUT VECTOR.
   60 DO 70 I = 1, N1
   70 Y(I) = W(NR+I)
      RETURN
C     ==================================================================
      END
      SUBROUTINE RPA
C     REAL ANALYSIS.
C     ==================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /FDATA/ F,RTTWO,N, N2, M, NC, NS, NDATA, NRES
      COMMON /FWORK/ W(2048)
C     ==================================================================
      NQ = N2
      NIR = NDATA
      NOR = NDATA + N
      DO 100 L = 1, M
      NII = NIR + N2
      NOI = NOR + N2
C==== TPRIME = 0.
      DO 10 IT = 1, NQ
      W(NOR+IT) = W(NIR+IT) + W(NIR+IT+NQ)
   10 W(NOI+IT) = W(NIR+IT) - W(NIR+IT+NQ)
C==== 0 .LT. TPRIME .LE. NP / 2
      NO1 = NQ
      NO2 = N2 - NQ
      IF(NO1 - NO2) 20, 40, 60
   20 NI1 = NO1 + NO1
      NI2 = NI1 + NQ
      CC = W(NC+NO1)
      SS = W(NS+NO1)
      DO 30 IT = 1, NQ
      RE = CC * W(NIR+NI2+IT) - SS * W(NII+NI2+IT)
      AI = SS * W(NIR+NI2+IT) + CC * W(NII+NI2+IT)
      W(NOR+NO1+IT) = W(NIR+NI1+IT) + RE
      W(NOR+NO2+IT) = W(NIR+NI1+IT) - RE
      W(NOI+NO1+IT) =   W(NII+NI1+IT) + AI
   30 W(NOI+NO2+IT) = - W(NII+NI1+IT) + AI
      NO1 = NO1 + NQ
      NO2 = NO2 - NQ
      IF(NO1 - NO2) 20, 40, 60
C==== TPRIME = NP/2.
   40 DO 50 IT = 1, NQ
      W(NOR+NO1+IT) = W(NII   +IT)
   50 W(NOI+NO1+IT) = W(NII+NQ+IT)
   60 NT = NIR
      NIR = NOR
      NOR = NT
      NQ = NQ / 2
  100 CONTINUE
      NRES = NIR
      RETURN
C     ==================================================================
      END
      SUBROUTINE RPS
C     REAL SYNTHESIS.
C     ==================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /FDATA/ F,RTTWO,N, N2, M, NC, NS, NDATA, NRES
      COMMON /FWORK/ W(2048)
C     ==================================================================
      NQ = 1
      NIR = NDATA
      NOR = NDATA + N
      L = M
   10 NII = NIR + N2
      NOI = NOR + N2
C==== TPRIME = 0.
      DO 20 IT = 1, NQ
      W(NOR+IT) = W(NIR+IT) + W(NII+IT)
   20 W(NOR+IT+NQ) = W(NIR+IT) - W(NII+IT)
C==== 0 .LT. TPRIME .LT. NP/2.
      NI1 = NQ
      NI2 = N2 - NQ
      IF(NI1 - NI2) 40, 80, 120
   40 NO1 = NI1 + NI1
      NO2 = NO1 + NQ
      CC = W(NC+NI1)
      SS = W(NS+NI1)
      DO 50 IT = 1, NQ
      W(NOR+NO1+IT) = W(NIR+NI1+IT) + W(NIR+NI2+IT)
      RE            = W(NIR+NI1+IT) - W(NIR+NI2+IT)
      W(NOI+NO1+IT) = W(NII+NI1+IT) - W(NII+NI2+IT)
      AI            = W(NII+NI1+IT) + W(NII+NI2+IT)
      W(NOR+NO2+IT) = CC * RE + SS * AI
   50 W(NOI+NO2+IT) = CC * AI - SS * RE
      NI1 = NI1 + NQ
      NI2 = NI2 - NQ
      IF(NI1 - NI2) 40, 80, 120
   80 DO 90 IT = 1, NQ
      W(NOI   +IT) = W(NIR+NI1+IT) * 2.0d0
   90 W(NOI+NQ+IT) = W(NII+NI1+IT) * 2.0d0
  120 NT = NIR
      NIR = NOR
      NOR = NT
      NQ = NQ + NQ
      L = L - 1
      IF(L .GT. 0) GO TO 10
      NRES = NIR
      RETURN
C     ==================================================================
      END
      SUBROUTINE RSA
C     REAL ODD ANALYSIS OR SYNTHESIS.
C     ==================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /FDATA/ F,RTTWO,N, N2, M, NC, NS, NDATA, NRES
      COMMON /FWORK/ W(2048)
C     ==================================================================
      NQ = N2
      NIB = NDATA + 1
      NOB = NDATA + N2 + 2
      DO 200 L = 1, M
      NIM = NIB + NQ
      NIE = NIM + NQ
      NQH = NQ / 2
      NQL = (NQ - 1) / 2
      NOM = NOB + NQL + 1
C==== TPRIME = 0.
      IF(NQL .EQ. 0) GO TO 20
      DO 10 IT = 1, NQL
      W(NOB+IT) = W(NIB+IT) - W(NIM-IT)
   10 W(NOM+IT) = W(NIB+IT) + W(NIM-IT)
   20 IF(NQH .NE. NQL) W(NOM) = 2.0d0 * W(NIB+NQH)
C==== 0 .LT. TPRIME .LT. NP/2.
      NO1 = NQ
      NO2 = N2 - NQ
      IF(NO1 - NO2) 40, 80, 120
   40 NI = NO1 + NO1
      W(NOB+NO1) =   W(NIB+NI) + W(NIM+NI)
      W(NOB+NO2) = - W(NIB+NI) + W(NIM+NI)
      IF(NQL .EQ. 0) GO TO 60
      CC = W(NC+NO1)
      SS = W(NS+NO1)
      DO 50 IT=1,NQL
      RE = CC * W(NIE+NI-IT) - SS * W(NIM+NI-IT)
      AI = SS * W(NIE+NI-IT) + CC * W(NIM+NI-IT)
      W(NOM+NO1+IT) = W(NIM+NI+IT) - RE
      W(NOM+NO2+IT) = W(NIM+NI+IT) + RE
      W(NOB+NO1+IT) =   W(NIB+NI+IT) + AI
   50 W(NOB+NO2+IT) = -  W(NIB+NI+IT) + AI
   60 IF(NQH .EQ. NQL) GO TO 70
      NR = NO1 / 2
      W(NOM+NO1)=2.d0*(W(NS+NR)*W(NIM+NI+NQH) + W(NC+NR)*W(NIB+NI+NQH))
      W(NOM+NO2)=2.d0*(W(NC+NR)*W(NIM+NI+NQH) - W(NS+NR)*W(NIB+NI+NQH))
   70 NO1 = NO1 + NQ
      NO2 = NO2 - NQ
      IF(NO1 - NO2) 40, 80, 120
   80 W(NOB+NO1) = W(NIM)
      IF(NQL .EQ. 0) GO TO 100
      DO 90 IT = 1, NQL
      W(NOM+NO1+IT) = W(NIM+IT)
   90 W(NOB+NO1+IT) = W(NIE-IT)
  100 IF(NQH .NE. NQL) W(NOM+NO1) = RTTWO * W(NIM+NQH)
  120 NT = NIB
      NIB = NOB
      NOB = NT
      NQ = NQH
  200 CONTINUE
      NRES = NIB - 1
      RETURN
C     ==================================================================
      END
      SUBROUTINE RCA
C     REAL EVEN ANALYSIS OR SYNTHESIS.
C     ==================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /FDATA/ F,RTTWO,N, N2, M, NC, NS, NDATA, NRES
      COMMON /FWORK/ W(2048)
C     ==================================================================
      NQ = N2
      NIB = NDATA + 1
      NOB = NDATA + N2 + 2
      DO 200 L = 1, M
      NIM = NIB + NQ
      NIE = NIM + NQ
      NQH = NQ / 2
      NQL = (NQ - 1) / 2
      NOM = NOB + NQL + 1
C==== TPRIME = 0.
      W(NOB   ) = W(NIB) + W(NIM)
      W(NOB+N2) = W(NIB) - W(NIM)
      IF(NQL .EQ. 0) GO TO 20
      DO 10 IT = 1, NQL
      W(NOB+IT) = W(NIB+IT) + W(NIM-IT)
   10 W(NOM+IT) = W(NIB+IT) - W(NIM-IT)
   20 IF(NQH .NE. NQL) W(NOM) = W(NIB+NQH) * 2.d0
C==== 0 .LT. TPRIME .LT. NP / 2
      NO1 = NQ
      NO2 = N2 - NQ
      IF(NO1 - NO2) 40, 80, 120
   40 NI = NO1 + NO1
      W(NOB+NO1) = W(NIB+NI) + W(NIM+NI)
      W(NOB+NO2) = W(NIB+NI) - W(NIM+NI)
      IF(NQL .EQ. 0) GO TO 60
      CC = W(NC+NO1)
      SS = W(NS+NO1)
      DO 50 IT = 1, NQL
      RE = CC * W(NIM+NI-IT) - SS * W(NIE+NI-IT)
      AI = SS * W(NIM+NI-IT) + CC * W(NIE+NI-IT)
      W(NOB+NO1+IT) = W(NIB+NI+IT) + RE
      W(NOB+NO2+IT) = W(NIB+NI+IT) - RE
      W(NOM+NO1+IT) =   W(NIM+NI+IT) - AI
   50 W(NOM+NO2+IT) = - W(NIM+NI+IT) - AI
   60 IF(NQH .EQ. NQL) GO TO 70
      NR = NO1 / 2
      W(NOM+NO1)=2.d0*(W(NC+NR)*W(NIB+NI+NQH) - W(NS+NR)*W(NIM+NI+NQH))
      W(NOM+NO2)=2.d0*(W(NS+NR)*W(NIB+NI+NQH) + W(NC+NR)*W(NIM+NI+NQH))
   70 NO1 = NO1 + NQ
      NO2 = NO2 - NQ
      IF(NO1 - NO2) 40, 80, 120
C==== TPRIME = NP/2
   80 W(NOB+NO1) = W(NIB+N2)
      IF(NQL .EQ. 0) GO TO 100
      DO 90 IT = 1, NQL
      W(NOB+NO1+IT) =   W(NIM+IT)
   90 W(NOM+NO1+IT) = - W(NIE-IT)
  100 IF(NQH .NE. NQL) W(NOM+NO1) = RTTWO * W(NIM+NQH)
  120 NT = NIB
      NIB = NOB
      NOB = NT
      NQ = NQH
  200 CONTINUE
      NRES = NIB - 1
      RETURN
C     ==================================================================
      END
      SUBROUTINE SETFT (MM)
C     PREPARE FIXED DATA FOR FAST FOURIER TRANSFORM.
C     ==================================================================
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON /FDATA/F,RTTWO, N, N2, M, NC, NS, NDATA, NRES
      COMMON /FWORK/ W(2048)
C     DATA (M = - 1)
            M = - 1
C     ==================================================================
      IF(MM .LT. 0) GO TO 90
      IF(MM .EQ. M) RETURN
      M = MM
      N = 2 ** M
      N2 = N / 2
      F = 1.d0 / dFLOAT (N)
      DA = 6.2831853071796d0 * F
      RTTWO = 2.0d0
      RTTWO = DSQRT (RTTWO)
      F = DSQRT (F)
      N1 = (N2 - 1) / 2
      NC = 0
      NS = N1
      NDATA = N1 + N1
      IF(N1 .LE. 0) RETURN
      DO 10 I = 1, N1
      A = dFLOAT(I) * DA
      W(I) = DCOS(A)
   10 W(N1+I) = DSIN(A)
      RETURN
   90 CONTINUE
C     ==================================================================
      END





