!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine psi_fil( psicon,ncequi )

         include 'double.inc'
         include 'prm.inc'
         include 'param_.inc'
         include 'comblc.inc'
         include 'comevl.inc'

         common /comext/ zaindk(nip,njp,nkp)

	   real *4 zaindk

         real*8  psicon(*)

         !ncpfc=nloc(npfc)

         !NCEQUI = Nk - NCPFC + NEQUI !!!!

         do iq=1,ncequi

		  zpscon=0.d0	  

           do j=2,nj1
           do i=2,ni1

            !if(ipr(i,j).ne.0) then
             vrftfa=zaindk(i,j,iq)
             zpscon=zpscon+vrftfa*curf(i,j)*dri(i)*dzj(j)
             !zpscon=zpscon+vrftfa*curf(i,j)*dri(i)*dzj(j)*amu0
            !endif

           enddo
           enddo

           psicon(iq)=zpscon
         enddo

         return
         end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



         subroutine extfil(pcequi,ncequi)

         include 'double.inc'
         INCLUDE 'prm.inc'
         include 'param_.inc'
         include 'comblc.inc'
         INCLUDE 'comevl.inc'


         real*8 aindk(nip,njp)
         real*8 pcequi(*)
         integer ncequi
         logical*4 exi

         !!amu0=0.4d0*pi

         ncpfc=nloc(npfc)
         !NCEQUI = Nk - NCPFC + NEQUI !!!!

          do 500 j=1,nj
          do 500 i=1,ni

           ue(i,j)=0.d0

 500     continue

        write(fname,'(a,a)') path(1:kname),'exf.wr'
        open(1,file=fname,form='formatted')
         !open(1,file='exf.wr',form='formatted')

          read(1,*) nkread,nequi
          !if(nk.ne.nkread) then
          ! write(6,*) '***ERROR:'
          ! write(6,*)' you change total number of currents'
          ! write(6,*)' nk_read=',nkread,'nk from trecur=',nk
          ! write(6,*)' put icont=0 and start again'
          ! stop
          !endif


         do iq=1,ncequi

          read(1,*) ((aindk(i,j),j=1,nj),i=1,ni)

          do j=1,nj
          do i=1,ni

           ue(i,j)=ue(i,j)+aindk(i,j)*PCEQui(iq)*amu0

          enddo
          enddo

         enddo


           close(1)

         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine exfmat(rk,zk,tk,nk,
     *                     rk1,zk1,rk2,zk2,
     *                     rk3,zk3,rk4,zk4,ntipe,
     *                     NECON, WECON )

         include 'double.inc'
         INCLUDE 'prm.inc'
         include 'param_.inc'
         include 'comblc.inc'
         INCLUDE 'comevl.inc'


         real*8 aindk(nip,njp)
         real*8 rk(*),zk(*),tk(*), WECON(*)
         real*8 rk1(*),zk1(*),rk2(*),zk2(*)
         real*8 rk3(*),zk3(*),rk4(*),zk4(*)
         integer nk
         integer ntipe(*), NECON(*)
         logical*4 exi

         !!amu0=0.4d0*pi

         ncpfc=nloc(npfc)
         nves   = nk - ncpfc
         ncequi = nequi + nves

        write(fname,'(a,a)') path(1:kname),'exf.wr'
        open(1,file=fname,form='formatted')
         !open(1,file='exf.wr',form='formatted')

         write(1,*) ncequi, nequi

         do 1010 iq=1,nequi

            do i=1,ni
            do j=1,nj
               aindk(i,j)=0.d0

            enddo
            enddo

            do i=1,ni
            do j=1,nj

               do ik=1,ncpfc
                 if( necon(ik) .eq. iq )  then
                  ddx=dsqrt( (r(i)-rk(ik))**2 + (z(j)-zk(ik))**2 )
                  zaindk=greeni(r(i),z(j),rk(ik),zk(ik))/pi
                  aindk(i,j)=aindk(i,j)+zaindk*wecon(ik)
                 end if
               enddo

            enddo
            enddo

            write(1,*) ((aindk(i,j),j=1,nj),i=1,ni)

 1010    continue

         ibeg=ncpfc + 1

         if(ibeg.gt.nk) go to 11


c	print *,' ibeg nk ncam==',ibeg,nk,nk-ibeg

c	read (*,*)

	ncam=0.

         do 10 ik=ibeg,nk

          ntip=ntipe(ik)

           if(ntip.eq.1) then
             r1=rk1(ik)
             z1=zk1(ik)
             r2=rk2(ik)
             z2=zk2(ik)
             dlong=dsqrt( (r2-r1)**2 + (z2-z1)**2 )
           endif

          do 100 i=1,ni
          do 100 j=1,nj

           ddx=dsqrt( (r(i)-rk(ik))**2 + (z(j)-zk(ik))**2 )

           if(ntip.eq.1) then
            call bint(r(i),z(j),r1,z1,r2,z2,fint,1)
            aindk(i,j)=fint/dlong
           else
            aindk(i,j)=greeni(r(i),z(j),rk(ik),zk(ik))/pi
           endif
 100     continue

          write(1,*) ((aindk(i,j),j=1,nj),i=1,ni)

	ncam=ncam+1

          !write(6,*) 'wrd ik=',ik
 10     continue
 11     continue

          write(1,*) ncam



           close(1)
          !write(6,*) 'wrd nk',nk



 1000   continue

         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine cfr_mat(rk,zk,tk,nk,
     *                      NECON, WECON )

         include 'double.inc'
         INCLUDE 'prm.inc'
         include 'param_.inc'
         include 'comblc.inc'
         INCLUDE 'comevl.inc'


         real*8 fr_indk(npfc0,npfc0),fz_indk(npfc0,npfc0)
         real*8 fpr_indk(nip,njp),fpz_indk(nip,njp)
         real*8 rk(*),zk(*),tk(*), WECON(*)
         integer nk
         integer  NECON(*)
         logical*4 exi



!!!!!!!!!coil coil interaction

        do i=1,npfc  !2
        do j=1,npfc  !2
!        coils number i j

          SumFRij=0.d0
          SumFZij=0.d0

        if(i.ne.j) then

         ib=Nloc(i-1)+1        
         ie=Nloc(i)
                 
         jb=Nloc(j-1)+1        
         je=Nloc(j)
                 
          do k=ib,ie
           do m=jb,je
               rkk=rk(k) !1.d0
               zkk=zk(k) !1.d-2
               rkm=rk(m) !1.d0
               zkm=zk(m) !-1.d-2
               !weconk=wecon(k)
               !weconm=wecon(m)
               !val_pi=pi
               !call GRENg(rk(k),zk(k), rk(m),zk(m), fgreen,dGdr,dGdz)
               !call grGREN(rk(k),zk(k), rk(m),zk(m), dGdr,dGdz)
               call grGREN(rkk,zkk, rkm,zkm, dGdr,dGdz)
               SumFRij=SumFRij+dGdr*wecon(k)*wecon(m)/rk(m)/pi
               SumFZij=SumFZij-dGdz*wecon(k)*wecon(m)*2.d0
           enddo
          enddo
          
            fr_indk(i,j)=SumFRij          
            fz_indk(i,j)=SumFZij
                      
        endif

!           Fr(i,j)= -SumFRij*Ii*Ij  !force on coil j         
!           Fz(i,j)= -SumFZij*Ii*Ij  !force on coil j        

        enddo
        enddo


        write(fname,'(a,a)') path(1:kname),'forcmat.wr'
        open(1,file=fname,form='formatted')

         write(1,*) npfc
         write(1,*) ((fr_indk(i,j),j=1,npfc),i=1,npfc)
         write(1,*) ((fz_indk(i,j),j=1,npfc),i=1,npfc)

!!!!!!!!!plasma coil interaction

          do k=1,npfc

            kb=Nloc(k-1)+1        
            ke=Nloc(k)
            
            do i=1,ni
            do j=1,nj
             SumFRPijk=0.d0
             SumFZPijk=0.d0
             
             do ik=kb,ke
              call grGREN(r(i),z(j), rk(ik),zk(ik), dGdr,dGdz)
              SumFRPijk=SumFRPijk+dGdr*wecon(ik)/rk(ik)/pi
              SumFZPijk=SumFZPijk-dGdz*wecon(ik)*2.d0
             enddo
              fpr_indk(i,j)= SumFRPijk            
              fpz_indk(i,j)= SumFZPijk            

            enddo
            enddo
            
         write(1,*) ((fpr_indk(i,j),j=1,nj),i=1,ni)
         write(1,*) ((fpz_indk(i,j),j=1,nj),i=1,ni)

          enddo

           close(1)

         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine coil_force

         include 'double.inc'
         INCLUDE 'prm.inc'
         include 'param_.inc'
         include 'comblc.inc'
         INCLUDE 'comevl.inc'

         real*8 frcr(npfc0),frcz(npfc0)

         real*8 fr_indk(npfc0,npfc0),fz_indk(npfc0,npfc0)
         real*8 fpr_indk(nip,njp),fpz_indk(nip,njp)
         logical*4 exi

! coil forces calculation
! forces frcz are full Fz forces on coil[MN]
! forces frcr are  Fr forces on coil lenth unit[MN/m]

        write(fname,'(a,a)') path(1:kname),'forcmat.wr'
        open(1,file=fname,form='formatted')

         read(1,*) npfc
         read(1,*) ((fr_indk(i,j),j=1,npfc),i=1,npfc)
         read(1,*) ((fz_indk(i,j),j=1,npfc),i=1,npfc)



        do k=1,npfc
         do iq=1,nequi
          if(nepfc(k) .eq. iq) then
           pfcur1(k)=pfceqw(iq)
           exit           
          endif
         enddo
        enddo


!!!!!!!!!coil coil interaction

        do m=1,npfc
          Frsum=0.d0        
          Fzsum=0.d0        
         do k=1,npfc
          if(k.ne.m)then
           Frsum=Frsum +fr_indk(k,m)*pfcur1(k)*pfcur1(m)       
           Fzsum=Fzsum -fz_indk(k,m)*pfcur1(k)*pfcur1(m)       
          endif         
         enddo
          frcr(m)=Frsum*amu0        
          frcz(m)=Fzsum*amu0        
        enddo

!!!!!!!!!plasma coil interaction

           Fzpl=0.d0

          do k=1,npfc
          
           read(1,*) ((fpr_indk(i,j),j=1,nj),i=1,ni)
           read(1,*) ((fpz_indk(i,j),j=1,nj),i=1,ni)

           do i=1,ni
           do j=1,nj
             iprij=ipr(i,j)
             if(iprij.eq.1)then            
              dcurij=curf(i,j)*(dri(i)*dzj(j))
              frcr(k)=frcr(k)+fpr_indk(i,j)*dcurij*pfcur1(k)   
              frcz(k)=frcz(k)-fpz_indk(i,j)*dcurij*pfcur1(k)
              Fzpl=Fzpl-fpr_indk(i,j)*dcurij*pfcur1(k)                   
             endif
           enddo
           enddo

          enddo

           close(1)

        write(fname,'(a,a)') path(1:kname),'coil_forces.wr'
        open(1,file=fname,form='formatted')
         write(1,*) npfc
         write(1,*) (frcr(k),k=1,npfc)
         write(1,*) (frcz(k),k=1,npfc)
        close(1)

!force limits for ITER

         !call iter_force_limits(frcz,frcr)

!force limits for ITER

        open(1,file='coil_forces.pr',form='formatted')
           write(1,'(a6,e13.5)') 'PF1',frcz(1) 
           write(1,'(a6,e13.5)') 'PF2',frcz(7) 
           write(1,'(a6,e13.5)') 'PF3',frcz(3) 
           write(1,'(a6,e13.5)') 'PF5',frcz(5) 
           write(1,'(a6,e13.5)') 'CS ',frcz(9) 
        close(1)



         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine iter_force_limits(frcz,frcr)

         include 'double.inc'
         dimension frcz(*),frcr(*)
         
        FzCSup1= frcz(5)      
        FzCSup2= FzCSup1+frcz(3)      
        FzCSup3= FzCSup2+frcz(1)      
        FzCSup4= FzCSup3+frcz(2)      
        FzCSup5= FzCSup4+frcz(4)      
        FzCSup6= FzCSup5+frcz(6)
        FzMXup= dmax1(FzCSup1,FzCSup2,FzCSup3,FzCSup4,FzCSup5,FzCSup6)  
                       
        
        FzCSdw1= frcz(6)      
        FzCSdw2= FzCSdw1+frcz(4)      
        FzCSdw3= FzCSdw2+frcz(2)      
        FzCSdw4= FzCSdw3+frcz(1)      
        FzCSdw5= FzCSdw4+frcz(3)      
        FzCSdw6= FzCSdw5+frcz(5)      
        FzMXdw= dmin1(FzCSdw1,FzCSdw2,FzCSdw3,FzCSdw4,FzCSdw5,FzCSdw6)  
       
        FzPF1= frcz(7)      
        FzPF2= frcz(8)      
        FzPF3= frcz(9)      
        FzPF4= frcz(10)      
        FzPF5= frcz(11)      
        FzPF6= frcz(12)      

         return
         end


         subroutine ext_fil(pcequi,ncequi)

         include 'double.inc'
         INCLUDE 'prm.inc'
         include 'param_.inc'
         include 'comblc.inc'
         INCLUDE 'comevl.inc'

         common /comext/ zaindk(nip,njp,nkp)
	 real *4 zaindk

         real*8 pcequi(*)
         integer ncequi

         !!amu0=0.4d0*pi
         !ncpfc=nloc(npfc)

 1000    continue

         do j=1,nj
         do i=1,ni
            ue(i,j)=0.d0
         enddo
         enddo

         do iq=1,ncequi
            egcurr=PCEQui(iq)	  
           do i=1,ni
           do j=1,nj
             vrftfa=zaindk(i,j,iq)
             ue(i,j)=ue(i,j)+vrftfa*egcurr*amu0
           enddo
           enddo
         enddo

         return
         end
C
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
      subroutine rdexf(ncequi)

      include 'double.inc'
      INCLUDE 'prm.inc'
      include 'param_.inc'
      include 'comblc.inc'
      INCLUDE 'comevl.inc'

      common /comext/ zaindk(nip,njp,nkp)

      real*4 zaindk

      real*8 aindk(nip,njp)

        write(fname,'(a,a)') path(1:kname),'exf.wr'
        open(1,file=fname,form='formatted')
         !open(1,file='exf.wr',form='formatted')

      read(1,*) nk,nequi

           !if(nk.ne.nkread) then
           ! write(6,*) '***ERROR:'
            !write(6,*)' you change total number of currents'
            !write(6,*)' nk_read=',nkread,'nk from trecur=',nk
            !write(6,*)' put icont=0 and start again'
            !stop
           !endif

         ncpfc=nloc(npfc)

         do iq=1,ncequi
            read(1,*) ((aindk(i,j),j=1,nj),i=1,ni)
           do j=1,nj
           do i=1,ni
             zaindk(i,j,iq)=aindk(i,j)
           enddo
           enddo
         enddo

!         ibeg=ncpfc + 1
!         if(ibeg.gt.nk) go to 22
!            iq=nequi
!         do ik=ibeg,nk
!            iq=iq+1
!            read(1) ((aindk(i,j),i=1,ni),j=1,nj)
!           do i=1,ni
!           do j=1,nj
!             zaindk(i,j,iq)=aindk(i,j)
!           enddo
!           enddo
!         enddo
!
! 22      continue

         close(1)

         return
         end
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine bndmat

         include 'double.inc'
         include 'param.inc'
         include 'comblc.inc'

        real*8 xk(nbndp),yk(nbndp)

        ib=1

        xk(ib)=r(1)
        yk(ib)=z(1)

        do 10 i=2,ni1

        ib=ib+1
        xk(ib)=r(i)
        yk(ib)=z(1)

 10     continue

        ib=ib+1
        xk(ib)=r(ni)
        yk(ib)=z(1)

        do 20 j=2,nj1

        ib=ib+1
        xk(ib)=r(ni)
        yk(ib)=z(j)

 20     continue

        ib=ib+1
        xk(ib)=r(ni)
        yk(ib)=z(nj)

        do 30 i=ni1,2,-1

        ib=ib+1
        xk(ib)=r(i)
        yk(ib)=z(nj)

 30     continue

        ib=ib+1
        xk(ib)=r(1)
        yk(ib)=z(nj)

        do 40 j=nj1,2,-1

        ib=ib+1
        xk(ib)=r(1)
        yk(ib)=z(j)

 40     continue

        ib=ib+1
        xk(ib)=r(1)
        yk(ib)=z(1)


        do ib=1,nbnd
        rr=xk(ib)
        zz=yk(ib)
        do ibc=1,nbnd

        r0=xk(ibc)
        z0=yk(ibc)

        r1=xk(ibc+1)
        z1=yk(ibc+1)

        call bint(rr,zz,R0,Z0,r1,z1,Fint,1)

        binadg(ibc,ib)=fint

        enddo
        enddo



        return
        end

c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine bndint(rk,zk,nk)

         include 'double.inc'
         include 'param.inc'
         include 'comblc.inc'

        real*8 xk(nbndp),yk(nbndp)
        real*8 rk(*),zk(*)
        integer nk

        ib=1

        xk(ib)=r(1)
        yk(ib)=z(1)

        do 10 i=2,ni1

        ib=ib+1
        xk(ib)=r(i)
        yk(ib)=z(1)

 10     continue

        ib=ib+1
        xk(ib)=r(ni)
        yk(ib)=z(1)

        do 20 j=2,nj1

        ib=ib+1
        xk(ib)=r(ni)
        yk(ib)=z(j)

 20     continue

        ib=ib+1
        xk(ib)=r(ni)
        yk(ib)=z(nj)

        do 30 i=ni1,2,-1

        ib=ib+1
        xk(ib)=r(i)
        yk(ib)=z(nj)

 30     continue

        ib=ib+1
        xk(ib)=r(1)
        yk(ib)=z(nj)

        do 40 j=nj1,2,-1

        ib=ib+1
        xk(ib)=r(1)
        yk(ib)=z(j)

 40     continue

        ib=ib+1
        xk(ib)=r(1)
        yk(ib)=z(1)


         nkin=0
         nkout=0

        do 500 ik=1,nk
        rr=rk(ik)
        zz=zk(ik)

         if(rr.gt.rmax .OR. rr.lt.rmin .OR.
     *      zz.gt.zmax .OR. zz.lt.zmin ) then

           nkout=nkout+1

           do 110 ib=1,nbnd

           r0=xk(ib)
           z0=yk(ib)

           r1=xk(ib+1)
           z1=yk(ib+1)

           call bint(rr,zz,R0,Z0,r1,z1,Fint,1)

           pinadg(nkout,ib)=fint

 110       continue
              itok(ik)=0
              jtok(ik)=0

         else

           nkin=nkin+1

           do 200 i=1,ni1

            if(rr.ge.r(i) .AND. rr.le.r(i+1)) then

              itok(ik)=i

              go to 210

             endif

 200       continue
 210       continue

           do 300 j=1,nj1

            if(zz.ge.z(j) .AND. zz.le.z(j+1)) then

              jtok(ik)=j

              go to 310

             endif

 300       continue
 310       continue

         endif

 500       continue

        return
        end

c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine flux(psitok,rk,zk,nk)

         include 'double.inc'
         include 'param.inc'
         include 'comblc.inc'

         real*8 psitok(*),rk(*),zk(*)

           ikout=0
           ikin=0

        do 100 ik=1,nk

           psitok(ik)=0.d0

         if(itok(ik).eq.0) then

           ikout=ikout+1

           do 110 ib=1,nbnd

           psitok(ik)=psitok(ik)+pinadg(ikout,ib)*dgdn(ib)

 110       continue

         else

           ikin=ikin+1

           i=itok(ik)
           j=jtok(ik)

           psitok(ik)=blin(i,j,rk(ik),zk(ik))

         endif

 100       continue

          if(nkin.ne.ikin .OR. nkout.ne.ikout) then
           !write(6,*) '*** ERROR: nkin.ne.ikin .OR. nkout.ne.ikout'
           !write(6,*) ' nkin,  ikin  = ', nkin,  ikin
           !write(6,*) ' nkout, ikout = ', nkout, ikout
           stop
          endif

        return
        end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c
        real*8 function blin(i,j,r0,z0)
c
         include 'double.inc'
         include 'param.inc'
         include 'comblc.inc'
c
        z1=z(j)
        z2=z(j+1)

        r1=r(i)
        r2=r(i+1)

        u1=ui(i,j)
        u2=ui(i+1,j)
        u3=ui(i+1,j+1)
        u4=ui(i,j+1)

        s1=(r2-r0)*(z2-z0)
        s3=(r0-r1)*(z0-z1)
        s2=(r0-r1)*(z2-z0)
        s4=(r2-r0)*(z0-z1)

        u0=(s1*u1+s2*u2+s3*u3+s4*u4)/(s1+s2+s3+s4)

        blin=u0

        return
        end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c
        real*8 function bline(i,j,r0,z0)
c
         include 'double.inc'
         include 'param.inc'
         include 'comblc.inc'
c
        z1=z(j)
        z2=z(j+1)

        r1=r(i)
        r2=r(i+1)

        u1=ue(i,j)
        u2=ue(i+1,j)
        u3=ue(i+1,j+1)
        u4=ue(i,j+1)

        s1=(r2-r0)*(z2-z0)
        s3=(r0-r1)*(z0-z1)
        s2=(r0-r1)*(z2-z0)
        s4=(r2-r0)*(z0-z1)

        u0=(s1*u1+s2*u2+s3*u3+s4*u4)/(s1+s2+s3+s4)

        bline=u0

        return
        end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        real*8 function blint(i,j,r0,z0)

         include 'double.inc'
         include 'param.inc'
         include 'comblc.inc'

        z1=z(j)
        z2=z(j+1)

        r1=r(i)
        r2=r(i+1)

        u1=u(i,j)
        u2=u(i+1,j)
        u3=u(i+1,j+1)
        u4=u(i,j+1)

        s1=(r2-r0)*(z2-z0)
        s3=(r0-r1)*(z0-z1)
        s2=(r0-r1)*(z2-z0)
        s4=(r2-r0)*(z0-z1)

        u0=(s1*u1+s2*u2+s3*u3+s4*u4)/(s1+s2+s3+s4)

        blint=u0

        return
        end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine grid

         include 'double.inc'
         include 'param.inc'
         include 'comblc.inc'


         ddr=(rmax-rmin)/dfloat(ni1)
         ddz=(zmax-zmin)/dfloat(nj1)

         r(1)=rmin
         z(1)=zmin


         do 10 i=2,ni

 10      r(i)=r(i-1)+ddr

         do 20 j=2,nj

 20      z(j)=z(j-1)+ddz

         return
         end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine geom

         include 'double.inc'
         include 'param.inc'
         include 'comblc.inc'

         do 30 i=1,ni1

           dr(i)=r(i+1)-r(i)
           r12(i)=(r(i+1)+r(i))*0.5d0
          if(i.eq.1) go to 30
           dri(i)=(r(i+1)-r(i-1))*0.5d0

 30      continue

         do 40 j=1,nj1

           dz(j)=z(j+1)-z(j)
          if(j.eq.1) go to 40
           dzj(j)=(z(j+1)-z(j-1))*0.5d0

 40      continue

         do 2223 ilm=1,nblm

          do 400 i=1,ni1

           if(rblm(ilm).lt.r(i+1) .AND. rblm(ilm).ge.r(i) ) then
           ic  =i
           go to 401
           endif

 400      continue
 401      iblm(ilm)=ic

          do 500 j=1,nj1

           if(zblm(ilm).lt.z(j+1) .AND. zblm(ilm).ge.z(j) ) then
           jc  =j
           go to 501
           endif

 500      continue
 501      jblm(ilm)=jc

C...      write(6,*)' iblm,jblm,ilim',iblm(ilm),jblm(ilm),ilm

 2223    continue

         return
         end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine zero(isol)

         include 'double.inc'
         include 'param.inc'
         include 'comblc.inc'

         integer isol

         !!amu0=0.4d0*pi

          do 100 i=1,ni1

           if(rm0.lt.r(i+1) .AND. rm0.ge.r(i) ) then
           ic  =i
           go to 101
           endif

 100      continue
 101      imax=ic

          do 200 j=1,nj1

           if(zm0.lt.z(j+1) .AND. zm0.ge.z(j) ) then
           jc  =j
           go to 201
           endif

 200      continue
 201      jmax=jc

        z1=z(jc)
        z2=z(jc+1)

        r1=r(ic)
        r2=r(ic+1)

        s1=(r2-rm0)*(z2-zm0)
        s3=(rm0-r1)*(zm0-z1)
        s2=(rm0-r1)*(z2-zm0)
        s4=(r2-rm0)*(zm0-z1)

        ssum=s1+s2+s3+s4

        tok1=amu0*tok*s1/ssum
        tok2=amu0*tok*s2/ssum
        tok3=amu0*tok*s3/ssum
        tok4=amu0*tok*s4/ssum

           do il=1,neqp
            right(il)=0.d0
           enddo

           do i=1,ni
           do j=1,nj
            curf(i,j)=0.d0
            g(i,j)=0.d0
           enddo
           enddo

             il=nlin(ic  ,jc  )
             right(il)=-tok1
	       curf(ic  ,jc  )=tok1/(dri(ic)*dzj(jc))

             il=nlin(ic+1,jc  )
             right(il)=-tok2
	       curf(ic+1,jc  )=tok2/(dri(ic+1)*dzj(jc))

             il=nlin(ic+1,jc+1)
             right(il)=-tok3
	       curf(ic+1,jc+1)=tok3/(dri(ic+1)*dzj(jc+1))

             il=nlin(ic  ,jc+1)
             right(il)=-tok4
	       curf(ic,jc+1  )=tok4/(dri(ic)*dzj(jc+1))

                ux0=-10d9

             call solve(isol,g)

             call bound

             !!++ call right1  !!spar 

             call solve(isol,ui)

cccccc       call psiful

         return
         end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine zero_ax(isol)

         include 'double.inc'
         include 'param.inc'
         include 'comblc.inc'

         integer isol

          do 100 i=1,ni1

           if(rm0.lt.r(i+1) .AND. rm0.ge.r(i) ) then
           ic  =i
           go to 101
           endif

 100      continue
 101      imax=ic

          do 200 j=1,nj1

           if(zm0.lt.z(j+1) .AND. zm0.ge.z(j) ) then
           jc  =j
           go to 201
           endif

 200      continue
 201      jmax=jc


           do il=1,neqp
            right(il)=0.d0
           enddo

           do i=1,ni
           do j=1,nj
            curf(i,j)=0.d0
            g(i,j)=0.d0
           enddo
           enddo

           tokint=0.d0
          do i=2,ni1
            r0=r(i)
           do j=2,nj1
            z0=z(j)
         call cur_map(curden,r0,z0)
             il=nlin(i,j)
             right(il)=-curden*dri(i)*dzj(j)
	       curf(i,j)=curden
             tokint=tokint+curden*dri(i)*dzj(j)
           enddo
          enddo
             tokint=tokint/amu0

                ux0=-10d9

             call solve(isol,g)

             call bound

             !!++ call right1  !!spar 

             call solve(isol,ui)

         return
         end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine cur_map(curden,rk,zk)

         include 'double.inc'
         include 'dim.inc'
         include 'compol.inc'

          sqrt(arg)=dsqrt(arg)

         !!amu0=0.4d0*pi

          curden=0.d0
          dr0=rk-rm
          dz0=zk-zm

          ro0=sqrt(dr0**2+dz0**2)

           tetp=dacos(dr0/ro0)
          if(dz0.lt.0.d0) then
           tet0=2.d0*pi-tetp
          else
           tet0=tetp
          endif

	   if(tet0.lt.teta(1)) tet0=tet0+2.d0*pi
	   if(tet0.gt.teta(nt)) tet0=tet0-2.d0*pi

         do j=2,nt1

          if(tet0.ge.teta(j) .AND. tet0.lt.teta(j+1)) jc=j

         enddo

          rob1=ro(nr,jc)
          rob2=ro(nr,jc+1)

          tet1=teta(jc)
          tet2=teta(jc+1)

         rob12=(rob1*(tet2-tet0)+rob2*(tet0-tet1))/(tet2-tet1)

        if(ro0.lt.rob12) then !!!

         do i=1,iplas-1

          ro1=ro(i,jc)
          ro2=ro(i,jc+1)
          ro3=ro(i+1,jc)
          ro4=ro(i+1,jc+1)

          ro12=(ro1*(tet2-tet0)+ro2*(tet0-tet1))/(tet2-tet1)
          ro34=(ro3*(tet2-tet0)+ro4*(tet0-tet1))/(tet2-tet1)

          if(ro0.gt.ro12 .AND. ro0.le.ro34) then
           ic=i
           go to 10
          endif 

         enddo

 10       continue

          ro1=ro(ic,jc)
          ro2=ro(ic+1,jc)
          ro3=ro(ic+1,jc+1)
          ro4=ro(ic,jc+1)

          u1=cur(ic,jc)
          u2=cur(ic+1,jc)
          u3=cur(ic+1,jc+1)
          u4=cur(ic,jc+1)

          curden=blintr(tet0,ro0,
     *                      tet1,tet2,ro1,ro2,ro3,ro4,u1,u2,u3,u4)

          curden=curden  !/amu0
        endif !!!

        return
        end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       real*8 function blintr(tet0,ro0,
     *                        tet1,tet2,ro1,ro2,ro3,ro4,u1,u2,u3,u4)

         include 'double.inc'

         ro14=(ro1*(tet2-tet0)+ro4*(tet0-tet1))/(tet2-tet1)
         ro23=(ro2*(tet2-tet0)+ro3*(tet0-tet1))/(tet2-tet1)

         u14=(u1*(tet2-tet0)+u4*(tet0-tet1))/(tet2-tet1)
         u23=(u2*(tet2-tet0)+u3*(tet0-tet1))/(tet2-tet1)

         blintr=(u14*(ro23-ro0)+u23*(ro0-ro14))/(ro23-ro14)

       return
       end

