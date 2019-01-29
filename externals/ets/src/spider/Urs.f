!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         real*8 function funppp(psi)

          implicit real*8(a-h,o-z)
          include 'urs.inc'
          common/comppp/ ppp(nursp),fff(nursp),www(nursp)

          if(psi.lt.0.d0) then
             funppp =0.d0
             return
          endif
          if(psi.gt.1.d0) then
             funppp =ppp(nurs)
             return
          endif

         i=1+psi*(nurs-1)

         if(i.eq.nurs) i=nurs-1
         if(i.lt.1) i=1

         dpsip=psit(i+1)-psi
         dpsim=psi-psit(i)

        zppp=(ppp(i)*dpsip+ppp(i+1)*dpsim)/(dpsip+dpsim)

          funppp = zppp

         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         real*8 function funfff(psi)

          implicit real*8(a-h,o-z)
          include 'urs.inc'
          common/comppp/ ppp(nursp),fff(nursp),www(nursp)

          if(psi.lt.0.d0) then
             funfff =0.d0
             return
          endif
          if(psi.gt.1.d0) then
             funfff =fff(nurs)
             return
          endif

         i=1+psi*(nurs-1)

         if(i.eq.nurs) i=nurs-1
         if(i.lt.1) i=1

         dpsip=psit(i+1)-psi
         dpsim=psi-psit(i)

        zfff=(fff(i)*dpsip+fff(i+1)*dpsim)/(dpsip+dpsim)

          funfff = zfff

         return
         end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         real*8 function funcur(ri,psi)

          implicit real*8(a-h,o-z)
          include 'urs.inc'

          if(psi.lt.0.d0) then
             funcur =0.d0
             return
          endif

          if(psi.gt.1.d0) then
             funcur =purs(nurs)*ri+furs(nurs)/ri+wurs(nurs)*ri**3
             return
          endif

         i=1+psi*(nurs-1)

         if(i.ge.nurs) i=nurs-1
         if(i.lt.1) i=1

         dpsip=psit(i+1)-psi
         dpsim=psi-psit(i)

         pp=(purs(i)*dpsip+purs(i+1)*dpsim)/(dpsip+dpsim)
         fp=(furs(i)*dpsip+furs(i+1)*dpsim)/(dpsip+dpsim)
         wp=(wurs(i)*dpsip+wurs(i+1)*dpsim)/(dpsip+dpsim)

          funcur = ri*pp + fp/ri + wp*ri**3

         return
         end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         real*8 function funcur_p(ri,psi)

          implicit real*8(a-h,o-z)
          include 'urs.inc'

          if(psi.lt.0.d0) then
             funcur_p =0.d0
             return
          endif

          if(psi.gt.1.d0) then
             funcur_p =purs(nurs)*ri
             return
          endif

         i=1+psi*(nurs-1)

         if(i.ge.nurs) i=nurs-1
         if(i.lt.1) i=1

         dpsip=psit(i+1)-psi
         dpsim=psi-psit(i)

         pp=(purs(i)*dpsip+purs(i+1)*dpsim)/(dpsip+dpsim)
         !!!fp=(furs(i)*dpsip+furs(i+1)*dpsim)/(dpsip+dpsim)

          funcur_p = ri*pp 

         return
         end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         real*8 function funcur_f(ri,psi)

          implicit real*8(a-h,o-z)
          include 'urs.inc'

          if(psi.lt.0.) then
             funcur_f =0.d0
             return
          endif

          if(psi.gt.1.d0) then
             funcur_f =furs(nurs)/ri
             return
          endif

         i=1+psi*(nurs-1)

         if(i.ge.nurs) i=nurs-1
         if(i.lt.1) i=1

         dpsip=psit(i+1)-psi
         dpsim=psi-psit(i)

         !!!pp=(purs(i)*dpsip+purs(i+1)*dpsim)/(dpsip+dpsim)
         fp=(furs(i)*dpsip+furs(i+1)*dpsim)/(dpsip+dpsim)

          funcur_f =  fp/ri

         return
         end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       subroutine taburs(ien,coin,nursb)
       
         use ppf_modul       

         include 'double.inc'
          include 'urs.inc' 
          parameter(nursp4=nursp+4,nursp6=nursp4*6)
          common/comppp/ ppp(nursp),fff(nursp),www(nursp)
          common/comsav/ alf0n
       common/com_flag/kastr
          !dimension pstab(nursp),pptab(nursp),fptab(nursp),wptab(nursp)
          dimension wptab(nursp)
          real*8 RRK(nursp4),CCK(nursp4),WRK(nursp6)
           real*8 CWK(4)

!       if(.not.allocated(pstab))
!     %allocate( pstab(nutab), pptab(nutab), fptab(nutab) )

c!!!!!!table for fast calculation P' and FF' as funct. of PSI_nor
c!!!!!! P' - pptab
c!!!!!! FF'- fptab
c!!!!!! PSI_nor - pstab
            
           if(kastr.eq.1) then 
            alw0p=0.d0
            alw1p=0.d0
            alw2p=0.d0
           endif

           if(ien.eq.0) then !!!!!!!!!!!!!!!!!!!!!!

              nurs=3999

c if nursb<0 p'and ff'are assumed to be defined as tab.(file 'tabppf.dat'
  

c            write(6,*) nurs
c
c            write(6,*) alf0p
c            write(6,*) alf1p
c            write(6,*) alf2p
c
c            write(6,*) bet0f
c            write(6,*) bet1f
c            write(6,*) bet2f

         psit(1)=0.d0
         dpsi=1.d0/(nurs-1.d0)

           do i=2,nurs-1

          zpsi=psit(i-1)+dpsi
          psit(i)=zpsi
           enddo

         psit(nurs)=1.d0

            if(nursb.gt.0) then

	    !write(6,*) 'taburs:initial calculation P and FF'

          do 10 i=1,nurs

          zpsi=psit(i)
          purs(i)=funpp(zpsi)
          furs(i)=funfp(zpsi)
          wurs(i)=funwp(zpsi)

	    !write(6,*) 'f/p,ps:',purs(i)/(furs(i)+1.d-15),zpsi,i
	    !pause ' '

 10       continue

            else

!        write(fname,'(a,a)') path(1:kname),'tabppf.dat'
!        open(1,file=fname,form='formatted')
!          !open(1,file='tabppf.dat')

!              read(1,*) nutab

!           do i=1,nutab

!              read(1,*) pstab(i),pptab(i),fptab(i)

!           enddo

	    do i=1,nutab

              pstab(i)=pstab(i)/pstab(nutab)

          enddo
                                              
!          close(1)                                                 

                        key_int=0
                     if(key_int.eq.0) then

           do i=2,nurs-1
            zpsi=1.d0-psit(i)
	      do j=1,nutab-1
             if(zpsi.ge.pstab(j) .AnD. zpsi.lt.pstab(j+1)) then
              dpsip=pstab(j+1)-zpsi
              dpsim=zpsi-pstab(j)
              purs(i)=( pptab(j)*dpsip+pptab(j+1)*dpsim )/(dpsip+dpsim)
              furs(i)=( fptab(j)*dpsip+fptab(j+1)*dpsim )/(dpsip+dpsim)
             endif
            enddo
           enddo

                     else

        CALL E01BAF(Nutab,pstab,pptab,RRK,CCK,
     *                          nutab+4,WRK,6*nutab+16,IFAIL)

        if(ifail.ne.0) write(*,*) 'ifail=',ifail
           do i=2,nurs-1

          zpsi=1.d0-psit(i)

          CALL E02BCF(Nutab+4,RRK,CCK,zpsi,0,CWk,IFAIL)
        if(ifail.ne.0) write(*,*) 'ifail=',ifail

          purs(i)=cwk(1)

           enddo

        CALL E01BAF(Nutab,pstab,fptab,RRK,CCK,
     *                          nutab+4,WRK,6*nutab+16,IFAIL)
        if(ifail.ne.0) write(*,*) 'ifail=',ifail

           do i=2,nurs-1

          zpsi=1.d0-psit(i)

          CALL E02BCF(Nutab+4,RRK,CCK,zpsi,0,CWk,IFAIL)
        if(ifail.ne.0) write(*,*) 'ifail=',ifail

          furs(i)=cwk(1)

           enddo

                     endif

                 purs(1)=pptab(nutab)
                 purs(nurs)=pptab(1)

                 furs(1)=fptab(nutab)
                 furs(nurs)=fptab(1)

           !!>>>>>>>>>>>>>
             do i=1,nurs
             zpsi=psit(i)
             wurs(i)=funwp(zpsi)
               !wurs(i)=0.d0
             enddo
           !!>>>>>>>>>>>>>
           
            endif
              ! open(1,file='tabus.wr')
              ! write(1,*) nurs
              ! write(1,*)(psit(i),i=1,nurs)
              ! write(1,*)(purs(i),i=1,nurs)
              ! write(1,*)(furs(i),i=1,nurs)
              ! close(1)
                 
            elseif(ien.eq.1) then !!!!!!!!!!
                

			 do i=1,nurs
                purs(i)=purs(i)*coin
               enddo

	!write(6,*) 'taburs:purs recalculation'
	!write(6,*) 'coin=',coin


            endif !!!!!!!!!!!!!!!!!!!!!!!!!


         ppp(1)=0.d0
         fff(1)=0.d0
         www(1)=0.d0
         dpsi=1.d0/(nurs-1.d0)

          do 20 i=2,nurs

          ppp(i)=ppp(i-1) +(purs(i-1)+purs(i))*dpsi*0.5d0
          fff(i)=fff(i-1) +(furs(i-1)+furs(i))*dpsi*0.5d0
          www(i)=www(i-1) +(wurs(i-1)+wurs(i))*dpsi*0.5d0

ccc       write(6,*) 'i,ppp(i)',i,ppp(i)
 20       continue

      !deallocate( pstab, pptab, fptab )

          return
          end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       subroutine tabper
          include 'double.inc'
          include 'urs.inc'
          parameter(nursp4=nursp+4,nursp6=nursp4*6)

	      do i=1,nurs
	    purs(i)=purs(i)*1.d3
	    furs(i)=furs(i)
	      enddo

	    return
	    end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       subroutine tabnor(cnor)
          include 'double.inc'
          include 'urs.inc'
          parameter(nursp4=nursp+4,nursp6=nursp4*6)
          common/comppp/ ppp(nursp),fff(nursp),www(nursp)

	      do i=1,nurs
	    purs(i)=cnor*purs(i)
	    furs(i)=cnor*furs(i)
	    ppp(i)=cnor*ppp(i)
	    fff(i)=cnor*fff(i)
	      enddo

	    return
	    end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         real*8 function funpp(psi)
c
          implicit real*8(a-h,o-z)
          include 'urs.inc'
          epss=1.d-10

          if(psi.lt.0.d0) then
          funpp=0.d0
          return
          endif

          psm = 1.d0-psi + epss

          zpp = dabs( 1.d0 -(dabs(psm))**alf1p ) + epss
          pp  = alf0p*( zpp**alf2p )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    Profile for TCV, shot 10396, time = 0.24
!
!           c0 = 0.0d0
!           c1 = 0.730930d0
!           c2 = 0.d0
!           c3 = 0.d0
!              
!           pp = alf0p*( c0 + c1*psi + c2*psi*2 + c3*psi**3 )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c-------
c.         cbi=0.86d0
c.         cr0=8.025d0
c.         cnp=-1.4d0
c.         cnf=-1.4d0
c.         cal=2.d0
c
c.         pp=(cbi/cr0)*( dexp( cnp*(1-psm**cal) ) - 1. )/
c.    /                 ( dexp( cnp ) - 1. )
c-------
!------------------------------------------------
!    Profile for TCV, shot 20333, time = 0.500
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
           del=0.0d0
           x=(psi-del)/(1.d0-del)
!------------------------------------------------
!  TCV, shot 20333, time=0.500
            c0 = 0.0d0
            c1 = 1.7791d0
            c2 = 0.d0
            c3 = 0.d0
!---------------------------------------------------------------------
!
!          if(x.gt.0.d0) then
!           pp = alf0p*( c0 + c1*x + c2*x**2 + c3*x**3 )
!           !pp = alf0p*( c0 + c1*psi + c2*psi**2 + c3*psi**3 )
!          else
!           pp = 0.d0
!          endif
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          funpp=pp
c-------
         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         real*8 function funfp(psi)
c
          implicit real*8(a-h,o-z)
          include 'urs.inc'

          epss=1.d-10

          if(psi.lt.0.d0) then
          funfp=0.d0
          return
          endif

          psm = 1.d0- psi + epss

          zzfp = dabs( 1.d0 - psm**bet1f ) + epss
          fp   = bet0f*( zzfp**bet2f )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    Profile for TCV, shot 10396, time = 0.24
!
!           c0 = 0.0d0
!           c1 =-0.147740d0
!           c2 = 2.542300d0
!           c3 = 0.d0
!               
!           fp = bet0f*( c0 + c1*psi + c2*psi**2 + c3*psi**3 )
c----------------------------------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c-------
c.         fp= 1.d0
c-------
c.         cbi=0.86d0
c.         cr0=8.025d0
c.         cnp=-1.4d0
c.         cnf=-1.4d0
c.         cal=2.d0
c
c.         fp=(1.-cbi)*cr0*( dexp( cnf*(1-psm**cal) ) -1. )/
c.    /                    ( dexp( cnf ) - 1. )
c-------
!------------------------------------------------
           del=0.0d0
           x=(psi-del)/(1.d0-del)
!    Profile for TCV, shot 20333, time = 0.500
             c0 = 0.0d0
             c1 = 2.467100d0
             c2 =-1.473200d0
             c3 = 0.d0
!----------------------------------------------------------------------
!
!          if(x.gt.0.d0) then
!            !fp = bet0f*( c0 + c1*psi + c2*psi**2 + c3*psi**3 )
!            fp = bet0f*( c0 + c1*x + c2*x**2 + c3*x**3 )
!          else
!           fp = 0.d0
!          endif
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          funfp=fp
c-------
         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         real*8 function tabw(psi)

          implicit real*8(a-h,o-z)
          include 'urs.inc'

          if(psi.lt.0.d0) then
             wp =0.d0
             return
          endif

          if(psi.gt.1.d0) then
             wp =wurs(nurs)
             return
          endif

         i=1+psi*(nurs-1)

         if(i.eq.nurs) i=nurs-1

         if(i.lt.1) i=1

         dpsip=psit(i+1)-psi
         dpsim=psi-psit(i)

         wp=(wurs(i)*dpsip+wurs(i+1)*dpsim)/(dpsip+dpsim)

          tabw = wp

         return
         end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         real*8 function tabp(psi)

          implicit real*8(a-h,o-z)
          include 'urs.inc'

          if(psi.lt.0.d0) then
             pp =0.d0
             return
          endif

          if(psi.gt.1.d0) then
             pp =purs(nurs)
             return
          endif

         i=1+psi*(nurs-1)

         if(i.eq.nurs) i=nurs-1

         if(i.lt.1) i=1

         dpsip=psit(i+1)-psi
         dpsim=psi-psit(i)

         pp=(purs(i)*dpsip+purs(i+1)*dpsim)/(dpsip+dpsim)

          tabp = pp

         return
         end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         real*8 function tabf(psi)

          implicit real*8(a-h,o-z)
          include 'urs.inc'

          if(psi.lt.0.d0) then

             fp =0.d0

             return

          endif

          if(psi.gt.1.d0) then

             fp =furs(nurs)

             return

          endif

         i=1+psi*(nurs-1)

         if(i.eq.nurs) i=nurs-1

         if(i.lt.1) i=1

         dpsip=psit(i+1)-psi
         dpsim=psi-psit(i)

         fp=(furs(i)*dpsip+furs(i+1)*dpsim)/(dpsip+dpsim)

          tabf = fp

         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         real*8 function funwp(psi)

          implicit real*8(a-h,o-z)
          include 'urs.inc'
          
          epss=1.d-10
          pi=3.14159265359d0

          if(psi.lt.0.d0) then
             funwp=0.d0
             return
          endif

          psm = 1.d0 - psi + epss

          zwp = dabs( 1.d0 -(dabs(psm))**alw1p ) + epss
          wp  = alw0p*( zwp**alw2p )
          !wp  = -alw0p*dsin( 2.d0*pi*psm )

          funwp = wp
          !funwp = 0.d0

         return
         end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!