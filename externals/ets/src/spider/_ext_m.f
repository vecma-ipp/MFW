
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         subroutine f_ext_fil(pcequi,ncequi)

         include 'double.inc'
         INCLUDE 'prm.inc'
          include 'parrc1.inc'
          include 'parrc2.inc'
         parameter(nekp=npfc0+nplim)
         include 'comrec.inc'
         INCLUDE 'comevl.inc'

         common /comext/ zaindk(nip,njp,nekp)
	 real *4 zaindk

         real*8 pcequi(*)
         integer ncequi

         ncpfc=nloc(npfc)

         do j=1,nj
         do i=1,ni
            ue(i,j)=0.d0
         enddo
         enddo

         do iq=1,ncequi
            egcurr=PCEQui(iq)*amu0	  
           do i=1,ni
           do j=1,nj
             vrftfa=zaindk(i,j,iq)
             ue(i,j)=ue(i,j)+vrftfa*egcurr
           enddo
           enddo
         enddo

         !open(1,file='ue_test.pr',form='formatted')
         !                  write(1,*)'ue'
         !                  write(1,*)'ni nj',ni,nj
         !                  write(1,*)((ue(i,j),i=1,ni),j=1,nj)
         !close(1)




         return
         end
C
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
        subroutine f_rdexf(ncequi)

         include 'double.inc'
         INCLUDE 'prm.inc'
          include 'parrc1.inc'
          include 'parrc2.inc'
         parameter(nekp=npfc0+nplim)
         include 'comrec.inc'
         INCLUDE 'comevl.inc'

      common /comext/ zaindk(nip,njp,nekp)

      real*4 zaindk

      real*8 aindk(nip,njp)

        write(fname,'(a,a)') path(1:kname),'exf.wr'
        open(1,file=fname)
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

