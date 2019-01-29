!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine eqa_in(alf0,alf1,alf2,bet0,bet1,bet2,nursb,
     *                  keyctr,igdf,nstep,platok, 
     *            pcequi,ncequi, b0cen,r0cen,
     *            rloop,zloop,nloop, rprob,zprob,nprob,
     *                     necon,wecon,ntipe )

         include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
         include 'dim.inc'
           !!include 'comlmtr.inc'
         include 'compol.inc'
         include 'compol_add.inc'
c   -----------------------------
         include 'urs.inc'
         parameter(nursp4=nursp+4,nursp6=nursp4*6)

          real*8   wecon(*)
          real*8   rloop(*),zloop(*),rprob(*),zprob(*)
          real*8   pcequi(*)

          integer  ntipe(*),necon(*)

          dimension pstab(nursp),qtab(nursp)

          real*8 RRK(nursp4),CCK(nursp4),WRK(nursp6)
          real*8 CWK(4)

          abs(xx)=dabs(xx)
          sqrt(xx)=dsqrt(xx)

            alf0p=alf0
            alf1p=alf1
            alf2p=alf2
            bet0f=bet0
            bet1f=bet1
            bet2f=bet2
            b0ax = b0cen
            r0ax = r0cen
            tok = platok

c...input initial data
        
        write(fname,'(a,a)') path(1:kname),'egg.dat'
        open(1,file=fname)

        !open(1,file='egg.dat')

         read(1,*) i_vac
         !read(1,*) nt
         !read(1,*) iplas
         read(1,*) alp
         read(1,*) k_dummy  !(is not used)
         read(1,*) nctrl

                        ! write(6,*) ' egg.dat'
        close(1)

             Nr=iplas+i_vac

             itrmax=100
             Nitmax=5
             nitdel=7
             nitbeg=5 !000


             cnor=1.d0

          jrolim=2  !+(nt-2)/2

         if(nctrl.eq.1 ) then
         
        write(fname,'(a,a)') path(1:kname),'limpnt_d.wr'
        open(1,file=fname)
        
        !open(1,file='limpnt_d.wr')

         read(1,*) nblm
         do 2215 i=1,nblm
         read(1,*) rblm(i),zblm(i)
 2215   continue

        close(1)
         endif

          call rdrec


             !call taburs(0,1.d0,nursb)
             !call f_grid(igdf,nstep)
             call grid_spdr

          call f_wrd

          fvac=b0cen*r0cen

          nr1=nr-1
          nt1=nt-1

          nr2=nr-2
          nt2=nt-2

          iplas1=iplas-1

             iplasm=iplas
             nroi=nr
             ntetj=nt


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                 go to 224
 
           if(keyctr.eq.10) then

        write(fname,'(a,a)') path(1:kname),'tab_q.dat'
        open(1,file=fname)
	    !open(1,file='tab_q.dat') 

	     read(1,*) nutab

	    do i=1,nutab

              read(1,*)  pstab(i), qtab(i)

          enddo

                                              
	    do i=1,nutab

              pstab(i)=pstab(i)/pstab(nutab)

           enddo

        CALL E01BAF(Nutab,pstab,qtab,RRK,CCK,
     *                          nutab+4,WRK,6*nutab+16,IFAIL)

           do i=1,iplas1

	     psia05=1.d0-0.5d0*(psia(i)+psia(i+1))  

          CALL E02BCF(Nutab+4,RRK,CCK,psia05,0,CWk,IFAIL)

          q(i)=cwk(1)*2.d0*pi

           enddo

                 qcen=qtab(1)
                 q(iplas)=qtab(nutab)*2.d0*pi

           endif

 224      continue         
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
             call f_rdexf(ncequi)

             call f_ext_fil(pcequi,ncequi)


          return
          end



