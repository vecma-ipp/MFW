hdw.dat is parameter file (xml)

h0?.dat is used for multiple processors

d0?.dat file is diagnostics data writen

p0?.dat is a snapshot file

t0?.dat is a state file

------------------------------

d1.dat files read in plotting:
 
gem/plots/plhdf5.f90:25:  OPEN(17,file='d1.dat',form='unformatted')

at plhdf5.f90 : 

d1.dat file has to be a HDF5 file 
different from d0?.dat files (diagnostics)
number of rows read is 2*ndg
odd rows is time
even rows are values of en and fl

i.e. row 264:
  enwrite(0,:)=enwrite(0,:)*(lperp/cs)
  enwrite(1:10,:)=enwrite(1:10,:)*(nne*kb*tte)*delta*delta
  enwrite(11:20,:)=enwrite(11:20,:)*cs*delta*delta 
     row 282:
  turbulence%var0d%dtime=enwrite(0,1:ndg)
  turbulence%var0d%en_exb=enwrite(1,1:ndg)
  turbulence%var0d%en_mag=enwrite(4,1:ndg)
  turbulence%var0d%en_el_th=enwrite(2,1:ndg)+enwrite(3,1:ndg)
  turbulence%var0d%en_ion_th(:,1)=enwrite(7,1:ndg)
  turbulence%var0d%en_el_par=enwrite(5,1:ndg)
  turbulence%var0d%en_ion_par(:,1)=enwrite(6,1:ndg)+enwrite(8,1:ndg)
  turbulence%var0d%en_tot=SUM(enwrite(1:8,1:ndg),1)
  turbulence%var0d%fl_el=enwrite(11,1:ndg)
  turbulence%var0d%fl_heatel=enwrite(12,1:ndg)
  turbulence%var0d%fl_ion(:,1)=enwrite(13,1:ndg)
  turbulence%var0d%fl_heation(:,1)=enwrite(14,1:ndg)
  turbulence%var0d%fl_magel=enwrite(15,1:ndg)
  turbulence%var0d%fl_magheatel=enwrite(16,1:ndg)
  turbulence%var0d%fl_magion(:,1)=enwrite(15,1:ndg)
  turbulence%var0d%flmagheation(:,1)=enwrite(17,1:ndg)

--------------------------------------------------------------------

\*.dat files (all) read by the actor

gem/actor/Makefile:140:DFILE	= $(RSER)/d$(irun).dat
gem/actor/Makefile:141:PFILE	= $(RSER)/p$(irun).dat
gem/actor/Makefile:142:TFILE	= $(RSER)/t$(irun).dat
gem/actor/Makefile:154:	rm -f p1.dat
gem/actor/Makefile:155:	ln -s $(TFILE) p1.dat
gem/actor/Makefile:161:	rm -f [dp][1-9].dat [dp][1-9][0-9].dat 
gem/actor/Makefile:164:	ln -s $(DFILE) d1.dat
gem/actor/Makefile:165:	ln -s $(PFILE) p1.dat
gem/actor/Makefile:167:	rm -f p[1-9].dat p[1-9][0-9].dat 
gem/actor/Makefile:168:	ln -s $(TFILE) p1.dat
gem/actor/snap.f90:20:  OPEN (14,file='p1.dat',form='unformatted',status='old')
gem/actor/tsnaps.f90:19:  OPEN (iu,file='tdw.dat',form='unformatted')


p0?.dat files written:


at main/psnaps.F90

  i0=nx00/npesx0
  j0=ny00/npesy0
  k0=ns00/npess0

  DO kp=0,npess0-1
     k1=kp*k0+ngds+1
     k9=kp*k0+ngds+k0

     DO ip=0,npesx0-1
        i1=ip*i0+ngdx+1
        i9=ip*i0+ngdx+i0

        DO jp=0,npesy0-1
           j1=jp*j0+ngdy+1
           j9=jp*j0+ngdy+j0

           WRITE (iu) uu(j1:j9,i1:i9,k1:k9,1:nvsnap)
        END DO
     END DO
  END DO

