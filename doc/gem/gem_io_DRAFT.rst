dat files read in plotting:
 
gem/plots/pltime.f90:18:  OPEN (12,file='hpl.dat',form='formatted',status='old')
gem/plots/pltime.f90:19:  OPEN (15,file='hdw.dat',form='formatted',status='old')
gem/plots/plhdf5.f90:25:  OPEN(17,file='d1.dat',form='unformatted')

at plhdf5.f90 : 

d1.dat file is a HDF5 file 
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


dat files read by the actor

gem/actor/gem.F90.orig:176:     IF (npes == 1) hfile='h00.dat'
gem/actor/Makefile:139:HFILE	= $(RSER)/h$(irun).dat
gem/actor/Makefile:140:DFILE	= $(RSER)/d$(irun).dat
gem/actor/Makefile:141:PFILE	= $(RSER)/p$(irun).dat
gem/actor/Makefile:142:TFILE	= $(RSER)/t$(irun).dat
gem/actor/Makefile:154:	rm -f p1.dat
gem/actor/Makefile:155:	ln -s $(TFILE) p1.dat
gem/actor/Makefile:160:	cp -p $(HFILE) hdw.dat
gem/actor/Makefile:161:	rm -f [dp][1-9].dat [dp][1-9][0-9].dat 
gem/actor/Makefile:164:	ln -s $(DFILE) d1.dat
gem/actor/Makefile:165:	ln -s $(PFILE) p1.dat
gem/actor/Makefile:167:	rm -f p[1-9].dat p[1-9][0-9].dat 
gem/actor/Makefile:168:	ln -s $(TFILE) p1.dat
gem/actor/binit.h90:3:  OPEN (15,file='hdw.dat',form='formatted',status='old')
gem/actor/binit.h90:36:     OPEN (15,file='h00.dat',form='formatted')
gem/actor/dinit.h90:3:  OPEN (12,file='hpl.dat',form='formatted',status='old')
gem/actor/dinit.h90:4:  OPEN (15,file='hdw.dat',form='formatted',status='old')
gem/actor/gem.F90:176:     IF (npes == 1) hfile='h00.dat'
gem/actor/pinit.h90:3:  OPEN (12,file='hpl.dat',form='formatted',status='old')
gem/actor/pinit.h90:4:  OPEN (15,file='hdw.dat',form='formatted',status='old')
gem/actor/snap.f90:20:  OPEN (14,file='p1.dat',form='unformatted',status='old')
gem/actor/metest.F90:29:  OPEN (12,file='hpl.dat',form='formatted',status='old')
gem/actor/metest.F90:30:  OPEN (15,file='hdw.dat',form='formatted',status='old')
gem/actor/metest.F90:56:  OPEN (15,file='h00.dat',form='formatted')
gem/actor/dw.F90:37:        OPEN (15,file='hdw.dat',form='formatted',status='old')
gem/actor/dw.F90:69:     IF (npes == 1) hfile='h00.dat'
gem/actor/tsnaps.f90:19:  OPEN (iu,file='tdw.dat',form='unformatted')
gem/actor/sinit.h90:3:  OPEN (12,file='hpl.dat',form='formatted',status='old')
gem/actor/sinit.h90:4:  OPEN (15,file='hdw.dat',form='formatted',status='old')
