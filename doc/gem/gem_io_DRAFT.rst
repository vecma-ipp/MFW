dat files read in plotting
 
gem/plots/pltime.f90:18:  OPEN (12,file='hpl.dat',form='formatted',status='old')
gem/plots/pltime.f90:19:  OPEN (15,file='hdw.dat',form='formatted',status='old')
gem/plots/plhdf5.f90:25:  OPEN(17,file='d1.dat',form='unformatted')
 
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
