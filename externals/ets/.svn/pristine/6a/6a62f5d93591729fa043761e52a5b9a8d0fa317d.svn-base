        SUBROUTINE wrcoil(nk,nkcoil,rk,zk,tk,necon,wecon)

        INCLUDE 'double.inc'

        real*8 rk(*),zk(*),tk(*),wecon(*)
        integer nk,nkcoil,necon(*)

        write(fname,'(a,a)') path(1:kname),'ecur.wr'
        open(1,file=fname,form='formatted')
         !open(1,file='ecur.wr',form='formatted')

         write(1,*) nk,nkcoil

         write(1,*) (rk(i),i=1,nk)
         write(1,*) (zk(j),j=1,nk)
         write(1,*) (tk(j),j=1,nk)
         write(1,*) (wecon(j),j=1,nkcoil)
         write(1,*) (necon(j),j=1,nkcoil)
         close(1)
            RETURN
            END

        SUBROUTINE rdcoil(nk,nkcoil,rk,zk,tk,necon,wecon)

        INCLUDE 'double.inc'

        real*8 rk(*),zk(*),tk(*),wecon(*)
        integer nk,nkcoil,necon(*)

        write(fname,'(a,a)') path(1:kname),'ecur.wr'
        open(1,file=fname,form='formatted')
         !open(1,file='ecur.wr',form='formatted')

         read(1,*) nk,nkcoil

         read(1,*) (rk(i),i=1,nk)
         read(1,*) (zk(j),j=1,nk)
         read(1,*) (tk(j),j=1,nk)
         read(1,*) (wecon(j),j=1,nkcoil)
         read(1,*) (necon(j),j=1,nkcoil)
         close(1)
            RETURN
            END

