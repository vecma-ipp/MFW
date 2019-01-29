       subroutine f_bndmat(rk,zk,nk,rlop,zlop,nlop,rprob,zprob,nprob)

         include 'double.inc'
        include 'parevo.inc'
        parameter(nkp=njlim)
         include 'dim.inc'
         include 'compol.inc'
         include 'compol_add.inc'
         real*8 rk(*),zk(*),rlop(*),zlop(*),rprob(*),zprob(*)


        do j=2,nt1
          rr=r(nr,j)
          zz=z(nr,j)
         do jb=2,nt1

          r0=r(nr,jb)
          z0=z(nr,jb)

          r1=r(nr,jb+1)
          z1=z(nr,jb+1)

          call bint(rr,zz,R0,Z0,r1,z1,Fint,1)

          binadg(jb,j)=fint

         enddo
        enddo

         return
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

           ik_out=0

        do ik=1,nk

          rr=rk(ik)
          zz=zk(ik)

         call numcel(rr,zz,ic,jc)

          if(ic.lt.nr) then !!!

           iprcon(ik)=1

          else !!!!!!!!!!!!!!!!!!!!!

           ik_out=ik_out+1
           iprcon(ik)=0

          endif !!!!!!!!!!!!!!!!!!!!

          do 110 j=2,nt1

           r0=r(nr,j)
           z0=z(nr,j)

           r1=r(nr,j+1)
           z1=z(nr,j+1)

           call bint(rr,zz,R0,Z0,r1,z1,Fint,1)

           pinadg(ik,j)=Fint
          !pinadg(ik_out,j)=Fint

          !psitok(ik)=psitok(ik)-Fint*(dgdn(j)+dgdn(j+1))*0.5d0

 110     continue

         ! endif !!!!!!!!!!!!!!!!!!!!

       enddo

           nk_out=ik_out

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

           ik_out=0

        do ik=1,nlop

          rr=rlop(ik)
          zz=zlop(ik)

         call numcel(rr,zz,ic,jc)

          if(ic.lt.nr) then !!!

           iprlop(ik)=1

          else !!!!!!!!!!!!!!!!!!!!!

           ik_out=ik_out+1
           iprlop(ik)=0

          do 120 j=2,nt1

        r0=r(nr,j)
        z0=z(nr,j)

        r1=r(nr,j+1)
        z1=z(nr,j+1)

         call bint(rr,zz,R0,Z0,r1,z1,Fint,1)

         adginl(ik_out,j)=Fint

         !psitok(ik)=psitok(ik)-Fint*(dgdn(j)+dgdn(j+1))*0.5d0

 120       continue

          endif !!!!!!!!!!!!!!!!!!!!

       enddo

           nlop_out=ik_out


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

           ik_out=0

        do ik=1,nprob

          rr=rprob(ik)
          zz=zprob(ik)

         call numcel(rr,zz,ic,jc)

          if(ic.lt.nr) then !!!

           iprprob(ik)=1

          else !!!!!!!!!!!!!!!!!!!!!

           ik_out=ik_out+1
           iprprob(ik)=0

          do 130 j=2,nt1

        r0=r(nr,j)
        z0=z(nr,j)

        r1=r(nr,j+1)
        z1=z(nr,j+1)

         call bint_d(rr,zz,R0,Z0,r1,z1,Fint_r,Fint_z,1)

         adginr(ik_out,j)=Fint_r
         adginz(ik_out,j)=Fint_z

         !psitok(ik)=psitok(ik)-Fint*(dgdn(j)+dgdn(j+1))*0.5d0

 130       continue

          endif !!!!!!!!!!!!!!!!!!!!

       enddo

           nprob_out=ik_out

        return
        end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

C********************************************************************
C********************************************************************
C********************************************************************
C********************************************************************

