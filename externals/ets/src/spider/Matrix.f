         !subroutine matrix_ffs
         subroutine matrix

         include 'double.inc'
         include 'param.inc'
         include 'comblc.inc'

          common /comffs/ axm(nip),ax0(nip),axp(nip)

!matrix pakage for fourier solver

         do i=2,ni1

          ri=r(i)
          drpl=dr(i)
          drmn=dr(i-1)
          rpl=r12(i)
          rmn=r12(i-1)

         do j=2,nj1

          dzpl=dz(j)
          dzmn=dz(j-1)


          axm(i)=ri/(drmn*rmn*dri(i))
          axp(i)=ri/(drpl*rpl*dri(i))
          ax0(i)=axm(i)+axp(i)

         enddo
         enddo

         return
         end
!****************************************************************
         subroutine matrix_spar
         !subroutine matrix

         include 'double.inc'
         include 'param.inc'
         include 'comblc.inc'



c   initialization of arrays ia(il),ja(im),a(im)
c
c   il - number of the matrix  line  (equation number)
c   im - number of an element in the array a(im)
c   ia(il) - number of the first nonzero element in the line  il
c   ja(im) - column number of a(im)


            il=0
            im=0



         do 100 j=2,nj1

          do 100 i=2,ni1

            il=il+1

            ia(il)=im+1


          dzpl=dz(j)
          dzmn=dz(j-1)

          ri=r(i)
          drpl=dr(i)
          drmn=dr(i-1)
          rpl=r12(i)
          rmn=r12(i-1)

          a1=dri(i)/(dzmn*ri)
          a2=dzj(j)/(drmn*rmn)
          a4=dzj(j)/(drpl*rpl)
          a5=dri(i)/(dzpl*ri)

          a3=-(a1+a2+a4+a5)

              if(j.eq.2) go to 10

c--1---cof to u(i,j-1)

          im=im+1

          ja(im)=nlin(i,j-1)

          a(im)=a1

 10           continue

              if(i.eq.2) go to 20

c--2---cof to u(i-1,j)

          im=im+1

          ja(im)=nlin(i-1,j)

          a(im)=a2

 20           continue

c--3---cof to u(i,j)

          im=im+1

          ja(im)=nlin(i,j)

          a(im)=a3

              if(i.eq.ni1) go to 30

c--4---cof to u(i+1,j)

          im=im+1

          ja(im)=nlin(i+1,j)

          a(im)=a4

 30           continue

              if(j.eq.nj1) go to 40

c--5---cof to u(i,j+1)

          im=im+1

          ja(im)=nlin(i,j+1)

          a(im)=a5

 40           continue

 100     continue

             nnz=im
             neq=il

         !write(6,*) 'matrix:neq,nnz',neq,nnz
         !write(6,*) 'matrix:neqp,lp',neqp,lp

           il=il+1
           ia(il)=im+1

             neqer=neqp-neq

         if(neqer.lt.0) then
         !write(6,*) '******error in subr. matrix:'
         !write(6,*) 'number of equations gt. then array dimension'
         !write(6,*) 'matrix:neqp,neq',neqp,neq
         stop
         endif

              lper=lp-nnz

         if( lper.lt.0) then
         !write(6,*) '******error in subr. matrix:'
         !write(6,*) 'number of nonzero el. gt. then array dimension'
         !write(6,*) 'matrix:lp,nnz',lp,nnz
         stop
         endif


         return
         end


         function nlin(i,j)

         include 'double.inc'
         include 'param.inc'
         include 'comblc.inc'

c        number of u(i,j)

         nlin=(j-2)*ni2+(i-1)

         return
         end
