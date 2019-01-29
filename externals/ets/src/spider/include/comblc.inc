           include 'comlmtr.inc'

         common /comcon/ alp,alpnew,tok,tokn,cnor,qcen,ucen,b0ax,r0ax

         common /comind/ ni,nj,ni1,nj1,ni2,nj2,nbnd,nkin,nkout

         common /comrz/  r(nip),z(njp),dr(nip),dz(njp),
     +                   dri(nip),dzj(njp),r12(nip)

         common /commat/ a(lp),right(neqp),ja(lp),ia(neq1p),neq,nnz

         common /comdom/ rmax,rmin,zmax,zmin

         common /comitr/ erru,errx,errm,eps,iter,itin,nrun,icont,
     +                   Nitl,Nitin

         common /comtok/ itok(nkp),jtok(nkp)

         common /compot/ u(nip,njp),ue(nip,njp),un(nip,njp),
     +                   ui(nip,njp),g(nip,njp),
     +                   ux0,ux1,ux2,up,um,rm,zm,
     +                  rx0,zx0,rx1,zx1,rx2,zx2,imax,jmax,
     +                  ix1,jx1,ix2,jx2,
     +                  rx10,zx10,rx20,zx20,rm0,zm0,
     +                  psi_bon

         common /combon/ dgdn(nbndp),binadg(nbndp,nbndp),
     +                   pinadg(nkp,nbndp)

         common /compla/ curf(nip,njp),f(nip,njp),q(nip,njp),
     +                   ipr(nip,njp)

         common /comstb/ rl,zl,clr,clz,iterbf


         common /comstp/ f_cur,nnstpp
