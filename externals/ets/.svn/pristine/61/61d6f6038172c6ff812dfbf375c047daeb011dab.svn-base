      psi0n=psi0(n)
      psin=psi(n)
      psin1=psi(n-1)
      fi0n=fi0(n)
      fin=fi(n)
      fin1=fi(n-1)
      fn=fbnd
      fn1=f(n-1)
      rin=ri(n)
      rin1=ri(n-1)
      al22n=alfa22(n)
      al22n1=alfa22(n-1)
      al22x=1.d1
      al33n=alp33k(n)
      al33n1=alfa33(n-1)
      dsn=dsk(n)
      dsn1=ds(n-1)
      dtsig=-dt/sigma(n)/dsn
      !dpsit=dpsidt(n)
      !dfit=dfidt(n)
      av=cps_bon
      bv=-bps_bon
      cv=-dps_bon
      psiext=psi_ext
      pdv=dpdpsi(n)*(psin-psin1)*dvk(n)/dsk(n)
      pbb=dpdpsi(n)*(psin-psin1)*cv
      cbutn=cbut_b(n)*dvk(n)


      a11= -(2*(rin1*al33n*fin-rin1*al33n*fi0n+2*rin1*fn*dtsig*al22n*al3
     #3n-al22n*psin*fn-al22n*psin*fn1+al22n*psi0n*fn+al22n*psi0n*fn1+2*c
     #butn*dtsig*al22n*al33n)/al33n**2/(-2*fn1*dtsig*al22n+fin-fi0n)**2*
     #(al33n*fin-al33n*fi0n+2*fn*dtsig*al22n*al33n)-2*rin1)/al22n*al22n1
     #/dsn1



      a12= -((2*(rin1*al33n*fin-rin1*al33n*fi0n+2*rin1*fn*dtsig*al22n*al
     #33n-al22n*psin*fn-al22n*psin*fn1+al22n*psi0n*fn+al22n*psi0n*fn1+2*
     #cbutn*dtsig*al22n*al33n)/al33n**2/(-2*fn1*dtsig*al22n+fin-fi0n)**2
     #*(-al22n*psin+al22n*psi0n)+4*(rin1*al33n*fin-rin1*al33n*fi0n+2*rin
     #1*fn*dtsig*al22n*al33n-al22n*psin*fn-al22n*psin*fn1+al22n*psi0n*fn
     #+al22n*psi0n*fn1+2*cbutn*dtsig*al22n*al33n)**2/al33n**2/(-2*fn1*dt
     #sig*al22n+fin-fi0n)**3*dtsig*al22n)/al22n+2*fn1/al33n)*al33n1/dsn1




      c11= -(2*(rin1*al33n*fin-rin1*al33n*fi0n+2*rin1*fn*dtsig*al22n*al3
     #3n-al22n*psin*fn-al22n*psin*fn1+al22n*psi0n*fn+al22n*psi0n*fn1+2*c
     #butn*dtsig*al22n*al33n)/al33n**2/(-2*fn1*dtsig*al22n+fin-fi0n)**2*
     #(al33n*fin-al33n*fi0n+2*fn*dtsig*al22n*al33n)-2*rin1)/al22n*al22n1
     #/dsn1-2*(rin1*al33n*fin-rin1*al33n*fi0n+2*rin1*fn*dtsig*al22n*al33
     #n-al22n*psin*fn-al22n*psin*fn1+al22n*psi0n*fn+al22n*psi0n*fn1+2*cb
     #utn*dtsig*al22n*al33n)/al33n**2/(-2*fn1*dtsig*al22n+fin-fi0n)**2*(
     #-al22n*fn-al22n*fn1)/al22n


      s1= -((2*(rin1*al33n*fin-rin1*al33n*fi0n+2*rin1*fn*dtsig*al22n*al
     #33n-al22n*psin*fn-al22n*psin*fn1+al22n*psi0n*fn+al22n*psi0n*fn1+2*
     #cbutn*dtsig*al22n*al33n)/al33n**2/(-2*fn1*dtsig*al22n+fin-fi0n)**2
     #*(-al22n*psin+al22n*psi0n)+4*(rin1*al33n*fin-rin1*al33n*fi0n+2*rin
     #1*fn*dtsig*al22n*al33n-al22n*psin*fn-al22n*psin*fn1+al22n*psi0n*fn
     #+al22n*psi0n*fn1+2*cbutn*dtsig*al22n*al33n)**2/al33n**2/(-2*fn1*dt
     #sig*al22n+fin-fi0n)**3*dtsig*al22n)/al22n+2*fn1/al33n)*al33n1/dsn1

      s2 = -(2*(rin1*al33n*fin-rin1*al33n*fi0n+2*rin1*fn*dtsig*al22n*al3
     #3n-al22n*psin*fn-al22n*psin*fn1+al22n*psi0n*fn+al22n*psi0n*fn1+2*c
     #butn*dtsig*al22n*al33n)/al33n/(-2*fn1*dtsig*al22n+fin-fi0n)**2*rin
     #1-2*(rin1*al33n*fin-rin1*al33n*fi0n+2*rin1*fn*dtsig*al22n*al33n-al
     #22n*psin*fn-al22n*psin*fn1+al22n*psi0n*fn+al22n*psi0n*fn1+2*cbutn*
     #dtsig*al22n*al33n)**2/al33n**2/(-2*fn1*dtsig*al22n+fin-fi0n)**3)/a
     #l22n

      c12= s1+s2




      a21= -2*av*(psin-psin1)-psin+psiext




      a22= 2*bv*fn1*al33n1/dsn1


      c21= -(1+av)*(psin-psin1)-psin+psiext-av*(psin-psin1)


      c22= 2*bv*fn1*al33n1/dsn1



      h1= ((rin1*al33n*fin-rin1*al33n*fi0n+2*rin1*fn*dtsig*al22n*al33n-
     #al22n*psin*fn-al22n*psin*fn1+al22n*psi0n*fn+al22n*psi0n*fn1+2*cbut
     #n*dtsig*al22n*al33n)**2/al33n**2/(-2*fn1*dtsig*al22n+fin-fi0n)**2-
     #rin1**2)/al22n-(fn**2-fn1**2)/al33n-pdv




      h2= (psin-psiext+av*(psin-psin1))*(psin-psin1)+bv*(fn**2-fn1**2)+
     #pbb





        a(n,1)= a11
        a(n,2)= a12
        a(n,3)= a21  ! 0.d0
        a(n,4)= a22  ! 0.d0 
        !a(n,3)= 0.d0
        !a(n,4)= 0.d0 

        c(n,1)= c11
        c(n,2)= c12
        c(n,3)= c21  ! -1.d0
        c(n,4)= c22  !  0.d0
        !c(n,3)=-1.d0
        !c(n,4)= 0.d0

	    h(n,1)=h1
	    h(n,2)=h2   !0.d0
	    !h(n,2)=0.d0







