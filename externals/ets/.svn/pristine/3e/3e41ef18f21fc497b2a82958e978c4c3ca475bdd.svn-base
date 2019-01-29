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
      al33n=alp33k(n)
      al33n1=alfa33(n-1)
      dsn=dsk(n)
      dsn1=ds(n-1)
      dtsig=-dt/sigma(n)/dsn
      !dpsit=dpsidt(n)
      !dfit=dfidt(n)
      av=cps_bon
      bv=-bps_bon
      psiext=psi_ext
      pdv=dpdpsi(n)*(psin-psin1)*dvk(n)/dsk(n)
      cbutn=cbut_b(n)*dvk(n)


      a11=-(2*(cbutn+rin1*fn)/fn1**2*fn-2*rin1)/al22n*al22n1/dsn1

      a12=-(-2*(cbutn+rin1*fn)**2/fn1**3/al22n+2*fn1/al33n)*al33n1/dsn1

      c11=-(2*(cbutn+rin1*fn)/fn1**2*fn-2*rin1)/al22n*al22n1/dsn1

      c12=-(-2*(cbutn+rin1*fn)**2/fn1**3/al22n+2*fn1/al33n)*al33n1/dsn1

      h1 =((cbutn+rin1*fn)**2/fn1**2-rin1**2)/al22n-(fn**2-fn1**2)/
     #al33n-pdv

        a(n,1)= a11
        a(n,2)= a12
        a(n,3)= 0.d0
        a(n,4)= 0.d0 

        c(n,1)= c11
        c(n,2)= c12
        c(n,3)=-1.d0
        c(n,4)= 0.d0

	    h(n,1)=h1
	    h(n,2)=0.d0






