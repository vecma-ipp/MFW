;	get the boundary surfaces

   readf,iu,atyp
      if (atyp eq 'soli') then ltyp=0
      if (atyp eq 'dash') then ltyp=2
   readf,iu,np
   rr=fltarr(np)
   zz=fltarr(np)
   readf,iu,rr
   readf,iu,zz
   oplot,rr,zz,line=ltyp
   readf,iu,atyp
      if (atyp eq 'soli') then ltyp=0
      if (atyp eq 'dash') then ltyp=2
   readf,iu,np
   readf,iu,rr
   readf,iu,zz
   oplot,rr,zz,line=ltyp

