	subroutine b_extrp(x0,X1,X2,X3, j1,j2,j3,j,yarr,jerr)
	real*8	yarr(*),u0,u1,u2,u3,x0,X1,X2,X3
c        include 'double.inc'
        include 'dim.inc'
	integer j,jerr,j1,j2,j3
	jerr=0 
  	u1	=yarr(j1)
	u2	=yarr(j2)
	u3	=yarr(j3)     
	call EXTRP2(x0,u0, X1,X2,X3, u1,u2,u3)
        if(j.gt.nrp.or.j.lt.1)then
	write(*,*) 'Equil: in b_extrp j = ', j,' out of range 1 <',j,' < ',nrp
	jerr=1
	return
	endif
	yarr(j)=u0
	return
	end
	
	
