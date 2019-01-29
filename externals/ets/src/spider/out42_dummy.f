	subroutine OUT42_C(ng,ygr,num,name)
	include 'double.inc'

	real*4 ygr(*)
	character *20 name

	print '(a20,(20(1p,e12.5)))',name,(ygr(i),i=1,ng)

	return
	end


!	subroutine out42(n_pr,a_print,num,apr)
!	include 'double.inc'
!	real*4  a_print(*)
!	character *20 apr
!       !return
!	call out42_c(n_pr,a_print,num,apr)
!
!
!	return
!	end




           subroutine out42(n,a,m,b)
             real*4 a(*)
             character*30 b

           return
           end

