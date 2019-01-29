module c_perf
  use iso_c_binding
  implicit none

  interface 
     subroutine c_getMillis(t) bind(c,name='getMillis')
       use iso_c_binding
       integer(kind=c_long_long), intent(out) :: t
     end subroutine c_getMillis
  end interface

end module c_perf
