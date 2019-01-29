subroutine f90wrap_update_file(filename, n, values, n0)
    implicit none
    external update_file
    
    character(*), intent(in) :: filename
    integer, intent(in) :: n
    real(4), intent(in), dimension(n0) :: values
    integer :: n0
    !f2py intent(hide), depend(values) :: n0 = shape(values,0)
    call update_file(filename, n, values)
end subroutine f90wrap_update_file

