program test_buf
  use init_cpo_wrapper
  use string_binding
  implicit none

  character(kind=c_char,len=F_STR_SIZE) :: cpo_file_in
  character(kind=c_char,len=F_STR_SIZE) :: corep_out
  character(kind=c_char,len=F_STR_SIZE) :: coret_out
  character(kind=c_char,len=F_STR_SIZE) :: cores_out
  character(kind=c_char,len=F_STR_SIZE) :: corei_out
  character(kind=c_char,len=F_STR_SIZE) :: coren_out
  character(kind=c_char,len=F_STR_SIZE) :: equil_out
  character(kind=c_char,len=F_STR_SIZE) :: toroidf_out

  integer(kind=c_signed_char), dimension(BUF_SIZE) :: corep
  integer(kind=c_signed_char), dimension(BUF_SIZE) :: coret
  integer(kind=c_signed_char), dimension(BUF_SIZE) :: cores
  integer(kind=c_signed_char), dimension(BUF_SIZE) :: corei
  integer(kind=c_signed_char), dimension(BUF_SIZE) :: coren
  integer(kind=c_signed_char), dimension(BUF_SIZE) :: equil
  integer(kind=c_signed_char), dimension(BUF_SIZE) :: toroidf



  cpo_file_in = c_char_"CPO_000004_000001"//char(0)

  print *,'call init_cpo2file'
  call init_cpo2file(cpo_file_in, &
       corep_out, coret_out, cores_out, corei_out, &
       coren_out, equil_out, toroidf_out)

  print *,'call init_cpo2buf'
  call init_cpo2buf(cpo_file_in, &
       corep, coret, cores, corei, &
       coren, equil, toroidf)

  print *,'now just have to check if diff on files'


end program test_buf
