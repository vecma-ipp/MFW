subroutine euitm_open_env(name, shot, run, retIdx, user, tokamak, version)
use itm_types
implicit none
integer shot,run,retIdx
character (len=*) :: name,user,tokamak,version
stop 'Should not call euitm_open_env'
end subroutine euitm_open_env

subroutine euitm_close(idx)
use itm_types
implicit none
integer idx
end subroutine euitm_close

subroutine begin_cpo_get(idx,path,int1,dum1)
use itm_types
implicit none
integer idx,int1,dum1
character (len=*) :: path
end subroutine begin_cpo_get

subroutine end_cpo_get(idx,path)
use itm_types
implicit none
integer idx
character (len=*) :: path
end subroutine end_cpo_get

subroutine begin_cpo_get_slice(idx,path,real1,dum1)
use itm_types
implicit none
integer idx,dum1
real (R8) real1
character (len=*) :: path
end subroutine begin_cpo_get_slice

subroutine end_cpo_get_slice(idx,path)
use itm_types
implicit none
integer idx
character (len=*) :: path
end subroutine end_cpo_get_slice

subroutine begin_cpo_put_slice(idx,path,real1)
use itm_types
implicit none
integer idx
real (R8) real1
character (len=*) :: path
end subroutine begin_cpo_put_slice

subroutine end_cpo_put_slice(idx,path)
use itm_types
implicit none
integer idx
character (len=*) :: path
end subroutine end_cpo_put_slice

subroutine begin_cpo_put_non_timed(idx,path)
use itm_types
implicit none
integer idx
character (len=*) :: path
end subroutine begin_cpo_put_non_timed

subroutine end_cpo_put_non_timed(idx,path)
use itm_types
implicit none
integer idx
character (len=*) :: path
end subroutine end_cpo_put_non_timed

subroutine begin_cpo_put_timed(idx,path,lentime,real1)
use itm_types
implicit none
integer idx,lentime
real (R8) real1
character (len=*) :: path
end subroutine begin_cpo_put_timed

subroutine end_cpo_put_timed(idx,path)
use itm_types
implicit none
integer idx
character (len=*) :: path
end subroutine end_cpo_put_timed

subroutine get_dimension(idx,path,string,ndims,dim1,dim2,dim3,dim4)
use itm_types
implicit none
integer idx, ndims, dim1, dim2, dim3, dim4
character (len=*) :: path, string
end subroutine get_dimension

subroutine get_int(idx,path,string,int0d,status)
use itm_types
implicit none
integer idx,status,int0d
character (len=*) :: path, string
end subroutine get_int

subroutine get_double(idx,path,string,double0d,status)
use itm_types
implicit none
integer idx,status
real (R8) double0d
character (len=*) :: path, string
end subroutine get_double

subroutine get_string(idx,path,string,longstring,status)
use itm_types
implicit none
integer idx,status
character (len=*) :: path, string, longstring
end subroutine get_string

subroutine get_vect1d_double(idx,path,string,vect1Ddouble,dim1,dum1,status)
use itm_types
implicit none
integer idx, ndims, dim1, dum1, status
real (R8) vect1Ddouble(dim1)
character (len=*) :: path, string
end subroutine get_vect1d_double

subroutine get_vect2d_double(idx,path,string,vect2Ddouble,dim1,dim2,dum1,dum2,status)
use itm_types
implicit none
integer idx, ndims, dim1, dim2, dum1, dum2, status
real (R8) vect2Ddouble(dim1,dim2)
character (len=*) :: path, string
end subroutine get_vect2d_double

subroutine get_vect3d_double(idx,path,string,vect3Ddouble,dim1,dim2,dim3,dum1,dum2,dum3,status)
use itm_types
implicit none
integer idx, ndims, dim1, dim2, dim3, dum1, dum2, dum3, status
real (R8) vect3Ddouble(dim1,dim2,dim3)
character (len=*) :: path, string
end subroutine get_vect3d_double

subroutine get_vect4d_double(idx,path,string,vect4Ddouble,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4,status)
use itm_types
implicit none
integer idx, ndims, dim1, dim2, dim3, dim4, dum1, dum2, dum3, dum4, status
real (R8) vect4Ddouble(dim1,dim2,dim3,dim4)
character (len=*) :: path, string
end subroutine get_vect4d_double

subroutine get_vect5d_double(idx,path,string,vect5Ddouble,dim1,dim2,dim3,dim4,dim5,dum1,dum2,dum3,dum4,dum5,status)
use itm_types
implicit none
integer idx, ndims, dim1, dim2, dim3, dim4, dim5, dum1, dum2, dum3, dum4, dum5, status
real (R8) vect5Ddouble(dim1,dim2,dim3,dim4,dim5)
character (len=*) :: path, string
end subroutine get_vect5d_double

subroutine get_vect6d_double(idx,path,string,vect6Ddouble,dim1,dim2,dim3,dim4,dim5,dim6,dum1,dum2,dum3,dum4,dum5,dum6,status)
use itm_types
implicit none
integer idx, ndims, dim1, dim2, dim3, dim4, dim5, dim6, dum1, dum2, dum3, dum4, dum5, dum6, status
real (R8) vect6Ddouble(dim1,dim2,dim3,dim4,dim5,dum6)
character (len=*) :: path, string
end subroutine get_vect6d_double

subroutine get_vect1d_int(idx,path,string,vect1Dint,dim1,dum1,status)
use itm_types
implicit none
integer idx, ndims, dim1, dum1, status, vect1Dint(dim1)
character (len=*) :: path, string
end subroutine get_vect1d_int

subroutine get_vect2d_int(idx,path,string,vect2Dint,dim1,dim2,dum1,dum2,status)
use itm_types
implicit none
integer idx, ndims, dim1, dim2, dum1, dum2, status, vect2Dint(dim1,dim2)
character (len=*) :: path, string
end subroutine get_vect2d_int

subroutine get_vect3d_int(idx,path,string,vect3Dint,dim1,dim2,dim3,dum1,dum2,dum3,status)
use itm_types
implicit none
integer idx, ndims, dim1, dim2, dim3, dum1, dum2, dum3, status, vect3Dint(dim1,dim2,dim3)
character (len=*) :: path, string
end subroutine get_vect3d_int

subroutine get_vect4d_int(idx,path,string,vect4Dint,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4,status)
use itm_types
implicit none
integer idx, ndims, dim1, dim2, dim3, dim4, dum1, dum2, dum3, dum4, status, vect4Dint(dim1,dim2,dim3,dim4)
character (len=*) :: path, string
end subroutine get_vect4d_int

subroutine get_vect1d_string(idx,path,string,stringpointer,dim1,dum1,status)
use itm_types
implicit none
integer idx, ndims, dim1, dum1, status
character (len=*) :: path, string
character(len=132), dimension(:), pointer :: stringpointer
end subroutine get_vect1d_string

subroutine get_int_slice(idx,path,string,int0d,twant,tret,interpol,status)
use itm_types
implicit none
real (R8) twant, tret
integer interpol
integer idx,status,int0d
character (len=*) :: path, string
end subroutine get_int_slice

subroutine get_double_slice(idx,path,string,double0d,twant,tret,interpol,status)
use itm_types
implicit none
real (R8) double0d
real (R8) twant, tret
integer interpol
integer idx,status
character (len=*) :: path, string
end subroutine get_double_slice

subroutine get_string_slice(idx,path,string,longstring,twant,tret,interpol,status)
use itm_types
implicit none
real (R8) twant, tret
integer interpol
integer idx,status
character (len=*) :: path, string, longstring
end subroutine get_string_slice

subroutine get_vect1d_double_slice(idx,path,string,vect1Ddouble,dim1,dum1,twant,tret,interpol,status)
use itm_types
implicit none
integer interpol
integer idx, ndims, dim1, dum1, status
real (R8) vect1Ddouble(dim1)
real (R8) twant, tret
character (len=*) :: path, string
end subroutine get_vect1d_double_slice

subroutine get_vect2d_double_slice(idx,path,string,vect2Ddouble,dim1,dim2,dum1,dum2,twant,tret,interpol,status)
use itm_types
implicit none
integer interpol
integer idx, ndims, dim1, dim2, dum1, dum2, status
real (R8) vect2Ddouble(dim1,dim2)
real (R8) twant, tret
character (len=*) :: path, string
end subroutine get_vect2d_double_slice

subroutine get_vect3d_double_slice(idx,path,string,vect3Ddouble,dim1,dim2,dim3,dum1,dum2,dum3,twant,tret,interpol,status)
use itm_types
implicit none
integer interpol
integer idx, ndims, dim1, dim2, dim3, dum1, dum2, dum3, status
real (R8) vect3Ddouble(dim1,dim2,dim3)
real (R8) twant, tret
character (len=*) :: path, string
end subroutine get_vect3d_double_slice

subroutine get_vect4d_double_slice(idx,path,string,vect4Ddouble,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4,twant,tret,interpol,status)
use itm_types
implicit none
integer interpol
integer idx, ndims, dim1, dim2, dim3, dim4, dum1, dum2, dum3, dum4, status
real (R8) vect4Ddouble(dim1,dim2,dim3,dim4)
real (R8) twant, tret
character (len=*) :: path, string
end subroutine get_vect4d_double_slice

subroutine get_vect5d_double_slice(idx,path,string,vect5Ddouble,dim1,dim2,dim3,dim4,dim5,  &
     dum1,dum2,dum3,dum4,dum5,twant,tret,interpol,status)
use itm_types
implicit none
integer interpol
integer idx, ndims, dim1, dim2, dim3, dim4, dim5, dum1, dum2, dum3, dum4, dum5, status
real (R8) vect5Ddouble(dim1,dim2,dim3,dim4,dim5)
real (R8) twant, tret
character (len=*) :: path, string
end subroutine get_vect5d_double_slice

subroutine get_vect6d_double_slice(idx,path,string,vect6Ddouble,dim1,dim2,dim3,dim4,dim5,dim6,  &
     dum1,dum2,dum3,dum4,dum5,dum6,twant,tret,interpol,status)
use itm_types
implicit none
integer interpol
integer idx, ndims, dim1, dim2, dim3, dim4, dim5, dim6, dum1, dum2, dum3, dum4, dum5, dum6, status
real (R8) vect6Ddouble(dim1,dim2,dim3,dim4,dim5,dum6)
real (R8) twant, tret
character (len=*) :: path, string
end subroutine get_vect6d_double_slice

subroutine get_vect1d_int_slice(idx,path,string,vect1Dint,dim1,dum1,twant,tret,interpol,status)
use itm_types
implicit none
integer interpol
integer idx, ndims, dim1, dum1, status, vect1Dint(dim1)
real (R8) twant, tret
character (len=*) :: path, string
end subroutine get_vect1d_int_slice

subroutine get_vect2d_int_slice(idx,path,string,vect2Dint,dim1,dim2,dum1,dum2,twant,tret,interpol,status)
use itm_types
implicit none
integer interpol
integer idx, ndims, dim1, dim2, dum1, dum2, status, vect2Dint(dim1,dim2)
real (R8) twant, tret
character (len=*) :: path, string
end subroutine get_vect2d_int_slice

subroutine get_vect3d_int_slice(idx,path,string,vect3Dint,dim1,dim2,dim3,dum1,dum2,dum3,twant,tret,interpol,status)
use itm_types
implicit none
integer interpol
integer idx, ndims, dim1, dim2, dim3, dum1, dum2, dum3, status, vect3Dint(dim1,dim2,dim3)
real (R8) twant, tret
character (len=*) :: path, string
end subroutine get_vect3d_int_slice

subroutine get_vect4d_int_slice(idx,path,string,vect4Dint,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4,twant,tret,interpol,status)
use itm_types
implicit none
integer interpol
integer idx, ndims, dim1, dim2, dim3, dim4, dum1, dum2, dum3, dum4, status, vect4Dint(dim1,dim2,dim3,dim4)
real (R8) twant, tret
character (len=*) :: path, string
end subroutine get_vect4d_int_slice

subroutine get_vect1d_string_slice(idx,path,string,stringpointer,dim1,dum1,twant,tret,interpol,status)
use itm_types
implicit none
integer interpol
integer idx, ndims, dim1, dum1, status
real (R8) twant, tret
character (len=*) :: path, string
character(len=132), dimension(:), pointer :: stringpointer
end subroutine get_vect1d_string_slice

subroutine put_int(idx,path,string,int0d)
use itm_types
implicit none
integer idx,int0d
character (len=*) :: path, string
end subroutine put_int

subroutine put_double(idx,path,string,double0d)
use itm_types
implicit none
integer idx
real (R8) double0d
character (len=*) :: path, string
end subroutine put_double

subroutine put_string(idx,path,string,longstring)
use itm_types
implicit none
integer idx
character (len=*) :: path, string, longstring
end subroutine put_string

subroutine put_vect1d_double(idx,path,string,vect1Ddouble,dim1,dum1)
use itm_types
implicit none
integer idx, ndims, dim1, dum1
real (R8) vect1Ddouble(dim1)
character (len=*) :: path, string
end subroutine put_vect1d_double

subroutine put_vect2d_double(idx,path,string,vect2Ddouble,dim1,dim2,dum1,dum2)
use itm_types
implicit none
integer idx, ndims, dim1, dim2, dum1, dum2
real (R8) vect2Ddouble(dim1,dim2)
character (len=*) :: path, string
end subroutine put_vect2d_double

subroutine put_vect3d_double(idx,path,string,vect3Ddouble,dim1,dim2,dim3,dum1,dum2,dum3)
use itm_types
implicit none
integer idx, ndims, dim1, dim2, dim3, dum1, dum2, dum3
real (R8) vect3Ddouble(dim1,dim2,dim3)
character (len=*) :: path, string
end subroutine put_vect3d_double

subroutine put_vect4d_double(idx,path,string,vect4Ddouble,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4)
use itm_types
implicit none
integer idx, ndims, dim1, dim2, dim3, dim4, dum1, dum2, dum3, dum4
real (R8) vect4Ddouble(dim1,dim2,dim3,dim4)
character (len=*) :: path, string
end subroutine put_vect4d_double

subroutine put_vect5d_double(idx,path,string,vect5Ddouble,dim1,dim2,dim3,dim4,dim5,dum1,dum2,dum3,dum4,dum5)
use itm_types
implicit none
integer idx, ndims, dim1, dim2, dim3, dim4, dim5, dum1, dum2, dum3, dum4, dum5
real (R8) vect5Ddouble(dim1,dim2,dim3,dim4,dim5)
character (len=*) :: path, string
end subroutine put_vect5d_double

subroutine put_vect6d_double(idx,path,string,vect6Ddouble,dim1,dim2,dim3,dim4,dim5,dim6,dum1,dum2,dum3,dum4,dum5,dum6)
use itm_types
implicit none
integer idx, ndims, dim1, dim2, dim3, dim4, dim5, dim6, dum1, dum2, dum3, dum4, dum5, dum6
real (R8) vect6Ddouble(dim1,dim2,dim3,dim4,dim5,dim6)
character (len=*) :: path, string
end subroutine put_vect6d_double

subroutine put_vect1d_int(idx,path,string,vect1Dint,dim1,dum1)
use itm_types
implicit none
integer idx, ndims, dim1, dum1, vect1Dint(dim1)
character (len=*) :: path, string
end subroutine put_vect1d_int

subroutine put_vect2d_int(idx,path,string,vect2Dint,dim1,dim2,dum1,dum2)
use itm_types
implicit none
integer idx, ndims, dim1, dim2, dum1, dum2, vect2Dint(dim1,dim2)
character (len=*) :: path, string
end subroutine put_vect2d_int

subroutine put_vect3d_int(idx,path,string,vect3Dint,dim1,dim2,dim3,dum1,dum2,dum3)
use itm_types
implicit none
integer idx, ndims, dim1, dim2, dim3, dum1, dum2, dum3, vect3Dint(dim1,dim2,dim3)
character (len=*) :: path, string
end subroutine put_vect3d_int

subroutine put_vect4d_int(idx,path,string,vect4Dint,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4)
use itm_types
implicit none
integer idx, ndims, dim1, dim2, dim3, dim4, dum1, dum2, dum3, dum4, vect4Dint(dim1,dim2,dim3,dim4)
character (len=*) :: path, string
end subroutine put_vect4d_int

subroutine put_vect1d_string(idx,path,string,stringpointer,dim1,dum1)
use itm_types
implicit none
integer idx, ndims, dim1, dum1
character (len=*) :: path, string
character(len=132), dimension(:), pointer :: stringpointer
end subroutine put_vect1d_string

subroutine put_int_slice(idx,path,string,int0d,twant,tret,interpol)
use itm_types
implicit none
integer interpol
integer idx,int0d
real (R8) twant, tret
character (len=*) :: path, string
end subroutine put_int_slice

subroutine put_double_slice(idx,path,string,double0d,twant,tret,interpol)
use itm_types
implicit none
integer interpol
integer idx
real (R8) double0d
real (R8) twant, tret
character (len=*) :: path, string
end subroutine put_double_slice

subroutine put_string_slice(idx,path,string,longstring,twant,tret,interpol)
use itm_types
implicit none
integer interpol
integer idx
real (R8) twant, tret
character (len=*) :: path, string, longstring
end subroutine put_string_slice

subroutine put_vect1d_double_slice(idx,path,string,vect1Ddouble,dim1,dum1,twant,tret,interpol)
use itm_types
implicit none
integer interpol
integer idx, ndims, dim1, dum1
real (R8) vect1Ddouble(dim1)
real (R8) twant, tret
character (len=*) :: path, string
end subroutine put_vect1d_double_slice

subroutine put_vect2d_double_slice(idx,path,string,vect2Ddouble,dim1,dim2,dum1,dum2,twant,tret,interpol)
use itm_types
implicit none
integer interpol
integer idx, ndims, dim1, dim2, dum1, dum2
real (R8) vect2Ddouble(dim1,dim2)
real (R8) twant, tret
character (len=*) :: path, string
end subroutine put_vect2d_double_slice

subroutine put_vect3d_double_slice(idx,path,string,vect3Ddouble,dim1,dim2,dim3,dum1,dum2,dum3,twant,tret,interpol)
use itm_types
implicit none
integer interpol
integer idx, ndims, dim1, dim2, dim3, dum1, dum2, dum3
real (R8) vect3Ddouble(dim1,dim2,dim3)
real (R8) twant, tret
character (len=*) :: path, string
end subroutine put_vect3d_double_slice

subroutine put_vect4d_double_slice(idx,path,string,vect4Ddouble,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4,twant,tret,interpol)
use itm_types
implicit none
integer interpol
integer idx, ndims, dim1, dim2, dim3, dim4, dum1, dum2, dum3, dum4
real (R8) vect4Ddouble(dim1,dim2,dim3,dim4)
real (R8) twant, tret
character (len=*) :: path, string
end subroutine put_vect4d_double_slice

subroutine put_vect5d_double_slice(idx,path,string,vect5Ddouble,dim1,dim2,dim3,dim4,dim5,  &
     dum1,dum2,dum3,dum4,dum5,twant,tret,interpol)
use itm_types
implicit none
integer interpol
integer idx, ndims, dim1, dim2, dim3, dim4, dim5, dum1, dum2, dum3, dum4, dum5
real (R8) vect5Ddouble(dim1,dim2,dim3,dim4,dim5)
real (R8) twant, tret
character (len=*) :: path, string
end subroutine put_vect5d_double_slice

subroutine put_vect6d_double_slice(idx,path,string,vect6Ddouble,dim1,dim2,dim3,dim4,dim5,dim6,  &
     dum1,dum2,dum3,dum4,dum5,dum6,twant,tret,interpol)
use itm_types
implicit none
integer interpol
integer idx, ndims, dim1, dim2, dim3, dim4, dim5, dim6, dum1, dum2, dum3, dum4, dum5, dum6
real (R8) vect6Ddouble(dim1,dim2,dim3,dim4,dim5,dim6)
real (R8) twant, tret
character (len=*) :: path, string
end subroutine put_vect6d_double_slice

subroutine put_vect1d_int_slice(idx,path,string,vect1Dint,dim1,dum1,twant,tret,interpol)
use itm_types
implicit none
integer interpol
integer idx, ndims, dim1, dum1, vect1Dint(dim1)
real (R8) twant, tret
character (len=*) :: path, string
end subroutine put_vect1d_int_slice

subroutine put_vect2d_int_slice(idx,path,string,vect2Dint,dim1,dim2,dum1,dum2,twant,tret,interpol)
use itm_types
implicit none
integer interpol
integer idx, ndims, dim1, dim2, dum1, dum2, vect2Dint(dim1,dim2)
real (R8) twant, tret
character (len=*) :: path, string
end subroutine put_vect2d_int_slice

subroutine put_vect3d_int_slice(idx,path,string,vect3Dint,dim1,dim2,dim3,dum1,dum2,dum3,twant,tret,interpol)
use itm_types
implicit none
integer interpol
integer idx, ndims, dim1, dim2, dim3, dum1, dum2, dum3, vect3Dint(dim1,dim2,dim3)
real (R8) twant, tret
character (len=*) :: path, string
end subroutine put_vect3d_int_slice

subroutine put_vect4d_int_slice(idx,path,string,vect4Dint,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4,twant,tret,interpol)
use itm_types
implicit none
integer interpol
integer idx, ndims, dim1, dim2, dim3, dim4, dum1, dum2, dum3, dum4, vect4Dint(dim1,dim2,dim3,dim4)
real (R8) twant, tret
character (len=*) :: path, string
end subroutine put_vect4d_int_slice

subroutine put_vect1d_string_slice(idx,path,string,stringpointer,dim1,dum1,twant,tret,interpol)
use itm_types
implicit none
integer interpol
integer idx, ndims, dim1, dum1
real (R8) twant, tret
character (len=*) :: path, string
character(len=132), dimension(:), pointer :: stringpointer
end subroutine put_vect1d_string_slice

subroutine delete_data(idx,path,string)
use itm_types
implicit none
integer idx
character (len=*) :: path, string
end subroutine delete_data

subroutine euitm_discard_cache(idx,cpopath,path)
use itm_types
implicit none
integer idx
character (len=*) :: cpopath, path
end subroutine euitm_discard_cache

subroutine euitm_flush_cache(idx,cpopath,path)
use itm_types
implicit none
integer idx
character (len=*) :: cpopath, path
end subroutine euitm_flush_cache

subroutine put_object(idx,cpopath,path,time,handle)
use itm_types
implicit none
integer idx, handle
real (R8) time
character (len=*) :: cpopath, path
end subroutine put_object

subroutine put_object_slice(idx,cpopath,path,time,handle)
use itm_types
implicit none
integer idx, handle
real (R8) time
character (len=*) :: cpopath, path
end subroutine put_object_slice

subroutine begin_object(expidx, handle)
use itm_types
implicit none
integer expidx, handle
end subroutine begin_object

subroutine put_string_in_object(expidx, handle, path, idx, data)
use itm_types
implicit none
integer expidx, idx, handle
character (len=*) :: path, data
end subroutine put_string_in_object

subroutine put_int_in_object(expidx, handle, path, idx, data)
use itm_types
implicit none
integer expidx, idx, handle, data
character (len=*) :: path
end subroutine put_int_in_object

subroutine put_float_in_object(expidx, handle, path, idx, data)
use itm_types
implicit none
integer expidx, idx, handle
real (R4) data
character (len=*) :: path
end subroutine put_float_in_object

subroutine put_double_in_object(expidx, handle, path, idx, data)
use itm_types
implicit none
integer expidx, idx, handle
real (R8) data
character (len=*) :: path
end subroutine put_double_in_object

subroutine put_vect1d_int_in_object(expidx, handle, path, idx, data, dim)
use itm_types
implicit none
integer expidx, idx, handle, data(:), dim
character (len=*) :: path
end subroutine put_vect1d_int_in_object

subroutine put_vect1d_float_in_object(expidx, handle, path, idx, data, dim)
use itm_types
implicit none
integer expidx, idx, handle, dim
real (R4) data(:)
character (len=*) :: path
end subroutine put_vect1d_float_in_object

subroutine put_vect1d_double_in_object(expidx, handle, path, idx, data, dim)
use itm_types
implicit none
integer expidx, idx, handle, dim
real (R8) data(:)
character (len=*) :: path
end subroutine put_vect1d_double_in_object

subroutine put_vect1d_string_in_object(expidx, handle, path, idx, data, dim, dimtab)
use itm_types
implicit none
integer expidx, idx, handle, dim, dimtab
character (len=*) :: path, data(:)
end subroutine put_vect1d_string_in_object

subroutine put_vect2d_int_in_object(expidx, handle, path, idx, data, dim1, dim2)
use itm_types
implicit none
integer expidx, idx, handle, data(:,:), dim1, dim2
character (len=*) :: path
end subroutine put_vect2d_int_in_object

subroutine put_vect2d_float_in_object(expidx, handle, path, idx, data, dim1, dim2)
use itm_types
implicit none
integer expidx, idx, handle, dim1, dim2
real (R4) data(:,:)
character (len=*) :: path
end subroutine put_vect2d_float_in_object

subroutine put_vect2d_double_in_object(expidx, handle, path, idx, data, dim1, dim2)
use itm_types
implicit none
integer expidx, idx, handle, dim1, dim2
real (R8) data(:,:)
character (len=*) :: path
end subroutine put_vect2d_double_in_object

subroutine put_vect3d_int_in_object(expidx, handle, path, idx, data, dim1, dim2, dim3)
use itm_types
implicit none
integer expidx, idx, handle, data(:,:,:), dim1, dim2, dim3
character (len=*) :: path
end subroutine put_vect3d_int_in_object

subroutine put_vect3d_float_in_object(expidx, handle, path, idx, data, dim1, dim2, dim3)
use itm_types
implicit none
integer expidx, idx, handle, dim1, dim2, dim3
real (R4) data(:,:,:)
character (len=*) :: path
end subroutine put_vect3d_float_in_object

subroutine put_vect3d_double_in_object(expidx, handle, path, idx, data, dim1, dim2, dim3)
use itm_types
implicit none
integer expidx, idx, handle, dim1, dim2, dim3
real (R8) data(:,:,:)
character (len=*) :: path
end subroutine put_vect3d_double_in_object

subroutine put_vect4d_double_in_object(expidx, handle, path, idx, data, dim1, dim2, dim3, dim4)
use itm_types
implicit none
integer expidx, idx, handle, dim1, dim2, dim3, dim4
real (R8) data(:,:,:,:)
character (len=*) :: path
end subroutine put_vect4d_double_in_object

subroutine put_vect5d_double_in_object(expidx, handle, path, idx, data, dim1, dim2, dim3, dim4, dim5, isTimed)
use itm_types
implicit none
integer expidx, idx, handle, dim1, dim2, dim3, dim4, dim5, isTimed
real (R8) data(:,:,:,:,:)
character (len=*) :: path
end subroutine put_vect5d_double_in_object

subroutine put_vect6d_double_in_object(expidx, handle, path, idx, data, dim1, dim2, dim3, dim4, dim5, dim6, isTimed)
use itm_types
implicit none
integer expidx, idx, handle, dim1, dim2, dim3, dim4, dim5, dim6, isTimed
real (R8) data(:,:,:,:,:,:)
character (len=*) :: path
end subroutine put_vect6d_double_in_object

subroutine put_vect7d_double_in_object(expidx, handle, path, idx, data, dim1, dim2, dim3, dim4, dim5, dim6, dim7, isTimed)
use itm_types
implicit none
integer expidx, idx, handle, dim1, dim2, dim3, dim4, dim5, dim6, dim7, isTimed
real (R8) data(:,:,:,:,:,:,:)
character (len=*) :: path
end subroutine put_vect7d_double_in_object

subroutine put_object_in_object(expidx, handle, path, idx, dataHandle)
use itm_types
implicit none
integer expidx, idx, handle, dataHandle
character (len=*) :: path
end subroutine put_object_in_object





subroutine get_object_dim(expIdx, handle, dim)
use itm_types
implicit none
integer expidx, handle, dim
end subroutine

subroutine get_object(expIdx, cpoPath, path, handle, isTimed, stat)
use itm_types
implicit none
integer expidx, handle
character (len=*) :: cpopath, path, isTimed, stat
end subroutine

subroutine get_dimension_from_object(expIdx, handle, path, idx, numDims, dim1, dim2, dim3, dim4, dim5, dim6, dim7)
use itm_types
implicit none
integer expidx, handle, idx, numDims, dim1, dim2, dim3, dim4, dim5, dim6, dim7
character (len=*) :: path
end subroutine

subroutine get_string_from_object(expIdx, handle, path, idx, data, stat)
use itm_types
implicit none
integer expidx, handle, idx, stat
character (len=*) :: path, data
end subroutine

subroutine get_int_from_object(expIdx, handle, path, idx, data, stat)
use itm_types
implicit none
integer expidx, handle, idx, stat, data
character (len=*) :: path
end subroutine

subroutine get_double_from_object(expIdx, handle, path, idx, data, stat)
use itm_types
implicit none
integer expidx, handle, idx, stat
real (R8) data
character (len=*) :: path
end subroutine

subroutine get_vect1d_int_from_object(expIdx, handle, path, idx, data, dim, retDim, stat)
use itm_types
implicit none
integer expidx, handle, idx, stat, data(:), dim, retDim
character (len=*) :: path
end subroutine

subroutine get_vect1d_float_from_object(expIdx, handle, path, idx, data, dim, retDim, stat)
use itm_types
implicit none
integer expidx, handle, idx, stat, dim, retDim
real (R4) data(:)
character (len=*) :: path
end subroutine

subroutine get_vect1d_double_from_object(expIdx, handle, path, idx, data, dim, retDim, stat)
use itm_types
implicit none
integer expidx, handle, idx, stat, dim, retDim
real (R8) data(:)
character (len=*) :: path
end subroutine

subroutine get_vect1d_string_from_object(expIdx, handle, path, idx, data, dim, retDim, stat)
use itm_types
implicit none
integer expidx, handle, idx, stat, dim, retDim
character (len=*) :: path, data(:)
end subroutine

subroutine get_vect2d_int_from_object(expIdx, handle, path, idx, data, dim1, dim2, retDim1, retDim2, stat)
use itm_types
implicit none
integer expidx, handle, idx, stat, data(:,:), dim1, dim2, retDim1, retDim2
character (len=*) :: path
end subroutine

subroutine get_vect2d_float_from_object(expIdx, handle, path, idx, data, dim1, dim2, retDim1, retDim2, stat)
use itm_types
implicit none
integer expidx, handle, idx, stat, dim1, dim2, retDim1, retDim2
real (R4) data(:,:)
character (len=*) :: path
end subroutine

subroutine get_vect2d_double_from_object(expIdx, handle, path, idx, data, dim1, dim2, retDim1, retDim2, stat)
use itm_types
implicit none
integer expidx, handle, idx, stat, dim1, dim2, retDim1, retDim2
real (R8) data(:,:)
character (len=*) :: path
end subroutine

subroutine get_vect3d_int_from_object(expIdx, handle, path, idx, data, dim1, dim2, dim3, retDim1, retDim2, retDim3, stat)
use itm_types
implicit none
integer expidx, handle, idx, stat, data(:,:,:), dim1, dim2, dim3, retDim1, retDim2, retDim3
character (len=*) :: path
end subroutine

subroutine get_vect3d_float_from_object(expIdx, handle, path, idx, data, dim1, dim2, dim3, retDim1, retDim2, retDim3, stat)
use itm_types
implicit none
integer expidx, handle, idx, stat, dim1, dim2, dim3, retDim1, retDim2, retDim3
real (R4) data(:,:,:)
character (len=*) :: path
end subroutine

subroutine get_vect3d_double_from_object(expIdx, handle, path, idx, data, dim1, dim2, dim3, retDim1, retDim2, retDim3, stat)
use itm_types
implicit none
integer expidx, handle, idx, stat, dim1, dim2, dim3, retDim1, retDim2, retDim3
real (R8) data(:,:,:)
character (len=*) :: path
end subroutine

subroutine get_vect4d_double_from_object(expIdx, handle, path, idx, data, dim1, dim2, dim3, dim4, retDim1, retDim2, retDim3, retDim4, stat)
use itm_types
implicit none
integer expidx, handle, idx, stat, dim1, dim2, dim3, dim4, retDim1, retDim2, retDim3, retDim4
real (R8) data(:,:,:,:)
character (len=*) :: path
end subroutine

subroutine get_vect5d_double_from_object(expIdx, handle, path, idx, data, dim1, dim2, dim3, dim4, dim5, retDim1, retDim2, retDim3, retDim4, retDim5, stat)
use itm_types
implicit none
integer expidx, handle, idx, stat, dim1, dim2, dim3, dim4, dim5, retDim1, retDim2, retDim3, retDim4, retDim5
real (R8) data(:,:,:,:,:)
character (len=*) :: path
end subroutine

subroutine get_vect6d_double_from_object(expIdx, handle, path, idx, data, dim1, dim2, dim3, dim4, dim5, dim6, retDim1, retDim2, retDim3, retDim4, retDim5, retDim6, stat)
use itm_types
implicit none
integer expidx, handle, idx, stat, dim1, dim2, dim3, dim4, dim5, dim6, retDim1, retDim2, retDim3, retDim4, retDim5, retDim6
real (R8) data(:,:,:,:,:,:)
character (len=*) :: path
end subroutine

subroutine get_vect7d_double_from_object(expIdx, handle, path, idx, data, dim1, dim2, dim3, dim4, dim5, dim6, dim7, retDim1, retDim2, retDim3, retDim4, retDim5, retDim6, retDim7, stat)
use itm_types
implicit none
integer expidx, handle, idx, stat, dim1, dim2, dim3, dim4, dim5, dim6, dim7, retDim1, retDim2, retDim3, retDim4, retDim5, retDim6, retDim7
real (R8) data(:,:,:,:,:,:,:)
character (len=*) :: path
end subroutine

subroutine get_object_from_object(expIdx, handle, path, idx, dataHandle, stat)
use itm_types
implicit none
integer expidx, handle, idx, dataHandle, stat
character (len=*) :: path
end subroutine

subroutine get_object_slice(expIdx, cpoPath, path, time, handle, stat)
use itm_types
implicit none
integer expidx, handle, idx, stat
real (R8) time
character (len=*) :: cpopath, path
end subroutine

subroutine release_object(expIdx, handle)
use itm_types
implicit none
integer expidx, handle
end subroutine
