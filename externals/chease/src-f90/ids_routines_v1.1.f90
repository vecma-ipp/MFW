
module ids_routines

use ids_schemas
 
use Actuator_ids_module
use Core_Profiles_ids_module
use Core_Sources_ids_module
use Core_Transport_ids_module
use Equilibrium_ids_module
use Magnetics_ids_module
use PF_ids_module
use SDN_ids_module
use TF_ids_module

contains


subroutine ids_get_times(idx,path,time)
implicit none
integer, parameter :: DP=kind(1.0D0)

integer :: idx, status
character*(*) :: path
real(DP), pointer :: time(:)

integer :: ndims,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4, dim5, dim6, dim7, lentime

call get_dimension(idx,path,"time",ndims,dim1,dim2,dim3,dim4,dim5,dim6,dim7)
lentime = dim1

call begin_IDS_get(idx, path,1,dum1)

allocate(time(lentime))
call get_vect1d_double(idx,path,"time",time,lentime,dum1,status)

end subroutine
end module
