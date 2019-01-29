
module ids_routines

use ids_schemas
 
use actuator_ids_module
use atomic_data_ids_module
use controllers_ids_module
use core_profiles_ids_module
use core_sources_ids_module
use core_transport_ids_module
use em_coupling_ids_module
use equilibrium_ids_module
use magnetics_ids_module
use pf_active_ids_module
use pf_passive_ids_module
use schedule_ids_module
use sdn_ids_module
use simulation_ids_module
use temporary_ids_module
use tf_ids_module

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
