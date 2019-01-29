
module ids_routines

use ids_schemas
use utilities_copy_struct


use actuator_ids_module
use actuator_copy_struct

use antennas_ids_module
use antennas_copy_struct

use atomic_data_ids_module
use atomic_data_copy_struct

use charge_exchange_ids_module
use charge_exchange_copy_struct

use controllers_ids_module
use controllers_copy_struct

use core_instant_changes_ids_module
use core_instant_changes_copy_struct

use core_profiles_ids_module
use core_profiles_copy_struct

use core_sources_ids_module
use core_sources_copy_struct

use core_transport_ids_module
use core_transport_copy_struct

use dataset_description_ids_module
use dataset_description_copy_struct

use edge_profiles_ids_module
use edge_profiles_copy_struct

use em_coupling_ids_module
use em_coupling_copy_struct

use equilibrium_ids_module
use equilibrium_copy_struct

use interfero_polarimeter_ids_module
use interfero_polarimeter_copy_struct

use magnetics_ids_module
use magnetics_copy_struct

use mhd_linear_ids_module
use mhd_linear_copy_struct

use nbi_ids_module
use nbi_copy_struct

use ntms_ids_module
use ntms_copy_struct

use pellets_ids_module
use pellets_copy_struct

use pf_active_ids_module
use pf_active_copy_struct

use pf_passive_ids_module
use pf_passive_copy_struct

use sawteeth_ids_module
use sawteeth_copy_struct

use schedule_ids_module
use schedule_copy_struct

use sdn_ids_module
use sdn_copy_struct

use temporary_ids_module
use temporary_copy_struct

use thomson_scattering_ids_module
use thomson_scattering_copy_struct

use tf_ids_module
use tf_copy_struct

use transport_solver_numerics_ids_module
use transport_solver_numerics_copy_struct

use wall_ids_module
use wall_copy_struct



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
