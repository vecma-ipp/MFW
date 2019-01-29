! IDS FORTRAN 90 type definitions
! Contains the type definition of all IDSs


module ids_utilities    ! declare the set of types common to all sub-trees

integer, parameter, public :: DP = kind(1.0d0)

type ids_plasma_composition_charge_states  !    Array of charge states considered   
  real(DP)  :: z_min=-9.0D40       ! /z_min - Minimum Z of the charge state bundle   
  real(DP)  :: z_max=-9.0D40       ! /z_max - Maximum Z of the charge state bundle
  character(len=132), dimension(:), pointer ::label => null()       ! /label - String identifying charge state (e.g. C+, C+2 , C+3, C+4, C+5, C+6, ...)
endtype

type ids_plasma_composition_ions  !    Array of plasma ions.   
  real(DP)  :: a=-9.0D40       ! /a - Mass of atom   
  real(DP)  :: z_ion=-9.0D40       ! /z_ion - Ion charge (of the dominant ionisation state; lumped ions are allowed)   
  real(DP)  :: z_n=-9.0D40       ! /z_n - Nuclear charge
  character(len=132), dimension(:), pointer ::label => null()       ! /label - String identifying ion (e.g. H+, D+, T+, He+2, C+, ...)
  integer  :: multiple_charge_state_flag=-999999999       ! /multiple_charge_state_flag - Multiple charge state calculation flag : 0-Only one charge state is considered; 1-Multiple charge st
  type (ids_plasma_composition_charge_states),pointer :: charge_states(:) => null()  ! /charge_states(i) - Array of charge states considered for this ion (non-empty if Multiple_Charge_State_Flag = 1)
endtype

type ids_plasma_composition_neutrals_components  !    Components to the atom or molecule   
  real(DP)  :: a=-9.0D40       ! /a - Mass of atom   
  real(DP)  :: z_n=-9.0D40       ! /z_n - Nuclear charge   
  real(DP)  :: multiplicity=-9.0D40       ! /multiplicity - Multiplicity of the atom
endtype

type ids_plasma_composition_neutrals  !    Array of neutral atoms or molecules
  type (ids_plasma_composition_neutrals_components),pointer :: components(:) => null()  ! /components(i) - Array of components to the atom or molecule
  integer,pointer  :: energies(:) => null()      ! /energies - Types energies considered for this neutral species : 0=cold, 1=thermal, 2= fast, 3=NBI
  character(len=132), dimension(:), pointer ::label => null()       ! /label - String identifying the atom or molecule (e.g. D2, DT, CD4, ...)
endtype

! SPECIAL STRUCTURE data / time
type ids_schedule_waveform_value  !    Values of the reference for the timebase
  real(DP), pointer  :: data(:) => null()     ! /value - Values of the reference for the timebase
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_schedule_waveform  !    Description of a named waveform, normally for scheduling
  character(len=132), dimension(:), pointer ::name => null()       ! /name - Reference name
  type (ids_schedule_waveform_value) :: value  ! /value - Values of the reference for the timebase
endtype

type ids_errorbar  !    Structure for 1D dynamic errorbar
  character(len=132), dimension(:), pointer ::type => null()       ! /type - Type of errorbar description which is used. Refers to the name of the sibling fields.
  real(DP),pointer  :: dynamic(:) => null()     ! /dynamic - Absolute error provided as a dynamic of same dimension as the related field.   
  real(DP)  :: constant=-9.0D40       ! /constant - Relative error in % provided as a constant
  character(len=132), dimension(:), pointer ::expression => null()       ! /expression - Absolute error provided as an Expression.
endtype

type ids_dynamic_type  !    Structure for 1D dynamic (prototype to be evaluated)
  real(DP), pointer  :: time(:) => null()  ! time
  real(DP),pointer  :: value(:) => null()     ! /value - Value of the dynamic. Value/Data is ugly. Moreover the metadata are not at the right place. 
endtype

type ids_b_tor_vacuum_1  !    Characteristics of the vacuum toroidal field. time assumed to be one level above   
  real(DP)  :: r0=-9.0D40       ! /r0 - Reference major radius where the vacuum toroidal magnetic field is given (usually a fixed position s
  real(DP),pointer  :: b0(:) => null()     ! /b0 - Vacuum toroidal field at R0 [T]; When using cocos = 11 (expected for ITER data), positive sign means
endtype

type ids_b_tor_vacuum_2  !    Characteristics of the vacuum toroidal field. time assumed to be two levels above   
  real(DP)  :: r0=-9.0D40       ! /r0 - Reference major radius where the vacuum toroidal magnetic field is given (usually a fixed position s
  real(DP),pointer  :: b0(:) => null()     ! /b0 - Vacuum toroidal field at b0. When using cocos = 11 (expected for ITER data), positive sign means ant
endtype

type ids_core_radial_grid  !    1D radial grid for core* IDSs
  real(DP),pointer  :: rho_tor_norm(:) => null()     ! /rho_tor_norm - Normalised toroidal flux coordinate. The normalizing value for rho_tor_norm, is the toroidal flux co
  real(DP),pointer  :: rho_tor(:) => null()     ! /rho_tor - Toroidal flux coordinate. The toroidal field used in its definition is indicated under vacuum_toroid
  real(DP),pointer  :: psi(:) => null()     ! /psi - Poloidal magnetic flux
  real(DP),pointer  :: volume(:) => null()     ! /volume - Volume enclosed inside the magnetic surface
  real(DP),pointer  :: area(:) => null()     ! /area - Cross-sectional area of the flux surface
endtype

type ids_rzphi0d_static  !    Structure for R, Z, Phi positions (0D, static)   
  real(DP)  :: r=-9.0D40       ! /r - Major radius   
  real(DP)  :: z=-9.0D40       ! /z - Height   
  real(DP)  :: phi=-9.0D40       ! /phi - Toroidal angle
endtype

type ids_rzphi1d_static  !    Structure for list of R, Z, Phi positions (1D, static)
  real(DP),pointer  :: r(:) => null()     ! /r - Major radius
  real(DP),pointer  :: z(:) => null()     ! /z - Height
  real(DP),pointer  :: phi(:) => null()     ! /phi - Toroidal angle
endtype

type ids_rz1d_constant  !    Structure for list of R, Z positions (1D, constant)
  real(DP),pointer  :: r(:) => null()     ! /r - Major radius
  real(DP),pointer  :: z(:) => null()     ! /z - Height
endtype

type ids_rz0d_dynamic_aos  !    Structure for scalar R, Z positions, dynamic within a type 3 array of structure (index on time)   
  real(DP)  :: r=-9.0D40       ! /r - Major radius   
  real(DP)  :: z=-9.0D40       ! /z - Height
endtype

type ids_rz1d_dynamic_aos  !    Structure for list of R, Z positions (1D list of Npoints, dynamic within a type 3 array of structure (index on time))
  real(DP),pointer  :: r(:) => null()     ! /r - Major radius
  real(DP),pointer  :: z(:) => null()     ! /z - Height
endtype

type ids_rz1d_dynamic  !    Structure for list of R, Z positions (1D, dynamic), time assumed to be 2 levels above
  real(DP),pointer  :: r(:) => null()     ! /r - Major radius
  real(DP),pointer  :: z(:) => null()     ! /z - Height
endtype

type ids_rz1d_static  !    Structure for list of R, Z positions (1D, constant)
  real(DP),pointer  :: r(:) => null()     ! /r - Major radius
  real(DP),pointer  :: z(:) => null()     ! /z - Height
endtype

type ids_rz0d_static  !    Structure for a single R, Z position (0D, constant)   
  real(DP)  :: r=-9.0D40       ! /r - Major radius   
  real(DP)  :: z=-9.0D40       ! /z - Height
endtype

type ids_identifier  !    Standard type for identifiers. The three fields: name, index and description are all representations of the same information. Asso
  character(len=132), dimension(:), pointer ::name => null()       ! /name - Short string identifier
  integer  :: index=-999999999       ! /index - Integer identifier (enumeration index within a list)
  character(len=132), dimension(:), pointer ::description => null()       ! /description - Verbose description
endtype

type ids_oblique_static  !    Oblique description of a 2D object   
  real(DP)  :: r=-9.0D40       ! /r - Geometric centre R   
  real(DP)  :: z=-9.0D40       ! /z - Geometric centre Z   
  real(DP)  :: length=-9.0D40       ! /length - Length   
  real(DP)  :: thickness=-9.0D40       ! /thickness - Thickness   
  real(DP)  :: alpha=-9.0D40       ! /alpha - Inclination of first angle TBD   
  real(DP)  :: beta=-9.0D40       ! /beta - Inclination of second angle TBD
endtype

type ids_rectangle_static  !    Rectangular description of a 2D object   
  real(DP)  :: r=-9.0D40       ! /r - Geometric centre R   
  real(DP)  :: z=-9.0D40       ! /z - Geometric centre Z   
  real(DP)  :: width=-9.0D40       ! /width - Horizontal full width   
  real(DP)  :: height=-9.0D40       ! /height - Vertical full height
endtype

type ids_outline_2d_geometry_static  !    Description of 2D geometry
  integer  :: geometry_type=-999999999       ! /geometry_type - Type used to describe the element shape (0 for 'Coordinates_RZ' or 1 for 'RZDRDZ') 
  type (ids_rz1d_static) :: outline  ! /outline - Irregular outline of the element
  type (ids_rectangle_static) :: rectangle  ! /rectangle - Rectangular description of the element
  type (ids_oblique_static) :: oblique  ! /oblique - Rectangular description of the element
endtype

type ids_plasma_composition  !    Generic declaration of Plasma Composition for a simulation
  type (ids_plasma_composition_ions),pointer :: ions(:) => null()  ! /plasma_composition/ions(i) - Array of plasma ions
  type (ids_plasma_composition_neutrals),pointer :: neutrals(:) => null()  ! /plasma_composition/neutrals(i) - Array of neutrals
endtype

type ids_code  !    Generic decription of the code specific parameters for the code that has produced this IDS
  character(len=132), dimension(:), pointer ::name => null()       ! /code/name - Name of the code
  character(len=132), dimension(:), pointer ::version => null()       ! /code/version - Version of the code
  character(len=132), dimension(:), pointer ::parameters => null()       ! /code/parameters - List of the code specific parameters in XML format
  integer,pointer  :: output_flag(:) => null()      ! /code/output_flag - Output flag : 0 means the run is successful, other values mean some difficulty has been encountered,
endtype

type ids_parameters_input  !    Code parameters block passed from the wrapper to the subroutine. Does not appear as such in the data structure. This is inserted i
  character(len=132), dimension(:), pointer ::parameters_value => null()       ! /parameters_input/parameters_value - Actual value of the code parameters (instance of Code_Parameters/Parameters in XML format)
  character(len=132), dimension(:), pointer ::parameters_default => null()       ! /parameters_input/parameters_default - Default value of the code parameters (instance of Code_Parameters/Parameters in XML format)
  character(len=132), dimension(:), pointer ::schema => null()       ! /parameters_input/schema - Code parameters schema
endtype

type ids_ids_properties  !    Interface Data Structure properties. This element identifies the node above as an IDS
  character(len=132), dimension(:), pointer ::comment => null()       ! /ids_properties/comment - Any comment describing the content of this IDS
  integer  :: homogeneous_time=-999999999       ! /ids_properties/homogeneous_time - 1 if the time of this IDS is homogeneous. In this case, the time values for this IDS are stored in .
  integer  :: cocos=-999999999       ! /ids_properties/cocos - Coordinate conventions of this IDS, expected to be 11 for ITER data
endtype


end module ! end of the utilities module

module ids_schemas       ! declaration of all IDSs

use ids_utilities

integer, parameter :: NON_TIMED=0
integer, parameter :: TIMED=1
integer, parameter :: TIMED_CLEAR=2


! ***********  Include actuator/dd_actuator.xsd
type ids_actuator  !    Generic simple description of a heating/current drive actuator, for a first simplified version of the Plasma Simulator component
  type (ids_ids_properties) :: ids_properties  ! /actuator/ids_properties - 
  character(len=132), dimension(:), pointer ::name => null()       ! /actuator/name - Name of the actuator (IC, EC, NBI, LH)
  character(len=132), dimension(:), pointer ::channels => null()       ! /actuator/channels - ID of the multiple channels of the actuator: Beam boxes for NBI, EC or IC launcher, ...
  real(DP),pointer  :: power(:,:) => null()     ! /actuator/power - Power delivered at the output of the actuator, in the vessel (NB this is before the coupling / beam 
  real(DP),pointer  :: generic_dynamic(:,:) => null()     ! /actuator/generic_dynamic - Generic 2D dynamic slot for storing an actuator control parameter (e.g. the angle of an ECRH mirror)
  type (ids_code) :: code  ! /actuator/code - 
  real(DP), pointer  :: time(:) => null()  ! time
endtype

! ***********  Include atomic_data/dd_atomic_data.xsd
type ids_atomic_data_process_charge_state  !    Process tables for a given charge state. Only one table is used for that process, defined by process(:)/table_dimension
  character(len=132), dimension(:), pointer ::label => null()       ! /label - String identifying charge state (e.g. C+, C+2 , C+3, C+4, C+5, C+6, ...)   
  real(DP)  :: z_min=-9.0D40       ! /z_min - Minimum Z of the charge state bundle   
  real(DP)  :: z_max=-9.0D40       ! /z_max - Maximum Z of the charge state bundle (equal to z_min if no bundle)   
  real(DP)  :: table_0d=-9.0D40       ! /table_0d - 0D table describing the process data
  real(DP),pointer  :: table_1d(:) => null()     ! /table_1d - 1D table describing the process data
  real(DP),pointer  :: table_2d(:,:) => null()     ! /table_2d - 2D table describing the process data
  real(DP),pointer  :: table_3d(:,:,:) => null()     ! /table_3d - 3D table describing the process data
  real(DP),pointer  :: table_4d(:,:,:,:) => null()     ! /table_4d - 4D table describing the process data
  real(DP),pointer  :: table_5d(:,:,:,:,:) => null()     ! /table_5d - 5D table describing the process data
  real(DP),pointer  :: table_6d(:,:,:,:,:,:) => null()     ! /table_6d - 6D table describing the process data
endtype

type ids_atomic_data_process  !    Definition of a process and its data
  character(len=132), dimension(:), pointer ::label => null()       ! /label - String identifying the process (e.g. EI, RC, ...)
  integer  :: table_dimension=-999999999       ! /table_dimension - Table dimensionality of the process (1 to 6), valid for all charge states. Indicates which of the ta
  integer  :: coordinate_index=-999999999       ! /coordinate_index - Index in tables_coord, specifying what coordinate systems to use for this process (valid for all tab
  character(len=132), dimension(:), pointer ::result_label => null()       ! /result_label - Description of the process result (rate, cross section, sputtering yield, ...)
  character(len=132), dimension(:), pointer ::result_units => null()       ! /result_units - Units of the process result
  integer  :: result_transformation=-999999999       ! /result_transformation - Transformation of the process result. Integer flag: 0=no transformation; 1=10^; 2=exp()
  type (ids_atomic_data_process_charge_state),pointer :: charge_state(:) => null()  ! /charge_state(i) - Process tables for a set of charge states. Only one table is used for that process, defined by proce
endtype

type ids_atomic_data_coordinate_system_coordinate  !    Description of a coordinate for atomic data tables. Can be either a range of real values or a set of discrete values (if interp_ty
  character(len=132), dimension(:), pointer ::label => null()       ! /label - Description of coordinate (e.g. "Electron temperature")
  real(DP),pointer  :: values(:) => null()     ! /values - Coordinate values
  integer  :: interpolation_type=-999999999       ! /interpolation_type - Interpolation strategy in this coordinate direction. Integer flag: 0=discrete (no interpolation); 1=
  integer,pointer  :: extrapolation_type(:) => null()      ! /extrapolation_type - Extrapolation strategy when leaving the domain. The first value of the vector describes the behaviou
  character(len=132), dimension(:), pointer ::value_labels => null()       ! /value_labels - String description of discrete coordinate values (if interpolation_type=0). E.g., for spectroscopic 
  character(len=132), dimension(:), pointer ::units => null()       ! /units - Units of coordinate (e.g. eV)
  integer  :: transformation=-999999999       ! /transformation - Coordinate transformation applied to coordinate values stored in coord. Integer flag: 0=none; 1=log1
  integer  :: spacing=-999999999       ! /spacing - Flag for specific coordinate spacing (for optimization purposes). Integer flag: 0=undefined; 1=unifo
endtype

type ids_atomic_data_coordinate_system  !    Description of a coordinate system for atomic data tables 
  type (ids_atomic_data_coordinate_system_coordinate),pointer :: coordinate(:) => null()  ! /coordinate(i) - Set of coordinates for that coordinate system. A coordinate an be either a range of real values or a
endtype

type ids_atomic_data  !    Atomic, molecular, nuclear and surface physics data. Each occurrence contains the atomic data for a given element (nuclear charge)
  type (ids_ids_properties) :: ids_properties  ! /atomic_data/ids_properties -    
  real(DP)  :: z_n=-9.0D40       ! /atomic_data/z_n - Nuclear charge   
  real(DP)  :: a=-9.0D40       ! /atomic_data/a - Mass of atom
  type (ids_atomic_data_process),pointer :: process(:) => null()  ! /atomic_data/process(i) - Description and data for a set of physical processes.
  type (ids_atomic_data_coordinate_system),pointer :: coordinate_system(:) => null()  ! /atomic_data/coordinate_system(i) - Array of possible coordinate systems for process tables
  type (ids_code) :: code  ! /atomic_data/code - 
  real(DP), pointer  :: time(:) => null()  ! time
endtype

! ***********  Include controllers/dd_controllers.xsd
! SPECIAL STRUCTURE data / time
type ids_controllers_statespace_a  !    A matrix
  real(DP), pointer  :: data(:,:,:) => null()     ! /a - A matrix
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_controllers_statespace_b  !    B matrix
  real(DP), pointer  :: data(:,:,:) => null()     ! /b - B matrix
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_controllers_statespace_c  !    C matrix
  real(DP), pointer  :: data(:,:,:) => null()     ! /c - C matrix
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_controllers_statespace_d  !    D matrix, normally proper and D=0
  real(DP), pointer  :: data(:,:,:) => null()     ! /d - D matrix, normally proper and D=0
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_controllers_statespace_deltat  !    Discrete time sampling interval ; if less than 1e-10, the controller is considered to be expressed in continuous time
  real(DP), pointer  :: data(:) => null()     ! /deltat - Discrete time sampling interval ; if less than 1e-10, the controller is considered to be expressed i
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_controllers_statespace  !    type for a statespace controller
  character(len=132), dimension(:), pointer ::state_names => null()       ! /state_names - Names of the states
  type (ids_controllers_statespace_a) :: a  ! /a - A matrix
  type (ids_controllers_statespace_b) :: b  ! /b - B matrix
  type (ids_controllers_statespace_c) :: c  ! /c - C matrix
  type (ids_controllers_statespace_d) :: d  ! /d - D matrix, normally proper and D=0
  type (ids_controllers_statespace_deltat) :: deltat  ! /deltat - Discrete time sampling interval ; if less than 1e-10, the controller is considered to be expressed i
endtype

! SPECIAL STRUCTURE data / time
type ids_controllers_pid_p  !    Proportional term
  real(DP), pointer  :: data(:,:,:) => null()     ! /p - Proportional term
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_controllers_pid_i  !    Integral term
  real(DP), pointer  :: data(:,:,:) => null()     ! /i - Integral term
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_controllers_pid_d  !    Derivative term
  real(DP), pointer  :: data(:,:,:) => null()     ! /d - Derivative term
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_controllers_pid_tau  !    Filter time-constant for the D-term
  real(DP), pointer  :: data(:) => null()     ! /tau - Filter time-constant for the D-term
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_controllers_pid  !    type for a MIMO PID controller
  type (ids_controllers_pid_p) :: p  ! /p - Proportional term
  type (ids_controllers_pid_i) :: i  ! /i - Integral term
  type (ids_controllers_pid_d) :: d  ! /d - Derivative term
  type (ids_controllers_pid_tau) :: tau  ! /tau - Filter time-constant for the D-term
endtype

! SPECIAL STRUCTURE data / time
type ids_controllers_linear_controller_inputs  !    Input signals; the timebase is common to inputs and outputs for any particular controller
  real(DP), pointer  :: data(:,:) => null()     ! /inputs - Input signals; the timebase is common to inputs and outputs for any particular controller
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_controllers_linear_controller_outputs  !    Output signals; the timebase is common to inputs and outputs for any particular controller
  real(DP), pointer  :: data(:,:) => null()     ! /outputs - Output signals; the timebase is common to inputs and outputs for any particular controller
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_controllers_linear_controller  !    type for a linear controller
  character(len=132), dimension(:), pointer ::name => null()       ! /name - Name of this controller
  character(len=132), dimension(:), pointer ::description => null()       ! /description - Description of this controller
  character(len=132), dimension(:), pointer ::controller_class => null()       ! /controller_class - One of a known class of controllers
  character(len=132), dimension(:), pointer ::input_names => null()       ! /input_names - Names of the input signals, following the SDN convention
  character(len=132), dimension(:), pointer ::output_names => null()       ! /output_names - Names of the output signals following the SDN convention
  type (ids_controllers_statespace) :: statespace  ! /statespace - Statespace controller in discrete or continuous time
  type (ids_controllers_pid) :: pid  ! /pid - Filtered PID controller
  type (ids_controllers_linear_controller_inputs) :: inputs  ! /inputs - Input signals; the timebase is common to inputs and outputs for any particular controller
  type (ids_controllers_linear_controller_outputs) :: outputs  ! /outputs - Output signals; the timebase is common to inputs and outputs for any particular controller
endtype

! SPECIAL STRUCTURE data / time
type ids_controllers_nonlinear_controller_inputs  !    Input signals; the timebase is common  to inputs and outputs for any particular controller
  real(DP), pointer  :: data(:,:) => null()     ! /inputs - Input signals; the timebase is common  to inputs and outputs for any particular controller
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_controllers_nonlinear_controller_outputs  !    Output signals; the timebase is common  to inputs and outputs for any particular controller
  real(DP), pointer  :: data(:,:) => null()     ! /outputs - Output signals; the timebase is common  to inputs and outputs for any particular controller
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_controllers_nonlinear_controller  !    Type for a nonlinear controller
  character(len=132), dimension(:), pointer ::name => null()       ! /name - Name of this controller
  character(len=132), dimension(:), pointer ::description => null()       ! /description - Description of this controller
  character(len=132), dimension(:), pointer ::controller_class => null()       ! /controller_class - One of a known class of controllers
  character(len=132), dimension(:), pointer ::input_names => null()       ! /input_names - Names of the input signals, following the SDN convention
  character(len=132), dimension(:), pointer ::output_names => null()       ! /output_names - Output signal names following the SDN convention
  character(len=132), dimension(:), pointer ::function => null()       ! /function - Method to be defined
  type (ids_controllers_nonlinear_controller_inputs) :: inputs  ! /inputs - Input signals; the timebase is common  to inputs and outputs for any particular controller
  type (ids_controllers_nonlinear_controller_outputs) :: outputs  ! /outputs - Output signals; the timebase is common  to inputs and outputs for any particular controller
endtype

type ids_controllers  !    Feedback and feedforward controllers
  type (ids_ids_properties) :: ids_properties  ! /controllers/ids_properties - 
  type (ids_controllers_linear_controller),pointer :: linear_controller(:) => null()  ! /controllers/linear_controller(i) - A linear controller, this is rather conventional
  type (ids_controllers_nonlinear_controller),pointer :: nonlinear_controller(:) => null()  ! /controllers/nonlinear_controller(i) - A non-linear controller, this is less conventional and will have to be developed
  real(DP), pointer  :: time(:) => null()  ! time
  type (ids_code) :: code  ! /controllers/code - 
endtype

! ***********  Include core_profiles/dd_core_profiles.xsd
type ids_core_profiles_ions_charge_states2  !    Quantities related to the a given charge state of the ion species   
  real(DP)  :: z_min=-9.0D40       ! /z_min - Minimum Z of the charge state bundle   
  real(DP)  :: z_max=-9.0D40       ! /z_max - Maximum Z of the charge state bundle (equal to z_min if no bundle)
  character(len=132), dimension(:), pointer ::label => null()       ! /label - String identifying charge state (e.g. C+, C+2 , C+3, C+4, C+5, C+6, ...)
  real(DP),pointer  :: n_z(:) => null()     ! /n_z - Density of the charge state considered (thermal+non-thermal)
  real(DP),pointer  :: n_z_fast(:) => null()     ! /n_z_fast - Density of fast (non-thermal) ions, for the charge state considered
  real(DP),pointer  :: t_z(:) => null()     ! /t_z - Temperature of the charge state considered
  real(DP),pointer  :: p_z(:) => null()     ! /p_z - Pressure of the charge state considered
  real(DP),pointer  :: p_z_fast_perpendicular(:) => null()     ! /p_z_fast_perpendicular - Fast perpendicular pressure of the charge state considered
  real(DP),pointer  :: p_z_fast_parallel(:) => null()     ! /p_z_fast_parallel - Fast parallel pressure of the charge state considered
  real(DP),pointer  :: v_tor_z(:) => null()     ! /v_tor_z - Toroidal velocity of the charge state considered
  real(DP),pointer  :: v_pol_z(:) => null()     ! /v_pol_z - Poloidal velocity of the charge state considered
endtype

type ids_core_profile_ions  !    Quantities related to a given ion species   
  real(DP)  :: a=-9.0D40       ! /a - Mass of atom   
  real(DP)  :: z_ion=-9.0D40       ! /z_ion - Ion charge (of the dominant ionisation state; lumped ions are allowed)   
  real(DP)  :: z_n=-9.0D40       ! /z_n - Nuclear charge
  character(len=132), dimension(:), pointer ::label => null()       ! /label - String identifying ion (e.g. H+, D+, T+, He+2, C+, ...)
  real(DP),pointer  :: n_i(:) => null()     ! /n_i - Ion density (thermal+non-thermal)
  real(DP),pointer  :: n_i_fast(:) => null()     ! /n_i_fast - Density of fast (non-thermal) ions, for the ion species considered
  real(DP),pointer  :: t_i(:) => null()     ! /t_i - Ion temperature of that ion species
  real(DP),pointer  :: p_i(:) => null()     ! /p_i - Ion pressure for that ion species
  real(DP),pointer  :: p_i_fast_perpendicular(:) => null()     ! /p_i_fast_perpendicular - Fast ion perpendicular pressure for that ion species
  real(DP),pointer  :: p_i_fast_parallel(:) => null()     ! /p_i_fast_parallel - Fast ion parallel pressure for that ion species
  real(DP),pointer  :: v_tor_i(:) => null()     ! /v_tor_i - Toroidal velocity of that ion species
  real(DP),pointer  :: v_pol_i(:) => null()     ! /v_pol_i - Poloidal velocity of that ion species
  integer  :: multiple_charge_state_flag=-999999999       ! /multiple_charge_state_flag - Multiple charge state calculation flag : 0-Only one charge state is considered; 1-Multiple charge st
  type (ids_core_profiles_ions_charge_states2),pointer :: charge_states(:) => null()  ! /charge_states(i) - Quantities related to the different charge states of the ion species
endtype

type ids_core_profiles_global_quantities  !    Various global quantities calculated from the fields solved in the transport equations and from the Derived Profiles
  real(DP),pointer  :: ip(:) => null()     ! /ip - Total plasma current
  real(DP),pointer  :: current_non_inductive(:) => null()     ! /current_non_inductive - Total non-inductive parallel current
  real(DP),pointer  :: current_bootstrap(:) => null()     ! /current_bootstrap - Bootstrap current
  real(DP),pointer  :: v_loop(:) => null()     ! /v_loop - LCFS loop voltage
  real(DP),pointer  :: li(:) => null()     ! /li - Internal inductance. The li_3 definition is used, i.e. li_3 = 2/R0/mu0^2/Ip^2 * int(Bp^2 dV).
  real(DP),pointer  :: beta_tor(:) => null()     ! /beta_tor - Toroidal beta, defined as the volume-averaged total perpendicular pressure divided by (B0^2/(2*mu0))
  real(DP),pointer  :: beta_tor_norm(:) => null()     ! /beta_tor_norm - Normalised toroidal beta, defined as 100 * beta_tor * a[m] * B0 [T] / ip [MA] 
  real(DP),pointer  :: beta_pol(:) => null()     ! /beta_pol - Poloidal beta. Defined as betap = 4 int(p dV) / [R_0 * mu_0 * Ip^2]
  real(DP),pointer  :: energy_diamagnetic(:) => null()     ! /energy_diamagnetic - Plasma energy content = 3/2 * integral over the plasma volume of the total perpendicular pressure 
endtype

type ids_core_profiles_profiles_1d  !    
  type (ids_core_radial_grid) :: grid  ! /grid - Radial grid
  real(DP),pointer  :: t_e(:) => null()     ! /t_e - Electron temperature
  real(DP),pointer  :: t_i_average(:) => null()     ! /t_i_average - Ion temperature (averaged on charge states and ion species)
  real(DP),pointer  :: n_e(:) => null()     ! /n_e - Electron density (thermal+non-thermal)
  real(DP),pointer  :: n_e_fast(:) => null()     ! /n_e_fast - Density of fast (non-thermal) electrons
  type (ids_core_profile_ions),pointer :: ions(:) => null()  ! /ions(i) - Quantities related to the different ion species
  real(DP),pointer  :: n_i_total_over_n_e(:) => null()     ! /n_i_total_over_n_e - Ratio of total ion density (sum over species and charge states) over electron density. (thermal+non-
  real(DP),pointer  :: momentum_tor(:) => null()     ! /momentum_tor - Total plasma toroidal momentum, summed over ion species and electrons 
  real(DP),pointer  :: zeff(:) => null()     ! /zeff - Effective charge
  real(DP),pointer  :: p_e(:) => null()     ! /p_e - Electron pressure
  real(DP),pointer  :: p_e_fast_perpendicular(:) => null()     ! /p_e_fast_perpendicular - Fast (non-thermal) electron perpendicular pressure
  real(DP),pointer  :: p_e_fast_parallel(:) => null()     ! /p_e_fast_parallel - Fast (non-thermal) electron parallel pressure
  real(DP),pointer  :: p_i_total(:) => null()     ! /p_i_total - Total ion pressure (sum over the ion species)
  real(DP),pointer  :: p_i_total_fast_perpendicular(:) => null()     ! /p_i_total_fast_perpendicular - Fast (non-thermal) total ion (sum over the ion species) perpendicular pressure
  real(DP),pointer  :: p_i_total_fast_parallel(:) => null()     ! /p_i_total_fast_parallel - Fast (non-thermal) total ion (sum over the ion species) parallel pressure
  real(DP),pointer  :: pressure_thermal(:) => null()     ! /pressure_thermal - Thermal pressure (electrons+ions)
  real(DP),pointer  :: pressure_perpendicular(:) => null()     ! /pressure_perpendicular - Total perpendicular pressure (electrons+ions, thermal+non-thermal)
  real(DP),pointer  :: pressure_parallel(:) => null()     ! /pressure_parallel - Total parallel pressure (electrons+ions, thermal+non-thermal)
  real(DP),pointer  :: j_total(:) => null()     ! /j_total - Total parallel current density = average(jtot.B) / B0, where B0 = Core_Profiles/Vacuum_Toroidal_Fiel
  real(DP),pointer  :: j_tor(:) => null()     ! /j_tor - Total toroidal current density = average(J_Tor/R) / average(1/R)
  real(DP),pointer  :: j_ohmic(:) => null()     ! /j_ohmic - Ohmic parallel current density = average(J_Ohmic.B) / B0, where B0 = Core_Profiles/Vacuum_Toroidal_F
  real(DP),pointer  :: j_non_inductive(:) => null()     ! /j_non_inductive - Non-inductive (includes bootstrap) parallel current density = average(jni.B) / B0, where B0 = Core_P
  real(DP),pointer  :: j_bootstrap(:) => null()     ! /j_bootstrap - Bootstrap current density = average(J_Bootstrap.B) / B0, where B0 = Core_Profiles/Vacuum_Toroidal_Fi
  real(DP),pointer  :: conductivity_parallel(:) => null()     ! /conductivity_parallel - Parallel conductivity
  real(DP),pointer  :: e_field_parallel(:) => null()     ! /e_field_parallel - Parallel electric field = average(E.B) / B0, where Core_Profiles/Vacuum_Toroidal_Field/ B0
  real(DP),pointer  :: q(:) => null()     ! /q - Safety factor
  real(DP),pointer  :: magnetic_shear(:) => null()     ! /magnetic_shear - Magnetic shear, defined as rho_tor/q . dq/drho_tor   
  real(DP)  :: time=-9.0D40       ! /time - Time
endtype

type ids_core_profiles  !    Core plasma radial profiles
  type (ids_ids_properties) :: ids_properties  ! /core_profiles/ids_properties - 
  type (ids_core_profiles_profiles_1d),pointer :: profiles_1d(:) => null()  ! /core_profiles/profiles_1d(i) - Core plasma radial profiles for various time slices
  type (ids_core_profiles_global_quantities) :: global_quantities  ! /core_profiles/global_quantities - Various global quantities derived from the profiles
  type (ids_b_tor_vacuum_1) :: vacuum_toroidal_field  ! /core_profiles/vacuum_toroidal_field - Characteristics of the vacuum toroidal field (used in rho_tor definition and in the normalization of
  type (ids_code) :: code  ! /core_profiles/code - 
  real(DP), pointer  :: time(:) => null()  ! time
endtype

! ***********  Include core_sources/dd_core_sources.xsd
type ids_core_sources_actuator_source_profiles_ions_charge_states  !    Source terms related to the a given charge state of the ion species   
  real(DP)  :: z_min=-9.0D40       ! /z_min - Minimum Z of the charge state bundle   
  real(DP)  :: z_max=-9.0D40       ! /z_max - Maximum Z of the charge state bundle
  character(len=132), dimension(:), pointer ::label => null()       ! /label - String identifying charge state (e.g. C+, C+2 , C+3, C+4, C+5, C+6, ...)
  real(DP),pointer  :: n_z_source(:) => null()     ! /n_z_source - Source term for the charge state density transport equation
  real(DP),pointer  :: t_z_source(:) => null()     ! /t_z_source - Source terms for the charge state energy transport equation
endtype

type ids_core_sources_actuator_source_profiles_ions  !    Source terms related to a given ion species   
  real(DP)  :: a=-9.0D40       ! /a - Mass of atom   
  real(DP)  :: z_ion=-9.0D40       ! /z_ion - Ion charge (of the dominant ionisation state; lumped ions are allowed)   
  real(DP)  :: z_n=-9.0D40       ! /z_n - Nuclear charge
  character(len=132), dimension(:), pointer ::label => null()       ! /label - String identifying ion (e.g. H+, D+, T+, He+2, C+, ...)
  real(DP),pointer  :: n_i_source(:) => null()     ! /n_i_source - Source term for ion density equation
  real(DP),pointer  :: t_i_source(:) => null()     ! /t_i_source - Source term for the ion energy transport equation.
  integer  :: multiple_charge_state_flag=-999999999       ! /multiple_charge_state_flag - Multiple charge state calculation flag : 0-Only one charge state is considered; 1-Multiple charge st
  type (ids_core_sources_actuator_source_profiles_ions_charge_states),pointer :: charge_states(:) => null()  ! /charge_states(i) - Source terms related to the different charge states of the ion species
endtype

type ids_core_sources_actuator_source_profiles  !    Source terms for a given time slice
  type (ids_core_radial_grid) :: grid  ! /grid - Radial grid
  real(DP),pointer  :: n_e_source(:) => null()     ! /n_e_source - Source term for electron density equation
  real(DP),pointer  :: t_e_source(:) => null()     ! /t_e_source - Source term for electron energy equation
  real(DP),pointer  :: t_i_average_source(:) => null()     ! /t_i_average_source - Source term for the total ion energy equation
  real(DP),pointer  :: momentum_tor_source(:) => null()     ! /momentum_tor_source - Source term for total toroidal momentum equation
  real(DP),pointer  :: conductivity_parallel(:) => null()     ! /conductivity_parallel - Parallel conductivity induced by this actuator
  type (ids_core_sources_actuator_source_profiles_ions),pointer :: ions(:) => null()  ! /ions(i) - Source terms related to the different ions species   
  real(DP)  :: time=-9.0D40       ! /time - Time
endtype

type ids_core_sources_actuator  !    Source terms for a given actuator
  character(len=132), dimension(:), pointer ::name => null()       ! /name - Name of the source term
  type (ids_core_sources_actuator_source_profiles),pointer :: profiles(:) => null()  ! /profiles(i) - Source profiles for various time slices
endtype

type ids_core_sources  !    Core plasma source terms (for the transport equations)
  type (ids_ids_properties) :: ids_properties  ! /core_sources/ids_properties - 
  type (ids_b_tor_vacuum_1) :: vacuum_toroidal_field  ! /core_sources/vacuum_toroidal_field - Characteristics of the vacuum toroidal field (used in Rho_Tor definition and in the normalization of
  type (ids_core_sources_actuator),pointer :: source(:) => null()  ! /core_sources/source(i) - Set of source terms
  type (ids_code) :: code  ! /core_sources/code - 
  real(DP), pointer  :: time(:) => null()  ! time
endtype

! ***********  Include core_transport/dd_core_transport.xsd
type ids_core_transport_model_1_density  !    Transport coefficients for density equations. Coordinates one level above.
  real(DP),pointer  :: d(:) => null()     ! /d - Effective diffusivity
  real(DP),pointer  :: v(:) => null()     ! /v - Effective convection
  real(DP),pointer  :: flux(:) => null()     ! /flux - Flux
endtype

type ids_core_transport_model_1_energy  !    Transport coefficients for energy equations. Coordinates one level above.
  real(DP),pointer  :: d(:) => null()     ! /d - Effective diffusivity
  real(DP),pointer  :: v(:) => null()     ! /v - Effective convection
  real(DP),pointer  :: flux(:) => null()     ! /flux - Flux
endtype

type ids_core_transport_model_1_momentum  !    Transport coefficients for momentum equations. Coordinates one level above.
  real(DP),pointer  :: d(:) => null()     ! /d - Effective diffusivity
  real(DP),pointer  :: v(:) => null()     ! /v - Effective convection
  real(DP),pointer  :: flux(:) => null()     ! /flux - Flux
endtype

type ids_core_transport_model_2_density  !    Transport coefficients for density equations. Coordinates two levels above.
  real(DP),pointer  :: d(:) => null()     ! /d - Effective diffusivity
  real(DP),pointer  :: v(:) => null()     ! /v - Effective convection
  real(DP),pointer  :: flux(:) => null()     ! /flux - Flux
endtype

type ids_core_transport_model_2_energy  !    Transport coefficients for energy equations. Coordinates two levels above.
  real(DP),pointer  :: d(:) => null()     ! /d - Effective diffusivity
  real(DP),pointer  :: v(:) => null()     ! /v - Effective convection
  real(DP),pointer  :: flux(:) => null()     ! /flux - Flux
endtype

type ids_core_transport_model_3_density  !    Transport coefficients for density equations. Coordinates three levels above.
  real(DP),pointer  :: d(:) => null()     ! /d - Effective diffusivity
  real(DP),pointer  :: v(:) => null()     ! /v - Effective convection
  real(DP),pointer  :: flux(:) => null()     ! /flux - Flux
endtype

type ids_core_transport_model_3_energy  !    Transport coefficients for energy equations. Coordinates three levels above.
  real(DP),pointer  :: d(:) => null()     ! /d - Effective diffusivity
  real(DP),pointer  :: v(:) => null()     ! /v - Effective convection
  real(DP),pointer  :: flux(:) => null()     ! /flux - Flux
endtype

type ids_core_transport_model_ions_charge_states  !    Transport coefficients related to the a given charge state of the ion species   
  real(DP)  :: z_min=-9.0D40       ! /z_min - Minimum Z of the charge state bundle   
  real(DP)  :: z_max=-9.0D40       ! /z_max - Maximum Z of the charge state bundle
  character(len=132), dimension(:), pointer ::label => null()       ! /label - String identifying charge state (e.g. C+, C+2 , C+3, C+4, C+5, C+6, ...)
  type (ids_core_transport_model_3_density) :: n_z_transport  ! /n_z_transport - Transport coefficients of for the density of the charge state considered (thermal+non-thermal)
  type (ids_core_transport_model_3_energy) :: t_z_transport  ! /t_z_transport - Transport coefficients of for the energy of the charge state considered 
endtype

type ids_core_transport_model_ions  !    Transport coefficients related to a given ion species   
  real(DP)  :: a=-9.0D40       ! /a - Mass of atom   
  real(DP)  :: z_ion=-9.0D40       ! /z_ion - Ion charge (of the dominant ionisation state; lumped ions are allowed)   
  real(DP)  :: z_n=-9.0D40       ! /z_n - Nuclear charge
  character(len=132), dimension(:), pointer ::label => null()       ! /label - String identifying ion (e.g. H+, D+, T+, He+2, C+, ...)
  type (ids_core_transport_model_2_density) :: n_i_transport  ! /n_i_transport - Transport coefficients for ion density equation
  type (ids_core_transport_model_2_energy) :: t_i_transport  ! /t_i_transport - Transport coefficients for ion energy equation
  integer  :: multiple_charge_state_flag=-999999999       ! /multiple_charge_state_flag - Multiple charge state calculation flag : 0-Only one charge state is considered; 1-Multiple charge st
  type (ids_core_transport_model_ions_charge_states),pointer :: charge_states(:) => null()  ! /charge_states(i) - Transport coefficients related to the different charge states of the ion species
endtype

type ids_core_transport_model_profiles  !    Transport coefficient profiles at a given time slice
  type (ids_core_radial_grid) :: grid_d  ! /grid_d - Grid for effective diffusivities and parallel conductivity
  type (ids_core_radial_grid) :: grid_v  ! /grid_v - Grid for effective convections
  type (ids_core_radial_grid) :: grid_flux  ! /grid_flux - Grid for fluxes
  real(DP),pointer  :: conductivity_parallel(:) => null()     ! /conductivity_parallel - Parallel conductivity
  type (ids_core_transport_model_1_density) :: n_e_transport  ! /n_e_transport - Transport coefficients for electron density equation
  type (ids_core_transport_model_1_energy) :: t_e_transport  ! /t_e_transport - Transport coefficients for electron energy equation
  type (ids_core_transport_model_1_energy) :: t_i_average_transport  ! /t_i_average_transport - Transport coefficients for averaged (over ion species) ion energy equation
  type (ids_core_transport_model_1_momentum) :: momentum_tor_transport  ! /momentum_tor_transport - Transport coefficients for total toroidal momentum equation
  type (ids_core_transport_model_ions),pointer :: ions(:) => null()  ! /ions(i) - Transport coefficients related to the various ions species   
  real(DP)  :: time=-9.0D40       ! /time - Time
endtype

type ids_core_transport_model  !    Transport coefficients for a given model
  character(len=132), dimension(:), pointer ::name => null()       ! /name - Name of the transport model   
  real(DP)  :: flux_multiplier=-9.0D40       ! /flux_multiplier - Multiplier applied to the particule flux when adding its contribution in the expression of the heat 
  type (ids_core_transport_model_profiles),pointer :: profiles(:) => null()  ! /profiles(i) - Transport coefficient profiles for various time slices
endtype

type ids_core_transport  !    Core plasma transport
  type (ids_ids_properties) :: ids_properties  ! /core_transport/ids_properties - 
  type (ids_b_tor_vacuum_1) :: vacuum_toroidal_field  ! /core_transport/vacuum_toroidal_field - Characteristics of the vacuum toroidal field (used in Rho_Tor definition and in the normalization of
  type (ids_core_transport_model),pointer :: model(:) => null()  ! /core_transport/model(i) - Transport is described by a combination of various transport models
  type (ids_code) :: code  ! /core_transport/code - 
  real(DP), pointer  :: time(:) => null()  ! time
endtype

! ***********  Include em_coupling/dd_em_coupling.xsd
type ids_em_coupling  !    Description of the axisymmetric mutual electromagnetics; does not include non-axisymmetric coil systems; the convention is Quantit
  type (ids_ids_properties) :: ids_properties  ! /em_coupling/ids_properties - 
  real(DP),pointer  :: mutual_active_active(:,:) => null()     ! /em_coupling/mutual_active_active - Mutual inductance coupling from active coils to active coils
  real(DP),pointer  :: mutual_passive_active(:,:) => null()     ! /em_coupling/mutual_passive_active - Mutual inductance coupling from active coils to passive loops
  real(DP),pointer  :: mutual_loops_active(:,:) => null()     ! /em_coupling/mutual_loops_active - Mutual inductance coupling from active coils to poloidal flux loops
  real(DP),pointer  :: field_probes_active(:,:) => null()     ! /em_coupling/field_probes_active - Poloidal field coupling from active coils to poloidal field probes
  real(DP),pointer  :: mutual_passive_passive(:,:) => null()     ! /em_coupling/mutual_passive_passive - Mutual inductance coupling from passive loops to passive loops
  real(DP),pointer  :: mutual_loops_passive(:,:) => null()     ! /em_coupling/mutual_loops_passive - Mutual  inductance coupling from passive  loops to poloidal flux loops
  real(DP),pointer  :: field_probes_passive(:,:) => null()     ! /em_coupling/field_probes_passive - Poloidal field coupling from passive loops to poloidal field probes
  real(DP),pointer  :: mutual_grid_grid(:,:) => null()     ! /em_coupling/mutual_grid_grid - Mutual inductance from equilibrium grid to itself
  real(DP),pointer  :: mutual_grid_active(:,:) => null()     ! /em_coupling/mutual_grid_active - Mutual inductance coupling from active coils to equilibrium grid  
  real(DP),pointer  :: mutual_grid_passive(:,:) => null()     ! /em_coupling/mutual_grid_passive - Mutual inductance coupling from passive loops to equilibrium grid
  real(DP),pointer  :: field_probes_grid(:,:) => null()     ! /em_coupling/field_probes_grid - Poloidal field coupling from equilibrium grid to poloidal field probes
  real(DP),pointer  :: mutual_loops_grid(:,:) => null()     ! /em_coupling/mutual_loops_grid - Mutual inductance from equilibrium grid to poloidal flux loops 
  character(len=132), dimension(:), pointer ::active_coils => null()       ! /em_coupling/active_coils - List of the names of the active PF+CS coils
  character(len=132), dimension(:), pointer ::passive_loops => null()       ! /em_coupling/passive_loops - List of the names of the passive loops
  character(len=132), dimension(:), pointer ::poloidal_probes => null()       ! /em_coupling/poloidal_probes - List of the names of poloidal field probes
  character(len=132), dimension(:), pointer ::flux_loops => null()       ! /em_coupling/flux_loops - List of the names of the axisymmetric flux loops
  character(len=132), dimension(:), pointer ::grid_points => null()       ! /em_coupling/grid_points - List of the names of the plasma region grid points
  type (ids_code) :: code  ! /em_coupling/code - 
  real(DP), pointer  :: time(:) => null()  ! time
endtype

! ***********  Include equilibrium/dd_equilibrium.xsd
type ids_equilibrium_boundary  !    Geometry of the plasma boundary
  integer  :: type=-999999999       ! /type - 0 (limiter) or 1 (diverted)
  type (ids_rz1d_dynamic_aos) :: lcfs  ! /lcfs - RZ description of the plasma boundary
  type (ids_rz0d_dynamic_aos) :: geometric_axis  ! /geometric_axis - RZ position of the geometric axis (defined as (Rmin+Rmax) / 2 of the boundary)   
  real(DP)  :: a_minor=-9.0D40       ! /a_minor - Minor radius of the plasma boundary (defined as (Rmax-Rmin) / 2 of the boundary)   
  real(DP)  :: elongation=-9.0D40       ! /elongation - Elongation of the plasma boundary   
  real(DP)  :: elongation_upper=-9.0D40       ! /elongation_upper - Elongation (upper half w.r.t. geometric axis) of the plasma boundary   
  real(DP)  :: elongation_lower=-9.0D40       ! /elongation_lower - Elongation (lower half w.r.t. geometric axis) of the plasma boundary   
  real(DP)  :: triangularity=-9.0D40       ! /triangularity - Triangularity of the plasma boundary   
  real(DP)  :: triangularity_upper=-9.0D40       ! /triangularity_upper - Upper triangularity of the plasma boundary   
  real(DP)  :: triangularity_lower=-9.0D40       ! /triangularity_lower - Lower triangularity of the plasma boundary
  type (ids_rz0d_dynamic_aos),pointer :: x_point(:) => null()  ! /x_point(i) - Array of X-points, for each of them the RZ position is given
  type (ids_rz0d_dynamic_aos),pointer :: strike_point(:) => null()  ! /strike_point(i) - Array of strike points, for each of them the RZ position is given
  type (ids_rz0d_dynamic_aos) :: active_limiter_point  ! /active_limiter_point - RZ position of the active limiter point (point of the plasma boundary in contact with the limiter)
endtype

type ids_equilibrium_global_quantities_qmin  !    Position and value of q_min   
  real(DP)  :: value=-9.0D40       ! /value - Minimum q value   
  real(DP)  :: rho_tor_norm=-9.0D40       ! /rho_tor_norm - Minimum q position in normalised toroidal flux coordinate
endtype

type ids_equlibrium_global_quantities  !    0D parameters of the equilibrium   
  real(DP)  :: beta_pol=-9.0D40       ! /beta_pol - Poloidal beta. Defined as betap = 4 int(p dV) / [R_0 * mu_0 * Ip^2]   
  real(DP)  :: beta_tor=-9.0D40       ! /beta_tor - Toroidal beta, defined as the volume-averaged total perpendicular pressure divided by (B0^2/(2*mu0))   
  real(DP)  :: beta_normal=-9.0D40       ! /beta_normal - Normalised toroidal beta, defined as 100 * beta_tor * a[m] * B0 [T] / ip [MA]    
  real(DP)  :: ip=-9.0D40       ! /ip - Plasma current. Positive sign means anti-clockwise when viewed from above.   
  real(DP)  :: li_3=-9.0D40       ! /li_3 - Internal inductance   
  real(DP)  :: volume=-9.0D40       ! /volume - Total plasma volume   
  real(DP)  :: area=-9.0D40       ! /area - Area of the LCFS poloidal cross section   
  real(DP)  :: surface=-9.0D40       ! /surface - Surface area of the toroidal flux surface   
  real(DP)  :: length_pol=-9.0D40       ! /length_pol - Poloidal length of the magnetic surface   
  real(DP)  :: psi_axis=-9.0D40       ! /psi_axis - Poloidal flux at the magnetic axis   
  real(DP)  :: psi_boundary=-9.0D40       ! /psi_boundary - Poloidal flux at the selected plasma boundary 
  type (ids_rz0d_dynamic_aos) :: magnetic_axis  ! /magnetic_axis - Magnetic axis position   
  real(DP)  :: b_tor_axis=-9.0D40       ! /b_tor_axis - Total toroidal magnetic field at the magnetic axis   
  real(DP)  :: q_axis=-9.0D40       ! /q_axis - q at the magnetic axis   
  real(DP)  :: q_95=-9.0D40       ! /q_95 - q at the 95% poloidal flux surface
  type (ids_equilibrium_global_quantities_qmin) :: q_min  ! /q_min - Minimum q value and position   
  real(DP)  :: w_mhd=-9.0D40       ! /w_mhd - Plasma energy content = 3/2 * int(p,dV) with p being the total pressure (thermal + fast particles) [
endtype

type ids_equilibrium_profiles_1d  !    Equilibrium profiles (1D radial grid) as a function of the poloidal flux
  real(DP),pointer  :: psi(:) => null()     ! /psi - Poloidal flux
  real(DP),pointer  :: phi(:) => null()     ! /phi - Toroidal flux
  real(DP),pointer  :: pressure(:) => null()     ! /pressure - Pressure
  real(DP),pointer  :: f(:) => null()     ! /f - Diamagnetic function (F=R B_Phi)
  real(DP),pointer  :: dpressure_dpsi(:) => null()     ! /dpressure_dpsi - Derivative of pressure w.r.t. psi
  real(DP),pointer  :: f_df_dpsi(:) => null()     ! /f_df_dpsi - Derivative of F w.r.t. Psi, multiplied with F
  real(DP),pointer  :: j_tor(:) => null()     ! /j_tor - Flux surface averaged toroidal current density = average(j_tor/R) / average(1/R)
  real(DP),pointer  :: j_parallel(:) => null()     ! /j_parallel - Flux surface averaged parallel current density = average(j.B) / B0, where B0 = Equilibrium/Global/To
  real(DP),pointer  :: q(:) => null()     ! /q - Safety factor
  real(DP),pointer  :: magnetic_shear(:) => null()     ! /magnetic_shear - Magnetic shear, defined as rho_tor/q . dq/drho_tor
  real(DP),pointer  :: r_inboard(:) => null()     ! /r_inboard - Radial coordinate (major radius) on the inboard side of the magnetic axis
  real(DP),pointer  :: r_outboard(:) => null()     ! /r_outboard - Radial coordinate (major radius) on the outboard side of the magnetic axis
  real(DP),pointer  :: rho_tor(:) => null()     ! /rho_tor - Toroidal flux coordinate. The toroidal field used in its definition is indicated under vacuum_toroid
  real(DP),pointer  :: rho_tor_norm(:) => null()     ! /rho_tor_norm - Normalised toroidal flux coordinate. The normalizing value for rho_tor_norm, is the toroidal flux co
  real(DP),pointer  :: dpsi_drho_tor(:) => null()     ! /dpsi_drho_tor - Derivative of Psi with respect to Rho_Tor
  real(DP),pointer  :: elongation(:) => null()     ! /elongation - Elongation
  real(DP),pointer  :: triangularity_upper(:) => null()     ! /triangularity_upper - Upper triangularity w.r.t. magnetic axis
  real(DP),pointer  :: triangularity_lower(:) => null()     ! /triangularity_lower - Lower triangularity w.r.t. magnetic axis
  real(DP),pointer  :: volume(:) => null()     ! /volume - Volume enclosed in the flux surface
  real(DP),pointer  :: dvolume_dpsi(:) => null()     ! /dvolume_dpsi - Radial derivative of the volume enclosed in the flux surface with respect to Psi
  real(DP),pointer  :: dvolume_drho_tor(:) => null()     ! /dvolume_drho_tor - Radial derivative of the volume enclosed in the flux surface with respect to Rho_Tor
  real(DP),pointer  :: area(:) => null()     ! /area - Cross-sectional area of the flux surface
  real(DP),pointer  :: darea_dpsi(:) => null()     ! /darea_dpsi - Radial derivative of the cross-sectional area of the flux surface with respect to Psi
  real(DP),pointer  :: surface(:) => null()     ! /surface - Surface area of the toroidal flux surface
  real(DP),pointer  :: trapped_fraction(:) => null()     ! /trapped_fraction - Trapped particle fraction
  real(DP),pointer  :: gm1(:) => null()     ! /gm1 - Flux surface averaged 1/R^2
  real(DP),pointer  :: gm2(:) => null()     ! /gm2 - Flux surface averaged grad_rho^2/R^2
  real(DP),pointer  :: gm3(:) => null()     ! /gm3 - Flux surface averaged grad_rho^2
  real(DP),pointer  :: gm4(:) => null()     ! /gm4 - Flux surface averaged 1/B^2
  real(DP),pointer  :: gm5(:) => null()     ! /gm5 - Flux surface averaged B^2
  real(DP),pointer  :: gm6(:) => null()     ! /gm6 - Flux surface averaged grad_rho^2/B^2
  real(DP),pointer  :: gm7(:) => null()     ! /gm7 - Flux surface averaged grad_rho
  real(DP),pointer  :: gm8(:) => null()     ! /gm8 - Flux surface averaged R
  real(DP),pointer  :: gm9(:) => null()     ! /gm9 - Flux surface averaged 1/R
  real(DP),pointer  :: b_average(:) => null()     ! /b_average - Flux surface averaged B
  real(DP),pointer  :: b_min(:) => null()     ! /b_min - Minimum(B) on the flux surface
  real(DP),pointer  :: b_max(:) => null()     ! /b_max - Maximum(B) on the flux surface
endtype

type ids_equilibrium_profiles_2d_grid  !    Definition of the 2D grid
  real(DP),pointer  :: dim1(:) => null()     ! /dim1 - First dimension values
  real(DP),pointer  :: dim2(:) => null()     ! /dim2 - Second dimension values
endtype

type ids_equilibrium_profiles_2d  !    Equilibrium 2D profiles in the poloidal plane
  type (ids_identifier) :: grid_type  ! /grid_type - Selection of one of a set of grid types. 1-rectangular (R,Z) grid, in this case the position arrays 
  type (ids_equilibrium_profiles_2d_grid) :: grid  ! /grid - Definition of the 2D grid
  real(DP),pointer  :: r(:,:) => null()     ! /r - Values of the major radius on the grid
  real(DP),pointer  :: z(:,:) => null()     ! /z - Values of the Height on the grid
  real(DP),pointer  :: psi(:,:) => null()     ! /psi - Values of the poloidal flux at the grid in the poloidal plane
  real(DP),pointer  :: theta(:,:) => null()     ! /theta - Values of the poloidal angle on the grid
  real(DP),pointer  :: phi(:,:) => null()     ! /phi - Toroidal flux
  real(DP),pointer  :: j_tor(:,:) => null()     ! /j_tor - Toroidal plasma current density
  real(DP),pointer  :: j_parallel(:,:) => null()     ! /j_parallel - Parallel (to magnetic field) plasma current density
  real(DP),pointer  :: b_r(:,:) => null()     ! /b_r - R component of the poloidal magnetic field
  real(DP),pointer  :: b_z(:,:) => null()     ! /b_z - Z component of the poloidal magnetic field
  real(DP),pointer  :: b_tor(:,:) => null()     ! /b_tor - Toroidal component of the magnetic field
endtype

type ids_equilibrium_coordinate_system  !    Flux surface coordinate system on a square grid of flux and poloidal angle
  type (ids_identifier) :: grid_type  ! /grid_type - Type of coordinate system
  type (ids_equilibrium_profiles_2d_grid) :: grid  ! /grid - Definition of the 2D grid
  real(DP),pointer  :: r(:,:) => null()     ! /r - Values of the major radius on the grid
  real(DP),pointer  :: z(:,:) => null()     ! /z - Values of the Height on the grid
  real(DP),pointer  :: jacobian(:,:) => null()     ! /jacobian - Jacobian of the coordinate system
  real(DP),pointer  :: g_11(:,:) => null()     ! /g_11 - metric coefficients g_11; g_ij=g^ij are contravariant metric tensor for the grid described by grid_t
  real(DP),pointer  :: g_12(:,:) => null()     ! /g_12 - metric coefficients g_12; g_ij=g^ij are contravariant metric tensor for the grid described by grid_t
  real(DP),pointer  :: g_13(:,:) => null()     ! /g_13 - metric coefficients g_13; g_ij=g^ij are contravariant metric tensor for the grid described by grid_t
  real(DP),pointer  :: g_22(:,:) => null()     ! /g_22 - metric coefficients g_22; g_ij=g^ij are contravariant metric tensor for the grid described by grid_t
  real(DP),pointer  :: g_23(:,:) => null()     ! /g_23 - metric coefficients g_23; g_ij=g^ij are contravariant metric tensor for the grid described by grid_t
  real(DP),pointer  :: g_33(:,:) => null()     ! /g_33 - metric coefficients g_33; g_ij=g^ij are contravariant metric tensor for the grid described by grid_t
endtype

type ids_equilibrium_time_slice  !    Equilibrium at a given time slice
  type (ids_equilibrium_boundary) :: boundary  ! /boundary - Description of the plasma boundary
  type (ids_equlibrium_global_quantities) :: global_quantities  ! /global_quantities - 0D parameters of the equilibrium
  type (ids_equilibrium_profiles_1d) :: profiles_1d  ! /profiles_1d - Equilibrium profiles (1D radial grid) as a function of the poloidal flux
  type (ids_equilibrium_profiles_2d),pointer :: profiles_2d(:) => null()  ! /profiles_2d(i) - Equilibrium 2D profiles in the poloidal plane. Multiple 2D representations of the equilibrium can be
  type (ids_equilibrium_coordinate_system) :: coordinate_system  ! /coordinate_system - Flux surface coordinate system on a square grid of flux and poloidal angle   
  real(DP)  :: time=-9.0D40       ! /time - Time
endtype

type ids_equilibrium  !    Description of a 2D, axi-symmetric, tokamak equilibrium; result of an equilibrium code.
  type (ids_ids_properties) :: ids_properties  ! /equilibrium/ids_properties - 
  type (ids_b_tor_vacuum_1) :: vacuum_toroidal_field  ! /equilibrium/vacuum_toroidal_field - Characteristics of the vacuum toroidal field (used in rho_tor definition and in the normalization of
  type (ids_equilibrium_time_slice),pointer :: time_slice(:) => null()  ! /equilibrium/time_slice(i) - Set of equilibria at various time slices
  type (ids_code) :: code  ! /equilibrium/code - 
  real(DP), pointer  :: time(:) => null()  ! time
endtype

! ***********  Include magnetics/dd_magnetics.xsd
! SPECIAL STRUCTURE data / time
type ids_magnetics_flux_loop_flux  !    Measured flux
  real(DP), pointer  :: data(:) => null()     ! /flux - Measured flux
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_magnetics_flux_loop  !    Flux loops
  character(len=132), dimension(:), pointer ::name => null()       ! /name - Name of the flux loop
  character(len=132), dimension(:), pointer ::identifier => null()       ! /identifier - ID of the flux loop
  type (ids_rzphi0d_static),pointer :: position(:) => null()  ! /position(i) - List of (R,Z,phi) points defining the position of the loop (see data structure documentation FLUXLOO
  type (ids_magnetics_flux_loop_flux) :: flux  ! /flux - Measured flux
endtype

! SPECIAL STRUCTURE data / time
type ids_magnetics_bpol_probe_field  !    Measured magnetic field
  real(DP), pointer  :: data(:) => null()     ! /field - Measured magnetic field
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_magnetics_bpol_probe  !    Poloidal field probes
  character(len=132), dimension(:), pointer ::name => null()       ! /name - Name of the probe
  character(len=132), dimension(:), pointer ::identifier => null()       ! /identifier - ID of the probe
  type (ids_rzphi0d_static) :: position  ! /position - R, Z, Phi position of the coil centre   
  real(DP)  :: poloidal_angle=-9.0D40       ! /poloidal_angle - Poloidal angle of the coil orientation   
  real(DP)  :: toroidal_angle=-9.0D40       ! /toroidal_angle - Toroidal angle of coil orientation (0 if fully in the poloidal plane)    
  real(DP)  :: area=-9.0D40       ! /area - Area of each turn of the coil   
  real(DP)  :: length=-9.0D40       ! /length - Length of the coil
  integer  :: turns=-999999999       ! /turns - Turns in the coil, including sign
  type (ids_magnetics_bpol_probe_field) :: field  ! /field - Measured magnetic field
endtype

! SPECIAL STRUCTURE data / time
type ids_magnetics_method_ip  !    Plasma current. When using cocos = 11 (expected for ITER data), positive sign means anti-clockwise when viewed from above.
  real(DP), pointer  :: data(:) => null()     ! /ip - Plasma current. When using cocos = 11 (expected for ITER data), positive sign means anti-clockwise w
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_magnetics_method_diamagnetic_flux  !    Diamagnetic flux
  real(DP), pointer  :: data(:) => null()     ! /diamagnetic_flux - Diamagnetic flux
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_magnetics_method  !    Processed quantities derived from the magnetic measurements, using various methods
  character(len=132), dimension(:), pointer ::name => null()       ! /name - Name of the data processing method
  type (ids_magnetics_method_ip) :: ip  ! /ip - Plasma current. When using cocos = 11 (expected for ITER data), positive sign means anti-clockwise w
  type (ids_magnetics_method_diamagnetic_flux) :: diamagnetic_flux  ! /diamagnetic_flux - Diamagnetic flux
endtype

type ids_magnetics  !    Magnetic diagnostics for equilibrium identification and plasma shape control.
  type (ids_ids_properties) :: ids_properties  ! /magnetics/ids_properties - 
  type (ids_magnetics_flux_loop),pointer :: flux_loop(:) => null()  ! /magnetics/flux_loop(i) - Flux loops; partial flux loops can be described
  type (ids_magnetics_bpol_probe),pointer :: bpol_probe(:) => null()  ! /magnetics/bpol_probe(i) - Poloidal field probes
  type (ids_magnetics_method),pointer :: method(:) => null()  ! /magnetics/method(i) - A method generating processed quantities derived from the magnetic measurements
  type (ids_code) :: code  ! /magnetics/code - 
  real(DP), pointer  :: time(:) => null()  ! time
endtype

! ***********  Include pf_active/dd_pf_active.xsd
! SPECIAL STRUCTURE data / time
type ids_pf_supplies_voltage  !    Voltage at the supply output
  real(DP), pointer  :: data(:) => null()     ! /voltage - Voltage at the supply output
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_pf_supplies_current  !    Current at the supply output, defined positive if it flows from point 1 to point 2 of the component in the pfcircuit description
  real(DP), pointer  :: data(:) => null()     ! /current - Current at the supply output, defined positive if it flows from point 1 to point 2 of the component 
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_pf_supplies  !    PF power supplies
  character(len=132), dimension(:), pointer ::name => null()       ! /name - Name of the PF supply
  character(len=132), dimension(:), pointer ::identifier => null()       ! /identifier - Identifier of the supply
  integer  :: type=-999999999       ! /type - Type of the supply; TBD add free description of non-linear power supplies   
  real(DP)  :: resistance=-9.0D40       ! /resistance - Power supply internal resistance   
  real(DP)  :: delay=-9.0D40       ! /delay - Pure delay in the supply
  real(DP),pointer  :: filter_numerator(:) => null()     ! /filter_numerator - Coefficients of the numerator, in increasing order : a0 + a1*s + ... + an*s^n; used for a linear sup
  real(DP),pointer  :: filter_denominator(:) => null()     ! /filter_denominator - Coefficients of the denominator, in increasing order : b0 + b1*s + ... + bm*s^m; used for a linear s   
  real(DP)  :: current_limit_max=-9.0D40       ! /current_limit_max - Maximum current in the supply   
  real(DP)  :: current_limit_min=-9.0D40       ! /current_limit_min - Minimum current in the supply   
  real(DP)  :: voltage_limit_max=-9.0D40       ! /voltage_limit_max - Maximum voltage from the supply   
  real(DP)  :: voltage_limit_min=-9.0D40       ! /voltage_limit_min - Minimum voltage from the supply   
  real(DP)  :: current_limiter_gain=-9.0D40       ! /current_limiter_gain - Gain to prevent overcurrent in a linear model of the supply   
  real(DP)  :: energy_limit_max=-9.0D40       ! /energy_limit_max - Maximum energy to be dissipated in the supply during a pulse
  character(len=132), dimension(:), pointer ::nonlinear_model => null()       ! /nonlinear_model - Description of the nonlinear transfer function of the supply
  type (ids_pf_supplies_voltage) :: voltage  ! /voltage - Voltage at the supply output
  type (ids_pf_supplies_current) :: current  ! /current - Current at the supply output, defined positive if it flows from point 1 to point 2 of the component 
endtype

! SPECIAL STRUCTURE data / time
type ids_pf_circuits_voltage  !    Voltage on the circuit
  real(DP), pointer  :: data(:) => null()     ! /voltage - Voltage on the circuit
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_pf_circuits_current  !    Current in the circuit
  real(DP), pointer  :: data(:) => null()     ! /current - Current in the circuit
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_pf_circuits  !    Circuits, connecting multiple PF coils to multiple supplies, defining the current and voltage relationships in the system
  character(len=132), dimension(:), pointer ::name => null()       ! /name - Name of the circuit
  character(len=132), dimension(:), pointer ::identifier => null()       ! /identifier - ID of the circuit
  character(len=132), dimension(:), pointer ::type => null()       ! /type - Type of the circuit
  integer,pointer  :: connections(:,:) => null()     ! /connections - Description of the supplies and coils connections (nodes) across the circuit. The matrix describing 
  type (ids_pf_circuits_voltage) :: voltage  ! /voltage - Voltage on the circuit
  type (ids_pf_circuits_current) :: current  ! /current - Current in the circuit
endtype

type ids_pf_coils_elements  !    Each PF coil is comprised of a number of cross-section elements described  individually
  character(len=132), dimension(:), pointer ::name => null()       ! /name - Name of this element of this coil
  character(len=132), dimension(:), pointer ::identifier => null()       ! /identifier - Identifier of this element of this coil
  integer  :: turns_with_sign=-999999999       ! /turns_with_sign - Number of effective turns in the element for calculating magnetic fields of the coil; includes the s   
  real(DP)  :: area=-9.0D40       ! /area - Cross-sectional areas of the element
  type (ids_outline_2d_geometry_static) :: geometry  ! /geometry - Cross-sectional shape of the element
endtype

! SPECIAL STRUCTURE data / time
type ids_pf_coils_current  !    Current in the coil
  real(DP), pointer  :: data(:) => null()     ! /current - Current in the coil
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_pf_coils_voltage  !    Voltage on the coil terminals
  real(DP), pointer  :: data(:) => null()     ! /voltage - Voltage on the coil terminals
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_pf_coils  !    Active PF coils
  character(len=132), dimension(:), pointer ::name => null()       ! /name - Name of the coil
  character(len=132), dimension(:), pointer ::identifier => null()       ! /identifier - Alphanumeric identifier of coils used for convenience   
  real(DP)  :: resistance=-9.0D40       ! /resistance - Coil resistance   
  real(DP)  :: energy_limit_max=-9.0D40       ! /energy_limit_max - Maximum Energy to be dissipated in the coil
  type (ids_pf_coils_elements),pointer :: element(:) => null()  ! /element(i) - Each PF coil is comprised of a number of cross-section elements described  individually
  type (ids_pf_coils_current) :: current  ! /current - Current in the coil
  type (ids_pf_coils_voltage) :: voltage  ! /voltage - Voltage on the coil terminals
endtype

! SPECIAL STRUCTURE data / time
type ids_pf_vertical_forces_force  !    Force 
  real(DP), pointer  :: data(:) => null()     ! /force - Force 
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_pf_vertical_forces  !    Vertical forces on the axisymmetric PF+CS coil system
  character(len=132), dimension(:), pointer ::name => null()       ! /name - Name of the force combination
  real(DP),pointer  :: combination(:) => null()     ! /combination - Coils involved in the force combinations. Normally the vertical force would be the full set of coils   
  real(DP)  :: limit_max=-9.0D40       ! /limit_max - Vertical force combination limit   
  real(DP)  :: limit_min=-9.0D40       ! /limit_min - Vertical force combination limit
  type (ids_pf_vertical_forces_force) :: force  ! /force - Force 
endtype

type ids_pf_active  !    Description of the axisymmetric active poloidal field (PF) coils and supplies; includes the limits of these systems; includes the 
  type (ids_ids_properties) :: ids_properties  ! /pf_active/ids_properties - 
  type (ids_pf_coils),pointer :: coil(:) => null()  ! /pf_active/coil(i) - Active PF coils
  type (ids_pf_vertical_forces),pointer :: vertical_force(:) => null()  ! /pf_active/vertical_force(i) - Vertical forces on the axisymmetric PF coil system
  type (ids_pf_circuits),pointer :: circuit(:) => null()  ! /pf_active/circuit(i) - Circuits, connecting multiple PF coils to multiple supplies, defining the current and voltage relati
  type (ids_pf_supplies),pointer :: supply(:) => null()  ! /pf_active/supply(i) - PF power supplies
  type (ids_code) :: code  ! /pf_active/code - 
  real(DP), pointer  :: time(:) => null()  ! time
endtype

! ***********  Include pf_passive/dd_pf_passive.xsd
type ids_pf_passive_loops  !    Passive axisymmetric conductor description in the form of non-connected loops; any connected loops are expressed as active coil ci
  character(len=132), dimension(:), pointer ::name => null()       ! /name - Name of the loop   
  real(DP)  :: area=-9.0D40       ! /area - Surface area of the passive loop   
  real(DP)  :: resistance=-9.0D40       ! /resistance - Passive loop resistance
  type (ids_outline_2d_geometry_static) :: geometry  ! /geometry - Shape of the passive loop
  real(DP),pointer  :: current(:) => null()     ! /current - Passive loop current
endtype

type ids_pf_passive  !    Description of the axisymmetric passive conductors, currents flowing in them
  type (ids_ids_properties) :: ids_properties  ! /pf_passive/ids_properties - 
  type (ids_pf_passive_loops),pointer :: loop(:) => null()  ! /pf_passive/loop(i) - Passive axisymmetric conductor description in the form of non-connected loops; any connected loops a
  type (ids_code) :: code  ! /pf_passive/code - 
  real(DP), pointer  :: time(:) => null()  ! time
endtype

! ***********  Include schedule/dd_schedule.xsd
type ids_schedule  !    Description of Pulse Schedule, to be enhanced as we go; we have chosen initially to have the controllers, schedule and SN defined 
  type (ids_ids_properties) :: ids_properties  ! /schedule/ids_properties - 
  type (ids_schedule_waveform),pointer :: waveform(:) => null()  ! /schedule/waveform(i) - Reference waveform to be used to drive feedback or feedforward controllers, or to control the contro
  type (ids_code) :: code  ! /schedule/code - 
  real(DP), pointer  :: time(:) => null()  ! time
endtype

! ***********  Include sdn/dd_sdn.xsd
type ids_sdn_topic_list  !    List of the groups of signals used at different reading and writing points
  character(len=132), dimension(:), pointer ::names => null()       ! /names - Names of the group of SDN signals
  integer,pointer  :: indices(:) => null()      ! /indices - Indices into the current SDN allocated list; it must be updated when the allocated list is changed
endtype

type ids_sdn_allocatable_signals  !    Dictionary of the signal names and definitions which can be allocated to the SDN
  character(len=132), dimension(:), pointer ::name => null()       ! /name - Name of the allocatable signal
  character(len=132), dimension(:), pointer ::definition => null()       ! /definition - Definition of the allocatable signal
  integer  :: ip_normalise=-999999999       ! /ip_normalise - 0 or 1 if this signal is multiplied by Ip to generate a control variable; this usage is specific to 
  integer  :: allocated_position=-999999999       ! /allocated_position - Allocation of signal to a position in the SDN (1..N); this will be implementation specific
  real(DP),pointer  :: value(:) => null()     ! /value - Signal value
endtype

type ids_sdn  !    Description of the Synchronous Data Network parameters and the signals on it
  type (ids_ids_properties) :: ids_properties  ! /sdn/ids_properties - 
  type (ids_sdn_allocatable_signals),pointer :: signal(:) => null()  ! /sdn/signal(i) - Dictionary of the signal names and definitions which can be allocated to the SDN
  type (ids_sdn_topic_list),pointer :: topic_list(:) => null()  ! /sdn/topic_list(i) - List of the groups of signals used at different reading and writing points; this is an implementatio
  real(DP), pointer  :: time(:) => null()  ! time
endtype

! ***********  Include simulation/dd_simulation.xsd
type ids_simulation  !    Simulation parameters for a general simulation
  type (ids_ids_properties) :: ids_properties  ! /simulation/ids_properties - 
  character(len=132), dimension(:), pointer ::comment_before => null()       ! /simulation/comment_before - Comment made when launching a simulation
  character(len=132), dimension(:), pointer ::comment_after => null()       ! /simulation/comment_after - Comment made at the end of a simulation   
  real(DP)  :: time_begin=-9.0D40       ! /simulation/time_begin - Start time   
  real(DP)  :: time_step=-9.0D40       ! /simulation/time_step - Time interval between steps   
  real(DP)  :: time_end=-9.0D40       ! /simulation/time_end - Stop time   
  real(DP)  :: time_restart=-9.0D40       ! /simulation/time_restart - Time to restart an old simulation
  character(len=132), dimension(:), pointer ::time_begun => null()       ! /simulation/time_begun - Actual wall-clock time simulation started
  character(len=132), dimension(:), pointer ::time_ended => null()       ! /simulation/time_ended - Actual wall-clock time simulation finished
  integer  :: iterations_max=-999999999       ! /simulation/iterations_max - Maximum number of simulation loops allowed
  integer  :: iterations_used=-999999999       ! /simulation/iterations_used - Number of simulation loops used
  character(len=132), dimension(:), pointer ::termination_condition => null()       ! /simulation/termination_condition - Expression to terminate the simulation, to be written in the platform language in the for or if (con
  integer  :: rate_plot_equilibrium=-999999999       ! /simulation/rate_plot_equilibrium - Plot the 2D images each N iterations
  character(len=132), dimension(:), pointer ::device_name => null()       ! /simulation/device_name - Name of the device being simulated
  character(len=132), dimension(:), pointer ::restart_simulation => null()       ! /simulation/restart_simulation - Identifier of the simulation to be restarted
endtype

! ***********  Include temporary/dd_temporary.xsd
type ids_temporary_constant_quantities_float_0d  !    Temporary constant Float_0D   
  real(DP)  :: value=-9.0D40       ! /value - Value
  type (ids_identifier) :: identifier  ! /identifier - Description of the quantity using the standard identifier structure
endtype

type ids_temporary_constant_quantities_int_0d  !    Temporary constant INT_0D
  integer  :: value=-999999999       ! /value - Value
  type (ids_identifier) :: identifier  ! /identifier - Description of the quantity using the standard identifier structure
endtype

type ids_temporary_constant_quantities_string_0d  !    Temporary constant STR_0D
  character(len=132), dimension(:), pointer ::value => null()       ! /value - Value
  type (ids_identifier) :: identifier  ! /identifier - Description of the quantity using the standard identifier structure
endtype

type ids_temporary_constant_quantities_float_1d  !    Temporary constant Float_1D
  real(DP),pointer  :: value(:) => null()     ! /value - Value
  type (ids_identifier) :: identifier  ! /identifier - Description of the quantity using the standard identifier structure
endtype

type ids_temporary_constant_quantities_int_1d  !    Temporary constant INT_1D
  integer,pointer  :: value(:) => null()      ! /value - Value
  type (ids_identifier) :: identifier  ! /identifier - Description of the quantity using the standard identifier structure
endtype

type ids_temporary_constant_quantities_string_1d  !    Temporary constant STR_1D
  character(len=132), dimension(:), pointer ::value => null()       ! /value - Value
  type (ids_identifier) :: identifier  ! /identifier - Description of the quantity using the standard identifier structure
endtype

! SPECIAL STRUCTURE data / time
type ids_temporary_dynamic_quantities_float_1d_value  !    Value
  real(DP), pointer  :: data(:) => null()     ! /value - Value
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_temporary_dynamic_quantities_float_1d  !    Temporary dynamic Float_1D
  type (ids_temporary_dynamic_quantities_float_1d_value) :: value  ! /value - Value
  type (ids_identifier) :: identifier  ! /identifier - Description of the quantity using the standard identifier structure
endtype

! SPECIAL STRUCTURE data / time
type ids_temporary_dynamic_quantities_int_1d_value  !    Value
  integer, pointer  :: data(:) => null()      ! /value - Value
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_temporary_dynamic_quantities_int_1d  !    Temporary dynamic Int_1D
  type (ids_temporary_dynamic_quantities_int_1d_value) :: value  ! /value - Value
  type (ids_identifier) :: identifier  ! /identifier - Description of the quantity using the standard identifier structure
endtype

! SPECIAL STRUCTURE data / time
type ids_temporary_dynamic_quantities_str_1d_value  !    Value
  character(len=132), dimension(:), pointer ::data => null()       ! /value - Value
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_temporary_dynamic_quantities_str_1d  !    Temporary dynamic STR_1D
  type (ids_temporary_dynamic_quantities_str_1d_value) :: value  ! /value - Value
  type (ids_identifier) :: identifier  ! /identifier - Description of the quantity using the standard identifier structure
endtype

type ids_temporary_constant_quantities_float_2d  !    Temporary constant Float_2D
  real(DP),pointer  :: value(:,:) => null()     ! /value - Value
  type (ids_identifier) :: identifier  ! /identifier - Description of the quantity using the standard identifier structure
endtype

type ids_temporary_constant_quantities_int_2d  !    Temporary constant INT_2D
  integer,pointer  :: value(:,:) => null()     ! /value - Value
  type (ids_identifier) :: identifier  ! /identifier - Description of the quantity using the standard identifier structure
endtype

! SPECIAL STRUCTURE data / time
type ids_temporary_dynamic_quantities_float_2d_value  !    Value
  real(DP), pointer  :: data(:,:) => null()     ! /value - Value
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_temporary_dynamic_quantities_float_2d  !    Temporary dynamic Float_2D
  type (ids_temporary_dynamic_quantities_float_2d_value) :: value  ! /value - Value
  type (ids_identifier) :: identifier  ! /identifier - Description of the quantity using the standard identifier structure
endtype

! SPECIAL STRUCTURE data / time
type ids_temporary_dynamic_quantities_int_2d_value  !    Value
  integer, pointer  :: data(:,:) => null()     ! /value - Value
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_temporary_dynamic_quantities_int_2d  !    Temporary dynamic INT_2D
  type (ids_temporary_dynamic_quantities_int_2d_value) :: value  ! /value - Value
  type (ids_identifier) :: identifier  ! /identifier - Description of the quantity using the standard identifier structure
endtype

type ids_temporary_constant_quantities_float_3d  !    Temporary constant Float_3D
  real(DP),pointer  :: value(:,:,:) => null()     ! /value - Value
  type (ids_identifier) :: identifier  ! /identifier - Description of the quantity using the standard identifier structure
endtype

type ids_temporary_constant_quantities_int_3d  !    Temporary constant INT_3D
  integer,pointer  :: value(:,:,:) => null()     ! /value - Value
  type (ids_identifier) :: identifier  ! /identifier - Description of the quantity using the standard identifier structure
endtype

! SPECIAL STRUCTURE data / time
type ids_temporary_dynamic_quantities_float_3d_value  !    Value
  real(DP), pointer  :: data(:,:,:) => null()     ! /value - Value
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_temporary_dynamic_quantities_float_3d  !    Temporary dynamic Float_3D
  type (ids_temporary_dynamic_quantities_float_3d_value) :: value  ! /value - Value
  type (ids_identifier) :: identifier  ! /identifier - Description of the quantity using the standard identifier structure
endtype

! SPECIAL STRUCTURE data / time
type ids_temporary_dynamic_quantities_int_3d_value  !    Value
  integer, pointer  :: data(:,:,:) => null()     ! /value - Value
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_temporary_dynamic_quantities_int_3d  !    Temporary dynamic INT_3D
  type (ids_temporary_dynamic_quantities_int_3d_value) :: value  ! /value - Value
  type (ids_identifier) :: identifier  ! /identifier - Description of the quantity using the standard identifier structure
endtype

! SPECIAL STRUCTURE data / time
type ids_temporary_dynamic_quantities_float_4d_value  !    Value
  real(DP), pointer  :: data(:,:,:,:) => null()     ! /value - Value
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_temporary_dynamic_quantities_float_4d  !    Temporary dynamic Float_4D
  type (ids_temporary_dynamic_quantities_float_4d_value) :: value  ! /value - Value
  type (ids_identifier) :: identifier  ! /identifier - Description of the quantity using the standard identifier structure
endtype

type ids_temporary_constant_quantities_float_4d  !    Temporary constant Float_4D
  real(DP),pointer  :: value(:,:,:,:) => null()     ! /value - Value
  type (ids_identifier) :: identifier  ! /identifier - Description of the quantity using the standard identifier structure
endtype

! SPECIAL STRUCTURE data / time
type ids_temporary_dynamic_quantities_float_5d_value  !    Value
  real(DP), pointer  :: data(:,:,:,:,:) => null()     ! /value - Value
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_temporary_dynamic_quantities_float_5d  !    Temporary dynamic Float_5D
  type (ids_temporary_dynamic_quantities_float_5d_value) :: value  ! /value - Value
  type (ids_identifier) :: identifier  ! /identifier - Description of the quantity using the standard identifier structure
endtype

type ids_temporary_constant_quantities_float_5d  !    Temporary constant Float_5D
  real(DP),pointer  :: value(:,:,:,:,:) => null()     ! /value - Value
  type (ids_identifier) :: identifier  ! /identifier - Description of the quantity using the standard identifier structure
endtype

! SPECIAL STRUCTURE data / time
type ids_temporary_dynamic_quantities_float_6d_value  !    Value
  real(DP), pointer  :: data(:,:,:,:,:,:) => null()     ! /value - Value
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_temporary_dynamic_quantities_float_6d  !    Temporary dynamic Float_6D
  type (ids_temporary_dynamic_quantities_float_6d_value) :: value  ! /value - Value
  type (ids_identifier) :: identifier  ! /identifier - Description of the quantity using the standard identifier structure
endtype

type ids_temporary_constant_quantities_float_6d  !    Temporary constant Float_6D
  real(DP),pointer  :: value(:,:,:,:,:,:) => null()     ! /value - Value
  type (ids_identifier) :: identifier  ! /identifier - Description of the quantity using the standard identifier structure
endtype

type ids_temporary  !    Storage of undeclared data model components
  type (ids_ids_properties) :: ids_properties  ! /temporary/ids_properties - 
  type (ids_temporary_constant_quantities_float_0d),pointer :: constant_float0d(:) => null()  ! /temporary/constant_float0d(i) - Constant 0D float
  type (ids_temporary_constant_quantities_int_0d),pointer :: constant_integer0d(:) => null()  ! /temporary/constant_integer0d(i) - Constant 0D integer
  type (ids_temporary_constant_quantities_string_0d),pointer :: constant_string0d(:) => null()  ! /temporary/constant_string0d(i) - Constant 0D string
  type (ids_temporary_constant_quantities_int_1d),pointer :: constant_integer1d(:) => null()  ! /temporary/constant_integer1d(i) - Constant 1D integer
  type (ids_temporary_constant_quantities_string_1d),pointer :: constant_string1d(:) => null()  ! /temporary/constant_string1d(i) - Constant 1D string
  type (ids_temporary_constant_quantities_float_1d),pointer :: constant_float1d(:) => null()  ! /temporary/constant_float1d(i) - Constant 1D float
  type (ids_temporary_dynamic_quantities_float_1d),pointer :: dynamic_float1d(:) => null()  ! /temporary/dynamic_float1d(i) - Dynamic 1D float
  type (ids_temporary_dynamic_quantities_str_1d),pointer :: dynamic_string1d(:) => null()  ! /temporary/dynamic_string1d(i) - Dynamic 1D string
  type (ids_temporary_dynamic_quantities_int_1d),pointer :: dynamic_integer1d(:) => null()  ! /temporary/dynamic_integer1d(i) - Dynamic 1D integer
  type (ids_temporary_constant_quantities_float_2d),pointer :: constant_float2d(:) => null()  ! /temporary/constant_float2d(i) - Constant 2D float
  type (ids_temporary_constant_quantities_int_2d),pointer :: constant_integer2d(:) => null()  ! /temporary/constant_integer2d(i) - Constant 2D integer
  type (ids_temporary_dynamic_quantities_float_2d),pointer :: dynamic_float2d(:) => null()  ! /temporary/dynamic_float2d(i) - Dynamic 2D float
  type (ids_temporary_dynamic_quantities_int_2d),pointer :: dynamic_integer2d(:) => null()  ! /temporary/dynamic_integer2d(i) - Dynamic 2D integer
  type (ids_temporary_constant_quantities_float_3d),pointer :: constant_float3d(:) => null()  ! /temporary/constant_float3d(i) - Constant 3D float
  type (ids_temporary_constant_quantities_int_3d),pointer :: constant_integer3d(:) => null()  ! /temporary/constant_integer3d(i) - Constant 3D integer
  type (ids_temporary_dynamic_quantities_float_3d),pointer :: dynamic_float3d(:) => null()  ! /temporary/dynamic_float3d(i) - Dynamic 3D float
  type (ids_temporary_dynamic_quantities_int_3d),pointer :: dynamic_integer3d(:) => null()  ! /temporary/dynamic_integer3d(i) - Dynamic 3D integer
  type (ids_temporary_constant_quantities_float_4d),pointer :: constant_float4d(:) => null()  ! /temporary/constant_float4d(i) - Constant 4D float
  type (ids_temporary_dynamic_quantities_float_4d),pointer :: dynamic_float4d(:) => null()  ! /temporary/dynamic_float4d(i) - Dynamic 4D float
  type (ids_temporary_constant_quantities_float_5d),pointer :: constant_float5d(:) => null()  ! /temporary/constant_float5d(i) - Constant 5D float
  type (ids_temporary_dynamic_quantities_float_5d),pointer :: dynamic_float5d(:) => null()  ! /temporary/dynamic_float5d(i) - Dynamic 5D float
  type (ids_temporary_constant_quantities_float_6d),pointer :: constant_float6d(:) => null()  ! /temporary/constant_float6d(i) - Constant 6D float
  type (ids_temporary_dynamic_quantities_float_6d),pointer :: dynamic_float6d(:) => null()  ! /temporary/dynamic_float6d(i) - Dynamic 6D float
  type (ids_code) :: code  ! /temporary/code - 
  real(DP), pointer  :: time(:) => null()  ! time
endtype

! ***********  Include tf/dd_tf.xsd
! SPECIAL STRUCTURE data / time
type ids_tf_coil_current  !    Current in the coil
  real(DP), pointer  :: data(:) => null()     ! /current - Current in the coil
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_tf_coil_voltage  !    Voltage on the coil terminals
  real(DP), pointer  :: data(:) => null()     ! /voltage - Voltage on the coil terminals
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_tf_coil  !    
  integer  :: turns=-999999999       ! /turns - Number of total turns in a toroidal field coil
  type (ids_tf_coil_current) :: current  ! /current - Current in the coil
  type (ids_tf_coil_voltage) :: voltage  ! /voltage - Voltage on the coil terminals
endtype

! SPECIAL STRUCTURE data / time
type ids_tf_b_tor_vacuum_r  !    Vacuum field times major radius in the toroidal field magnet. Positive sign means anti-clockwise when viewed from above
  real(DP), pointer  :: data(:) => null()     ! /tf/b_tor_vacuum_r - Vacuum field times major radius in the toroidal field magnet. Positive sign means anti-clockwise whe
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_tf  !    Toroidal field coils (all assumed to be identical). If individual description of coil is needed, use the same description as in pf
  type (ids_ids_properties) :: ids_properties  ! /tf/ids_properties - 
  type (ids_tf_coil),pointer :: coil(:) => null()  ! /tf/coil(i) - Number of coils around the tokamak
  type (ids_tf_b_tor_vacuum_r) :: b_tor_vacuum_r  ! /tf/b_tor_vacuum_r - Vacuum field times major radius in the toroidal field magnet. Positive sign means anti-clockwise whe
  real(DP), pointer  :: time(:) => null()  ! time
endtype

end module

