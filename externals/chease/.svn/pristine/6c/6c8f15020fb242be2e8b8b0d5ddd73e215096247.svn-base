! IDS FORTRAN 90 type definitions
! Contains the type definition of all IDSs


module ids_utilities    ! declare the set of types common to all sub-trees

integer, parameter, public :: DP = kind(1.0d0)

type ids_identifier  !    Standard type for identifiers (constant). The three fields: name, index and description are all representations of the same inform
  character(len=132), dimension(:), pointer ::name => null()       ! /name - Short string identifier
  integer  :: index=-999999999       ! /index - Integer identifier (enumeration index within a list)
  character(len=132), dimension(:), pointer ::description => null()       ! /description - Verbose description
endtype

type ids_identifier_dynamic_aos3  !    Standard type for identifiers (dynamic within type 3 array of structure (index on time)). The three fields: name, index and descri
  character(len=132), dimension(:), pointer ::name => null()       ! /name - Short string identifier
  integer  :: index=-999999999       ! /index - Integer identifier (enumeration index within a list)
  character(len=132), dimension(:), pointer ::description => null()       ! /description - Verbose description
endtype

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

type ids_plasma_composition_species  !    Description of simple species (elements) without declaration of their ionisation state
  real(DP)  :: a=-9.0D40       ! /a - Mass of atom
  real(DP)  :: z_n=-9.0D40       ! /z_n - Nuclear charge
  character(len=132), dimension(:), pointer ::label => null()       ! /label - String identifying the species (e.g. H, D, T, ...)
endtype

type ids_plasma_composition_neutral_element  !    Element entering in the composition of the neutral atom or molecule
  real(DP)  :: a=-9.0D40       ! /a - Mass of atom
  real(DP)  :: z_n=-9.0D40       ! /z_n - Nuclear charge
  real(DP)  :: multiplicity=-9.0D40       ! /multiplicity - Multiplicity of the atom
endtype

type ids_plasma_composition_neutral  !    Definition of a neutral atom or molecule
  type (ids_plasma_composition_neutral_element),pointer :: element(:) => null()  ! /element(i) - List of elements forming the atom or molecule
  type (ids_identifier),pointer :: type(:) => null()  ! /type(i) - List of neutral types, in terms of energy, considered for that neutral species. ID =1: cold; 2: ther
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

type ids_b_tor_vacuum_1  !    Characteristics of the vacuum toroidal field. time assumed to be one level above
  real(DP)  :: r0=-9.0D40       ! /r0 - Reference major radius where the vacuum toroidal magnetic field is given (usually a fixed position s
  real(DP),pointer  :: b0(:) => null()     ! /b0 - Vacuum toroidal field at R0 [T]; Positive sign means anti-clockwise when viewed from above. The prod
endtype

type ids_b_tor_vacuum_2  !    Characteristics of the vacuum toroidal field. time assumed to be two levels above
  real(DP)  :: r0=-9.0D40       ! /r0 - Reference major radius where the vacuum toroidal magnetic field is given (usually a fixed position s
  real(DP),pointer  :: b0(:) => null()     ! /b0 - Vacuum toroidal field at b0. Positive sign means anti-clockwise when viewed from above. The product 
endtype

type ids_core_radial_grid  !    1D radial grid for core* IDSs
  real(DP),pointer  :: rho_tor_norm(:) => null()     ! /rho_tor_norm - Normalised toroidal flux coordinate. The normalizing value for rho_tor_norm, is the toroidal flux co
  real(DP),pointer  :: rho_tor(:) => null()     ! /rho_tor - Toroidal flux coordinate. The toroidal field used in its definition is indicated under vacuum_toroid
  real(DP),pointer  :: psi(:) => null()     ! /psi - Poloidal magnetic flux
  real(DP),pointer  :: volume(:) => null()     ! /volume - Volume enclosed inside the magnetic surface
  real(DP),pointer  :: area(:) => null()     ! /area - Cross-sectional area of the flux surface
endtype

type ids_core_profiles_ions_charge_states2  !    Quantities related to the a given charge state of the ion species
  real(DP)  :: z_min=-9.0D40       ! /z_min - Minimum Z of the charge state bundle
  real(DP)  :: z_max=-9.0D40       ! /z_max - Maximum Z of the charge state bundle (equal to z_min if no bundle)
  real(DP)  :: z_average=-9.0D40       ! /z_average - Average Z of the charge state bundle (equal to z_min if no bundle), = sum (Z*x_z) where x_z is the r
  real(DP)  :: z_square_average=-9.0D40       ! /z_square_average - Average Z square of the charge state bundle (equal to z_min if no bundle), = sum (Z^2*x_z) where x_z
  real(DP)  :: ionisation_potential=-9.0D40       ! /ionisation_potential - Cumulative and average ionisation potential to reach a given bundle. Defined as sum (x_z* (sum of Ep
  character(len=132), dimension(:), pointer ::label => null()       ! /label - String identifying charge state (e.g. C+, C+2 , C+3, C+4, C+5, C+6, ...)
  real(DP)  :: vibrational_level=-9.0D40       ! /vibrational_level - Vibrational level (can be bundled)
  character(len=132), dimension(:), pointer ::vibrational_mode => null()       ! /vibrational_mode - Vibrational mode of this state, e.g. "A_g". Need to define, or adopt a standard nomenclature.
  integer  :: is_neutral=-999999999       ! /is_neutral - Flag specifying if this state corresponds to a neutral (1) or not (0)
  type (ids_identifier) :: neutral_type  ! /neutral_type - Neutral type (if the considered state is a neutral), in terms of energy. ID =1: cold; 2: thermal; 3:
  character(len=132), dimension(:), pointer ::electron_configuration => null()       ! /electron_configuration - Configuration of atomic orbitals of this state, e.g. 1s2-2s1
  real(DP),pointer  :: temperature(:) => null()     ! /temperature - Temperature
  real(DP),pointer  :: density(:) => null()     ! /density - Density (thermal+non-thermal)
  real(DP),pointer  :: density_fast(:) => null()     ! /density_fast - Density of fast (non-thermal) particles
  real(DP),pointer  :: pressure(:) => null()     ! /pressure - Pressure
  real(DP),pointer  :: pressure_fast_perpendicular(:) => null()     ! /pressure_fast_perpendicular - Fast (non-thermal) perpendicular pressure
  real(DP),pointer  :: pressure_fast_parallel(:) => null()     ! /pressure_fast_parallel - Fast (non-thermal) parallel pressure
  real(DP),pointer  :: velocity_tor(:) => null()     ! /velocity_tor - Toroidal velocity
  real(DP),pointer  :: velocity_pol(:) => null()     ! /velocity_pol - Poloidal velocity
endtype

type ids_core_profile_ions  !    Quantities related to a given ion species
  type (ids_plasma_composition_neutral_element),pointer :: element(:) => null()  ! /element(i) - List of elements forming the atom or molecule
  real(DP)  :: z_ion=-9.0D40       ! /z_ion - Ion charge (of the dominant ionisation state; lumped ions are allowed)
  character(len=132), dimension(:), pointer ::label => null()       ! /label - String identifying ion (e.g. H+, D+, T+, He+2, C+, ...)
  real(DP),pointer  :: temperature(:) => null()     ! /temperature - Temperature (average over charge states when multiple charge states are considered)
  real(DP),pointer  :: density(:) => null()     ! /density - Density (thermal+non-thermal) (sum over charge states when multiple charge states are considered)
  real(DP),pointer  :: density_fast(:) => null()     ! /density_fast - Density of fast (non-thermal) particles (sum over charge states when multiple charge states are cons
  real(DP),pointer  :: pressure(:) => null()     ! /pressure - Pressure (sum over charge states when multiple charge states are considered)
  real(DP),pointer  :: pressure_fast_perpendicular(:) => null()     ! /pressure_fast_perpendicular - Fast (non-thermal) perpendicular pressure  (sum over charge states when multiple charge states are c
  real(DP),pointer  :: pressure_fast_parallel(:) => null()     ! /pressure_fast_parallel - Fast (non-thermal) parallel pressure  (sum over charge states when multiple charge states are consid
  real(DP),pointer  :: velocity_tor(:) => null()     ! /velocity_tor - Toroidal velocity (average over charge states when multiple charge states are considered)
  real(DP),pointer  :: velocity_pol(:) => null()     ! /velocity_pol - Poloidal velocity (average over charge states when multiple charge states are considered)
  integer  :: multiple_states_flag=-999999999       ! /multiple_states_flag - Multiple states calculation flag : 0-Only one state is considered; 1-Multiple states are considered 
  type (ids_core_profiles_ions_charge_states2),pointer :: state(:) => null()  ! /state(i) - Quantities related to the different states of the species (ionisation, energy, excitation, ...)
endtype

type ids_core_profiles_profiles_1d_electrons  !    Quantities related to electrons
  real(DP),pointer  :: temperature(:) => null()     ! /temperature - Temperature
  real(DP),pointer  :: density(:) => null()     ! /density - Density (thermal+non-thermal)
  real(DP),pointer  :: density_fast(:) => null()     ! /density_fast - Density of fast (non-thermal) particles
  real(DP),pointer  :: pressure(:) => null()     ! /pressure - Pressure
  real(DP),pointer  :: pressure_fast_perpendicular(:) => null()     ! /pressure_fast_perpendicular - Fast (non-thermal) perpendicular pressure
  real(DP),pointer  :: pressure_fast_parallel(:) => null()     ! /pressure_fast_parallel - Fast (non-thermal) parallel pressure
  real(DP),pointer  :: velocity_tor(:) => null()     ! /velocity_tor - Toroidal velocity
  real(DP),pointer  :: velocity_pol(:) => null()     ! /velocity_pol - Poloidal velocity
endtype

type ids_generic_grid_scalar  !    Scalar values on a generic grid (dynamic within a type 3 AoS)
  integer  :: grid_index=-999999999       ! /grid_index - Index of the grid this scalar quantity is associated with
  integer  :: subgrid_index=-999999999       ! /subgrid_index - Index of the subgrid (as stored in generic_grid/subgrid) the data is stored on
  real(DP),pointer  :: values(:) => null()     ! /values - One scalar value is stored per element in the subgrid.
endtype

type ids_generic_grid_vector  !    Vector values on a generic grid (dynamic within a type 3 AoS)
  integer  :: grid_index=-999999999       ! /grid_index - Index of the grid this scalar quantity is associated with
  integer  :: subgrid_index=-999999999       ! /subgrid_index - Index of the subgrid (as stored in generic_grid/subgrid) the data is stored on
  real(DP),pointer  :: values(:,:) => null()     ! /values - List of vector components, one list per element in the subgrid. First dimenstion: element index. Sec
endtype

type ids_generic_grid_matrix  !    Matrix values on a generic grid (dynamic within a type 3 AoS)
  integer  :: grid_index=-999999999       ! /grid_index - Index of the grid this scalar quantity is associated with
  integer  :: subgrid_index=-999999999       ! /subgrid_index - Index of the subgrid (as stored in generic_grid/subgrid) the data is stored on
  real(DP),pointer  :: values(:,:,:) => null()     ! /values - List of matrix components, one list per element in the subgrid. First dimenstion: element index. Sec
endtype

type ids_generic_grid_dynamic_space_dimension_object_boundary  !    Generic grid, description of an object boundary and its neighbours (dynamic within a type 3 AoS)
  integer  :: index=-999999999       ! /index - Index of this (n-1)-dimensional boundary object
  integer,pointer  :: neighbours(:) => null()      ! /neighbours - List of indices of the n-dimensional objects adjacent to the given n-dimensional object. An object c
endtype

type ids_generic_grid_dynamic_space_dimension_object  !    Generic grid, list of objects of a given dimension within a space (dynamic within a type 3 AoS)
  type (ids_generic_grid_dynamic_space_dimension_object_boundary),pointer :: boundary(:) => null()  ! /boundary(i) - Set of  (n-1)-dimensional objects defining the boundary of this n-dimensional object
  real(DP),pointer  :: geometry(:) => null()     ! /geometry - Geometry data associated with the object. Its dimension depends on the type of object, geometry and 
  real(DP)  :: measure=-9.0D40       ! /measure - Measure of the space object, i.e. physical size (length for 1d, area for 2d, volume for 3d objects,.
endtype

type ids_generic_grid_dynamic_space_dimension  !    Generic grid, list of dimensions within a space (dynamic within a type 3 AoS)
  type (ids_generic_grid_dynamic_space_dimension_object),pointer :: object(:) => null()  ! /object(i) - Set of objects for a given dimension
endtype

type ids_generic_grid_dynamic_space  !    Generic grid space (dynamic within a type 3 AoS)
  type (ids_identifier_dynamic_aos3) :: geometry_type  ! /geometry_type - Type of space geometry (0: standard, 1:Fourier)
  integer,pointer  :: coordinates_type(:) => null()      ! /coordinates_type - Type of coordinates describing the physical space, for every coordinate of the space. The size of th
  type (ids_generic_grid_dynamic_space_dimension),pointer :: objects_per_dimension(:) => null()  ! /objects_per_dimension(i) - Definition of the space objects for every dimension (from one to the dimension of the highest-dimens
endtype

type ids_generic_grid_dynamic_subgrid_element_object  !    Generic grid, object part of an element part of a subgrid (dynamic within a type 3 AoS)
  integer  :: space=-999999999       ! /space - Index of the space from which that object is taken
  integer  :: dimension=-999999999       ! /dimension - Dimension of the object
  integer  :: index=-999999999       ! /index - Object index
endtype

type ids_generic_grid_dynamic_subgrid_element  !    Generic grid, element part of a subgrid (dynamic within a type 3 AoS)
  type (ids_generic_grid_dynamic_subgrid_element_object),pointer :: object(:) => null()  ! /object(i) - Set of objects defining the element
endtype

type ids_generic_grid_dynamic_subgrid_metric  !    Generic grid, metric description for a given subgrid and base (dynamic within a type 3 AoS)
  real(DP),pointer  :: jacobian(:) => null()     ! /jacobian - Metric Jacobian
  real(DP),pointer  :: g11_covariant(:) => null()     ! /g11_covariant - Metric coefficient g11 from the covariant metric tensor
  real(DP),pointer  :: g12_covariant(:) => null()     ! /g12_covariant - Metric coefficient g12 from the covariant metric tensor
  real(DP),pointer  :: g13_covariant(:) => null()     ! /g13_covariant - Metric coefficient g13 from the covariant metric tensor
  real(DP),pointer  :: g21_covariant(:) => null()     ! /g21_covariant - Metric coefficient g21 from the covariant metric tensor
  real(DP),pointer  :: g22_covariant(:) => null()     ! /g22_covariant - Metric coefficient g22 from the covariant metric tensor
  real(DP),pointer  :: g23_covariant(:) => null()     ! /g23_covariant - Metric coefficient g23 from the covariant metric tensor
  real(DP),pointer  :: g31_covariant(:) => null()     ! /g31_covariant - Metric coefficient g31 from the covariant metric tensor
  real(DP),pointer  :: g32_covariant(:) => null()     ! /g32_covariant - Metric coefficient g32 from the covariant metric tensor
  real(DP),pointer  :: g33_covariant(:) => null()     ! /g33_covariant - Metric coefficient g33 from the covariant metric tensor
  real(DP),pointer  :: g11_contravariant(:) => null()     ! /g11_contravariant - Metric coefficient g11 from the contravariant metric tensor
  real(DP),pointer  :: g12_contravariant(:) => null()     ! /g12_contravariant - Metric coefficient g12 from the contravariant metric tensor
  real(DP),pointer  :: g13_contravariant(:) => null()     ! /g13_contravariant - Metric coefficient g13 from the contravariant metric tensor
  real(DP),pointer  :: g21_contravariant(:) => null()     ! /g21_contravariant - Metric coefficient g21 from the contravariant metric tensor
  real(DP),pointer  :: g22_contravariant(:) => null()     ! /g22_contravariant - Metric coefficient g22 from the contravariant metric tensor
  real(DP),pointer  :: g23_contravariant(:) => null()     ! /g23_contravariant - Metric coefficient g23 from the contravariant metric tensor
  real(DP),pointer  :: g31_contravariant(:) => null()     ! /g31_contravariant - Metric coefficient g31 from the contravariant metric tensor
  real(DP),pointer  :: g32_contravariant(:) => null()     ! /g32_contravariant - Metric coefficient g32 from the contravariant metric tensor
  real(DP),pointer  :: g33_contravariant(:) => null()     ! /g33_contravariant - Metric coefficient g33 from the contravariant metric tensor
endtype

type ids_generic_grid_dynamic_subgrid  !    Generic grid subgrid (dynamic within a type 3 AoS)
  type (ids_identifier_dynamic_aos3) :: identifier  ! /identifier - Subgrid identifier
  integer  :: dimension=-999999999       ! /dimension - Space dimension of the subgrid elements. This must be equal to the sum of the dimensions of the indi
  type (ids_generic_grid_dynamic_subgrid_element),pointer :: element(:) => null()  ! /element(i) - Set of elements defining the subgrid. An element is defined by a combination of objects from potenti
  type (ids_generic_grid_dynamic_subgrid_metric),pointer :: base(:) => null()  ! /base(i) - Set of bases for the subgrid. For each base, the structure describes the projection of the base vect
  type (ids_generic_grid_dynamic_subgrid_metric) :: metric  ! /metric - Metric of the canonical frame onto Cartesian coordinates
endtype

type ids_generic_grid_dynamic  !    Generic grid (dynamic within a type 3 AoS)
  type (ids_identifier_dynamic_aos3) :: identifier  ! /identifier - Grid identifier
  type (ids_generic_grid_dynamic_space),pointer :: space(:) => null()  ! /space(i) - Set of grid spaces
  type (ids_generic_grid_dynamic_subgrid),pointer :: subgrid(:) => null()  ! /subgrid(i) - Set of subgrids
endtype

type ids_equilibrium_profiles_2d_grid  !    Definition of the 2D grid
  real(DP),pointer  :: dim1(:) => null()     ! /dim1 - First dimension values
  real(DP),pointer  :: dim2(:) => null()     ! /dim2 - Second dimension values
endtype

type ids_equilibrium_coordinate_system  !    Flux surface coordinate system on a square grid of flux and poloidal angle
  type (ids_identifier) :: grid_type  ! /grid_type - Type of coordinate system
  type (ids_equilibrium_profiles_2d_grid) :: grid  ! /grid - Definition of the 2D grid
  real(DP),pointer  :: r(:,:) => null()     ! /r - Values of the major radius on the grid
  real(DP),pointer  :: z(:,:) => null()     ! /z - Values of the Height on the grid
  real(DP),pointer  :: jacobian(:,:) => null()     ! /jacobian - Jacobian of the coordinate system
  real(DP),pointer  :: g11_covariant(:,:) => null()     ! /g11_covariant - metric coefficients g11,  covariant metric tensor for the grid described by grid_type
  real(DP),pointer  :: g12_covariant(:,:) => null()     ! /g12_covariant - metric coefficients g12,  covariant metric tensor for the grid described by grid_type
  real(DP),pointer  :: g13_covariant(:,:) => null()     ! /g13_covariant - metric coefficients g13,  covariant metric tensor for the grid described by grid_type
  real(DP),pointer  :: g22_covariant(:,:) => null()     ! /g22_covariant - metric coefficients g22,  covariant metric tensor for the grid described by grid_type
  real(DP),pointer  :: g23_covariant(:,:) => null()     ! /g23_covariant - metric coefficients g23,  covariant metric tensor for the grid described by grid_type
  real(DP),pointer  :: g33_covariant(:,:) => null()     ! /g33_covariant - metric coefficients g33,  covariant metric tensor for the grid described by grid_type
  real(DP),pointer  :: g11_contravariant(:,:) => null()     ! /g11_contravariant - metric coefficients g11,  contravariant metric tensor for the grid described by grid_type
  real(DP),pointer  :: g12_contravariant(:,:) => null()     ! /g12_contravariant - metric coefficients g12,  contravariant metric tensor for the grid described by grid_type
  real(DP),pointer  :: g13_contravariant(:,:) => null()     ! /g13_contravariant - metric coefficients g13,  contravariant metric tensor for the grid described by grid_type
  real(DP),pointer  :: g22_contravariant(:,:) => null()     ! /g22_contravariant - metric coefficients g22,  contravariant metric tensor for the grid described by grid_type
  real(DP),pointer  :: g23_contravariant(:,:) => null()     ! /g23_contravariant - metric coefficients g23,  contravariant metric tensor for the grid described by grid_type
  real(DP),pointer  :: g33_contravariant(:,:) => null()     ! /g33_contravariant - metric coefficients g33,  contravariant metric tensor for the grid described by grid_type
endtype

type ids_rzphi0d_static  !    Structure for R, Z, Phi positions (0D, static)
  real(DP)  :: r=-9.0D40       ! /r - Major radius
  real(DP)  :: z=-9.0D40       ! /z - Height
  real(DP)  :: phi=-9.0D40       ! /phi - Toroidal angle
endtype

type ids_rzphi0d_dynamic_aos3  !    Structure for R, Z, Phi positions (0D, dynamic within a type 3 array of structure (index on time))
  real(DP)  :: r=-9.0D40       ! /r - Major radius
  real(DP)  :: z=-9.0D40       ! /z - Height
  real(DP)  :: phi=-9.0D40       ! /phi - Toroidal angle
endtype

type ids_rzphi1d_static  !    Structure for list of R, Z, Phi positions (1D, static)
  real(DP),pointer  :: r(:) => null()     ! /r - Major radius
  real(DP),pointer  :: z(:) => null()     ! /z - Height
  real(DP),pointer  :: phi(:) => null()     ! /phi - Toroidal angle
endtype

! SPECIAL STRUCTURE data / time
type ids_rzphi1d_dynamic_aos1_r  !    Major radius
  real(DP), pointer  :: data(:) => null()     ! /r - Major radius
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_rzphi1d_dynamic_aos1_z  !    Height
  real(DP), pointer  :: data(:) => null()     ! /z - Height
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_rzphi1d_dynamic_aos1_phi  !    Toroidal angle
  real(DP), pointer  :: data(:) => null()     ! /phi - Toroidal angle
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_rzphi1d_dynamic_aos1  !    Structure for list of R, Z, Phi positions (1D, dynamic within a type 1 array of structure (indexed on objects, data/time structure
  type (ids_rzphi1d_dynamic_aos1_r) :: r  ! /r - Major radius
  type (ids_rzphi1d_dynamic_aos1_z) :: z  ! /z - Height
  type (ids_rzphi1d_dynamic_aos1_phi) :: phi  ! /phi - Toroidal angle
endtype

type ids_rzphi1d_dynamic_aos3  !    Structure for R, Z, Phi positions (1D, dynamic within a type 3 array of structure (index on time))
  real(DP),pointer  :: r(:) => null()     ! /r - Major radius
  real(DP)  :: z=-9.0D40       ! /z - Height
  real(DP)  :: phi=-9.0D40       ! /phi - Toroidal angle
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

type ids_rz0d_static  !    Structure for a single R, Z position (0D, static)
  real(DP)  :: r=-9.0D40       ! /r - Major radius
  real(DP)  :: z=-9.0D40       ! /z - Height
endtype

type ids_rz0d_constant  !    Structure for a single R, Z position (0D, constant)
  real(DP)  :: r=-9.0D40       ! /r - Major radius
  real(DP)  :: z=-9.0D40       ! /z - Height
endtype

type ids_line_of_sight_2points  !    Generic description of a line of sight, defined by two points
  type (ids_rzphi0d_static) :: first_point  ! /first_point - Position of the first point
  type (ids_rzphi0d_static) :: second_point  ! /second_point - Position of the second point
endtype

type ids_line_of_sight_2points_dynamic_aos3  !    Generic description of a line of sight, defined by two points, dynamic within a type 3 array of structure (index on time)
  type (ids_rzphi0d_dynamic_aos3) :: first_point  ! /first_point - Position of the first point
  type (ids_rzphi0d_dynamic_aos3) :: second_point  ! /second_point - Position of the second point
endtype

type ids_line_of_sight_3points  !    Generic description of a line of sight, defined by two points (one way) and an optional third point to indicate the direction of r
  type (ids_rzphi0d_static) :: first_point  ! /first_point - Position of the first point
  type (ids_rzphi0d_static) :: second_point  ! /second_point - Position of the second point
  type (ids_rzphi0d_static) :: third_point  ! /third_point - Position of the third point
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
  integer  :: geometry_type=-999999999       ! /geometry_type - Type used to describe the element shape (1:'outline', 2:'rectangle', 3:'oblique') 
  type (ids_rz1d_static) :: outline  ! /outline - Irregular outline of the element
  type (ids_rectangle_static) :: rectangle  ! /rectangle - Rectangular description of the element
  type (ids_oblique_static) :: oblique  ! /oblique - Trapezoidal description of the element
endtype

type ids_data_entry  !    Definition of a data entry
  character(len=132), dimension(:), pointer ::user => null()       ! /user - Username
  character(len=132), dimension(:), pointer ::machine => null()       ! /machine - Name of the experimental device to which this data is related
  character(len=132), dimension(:), pointer ::pulse_type => null()       ! /pulse_type - Type of the data entry, e.g. "pulse", "simulation", ...
  integer  :: pulse=-999999999       ! /pulse - Pulse number
  integer  :: run=-999999999       ! /run - Run number
endtype

type ids_plasma_composition  !    Generic declaration of Plasma Composition for a simulation
  type (ids_plasma_composition_ions),pointer :: ions(:) => null()  ! /plasma_composition/ions(i) - Array of plasma ions
  type (ids_plasma_composition_neutral),pointer :: neutrals(:) => null()  ! /plasma_composition/neutrals(i) - Array of neutrals
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

! ***********  Include antennas/dd_antennas.xsd
! SPECIAL STRUCTURE data / time
type ids_antennas_beam_spot_size  !    Size of the spot ellipse
  real(DP), pointer  :: data(:,:) => null()     ! /size - Size of the spot ellipse
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_antennas_beam_spot_angle  !    Rotation angle for the spot ellipse
  real(DP), pointer  :: data(:) => null()     ! /angle - Rotation angle for the spot ellipse
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_antennas_beam_spot  !    Spot ellipse characteristics
  type (ids_antennas_beam_spot_size) :: size  ! /size - Size of the spot ellipse
  type (ids_antennas_beam_spot_angle) :: angle  ! /angle - Rotation angle for the spot ellipse
endtype

! SPECIAL STRUCTURE data / time
type ids_antennas_beam_phase_curvature  !    Inverse curvature radii for the phase ellipse, positive/negative for divergent/convergent beams
  real(DP), pointer  :: data(:,:) => null()     ! /curvature - Inverse curvature radii for the phase ellipse, positive/negative for divergent/convergent beams
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_antennas_beam_phase_angle  !    Rotation angle for the phase ellipse
  real(DP), pointer  :: data(:) => null()     ! /angle - Rotation angle for the phase ellipse
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_antennas_beam_phase  !    Phase ellipse characteristics
  type (ids_antennas_beam_phase_curvature) :: curvature  ! /curvature - Inverse curvature radii for the phase ellipse, positive/negative for divergent/convergent beams
  type (ids_antennas_beam_phase_angle) :: angle  ! /angle - Rotation angle for the phase ellipse
endtype

type ids_antennas_beam  !    Beam characteristics
  type (ids_antennas_beam_spot) :: spot  ! /spot - Spot ellipse characteristics
  type (ids_antennas_beam_phase) :: phase  ! /phase - Phase ellipse characteristics
endtype

! SPECIAL STRUCTURE data / time
type ids_antennas_ec_power  !    Power
  real(DP), pointer  :: data(:) => null()     ! /power - Power
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_antennas_ec_mode  !    Incoming wave mode (+ or -1 for O/X mode)
  integer, pointer  :: data(:) => null()      ! /mode - Incoming wave mode (+ or -1 for O/X mode)
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_antennas_ec_launching_angle_pol  !    Poloidal launching angle between the horizontal plane and the poloidal component of the nominal beam centerline. tan(angle_pol)=-k
  real(DP), pointer  :: data(:) => null()     ! /launching_angle_pol - Poloidal launching angle between the horizontal plane and the poloidal component of the nominal beam
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_antennas_ec_launching_angle_tor  !    Toroidal launching angle between the poloidal plane and the nominal beam centerline. sin(angle_tor)=k_phi
  real(DP), pointer  :: data(:) => null()     ! /launching_angle_tor - Toroidal launching angle between the poloidal plane and the nominal beam centerline. sin(angle_tor)=
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_antennas_ec  !    Electron Cyclotron Antenna
  character(len=132), dimension(:), pointer ::name => null()       ! /name - Name of the antenna
  real(DP)  :: frequency=-9.0D40       ! /frequency - Frequency
  type (ids_antennas_ec_power) :: power  ! /power - Power
  type (ids_antennas_ec_mode) :: mode  ! /mode - Incoming wave mode (+ or -1 for O/X mode)
  type (ids_rzphi1d_dynamic_aos1) :: launching_position  ! /launching_position - Launching position of the beam
  type (ids_antennas_ec_launching_angle_pol) :: launching_angle_pol  ! /launching_angle_pol - Poloidal launching angle between the horizontal plane and the poloidal component of the nominal beam
  type (ids_antennas_ec_launching_angle_tor) :: launching_angle_tor  ! /launching_angle_tor - Toroidal launching angle between the poloidal plane and the nominal beam centerline. sin(angle_tor)=
  type (ids_antennas_beam) :: beam  ! /beam - Beam characteristics
endtype

! SPECIAL STRUCTURE data / time
type ids_antennas_ic_strap_current  !    Root mean square current flowing along the strap
  real(DP), pointer  :: data(:) => null()     ! /current - Root mean square current flowing along the strap
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_antennas_ic_strap_phase  !    Phase of the strap current
  real(DP), pointer  :: data(:) => null()     ! /phase - Phase of the strap current
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_antennas_ic_strap  !    Properties of IC antenna strap
  type (ids_rzphi0d_static) :: position  ! /position - R, Z, Phi position of the strap centre
  real(DP)  :: width_tor=-9.0D40       ! /width_tor - Width of strap in the toroidal direction
  real(DP)  :: distance_to_conductor=-9.0D40       ! /distance_to_conductor - Distance to conducting wall or other conductor behind the antenna strap
  type (ids_outline_2d_geometry_static) :: geometry  ! /geometry - Cross-sectional shape of the strap
  type (ids_antennas_ic_strap_current) :: current  ! /current - Root mean square current flowing along the strap
  type (ids_antennas_ic_strap_phase) :: phase  ! /phase - Phase of the strap current
endtype

type ids_antennas_ic_surface_current  !    Description of the IC surface current on the antenna straps and on passive components.
  integer,pointer  :: m_pol(:) => null()      ! /m_pol - Poloidal mode numbers, used to describe the spectrum of the antenna current. The poloidal angle is d
  integer,pointer  :: n_tor(:) => null()      ! /n_tor - Toroidal mode numbers, used to describe the spectrum of the antenna current
  integer,pointer  :: spectrum(:) => null()      ! /spectrum - Spectrum of the total surface current on the antenna strap and passive components expressed in poloi
  real(DP)  :: time=-9.0D40       ! /time - Time
endtype

! SPECIAL STRUCTURE data / time
type ids_antennas_ic_frequency  !    Frequency
  real(DP), pointer  :: data(:) => null()     ! /frequency - Frequency
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_antennas_ic_power  !    Power
  real(DP), pointer  :: data(:) => null()     ! /power - Power
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_antennas_ic  !    Ion Cyclotron Antenna
  character(len=132), dimension(:), pointer ::name => null()       ! /name - Name of the antenna
  type (ids_antennas_ic_frequency) :: frequency  ! /frequency - Frequency
  type (ids_antennas_ic_power) :: power  ! /power - Power
  type (ids_antennas_ic_strap),pointer :: strap(:) => null()  ! /strap(i) - Properties of IC antenna straps
  type (ids_antennas_ic_surface_current),pointer :: surface_current(:) => null()  ! /surface_current(i) - Description of the IC surface current on the antenna straps and on passive components, for every tim
endtype

type ids_antennas  !    Antenna systems for heating and current drive in the electron cyclotron (EC) and ion cylcotron (IC) frequencies.
  type (ids_ids_properties) :: ids_properties  ! /antennas/ids_properties - 
  type (ids_antennas_ec),pointer :: ec(:) => null()  ! /antennas/ec(i) - Electron Cyclotron antennas
  type (ids_rz0d_constant) :: reference_point  ! /antennas/reference_point - Reference point used to define the poloidal angle, e.g. the geometrical centre of the vacuum vessel.
  type (ids_antennas_ic),pointer :: ic(:) => null()  ! /antennas/ic(i) - Ion Cyclotron antennas
  type (ids_code) :: code  ! /antennas/code - 
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

! ***********  Include charge_exchange/dd_charge_exchange.xsd
! SPECIAL STRUCTURE data / time
type ids_charge_exchange_channel_temperature  !    Ion temperature (of the emmitting impurity)
  real(DP), pointer  :: data(:) => null()     ! /temperature - Ion temperature (of the emmitting impurity)
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_charge_exchange_channel_velocity_tor  !    Toroidal velocity (of the emmitting impurity)
  real(DP), pointer  :: data(:) => null()     ! /velocity_tor - Toroidal velocity (of the emmitting impurity)
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_charge_exchange_channel_velocity_pol  !    Poloidal velocity (of the emmitting impurity)
  real(DP), pointer  :: data(:) => null()     ! /velocity_pol - Poloidal velocity (of the emmitting impurity)
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_charge_exchange_channel  !    Charge exchange channel
  character(len=132), dimension(:), pointer ::name => null()       ! /name - Name of the channel
  character(len=132), dimension(:), pointer ::identifier => null()       ! /identifier - ID of the channel
  real(DP),pointer  :: a(:) => null()     ! /a - Mass of atom of the emmitting impurity. Varies according to channels since they are spanning differe
  real(DP),pointer  :: z_ion(:) => null()     ! /z_ion - Ion charge of the emmitting impurity. Varies according to channels since they are spanning different
  real(DP),pointer  :: z_n(:) => null()     ! /z_n - Nuclear charge of the emmitting impurity. Varies according to channels since they are spanning diffe
  character(len=132), dimension(:), pointer ::label => null()       ! /label - String identifying the emmitting impurity (e.g. H+, D+, T+, He+2, C+6, ...)
  type (ids_rzphi1d_dynamic_aos1) :: position  ! /position - Position of the measurements
  type (ids_charge_exchange_channel_temperature) :: temperature  ! /temperature - Ion temperature (of the emmitting impurity)
  type (ids_charge_exchange_channel_velocity_tor) :: velocity_tor  ! /velocity_tor - Toroidal velocity (of the emmitting impurity)
  type (ids_charge_exchange_channel_velocity_pol) :: velocity_pol  ! /velocity_pol - Poloidal velocity (of the emmitting impurity)
endtype

type ids_charge_exchange  !    Charge exchange spectroscopy diagnostic
  type (ids_ids_properties) :: ids_properties  ! /charge_exchange/ids_properties - 
  type (ids_charge_exchange_channel),pointer :: channel(:) => null()  ! /charge_exchange/channel(i) - Set of channels (lines-of-sight)
  type (ids_code) :: code  ! /charge_exchange/code - 
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

! ***********  Include core_instant_changes/dd_core_instant_changes.xsd
type ids_core_instant_changes_change_profiles  !    instant_change terms for a given time slice
  type (ids_core_radial_grid) :: grid  ! /grid - Radial grid
  type (ids_core_profiles_profiles_1d_electrons) :: electrons  ! /electrons - Change of electrons-related quantities
  real(DP),pointer  :: t_i_average(:) => null()     ! /t_i_average - change of average ion temperature
  real(DP),pointer  :: momentum_tor(:) => null()     ! /momentum_tor - change of total toroidal momentum
  type (ids_core_profile_ions),pointer :: ion(:) => null()  ! /ion(i) - changes related to the different ions and neutral species
  real(DP)  :: time=-9.0D40       ! /time - Time
endtype

type ids_core_instant_changes_change  !    instant_change terms for a given instant_change
  type (ids_identifier) :: identifier  ! /identifier - Instant change term identifier
  type (ids_core_instant_changes_change_profiles),pointer :: profiles_1d(:) => null()  ! /profiles_1d(i) - instant_change profiles for various time slices
endtype

type ids_core_instant_changes  !    Instant changes of the radial core plasma profiles due to pellet, MHD, ... 
  type (ids_ids_properties) :: ids_properties  ! /core_instant_changes/ids_properties - 
  type (ids_b_tor_vacuum_1) :: vacuum_toroidal_field  ! /core_instant_changes/vacuum_toroidal_field - Characteristics of the vacuum toroidal field (used in Rho_Tor definition and in the normalization of
  type (ids_core_instant_changes_change),pointer :: change(:) => null()  ! /core_instant_changes/change(i) - Set of instant change terms
  type (ids_code) :: code  ! /core_instant_changes/code - 
  real(DP), pointer  :: time(:) => null()  ! time
endtype

! ***********  Include core_profiles/dd_core_profiles.xsd
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
  type (ids_core_profiles_profiles_1d_electrons) :: electrons  ! /electrons - Quantities related to the electrons
  type (ids_core_profile_ions),pointer :: ion(:) => null()  ! /ion(i) - Quantities related to the different ion and neutral species
  real(DP),pointer  :: t_i_average(:) => null()     ! /t_i_average - Ion temperature (averaged on charge states and ion species)
  real(DP),pointer  :: n_i_total_over_n_e(:) => null()     ! /n_i_total_over_n_e - Ratio of total ion density (sum over species and charge states) over electron density. (thermal+non-
  real(DP),pointer  :: momentum_tor(:) => null()     ! /momentum_tor - Total plasma toroidal momentum, summed over ion species and electrons 
  real(DP),pointer  :: zeff(:) => null()     ! /zeff - Effective charge
  real(DP),pointer  :: pressure_ion_total(:) => null()     ! /pressure_ion_total - Total (sum over ion species) thermal ion pressure
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
type ids_core_sources_source_profiles_1d_ions_charge_states  !    Source terms related to the a given charge state of the ion species
  real(DP)  :: z_min=-9.0D40       ! /z_min - Minimum Z of the charge state bundle
  real(DP)  :: z_max=-9.0D40       ! /z_max - Maximum Z of the charge state bundle
  character(len=132), dimension(:), pointer ::label => null()       ! /label - String identifying charge state (e.g. C+, C+2 , C+3, C+4, C+5, C+6, ...)
  real(DP)  :: vibrational_level=-9.0D40       ! /vibrational_level - Vibrational level (can be bundled)
  character(len=132), dimension(:), pointer ::vibrational_mode => null()       ! /vibrational_mode - Vibrational mode of this state, e.g. "A_g". Need to define, or adopt a standard nomenclature.
  integer  :: is_neutral=-999999999       ! /is_neutral - Flag specifying if this state corresponds to a neutral (1) or not (0)
  type (ids_identifier) :: neutral_type  ! /neutral_type - Neutral type (if the considered state is a neutral), in terms of energy. ID =1: cold; 2: thermal; 3:
  character(len=132), dimension(:), pointer ::electron_configuration => null()       ! /electron_configuration - Configuration of atomic orbitals of this state, e.g. 1s2-2s1
  real(DP),pointer  :: particles(:) => null()     ! /particles - Source term for the charge state density transport equation
  real(DP),pointer  :: energy(:) => null()     ! /energy - Source terms for the charge state energy transport equation
endtype

type ids_core_sources_source_profiles_1d_ions  !    Source terms related to a given ion species
  type (ids_plasma_composition_neutral_element),pointer :: element(:) => null()  ! /element(i) - List of elements forming the atom or molecule
  real(DP)  :: z_ion=-9.0D40       ! /z_ion - Ion charge (of the dominant ionisation state; lumped ions are allowed)
  character(len=132), dimension(:), pointer ::label => null()       ! /label - String identifying ion (e.g. H+, D+, T+, He+2, C+, ...)
  real(DP),pointer  :: particles(:) => null()     ! /particles - Source term for ion density equation
  real(DP),pointer  :: energy(:) => null()     ! /energy - Source term for the ion energy transport equation.
  integer  :: multiple_states_flag=-999999999       ! /multiple_states_flag - Multiple states calculation flag : 0-Only one state is considered; 1-Multiple states are considered 
  type (ids_core_sources_source_profiles_1d_ions_charge_states),pointer :: state(:) => null()  ! /state(i) - Source terms related to the different charge states of the species (ionisation, energy, excitation, 
endtype

type ids_core_sources_source_profiles_1d_electrons  !    Source terms related to electrons
  real(DP),pointer  :: particles(:) => null()     ! /particles - Source term for electron density equation
  real(DP),pointer  :: energy(:) => null()     ! /energy - Source term for the electron energy equation
endtype

type ids_core_sources_source_profiles_1d  !    Source terms for a given time slice
  type (ids_core_radial_grid) :: grid  ! /grid - Radial grid
  type (ids_core_sources_source_profiles_1d_electrons) :: electrons  ! /electrons - Sources for electrons
  real(DP),pointer  :: total_ion_energy(:) => null()     ! /total_ion_energy - Source term for the total ion energy equation
  real(DP),pointer  :: momentum_tor(:) => null()     ! /momentum_tor - Source term for total toroidal momentum equation
  real(DP),pointer  :: conductivity_parallel(:) => null()     ! /conductivity_parallel - Parallel conductivity induced by this source
  type (ids_core_sources_source_profiles_1d_ions),pointer :: ion(:) => null()  ! /ion(i) - Source terms related to the different ions and neutral species
  real(DP)  :: time=-9.0D40       ! /time - Time
endtype

type ids_core_sources_source  !    Source terms for a given actuator
  type (ids_identifier) :: identifier  ! /identifier - Source term identifier
  type (ids_core_sources_source_profiles_1d),pointer :: profiles_1d(:) => null()  ! /profiles_1d(i) - Source profiles for various time slices
endtype

type ids_core_sources  !    Core plasma source terms (for the transport equations)
  type (ids_ids_properties) :: ids_properties  ! /core_sources/ids_properties - 
  type (ids_b_tor_vacuum_1) :: vacuum_toroidal_field  ! /core_sources/vacuum_toroidal_field - Characteristics of the vacuum toroidal field (used in Rho_Tor definition and in the normalization of
  type (ids_core_sources_source),pointer :: source(:) => null()  ! /core_sources/source(i) - Set of source terms
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
  real(DP)  :: vibrational_level=-9.0D40       ! /vibrational_level - Vibrational level (can be bundled)
  character(len=132), dimension(:), pointer ::vibrational_mode => null()       ! /vibrational_mode - Vibrational mode of this state, e.g. "A_g". Need to define, or adopt a standard nomenclature.
  integer  :: is_neutral=-999999999       ! /is_neutral - Flag specifying if this state corresponds to a neutral (1) or not (0)
  type (ids_identifier) :: neutral_type  ! /neutral_type - Neutral type (if the considered state is a neutral), in terms of energy. ID =1: cold; 2: thermal; 3:
  character(len=132), dimension(:), pointer ::electron_configuration => null()       ! /electron_configuration - Configuration of atomic orbitals of this state, e.g. 1s2-2s1
  type (ids_core_transport_model_3_density) :: particles  ! /particles - Transport quantities related to density equation of the charge state considered (thermal+non-thermal
  type (ids_core_transport_model_3_energy) :: energy  ! /energy - Transport quantities related to the energy equation of the charge state considered 
endtype

type ids_core_transport_model_ions  !    Transport coefficients related to a given ion species
  type (ids_plasma_composition_neutral_element),pointer :: element(:) => null()  ! /element(i) - List of elements forming the atom or molecule
  real(DP)  :: z_ion=-9.0D40       ! /z_ion - Ion charge (of the dominant ionisation state; lumped ions are allowed)
  character(len=132), dimension(:), pointer ::label => null()       ! /label - String identifying ion (e.g. H+, D+, T+, He+2, C+, ...)
  type (ids_core_transport_model_2_density) :: particles  ! /particles - Transport related to the ion density equation
  type (ids_core_transport_model_2_energy) :: energy  ! /energy - Transport coefficients related to the ion energy equation
  integer  :: multiple_states_flag=-999999999       ! /multiple_states_flag - Multiple states calculation flag : 0-Only one state is considered; 1-Multiple states are considered 
  type (ids_core_transport_model_ions_charge_states),pointer :: state(:) => null()  ! /state(i) - Transport coefficients related to the different states of the species
endtype

type ids_core_transport_model_electrons  !    Transport coefficients related to electrons
  type (ids_core_transport_model_2_density) :: particles  ! /particles - Transport quantities for the electron density equation
  type (ids_core_transport_model_2_energy) :: energy  ! /energy - Transport quantities for the electron energy equation
endtype

type ids_core_transport_model_profiles_1d  !    Transport coefficient profiles at a given time slice
  type (ids_core_radial_grid) :: grid_d  ! /grid_d - Grid for effective diffusivities and parallel conductivity
  type (ids_core_radial_grid) :: grid_v  ! /grid_v - Grid for effective convections
  type (ids_core_radial_grid) :: grid_flux  ! /grid_flux - Grid for fluxes
  real(DP),pointer  :: conductivity_parallel(:) => null()     ! /conductivity_parallel - Parallel conductivity
  type (ids_core_transport_model_electrons) :: electrons  ! /electrons - Transport quantities related to the electrons
  type (ids_core_transport_model_1_energy) :: total_ion_energy  ! /total_ion_energy - Transport coefficients for the total ion energy equation
  type (ids_core_transport_model_1_momentum) :: momentum_tor  ! /momentum_tor - Transport coefficients for total toroidal momentum equation
  type (ids_core_transport_model_ions),pointer :: ion(:) => null()  ! /ion(i) - Transport coefficients related to the various ion and neutral species
  real(DP)  :: time=-9.0D40       ! /time - Time
endtype

type ids_core_transport_model  !    Transport coefficients for a given model
  type (ids_identifier) :: identifier  ! /identifier - Transport model identifier
  real(DP)  :: flux_multiplier=-9.0D40       ! /flux_multiplier - Multiplier applied to the particule flux when adding its contribution in the expression of the heat 
  type (ids_core_transport_model_profiles_1d),pointer :: profiles_1d(:) => null()  ! /profiles_1d(i) - Transport coefficient profiles for various time slices
endtype

type ids_core_transport  !    Core plasma transport
  type (ids_ids_properties) :: ids_properties  ! /core_transport/ids_properties - 
  type (ids_b_tor_vacuum_1) :: vacuum_toroidal_field  ! /core_transport/vacuum_toroidal_field - Characteristics of the vacuum toroidal field (used in Rho_Tor definition and in the normalization of
  type (ids_core_transport_model),pointer :: model(:) => null()  ! /core_transport/model(i) - Transport is described by a combination of various transport models
  type (ids_code) :: code  ! /core_transport/code - 
  real(DP), pointer  :: time(:) => null()  ! time
endtype

! ***********  Include dataset_description/dd_dataset_description.xsd
type ids_dataset_description_simulation  !    Description of the general simulation characteristics, if this data entry has been produced by a simulation. Several nodes describ
  character(len=132), dimension(:), pointer ::comment_before => null()       ! /comment_before - Comment made when launching a simulation
  character(len=132), dimension(:), pointer ::comment_after => null()       ! /comment_after - Comment made at the end of a simulation
  real(DP)  :: time_begin=-9.0D40       ! /time_begin - Start time
  real(DP)  :: time_step=-9.0D40       ! /time_step - Time interval between main steps, e.g. storage step (if relevant and constant)
  real(DP)  :: time_end=-9.0D40       ! /time_end - Stop time
  real(DP)  :: time_restart=-9.0D40       ! /time_restart - Time of the last restart done during the simulation
  real(DP)  :: time_current=-9.0D40       ! /time_current - Current time of the simulation
  character(len=132), dimension(:), pointer ::time_begun => null()       ! /time_begun - Actual wall-clock time simulation started
  character(len=132), dimension(:), pointer ::time_ended => null()       ! /time_ended - Actual wall-clock time simulation finished
  character(len=132), dimension(:), pointer ::workflow => null()       ! /workflow - Description of the workflow which has been used to produce this data entry (e.g. copy of the Kepler 
endtype

type ids_dataset_description  !    General description of the dataset (collection of all IDSs within the given database entry). Main description text to be put in id
  type (ids_ids_properties) :: ids_properties  ! /dataset_description/ids_properties - 
  type (ids_data_entry) :: data_entry  ! /dataset_description/data_entry - Definition of this data entry
  type (ids_data_entry) :: parent_entry  ! /dataset_description/parent_entry - Definition of the parent data entry, if the present data entry has been generated by applying a give
  character(len=132), dimension(:), pointer ::imas_version => null()       ! /dataset_description/imas_version - Version of the IMAS infrastructure used to produce this data entry. Refers to the global IMAS reposi
  character(len=132), dimension(:), pointer ::dd_version => null()       ! /dataset_description/dd_version - Version of the physics data dictionary of this dataset
  type (ids_dataset_description_simulation) :: simulation  ! /dataset_description/simulation - Description of the general simulation characteristics, if this data entry has been produced by a sim
  real(DP), pointer  :: time(:) => null()  ! time
endtype

! ***********  Include edge_profiles/dd_edge_profiles.xsd
type ids_edge_profiles_time_slice_vector_components  !    Vector components in radial, diamgnetic and poloidal directions on a generic grid
  type (ids_generic_grid_scalar),pointer :: radial(:) => null()  ! /radial(i) - Radial component, given on various subgrids
  type (ids_generic_grid_scalar),pointer :: diamagnetic(:) => null()  ! /diamagnetic(i) - Diamagnetic component, given on various subgrids
  type (ids_generic_grid_scalar),pointer :: poloidal(:) => null()  ! /poloidal(i) - Poloidal component, given on various subgrids
endtype

type ids_edge_profiles_time_slice_ion_charge_state  !    Quantities related to the a given charge state of the ion species
  real(DP)  :: z_min=-9.0D40       ! /z_min - Minimum Z of the charge state bundle (z_min = z_max = 0 for a neutral)
  real(DP)  :: z_max=-9.0D40       ! /z_max - Maximum Z of the charge state bundle (equal to z_min if no bundle)
  type (ids_generic_grid_scalar),pointer :: z_average(:) => null()  ! /z_average(i) - Average Z of the charge state bundle (equal to z_min if no bundle), = sum (Z*x_z) where x_z is the r
  type (ids_generic_grid_scalar),pointer :: z_square_average(:) => null()  ! /z_square_average(i) - Average Z square of the charge state bundle (equal to z_min if no bundle), = sum (Z^2*x_z) where x_z
  type (ids_generic_grid_scalar),pointer :: ionisation_potential(:) => null()  ! /ionisation_potential(i) - Cumulative and average ionisation potential to reach a given bundle. Defined as sum (x_z* (sum of Ep
  character(len=132), dimension(:), pointer ::label => null()       ! /label - String identifying charge state (e.g. C+, C+2 , C+3, C+4, C+5, C+6, ...)
  character(len=132), dimension(:), pointer ::electron_configuration => null()       ! /electron_configuration - Configuration of atomic orbitals of this state, e.g. 1s2-2s1
  real(DP)  :: vibrational_level=-9.0D40       ! /vibrational_level - Vibrational level (can be bundled)
  character(len=132), dimension(:), pointer ::vibrational_mode => null()       ! /vibrational_mode - Vibrational mode of this state, e.g. "A_g". Need to define, or adopt a standard nomenclature.
  integer  :: is_neutral=-999999999       ! /is_neutral - Flag specifying if this state corresponds to a neutral (1) or not (0)
  type (ids_identifier) :: neutral_type  ! /neutral_type - Neutral type (if the considered state is a neutral), in terms of energy. ID =1: cold; 2: thermal; 3:
  type (ids_generic_grid_scalar),pointer :: temperature(:) => null()  ! /temperature(i) - Temperature, given on various subgrids
  type (ids_generic_grid_scalar),pointer :: density(:) => null()  ! /density(i) - Density (thermal+non-thermal), given on various subgrids
  type (ids_generic_grid_scalar),pointer :: density_fast(:) => null()  ! /density_fast(i) - Density of fast (non-thermal) particles, given on various subgrids
  type (ids_generic_grid_scalar),pointer :: pressure(:) => null()  ! /pressure(i) - Pressure, given on various subgrids
  type (ids_generic_grid_scalar),pointer :: pressure_fast_perpendicular(:) => null()  ! /pressure_fast_perpendicular(i) - Fast (non-thermal) perpendicular pressure, given on various subgrids
  type (ids_generic_grid_scalar),pointer :: pressure_fast_parallel(:) => null()  ! /pressure_fast_parallel(i) - Fast (non-thermal) parallel pressure, given on various subgrids
  type (ids_generic_grid_scalar),pointer :: velocity_tor(:) => null()  ! /velocity_tor(i) - Toroidal velocity, given on various subgrids
  type (ids_generic_grid_scalar),pointer :: velocity_pol(:) => null()  ! /velocity_pol(i) - Poloidal velocity, given on various subgrids
  type (ids_generic_grid_scalar),pointer :: velocity_diamagnetic(:) => null()  ! /velocity_diamagnetic(i) - Velocity in the diamagnetic direction, given on various subgrids
  type (ids_generic_grid_scalar),pointer :: velocity_diamagnetic_drift_diamagnetic(:) => null()  ! /velocity_diamagnetic_drift_diamagnetic(i) - Velocity due to the diamagnetic drift in the diamagnetic direction, given on various subgrids
  type (ids_generic_grid_scalar),pointer :: velocity_exb_drift_radial(:) => null()  ! /velocity_exb_drift_radial(i) - Velocity due to the ExB drift in the radial direction, given on various subgrids
  type (ids_generic_grid_scalar),pointer :: velocity_exb_drift_diamagnetic(:) => null()  ! /velocity_exb_drift_diamagnetic(i) - Velocity due to the ExB drift in the diamagnetic direction, given on various subgrids
  type (ids_generic_grid_scalar),pointer :: energy_density_kinetic(:) => null()  ! /energy_density_kinetic(i) - Kinetic energy density, given on various subgrids
  type (ids_generic_grid_scalar),pointer :: distribution_function(:) => null()  ! /distribution_function(i) - Distribution function, given on various subgrids
endtype

type ids_edge_profiles_time_slice_ion  !    Quantities related to a given ion species
  type (ids_plasma_composition_neutral_element),pointer :: element(:) => null()  ! /element(i) - List of elements forming the atom or molecule
  real(DP)  :: z_ion=-9.0D40       ! /z_ion - Ion charge (of the dominant ionisation state; lumped ions are allowed).
  character(len=132), dimension(:), pointer ::label => null()       ! /label - String identifying the species (e.g. H+, D+, T+, He+2, C+, D2, DT, CD4, ...)
  type (ids_generic_grid_scalar),pointer :: temperature(:) => null()  ! /temperature(i) - Temperature (average over charge states when multiple charge states are considered), given on variou
  type (ids_generic_grid_scalar),pointer :: density(:) => null()  ! /density(i) - Density (thermal+non-thermal) (sum over charge states when multiple charge states are considered), g
  type (ids_generic_grid_scalar),pointer :: density_fast(:) => null()  ! /density_fast(i) - Density of fast (non-thermal) particles (sum over charge states when multiple charge states are cons
  type (ids_generic_grid_scalar),pointer :: pressure(:) => null()  ! /pressure(i) - Pressure (average over charge states when multiple charge states are considered), given on various s
  type (ids_generic_grid_scalar),pointer :: pressure_fast_perpendicular(:) => null()  ! /pressure_fast_perpendicular(i) - Fast (non-thermal) perpendicular pressure (average over charge states when multiple charge states ar
  type (ids_generic_grid_scalar),pointer :: pressure_fast_parallel(:) => null()  ! /pressure_fast_parallel(i) - Fast (non-thermal) parallel pressure (average over charge states when multiple charge states are con
  type (ids_generic_grid_scalar),pointer :: velocity_tor(:) => null()  ! /velocity_tor(i) - Toroidal velocity (average over charge states when multiple charge states are considered), given on 
  type (ids_generic_grid_scalar),pointer :: velocity_pol(:) => null()  ! /velocity_pol(i) - Poloidal velocity (average over charge states when multiple charge states are considered), given on 
  type (ids_generic_grid_scalar),pointer :: velocity_diamagnetic(:) => null()  ! /velocity_diamagnetic(i) - Velocity in the diamagnetic direction (average over charge states when multiple charge states are co
  type (ids_generic_grid_scalar),pointer :: energy_density_kinetic(:) => null()  ! /energy_density_kinetic(i) - Kinetic energy density (sum over states when multiple states are considered), given on various subgr
  integer  :: multiple_charge_states_flag=-999999999       ! /multiple_charge_states_flag - Multiple charge state calculation flag : 0-Only one charge state is considered; 1-Multiple charge st
  type (ids_edge_profiles_time_slice_ion_charge_state),pointer :: state(:) => null()  ! /state(i) - Quantities related to the different states of the species (ionisation, energy, excitation, ...)
endtype

type ids_edge_profiles_time_slice_electrons  !    Quantities related to electrons
  type (ids_generic_grid_scalar),pointer :: temperature(:) => null()  ! /temperature(i) - Temperature, given on various subgrids
  type (ids_generic_grid_scalar),pointer :: density(:) => null()  ! /density(i) - Density (thermal+non-thermal), given on various subgrids
  type (ids_generic_grid_scalar),pointer :: density_fast(:) => null()  ! /density_fast(i) - Density of fast (non-thermal) particles, given on various subgrids
  type (ids_generic_grid_scalar),pointer :: pressure(:) => null()  ! /pressure(i) - Pressure, given on various subgrids
  type (ids_generic_grid_scalar),pointer :: pressure_fast_perpendicular(:) => null()  ! /pressure_fast_perpendicular(i) - Fast (non-thermal) perpendicular pressure, given on various subgrids
  type (ids_generic_grid_scalar),pointer :: pressure_fast_parallel(:) => null()  ! /pressure_fast_parallel(i) - Fast (non-thermal) parallel pressure, given on various subgrids
  type (ids_generic_grid_scalar),pointer :: velocity_tor(:) => null()  ! /velocity_tor(i) - Toroidal velocity, given on various subgrids
  type (ids_generic_grid_scalar),pointer :: velocity_pol(:) => null()  ! /velocity_pol(i) - Poloidal velocity, given on various subgrids
  type (ids_generic_grid_scalar),pointer :: velocity_diamagnetic(:) => null()  ! /velocity_diamagnetic(i) - Velocity in the diamagnetic direction, given on various subgrids
  type (ids_generic_grid_scalar),pointer :: distribution_function(:) => null()  ! /distribution_function(i) - Distribution function, given on various subgrids
endtype

type ids_edge_profiles_time_slice  !    edge plasma description for a given time slice
  type (ids_generic_grid_dynamic) :: grid  ! /grid - Grid description
  type (ids_edge_profiles_time_slice_electrons) :: electrons  ! /electrons - Quantities related to the electrons
  type (ids_edge_profiles_time_slice_ion),pointer :: ion(:) => null()  ! /ion(i) - Quantities related to the different ion and neutral species
  type (ids_generic_grid_scalar),pointer :: t_i_average(:) => null()  ! /t_i_average(i) - Ion temperature (averaged on charge states and ion species), given on various subgrids
  type (ids_generic_grid_scalar),pointer :: n_i_total_over_n_e(:) => null()  ! /n_i_total_over_n_e(i) - Ratio of total ion density (sum over species and charge states) over electron density. (thermal+non-
  type (ids_generic_grid_scalar),pointer :: zeff(:) => null()  ! /zeff(i) - Effective charge, given on various subgrids
  type (ids_generic_grid_scalar),pointer :: pressure_thermal(:) => null()  ! /pressure_thermal(i) - Thermal pressure (electrons+ions), given on various subgrids
  type (ids_generic_grid_scalar),pointer :: pressure_perpendicular(:) => null()  ! /pressure_perpendicular(i) - Total perpendicular pressure (electrons+ions, thermal+non-thermal), given on various subgrids
  type (ids_generic_grid_scalar),pointer :: pressure_parallel(:) => null()  ! /pressure_parallel(i) - Total parallel pressure (electrons+ions, thermal+non-thermal), given on various subgrids
  type (ids_edge_profiles_time_slice_vector_components) :: j_anomalous  ! /j_anomalous - Anomalous current density
  type (ids_edge_profiles_time_slice_vector_components) :: j_inertial  ! /j_inertial - Inertial current density
  type (ids_edge_profiles_time_slice_vector_components) :: j_ion_neutral_friction  ! /j_ion_neutral_friction - Current density due to ion neutral friction
  type (ids_edge_profiles_time_slice_vector_components) :: j_parallel_viscosity  ! /j_parallel_viscosity - Current density due to the parallel viscosity
  type (ids_edge_profiles_time_slice_vector_components) :: j_perpendicular_viscosity  ! /j_perpendicular_viscosity - Current density due to the perpendicular viscosity
  type (ids_edge_profiles_time_slice_vector_components) :: j_heat_viscosity  ! /j_heat_viscosity - Current density due to the heat viscosity
  type (ids_edge_profiles_time_slice_vector_components) :: j_pfirsch_schlueter  ! /j_pfirsch_schlueter - Current density due to Pfirsch-Schlüter effects
  type (ids_edge_profiles_time_slice_vector_components) :: j_diamagnetic  ! /j_diamagnetic - Current density due to diamagnetic drift
  type (ids_generic_grid_scalar),pointer :: conductivity_parallel(:) => null()  ! /conductivity_parallel(i) - Parallel conductivity, given on various subgrids
  type (ids_generic_grid_scalar),pointer :: e_field_parallel(:) => null()  ! /e_field_parallel(i) - Parallel electric field = average(E.B) / B0, where edge_Profiles/Vacuum_Toroidal_Field/ B0, given on
  type (ids_generic_grid_scalar),pointer :: connection_length(:) => null()  ! /connection_length(i) - Total length of the flux tube, given on various subgrids
  type (ids_generic_grid_scalar),pointer :: distance_along_field_line(:) => null()  ! /distance_along_field_line(i) - Distance to closest wall intersection with the flux tube, along the field line (in SOL) and equals t
  real(DP)  :: time=-9.0D40       ! /time - Time
endtype

type ids_edge_profiles  !    Edge plasma profiles (includes the scrape-off layer and possibly part of the confined plasma)
  type (ids_ids_properties) :: ids_properties  ! /edge_profiles/ids_properties - 
  type (ids_edge_profiles_time_slice),pointer :: profiles_ggd(:) => null()  ! /edge_profiles/profiles_ggd(i) - edge plasma quantities represented using the general grid description, for various time slices
  type (ids_code) :: code  ! /edge_profiles/code - 
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
  type (ids_rz0d_dynamic_aos) :: geometric_axis  ! /geometric_axis - RZ position of the geometric axis (defined as (Rmin+Rmax) / 2 and (Zmin+Zmax) / 2 of the boundary)
  real(DP)  :: minor_radius=-9.0D40       ! /minor_radius - Minor radius of the plasma boundary (defined as (Rmax-Rmin) / 2 of the boundary)
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

type ids_equilibrium_global_quantities_magnetic_axis  !    R, Z, and Btor at magnetic axis, dynamic within a type 3 array of structure (index on time)
  real(DP)  :: r=-9.0D40       ! /r - Major radius of the magnetic axis
  real(DP)  :: z=-9.0D40       ! /z - Height of the magnetic axis
  real(DP)  :: b_tor=-9.0D40       ! /b_tor - Total toroidal magnetic field at the magnetic axis
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
  type (ids_equilibrium_global_quantities_magnetic_axis) :: magnetic_axis  ! /magnetic_axis - Magnetic axis position and toroidal field
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

! ***********  Include interfero_polarimeter/dd_interfero_polarimeter.xsd
! SPECIAL STRUCTURE data / time
type ids_interfero_polarimeter_channel_wavelength_interf_n_e_line  !    Line integrated density estimated from this wavelength
  real(DP), pointer  :: data(:) => null()     ! /n_e_line - Line integrated density estimated from this wavelength
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_interfero_polarimeter_channel_wavelength_interf  !    Value of the wavelength and density estimators associated to an interferometry wavelength
  real(DP)  :: value=-9.0D40       ! /value - Wavelength value
  type (ids_interfero_polarimeter_channel_wavelength_interf_n_e_line) :: n_e_line  ! /n_e_line - Line integrated density estimated from this wavelength
endtype

! SPECIAL STRUCTURE data / time
type ids_interfero_polarimeter_channel_n_e_line  !    Line integrated density, possibly obtained by a combination of multiple interferometry wavelengths
  real(DP), pointer  :: data(:) => null()     ! /n_e_line - Line integrated density, possibly obtained by a combination of multiple interferometry wavelengths
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_interfero_polarimeter_channel_faraday_angle  !    Faraday angle (variation of the Faraday angle induced by crossing the plasma) 
  real(DP), pointer  :: data(:) => null()     ! /faraday_angle - Faraday angle (variation of the Faraday angle induced by crossing the plasma) 
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_interfero_polarimeter_channel_ellipticity  !    Ellipticity
  real(DP), pointer  :: data(:) => null()     ! /ellipticity - Ellipticity
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_interfero_polarimeter_channel_validity_timed  !    Indicator of the validity of the channel as a function of time (0 means valid, negative values mean non-valid)
  integer, pointer  :: data(:) => null()      ! /validity_timed - Indicator of the validity of the channel as a function of time (0 means valid, negative values mean 
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_interfero_polarimeter_channel  !    Charge exchange channel
  character(len=132), dimension(:), pointer ::name => null()       ! /name - Name of the channel
  character(len=132), dimension(:), pointer ::identifier => null()       ! /identifier - ID of the channel
  type (ids_line_of_sight_3points) :: line_of_sight  ! /line_of_sight - Description of the line of sight of the channel, defined by two points when the beam is not reflecte
  real(DP)  :: wavelength_polarimetry=-9.0D40       ! /wavelength_polarimetry - Wavelength used for polarimetry
  type (ids_interfero_polarimeter_channel_wavelength_interf),pointer :: wavelength_interferometry(:) => null()  ! /wavelength_interferometry(i) - Set of wavelengths used for interferometry
  real(DP)  :: polarisation_initial=-9.0D40       ! /polarisation_initial - Initial polarisation vector before entering the plasma
  real(DP)  :: ellipticity_initial=-9.0D40       ! /ellipticity_initial - Initial ellipticity before entering the plasma
  type (ids_interfero_polarimeter_channel_n_e_line) :: n_e_line  ! /n_e_line - Line integrated density, possibly obtained by a combination of multiple interferometry wavelengths
  type (ids_interfero_polarimeter_channel_faraday_angle) :: faraday_angle  ! /faraday_angle - Faraday angle (variation of the Faraday angle induced by crossing the plasma) 
  type (ids_interfero_polarimeter_channel_ellipticity) :: ellipticity  ! /ellipticity - Ellipticity
  type (ids_interfero_polarimeter_channel_validity_timed) :: validity_timed  ! /validity_timed - Indicator of the validity of the channel as a function of time (0 means valid, negative values mean 
  integer  :: validity=-999999999       ! /validity - Indicator of the validity of the channel for the whole acquisition period (0 means valid, negative v
endtype

type ids_interfero_polarimeter  !    Interfero-polarimeter diagnostic
  type (ids_ids_properties) :: ids_properties  ! /interfero_polarimeter/ids_properties - 
  type (ids_interfero_polarimeter_channel),pointer :: channel(:) => null()  ! /interfero_polarimeter/channel(i) - Set of channels (lines-of-sight)
  type (ids_code) :: code  ! /interfero_polarimeter/code - 
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
type ids_magnetics_method_ip  !    Plasma current. Positive sign means anti-clockwise when viewed from above.
  real(DP), pointer  :: data(:) => null()     ! /ip - Plasma current. Positive sign means anti-clockwise when viewed from above.
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_magnetics_method_diamagnetic_flux  !    Diamagnetic flux
  real(DP), pointer  :: data(:) => null()     ! /diamagnetic_flux - Diamagnetic flux
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_magnetics_method  !    Processed quantities derived from the magnetic measurements, using various methods
  character(len=132), dimension(:), pointer ::name => null()       ! /name - Name of the data processing method
  type (ids_magnetics_method_ip) :: ip  ! /ip - Plasma current. Positive sign means anti-clockwise when viewed from above.
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

! ***********  Include mhd_linear/dd_mhd_linear.xsd
type ids_mhd_linear_vector  !    Vector structure for the MHD IDS
  real(DP),pointer  :: coordinate1(:,:) => null()     ! /coordinate1 - First coordinate (radial)
  real(DP),pointer  :: coordinate2(:,:) => null()     ! /coordinate2 - Second coordinate (poloidal)
  real(DP),pointer  :: coordinate3(:,:) => null()     ! /coordinate3 - Third coordinate (toroidal)
endtype

type ids_mhd_linear_time_slice_toroidal_mode_vaccuum  !    MHD modes in the vaccuum
  type (ids_identifier) :: poloidal_grid_type  ! /poloidal_grid_type - Selection of one of a set of grid types. 1: poloidal mode number; 2: poloidal angle
  integer,pointer  :: poloidal_grid(:,:) => null()     ! /poloidal_grid - Array of poloidal grid values for the various radial positions
  type (ids_mhd_linear_vector) :: a_perturbed  ! /a_perturbed - Pertubed vector potential for given toroidal mode number
  type (ids_mhd_linear_vector) :: b_perturbed  ! /b_perturbed - Pertubed magnetic field for given toroidal mode number
endtype

type ids_mhd_linear_time_slice_toroidal_mode_plasma  !    MHD modes in the confined plasma
  type (ids_identifier) :: poloidal_grid_type  ! /poloidal_grid_type - Selection of one of a set of grid types. 1: poloidal mode number; 2: poloidal angle
  integer,pointer  :: poloidal_grid(:,:) => null()     ! /poloidal_grid - Array of poloidal grid values for the various radial positions
  real(DP),pointer  :: displacement_perpendicular(:,:) => null()     ! /displacement_perpendicular - Perpendicular displacement of the modes
  real(DP),pointer  :: displacement_parallel(:,:) => null()     ! /displacement_parallel - Parallel displacement of the modes
  real(DP),pointer  :: tau_alfven(:) => null()     ! /tau_alfven - Alven time=R/vA=R0 sqrt(mi ni(rho))/B0
  real(DP),pointer  :: tau_resistive(:) => null()     ! /tau_resistive - Resistive time = mu_0 rho*rho/1.22/eta_neo
  type (ids_mhd_linear_vector) :: a_perturbed  ! /a_perturbed - Pertubed vector potential for given toroidal mode number
  type (ids_mhd_linear_vector) :: b_field_perturbed  ! /b_field_perturbed - Pertubed magnetic field for given toroidal mode number
  type (ids_mhd_linear_vector) :: velocity_perturbed  ! /velocity_perturbed - Pertubed velocity for given toroidal mode number
  real(DP),pointer  :: pressure_perturbed(:,:) => null()     ! /pressure_perturbed - Perturbed pressure for given toroidal mode number
  real(DP),pointer  :: mass_density_perturbed(:,:) => null()     ! /mass_density_perturbed - Perturbed mass density for given toroidal mode number
  real(DP),pointer  :: temperature_perturbed(:,:) => null()     ! /temperature_perturbed - Perturbed temperature for given toroidal mode number
endtype

type ids_mhd_linear_time_slice_toroidal_modes  !    Vector of toroidal modes
  integer  :: n_tor=-999999999       ! /n_tor - Toroidal mode number of the MHD mode
  real(DP)  :: growthrate=-9.0D40       ! /growthrate - Linear growthrate of the mode
  real(DP)  :: frequency=-9.0D40       ! /frequency - Frequency of the mode
  type (ids_mhd_linear_time_slice_toroidal_mode_plasma) :: plasma  ! /plasma - MHD modes in the confined plasma
  type (ids_mhd_linear_time_slice_toroidal_mode_vaccuum) :: vaccuum  ! /vaccuum - MHD modes in the vaccum
endtype

type ids_mhd_linear_time_slice  !    Time slice description of linear MHD stability
  type (ids_core_radial_grid) :: grid_plasma  ! /grid_plasma - Radial grid within plasma (for quantities below toroidal_mode/plasma)
  type (ids_mhd_linear_time_slice_toroidal_modes),pointer :: toroidal_mode(:) => null()  ! /toroidal_mode(i) - Vector of toroidal modes
  real(DP)  :: time=-9.0D40       ! /time - Time
endtype

type ids_mhd_linear  !    Magnetohydronamic linear stability
  type (ids_ids_properties) :: ids_properties  ! /mhd_linear/ids_properties - 
  type (ids_b_tor_vacuum_1) :: vacuum_toroidal_field  ! /mhd_linear/vacuum_toroidal_field - Characteristics of the vacuum toroidal field (used in rho_tor definition and in the normalization of
  type (ids_mhd_linear_time_slice),pointer :: time_slice(:) => null()  ! /mhd_linear/time_slice(i) - Core plasma radial profiles for various time slices
  type (ids_code) :: code  ! /mhd_linear/code - 
  real(DP), pointer  :: time(:) => null()  ! time
endtype

! ***********  Include nbi/dd_nbi.xsd
type ids_nbi_unit_beamlets_group_beamlet  !    Detailed information on beamlets
  type (ids_rzphi0d_static) :: position  ! /position - Position of beamlets
  real(DP)  :: tangency_radius=-9.0D40       ! /tangency_radius - Tangency radius (major radius where the central line of a beamlet is tangent to a circle around the 
  real(DP)  :: angle=-9.0D40       ! /angle - Angle of inclination between a line at the centre of a beamlet and the horiontal plane
  real(DP)  :: power_fraction=-9.0D40       ! /power_fraction - Fraction of power of a unit injected by a beamlet
endtype

type ids_nbi_unit_beamlets_group_divergence  !    Describes a divergence component of a group of beamlets
  real(DP)  :: particles_fraction=-9.0D40       ! /particles_fraction - Fraction of injected particles in the component
  real(DP)  :: vertical=-9.0D40       ! /vertical - The vertical beamlet divergence of the component. Here the divergence is defined for Gaussian beams 
  real(DP)  :: horizontal=-9.0D40       ! /horizontal - The horiztonal beamlet divergence of the component. Here the divergence is defined for Gaussian beam
endtype

type ids_nbi_unit_beamlets_group_focus  !    Describes of a group of beamlets is focused
  real(DP)  :: focal_length_horizontal=-9.0D40       ! /focal_length_horizontal - Horizontal focal length along the beam line, i.e. the point along the centre of the beamlet-group wh
  real(DP)  :: focal_length_vertical=-9.0D40       ! /focal_length_vertical - Vertical focal length along the beam line, i.e. the point along the centre of the beamlet-group wher
  real(DP)  :: width_min_horizontal=-9.0D40       ! /width_min_horizontal - The horizontal width of the beamlets group at the at the horizontal focal point
  real(DP)  :: width_min_vertical=-9.0D40       ! /width_min_vertical - The vertical width of the beamlets group at the at the vertical focal point
endtype

type ids_nbi_unit_beamlets_group  !    Group of beamlets
  type (ids_rzphi0d_static) :: position  ! /position - R, Z, Phi position of the coil centre
  real(DP)  :: tangency_radius=-9.0D40       ! /tangency_radius - Tangency radius (major radius where the central line of a NBI unit is tangent to a circle around the
  real(DP)  :: angle=-9.0D40       ! /angle - Angle of inclination between a beamlet at the centre of the injection unit surface and the horiontal
  integer  :: direction=-999999999       ! /direction - Direction of the beam seen from above the torus: -1 = clockwise; 1 = counter clockwise
  real(DP)  :: width_horizontal=-9.0D40       ! /width_horizontal - Horizontal width of the beam group at the injection unit surface (or grounded grid)
  real(DP)  :: width_vertical=-9.0D40       ! /width_vertical - Vertical width of the beam group at the injection unit surface (or grounded grid)
  type (ids_nbi_unit_beamlets_group_focus) :: focus  ! /focus - Describes how the beamlet group is focused
  type (ids_nbi_unit_beamlets_group_divergence),pointer :: divergence_component(:) => null()  ! /divergence_component(i) - Detailed information on beamlet divergence. Divergence is described as a superposition of Gaussian c
  type (ids_nbi_unit_beamlets_group_beamlet),pointer :: beamlet(:) => null()  ! /beamlet(i) - Detailed information on beamlets
endtype

! SPECIAL STRUCTURE data / time
type ids_nbi_unit_power  !    Power
  real(DP), pointer  :: data(:) => null()     ! /power - Power
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_nbi_unit_energy  !    Full energy of the injected species (acceleration of a single atom)
  real(DP), pointer  :: data(:) => null()     ! /energy - Full energy of the injected species (acceleration of a single atom)
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_nbi_unit_beam_current_fraction  !    Fractions of beam current distributed among the different energies, the first index corresponds to the fast neutrals energy (1:ful
  real(DP), pointer  :: data(:,:) => null()     ! /beam_current_fraction - Fractions of beam current distributed among the different energies, the first index corresponds to t
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_nbi_unit_beam_power_fraction  !    Fractions of beam power distributed among the different energies, the first index corresponds to the fast neutrals energy (1:full,
  real(DP), pointer  :: data(:,:) => null()     ! /beam_power_fraction - Fractions of beam power distributed among the different energies, the first index corresponds to the
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_nbi_unit  !    NBI unit
  character(len=132), dimension(:), pointer ::name => null()       ! /name - Name of the NBI unit
  character(len=132), dimension(:), pointer ::id => null()       ! /id - ID of the NBI unit
  type (ids_plasma_composition_species) :: species  ! /species - Injected species
  type (ids_nbi_unit_power) :: power  ! /power - Power
  type (ids_nbi_unit_energy) :: energy  ! /energy - Full energy of the injected species (acceleration of a single atom)
  type (ids_nbi_unit_beam_current_fraction) :: beam_current_fraction  ! /beam_current_fraction - Fractions of beam current distributed among the different energies, the first index corresponds to t
  type (ids_nbi_unit_beam_power_fraction) :: beam_power_fraction  ! /beam_power_fraction - Fractions of beam power distributed among the different energies, the first index corresponds to the
  type (ids_nbi_unit_beamlets_group),pointer :: beamlets_group(:) => null()  ! /beamlets_group(i) - Group of beamlets with common vertical and horizontal focal point. If there are no common focal poin
endtype

type ids_nbi  !    Neutral Beam Injection systems and description of the fast neutrals that arrive into the torus
  type (ids_ids_properties) :: ids_properties  ! /nbi/ids_properties - 
  type (ids_nbi_unit),pointer :: unit(:) => null()  ! /nbi/unit(i) - The NBI system is described as a set of units of which the power can be controlled individually.
  type (ids_code) :: code  ! /nbi/code - 
  real(DP), pointer  :: time(:) => null()  ! time
endtype

! ***********  Include ntms/dd_ntms.xsd
type ids_ntm_time_slice_mode_detailed_evolution_deltaw  !    deltaw contribution to the Rutherford equation (detailed evolution)
  real(DP),pointer  :: value(:) => null()     ! /value - Value of the contribution
  character(len=132), dimension(:), pointer ::name => null()       ! /name - Name of the contribution
endtype

type ids_ntm_time_slice_mode_detailed_evolution_torque  !    torque contribution to the Rutherford equation (detailed evolution)
  real(DP),pointer  :: value(:) => null()     ! /value - Value of the contribution
  character(len=132), dimension(:), pointer ::name => null()       ! /name - Name of the contribution
endtype

type ids_ntm_time_slice_mode_detailed_evolution  !    Detailed NTM evolution on a finer timebase than the time_slice array of structure
  real(DP),pointer  :: time_detailed(:) => null()     ! /time_detailed - Time array used to describe the detailed evolution of the NTM
  real(DP),pointer  :: width(:) => null()     ! /width - Full width of the mode
  real(DP),pointer  :: dwidth_dt(:) => null()     ! /dwidth_dt - Time derivative of the full width of the mode
  real(DP),pointer  :: phase(:) => null()     ! /phase - Phase of the mode
  real(DP),pointer  :: dphase_dt(:) => null()     ! /dphase_dt - Time derivative of the phase of the mode
  real(DP),pointer  :: frequency(:) => null()     ! /frequency - Frequency of the mode
  real(DP),pointer  :: dfrequency_dt(:) => null()     ! /dfrequency_dt - Time derivative of the frequency of the mode
  integer  :: n_tor=-999999999       ! /n_tor - Toroidal mode number
  integer  :: m_pol=-999999999       ! /m_pol - Poloidal mode number
  type (ids_ntm_time_slice_mode_detailed_evolution_deltaw),pointer :: deltaw(:) => null()  ! /deltaw(i) - deltaw contributions to the Rutherford equation
  type (ids_ntm_time_slice_mode_detailed_evolution_torque),pointer :: torque(:) => null()  ! /torque(i) - torque contributions to the Rutherford equation
  character(len=132), dimension(:), pointer ::calculation_method => null()       ! /calculation_method - Description of how the mode evolution is calculated
  real(DP),pointer  :: delta_diff(:,:) => null()     ! /delta_diff - Extra diffusion coefficient for the transport equations of Te, ne, Ti 
  real(DP),pointer  :: rho_tor_norm(:) => null()     ! /rho_tor_norm - Normalised flux coordinate on which the mode is centred
  real(DP),pointer  :: rho_tor(:) => null()     ! /rho_tor - Flux coordinate on which the mode is centred
endtype

type ids_ntm_time_slice_mode_evolution_deltaw  !    deltaw contribution to the Rutherford equation
  real(DP)  :: value=-9.0D40       ! /value - Value of the contribution
  character(len=132), dimension(:), pointer ::name => null()       ! /name - Name of the contribution
endtype

type ids_ntm_time_slice_mode_evolution_torque  !    torque contribution to the Rutherford equation
  real(DP)  :: value=-9.0D40       ! /value - Value of the contribution
  character(len=132), dimension(:), pointer ::name => null()       ! /name - Name of the contribution
endtype

type ids_ntm_time_slice_mode_onset  !    Onset characteristics of an NTM
  real(DP)  :: width=-9.0D40       ! /width - Seed island full width at onset time
  real(DP)  :: time_onset=-9.0D40       ! /time_onset - Onset time
  real(DP)  :: time_offset=-9.0D40       ! /time_offset - Offset time (when a mode disappears). If the mode reappears later in the simulation, use another ind
  real(DP)  :: phase=-9.0D40       ! /phase - Phase of the mode at onset
  integer  :: n_tor=-999999999       ! /n_tor - Toroidal mode number
  integer  :: m_pol=-999999999       ! /m_pol - Poloidal mode number
  character(len=132), dimension(:), pointer ::cause => null()       ! /cause - Cause of the mode onset
endtype

type ids_ntm_time_slice_mode  !    Description of an NTM
  type (ids_ntm_time_slice_mode_onset) :: onset  ! /onset - NTM onset characteristics
  real(DP)  :: width=-9.0D40       ! /width - Full width of the mode
  real(DP)  :: dwidth_dt=-9.0D40       ! /dwidth_dt - Time derivative of the full width of the mode
  real(DP)  :: phase=-9.0D40       ! /phase - Phase of the mode
  real(DP)  :: dphase_dt=-9.0D40       ! /dphase_dt - Time derivative of the phase of the mode
  real(DP)  :: frequency=-9.0D40       ! /frequency - Frequency of the mode
  real(DP)  :: dfrequency_dt=-9.0D40       ! /dfrequency_dt - Time derivative of the frequency of the mode
  integer  :: n_tor=-999999999       ! /n_tor - Toroidal mode number
  integer  :: m_pol=-999999999       ! /m_pol - Poloidal mode number
  type (ids_ntm_time_slice_mode_evolution_deltaw),pointer :: deltaw(:) => null()  ! /deltaw(i) - deltaw contributions to the Rutherford equation
  type (ids_ntm_time_slice_mode_evolution_torque),pointer :: torque(:) => null()  ! /torque(i) - torque contributions to the Rutherford equation
  character(len=132), dimension(:), pointer ::calculation_method => null()       ! /calculation_method - Description of how the mode evolution is calculated
  real(DP),pointer  :: delta_diff(:) => null()     ! /delta_diff - Extra diffusion coefficient for the transport equations of Te, ne, Ti 
  real(DP)  :: rho_tor_norm=-9.0D40       ! /rho_tor_norm - Normalised flux coordinate on which the mode is centred
  real(DP)  :: rho_tor=-9.0D40       ! /rho_tor - Flux coordinate on which the mode is centred
  type (ids_ntm_time_slice_mode_detailed_evolution) :: detailed_evolution  ! /detailed_evolution - Detailed NTM evolution on a finer timebase than the time_slice array of structure
endtype

type ids_ntm_time_slice  !    Time slice description of NTMs
  type (ids_ntm_time_slice_mode),pointer :: mode(:) => null()  ! /mode(i) - List of the various NTM modes appearing during the simulation. If a mode appears several times, use 
  real(DP)  :: time=-9.0D40       ! /time - Time
endtype

type ids_ntms  !    Description of neoclassical tearing modes
  type (ids_ids_properties) :: ids_properties  ! /ntms/ids_properties - 
  type (ids_b_tor_vacuum_1) :: vacuum_toroidal_field  ! /ntms/vacuum_toroidal_field - Characteristics of the vacuum toroidal field (used in rho_tor definition)
  type (ids_ntm_time_slice),pointer :: time_slice(:) => null()  ! /ntms/time_slice(i) - Description of neoclassical tearing modes for various time slices
  type (ids_code) :: code  ! /ntms/code - 
  real(DP), pointer  :: time(:) => null()  ! time
endtype

! ***********  Include pellets/dd_pellets.xsd
type ids_pellets_time_slice_pellet_shape  !    Initial shape of a pellet at launch
  type (ids_identifier) :: type  ! /type - Identifier structure for the shape type: 1-spherical; 2-cylindrical; 3-rectangular
  real(DP),pointer  :: size(:) => null()     ! /size - Size of the pellet in the various dimensions, depending on the shape type. Spherical pellets: size(1
endtype

type ids_pellets_time_slice_pellet_species  !    Species included in pellet compoisition
  real(DP)  :: a=-9.0D40       ! /a - Mass of atom
  real(DP)  :: z_n=-9.0D40       ! /z_n - Nuclear charge
  character(len=132), dimension(:), pointer ::label => null()       ! /label - String identifying the species (e.g. H, D, T, ...)
  real(DP)  :: density=-9.0D40       ! /density - Material density of the species in the pellet
  real(DP)  :: fraction=-9.0D40       ! /fraction - Fraction of the species atoms in the pellet
  real(DP)  :: sublimation_energy=-9.0D40       ! /sublimation_energy - Sublimation energy per atom
endtype

type ids_pellets_time_slice_pellet_path_profiles  !    1-D profiles of plasma and pellet along the pellet path
  real(DP),pointer  :: distance(:) => null()     ! /distance - Distance along the pellet path, with the origin taken at path_geometry/first_point. Used as the main
  real(DP),pointer  :: rho_tor_norm(:) => null()     ! /rho_tor_norm - Normalised toroidal coordinate along the pellet path 
  real(DP),pointer  :: psi(:) => null()     ! /psi - Poloidal flux along the pellet path 
  real(DP),pointer  :: velocity(:) => null()     ! /velocity - Pellet velocity along the pellet path 
  real(DP),pointer  :: n_e(:) => null()     ! /n_e - Electron density along the pellet path 
  real(DP),pointer  :: t_e(:) => null()     ! /t_e - Electron temperature along the pellet path 
  real(DP),pointer  :: ablation_rate(:) => null()     ! /ablation_rate - Ablation rate (electrons) along the pellet path 
  real(DP),pointer  :: ablated_particles(:) => null()     ! /ablated_particles - Number of ablated particles (electrons) along the pellet path 
  real(DP),pointer  :: rho_tor_norm_drift(:) => null()     ! /rho_tor_norm_drift - Difference to due ExB drifts between the ablation and the final deposition locations, in terms of th
  type (ids_rzphi1d_dynamic_aos3) :: position  ! /position - Position along the pellet path
endtype

type ids_pellets_time_slice_pellet  !    Description of a pellet
  type (ids_pellets_time_slice_pellet_shape) :: shape  ! /shape - Initial shape of a pellet at launch
  type (ids_pellets_time_slice_pellet_species),pointer :: species(:) => null()  ! /species(i) - Set of species included in the pellet composition
  real(DP)  :: velocity_initial=-9.0D40       ! /velocity_initial - Initial velocity of the pellet as it enters the vaccum chamber
  type (ids_line_of_sight_2points_dynamic_aos3) :: path_geometry  ! /path_geometry - Geometry of the pellet path in the vaccuum chamber
  type (ids_pellets_time_slice_pellet_path_profiles) :: path_profiles  ! /path_profiles - 1-D profiles of plasma and pellet along the pellet path 
endtype

type ids_pellets_time_slice  !    Time slice description of pellets
  type (ids_pellets_time_slice_pellet),pointer :: pellet(:) => null()  ! /pellet(i) - Set of pellets ablated in the plasma at a given time
  real(DP)  :: time=-9.0D40       ! /time - Time
endtype

type ids_pellets  !    Description of pellets launched into the plasma
  type (ids_ids_properties) :: ids_properties  ! /pellets/ids_properties - 
  type (ids_pellets_time_slice),pointer :: time_slice(:) => null()  ! /pellets/time_slice(i) - Description of the pellets launched at various time slices. The time of this structure corresponds t
  type (ids_code) :: code  ! /pellets/code - 
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

! ***********  Include sawteeth/dd_sawteeth.xsd
type ids_sawteeth_profiles_1d  !    Core profiles after sawtooth crash
  type (ids_core_radial_grid) :: grid  ! /grid - Radial grid
  real(DP),pointer  :: t_e(:) => null()     ! /t_e - Electron temperature
  real(DP),pointer  :: t_i_average(:) => null()     ! /t_i_average - Ion temperature (averaged on charge states and ion species)
  real(DP),pointer  :: n_e(:) => null()     ! /n_e - Electron density (thermal+non-thermal)
  real(DP),pointer  :: n_e_fast(:) => null()     ! /n_e_fast - Density of fast (non-thermal) electrons
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
  real(DP),pointer  :: phi(:) => null()     ! /phi - Toroidal flux
  real(DP),pointer  :: psi_star_pre_crash(:) => null()     ! /psi_star_pre_crash - Psi* = psi - phi, just before the sawtooth crash
  real(DP),pointer  :: psi_star_post_crash(:) => null()     ! /psi_star_post_crash - Psi* = psi - phi, after the sawtooth crash
  real(DP)  :: time=-9.0D40       ! /time - Time
endtype

type ids_sawteeth_diagnostics  !    Detailed information about the sawtooth characteristics
  real(DP),pointer  :: magnetic_shear_q1(:) => null()     ! /magnetic_shear_q1 - Magnetic shear at surface q = 1, defined as rho_tor/q . dq/drho_tor
  real(DP),pointer  :: rho_tor_norm_q1(:) => null()     ! /rho_tor_norm_q1 - Normalised toroidal flux coordinate at surface q = 1
  real(DP),pointer  :: rho_tor_norm_inversion(:) => null()     ! /rho_tor_norm_inversion - Normalised toroidal flux coordinate at inversion radius
  real(DP),pointer  :: rho_tor_norm_mixing(:) => null()     ! /rho_tor_norm_mixing - Normalised toroidal flux coordinate at mixing radius
endtype

type ids_sawteeth  !    Description of sawtooth events. This IDS must be used in homogeneous_time = 1 mode
  type (ids_ids_properties) :: ids_properties  ! /sawteeth/ids_properties - 
  type (ids_b_tor_vacuum_1) :: vacuum_toroidal_field  ! /sawteeth/vacuum_toroidal_field - Characteristics of the vacuum toroidal field (used in rho_tor definition)
  type (ids_sawteeth_profiles_1d),pointer :: profiles_1d(:) => null()  ! /sawteeth/profiles_1d(i) - Core profiles after sawtooth crash for various time slices
  type (ids_sawteeth_diagnostics) :: diagnostics  ! /sawteeth/diagnostics - Detailed information about the sawtooth characteristics
  type (ids_code) :: code  ! /sawteeth/code - 
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

! ***********  Include thomson_scattering/dd_thomson_scattering.xsd
! SPECIAL STRUCTURE data / time
type ids_thomson_scattering_channel_t_e  !    Electron temperature
  real(DP), pointer  :: data(:) => null()     ! /t_e - Electron temperature
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
! SPECIAL STRUCTURE data / time
type ids_thomson_scattering_channel_n_e  !    Electron density
  real(DP), pointer  :: data(:) => null()     ! /n_e - Electron density
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_thomson_scattering_channel  !    Thomson scattering channel
  character(len=132), dimension(:), pointer ::name => null()       ! /name - Name of the channel
  character(len=132), dimension(:), pointer ::identifier => null()       ! /identifier - ID of the channel
  type (ids_rzphi0d_static) :: position  ! /position - Position of the measurements (intersection between laser beam and line of sight)
  type (ids_thomson_scattering_channel_t_e) :: t_e  ! /t_e - Electron temperature
  type (ids_thomson_scattering_channel_n_e) :: n_e  ! /n_e - Electron density
endtype

type ids_thomson_scattering  !    Thomson scattering diagnostic
  type (ids_ids_properties) :: ids_properties  ! /thomson_scattering/ids_properties - 
  type (ids_thomson_scattering_channel),pointer :: channel(:) => null()  ! /thomson_scattering/channel(i) - Set of channels (lines-of-sight)
  type (ids_code) :: code  ! /thomson_scattering/code - 
  real(DP), pointer  :: time(:) => null()  ! time
endtype

! ***********  Include transport_solver_numerics/dd_transport_solver_numerics.xsd
type ids_numerics_profiles_1d_derivatives_charge_state_d  !    Quantities related to a given charge state, derivatives with respect to a given quantity
  real(DP),pointer  :: temperature(:) => null()     ! /temperature - Temperature
  real(DP),pointer  :: density(:) => null()     ! /density - Density (thermal+non-thermal)
  real(DP),pointer  :: density_fast(:) => null()     ! /density_fast - Density of fast (non-thermal) particles
  real(DP),pointer  :: pressure(:) => null()     ! /pressure - Pressure
  real(DP),pointer  :: pressure_fast_perpendicular(:) => null()     ! /pressure_fast_perpendicular - Fast (non-thermal) perpendicular pressure
  real(DP),pointer  :: pressure_fast_parallel(:) => null()     ! /pressure_fast_parallel - Fast (non-thermal) parallel pressure
  real(DP),pointer  :: velocity_tor(:) => null()     ! /velocity_tor - Toroidal velocity
  real(DP),pointer  :: velocity_pol(:) => null()     ! /velocity_pol - Poloidal velocity
endtype

type ids_numerics_profiles_1d_derivatives_charge_state  !    Quantities related to a given charge state
  real(DP)  :: z_min=-9.0D40       ! /z_min - Minimum Z of the charge state bundle
  real(DP)  :: z_max=-9.0D40       ! /z_max - Maximum Z of the charge state bundle (equal to z_min if no bundle)
  character(len=132), dimension(:), pointer ::label => null()       ! /label - String identifying charge state (e.g. C+, C+2 , C+3, C+4, C+5, C+6, ...)
  type (ids_numerics_profiles_1d_derivatives_charge_state_d) :: d_drho_tor_norm  ! /d_drho_tor_norm - First derivatives with respect to the normalised toroidal flux
  type (ids_numerics_profiles_1d_derivatives_charge_state_d) :: d2_drho_tor_norm2  ! /d2_drho_tor_norm2 - Second derivatives with respect to the normalised toroidal flux
  type (ids_numerics_profiles_1d_derivatives_charge_state_d) :: d_dt  ! /d_dt - First derivatives with respect to time
endtype

type ids_numerics_profiles_1d_derivatives_ion_d  !    Quantities related to an ion species, derivatives with respect to a given quantity
  real(DP),pointer  :: temperature(:) => null()     ! /temperature - Temperature (average over charge states when multiple charge states are considered)
  real(DP),pointer  :: density(:) => null()     ! /density - Density (thermal+non-thermal) (sum over charge states when multiple charge states are considered)
  real(DP),pointer  :: density_fast(:) => null()     ! /density_fast - Density of fast (non-thermal) particles (sum over charge states when multiple charge states are cons
  real(DP),pointer  :: pressure(:) => null()     ! /pressure - Pressure (average over charge states when multiple charge states are considered)
  real(DP),pointer  :: pressure_fast_perpendicular(:) => null()     ! /pressure_fast_perpendicular - Fast (non-thermal) perpendicular pressure  (average over charge states when multiple charge states a
  real(DP),pointer  :: pressure_fast_parallel(:) => null()     ! /pressure_fast_parallel - Fast (non-thermal) parallel pressure  (average over charge states when multiple charge states are co
  real(DP),pointer  :: velocity_tor(:) => null()     ! /velocity_tor - Toroidal velocity (average over charge states when multiple charge states are considered)
  real(DP),pointer  :: velocity_pol(:) => null()     ! /velocity_pol - Poloidal velocity (average over charge states when multiple charge states are considered)
endtype

type ids_numerics_profiles_1d_derivatives_ion  !    Quantities related to an ion species
  real(DP)  :: a=-9.0D40       ! /a - Mass of atom
  real(DP)  :: z_ion=-9.0D40       ! /z_ion - Ion charge (of the dominant ionisation state; lumped ions are allowed)
  real(DP)  :: z_n=-9.0D40       ! /z_n - Nuclear charge
  character(len=132), dimension(:), pointer ::label => null()       ! /label - String identifying ion (e.g. H+, D+, T+, He+2, C+, ...)
  type (ids_numerics_profiles_1d_derivatives_ion_d) :: d_drho_tor_norm  ! /d_drho_tor_norm - First derivatives with respect to the normalised toroidal flux
  type (ids_numerics_profiles_1d_derivatives_ion_d) :: d2_drho_tor_norm2  ! /d2_drho_tor_norm2 - Second derivatives with respect to the normalised toroidal flux
  type (ids_numerics_profiles_1d_derivatives_ion_d) :: d_dt  ! /d_dt - First derivatives with respect to time
  integer  :: multiple_charge_states_flag=-999999999       ! /multiple_charge_states_flag - Multiple charge state calculation flag : 0-Only one charge state is considered; 1-Multiple charge st
  type (ids_numerics_profiles_1d_derivatives_charge_state),pointer :: charge_state(:) => null()  ! /charge_state(i) - Quantities related to the different charge states of the ion species
endtype

type ids_numerics_profiles_1d_derivatives_electrons_d  !    Quantities related to electrons, derivatives with respect to a given quantity
  real(DP),pointer  :: temperature(:) => null()     ! /temperature - Temperature
  real(DP),pointer  :: density(:) => null()     ! /density - Density (thermal+non-thermal)
  real(DP),pointer  :: density_fast(:) => null()     ! /density_fast - Density of fast (non-thermal) particles
  real(DP),pointer  :: pressure(:) => null()     ! /pressure - Pressure
  real(DP),pointer  :: pressure_fast_perpendicular(:) => null()     ! /pressure_fast_perpendicular - Fast (non-thermal) perpendicular pressure
  real(DP),pointer  :: pressure_fast_parallel(:) => null()     ! /pressure_fast_parallel - Fast (non-thermal) parallel pressure
  real(DP),pointer  :: velocity_tor(:) => null()     ! /velocity_tor - Toroidal velocity
  real(DP),pointer  :: velocity_pol(:) => null()     ! /velocity_pol - Poloidal velocity
endtype

type ids_numerics_profiles_1d_derivatives_electrons  !    Quantities related to electrons
  type (ids_numerics_profiles_1d_derivatives_electrons_d) :: d_drho_tor_norm  ! /d_drho_tor_norm - First derivatives with respect to the normalised toroidal flux
  type (ids_numerics_profiles_1d_derivatives_electrons_d) :: d2_drho_tor_norm2  ! /d2_drho_tor_norm2 - Second derivatives with respect to the normalised toroidal flux
  type (ids_numerics_profiles_1d_derivatives_electrons_d) :: d_dt  ! /d_dt - First derivatives with respect to time
endtype

type ids_numerics_profiles_1d_derivatives_total_ions  !    Quantities related to total ion quantities, derivatives with respect to a given quantity
  real(DP),pointer  :: n_i_total_over_n_e(:) => null()     ! /n_i_total_over_n_e - Ratio of total ion density (sum over species and charge states) over electron density. (thermal+non-
  real(DP),pointer  :: pressure_ion_total(:) => null()     ! /pressure_ion_total - Total thermal ion pressure
endtype

type ids_numerics_profiles_1d_derivatives  !    Radial profiles derivatives for a given time slice
  type (ids_core_radial_grid) :: grid  ! /grid - Radial grid
  type (ids_numerics_profiles_1d_derivatives_electrons) :: electrons  ! /electrons - Quantities related to the electrons
  type (ids_numerics_profiles_1d_derivatives_ion),pointer :: ion(:) => null()  ! /ion(i) - Quantities related to the different ion species
  type (ids_numerics_profiles_1d_derivatives_total_ions) :: d_drho_tor_norm  ! /d_drho_tor_norm - First derivatives of total ion quantities with respect to the normalised toroidal flux
  type (ids_numerics_profiles_1d_derivatives_total_ions) :: d2_drho_tor_norm2  ! /d2_drho_tor_norm2 - Second derivatives of total ion quantities with respect to the normalised toroidal flux
  type (ids_numerics_profiles_1d_derivatives_total_ions) :: d_dt  ! /d_dt - First derivatives of total ion quantities with respect to time
  real(DP),pointer  :: dpsi_dt_cphi(:) => null()     ! /dpsi_dt_cphi - Derivative of the poloidal flux profile with respect to time, at constant toroidal flux
  real(DP)  :: time=-9.0D40       ! /time - Time
endtype

type ids_numerics_bc_1d_current_diffusion  !    Boundary conditions for the current diffusion equation
  type (ids_identifier) :: identifier  ! /identifier - Identifier of the boundary condition type. ID = 1: poloidal flux; 2: ip; 3: loop voltage; 4: undefin
  real(DP),pointer  :: value(:) => null()     ! /value - Value of the boundary condition. For ID = 1 to 3, only the first position in the vector is used. For
  real(DP)  :: rho_tor_norm=-9.0D40       ! /rho_tor_norm - Position, in normalised toroidal flux, at which the boundary condition is imposed. Outside this posi
endtype

type ids_numerics_bc_1d_bc  !    Boundary conditions for a given transport equation
  type (ids_identifier) :: identifier  ! /identifier - Identifier of the boundary condition type. ID = 1: value of the field y; 2: radial derivative of the
  real(DP),pointer  :: value(:) => null()     ! /value - Value of the boundary condition. For ID = 1 to 4, only the first position in the vector is used. For
  real(DP)  :: rho_tor_norm=-9.0D40       ! /rho_tor_norm - Position, in normalised toroidal flux, at which the boundary condition is imposed. Outside this posi
endtype

type ids_numerics_bc_1d_electrons  !    Boundary conditions for the electron related transport equations
  type (ids_numerics_bc_1d_bc) :: particles  ! /particles - Boundary condition for the electron density equation (density if ID = 1) 
  type (ids_numerics_bc_1d_bc) :: energy  ! /energy - Boundary condition for the electron energy equation (temperature if ID = 1) 
endtype

type ids_numerics_bc_1d_ion_charge_state  !    Boundary conditions for a given charge state related transport equations
  real(DP)  :: z_min=-9.0D40       ! /z_min - Minimum Z of the charge state bundle
  real(DP)  :: z_max=-9.0D40       ! /z_max - Maximum Z of the charge state bundle
  character(len=132), dimension(:), pointer ::label => null()       ! /label - String identifying charge state (e.g. C+, C+2 , C+3, C+4, C+5, C+6, ...)
  type (ids_numerics_bc_1d_bc) :: particles  ! /particles - Boundary condition for the charge state density equation (density if ID = 1) 
  type (ids_numerics_bc_1d_bc) :: energy  ! /energy - Boundary condition for the charge state energy equation (temperature if ID = 1) 
endtype

type ids_numerics_bc_1d_ion  !    Boundary conditions for a given ion species related transport equations
  real(DP)  :: a=-9.0D40       ! /a - Mass of atom
  real(DP)  :: z_ion=-9.0D40       ! /z_ion - Ion charge (of the dominant ionisation state; lumped ions are allowed)
  real(DP)  :: z_n=-9.0D40       ! /z_n - Nuclear charge
  character(len=132), dimension(:), pointer ::label => null()       ! /label - String identifying ion (e.g. H+, D+, T+, He+2, C+, ...)
  type (ids_numerics_bc_1d_bc) :: particles  ! /particles - Boundary condition for the ion density equation (density if ID = 1) 
  type (ids_numerics_bc_1d_bc) :: energy  ! /energy - Boundary condition for the ion energy equation (temperature if ID = 1) 
  integer  :: multiple_charge_states_flag=-999999999       ! /multiple_charge_states_flag - Multiple charge state calculation flag : 0-Only one charge state is considered; 1-Multiple charge st
  type (ids_numerics_bc_1d_ion_charge_state),pointer :: charge_state(:) => null()  ! /charge_state(i) - Boundary conditions related to the different charge states of the ion species
endtype

type ids_numerics_bc_1d  !    Boundary conditions of radial transport equations for a given time slice
  type (ids_numerics_bc_1d_current_diffusion) :: current_diffusion  ! /current_diffusion - Boundary condition for the current diffusion equation.
  type (ids_numerics_bc_1d_electrons) :: electrons  ! /electrons - Quantities related to the electrons
  type (ids_numerics_bc_1d_ion),pointer :: ion(:) => null()  ! /ion(i) - Quantities related to the different ion species
  type (ids_numerics_bc_1d_bc) :: energy_ion_total  ! /energy_ion_total - Boundary condition for the ion total (sum over species) energy equation (temperature if ID = 1) 
  real(DP)  :: time=-9.0D40       ! /time - Time
endtype

type ids_numerics_convergence_equations_single  !    Convergence details of a given transport equation
  integer  :: iterations_n=-999999999       ! /iterations_n - Number of iterations carried out in the convergence loop
endtype

type ids_numerics_convergence_equations_ion_charge_state  !    Boundary conditions for a given charge state related transport equations
  real(DP)  :: z_min=-9.0D40       ! /z_min - Minimum Z of the charge state bundle
  real(DP)  :: z_max=-9.0D40       ! /z_max - Maximum Z of the charge state bundle
  character(len=132), dimension(:), pointer ::label => null()       ! /label - String identifying charge state (e.g. C+, C+2 , C+3, C+4, C+5, C+6, ...)
  type (ids_numerics_convergence_equations_single) :: particles  ! /particles - Convergence details of the charge state density equation
  type (ids_numerics_convergence_equations_single) :: energy  ! /energy - Convergence details of the charge state energy equation 
endtype

type ids_numerics_convergence_equations_ion  !    Convergence details of a given ion species related transport equations
  real(DP)  :: a=-9.0D40       ! /a - Mass of atom
  real(DP)  :: z_ion=-9.0D40       ! /z_ion - Ion charge (of the dominant ionisation state; lumped ions are allowed)
  real(DP)  :: z_n=-9.0D40       ! /z_n - Nuclear charge
  character(len=132), dimension(:), pointer ::label => null()       ! /label - String identifying ion (e.g. H+, D+, T+, He+2, C+, ...)
  type (ids_numerics_convergence_equations_single) :: particles  ! /particles - Convergence details of the  ion density equation
  type (ids_numerics_convergence_equations_single) :: energy  ! /energy - Convergence details of the ion energy equation
  integer  :: multiple_charge_states_flag=-999999999       ! /multiple_charge_states_flag - Multiple charge state calculation flag : 0-Only one charge state is considered; 1-Multiple charge st
  type (ids_numerics_convergence_equations_ion_charge_state),pointer :: charge_state(:) => null()  ! /charge_state(i) - Convergence details of the related to the different charge states transport equations
endtype

type ids_numerics_convergence_equations_electrons  !    Convergence details for the electron related equations
  type (ids_numerics_convergence_equations_single) :: particles  ! /particles - Convergence details of the electron density equation
  type (ids_numerics_convergence_equations_single) :: energy  ! /energy - Convergence details of the electron energy equation
endtype

type ids_numerics_convergence_equations  !    Convergence details of a given transport equation
  type (ids_numerics_convergence_equations_single) :: current_diffusion  ! /current_diffusion - Convergence details of the current diffusion equation
  type (ids_numerics_convergence_equations_electrons) :: electrons  ! /electrons - Quantities related to the electrons
  type (ids_numerics_convergence_equations_ion),pointer :: ion(:) => null()  ! /ion(i) - Quantities related to the different ion species
  type (ids_numerics_convergence_equations_single) :: energy_ion_total  ! /energy_ion_total - Convergence details of the ion total (sum over species) energy equation 
  real(DP)  :: time=-9.0D40       ! /time - Time
endtype

! SPECIAL STRUCTURE data / time
type ids_numerics_convergence_time_step  !    Internal time step used by the transport solver (assuming all transport equations are solved with the same time step)
  real(DP), pointer  :: data(:) => null()     ! /time_step - Internal time step used by the transport solver (assuming all transport equations are solved with th
  real(DP), pointer  :: time(:) => null()  ! time
endtype
 
type ids_numerics_convergence  !    Convergence details
  type (ids_numerics_convergence_time_step) :: time_step  ! /time_step - Internal time step used by the transport solver (assuming all transport equations are solved with th
  type (ids_numerics_convergence_equations),pointer :: equations(:) => null()  ! /equations(i) - Convergence details of the transport equations, for various time slices
endtype

type ids_transport_solver_numerics  !    Numerical quantities used by transport solvers and convergence details
  type (ids_ids_properties) :: ids_properties  ! /transport_solver_numerics/ids_properties - 
  type (ids_numerics_profiles_1d_derivatives),pointer :: derivatives_1d(:) => null()  ! /transport_solver_numerics/derivatives_1d(i) - Radial profiles derivatives for various time slices
  type (ids_numerics_bc_1d),pointer :: boundary_conditions_1d(:) => null()  ! /transport_solver_numerics/boundary_conditions_1d(i) - Boundary conditions of the radial transport equations for various time slices
  type (ids_numerics_convergence) :: convergence  ! /transport_solver_numerics/convergence - Convergence details
  type (ids_b_tor_vacuum_1) :: vacuum_toroidal_field  ! /transport_solver_numerics/vacuum_toroidal_field - Characteristics of the vacuum toroidal field (used in rho_tor definition and in the normalization of
  type (ids_code) :: code  ! /transport_solver_numerics/code - 
  real(DP), pointer  :: time(:) => null()  ! time
endtype

! ***********  Include wall/dd_wall.xsd
type ids_wall_global_quantitites  !    Simple 0D description of plasma-wall interaction
  real(DP),pointer  :: pumping_speed(:,:) => null()     ! /pumping_speed - Pumping speed for the various neutral species
  real(DP),pointer  :: gas_puff(:,:) => null()     ! /gas_puff - Gas puff for the various neutral species
  real(DP),pointer  :: wall_inventory(:,:) => null()     ! /wall_inventory - Wall inventory, i.e. cumulated exchange of neutral species between plasma and wall from t = 0, posit
  real(DP),pointer  :: recycling_coefficient(:,:) => null()     ! /recycling_coefficient - Recycling coefficient for the various neutral species
  real(DP),pointer  :: temperature(:) => null()     ! /temperature - Wall temperature
  real(DP),pointer  :: power_from_plasma(:) => null()     ! /power_from_plasma - Power flowing from the plasma to the wall
  real(DP),pointer  :: power_to_cooling(:) => null()     ! /power_to_cooling - Power to cooling systems
endtype

type ids_wall  !    Description of the torus wall and its interaction with the plasma
  type (ids_ids_properties) :: ids_properties  ! /wall/ids_properties - 
  type (ids_plasma_composition_neutral),pointer :: neutral(:) => null()  ! /wall/neutral(i) - List of considered neutral species. Should this be static (as now) or dynamic ?
  type (ids_wall_global_quantitites) :: global_quantities  ! /wall/global_quantities - Simple 0D description of plasma-wall interaction
  type (ids_code) :: code  ! /wall/code - 
  real(DP), pointer  :: time(:) => null()  ! time
endtype

end module

