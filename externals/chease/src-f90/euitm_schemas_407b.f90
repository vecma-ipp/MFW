! ITM FORTRAN 90 type definitions

! Version phase 4.07b,  generated 14/09/2009

module euITM_utilities    ! declare the set of types common to all sub-trees

integer, parameter, private :: DP=kind(1.0D0)

type type_codeparam  !    
  character(len=132), dimension(:), pointer ::codename => null()       ! /codeparam/codename - Name of the code
  character(len=132), dimension(:), pointer ::codeversion => null()       ! /codeparam/codeversion - Version of the code (as in the ITM repository)
  character(len=132), dimension(:), pointer ::parameters => null()       ! /codeparam/parameters - List of the code specific parameters, string expected to be in XML format.
  character(len=132), dimension(:), pointer ::output_diag => null()       ! /codeparam/output_diag - List of the code specific diagnostic/output, string expected to be in XML format.
  integer  :: output_flag=-999999999       ! /codeparam/output_flag - Output flag : 0 means the run is successful, other values meaning some difficulty has been encountered, the exact meaning is then 
endtype

type type_param  !    
  character(len=132), dimension(:), pointer ::parameters => null()       ! /param/parameters - Actual value of the code parameters (instance of coparam/parameters in XML format).
  character(len=132), dimension(:), pointer ::default_param => null()       ! /param/default_param - Default value of the code parameters (instance of coparam/parameters in XML format).
  character(len=132), dimension(:), pointer ::schema => null()       ! /param/schema - Code parameters schema.
endtype

type type_composition  !    
  real(DP),pointer  :: amn(:) => null()     ! /composition/amn - Atomic mass number (lumped ions are allowed); Vector (nion)
  real(DP),pointer  :: zn(:) => null()     ! /composition/zn - Nuclear charge (lumped ions are allowed); Vector (nion)
  real(DP),pointer  :: zion(:) => null()     ! /composition/zion - Ion charge (of the dominant ionisation state; lumped ions are allowed); Vector (nion)
  integer,pointer  :: imp_flag(:) => null()      ! /composition/imp_flag - Multiple charge state calculation flag : 0-Only one charge state is considered; 1-Multiple charge state are considered and are des
endtype

type type_whatref  !    
  character(len=132), dimension(:), pointer ::user => null()       ! /whatref/user - Name of the user if private data, public if public ITM database.
  character(len=132), dimension(:), pointer ::machine => null()       ! /whatref/machine - Name of the device
  integer  :: shot=-999999999       ! /whatref/shot - Shot number
  integer  :: run=-999999999       ! /whatref/run - Run number
  integer  :: occurrence=-999999999       ! /whatref/occurrence - Occurrence number of the CPO in the reference entry
endtype

type type_putinfo  !    Structure which is type independent, describing the data item
  character(len=132), dimension(:), pointer ::putmethod => null()       ! /putinfo/putmethod - Storage method for this data
  character(len=132), dimension(:), pointer ::putaccess => null()       ! /putinfo/putaccess - Instructions to access the data using this method
  character(len=132), dimension(:), pointer ::putlocation => null()       ! /putinfo/putlocation - Name of this data under this method
  character(len=132), dimension(:), pointer ::rights => null()       ! /putinfo/rights - Access rights to this data
endtype

type type_datainfo  !    
  character(len=132), dimension(:), pointer ::dataprovider => null()       ! /datainfo/dataprovider - Name of the actual data provider (the person who filled the data)
  character(len=132), dimension(:), pointer ::putdate => null()       ! /datainfo/putdate - Date at which the data has been put in the DB
  character(len=132), dimension(:), pointer ::source => null()       ! /datainfo/source - Exact reference of the data source (e.g. original reference in the native machine data base)
  character(len=132), dimension(:), pointer ::comment => null()       ! /datainfo/comment - Any additional comment
  integer  :: isref=-999999999       ! /datainfo/isref - 1 if the data can be found in the present data base entry; 2 if the data can be found in a parent data base entry; 0 if no data co
  type (type_whatref) :: whatref  ! /datainfo/whatref - 
  type (type_putinfo) :: putinfo  ! /datainfo/putinfo - 
endtype

type type_spot  !    
  real(DP),pointer  :: waist(:,:) => null()     ! /spot/waist - Waist for the spot ellipse [m], Matrix (nantenna,2). Time-dependent
  real(DP),pointer  :: angle(:) => null()     ! /spot/angle - Rotation angle for the spot ellipse [rd], Vector(nantenna). Time-dependent
endtype

type type_phaseellipse  !    
  real(DP),pointer  :: invcurvrad(:,:) => null()     ! /phaseellipse/invcurvrad - Inverse curvature radii for the phase ellipse [m-1], Matrix (nantenna,2). Time-dependent
  real(DP),pointer  :: angle(:) => null()     ! /phaseellipse/angle - Rotation angle for the phase ellipse [rd], Vector(nantenna). Time-dependent
endtype

type type_b0r0  !    Characteristics of the vacuum toroidal field, redundant with the toroidfield CPO, normalisation used by the ETS
  real(DP)  :: r0=-9.0D40       ! /r0 - Characteristic major radius of the device (used in publications, usually middle of the vessel at the equatorial midplane) [m]. Sca
  real(DP)  :: b0=-9.0D40       ! /b0 - Vacuum field at r0 [T]; Positive sign means anti-clockwise when viewed from above. Scalar. Time-dependent. 
endtype

type type_boundaryel  !    Structure for the boundary condition of core transport equations (electrons) Time-dependent;
  real(DP),pointer  :: value(:) => null()     ! /value - Value of the boundary condition (in case flag = 2). Unit depends on type, respectively [1-field, 2-field.m^-1, 3-m, 4-field.s^-1].
  character(len=132), dimension(:), pointer ::source => null()       ! /source - Source of the boundary condition (any comment describing its origin : code, path to diagnostic signals, massaging); String
  integer  :: type=-999999999       ! /type - Type of the boundary condition for the transport solver (in case flag = 2). 0- equation not solved; 1- value of the field y; 2-rad
  real(DP)  :: rho_tor=-9.0D40       ! /rho_tor - Position of the boundary condition (in terms of toroidal flux coordinate) for the transport solver [m]. Outside this boundary, the
endtype

type type_boundaryion  !    Structure for the boundary condition of core transport equations (ions) Time-dependent
  real(DP),pointer  :: value(:,:) => null()     ! /value - Value of the boundary condition (in case flag = 2). Unit depends on type, respectively [1-field, 2-field.m^-1, 3-m, 4-field.s^-1].
  character(len=132), dimension(:), pointer ::source => null()       ! /source - Source of the boundary condition (any comment describing its origin : code, path to diagnostic signals, massaging); Array of strin
  integer,pointer  :: type(:) => null()      ! /type - Type of the boundary condition for the transport solver (in case flag = 2). 0- equation not solved; 1- value of the field y; 2-rad
  real(DP),pointer  :: rho_tor(:) => null()     ! /rho_tor - Position of the boundary condition (in terms of toroidal flux coordinate) for the transport solver [m]. Outside this boundary, the
endtype

type type_entry_def  !    Structure defining a database entry
  character(len=132), dimension(:), pointer ::user => null()       ! /user - Name of the user if private data. Value should be ITM if stored in the official common ITM tree
  character(len=132), dimension(:), pointer ::machine => null()       ! /machine - Name of the device
  integer  :: shot=-999999999       ! /shot - Shot number
  integer  :: run=-999999999       ! /run - Run number
endtype

type type_exp0D  !    Structure for experimental time-dependent scalar signal
  real(DP)  :: value=-9.0D40       ! /value - Signal value; Time-dependent; Scalar
  real(DP)  :: abserror=-9.0D40       ! /abserror - Absolute error on signal; Time-dependent; Scalar
  real(DP)  :: relerror=-9.0D40       ! /relerror - Relative error on signal (normalised to signal value); Time-dependent; Scalar
endtype

type type_exp1D  !    Structure for experimental 1D signal
  real(DP),pointer  :: value(:) => null()     ! /value - Signal value; Time-dependent; Vector
  real(DP),pointer  :: abserror(:) => null()     ! /abserror - Absolute error on signal; Time-dependent; Vector
  real(DP),pointer  :: relerror(:) => null()     ! /relerror - Relative error on signal (normalised to signal value); Time-dependent; Vector
endtype

type type_exp2D  !    Structure for experimental 2D signal
  real(DP),pointer  :: value(:,:) => null()     ! /value - Signal value; Time-dependent; Matrix
  real(DP),pointer  :: abserror(:,:) => null()     ! /abserror - Absolute error on signal; Time-dependent; Matrix
  real(DP),pointer  :: relerror(:,:) => null()     ! /relerror - Relative error on signal (normalised to signal value); Time-dependent; Matrix
endtype

type type_offdiagel  !    Subtree containing the full transport matrix from a transport model, for the electrons. Time-dependent.
  real(DP),pointer  :: d_ni(:,:) => null()     ! /d_ni - Off-Diagonal term coupling ion density gradient to the transport equation [m.^2.s^-1]. Time-dependent. Matrix (nrho,nion)
  real(DP),pointer  :: d_ti(:,:) => null()     ! /d_ti - Off-Diagonal term coupling ion density gradient to the transport equation [m.^2.s^-1]. Time-dependent. Matrix (nrho,nion)
  real(DP),pointer  :: d_ne(:) => null()     ! /d_ne - Off-Diagonal term coupling electron density gradient to the transport equation [m.^2.s^-1]. Time-dependent. Vector (nrho)
  real(DP),pointer  :: d_te(:) => null()     ! /d_te - Off-Diagonal term coupling electron temperature gradient to the transport equation [m.^2.s^-1]. Time-dependent. Vector (nrho)
  real(DP),pointer  :: d_epar(:) => null()     ! /d_epar - Off-Diagonal term coupling parallel electric field to the transport equation [m.^2.s^-1]. Time-dependent. Vector (nrho)
  real(DP),pointer  :: d_mtor(:) => null()     ! /d_mtor - Off-Diagonal term coupling total toroidal momentum to the transport equation [m.^2.s^-1]. Time-dependent. Vector (nrho)
endtype

type type_offdiagion  !    Subtree containing the full transport matrix from a transport model, for the various ion species
  real(DP),pointer  :: d_ni(:,:,:) => null()     ! /d_ni - Off-Diagonal term coupling ion density gradient to the transport equation [m.^2.s^-1]. Time-dependent. Array3d (nrho,nion,nion)
  real(DP),pointer  :: d_ti(:,:,:) => null()     ! /d_ti - Off-Diagonal term coupling ion density gradient to the transport equation [m.^2.s^-1]. Time-dependent. Array3d (nrho,nion,nion)
  real(DP),pointer  :: d_ne(:,:) => null()     ! /d_ne - Off-Diagonal term coupling electron density gradient to the transport equation [m.^2.s^-1]. Time-dependent. Matrix (nrho,nion)
  real(DP),pointer  :: d_te(:,:) => null()     ! /d_te - Off-Diagonal term coupling electron temperature gradient to the transport equation [m.^2.s^-1]. Time-dependent. Matrix (nrho,nion)
  real(DP),pointer  :: d_epar(:,:) => null()     ! /d_epar - Off-Diagonal term coupling parallel electric field to the transport equation [m.^2.s^-1]. Time-dependent. Matrix (nrho,nion)
  real(DP),pointer  :: d_mtor(:,:) => null()     ! /d_mtor - Off-Diagonal term coupling total toroidal momentum to the transport equation [m.^2.s^-1]. Time-dependent. Matrix (nrho,nion)
endtype

type type_reduced  !    Structure for a reduced data signal (0D data)
  real(DP)  :: value=-9.0D40       ! /value - Data value; Real
  character(len=132), dimension(:), pointer ::source => null()       ! /source - Path to the source signal (diagnostic or genprof, from which to read all info on the signal); String
  real(DP)  :: time=-9.0D40       ! /time - Time (exact time slice used from the time array of the source signal); Real
endtype

type type_reggrid  !    Generic structure for a regular grid
  real(DP),pointer  :: dim1(:) => null()     ! /dim1 - First dimension values; Vector (ndim1) 
  real(DP),pointer  :: dim2(:) => null()     ! /dim2 - Second dimension values; Vector (ndim2) 
endtype

type type_rf_beam  !    Beam characteristics (RF wave description)
  type (type_spot) :: spot  ! /spot - Spot characteristics
  type (type_phaseellipse) :: phaseellipse  ! /phaseellipse - Phase ellipse characteristics of the spot
endtype

type type_rz0D  !    Structure for one (R,Z) position (0D)
  real(DP)  :: r=-9.0D40       ! /r - Major radius [m]
  real(DP)  :: z=-9.0D40       ! /z - Altitude [m]
endtype

type type_rz1D  !    Structure for list of R,Z positions (1D)
  real(DP),pointer  :: r(:) => null()     ! /r - Major radius [m]
  real(DP),pointer  :: z(:) => null()     ! /z - Altitude [m]
endtype

type type_rz1D_npoints  !    Structure for list of R,Z positions (1D)
  real(DP),pointer  :: r(:) => null()     ! /r - Major radius [m]. Vector(max_npoints). Time-dependent
  real(DP),pointer  :: z(:) => null()     ! /z - Altitude [m]. Vector(max_npoints). Time-dependent
  integer  :: npoints=-999999999       ! /npoints - Number of meaningful points in the above vectors at a given time slice. Time-dependent
endtype

type type_rz2D  !    Structure for list of R,Z positions (2D)
  real(DP),pointer  :: r(:,:) => null()     ! /r - Major radius [m]
  real(DP),pointer  :: z(:,:) => null()     ! /z - Altitude [m]
endtype

type type_rz3D  !    Structure for list of R,Z positions (3D)
  real(DP),pointer  :: r(:,:,:) => null()     ! /r - Major radius [m]
  real(DP),pointer  :: z(:,:,:) => null()     ! /z - Altitude [m]
endtype

type type_rzphi1D  !    Structure for list of R,Z,phi positions (1D)
  real(DP),pointer  :: r(:) => null()     ! /r - Major radius [m]
  real(DP),pointer  :: z(:) => null()     ! /z - Altitude [m]
  real(DP),pointer  :: phi(:) => null()     ! /phi - Toroidal angle [rad]
endtype

type type_rzphi2D  !    Structure for list of R,Z,phi positions (2D)
  real(DP),pointer  :: r(:,:) => null()     ! /r - Major radius [m]
  real(DP),pointer  :: z(:,:) => null()     ! /z - Altitude [m]
  real(DP),pointer  :: phi(:,:) => null()     ! /phi - Toroidal angle [rad]
endtype

type type_setup_line  !    Geometric description of the lines of sight for line integral diagnostic
   type (type_rzphi1D)  :: pivot_point  ! /pivot_point - Pivot point of each line of sight; Vector (nchords)
  real(DP),pointer  :: polchordang(:) => null()     ! /polchordang - Viewing angle in poloidal plane [rad]; 0 is directed towards low field side, pi is towards high field side. Positive is anti-clock
  real(DP),pointer  :: torchordang(:) => null()     ! /torchordang - Viewing angle in horizontal plane [rad]; positive is anti-clockwise when viewed from above. Vector (nchords)
   type (type_rzphi1D)  :: second_point  ! /second_point - Second point defining the line of sight together with the pivot_point. This data is redundant with polchordang and torchordang, bu
  integer  :: nchordpoints=-999999999       ! /nchordpoints - Number of points along the viewing chords (used for synthetic diagnostic signal reconstruction)
endtype

type type_source_ion  !    Subtree containing source terms for the various ion species
  real(DP),pointer  :: exp(:,:) => null()     ! /exp - Explicit source term [same unit as root quantity]. Time-dependent. Matrix (nrho,nion)
  real(DP),pointer  :: imp(:,:) => null()     ! /imp - Implicit source term [s^-1.m^-3]. Time-dependent. Matrix (nrho,nion)
endtype

type type_source_imp  !    Subtree containing source terms for the impurity species
  real(DP),pointer  :: exp(:,:,:) => null()     ! /exp - Explicit source term [same unit as root quantity]. Time-dependent. Array3d (nrho,nimp,max_nzimp)
  real(DP),pointer  :: imp(:,:,:) => null()     ! /imp - Implicit source term [s^-1.m^-3]. Time-dependent. Array3d (nrho,nimp,max_nzimp)
endtype

type type_source_el  !    Subtree containing source terms for electrons
  real(DP),pointer  :: exp(:) => null()     ! /exp - Explicit source term [same unit as root quantity]. Time-dependent. Vector (nrho)
  real(DP),pointer  :: imp(:) => null()     ! /imp - Implicit source term [s^-1.m^-3]. Time-dependent. Vector (nrho)
endtype

type type_transcoefion  !    Subtree containing transport coefficients from a transport model, for the various ion species, including the energy exchange term 
  real(DP),pointer  :: diff_eff(:,:) => null()     ! /diff_eff - Effective diffusivity [m^2.s^-1]. Time-dependent. Matrix (nrho,nion)
  real(DP),pointer  :: vconv_eff(:,:) => null()     ! /vconv_eff - Effective convection [m.s^-1]. Time-dependent. Matrix (nrho,nion)
  real(DP),pointer  :: exchange(:,:) => null()     ! /exchange - Ion to electron energy exchange [W.m^-3]. Time-dependent. Matrix(nrho,nion).
  real(DP),pointer  :: qgi(:,:) => null()     ! /qgi - Energy exchange term due to transport. [W.m^-3]. Time-dependent. Matrix (nrho,nion)
  real(DP),pointer  :: flux(:,:) => null()     ! /flux - Flux. Not used in transport equations [field.m.s^-1,.m^-3 if field is not a density itself]. Time-dependent. Matrix (nrho,nion)
   type (type_offdiagion)  :: off_diagonal  ! /off_diagonal - Details of the transport matrix, just for diagnostic (not used in transport equations). Time-dependent.
  integer  :: flag=-999999999       ! /flag - Flag describing the form of transport produced by the original model : 0- not calculated, 1- D and V, 2- flux, 3- full transport m
endtype

type type_transcoefel  !    Subtree containing transport coefficients from a transport model, for the electrons
  real(DP),pointer  :: diff_eff(:) => null()     ! /diff_eff - Effective diffusivity [m^2.s^-1]. Time-dependent. Vector (nrho)
  real(DP),pointer  :: vconv_eff(:) => null()     ! /vconv_eff - Effective convection [m.s^-1]. Time-dependent. Vector (nrho)
  real(DP),pointer  :: flux(:) => null()     ! /flux - Flux. Not used in transport equations [field.m.s^-1,.m^-3 if field is not a density itself]. Time-dependent. Vector (nrho)
   type (type_offdiagel)  :: off_diagonal  ! /off_diagonal - Details of the transport matrix, just for diagnostic (not used in transport equations). Time-dependent.
  integer  :: flag=-999999999       ! /flag - Flag describing the form of transport produced by the original model : 0- not calculated, 1- D and V, 2- flux, 3- full transport m
endtype

type type_transcoefvtor  !    Subtree containing transport coefficients from a transport model, for the various ion species
  real(DP),pointer  :: diff_eff(:,:) => null()     ! /diff_eff - Effective diffusivity [m^2.s^-1]. Time-dependent. Matrix (nrho,nion)
  real(DP),pointer  :: vconv_eff(:,:) => null()     ! /vconv_eff - Effective convection [m.s^-1]. Time-dependent. Matrix (nrho,nion)
  real(DP),pointer  :: flux(:,:) => null()     ! /flux - Flux. Not used in transport equations [field.m.s^-1,.m^-3 if field is not a density itself]. Time-dependent. Matrix (nrho,nion)
   type (type_offdiagion)  :: off_diagonal  ! /off_diagonal - Details of the transport matrix, just for diagnostic (not used in transport equations). Time-dependent.
  integer  :: flag=-999999999       ! /flag - Flag describing the form of transport produced by the original model : 0- not calculated, 1- D and V, 2- flux, 3- full transport m
endtype

type type_transcoefimp  !    Subtree containing transport coefficients from a transport model, for the various impurity species (multiple charge states)
  real(DP),pointer  :: diff_eff(:,:,:) => null()     ! /diff_eff - Effective diffusivity [m^2.s^-1]. Time-dependent. Array3d (nrho,nimp,max_nzimp)
  real(DP),pointer  :: vconv_eff(:,:,:) => null()     ! /vconv_eff - Effective convection [m.s^-1]. Time-dependent. Array3d (nrho,nimp,max_nzimp)
  real(DP),pointer  :: exchange(:,:,:) => null()     ! /exchange - Ion to electron energy exchange [W.m^-3]. Time-dependent. Array3d (nrho,nimp,max_nzimp)
  real(DP),pointer  :: flux(:,:,:) => null()     ! /flux - Flux. Not used in transport equations [field.m.s^-1,.m^-3 if field is not a density itself]. Time-dependent. Array3d (nrho,nimp,ma
  integer  :: flag=-999999999       ! /flag - Flag describing the form of transport produced by the original model : 0- not calculated, 1- D and V, 2- flux, 3- full transport m
endtype

type type_lineintegraldiag  !    General line integral diagnostic
  type (type_datainfo) :: datainfo  ! /datainfo - 
  character(len=132), dimension(:), pointer ::expression => null()       ! /expression - Formal expression for the line integral to be evaluated as a function of ne, ni, Te, Ti, Zeff, Br, Bz
   type (type_setup_line)  :: setup_line  ! /setup_line - Geometric description of the lines of sight
   type (type_exp1D)  :: measure  ! /measure - Measured value. Time-dependent; Vector (nchords)
  real(DP)  :: time=-9.0D40       ! /time - Time [s]; Time-dependent; Scalar
endtype

end module ! end of the utilities module

module     euitm_schemas       ! declaration of all CPOs

use euITM_utilities

integer, parameter, private :: DP=kind(1.0D0)

! ***********  Include antennas.xsd
type type_waveguides  !    
  integer,pointer  :: nwm_theta(:) => null()      ! /modules/waveguides/nwm_theta - Number of waveguides per module in the poloidal direction. Vector of integers (nantenna_lh).
  integer,pointer  :: nwm_phi(:) => null()      ! /modules/waveguides/nwm_phi - Number of waveguides per module in the toroidal direction. Vector of integers (nantenna_lh).
  integer,pointer  :: mask(:,:) => null()     ! /modules/waveguides/mask - Mask of passive and active waveguides for an internal module, Matrix of integers (nantenna_lh,max_nwm_phi)
  integer,pointer  :: npwbm_phi(:) => null()      ! /modules/waveguides/npwbm_phi - Number of passive waveguide between modules in the toroidal direction. Vector of integers (nantenna_lh).
  integer,pointer  :: npwe_phi(:) => null()      ! /modules/waveguides/npwe_phi - Number of passive waveguides on each antenna edge in the toroidal direction. Vector of integers (nantenna_lh).
  real(DP),pointer  :: sw_theta(:) => null()     ! /modules/waveguides/sw_theta - Spacing between poloidally neighboring waveguides [m], Vector (nantenna_lh)
  real(DP),pointer  :: hw_theta(:) => null()     ! /modules/waveguides/hw_theta - Height of waveguides in the poloidal direction [m], Vector (nantenna_lh)
  real(DP),pointer  :: bwa(:) => null()     ! /modules/waveguides/bwa - Width of active waveguides [m], Vector (nantenna_lh)
  real(DP),pointer  :: biwp(:) => null()     ! /modules/waveguides/biwp - Width of internal passive waveguides [m], Vector (nantenna_lh)
  real(DP),pointer  :: bewp(:) => null()     ! /modules/waveguides/bewp - Width of edge passive waveguides [m], Vector (nantenna_lh)
  real(DP),pointer  :: e_phi(:,:) => null()     ! /modules/waveguides/e_phi - Thickness between waveguides in the toroidal direction [m], Matrix (nantenna_lh,nthick_phi). Reminder : nthick_phi = nmp_phi*nwm_p
  real(DP),pointer  :: scl(:,:) => null()     ! /modules/waveguides/scl - Short circuit length for passive waveguides [m], Matrix (nantenna_lh,nshort_phi). Reminder : nshort _phi = nmp_phi* npwm_phi  + (n
endtype

type type_modules  !    
  integer,pointer  :: nma_theta(:) => null()      ! /modules/nma_theta - Number of modules per antenna in the poloidal direction. Vector of integers (nantenna_lh).
  integer,pointer  :: nma_phi(:) => null()      ! /modules/nma_phi - Number of modules per antenna in the toroidal direction. Vector of integers (nantenna_lh).
  real(DP),pointer  :: sm_theta(:) => null()     ! /modules/sm_theta - Spacing between poloidally neighboring modules [m], Vector (nantenna_lh)
  real(DP),pointer  :: amplitude(:,:,:) => null()     ! /modules/amplitude - Amplitude of the TE10 mode injected in the module [W], Array 3D (nantenna_lh,nma_phi,nm_theta). Time-dependent
  real(DP),pointer  :: phase(:,:,:) => null()     ! /modules/phase - Phase of the TE10 mode injected in the module [rd], Array 3D (nantenna_lh, nma_phi, nm_theta). Time-dependent
  type (type_waveguides) :: waveguides  ! /modules/waveguides - Waveguides description
endtype

type type_antennalh_setup  !    Detailed description of LH antennas
  type (type_modules) :: modules  ! /modules - Modules description
endtype

type type_launchangles  !    
  real(DP),pointer  :: alpha(:) => null()     ! /antennas/antenna_ec/launchangles/alpha - Poloidal launching angle between the horizontal plane and the poloidal component of the nominal beam centerline (trigonometric con
  real(DP),pointer  :: beta(:) => null()     ! /antennas/antenna_ec/launchangles/beta - Toroidal launching angle between the horizontal plane and the poloidal component of the nominal beam centerline (trigonometric con
endtype

type type_plasmaedge  !    
  integer,pointer  :: nmode(:) => null()      ! /antennas/antenna_lh/plasmaedge/nmode - Number of modes used for antenna/plasma coupling. Vector of integers (nantenna_lh).
  integer,pointer  :: npoints(:) => null()      ! /antennas/antenna_lh/plasmaedge/npoints - Number of points in the distance grid. Vector of integers (nantenna_lh).
  real(DP),pointer  :: distance(:,:) => null()     ! /antennas/antenna_lh/plasmaedge/distance - Grid for electron density, defined as the perpendicular distance to the antenna waveguide plane (the origin being described in the
  real(DP),pointer  :: density(:,:) => null()     ! /antennas/antenna_lh/plasmaedge/density - Electron density in front of the antenna [m^-3]. Matrix (nantenna_lh,max_npoints). Time-dependent.
endtype

type type_antenna_ec  !    
  character(len=132), dimension(:), pointer ::name => null()       ! /antennas/antenna_ec/name - Antenna name, Vector of strings (nantenna_ec)
  real(DP),pointer  :: frequency(:) => null()     ! /antennas/antenna_ec/frequency - Frequency [Hz], Vector (nantenna_ec)
   type (type_exp1D)  :: power  ! /antennas/antenna_ec/power - Power [W], Vector (nantenna_ec). Time-dependent
  integer,pointer  :: mode(:) => null()      ! /antennas/antenna_ec/mode - Incoming wave mode (+ or -1 for O/X mode), Vector of integers (nantenna_ec). Time-dependent
   type (type_rzphi1D)  :: position  ! /antennas/antenna_ec/position - Reference global position of the last mirror. Vectors (nantenna_ec). Time-dependent
  type (type_launchangles) :: launchangles  ! /antennas/antenna_ec/launchangles - Launching angles of the beam
   type (type_rf_beam)  :: beam  ! /antennas/antenna_ec/beam - Beam characteristics
endtype

type type_antenna_ic  !    
  character(len=132), dimension(:), pointer ::name => null()       ! /antennas/antenna_ic/name - Antenna name, Vector of strings (nantenna_ic)
  real(DP),pointer  :: frequency(:) => null()     ! /antennas/antenna_ic/frequency - Frequency [Hz], Vector (nantenna_ic)
   type (type_exp1D)  :: power  ! /antennas/antenna_ic/power - Power [W], Vector (nantenna_ic). Time-dependent
   type (type_rzphi1D)  :: position  ! /antennas/antenna_ic/position - Reference global antenna position. Vectors (nantenna_ic). Time-dependent
endtype

type type_antenna_lh  !    
  character(len=132), dimension(:), pointer ::name => null()       ! /antennas/antenna_lh/name - Antenna name, Vector of strings (nantenna_lh)
  real(DP),pointer  :: frequency(:) => null()     ! /antennas/antenna_lh/frequency - Frequency [Hz], Vector (nantenna_lh)
   type (type_exp1D)  :: power  ! /antennas/antenna_lh/power - Power [W], Vector (nantenna_lh). Time-dependent
  integer,pointer  :: mode(:) => null()      ! /antennas/antenna_lh/mode - Incoming wave mode (+ or -1 for slow/fast LH wave mode). Vector of integers (nantenna_lh). Time-dependent
   type (type_rzphi1D)  :: position  ! /antennas/antenna_lh/position - Reference global antenna position. Vectors (nantenna_lh). Time-dependent
   type (type_antennalh_setup)  :: setup  ! /antennas/antenna_lh/setup - Detailed description of LH antennas.
  type (type_plasmaedge) :: plasmaedge  ! /antennas/antenna_lh/plasmaedge - Plasma edge characteristics in front of the antenna.
   type (type_rf_beam)  :: beam  ! /antennas/antenna_lh/beam - Beam characteristics
endtype

type type_antennas  !    
  type (type_datainfo) :: datainfo  ! /antennas/datainfo - 
  type (type_antenna_ec) :: antenna_ec  ! /antennas/antenna_ec - Electron Cyclotron antennas
  type (type_antenna_ic) :: antenna_ic  ! /antennas/antenna_ic - Ion Cyclotron antennas
  type (type_antenna_lh) :: antenna_lh  ! /antennas/antenna_lh - Lower Hybrid antennas
  type (type_codeparam) :: codeparam  ! /antennas/codeparam - 
  real(DP)  :: time=-9.0D40       ! /antennas/time - Time [s]; Time-dependent; Scalar
endtype

! ***********  Include controllers.xsd
type type_statespace  !    
  character(len=132), dimension(:), pointer ::observable => null()       ! /controllers/statespace/observable - Name of the observable signals (for ABCD type description); List of signal names (as they appear in the data structure, subtrees d
  real(DP),pointer  :: A(:,:) => null()     ! /controllers/statespace/A - A matrix
  real(DP),pointer  :: B(:,:) => null()     ! /controllers/statespace/B - B matrix
  real(DP),pointer  :: C(:,:) => null()     ! /controllers/statespace/C - C matrix
  real(DP),pointer  :: D(:,:) => null()     ! /controllers/statespace/D - D matrix
  real(DP)  :: deltat=-9.0D40       ! /controllers/statespace/deltat - Discrete time sampling interval [s]; scalar
endtype

type type_transferpid  !    
  real(DP)  :: gp=-9.0D40       ! /controllers/transferpid/gp - Proportional gain. Scalar.
  real(DP)  :: gd=-9.0D40       ! /controllers/transferpid/gd - Derivative gain. Scalar.
  real(DP)  :: gi=-9.0D40       ! /controllers/transferpid/gi - Integral gain. Scalar.
endtype

type type_controllers  !    
  type (type_datainfo) :: datainfo  ! /controllers/datainfo - 
  character(len=132), dimension(:), pointer ::name => null()       ! /controllers/name - Name of this controller
  character(len=132), dimension(:), pointer ::purpose => null()       ! /controllers/purpose - Purpose of this controller
  character(len=132), dimension(:), pointer ::type => null()       ! /controllers/type - One of a known type of controllers
  character(len=132), dimension(:), pointer ::input => null()       ! /controllers/input - Name of the input signals; List of signal names (as they appear in the data structure, subtrees denoted by "/" as in a Unix path);
  character(len=132), dimension(:), pointer ::output => null()       ! /controllers/output - Name of the output signals (actuators); List of signal names (as they appear in the data structure, subtrees denoted by "/" as in 
  character(len=132), dimension(:), pointer ::descriptor => null()       ! /controllers/descriptor - Simulikn/Scicos controller description, in case the controller is implemented under such tools. String.
  type (type_statespace) :: statespace  ! /controllers/statespace - Statespace controller in discrete time, formally described as follows : Xn+1 = A*Xn+B*Un; and Yn = C*Xn+D*Un; X is the vector of o
  type (type_transferpid) :: transferpid  ! /controllers/transferpid - Transfer function used to define a PID controller
  real(DP),pointer  :: reference(:) => null()     ! /controllers/reference - Time-dependent reference value to be reached by the controller. The corresponding signal name is given by the "input" node; Time-d
  type (type_codeparam) :: codeparam  ! /controllers/codeparam - 
  real(DP)  :: time=-9.0D40       ! /controllers/time - Time [s]; Time-dependent; Scalar
endtype

! ***********  Include coredelta.xsd
type type_coredelta  !    
  type (type_datainfo) :: datainfo  ! /coredelta/datainfo - 
  type (type_composition) :: composition  ! /coredelta/composition - 
  real(DP),pointer  :: rho_tor(:) => null()     ! /coredelta/rho_tor - Toroidal flux coordinate (not normalised, equivalent to rho_tor_norm) [m];  Vector (nrho). Time-dependent.
  real(DP),pointer  :: rho_tor_norm(:) => null()     ! /coredelta/rho_tor_norm - Normalised toroidal flux coordinate values (= rho_tor normalised to the value at the last grid point); Vector (nrho)
  real(DP),pointer  :: delta_psi(:) => null()     ! /coredelta/delta_psi - Instant change of the poloidal flux [Wb]. Time-dependent. Vector(nrho).
  real(DP),pointer  :: delta_te(:) => null()     ! /coredelta/delta_te - Instant change of the electron temperature [eV]. Time-dependent. Vector(nrho). 
  real(DP),pointer  :: delta_ti(:,:) => null()     ! /coredelta/delta_ti - Instant change of the ion temperature [eV]. Time-dependent. Matrix (nrho,nion).
  real(DP),pointer  :: delta_tz(:,:,:) => null()     ! /coredelta/delta_tz - Instant change of the impurity (multiple charge states) temperature [eV]. Time-dependent. Array3d (nrho,nimp,max_nzimp).
  real(DP),pointer  :: delta_ne(:) => null()     ! /coredelta/delta_ne - Instant change of the electron density [m^-3]. Time-dependent. Vector(nrho).
  real(DP),pointer  :: delta_ni(:,:) => null()     ! /coredelta/delta_ni - Instant change of the ion density [m^-3]. Time-dependent. Matrix (nrho,nion).
  real(DP),pointer  :: delta_nz(:,:,:) => null()     ! /coredelta/delta_nz - Instant change of the impurity (multiple charge states) density [m^-3]. Time-dependent. Array3d (nrho,nimp,max_nzimp).
  real(DP),pointer  :: delta_vtor(:,:) => null()     ! /coredelta/delta_vtor - Instant change of the toroidal toroidal velocity [m.s^-1]. Time-dependent. Matrix (nrho,nion).
  type (type_codeparam) :: codeparam  ! /coredelta/codeparam - 
  real(DP)  :: time=-9.0D40       ! /coredelta/time - Time [s]; Time-dependent; Scalar
endtype

! ***********  Include coreimpur.xsd
type type_sourceimp  !    Structure for the total source term for the transport equation (impurities). Time-dependent;
  real(DP),pointer  :: value(:,:,:) => null()     ! /value - Value of the source term [m^-3.s^-1]; Time-dependent; Array3D (nrho,nimp,max_nzimp)
  real(DP),pointer  :: integral(:,:,:) => null()     ! /integral - Integral from 0 to rho of the source term. Time-dependent; Array3D(nsource,nimp,max_nzimp)
  character(len=132), dimension(:), pointer ::source => null()       ! /source - Source of the profile (any comment describing the origin of the impurity profiles : code, path to diagnostic signals, massaging, .
endtype

type type_coretransimp  !    Structure for the transport coefficients for the transport equation (impurities). Time-dependent;
  real(DP),pointer  :: diff(:,:,:) => null()     ! /diff - Diffusion coefficient [m^2.s^-1]. Time-dependent; Array3D(nrho,nimp,max_nzimp)
  real(DP),pointer  :: vconv(:,:,:) => null()     ! /vconv - Convection coefficient [m.s^-1]. Time-dependent; Array3D (nrho,nimp,max_nzimp)
  character(len=132), dimension(:), pointer ::source => null()       ! /source - Source of the profile (any comment describing the origin of the impurity profiles : code, path to diagnostic signals, massaging, .
endtype

type type_boundaryimp  !    Structure for the boundary condition of core transport equations (impurities) Time-dependent
  real(DP),pointer  :: value(:,:,:) => null()     ! /value - Value of the boundary condition (in case flag = 2). Unit depends on type, respectively [1-field, 2-field.m^-1, 3-m, 4-field.s^-1].
  character(len=132), dimension(:), pointer ::source => null()       ! /source - Source of the boundary condition (any comment describing its origin : code, path to diagnostic signals, massaging); Array of strin
  integer,pointer  :: type(:,:) => null()     ! /type - Type of the boundary condition for the transport solver (in case flag = 2). 0- equation not solved; 1- value of the field y; 2-rad
  real(DP),pointer  :: rho(:,:) => null()     ! /rho - Position of the boundary condition (in terms of toroidal flux coordinate) for the transport solver [m]. Outside this boundary, the
  type (type_codeparam) :: codeparam  ! /codeparam - 
endtype

type type_fluximp  !    Structure for the fluxes of a field of the core transport equations (impurities); Time-dependent;
  real(DP),pointer  :: flux_dv(:,:,:) => null()     ! /flux_dv - Flux of the field calculated from the transport coefficients. Time-dependent; Array3D (nrho,nion,max_nzimp)
  real(DP),pointer  :: flux_interp(:,:,:) => null()     ! /flux_interp - Interpretative flux deduced from measured data, the integral of the source term, and the time derivative of the field. Time-depend
endtype

type type_desc_impur  !    
  real(DP),pointer  :: amn(:) => null()     ! /coreimpur/desc_impur/amn - Atomic mass number of the impurity; Vector (nimp)
  integer,pointer  :: zn(:) => null()      ! /coreimpur/desc_impur/zn - Nuclear charge of the impurity; Vector (nimp)
  integer,pointer  :: i_ion(:) => null()      ! /coreimpur/desc_impur/i_ion - Index of the impurity species in the coreprof ion species ordering. Vector (nimp)
  integer,pointer  :: nzimp(:) => null()      ! /coreimpur/desc_impur/nzimp - Number of charge states (or bundles) considered for each impurity species. Vector (nimp)
  integer,pointer  :: zmin(:,:) => null()     ! /coreimpur/desc_impur/zmin - Minimum Z of impurity ionisation state bundle. Matrix (nimp,max_nzimp)
  integer,pointer  :: zmax(:,:) => null()     ! /coreimpur/desc_impur/zmax - Maximum Z of impurity ionisation state bundle. If no bundle, zmax=zmin. Matrix (nimp,max_nzimp)
endtype

type type_coreimpur  !    
  type (type_datainfo) :: datainfo  ! /coreimpur/datainfo - 
  real(DP),pointer  :: rho_tor_norm(:) => null()     ! /coreimpur/rho_tor_norm - Normalised toroidal flux coordinate values (= rho_tor normalised to the value at the last grid point); Vector (nrho)
  real(DP),pointer  :: rho_tor(:) => null()     ! /coreimpur/rho_tor - Toroidal flux coordinate (not normalised, equivalent to rho_tor_norm) [m];  Vector (nrho). Time-dependent.
  character(len=132), dimension(:), pointer ::source => null()       ! /coreimpur/source - Source of the profile (any comment describing the origin of the impurity profiles : code, path to diagnostic signals, massaging, .
  integer,pointer  :: flag(:) => null()      ! /coreimpur/flag - Flag describing how the profile has been processed : 0-not calculated; 1-interpretative; 2-calculated by the transport solver; 3-c
  type (type_desc_impur) :: desc_impur  ! /coreimpur/desc_impur - Description of the impurities and their charge states
  real(DP),pointer  :: z(:,:,:) => null()     ! /coreimpur/z - Impurity ionisation state (averaged for bundle); Time-dependent; Array3D (nrho,nimp,max_nzimp)
  real(DP),pointer  :: zsq(:,:,:) => null()     ! /coreimpur/zsq - Z^2, Square of impurity ionisation state (averaged for bundle); Time-dependent; Array3D (nrho,nimp,max_nzimp)
  real(DP),pointer  :: nz(:,:,:) => null()     ! /coreimpur/nz - Density of impurity in a given charge state [m^-3]. Time-dependent; Array3D (nrho,nimp,max_nzimp)
   type (type_sourceimp)  :: source_term  ! /coreimpur/source_term - Source term for each charge state. Time-dependent.
   type (type_boundaryimp)  :: boundary  ! /coreimpur/boundary - Boundary condition for each charge state. Time-dependent
   type (type_coretransimp)  :: transp_coef  ! /coreimpur/transp_coef - Transport coefficients for each charge state
   type (type_fluximp)  :: flux  ! /coreimpur/flux - Fluxes of impurity particles, two definitions [m^-2.s^-1]. Time-dependent.
  real(DP),pointer  :: time_deriv(:,:,:) => null()     ! /coreimpur/time_deriv - Integral of the time derivative term of the transport equation. Time-dependent. Array3D (nrho,nimp,max_nzimp)
  character(len=132), dimension(:), pointer ::atomic_data => null()       ! /coreimpur/atomic_data - Reference for the atomic data used for each impurity. Array of strings (nimp)
  real(DP)  :: time=-9.0D40       ! /coreimpur/time - Time [s]; Time-dependent; Scalar.
  type (type_codeparam) :: codeparam  ! /coreimpur/codeparam - 
endtype

! ***********  Include coreneutrals.xsd
type type_atomlist  !    
  real(DP),pointer  :: amn(:) => null()     ! /atomlist/amn - Atomic mass number; Vector (natm)
  real(DP),pointer  :: zn(:) => null()     ! /atomlist/zn - Nuclear charge; Vector (natm)
endtype

type type_neutrallist  !    
  integer,pointer  :: ncomp(:) => null()      ! /neutrallist/ncomp - For each neutral species, number of distinct atoms that enter the composition of this species (1 if the neutral is an atom, more f
  integer,pointer  :: tatm(:,:) => null()     ! /neutrallist/tatm - For each neutral species, and each of its atomic component, index of the atom (referring to the atomlist). Matrix of integers (nne
  integer,pointer  :: multatm(:,:) => null()     ! /neutrallist/multatm - For each neutral species, and each of its atomic component, number of such atoms. Matrix of integers (nneut,max_ncomp)
endtype

type type_typelist  !    
  integer,pointer  :: ntype(:) => null()      ! /typelist/ntype - For each neutral species, number of possible types considered (in terms of energy : cold, thermal, fast, NBI, ...). Vector of inte
  integer,pointer  :: type(:,:) => null()     ! /typelist/type - Type of neutral, in terms of energy : 0=cold, 1=thermal, 2= fast, 3=NBI. Matrix of integers (nneut,max_ntype)
endtype

type type_boundary_neutrals  !    Structure for the boundary condition of core transport equations (neutrals). Time-dependent;
  real(DP),pointer  :: value(:,:,:) => null()     ! /value - Value of the boundary condition. Unit depends on type, respectively [1-field, 2-field.m^-1, 3-m, 4-field.s^-1]. For type 1 to 4, o
  integer,pointer  :: type(:,:) => null()     ! /type - Type of the boundary condition for the transport solver. 0- equation not solved; 1- value of the field y; 2-radial derivative of t
  integer,pointer  :: rho_tor(:,:) => null()     ! /rho_tor - Position of the boundary condition (in terms of toroidal flux coordinate) for the transport solver [m]. Outside this boundary, the
endtype

type type_corefieldneutral  !    Structure for a main field of core neutral transport equations; Time-dependent;
  real(DP),pointer  :: value(:,:,:) => null()     ! /value - Signal value; Array3D(nrho,nneut,max_ntype). Time-dependent
  real(DP),pointer  :: flux(:,:,:) => null()     ! /flux - Net neutral flux through the magnetic surface, positive values correspond to the direction from the center to the edge [s^-1]. Arr
   type (type_boundary_neutrals)  :: boundary  ! /boundary - Boundary condition for the transport equation. Time-dependent.
endtype

type type_corefieldneutrale  !    Structure for a main field of core neutral transport equations, (Temperature, with flux as energy); Time-dependent;
  real(DP),pointer  :: value(:,:,:) => null()     ! /value - Signal value; Array3D(nrho,nneut,max_ntype). Time-dependent
  real(DP),pointer  :: flux(:,:,:) => null()     ! /flux - Net flux of the kinetic energy through the magnetic surface (3/2*E*n*V), positive values correspond to the direction from the cent
   type (type_boundary_neutrals)  :: boundary  ! /boundary - Boundary condition for the transport equation. Time-dependent.
endtype

type type_corefieldneutralv  !    Structure for a main field of core neutral transport equations (without flux variable); Time-dependent;
  real(DP),pointer  :: value(:,:,:) => null()     ! /value - Signal value; Array3D(nrho,nneut,max_ntype)Time-dependent; 
   type (type_boundary_neutrals)  :: boundary  ! /boundary - Boundary condition for the transport equation. Time-dependent.
endtype

type type_corefieldneutralv0  !    Neutral velocity
   type (type_corefieldneutralv)  :: toroidal  ! /toroidal - Neutral velocity in the toroidal direction [m.s^-1]. Positive is anti-clockwise when viewed from above. Time-dependent;
   type (type_corefieldneutralv)  :: poloidal  ! /poloidal - Velocity of neutrals in the poloidal direction. 0 is directed towards low field side, pi is towards high field side. Positive is a
   type (type_corefieldneutralv)  :: radial  ! /radial - Neutral velocity in the radial direction (perpendicular to the magnetic surface), positive is from the centre to the edge [m.s^-1]
endtype

type type_recycling_neutrals  !    Recycling coefficients
  real(DP),pointer  :: particles(:,:) => null()     ! /particles - Particle recycling coefficient corresponding to the conversion of ion type IION to the neutral type INEUT. Matrix(nneut,nion). Tim
  real(DP),pointer  :: energy(:,:) => null()     ! /energy - Energy recycling coefficient corresponding to the conversion of ion type IION to the neutral type INEUT. Matrix(nneut,nion). Time-
endtype

type type_sputtering_neutrals  !    Sputtering coefficients
  real(DP),pointer  :: physical(:,:) => null()     ! /physical - Effective coefficient of physical sputtering of the neutral type INEUT due to ion type IION. Matrix(nneut,nion). Time-dependent.
  real(DP),pointer  :: chemical(:,:) => null()     ! /chemical - Effective coefficient of chemical sputtering of the neutral type INEUT due to ion type IION. Matrix(nneut,nion). Time-dependent.
endtype

type type_composition_neutrals  !    Description of neutrals species
  type (type_atomlist) :: atomlist  ! /atomlist - List of the atoms that enter the composition of the neutral species
  type (type_neutrallist) :: neutrallist  ! /neutrallist - Definition of neutral species
  type (type_typelist) :: typelist  ! /typelist - Definition of types for each neutral species
endtype

type type_coefficients_neutrals  !    Recycling and sputtering coefficients used by the neutral solver. The nion index refers to the various ions (and charge states) co
   type (type_recycling_neutrals)  :: recycling  ! /recycling - Recycling coefficients
   type (type_sputtering_neutrals)  :: sputtering  ! /sputtering - Sputtering coefficients
endtype

type type_profiles_neutrals  !    Profiles derived from the fields solved in the transport equations, or from experiment.
   type (type_corefieldneutral)  :: n0  ! /n0 - Neutral density [m^-3].  Time-dependent;
   type (type_corefieldneutrale)  :: t0  ! /t0 - Neutral temperature [eV]. Time-dependent;
   type (type_corefieldneutralv0)  :: v0  ! /v0 - Neutral velocity
  real(DP),pointer  :: prad0(:,:) => null()     ! /prad0 - Power radiated by neutrals [W.m^-3]. Matrix (nrho,nneut). Time-dependent.
endtype

type type_coreneutrals  !    
  type (type_datainfo) :: datainfo  ! /coreneutrals/datainfo - 
  real(DP),pointer  :: rho_tor(:) => null()     ! /coreneutrals/rho_tor - Toroidal flux coordinate (not normalised, equivalent to rho_tor_norm) [m];  Vector (nrho). Time-dependent.
  real(DP),pointer  :: rho_tor_norm(:) => null()     ! /coreneutrals/rho_tor_norm - Normalised toroidal flux coordinate values (= rho_tor normalised to the value at the last grid point); Vector (nrho)
   type (type_composition_neutrals)  :: composition  ! /coreneutrals/composition - Description of neutrals species
   type (type_profiles_neutrals)  :: profiles  ! /coreneutrals/profiles - Profiles derived from the fields solved in the transport equations, or from experiment.
   type (type_coefficients_neutrals)  :: coefficients  ! /coreneutrals/coefficients - Recycling and sputtering coefficients used by the neutral solver. The nion index refers to the various ions (and charge states) co
  type (type_codeparam) :: codeparam  ! /coreneutrals/codeparam - 
  real(DP)  :: time=-9.0D40       ! /coreneutrals/time - Time [s]; Time-dependent; Scalar
endtype

! ***********  Include coreprof.xsd
type type_sourceel  !    Structure for the total source term for the transport equation (electrons). Time-dependent;
  real(DP),pointer  :: value(:) => null()     ! /value - Value of the source term; Time-dependent; Vector (nrho)
  real(DP),pointer  :: integral(:) => null()     ! /integral - Integral from 0 to rho of the source term. Time-dependent; Vector (nrho)
  character(len=132), dimension(:), pointer ::source => null()       ! /source - Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); Strin
endtype

type type_sourceion  !    Structure for the total source term for the transport equation (ions). Time-dependent;
  real(DP),pointer  :: value(:,:) => null()     ! /value - Value of the source term; Time-dependent; Matrix (nrho,nion)
  real(DP),pointer  :: integral(:,:) => null()     ! /integral - Integral from 0 to rho of the source term. Time-dependent; Matrix (nrho,nion)
  character(len=132), dimension(:), pointer ::source => null()       ! /source - Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); Array
endtype

type type_coretransel  !    Structure for the transport coefficients for the transport equation (electrons). Time-dependent;
  real(DP),pointer  :: diff(:) => null()     ! /diff - Diffusion coefficient [m^2.s^-1]. Time-dependent; Vector (nrho)
  real(DP),pointer  :: vconv(:) => null()     ! /vconv - Convection coefficient [m.s^-1]. Time-dependent; Vector (nrho)
  character(len=132), dimension(:), pointer ::source => null()       ! /source - Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); Strin
endtype

type type_coretransion  !    Structure for the transport coefficients for the transport equation (ions). Time-dependent;
  real(DP),pointer  :: diff(:,:) => null()     ! /diff - Diffusion coefficient [m^2.s^-1]. Time-dependent; Matrix (nrho,nion)
  real(DP),pointer  :: vconv(:,:) => null()     ! /vconv - Convection coefficient [m.s^-1]. Time-dependent; Matrix (nrho,nion)
  character(len=132), dimension(:), pointer ::source => null()       ! /source - Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); Array
endtype

type type_fluxel  !    Structure for the fluxes of a field of the core transport equations (electrons); Time-dependent;
  real(DP),pointer  :: flux_dv(:) => null()     ! /flux_dv - Flux of the field calculated from the transport coefficients. Time-dependent; Vector (nrho)
  real(DP),pointer  :: flux_interp(:) => null()     ! /flux_interp - Interpretative flux deduced from measured data, the integral of the source term, and the time derivative of the field. Time-depend
endtype

type type_fluxion  !    Structure for the fluxes of a field of the core transport equations (ions); Time-dependent;
  real(DP),pointer  :: flux_dv(:,:) => null()     ! /flux_dv - Flux of the field calculated from the transport coefficients. Time-dependent; Matrix (nrho,nion)
  real(DP),pointer  :: flux_interp(:,:) => null()     ! /flux_interp - Interpretative flux deduced from measured data, the integral of the source term, and the time derivative of the field. Time-depend
endtype

type type_corefield  !    Structure for a main field of core transport equations; Time-dependent;
  real(DP),pointer  :: value(:) => null()     ! /value - Signal value; Time-dependent; Vector (nrho)
  character(len=132), dimension(:), pointer ::source => null()       ! /source - Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); Strin
  integer  :: flag=-999999999       ! /flag - Flag describing how the profile has been processed : 0-not calculated 1-interpretative; 2-calculated by the transport solver; 3-ca
   type (type_boundaryel)  :: boundary  ! /boundary - Boundary condition for the transport equation. Time-dependent.
   type (type_sourceel)  :: source_term  ! /source_term - Total source term for the transport equation. Time-dependent.
   type (type_coretransel)  :: transp_coef  ! /transp_coef - Total transport coefficients. Time-dependent.
   type (type_fluxel)  :: flux  ! /flux - Fluxes of the quantity, two definitions. Time-dependent.
  real(DP),pointer  :: time_deriv(:) => null()     ! /time_deriv - Integral of the time derivative term of the transport equation. Time-dependent. Vector (nrho)
  type (type_codeparam) :: codeparam  ! /codeparam - 
endtype

type type_corefieldion  !    Structure for an ion field of core transport equations; Time-dependent;
  real(DP),pointer  :: value(:,:) => null()     ! /value - Signal value; Time-dependent; Vector (nrho,nion)
  character(len=132), dimension(:), pointer ::source => null()       ! /source - Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); Array
  integer,pointer  :: flag(:) => null()      ! /flag - Flag describing how the profile has been processed : 0-not calculated 1-interpretative; 2-calculated by the transport solver; 3-ca
   type (type_boundaryion)  :: boundary  ! /boundary - Boundary condition for the transport equation
   type (type_sourceion)  :: source_term  ! /source_term - Total source term for the transport equation. Time-dependent.
   type (type_coretransion)  :: transp_coef  ! /transp_coef - Total transport coefficients. Time-dependent.
   type (type_fluxion)  :: flux  ! /flux - Fluxes of the quantity, two definitions. Time-dependent.
  real(DP),pointer  :: time_deriv(:,:) => null()     ! /time_deriv - Integral of the time derivative term of the transport equation. Time-dependent. Matrix (nrho,nion)
  type (type_codeparam) :: codeparam  ! /codeparam - 
endtype

type type_coreprofile  !    Structure for core plasma profile; Time-dependent
  real(DP),pointer  :: value(:) => null()     ! /value - Signal value; Time-dependent; Vector (nrho)
  character(len=132), dimension(:), pointer ::source => null()       ! /source - Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); Strin
endtype

type type_coreprofion  !    Structure for core plasma ion profile; Time-dependent
  real(DP),pointer  :: value(:,:) => null()     ! /value - Signal value; Time-dependent; Vector (nrho,nion)
  character(len=132), dimension(:), pointer ::source => null()       ! /source - Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); Array
endtype

type type_boundary  !    
  real(DP),pointer  :: value(:) => null()     ! /coreprof/psi/boundary/value - Value of the boundary condition (in case flag = 2). Unit depends on type, respectively [1-Wb, 2-A, 3-V]. For type 1 to 3, only the
  character(len=132), dimension(:), pointer ::source => null()       ! /coreprof/psi/boundary/source - Source of the boundary condition (any comment describing its origin : code, path to diagnostic signals, massaging); String
  integer  :: type=-999999999       ! /coreprof/psi/boundary/type - Type of the boundary condition for the transport solver (in case flag = 2).     0- equation not solved; 1- edge value of poloidal 
  real(DP)  :: rho=-9.0D40       ! /coreprof/psi/boundary/rho - Position of the boundary condition (in terms of toroidal flux coordinate) for the transport solver [m].  Outside this boundary, th
  type (type_codeparam) :: codeparam  ! /coreprof/psi/boundary/codeparam - 
endtype

type type_jni  !    
  real(DP),pointer  :: value(:) => null()     ! /coreprof/psi/jni/value - Value of jni; Time-dependent; Vector (nrho)
  real(DP),pointer  :: integral(:) => null()     ! /coreprof/psi/jni/integral - Integral from 0 to rho of jni. Time-dependent; Vector (nrho)
  character(len=132), dimension(:), pointer ::source => null()       ! /coreprof/psi/jni/source - Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); Strin
endtype

type type_toroid_field  !    
  real(DP)  :: b0=-9.0D40       ! /coreprof/toroid_field/b0 - Vacuum field at r0 [T]; Time-dependent. Scalar.
  real(DP)  :: b0prime=-9.0D40       ! /coreprof/toroid_field/b0prime - Time derivative of the vacuum field at r0 [T/s]; Time-dependent. Scalar.
  real(DP)  :: r0=-9.0D40       ! /coreprof/toroid_field/r0 - Characteristic major radius of the device (used in publications, usually middle of the vessel at the equatorial midplane) [m]. Sca
  real(DP)  :: time=-9.0D40       ! /coreprof/toroid_field/time - Time [s] (exact time slice used from the time array of the source signal, here the toroidfield CPO. If the time slice does not exi
endtype

type type_psi  !    
  real(DP),pointer  :: value(:) => null()     ! /coreprof/psi/value - Signal value [Wb]; Time-dependent; Vector (nrho)
  character(len=132), dimension(:), pointer ::source => null()       ! /coreprof/psi/source - Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); Strin
  integer  :: flag=-999999999       ! /coreprof/psi/flag - Flag describing how the profile has been processed : 0-not calculated; 1-interpretative; 2-calculated by the transport solver; 3-c
  type (type_boundary) :: boundary  ! /coreprof/psi/boundary - Boundary condition for the transport equation. Time-dependent.
  type (type_jni) :: jni  ! /coreprof/psi/jni - Non-inductive parallel current density [A/m^2]; Time-dependent;
   type (type_coreprofile)  :: sigma_par  ! /coreprof/psi/sigma_par - Parallel conductivity [ohm^-1.m^-1]. Time-dependent
  type (type_codeparam) :: codeparam  ! /coreprof/psi/codeparam - 
endtype

type type_profiles1d  !    
   type (type_coreprofile)  :: pe  ! /coreprof/profiles1d/pe - Electron pressure [Pa]; Time-dependent;
   type (type_coreprofion)  :: pi  ! /coreprof/profiles1d/pi - Ion pressure [Pa]; Time-dependent;
   type (type_coreprofile)  :: pr_th  ! /coreprof/profiles1d/pr_th - Thermal pressure (electrons+ions) [Pa]; Time-dependent;
   type (type_coreprofile)  :: pr_perp  ! /coreprof/profiles1d/pr_perp - Total perpendicular pressure (electrons+ions, thermal+non-thermal) [Pa]; Time-dependent;
   type (type_coreprofile)  :: pr_parallel  ! /coreprof/profiles1d/pr_parallel - Total parallel pressure (electrons+ions, thermal+non-thermal) [Pa]; Time-dependent;
   type (type_coreprofile)  :: jtot  ! /coreprof/profiles1d/jtot - total parallel current density = average(jtot.B) / B0, where B0 = coreprof/toroid_field/b0 [A/m^2]; Time-dependent;
   type (type_coreprofile)  :: jni  ! /coreprof/profiles1d/jni - non-inductive parallel current density = average(jni.B) / B0, where B0 = coreprof/toroid_field/b0 [A/m^2]; Time-dependent;
   type (type_coreprofile)  :: joh  ! /coreprof/profiles1d/joh - ohmic parallel current density = average(joh.B) / B0, where B0 = coreprof/toroid_field/b0 [A/m^2]; Time-dependent;
   type (type_coreprofile)  :: vloop  ! /coreprof/profiles1d/vloop - Toroidal loop voltage [V].  Time-dependent.
   type (type_coreprofile)  :: sigmapar  ! /coreprof/profiles1d/sigmapar - Parallel conductivity [ohm^-1.m^-1].  Time-dependent. 
   type (type_coreprofile)  :: qoh  ! /coreprof/profiles1d/qoh - ohmic heating [W/m^3]; Time-dependent;
   type (type_coreprofile)  :: eparallel  ! /coreprof/profiles1d/eparallel - Parallel electric field = average(E.B) / B0, where B0 = coreprof/toroid_field/b0 [V.m^-1].  Time-dependent. 
   type (type_coreprofile)  :: e_b  ! /coreprof/profiles1d/e_b - Average(E.B) [V.T.m^-1].  Time-dependent. 
   type (type_coreprofile)  :: q  ! /coreprof/profiles1d/q - Safety factor profile; Time-dependent;
   type (type_coreprofile)  :: shear  ! /coreprof/profiles1d/shear - Magnetic shear profile; Time-dependent;
   type (type_coreprofion)  :: ns  ! /coreprof/profiles1d/ns - Density of fast ions, for the various ion species [m^-3]; Time-dependent;
   type (type_coreprofion)  :: mtor  ! /coreprof/profiles1d/mtor - Toroidal momentum of the various ion species [UNITS?]; Time-dependent;
   type (type_coreprofion)  :: wtor  ! /coreprof/profiles1d/wtor - Angular toroidal rotation frequency of the various ion species [s^-1]; Time-dependent;
   type (type_coreprofile)  :: zeff  ! /coreprof/profiles1d/zeff - Effective charge profile; Time-dependent;
endtype

type type_globalparam  !    
  real(DP)  :: current_tot=-9.0D40       ! /coreprof/globalparam/current_tot - Total plasma current [A]; Time-dependent; Scalar
  real(DP)  :: current_bnd=-9.0D40       ! /coreprof/globalparam/current_bnd - Plasma current inside transport solver boundary rho_tor_bnd [A]; Time-dependent; Scalar
  real(DP)  :: vloop=-9.0D40       ! /coreprof/globalparam/vloop - Toroidal loop voltage [V]; Time-dependent; Scalar
  real(DP)  :: li=-9.0D40       ! /coreprof/globalparam/li - Internal inductance; Time-dependent; Scalar
endtype

type type_coreprof  !    
  type (type_datainfo) :: datainfo  ! /coreprof/datainfo - 
  real(DP),pointer  :: rho_tor_norm(:) => null()     ! /coreprof/rho_tor_norm - Normalised toroidal flux coordinate values (= rho_tor normalised to the value at the last grid point); Vector (nrho)
  real(DP),pointer  :: rho_tor(:) => null()     ! /coreprof/rho_tor - Toroidal flux coordinate (not normalised, equivalent to rho_tor_norm) [m];  Vector (nrho). Time-dependent.
  real(DP),pointer  :: drho_dt(:) => null()     ! /coreprof/drho_dt - Time derivative of rho_tor  [m/s];  Vector (nrho). Time-dependent.
  type (type_toroid_field) :: toroid_field  ! /coreprof/toroid_field - Toroidal field information  entering the definition of rho_tor, for reference only. The physical value of the toroidal field shoul
  type (type_composition) :: composition  ! /coreprof/composition - 
  type (type_psi) :: psi  ! /coreprof/psi - Poloidal magnetic flux [Wb]; Time-dependent;
   type (type_corefield)  :: te  ! /coreprof/te - Electron temperature [eV]; (source term in [W.m^-3]). Time-dependent;
   type (type_corefieldion)  :: ti  ! /coreprof/ti - Ion temperature [eV]; (source term in [W.m^-3]). Time-dependent;
   type (type_corefield)  :: ne  ! /coreprof/ne - Electron density [m^-3]; (source term in [m^-3]).Time-dependent;
   type (type_corefieldion)  :: ni  ! /coreprof/ni - Ion density [m^-3]; (source term in [m^-3]). Time-dependent;
   type (type_corefieldion)  :: vtor  ! /coreprof/vtor - Toroidal velocity of the various ion species [m.s^-1]; Time-dependent;
  type (type_profiles1d) :: profiles1d  ! /coreprof/profiles1d - Profiles derived from the fields solved in the transport equations, or from experiment.
  type (type_globalparam) :: globalparam  ! /coreprof/globalparam - Various global quantities calculated from the 1D profiles. Time-dependent
  type (type_codeparam) :: codeparam  ! /coreprof/codeparam - 
  real(DP)  :: time=-9.0D40       ! /coreprof/time - Time [s]; Time-dependent; Scalar
endtype

! ***********  Include coresource.xsd
type type_coresource  !    
  type (type_datainfo) :: datainfo  ! /coresource/datainfo - 
  real(DP),pointer  :: rho_tor(:) => null()     ! /coresource/rho_tor - Toroidal flux coordinate (not normalised, equivalent to rho_tor_norm) [m];  Vector (nrho). Time-dependent.
  real(DP),pointer  :: rho_tor_norm(:) => null()     ! /coresource/rho_tor_norm - Normalised toroidal flux coordinate values (= rho_tor normalised to the value at the last grid point); Vector (nrho)
  type (type_composition) :: composition  ! /coresource/composition - 
   type (type_b0r0)  :: toroid_field  ! /coresource/toroid_field - Characteristics of the vacuum toroidal field, redundant with the toroidfield CPO, to document the normalisation of j in this CPO.
  real(DP),pointer  :: j(:) => null()     ! /coresource/j - Parallel current source for psi transport equation, = average(j.B) / B0, where B0  = coresource/toroid_field/b0 [A.m^-2]. Vector(n
  real(DP),pointer  :: sigma(:) => null()     ! /coresource/sigma - Induced parallel conductivity [ohm^-1.m^-1]. EXACT DEFINITION PENDING. Vector(nrho). Time-dependent.
   type (type_source_ion)  :: si  ! /coresource/si - Particle source for ion density transport equation [m^-3.s^-1]. Time-dependent.
   type (type_source_el)  :: se  ! /coresource/se - Particle source for electron density transport equation [m^-3.s^-1]. Time-dependent.
   type (type_source_imp)  :: sz  ! /coresource/sz - Particle source for impurity density transport equation [m^-3.s^-1]. Time-dependent.
   type (type_source_ion)  :: qi  ! /coresource/qi - Heat source for ion heat transport equations [W.m^-3]. Time-dependent.
   type (type_source_el)  :: qe  ! /coresource/qe - Heat source for electron heat transport equation [W.m^-3]. Time-dependent.
   type (type_source_imp)  :: qz  ! /coresource/qz - Heat source for impurity heat transport equations [W.m^-3].  Time-dependent.
   type (type_source_ion)  :: ui  ! /coresource/ui - Velocity source for toroidal velocity transport equation [kg.m^-1.s^-2]. Vector(nrho). Time-dependent.
  type (type_codeparam) :: codeparam  ! /coresource/codeparam - 
  real(DP)  :: time=-9.0D40       ! /coresource/time - Time [s]; Time-dependent; Scalar
endtype

! ***********  Include coretransp.xsd
type type_ni_transp  !    
  real(DP),pointer  :: diff_eff(:,:,:) => null()     ! /coretransp/ni_transp/diff_eff - Effective diffusivity [m^2.s^-1]. The last index of the array describes which multiplier should be applied to the particule flux w
  real(DP),pointer  :: vconv_eff(:,:,:) => null()     ! /coretransp/ni_transp/vconv_eff - Effective convection [m.s^-1]. The last index of the array describes which multiplier should be applied to the particule flux when
  real(DP),pointer  :: flux(:,:) => null()     ! /coretransp/ni_transp/flux - Flux. Not used in transport equations [field.m.s^-1,.m^-3 if field is not a density itself]. Time-dependent. Matrix (nrho,nion)
   type (type_offdiagion)  :: off_diagonal  ! /coretransp/ni_transp/off_diagonal - Details of the transport matrix, just for diagnostic (not used in transport equations). Time-dependent.
  integer  :: flag=-999999999       ! /coretransp/ni_transp/flag - Flag describing the form of transport produced by the original model : 0- not calculated, 1- D and V, 2- flux, 3- full transport m
endtype

type type_ne_transp  !    
  real(DP),pointer  :: diff_eff(:,:) => null()     ! /coretransp/ne_transp/diff_eff - Effective diffusivity [m^2.s^-1]. The last index of the array describes which multiplier should be applied to the particule flux w
  real(DP),pointer  :: vconv_eff(:,:) => null()     ! /coretransp/ne_transp/vconv_eff - Effective convection [m.s^-1]. The last index of the array describes which multiplier should be applied to the particule flux when
  real(DP),pointer  :: flux(:) => null()     ! /coretransp/ne_transp/flux - Flux. Not used in transport equations [field.m.s^-1,.m^-3 if field is not a density itself]. Time-dependent. Vector (nrho)
   type (type_offdiagel)  :: off_diagonal  ! /coretransp/ne_transp/off_diagonal - Details of the transport matrix, just for diagnostic (not used in transport equations). Time-dependent.
  integer  :: flag=-999999999       ! /coretransp/ne_transp/flag - Flag describing the form of transport produced by the original model : 0- not calculated, 1- D and V, 2- flux, 3- full transport m
endtype

type type_coretransp  !    
  type (type_datainfo) :: datainfo  ! /coretransp/datainfo - 
  type (type_composition) :: composition  ! /coretransp/composition - 
  real(DP),pointer  :: rho_tor_norm(:) => null()     ! /coretransp/rho_tor_norm - Normalised toroidal flux coordinate values (= rho_tor normalised to the value at the last grid point); Vector (nrho)
  real(DP),pointer  :: rho_tor(:) => null()     ! /coretransp/rho_tor - Toroidal flux coordinate (not normalised, equivalent to rho_tor_norm) [m];  Vector (nrho). Time-dependent.
  real(DP),pointer  :: sigma(:) => null()     ! /coretransp/sigma - Parallel conductivity [ohm^-1.m^-1]. Time-dependent. Vector(nrho).
  type (type_ni_transp) :: ni_transp  ! /coretransp/ni_transp - Transport coefficients for ion density equation. Time-dependent.
  type (type_ne_transp) :: ne_transp  ! /coretransp/ne_transp - Transport coefficients for electron density equation. Time-dependent.
   type (type_transcoefimp)  :: nz_transp  ! /coretransp/nz_transp - Transport coefficients for impurity (multiple charge state) density equation. Time-dependent.
   type (type_transcoefion)  :: ti_transp  ! /coretransp/ti_transp - Transport coefficients for ion temperature equation. Time-dependent.
   type (type_transcoefel)  :: te_transp  ! /coretransp/te_transp - Transport coefficients for electron temperature equation. Time-dependent.
   type (type_transcoefimp)  :: tz_transp  ! /coretransp/tz_transp - Transport coefficients for impurity  (multiple charge state) temperature equation. Time-dependent.
   type (type_transcoefvtor)  :: vtor_transp  ! /coretransp/vtor_transp - Transport coefficients for toroidal velocity equation. Time-dependent.
  type (type_codeparam) :: codeparam  ! /coretransp/codeparam - 
  real(DP)  :: time=-9.0D40       ! /coretransp/time - Time [s]; Time-dependent; Scalar
endtype

! ***********  Include eqcoord_sys.xsd
type type_coord_sys  !    
  character(len=132), dimension(:), pointer ::grid_type => null()       ! /coord_sys/grid_type - Type of coordinate system
   type (type_reggrid)  :: grid  ! /coord_sys/grid - Regular grid definition; Time-dependent
  real(DP),pointer  :: jacobian(:,:) => null()     ! /coord_sys/jacobian - Jacobian of the coordinate system; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: g_11(:,:) => null()     ! /coord_sys/g_11 - metric coefficients g_11; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: g_12(:,:) => null()     ! /coord_sys/g_12 - metric coefficients g_12; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: g_13(:,:) => null()     ! /coord_sys/g_13 - metric coefficients g_13; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: g_22(:,:) => null()     ! /coord_sys/g_22 - metric coefficients g_22; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: g_23(:,:) => null()     ! /coord_sys/g_23 - metric coefficients g_23; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: g_33(:,:) => null()     ! /coord_sys/g_33 - metric coefficients g_33; Time-dependent; Matrix (ndim1, ndim2)
   type (type_rz2D)  :: position  ! /coord_sys/position - R and Z position of grid points; Time-dependent; Matrix (ndim1, ndim2)
endtype

! ***********  Include eqconstraint.xsd
type type_eqmes0D  !    Structure for equilibrium measurement 0D signal
  real(DP)  :: measured=-9.0D40       ! /measured - Measured value of the signal; Time-dependent; Scalar.
  character(len=132), dimension(:), pointer ::source => null()       ! /source - Path to the source signal (diagnostic or genprof, from which to read all info on the signal); String
  real(DP)  :: time=-9.0D40       ! /time - Time (exact time slice used from the time array of the source signal. If the time slice does not exist in the time array of the so
  integer  :: exact=-999999999       ! /exact - 1 means exact data, is not fitted; 0 means the equilibrium code does a least square fit; scalar integer
  real(DP)  :: weight=-9.0D40       ! /weight - weight given to the measurement (>= 0); Time-dependent; Scalar.
  real(DP)  :: sigma=-9.0D40       ! /sigma - standard deviation of the measurement; Time-dependent; Scalar.
  real(DP)  :: calculated=-9.0D40       ! /calculated - Signal as recalculated by the equilibrium code; Time-dependent; Scalar.
  real(DP)  :: chi2=-9.0D40       ! /chi2 - chi^2 of (calculated-measured); Time-dependent; Scalar.
endtype

type type_eqmes1D  !    Structure for equilibrium measurement 1D signal
  real(DP),pointer  :: measured(:) => null()     ! /measured - Measured value of the signal; Time-dependent; Array(nmeas)
  character(len=132), dimension(:), pointer ::source => null()       ! /source - Path to the source signal (diagnostic or genprof, from which to read all info on the signal), e.g. 'magdiag/bpol_probes/measure/va
  real(DP)  :: time=-9.0D40       ! /time - Exact time slice used from the time array of the source signal. If the time slice does not exist in the time array of the source s
  integer,pointer  :: exact(:) => null()      ! /exact - 1 means exact data, is not fitted; 0 means the equilibrium code does a least square fit; Time-dependent; Array(nmeas)
  real(DP),pointer  :: weight(:) => null()     ! /weight - weight given to the measurement (>= 0); Time-dependent; Array(nmeas)
  real(DP),pointer  :: sigma(:) => null()     ! /sigma - standard deviation of the measurement; Time-dependent; Array(nmeas)
  real(DP),pointer  :: calculated(:) => null()     ! /calculated - Signal as recalculated by the equilibrium code; Time-dependent; Array(nmeas)
  real(DP),pointer  :: chi2(:) => null()     ! /chi2 - chi^2 of (calculated-measured); Time-dependent; Array(nmeas)
endtype

type type_isoflux  !    
   type (type_rz1D)  :: position  ! /eqconstraint/isoflux/position - Position of the points at which the flux is considered the same; Time-dependent; Vector (nmeas)
  character(len=132), dimension(:), pointer ::source => null()       ! /eqconstraint/isoflux/source - Description or path to the source signal (diagnostic or genprof, from which to read all info on the signal), e.g. 'magdiag/bpol_pr
  real(DP),pointer  :: weight(:) => null()     ! /eqconstraint/isoflux/weight - weight given to the measurement (>= 0); -1 if exact data; Time-dependent; Vector (nmeas)
  real(DP),pointer  :: sigma(:) => null()     ! /eqconstraint/isoflux/sigma - standard deviation of the measurement; Time-dependent; Vector (nmeas)
  real(DP),pointer  :: calculated(:) => null()     ! /eqconstraint/isoflux/calculated - Signal as recalculated by the equilibrium code; Time-dependent; Vector (nmeas)
  real(DP),pointer  :: chi2(:) => null()     ! /eqconstraint/isoflux/chi2 - chi^2 of (calculated-measured); Time-dependent; Vector (nmeas)
endtype

type type_magnet_iron  !    
   type (type_eqmes1D)  :: mr  ! /eqconstraint/magnet_iron/mr - Magnetisation along the R axis [T];
   type (type_eqmes1D)  :: mz  ! /eqconstraint/magnet_iron/mz - Magnetisation along the Z axis [T];
endtype

type type_q  !    
  real(DP),pointer  :: qvalue(:) => null()     ! /eqconstraint/q/qvalue - Safety factor values; Time-dependent; Vector (nmeas)
   type (type_rz1D)  :: position  ! /eqconstraint/q/position - Major radius of the given safety factor values [m]; Time-dependent; Vector (nmeas)
  character(len=132), dimension(:), pointer ::source => null()       ! /eqconstraint/q/source - Description or path to the source signal (diagnostic or genprof, from which to read all info on the signal), e.g. 'magdiag/bpol_pr
  integer  :: exact=-999999999       ! /eqconstraint/q/exact - 1 means exact data, is not fitted; 0 means the equilibrium code does a least square fit; scalar integer
  real(DP),pointer  :: weight(:) => null()     ! /eqconstraint/q/weight - weight given to the measurement (>= 0); Time-dependent; Vector (nmeas)
  real(DP),pointer  :: sigma(:) => null()     ! /eqconstraint/q/sigma - standard deviation of the measurement; Time-dependent; Vector (nmeas)
  real(DP),pointer  :: calculated(:) => null()     ! /eqconstraint/q/calculated - Signal as recalculated by the equilibrium code; Time-dependent; Vector (nmeas)
  real(DP),pointer  :: chi2(:) => null()     ! /eqconstraint/q/chi2 - chi^2 of (calculated-measured); Time-dependent; Vector (nmeas)
endtype

type type_xpts  !    
   type (type_rz1D)  :: position  ! /eqconstraint/xpts/position - Position of the X-point(s); Time-dependent; Vector (nmeas)
  character(len=132), dimension(:), pointer ::source => null()       ! /eqconstraint/xpts/source - Description or path to the source signal (diagnostic or genprof, from which to read all info on the signal), e.g. 'magdiag/bpol_pr
  real(DP),pointer  :: weight(:) => null()     ! /eqconstraint/xpts/weight - weight given to the measurement (>= 0); -1 if exact data; Time-dependent; Vector (nmeas)
  real(DP),pointer  :: sigma(:) => null()     ! /eqconstraint/xpts/sigma - standard deviation of the measurement; Time-dependent; Vector (nmeas)
  real(DP),pointer  :: calculated(:) => null()     ! /eqconstraint/xpts/calculated - Signal as recalculated by the equilibrium code; Time-dependent; Vector (nmeas)
  real(DP),pointer  :: chi2(:) => null()     ! /eqconstraint/xpts/chi2 - chi^2 of (calculated-measured); Time-dependent; Vector (nmeas)
endtype

type type_eqconstraint  !    
   type (type_eqmes1D)  :: bpol  ! /eqconstraint/bpol - poloidal pickup coils [T]
   type (type_eqmes0D)  :: bvac_r  ! /eqconstraint/bvac_r - Vacuum field times radius in the toroidal field magnet [T.m];
   type (type_eqmes1D)  :: faraday  ! /eqconstraint/faraday - Faraday rotation angles [rad]
   type (type_eqmes1D)  :: flux  ! /eqconstraint/flux - Poloidal flux loops [Wb]
   type (type_eqmes0D)  :: i_plasma  ! /eqconstraint/i_plasma - Plasma current [A];
  type (type_isoflux) :: isoflux  ! /eqconstraint/isoflux - Point series at which the flux is considered the same
   type (type_eqmes1D)  :: jsurf  ! /eqconstraint/jsurf - Average of current density on the flux surface [A/m^2]
  type (type_magnet_iron) :: magnet_iron  ! /eqconstraint/magnet_iron - Magnetisation in iron segments [T]
   type (type_eqmes1D)  :: mse  ! /eqconstraint/mse - MSE angles [rad]
   type (type_eqmes1D)  :: ne  ! /eqconstraint/ne - Electron density [m^-3 for local measurement, m^-2 if line integrated]
   type (type_eqmes1D)  :: pfcurrent  ! /eqconstraint/pfcurrent - Current in poloidal field coils [A]
   type (type_eqmes1D)  :: pressure  ! /eqconstraint/pressure - Total pressure [Pa]
  type (type_q) :: q  ! /eqconstraint/q - Safety factor
  type (type_xpts) :: xpts  ! /eqconstraint/xpts - Position of the X-point(s)
endtype

! ***********  Include emcalc.xsd
type type_plasmagrid  !    
  character(len=132), dimension(:), pointer ::grid_type => null()       ! /emcalc/plasmagrid/grid_type - Selection of one of a set of grid types
   type (type_reggrid)  :: grid  ! /emcalc/plasmagrid/grid - Regular grid of the plasma area
  real(DP),pointer  :: dflux_grid(:,:,:) => null()     ! /emcalc/plasmagrid/dflux_grid - Coupling from plasma grid currents to the magdiag flux loops [H]; Array 3D (nloops from magdiag/flux_loops,ndim1,ndim2)
  real(DP),pointer  :: bpol_grid(:,:,:) => null()     ! /emcalc/plasmagrid/bpol_grid - Coupling from plasma grid currents to the magdiag bpol [T]; Array 3D (nprobes from magdiag/bpol_probes,ndim1,ndim2);  
  real(DP),pointer  :: pol_grid(:,:,:) => null()     ! /emcalc/plasmagrid/pol_grid - Coupling from plasma grid currents to the poloidal coils [active, passive] [H]; Array 3D (nloops from magdiag/flux_loops,ndim1,ndi
  real(DP),pointer  :: grid_grid(:,:,:,:) => null()     ! /emcalc/plasmagrid/grid_grid - Coupling from plasma grid currents to the plasma grid currents [H]; Array 4D (ndim1,ndim2,ndim1,ndim2)
endtype

type type_emcalc  !    
  type (type_datainfo) :: datainfo  ! /emcalc/datainfo - Title of the block, for display
  integer,pointer  :: modelnrnz(:) => null()      ! /emcalc/modelnrnz - nR and nZ used to model the poloidal field coils; Vector(2)
  integer  :: npolcurrents=-999999999       ! /emcalc/npolcurrents - Number of poloidal currents (circuits) = nactive + npassive
  real(DP),pointer  :: mut_pol_pol(:,:) => null()     ! /emcalc/mut_pol_pol - Mutual inductance matrix between all poloidal circuits in order [active, passive] [H]; Matrix (npol,npol);  
  real(DP),pointer  :: br_pol_pol(:,:) => null()     ! /emcalc/br_pol_pol - Radial field at poloidal currents due to poloidal currents [T]; Matrix (npol,npol);  
  real(DP),pointer  :: bz_pol_pol(:,:) => null()     ! /emcalc/bz_pol_pol - Vertical field at poloidal currents due to poloidal currents [T]; Matrix (npol,npol);  
  real(DP),pointer  :: bpol_pol(:,:) => null()     ! /emcalc/bpol_pol - Field from poloidal currents at the magdiag bpol [T]; Matrix (nprobes from magdiag/bpol_probes,npol);  
  real(DP),pointer  :: dflux_pol(:,:) => null()     ! /emcalc/dflux_pol - Coupling from poloidal currents to the magdiag flux loops [H]; Matrix(nloops from magdiag/flux_loops,npol)
  type (type_plasmagrid) :: plasmagrid  ! /emcalc/plasmagrid - Basic plasma grid
endtype

! ***********  Include flush.xsd
type type_flush  !    
  type (type_datainfo) :: datainfo  ! /flush/datainfo - 
   type (type_rz1D)  :: position  ! /flush/position - Major radius and altitude of the FLUSH grid [m]; Time-dependent; Vectors resp. (nR) and (nZ)
  real(DP),pointer  :: coef(:,:) => null()     ! /flush/coef - Coefficients of the fit; Time-dependent; Matrix 2D (nR,nZ)
  type (type_codeparam) :: codeparam  ! /flush/codeparam - 
endtype

! ***********  Include fp.xsd
type type_fp_src_snk_vol  !    
  real(DP),pointer  :: particles(:,:) => null()     ! /particles - Source/sink particles [1/s]; Time-dependedent; Matrix(ndist_spec, npsin)
  real(DP),pointer  :: power(:,:) => null()     ! /power - Power associated with the source/sink of particles [W]; Time-dependent; Matrix(ndist_spec, npsin)
  real(DP),pointer  :: torque(:,:) => null()     ! /torque - Torque due to the source/sink of particles [N.m]; Time-dependent; Matrix (ndist_spec, npsin).
endtype

type type_fp_src_snk_surf  !    
  real(DP),pointer  :: particlesd(:,:) => null()     ! /particlesd - Source/sink  particles [s^-1 m^-3]; Time-dependedent; Matrix(ndist_spec, npsin)
  real(DP),pointer  :: powerd(:,:) => null()     ! /powerd - Power density associated with the source/sink of particles [W.m^-3]; Time-dependent; Matrix(ndist_spec, npsin)
  real(DP),pointer  :: torqued(:,:) => null()     ! /torqued - Torque density due to the source/sink of particles [N.m^-2]; Time-dependent; Matrix(ndist_spec, npsin).
endtype

type type_fp_src_snk_tot  !    
  real(DP),pointer  :: particles(:) => null()     ! /particles - Source/sink  particles [1/s]; Time-dependedent; Vector(ndist_spec)
  real(DP),pointer  :: power(:) => null()     ! /power - Power associated with the source/sink of particles [W]; Time-dependent; Vector(ndist_spec)
  real(DP),pointer  :: torque(:) => null()     ! /torque - Torque due to the source/sink of particles [N.m]; Time-dependent; Vector (ndist_spec).
endtype

type type_fp_test_part  !    Distribution given as a set of test particles.
  real(DP),pointer  :: nvar(:) => null()     ! /nvar - Number of variables associated with a test particle; Vector (ndist_spec)
  integer,pointer  :: var_id(:,:) => null()     ! /var_id - Identification of a variable; Matrix (ndist_spec, 5)
  real(DP),pointer  :: var1(:,:) => null()     ! /var1 - Phase space variables one characterising a test particle; Time-dependent; Matrix (ndist_spec, ntpart)
  real(DP),pointer  :: var2(:,:) => null()     ! /var2 - Phase space variables two characterising a test particle; Time-dependent; Matrix  (ndist_spec, ntpart)
  real(DP),pointer  :: var3(:,:) => null()     ! /var3 - Phase space variables three characterising a test particle; Time-dependent; Matrix (ndist_spec, ntpart)
  integer,pointer  :: var4(:,:) => null()     ! /var4 - Phase space variables four characterising a test particle; Time-dependent; Matrix  (ndist_spec, ntpart)
  integer,pointer  :: var5(:,:) => null()     ! /var5 - Phase space variables five characterising a test particle; Time-dependent; Matrix  (ndist_spec, ntpart)
  real(DP),pointer  :: weight(:,:) => null()     ! /weight - Weight of a test particle; Time-dependent; Matrix (ndist_spec, ntpart)
endtype

type type_fp_grid  !    Grid on which the distribution function is calculated.
  real(DP),pointer  :: dim1(:,:) => null()     ! /dim1 - First dimension in phase space; Time-dependent; Matrix (ndist_spec, max_ndim1).
  integer,pointer  :: ndim1(:) => null()      ! /ndim1 - Size of the first dimension in phase space, for each species; Vector (ndist_spec).
  real(DP),pointer  :: dim2(:,:) => null()     ! /dim2 - Second dimension in phase space; Time-dependent; Matrix (ndist_spec, max_ndim2).
  integer,pointer  :: ndim2(:) => null()      ! /ndim2 - Size of the second dimension in phase space, for each species; Vector (ndist_spec).
  real(DP),pointer  :: dim3(:,:) => null()     ! /dim3 - Third dimension in phase space; Time-dependent; Matrix (ndist_spec, max_ndim3).
  integer,pointer  :: ndim3(:) => null()      ! /ndim3 - Size of the third dimension in phase space, for each species; Vector (ndist_spec).
  real(DP),pointer  :: jacobian(:,:,:,:,:) => null()     ! /jacobian - Jacobian of the transformation of the phase space grid variables; Time-dependent; vector (ndist_spec, max_ndim1, max_ndim2, max_nd
endtype

type type_fp_ff  !    Orbit averaged (or Bounce averaged) zero order distribution function.
  integer,pointer  :: grid_type(:) => null()      ! /grid_type - Type of grid. Vector (ndist_spec).
   type (type_fp_grid)  :: grid  ! /grid - Grid on which the distribution function is calculated.
  real(DP),pointer  :: value(:,:,:,:) => null()     ! /value - Orbit (or bounce) averaged distribution function given on a grid [1/m^3 (m/s)^-3]; Time-dependent; array(ndist_spec, max_ndim1, ma
endtype

type type_fp_particle_src  !    Particle source
   type (type_fp_src_snk_tot)  :: total  ! /total - Total source of particles and power (NBI, fusion products, pellets etc.)
   type (type_fp_src_snk_vol)  :: volume_intgr  ! /volume_intgr - Volume integrated source of particles and power (NBI, fusion products, pellets etc.)
   type (type_fp_src_snk_surf)  :: flux_surf_av  ! /flux_surf_av - Flux surface averaged source of particles and power (NBI, fusion products, pellets etc.)
endtype

type type_fp_wave_src  !    Auxiliary wave absorbed by the distribution species
  character(len=132), dimension(:), pointer ::type => null()       ! /type - Wave type (LH, EC, IC, ...), can be a concatenation of these if several wave types are absorbed by this species. Vector of strings
  real(DP),pointer  :: wave_power(:) => null()     ! /wave_power - Auxiliary wave power absorbed by the distribution species [W]; Time-dependent; Vector (ndist_spec).
  real(DP),pointer  :: wave_powerd(:,:) => null()     ! /wave_powerd - Auxiliary flux surface averaged wave power density absorbed by the distribution species [W/m^3]; Time-dependent; Matrix (ndist_spe
endtype

type type_fp_input_src  !    Input sources of particles and power for the distribution species (to aid diagnosing the code output).
   type (type_fp_particle_src)  :: particle_src  ! /particle_src - Particle source
   type (type_fp_wave_src)  :: wave_src  ! /wave_src - Auxiliary wave absorbed by the distribution species
endtype

type type_fp_prof_vol_dist_losses  !    Losses of the distribution species.
   type (type_fp_src_snk_vol)  :: orb_loss  ! /orb_loss - Particle losses.
   type (type_fp_src_snk_vol)  :: neutr_loss  ! /neutr_loss - Losses due to charge exchange events
endtype

type type_fp_prof_surf_dist_losses  !    Losses of the distribution species.
   type (type_fp_src_snk_surf)  :: orb_loss  ! /orb_loss - Particle losses.
   type (type_fp_src_snk_surf)  :: neutr_loss  ! /neutr_loss - Losses due to charge exchange events
endtype

type type_fp_prof_surf_nucl_reac_th  !    Nuclear reactions between the cacluated species and oher species assumed to have thermal distributions.
  real(DP),pointer  :: rated(:,:,:) => null()     ! /rated - Reaction rate [s^-1.m^-3]; Time dependent; Array3d(ndist_spec, nreac_max, npsin)
  real(DP),pointer  :: powerd(:,:,:) => null()     ! /powerd - Nuclear reaction power density [W.m^-3]; Time dependent; Array3d(ndist_spec, nreac_max, npsin)
endtype

type type_fp_prof_surf_nucl_reac_sf  !    Nuclear reactions of the calculated species with itself (thermal + non-thermal).
  real(DP),pointer  :: rate(:,:) => null()     ! /rate - Reaction rate [s^-1.m^-3]; Time-dependent; Matrix (ndist_spec, npsin)
  real(DP),pointer  :: power(:,:) => null()     ! /power - Fusion reaction power [W.m^-3]; Time-dependent; Matrxi (ndist_spec, npsin)
endtype

type type_fp_prof_surf_energies  !    Energy densities of a distribution species.
  real(DP),pointer  :: total(:,:) => null()     ! /total - Energy density of of a distribution species [J/m^3]; Time-dependent; Matrix(ndist_spec, npsin)
  real(DP),pointer  :: perp(:,:) => null()     ! /perp - Perpendicular energy density of a distribution species [J/m^3] Time-dependent; Matrix(ndist_spec, npsin)
  real(DP),pointer  :: parallel(:,:) => null()     ! /parallel - Parallel energy densityt of a distribution species [J/m^3] Time-dependent; Matrix(ndist_spec, npsin).
endtype

type type_fp_nucl_reac_sf  !    Nuclear reactions of the calculated species with itself (thermal + non-thermal).
  real(DP),pointer  :: rate(:) => null()     ! /rate - Reaction rate [1/s]; Time-dependent; Vector (ndist_spec)
  real(DP),pointer  :: power(:) => null()     ! /power - Fusion reaction power[W]; Time-dependent; Vector (ndist_spec)
endtype

type type_fp_prof_surf_dist  !    Quantities related to the calculated distribution function (energy densities, sources, sinks  etc.)
   type (type_fp_prof_surf_energies)  :: energiesd  ! /energiesd - Energy densities of a distribution species.
   type (type_fp_prof_surf_dist_losses)  :: lossesd  ! /lossesd - Particle loss densities due to charge exchange events with neutrals.
   type (type_fp_src_snk_surf)  :: therm_src  ! /therm_src - Flux surface averaged source of particles and power due to particles of the distribution species being thermalised (merging into t
  real(DP),pointer  :: driven_currd(:,:) => null()     ! /driven_currd - Current density of non-thermal particles of the distribution species (excluding electron back current for fast ions) [A.m^-2]; Tim
  real(DP),pointer  :: trqd_jrxb(:,:) => null()     ! /trqd_jrxb - Torque density due to radial currents of non-thermal particles of the distribution species [N.m^-2]; Time-dependent; Matrix(ndist_
   type (type_fp_prof_surf_nucl_reac_th)  :: nucl_reac_th  ! /nucl_reac_th - Nuclear reactions between the cacluated species and oher species assumed to have thermal distributions.
   type (type_fp_prof_surf_nucl_reac_sf)  :: nucl_reac_sf  ! /nucl_reac_sf - Nuclear reactions of the calculated species with itself (thermal + non-thermal).
endtype

type type_fp_prof_vol_nucl_reac_sf  !    Nuclear reactions of the calculated species with itself (thermal + non-thermal).
  real(DP),pointer  :: rate(:,:) => null()     ! /rate - Reaction rate [1/s]; Time-dependent; Matrix (ndist_spec, npsin)
  real(DP),pointer  :: power(:,:) => null()     ! /power - Fusion reaction power[W]; Time-dependent; Matrxi (ndist_spec, npsin)
endtype

type type_fp_prof_vol_nucl_reac_th  !    Nuclear reactions between the calculated species and other species assumed to have thermal distributions.
  real(DP),pointer  :: rate(:,:,:) => null()     ! /rate - Reaction rate [1/s]; Time-dependent; Array3D (ndist_spec, max_nreac, npsin)
  real(DP),pointer  :: power(:,:,:) => null()     ! /power - Fusion reaction power[W]; Time-dependent; Array3D(ndist_spec, max_nreac,npsin)
endtype

type type_fp_nucl_reac_th  !    Nuclear reactions between the calculated species and other species assumed to have thermal distributions.
  real(DP),pointer  :: rate(:,:) => null()     ! /rate - Reaction rate [1/s]; Time-dependent; Matrix (ndist_spec, max_nreac)
  real(DP),pointer  :: power(:,:) => null()     ! /power - Fusion reaction power[W]; Time-dependent; Matrix(ndist_spec, max_nreac)
endtype

type type_fp_glob_dist_losses  !    Losses of the distribution species (orbit losses and neutralisation losses).
   type (type_fp_src_snk_tot)  :: orb_loss  ! /orb_loss - Losses due to orbits intersecting a material surface.
   type (type_fp_src_snk_tot)  :: neutr_loss  ! /neutr_loss - Losses due to neutralisation of distribution ions (charge exchange etc.)
endtype

type type_fp_glob_dist_energies  !    Different energy contents of the distribution species (global branch)
  real(DP),pointer  :: total(:) => null()     ! /total - Energy content of of a distribution species [J]; Time-dependent; Vector(ndist_spec)
  real(DP),pointer  :: perp(:) => null()     ! /perp - Perpendicular energy content of of a distribution species [J] Time-dependent; Vector(ndist_spec)
  real(DP),pointer  :: parallel(:) => null()     ! /parallel - Parallel energy content of of a distribution species [J] Time-dependent; Vector(ndist_spec)
endtype

type type_fp_glob_dist  !    Quantities related to the calculated distribution function (energy contents, sources, sinks  etc.)
   type (type_fp_glob_dist_energies)  :: energies  ! /energies - Different energy contents of the distribution species
   type (type_fp_glob_dist_losses)  :: losses  ! /losses - Losses of the distribution species (orbit losses and neutralisation losses).
   type (type_fp_src_snk_tot)  :: therm_src  ! /therm_src - Source particles and power due to particles of the distribution species being thermalised (merging into the thermal plasma).
  real(DP),pointer  :: driven_cur(:) => null()     ! /driven_cur - Current of non-thermal particles (excluding electron back current for fast ions) [A]; Time-dependent; Vector(ndist_spec).
  real(DP),pointer  :: trq_j_rxb(:) => null()     ! /trq_j_rxb - Torque due to radial currents of non-thermal particles [N.m]; Time-dependent; Vector(ndist_spec).
   type (type_fp_nucl_reac_th)  :: nucl_reac_th  ! /nucl_reac_th - Nuclear reactions between the calculated species and other species assumed to have thermal distributions.
   type (type_fp_nucl_reac_sf)  :: nucl_reac_sf  ! /nucl_reac_sf - Nuclear reactions of the calculated species with itself (thermal + non-thermal).
endtype

type type_fp_glob_coll  !    Quantities related to transfer from the calculated distribution function to background species via collisions.
  real(DP),pointer  :: power_i(:,:) => null()     ! /power_i - Collisional power to ions [W]; Time-dependent; Matrix(ndist_spec, nion)
  real(DP),pointer  :: power_e(:) => null()     ! /power_e - Collisional power to the electrons [W]; Time-dependent; Vector(ndist_spec)
  real(DP),pointer  :: trq_i(:,:) => null()     ! /trq_i - Collisional torque to background ions [N.m]; Time dependent; Matrix (ndist_spec, nion)
  real(DP),pointer  :: trq_e(:) => null()     ! /trq_e - Collisional torque to electrons [N.m]; Time dependent; Vector(ndist_spec)
endtype

type type_fp_prof_vol_coll  !    Quantities related to transfer from the calculated distribution function to background species via collisions.
  real(DP),pointer  :: power_i(:,:,:) => null()     ! /power_i - Collisional power to ions [W]; Time-dependent; Array3d(ndist_spec, nion, npsin)
  real(DP),pointer  :: power_e(:,:) => null()     ! /power_e - Collisional power to the electrons [W]; Time-dependent; Matrix(ndist_spec,npsin)
  real(DP),pointer  :: trq_i(:,:,:) => null()     ! /trq_i - Collisional torque to background ions [N.m]; Time dependent; Array3d (ndist_spec, nion, npsin)
  real(DP),pointer  :: trq_e(:,:) => null()     ! /trq_e - Collisional torque to electrons [N.m]; Time dependent; Matrix(ndist_spec,npsin)
endtype

type type_fp_prof_surf_coll  !    Quantities related to transfer from the calculated distribution function to background species via collisions.
  real(DP),pointer  :: powerd_i(:,:,:) => null()     ! /powerd_i - Collisional power to ions [W.m^-3]; Time-dependent; Array3d(ndist_spec, nion, npsin)
  real(DP),pointer  :: powerd_e(:,:) => null()     ! /powerd_e - Collisional power to the electrons [W.m^-3]; Time-dependent; Matrix(ndist_spec,npsin)
  real(DP),pointer  :: trqd_i(:,:,:) => null()     ! /trqd_i - Collisional torque to background ions [N.m^-2]; Time dependent; Array3d (ndist_spec, nion, npsin)
  real(DP),pointer  :: trqd_e(:,:) => null()     ! /trqd_e - Collisional torque to electrons [N.m^-2]; Time dependent; Matrix(ndist_spec,npsin)
endtype

type type_fp_prof_vol_dist_energies  !    Different energy contents of the distribution species (profiles/volume averaged branch)
  real(DP),pointer  :: total(:,:) => null()     ! /total - Energy content of of a distribution species [J]; Time-dependent; Matrix(ndist_spec,npsin)
  real(DP),pointer  :: perp(:,:) => null()     ! /perp - Perpendicular energy content of of a distribution species [J] Time-dependent; Matrix(ndist_spec,npsin)
  real(DP),pointer  :: parallel(:,:) => null()     ! /parallel - Parallel energy content of of a distribution species [J] Time-dependent; Matrix(ndist_spec,npsin)
endtype

type type_fp_prof_vol_dist  !    Quantities related to the calculated distribution function (energy contents, sources, sinks  etc.) ADD psin DIMENSION EVERYWHERE
   type (type_fp_prof_vol_dist_energies)  :: energies  ! /energies - Energy components of a distribution species.
   type (type_fp_prof_vol_dist_losses)  :: losses  ! /losses - Losses of the distribution species.
   type (type_fp_src_snk_vol)  :: therm_src  ! /therm_src - Source particles and power due to particles of the distribution species being thermalised (merging into the thermal plasma).
  real(DP),pointer  :: driven_cur(:,:) => null()     ! /driven_cur - Current of non-thermal particles (does not include electron back current for fast ions) [A]; Time-dependent; Matrix(ndist_spec,nps
  real(DP),pointer  :: trq_j_rxb(:,:) => null()     ! /trq_j_rxb - Torque due to radial currents of non-thermal particles of the distribution species [N.m]; Time-dependent; Matrix(ndist_spec,npsin)
   type (type_fp_prof_vol_nucl_reac_th)  :: nucl_reac_th  ! /nucl_reac_th - Nuclear reactions involving the distribution species and other species assumed to be thermal.
   type (type_fp_prof_vol_nucl_reac_sf)  :: nucl_reac_sf  ! /nucl_reac_sf - Nuclear reactions of the calculated species with itself (thermal + non-thermal).
endtype

type type_fp_dist_func  !    Distribution functions
  integer,pointer  :: sol_type(:) => null()      ! /sol_type - Solution type: 1 - full-f; 2 - delta-f. For the latter case delta-f is given by the test particles and the unperturbed distributio
   type (type_fp_test_part)  :: test_part  ! /test_part - Distribution given as a set of test particles.
   type (type_fp_ff)  :: f0  ! /f0 - Orbit averaged (or Bounce averaged) zero order distribution function.
   type (type_fp_ff)  :: fullf  ! /fullf - Orbit averaged (or Bounce averaged) full-f distribution function. 
endtype

type type_fp_glob  !    Global parameters (in most cases volume integrated and surface averaged quanatities).
   type (type_fp_glob_dist)  :: dist_spec  ! /dist_spec - Quantities related to the calculated distribution function (energy contents, sources, sinks  etc.)
   type (type_fp_glob_coll)  :: collision_bg  ! /collision_bg - Quantities related to transfer from the calculated distribution function to background species via collisions.
endtype

type type_fp_nucl_reac  !    Information on nuclear reactions involving the calculated species.
  integer,pointer  :: nreacs(:) => null()      ! /nreacs - Number of possible nuclear reactions (with background species and for different branches); Vector(ndist_spec)
  integer,pointer  :: point_reac(:,:) => null()     ! /point_reac - Pointer to a species in composition who can undergo a nuclear reaction with the calculated species; Matrix(ndist_spec, max_nreac)
  integer,pointer  :: id_reac(:,:) => null()     ! /id_reac - Identification of the reaction between the calculated species and a background species (including which branch if applicable); Tim
endtype

type type_fp_prof_vol  !    Volume integrated quantities (inside the given psin)
   type (type_fp_prof_vol_dist)  :: dist_spec  ! /dist_spec - Quantities related to the calculated distribution function (energy contents, sources, sinks  etc.)
   type (type_fp_prof_vol_coll)  :: collision_bg  ! /collision_bg - Quantities related to transfer from the calculated distribution function to background species via collisions.
endtype

type type_fp_prof_surf  !    Flux surface averaged quantities.
   type (type_fp_prof_surf_dist)  :: dist_spec  ! /dist_spec - Quantities related to the calculated distribution function (energy densities, sources, sinks  etc.)
   type (type_fp_prof_surf_coll)  :: collision_bg  ! /collision_bg - Quantities related to transfer from the calculated distribution function to background species via collisions.
endtype

type type_fp_profiles  !    Profiles (volume integrated and flux surface averaged)
  real(DP),pointer  :: psin(:) => null()     ! /psin - Grid points in normalized poloidal flux function; Vector(npsin)
   type (type_fp_prof_vol)  :: volume_intgr  ! /volume_intgr - Volume integrated quantities (inside the given psin)
   type (type_fp_prof_surf)  :: flux_surf_av  ! /flux_surf_av - Flux surface averaged quantities.
endtype

type type_dist_func  !    
  integer,pointer  :: sol_type(:) => null()      ! /fp/dist_func/sol_type - Solution type: 1 - full-f; 2 - delta-f. For the latter case delta-f is given by the test particles and the unperturbed distributio
   type (type_fp_test_part)  :: test_part  ! /fp/dist_func/test_part - Distribution given as a set of test particles.
   type (type_fp_ff)  :: f0  ! /fp/dist_func/f0 - Orbit averaged (or Bounce averaged) zero order distribution function.
   type (type_fp_ff)  :: fullf  ! /fp/dist_func/fullf - Orbit averaged (or Bounce averaged) full-f distribution function. 
endtype

type type_fp  !    
  type (type_datainfo) :: datainfo  ! /fp/datainfo - 
  type (type_composition) :: composition  ! /fp/composition - 
  integer,pointer  :: calc_spec(:) => null()      ! /fp/calc_spec - Pointer to the species for which the distribution function(s) is/are calculated and whose characteristics are given in composition
   type (type_fp_nucl_reac)  :: nucl_reac  ! /fp/nucl_reac - Information on nuclear reactions involving the calculated species.
   type (type_fp_glob)  :: global_param  ! /fp/global_param - Global parameters (in most cases volume integrated and surface averaged quanatities).
   type (type_fp_profiles)  :: profiles  ! /fp/profiles - Profiles (volume integrated and flux surface averaged)
  type (type_dist_func) :: dist_func  ! /fp/dist_func - Distribution functions
   type (type_fp_input_src)  :: input_src  ! /fp/input_src - Input sources of particles and power for the distribution species (to aid diagnosing the code output).
  real(DP)  :: time=-9.0D40       ! /fp/time - Time [s]; Time-dependent; Scalar
  type (type_codeparam) :: codeparam  ! /fp/codeparam - 
endtype

! ***********  Include eqgeometry.xsd
type type_eqgeometry  !    
  character(len=132), dimension(:), pointer ::source => null()       ! /eqgeometry/source - Comment describing the origin of the eqgeometry data; String
  integer  :: boundarytype=-999999999       ! /eqgeometry/boundarytype - 0 (limiter) or 1 (separatrix); Integer; Time-dependent; string;
   type (type_rz1D_npoints)  :: boundary  ! /eqgeometry/boundary - RZ description of the plasma boundary; Time-dependent;
   type (type_rz0D)  :: geom_axis  ! /eqgeometry/geom_axis - position of the geometric axis [m]; Time-dependent; Scalar
  real(DP)  :: a_minor=-9.0D40       ! /eqgeometry/a_minor - Minor radius of the plasma boundary [m]; Time-dependent; Scalar
  real(DP)  :: elongation=-9.0D40       ! /eqgeometry/elongation - Elongation of the plasma boundary; Time-dependent; Scalar
  real(DP)  :: tria_upper=-9.0D40       ! /eqgeometry/tria_upper - Upper triangularity of the plasma boundary; Time-dependent; Scalar
  real(DP)  :: tria_lower=-9.0D40       ! /eqgeometry/tria_lower - Lower triangularity of the plasma boundary; Time-dependent; Scalar
   type (type_rz1D)  :: xpts  ! /eqgeometry/xpts - Position of the Xpoints, first is the active xpoint if diverted [m]; Time-dependent; Vector (npoint)
   type (type_rz0D)  :: left_low_st  ! /eqgeometry/left_low_st - Position of the lower left strike point [m]; Time-dependent; Scalar
   type (type_rz0D)  :: right_low_st  ! /eqgeometry/right_low_st - Position of the lower right strike point [m]; Time-dependent; Scalar
   type (type_rz0D)  :: left_up_st  ! /eqgeometry/left_up_st - Position of the upper left strike point [m]; Time-dependent; Scalar
   type (type_rz0D)  :: right_up_st  ! /eqgeometry/right_up_st - Position of the upper right strike point [m]; Time-dependent; Scalar
   type (type_rz0D)  :: active_limit  ! /eqgeometry/active_limit - Position of the active limiter point (point of the plasma boundary in contact with the limiter) [m]; Set R = 0 for X-point plasma;
endtype

! ***********  Include eqglobal.xsd
type type_mag_axis  !    
   type (type_rz0D)  :: position  ! /global_param/mag_axis/position - Position of the magnetic axis [m]; Time-dependent; Scalar; 
  real(DP)  :: bphi=-9.0D40       ! /global_param/mag_axis/bphi - Total toroidal magnetic field at the magnetic axis [T]; Time-dependent; Scalar
  real(DP)  :: q=-9.0D40       ! /global_param/mag_axis/q - q at the magnetic axis; Time-dependent; Scalar
endtype

type type_global_param  !    
  real(DP)  :: beta_pol=-9.0D40       ! /global_param/beta_pol - poloidal beta; Time-dependent; Scalar
  real(DP)  :: beta_tor=-9.0D40       ! /global_param/beta_tor - toroidal beta; Time-dependent; Scalar
  real(DP)  :: beta_normal=-9.0D40       ! /global_param/beta_normal - normalised beta; Time-dependent; Scalar
  real(DP)  :: i_plasma=-9.0D40       ! /global_param/i_plasma - total toroidal plasma current [A]; Positive sign means anti-clockwise when viewed from above. Time-dependent; Scalar
  real(DP)  :: li=-9.0D40       ! /global_param/li - internal inductance; Time-dependent; Scalar
  real(DP)  :: volume=-9.0D40       ! /global_param/volume - total plasma volume [m^3]; Time-dependent; Scalar
  real(DP)  :: area=-9.0D40       ! /global_param/area - area poloidal cross section [m^2]; Time-dependent; Scalar
  real(DP)  :: psi_ax=-9.0D40       ! /global_param/psi_ax - poloidal flux at the magnetic axis [Wb]; Time-dependent; Scalar
  real(DP)  :: psi_bound=-9.0D40       ! /global_param/psi_bound - poloidal flux at the selected plasma boundary (separatrix for a free boundary code; fixed boundary for fixed boundary code) [Wb]; 
  type (type_mag_axis) :: mag_axis  ! /global_param/mag_axis - Magnetic axis values
  real(DP)  :: q_95=-9.0D40       ! /global_param/q_95 - q at the 95% poloidal flux surface; Time-dependent; Scalar
  real(DP)  :: q_min=-9.0D40       ! /global_param/q_min - minimum q value in the plasma; Time-dependent; Scalar
   type (type_b0r0)  :: toroid_field  ! /global_param/toroid_field - Characteristics of the vacuum toroidal field, redundant with the toroidfield CPO, to be used by the ETS
  real(DP)  :: w_mhd=-9.0D40       ! /global_param/w_mhd - Plasma energy content = 3/2 * int(p,dV) with p being the total pressure (thermal + fast particles). Time-dependent; Scalar
endtype

! ***********  Include eqprofiles2d.xsd
type type_grid  !    
  real(DP),pointer  :: dim1(:) => null()     ! /profiles_2d/grid/dim1 - First dimension values; Time-dependent; Vector (ndim1) 
  real(DP),pointer  :: dim2(:) => null()     ! /profiles_2d/grid/dim2 - Second dimension values; Time-dependent; Vector (ndim2) 
  integer,pointer  :: connect(:,:) => null()     ! /profiles_2d/grid/connect - In case of a finite elemnt representation, lists the points (3 for triangles, 4 for quadrangles) which define a finite element. In
endtype

type type_profiles_2d  !    
  character(len=132), dimension(:), pointer ::grid_type => null()       ! /profiles_2d/grid_type - Selection of one of a set of grid types
  type (type_grid) :: grid  ! /profiles_2d/grid - definition of the 2D grid
  real(DP),pointer  :: psi_grid(:,:) => null()     ! /profiles_2d/psi_grid - values of the poloidal flux at the grid in the poloidal plane [Wb]; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: jphi_grid(:,:) => null()     ! /profiles_2d/jphi_grid - toroidal plasma current density [A m-2]; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: jpar_grid(:,:) => null()     ! /profiles_2d/jpar_grid - parallel (to magnetic field) plasma current density [A m-2]; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: br(:,:) => null()     ! /profiles_2d/br - R component of the poloidal magnetic field at the specified grid [T]; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: bz(:,:) => null()     ! /profiles_2d/bz - Z component of the poloidal magnetic field at the specified grid [T]; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: bphi(:,:) => null()     ! /profiles_2d/bphi - toroidal component of the magnetic field at the specified grid [T]; Time-dependent; Matrix (ndim1, ndim2)
endtype

! ***********  Include eqprofiles.xsd
type type_profiles_1d  !    
  real(DP),pointer  :: psi(:) => null()     ! /profiles_1d/psi - Poloidal flux [Wb], without 1/2pi and such that Bp=|grad psi| /R/2/pi. Time-dependent; Vector (npsi)
  real(DP),pointer  :: phi(:) => null()     ! /profiles_1d/phi - toroidal flux [Wb]; Time-dependent; Vector (npsi)
  real(DP),pointer  :: pressure(:) => null()     ! /profiles_1d/pressure - pressure profile as a function of the poloidal flux [Pa]; Time-dependent; Vector (npsi)
  real(DP),pointer  :: F_dia(:) => null()     ! /profiles_1d/F_dia - diamagnetic profile (R B_phi) [T m]; Time-dependent; Vector (npsi)
  real(DP),pointer  :: pprime(:) => null()     ! /profiles_1d/pprime - psi derivative of the pressure profile [Pa/Wb]; Time-dependent; Vector (npsi)
  real(DP),pointer  :: ffprime(:) => null()     ! /profiles_1d/ffprime - psi derivative of F_dia multiplied with F_dia [T^2 m^2/Wb]; Time-dependent; Vector (npsi)
  real(DP),pointer  :: jphi(:) => null()     ! /profiles_1d/jphi - flux surface averaged toroidal current density = average(jphi/R) / average(1/R) [A/m^2]; Time-dependent; Vector (npsi)
  real(DP),pointer  :: jparallel(:) => null()     ! /profiles_1d/jparallel - flux surface averaged parallel current density = average(j.B) / B0, where B0 = equilibrium/global_param/toroid_field/b0 ; [A/m^2];
  real(DP),pointer  :: q(:) => null()     ! /profiles_1d/q - Safety factor = dphi/dpsi [-]; Time-dependent; Vector (npsi)
  real(DP),pointer  :: r_inboard(:) => null()     ! /profiles_1d/r_inboard - radial coordinate (major radius) at the height and on the left of the magnetic axis [m]; Time-dependent; Vector (npsi)
  real(DP),pointer  :: r_outboard(:) => null()     ! /profiles_1d/r_outboard - radial coordinate (major radius) at the height and on the right of the magnetic axis [m]; Time-dependent; Vector (npsi)
  real(DP),pointer  :: rho_tor(:) => null()     ! /profiles_1d/rho_tor - Toroidal flux coordinate [m], to be used by the ETS and in many CPOs (coreprof, ...). Defined as sqrt(phi/pi/B0), where B0 = equil
  real(DP),pointer  :: rho_vol(:) => null()     ! /profiles_1d/rho_vol - Normalised radial coordinate related to the plasma volume. Defined as sqrt(volume / volume[LCFS]). Time-dependent; Vector (npsi)
  real(DP),pointer  :: beta_pol(:) => null()     ! /profiles_1d/beta_pol - poloidal beta (inside the magnetic surface); Time-dependent; Vector (npsi)
  real(DP),pointer  :: li(:) => null()     ! /profiles_1d/li - internal inductance (inside the magnetic surface); Time-dependent; Vector (npsi)
  real(DP),pointer  :: elongation(:) => null()     ! /profiles_1d/elongation - Elongation; Time-dependent; Vector (npsi)
  real(DP),pointer  :: tria_upper(:) => null()     ! /profiles_1d/tria_upper - Upper triangularity profile; Time-dependent; Vector (npsi)
  real(DP),pointer  :: tria_lower(:) => null()     ! /profiles_1d/tria_lower - Lower triangularity profile; Time-dependent; Vector (npsi)
  real(DP),pointer  :: volume(:) => null()     ! /profiles_1d/volume - Volume enclosed in the flux surface [m^3]; Time-dependent; Vector (npsi)
  real(DP),pointer  :: vprime(:) => null()     ! /profiles_1d/vprime - Radial derivative of the volume enclosed in the flux surface, i.e. dV/drho_tor [m^2]; Time-dependent; Vector (npsi)
  real(DP),pointer  :: area(:) => null()     ! /profiles_1d/area - Cross-sectional area of the flux surface [m^2]; Time-dependent; Vector (npsi)
  real(DP),pointer  :: aprime(:) => null()     ! /profiles_1d/aprime - Radial derivative of the cross-sectional area of the flux surface, i.e. darea/drho_tor [m^2]; Time-dependent; Vector (npsi)
  real(DP),pointer  :: surface(:) => null()     ! /profiles_1d/surface - Surface area of the flux surface [m^2]; Time-dependent; Vector (npsi)
  real(DP),pointer  :: ftrap(:) => null()     ! /profiles_1d/ftrap - Trapped particle fraction; Time-dependent; Vector (npsi)
  real(DP),pointer  :: gm1(:) => null()     ! /profiles_1d/gm1 - average(1/R^2); Time-dependent; Vector (npsi)
  real(DP),pointer  :: gm2(:) => null()     ! /profiles_1d/gm2 - average(grad_rho^2/R^2); Time-dependent; Vector (npsi)
  real(DP),pointer  :: gm3(:) => null()     ! /profiles_1d/gm3 - average(grad_rho^2); Time-dependent; Vector (npsi)
  real(DP),pointer  :: gm4(:) => null()     ! /profiles_1d/gm4 - average(1/B^2) [T^-2]; Time-dependent; Vector (npsi)
  real(DP),pointer  :: gm5(:) => null()     ! /profiles_1d/gm5 - average(B^2) [T^2]; Time-dependent; Vector (npsi)
  real(DP),pointer  :: gm6(:) => null()     ! /profiles_1d/gm6 - average(grad_rho^2/B^2)  [T^-2]; Time-dependent; Vector (npsi)
  real(DP),pointer  :: gm7(:) => null()     ! /profiles_1d/gm7 - average(grad_rho); Time-dependent; Vector (npsi)
  real(DP),pointer  :: gm8(:) => null()     ! /profiles_1d/gm8 - average(R); Time-dependent; Vector (npsi)
  real(DP),pointer  :: gm9(:) => null()     ! /profiles_1d/gm9 - average(1/R); Time-dependent; Vector (npsi)
endtype

! ***********  Include equilibrium.xsd
type type_equilibrium  !    
  type (type_datainfo) :: datainfo  ! /equilibrium/datainfo - 
  type (type_eqconstraint) :: eqconstraint  ! /equilibrium/eqconstraint - 
  type (type_eqgeometry) :: eqgeometry  ! /equilibrium/eqgeometry - 
  type (type_flush) :: flush  ! /equilibrium/flush - 
  type (type_global_param) :: global_param  ! /equilibrium/global_param - 
  type (type_profiles_1d) :: profiles_1d  ! /equilibrium/profiles_1d - 
  type (type_profiles_2d) :: profiles_2d  ! /equilibrium/profiles_2d - 
  type (type_coord_sys) :: coord_sys  ! /equilibrium/coord_sys - 
  real(DP)  :: time=-9.0D40       ! /equilibrium/time - Time [s]; Time-dependent; Scalar
  type (type_codeparam) :: codeparam  ! /equilibrium/codeparam - 
endtype

! ***********  Include ironmodel.xsd
type type_permeability  !    
  real(DP),pointer  :: b(:,:) => null()     ! /ironmodel/desc_iron/permeability/b - List of B values for description of the mur(B) dependence [T]; Matrix (nsegment,nB)
  real(DP),pointer  :: mur(:,:) => null()     ! /ironmodel/desc_iron/permeability/mur - Relative permeability mur(B) [dimensionless]; Matrix (nsegment,nB)
endtype

type type_geom_iron  !    
  integer,pointer  :: npoints(:) => null()      ! /ironmodel/desc_iron/geom_iron/npoints - Number of points describing an element (irregular outline rzcoordinate); Vector (nsegment)
   type (type_rz2D)  :: rzcoordinate  ! /ironmodel/desc_iron/geom_iron/rzcoordinate - Irregular outline [m]; 2D arrays (nsegment,max_npoints)
endtype

type type_desc_iron  !    
  character(len=132), dimension(:), pointer ::name => null()       ! /ironmodel/desc_iron/name - Name of circuit. Array of strings (ncircuit).
  character(len=132), dimension(:), pointer ::id => null()       ! /ironmodel/desc_iron/id - ID of circuit.  Array of strings (ncircuit).
  type (type_permeability) :: permeability  ! /ironmodel/desc_iron/permeability - Permeability model (can be different for each iron segment)
  type (type_geom_iron) :: geom_iron  ! /ironmodel/desc_iron/geom_iron - Geometry of the iron segments
endtype

type type_magnetise  !    
   type (type_exp1D)  :: mr  ! /ironmodel/magnetise/mr - Magnetisation along the R axis [T]; Time-dependent; Vector (nsegment)
   type (type_exp1D)  :: mz  ! /ironmodel/magnetise/mz - Magnetisation along the Z axis [T]; Time-dependent; Vector (nsegment)
endtype

type type_ironmodel  !    
  type (type_datainfo) :: datainfo  ! /ironmodel/datainfo - 
  type (type_desc_iron) :: desc_iron  ! /ironmodel/desc_iron - Description of the iron segments
  type (type_magnetise) :: magnetise  ! /ironmodel/magnetise - Magnetisation M of the iron segment, assumed to be constant inside a given iron segment. Reminder : H = 1/mu0 * B - mur * M; [A/m]
  real(DP)  :: time=-9.0D40       ! /ironmodel/time - Time [s]; Time-dependent; Scalar
endtype

! ***********  Include launchs.xsd
type type_spectrum  !    
  integer,pointer  :: nn_phi(:) => null()      ! /launchs/spectrum/nn_phi - Number of points for the discretization of the spectrum in the toroidal direction, Vector of integers (nantenna).
  integer,pointer  :: nn_theta(:) => null()      ! /launchs/spectrum/nn_theta - Number of points for the discretization of the spectrum in the poloidal direction, Vector of integers (nantenna).
  real(DP),pointer  :: n_phi(:,:) => null()     ! /launchs/spectrum/n_phi - Refraction index in the toroidal direction, Matrix (nantenna,max_nn_phi).
  real(DP),pointer  :: n_theta(:,:) => null()     ! /launchs/spectrum/n_theta - Refraction index in poloidal direction, Matrix (nantenna,max_nn_theta).
  real(DP),pointer  :: power(:,:,:) => null()     ! /launchs/spectrum/power - W/dNphi/dNtheta [W], Array (nantenna, max_nn_phi, max_nn_theta). Time-dependent
endtype

type type_launchs  !    
  type (type_datainfo) :: datainfo  ! /launchs/datainfo - 
  character(len=132), dimension(:), pointer ::name => null()       ! /launchs/name - Antenna name, Vector of strings (nantenna)
  character(len=132), dimension(:), pointer ::type => null()       ! /launchs/type - Wave type (LH, EC, IC, ...), Vector of strings (nantenna)
  real(DP),pointer  :: frequency(:) => null()     ! /launchs/frequency - Wave frequency [Hz], Vector (nantenna).
  integer,pointer  :: mode(:) => null()      ! /launchs/mode - Incoming wave mode (defined as in the antennas CPO). Vector of integers (nantenna). Time-dependent
   type (type_rzphi1D)  :: position  ! /launchs/position - Reference global position of the antenna. Time-dependent
  type (type_spectrum) :: spectrum  ! /launchs/spectrum - Spectral properties of the wave.
   type (type_rf_beam)  :: beam  ! /launchs/beam - Beam characteristics
  type (type_codeparam) :: codeparam  ! /launchs/codeparam - 
  real(DP)  :: time=-9.0D40       ! /launchs/time - Time [s]; Time-dependent; Scalar
endtype

! ***********  Include limiter.xsd
type type_limiter  !    
  type (type_datainfo) :: datainfo  ! /limiter/datainfo - 
   type (type_rz1D)  :: position  ! /limiter/position - Position (R,Z coordinates) of the limiter [m]; Vector(npoints)
endtype

! ***********  Include magdiag.xsd
type type_setup_floops  !    
  character(len=132), dimension(:), pointer ::name => null()       ! /magdiag/flux_loops/setup_floops/name - Name of loop. Array of strings (nloops).
  character(len=132), dimension(:), pointer ::id => null()       ! /magdiag/flux_loops/setup_floops/id - ID of loop. Array of strings (nloops).
   type (type_rzphi2D)  :: position  ! /magdiag/flux_loops/setup_floops/position - List of (R,Z,phi) points defining the position of the loop (see data structure documentation FLUXLOOPposition.pdf); Matrices (nloo
  integer,pointer  :: npoints(:) => null()      ! /magdiag/flux_loops/setup_floops/npoints - Number of points describing each loop in the "position" matrices. Vector (nloops)
endtype

type type_setup_bprobe  !    
  character(len=132), dimension(:), pointer ::name => null()       ! /magdiag/bpol_probes/setup_bprobe/name - Name of the probe.  Array of strings (nprobes).
  character(len=132), dimension(:), pointer ::id => null()       ! /magdiag/bpol_probes/setup_bprobe/id - ID of the probe.  Array of strings (nprobes).
   type (type_rz1D)  :: position  ! /magdiag/bpol_probes/setup_bprobe/position - RZ of coil centre [m]; Vector (nprobes)
  real(DP),pointer  :: polangle(:) => null()     ! /magdiag/bpol_probes/setup_bprobe/polangle - Poloidal angle of coil orientation (w.r.t. horizontal ?? to be checked) [rad]; Vector (nprobes)
  real(DP),pointer  :: torangle(:) => null()     ! /magdiag/bpol_probes/setup_bprobe/torangle - Toroidal angle of coil orientation (0 if fully in the poloidal plane) [rad] ; Vector (nprobes)
  real(DP),pointer  :: area(:) => null()     ! /magdiag/bpol_probes/setup_bprobe/area - Area of coil [m^2]; Vector (nprobes)
  real(DP),pointer  :: length(:) => null()     ! /magdiag/bpol_probes/setup_bprobe/length - Length of coil [m]; Vector (nprobes)
  integer,pointer  :: turns(:) => null()      ! /magdiag/bpol_probes/setup_bprobe/turns - Turns in the coil; Vector (nprobes)
endtype

type type_flux_loops  !    
  type (type_setup_floops) :: setup_floops  ! /magdiag/flux_loops/setup_floops - diagnostic setup information
   type (type_exp1D)  :: measure  ! /magdiag/flux_loops/measure - Measured flux [Wb]; Time-dependent; Vector (nloops)
endtype

type type_bpol_probes  !    
  type (type_setup_bprobe) :: setup_bprobe  ! /magdiag/bpol_probes/setup_bprobe - diagnostic setup information
   type (type_exp1D)  :: measure  ! /magdiag/bpol_probes/measure - Measured value [T]; Time-dependent; Vector (nprobes)
endtype

type type_magdiag  !    
  type (type_datainfo) :: datainfo  ! /magdiag/datainfo - 
   type (type_exp0D)  :: ip  ! /magdiag/ip - Plasma current [A]. Positive sign means anti-clockwise when viewed from above.  Time-dependent. Scalar
   type (type_exp0D)  :: diamagflux  ! /magdiag/diamagflux - Diamagnetic flux [Wb]; Time-dependent; Scalar
  type (type_flux_loops) :: flux_loops  ! /magdiag/flux_loops - Poloidal flux loops RZ coordinates have 1 component for the full loop and two if there is a negative reference loop
  type (type_bpol_probes) :: bpol_probes  ! /magdiag/bpol_probes - Poloidal field probes
  real(DP)  :: time=-9.0D40       ! /magdiag/time - Time [s]; Time-dependent; Scalar
endtype

! ***********  Include msediag.xsd
type type_setup_mse  !    
   type (type_rz1D)  :: rzgamma  ! /msediag/setup_mse/rzgamma - RZ of intersection between beam and line of sight [m]; Vector (nchords)
  real(DP),pointer  :: geom_coef(:,:) => null()     ! /msediag/setup_mse/geom_coef - Geometric coefficients (6) describing the angle between beam and line of sight; The first dimension contains succesively : numerat
endtype

type type_msediag  !    
  type (type_datainfo) :: datainfo  ! /msediag/datainfo - 
  type (type_setup_mse) :: setup_mse  ! /msediag/setup_mse - diagnostic setup information
   type (type_exp1D)  :: measure  ! /msediag/measure - Measured value (MSE angle gamma [rad]). Time-dependent; Vector (nchords)
  real(DP)  :: time=-9.0D40       ! /msediag/time - Time [s]; Time-dependent; Scalar
endtype

! ***********  Include mhd.xsd
type type_mhd  !    
  type (type_datainfo) :: datainfo  ! /mhd/datainfo - 
  integer,pointer  :: n(:) => null()      ! /mhd/n - Toroidal mode number; Time-dependent; Vector (nn)
  integer,pointer  :: m(:,:) => null()     ! /mhd/m - Poloidal mode number; Time-dependent; Matrix (nn,nm)
  real(DP),pointer  :: psi(:) => null()     ! /mhd/psi - Position in poloidal flux [Wb] (without 1/2pi and such that Bp=|grad psi| /R/2/pi). Time-dependent; Vector (npsi)
  real(DP),pointer  :: frequency(:) => null()     ! /mhd/frequency - Frequency of the mode [Hz]; Time-dependent; Vector (nn)
  real(DP),pointer  :: growthrate(:) => null()     ! /mhd/growthrate - Linear growthrate of the mode [Hz]; Time-dependent; Vector (nn)
  real(DP),pointer  :: disp_perp(:,:,:) => null()     ! /mhd/disp_perp - Perpendicular displacement of the mode [m]; Time-dependent; Array 3D (npsi,nn,nm)
  real(DP),pointer  :: disp_par(:,:,:) => null()     ! /mhd/disp_par - Parallel displacement of the mode [m]; Time-dependent; Array 3D (npsi,nn,nm)
  real(DP),pointer  :: tau_alfven(:) => null()     ! /mhd/tau_alfven - Alven time=R/vA=R0 sqrt(mi ni(rho))/B0 [s]; Definitions of R0, BO, mi, ni to be clarified. rho grid should be included in the MHD 
  real(DP),pointer  :: tau_resistive(:) => null()     ! /mhd/tau_resistive - Resistive time = mu_0 rho*rho/1.22/eta_neo [s]; Source of eta_neo to be clarified. Time-dependent; Vector (npsi)
  real(DP)  :: time=-9.0D40       ! /mhd/time - Time [s]; Time-dependent; Scalar.
  type (type_codeparam) :: codeparam  ! /mhd/codeparam - 
endtype

! ***********  Include neoclassic.xsd
type type_neoclassic  !    
  type (type_datainfo) :: datainfo  ! /neoclassic/datainfo - 
  real(DP),pointer  :: rho_tor_norm(:) => null()     ! /neoclassic/rho_tor_norm - Normalised toroidal flux coordinate values (= rho_tor normalised to the value at the last grid point); Vector (nrho)
  real(DP),pointer  :: rho_tor(:) => null()     ! /neoclassic/rho_tor - Toroidal flux coordinate (not normalised, equivalent to rho_tor_norm) [m];  Vector (nrho). Time-dependent.
   type (type_transcoefion)  :: ni_neo  ! /neoclassic/ni_neo - Neoclassical transport coefficients for ion density equation. Time-dependent.
   type (type_transcoefel)  :: ne_neo  ! /neoclassic/ne_neo - Neoclassical transport coefficients for electron density equation. Time-dependent.
   type (type_transcoefimp)  :: nz_neo  ! /neoclassic/nz_neo - Neoclassical transport coefficients for impurity (multiple charge state) density equation. Time-dependent.
   type (type_transcoefion)  :: ti_neo  ! /neoclassic/ti_neo - Neoclassical transport coefficients for ion temperature equation. Time-dependent.
   type (type_transcoefel)  :: te_neo  ! /neoclassic/te_neo - Neoclassical transport coefficients for electron temperature equation. Time-dependent.
   type (type_transcoefimp)  :: tz_neo  ! /neoclassic/tz_neo - Neoclassical transport coefficients for impurity  (multiple charge state) temperature equation. Time-dependent.
   type (type_transcoefel)  :: mtor_neo  ! /neoclassic/mtor_neo - Neoclassical transport coefficients for total toroidal momentum equation. Time-dependent.
  real(DP),pointer  :: sigma(:) => null()     ! /neoclassic/sigma - Neoclassical conductivity [ohm^-1.m^-1]. Time-dependent. Vector(nrho).
  real(DP),pointer  :: jboot(:) => null()     ! /neoclassic/jboot - Bootstrap current density [A.m^-2]. Time-dependent. Vector(nrho).
  real(DP),pointer  :: er(:) => null()     ! /neoclassic/er - Radial electric field [V/m]. Time-dependent. Vector(nrho).
  real(DP),pointer  :: vpol(:,:) => null()     ! /neoclassic/vpol - Neoclassical poloidal rotation of for each ion species [m/s]. Time-dependent. Matrix(nrho,nion).
  real(DP),pointer  :: fext(:,:,:) => null()     ! /neoclassic/fext - Moments of parallel external force on each ion species [T.J.m^-3]. Time-dependent. Array3D(nrho,nion,nmoment).
  real(DP),pointer  :: jext(:) => null()     ! /neoclassic/jext - Current density response to fext [A.m^-2]. Time-dependent. Vector(nrho).
  real(DP)  :: time=-9.0D40       ! /neoclassic/time - Time [s]; Time-dependent; Scalar.
  type (type_codeparam) :: codeparam  ! /neoclassic/codeparam - 
endtype

! ***********  Include orbit.xsd
type type_orbit_pos  !    Complex type for orbit position (Vector)
  real(DP),pointer  :: r(:) => null()     ! /r - Major radius [m]; Time-dependent; Vector (norbits). 
  real(DP),pointer  :: z(:) => null()     ! /z - Altitude [m]; Time-dependent; Vector (norbits).
  real(DP),pointer  :: psi(:) => null()     ! /psi - Position in psi [normalised poloidal flux]; Time-dependent; Vector (norbits).
  real(DP),pointer  :: theta_b(:) => null()     ! /theta_b - Poloidal Boozer angle [rad]; Time-dependent; Vector (norbits). 
endtype

type type_midplane  !    
   type (type_orbit_pos)  :: outer  ! /orbit/orb_glob_dat/special_pos/midplane/outer - Position at outer mid-plane
   type (type_orbit_pos)  :: inner  ! /orbit/orb_glob_dat/special_pos/midplane/inner - Position at inner mid-plane
endtype

type type_turning_pts  !    
   type (type_orbit_pos)  :: upper  ! /orbit/orb_glob_dat/special_pos/turning_pts/upper - Position at upper turning point
   type (type_orbit_pos)  :: lower  ! /orbit/orb_glob_dat/special_pos/turning_pts/lower - Position at lower turning point
endtype

type type_special_pos  !    
  type (type_midplane) :: midplane  ! /orbit/orb_glob_dat/special_pos/midplane - Intersections with the midplane
  type (type_turning_pts) :: turning_pts  ! /orbit/orb_glob_dat/special_pos/turning_pts - Location of turning points
endtype

type type_orbitt_id  !    
  real(DP)  :: amn=-9.0D40       ! /orbit/orbitt_id/amn - Atomic mass of the ion; Scalar
  real(DP)  :: zion=-9.0D40       ! /orbit/orbitt_id/zion - Atomic charge of the ion; Scalar
  real(DP),pointer  :: energy(:) => null()     ! /orbit/orbitt_id/energy - Energy of the ion [keV]; Time-dependent; Vector (norbits).
  real(DP),pointer  :: magn_mom(:) => null()     ! /orbit/orbitt_id/magn_mom - Magnetic momentum [kg m^2 / s^2 / T]; Time-dependent, Vector(norbits).
  real(DP),pointer  :: p_phi(:) => null()     ! /orbit/orbitt_id/p_phi - toroidal angular momentum [kg m^2 / s]; Time-dependent; Vector(norbits);
  integer,pointer  :: sigma(:) => null()      ! /orbit/orbitt_id/sigma - Sign of parallel velocity at psi=psi_max along the orbit; Time-dependent; Vector(norbits)
endtype

type type_orb_trace  !    
  real(DP),pointer  :: time_orb(:,:) => null()     ! /orbit/orb_trace/time_orb - Time along the orbit  [s]; Time-dependent; Matrix (norbits, max_ntorb)
  integer,pointer  :: ntorb(:) => null()      ! /orbit/orb_trace/ntorb - Number of time slices along the orbit, for each orbit. Time-dependent; Vector (norbits)
  real(DP),pointer  :: r(:,:) => null()     ! /orbit/orb_trace/r - Major radius of the guiding centre [m], Major radius; Time-dependent; Matrix (norbits, max_ntorb). 
  real(DP),pointer  :: z(:,:) => null()     ! /orbit/orb_trace/z - Altitude of the guiding centre [m]; Time-dependent; Matrix (norbits, max_ntorb).
  real(DP),pointer  :: psi(:,:) => null()     ! /orbit/orb_trace/psi - Guiding centre position in psi [normalised poloidal flux]; Time-dependent; Matrix (norbits, max_ntorb)).
  real(DP),pointer  :: theta_b(:,:) => null()     ! /orbit/orb_trace/theta_b - Position of the guiding centre in poloidal Boozer angle [rad]; Time-dependent; Matrix (norbits, max_ntorb). 
  real(DP),pointer  :: v_parallel(:,:) => null()     ! /orbit/orb_trace/v_parallel - Parallel velocity along the orbit [m/s]; Time-dependent; Matrix (norbits, max_ntorb).
  real(DP),pointer  :: v_perp(:,:) => null()     ! /orbit/orb_trace/v_perp - Perpendicular velocity along the orbit [m/s]; Time-dependent; Matrix (norbits, max_ntorb).
endtype

type type_orb_glob_dat  !    
  integer,pointer  :: orbit_type(:) => null()      ! /orbit/orb_glob_dat/orbit_type - Identifier of orbit type: 0 trapped, -1 co-passing, + 1 counter-passing ; Time-dependent; Vector (norbits)
  real(DP),pointer  :: omega_b(:) => null()     ! /orbit/orb_glob_dat/omega_b - Bounce angular frequency rad/s; Time-dependent; Vector (norbits)
  real(DP),pointer  :: omega_phi(:) => null()     ! /orbit/orb_glob_dat/omega_phi - Toroidal angular precession frequency [rad/s]; Time-dependent; Vector (norbits).
  real(DP),pointer  :: omega_c_av(:) => null()     ! /orbit/orb_glob_dat/omega_c_av - Orbit averaged cyclotron frequency [rad/a]; Time-dependent; Vector(norbits).
  type (type_special_pos) :: special_pos  ! /orbit/orb_glob_dat/special_pos - Special positions along an orbit (like turning points).
endtype

type type_orbit  !    
  type (type_datainfo) :: datainfo  ! /orbit/datainfo - 
  type (type_orbitt_id) :: orbitt_id  ! /orbit/orbitt_id - Parameters identifying an orbit
  type (type_orb_trace) :: orb_trace  ! /orbit/orb_trace - Position of particle in 5D space (3D in real and 2D in velocity).
  type (type_orb_glob_dat) :: orb_glob_dat  ! /orbit/orb_glob_dat - Global quantities associated with an orbit.
  type (type_codeparam) :: codeparam  ! /orbit/codeparam - 
  real(DP)  :: time=-9.0D40       ! /orbit/time - Time [s]; Time-dependent; Scalar
endtype

! ***********  Include pfgeometry.xsd
type type_pfgeometry  !    
  integer,pointer  :: type(:,:) => null()     ! /pfgeometry/type - Type used to describe a coil shape (0 for 'rzcoordinates' or 1 for 'rzdrdz'); Matrix of integers (ncoils,max_nelements)
  integer,pointer  :: npoints(:,:) => null()     ! /pfgeometry/npoints - Number of points describing an element (irregular outline rzcoordinates); Matrix (ncoils,max_nelements)
   type (type_rz3D)  :: rzcoordinate  ! /pfgeometry/rzcoordinate - Irregular outline [m]; 3D arrays (ncoils,max_nelements,max_npoints)
  real(DP),pointer  :: rzdrdz(:,:,:) => null()     ! /pfgeometry/rzdrdz - 4-vector defining Centre R,Z and full extents dR, dZ [m]; 3D Array (ncoils,max_nelements,4)
endtype

! ***********  Include pfelement.xsd
type type_pfelement  !    
  character(len=132), dimension(:), pointer ::name => null()       ! /pfelement/name - Name of this element. Should be a matrix of strings (ncoils,max_nelements), but not supported by the UAL yet.
  character(len=132), dimension(:), pointer ::id => null()       ! /pfelement/id - ID of this element. Should be a matrix of strings (ncoils,max_nelements), but not supported by the UAL yet.
  real(DP),pointer  :: turnsign(:,:) => null()     ! /pfelement/turnsign - Sign of turn and fraction of a turn for calculating magnetic field of the Element; Matrix (ncoils,max_nelements)
  real(DP),pointer  :: area(:,:) => null()     ! /pfelement/area - Surface area of this element [m^2]; Matrix (ncoils,max_nelements)
  type (type_pfgeometry) :: pfgeometry  ! /pfelement/pfgeometry - 
endtype

! ***********  Include pfcircuits.xsd
type type_pfcircuits  !    
  character(len=132), dimension(:), pointer ::name => null()       ! /pfcircuits/name - Name of circuit, array of strings (ncircuits)
  character(len=132), dimension(:), pointer ::id => null()       ! /pfcircuits/id - ID of circuit, array of strings (ncircuits)
  character(len=132), dimension(:), pointer ::type => null()       ! /pfcircuits/type - Type of circuit, array of strings (ncircuits)
  integer,pointer  :: nnodes(:) => null()      ! /pfcircuits/nnodes - Number of nodes used to describe a circuit. Vector (ncircuits)
  integer,pointer  :: connections(:,:,:) => null()     ! /pfcircuits/connections - Description of the supplies and coils connections (nodes) across each circuit. Array 3D (ncircuits,max_nnodes,2*ncomponents), desc
endtype

! ***********  Include pfcoils.xsd
type type_desc_pfcoils  !    
  character(len=132), dimension(:), pointer ::name => null()       ! /pfcoils/desc_pfcoils/name - Name of coil. Array of strings (ncoils)
  character(len=132), dimension(:), pointer ::id => null()       ! /pfcoils/desc_pfcoils/id - ID of coil. Array of strings (ncoils)
  real(DP),pointer  :: res(:) => null()     ! /pfcoils/desc_pfcoils/res - Coil resistance [Ohm]; Vector (ncoils)
  real(DP),pointer  :: emax(:) => null()     ! /pfcoils/desc_pfcoils/emax - Maximum Energy to be dissipated in coils [J]; Vector (ncoils)
  integer,pointer  :: nelement(:) => null()      ! /pfcoils/desc_pfcoils/nelement - Number of elements used to describe a coil; Vector (ncoils)
  type (type_pfelement) :: pfelement  ! /pfcoils/desc_pfcoils/pfelement - Filament describing part of the coil as a rectangle
endtype

type type_pfcoils  !    
  type (type_desc_pfcoils) :: desc_pfcoils  ! /pfcoils/desc_pfcoils - Description of the coils
   type (type_exp1D)  :: coilcurrent  ! /pfcoils/coilcurrent - Circuit feed current in the coil , defined positive if it flows from point 1 to point 2 of the component in the pfcircuit descript
   type (type_exp1D)  :: coilvoltage  ! /pfcoils/coilvoltage - Voltage on the full coil [V]; Time-dependent; Vector (ncoils)
endtype

! ***********  Include pfpassive.xsd
type type_pfpageometry  !    
  integer,pointer  :: type(:) => null()      ! /pfpassive/pfpageometry/type - Type used to describe the shape (0 for 'rzcoordinates' or 1 for 'rzdrdz'); Vector of integers (nelements)
  integer,pointer  :: npoints(:) => null()      ! /pfpassive/pfpageometry/npoints - Number of points describing an element (irregular outline rzcoordinates); Vector of integers (nelements)
   type (type_rz2D)  :: rzcoordinate  ! /pfpassive/pfpageometry/rzcoordinate - Irregular outline [m]; Matrix (nelements,max_npoints)
  real(DP),pointer  :: rzdrdz(:,:) => null()     ! /pfpassive/pfpageometry/rzdrdz - 4-vector defining Centre R,Z and full extents dR, dZ [m]; Matrix (nelements,4)
endtype

type type_pfpassive  !    
  real(DP),pointer  :: area(:) => null()     ! /pfpassive/area - Surface area of this passive element [m^2]; Vector (nelements)
  real(DP),pointer  :: res(:) => null()     ! /pfpassive/res - Passive element resistance [Ohm]; Vector (nelements)
  type (type_pfpageometry) :: pfpageometry  ! /pfpassive/pfpageometry - Geometry of the passive elements
endtype

! ***********  Include pfsupplies.xsd
type type_filter  !    
  real(DP),pointer  :: num(:,:) => null()     ! /pfsupplies/desc_supply/filter/num - Coefficients of the numerator, in increasing order : a0 + a1*s + ... + an*s^n; Matrix (nsupplies,n)
  real(DP),pointer  :: den(:,:) => null()     ! /pfsupplies/desc_supply/filter/den - Coefficients of the denominator, in increasing order : b0 + b1*s + ... + bm*s^m; Matrix (nsupplies,m)
endtype

type type_desc_supply  !    
  character(len=132), dimension(:), pointer ::name => null()       ! /pfsupplies/desc_supply/name - Name of the supply; Array of strings (nsupplies)
  character(len=132), dimension(:), pointer ::id => null()       ! /pfsupplies/desc_supply/id - ID of the supply; Array of strings (nsupplies)
  character(len=132), dimension(:), pointer ::type => null()       ! /pfsupplies/desc_supply/type - Type of supply; Array of strings (nsupplies)
  real(DP),pointer  :: delay(:) => null()     ! /pfsupplies/desc_supply/delay - Pure delay in the supply [s]; Vector (nsupplies)
  type (type_filter) :: filter  ! /pfsupplies/desc_supply/filter - Laplace proper filter 
  real(DP),pointer  :: imin(:) => null()     ! /pfsupplies/desc_supply/imin - Minimum current [A]; Vector (nsupplies)
  real(DP),pointer  :: imax(:) => null()     ! /pfsupplies/desc_supply/imax - Maximum current [A]; Vector (nsupplies)
  real(DP),pointer  :: res(:) => null()     ! /pfsupplies/desc_supply/res - Supply internal resistance [Ohm]; Vector (nsupplies)
  real(DP),pointer  :: umin(:) => null()     ! /pfsupplies/desc_supply/umin - Minimum voltage [V]; Vector (nsupplies)
  real(DP),pointer  :: umax(:) => null()     ! /pfsupplies/desc_supply/umax - Maximum voltage [V]; Vector (nsupplies)
  real(DP),pointer  :: emax(:) => null()     ! /pfsupplies/desc_supply/emax - Maximum Energy to be dissipated in supply [J]; Vector (nsupplies)
endtype

type type_pfsupplies  !    
  type (type_desc_supply) :: desc_supply  ! /pfsupplies/desc_supply - Description of the power supplies
   type (type_exp1D)  :: voltage  ! /pfsupplies/voltage - Voltage at the supply output [V]; Time-dependent; Vector  (nsupplies)
   type (type_exp1D)  :: current  ! /pfsupplies/current - Current at the supply output, defined positive if it flows from point 1 to point 2 of the component in the pfcircuit description [
endtype

! ***********  Include pfsystems.xsd
type type_pfsystems  !    
  type (type_datainfo) :: datainfo  ! /pfsystems/datainfo - 
  type (type_pfcoils) :: pfcoils  ! /pfsystems/pfcoils - 
  type (type_pfpassive) :: pfpassive  ! /pfsystems/pfpassive - 
  type (type_pfcircuits) :: pfcircuits  ! /pfsystems/pfcircuits - 
  type (type_pfsupplies) :: pfsupplies  ! /pfsystems/pfsupplies - 
  real(DP)  :: time=-9.0D40       ! /pfsystems/time - Time [s]; Time-dependent; Scalar
endtype

! ***********  Include toroidalfield.xsd
type type_toroidfield  !    
  type (type_datainfo) :: datainfo  ! /toroidfield/datainfo - 
  integer  :: nturns=-999999999       ! /toroidfield/nturns - Number of total turns in the toroidal field coil
  integer  :: ncoils=-999999999       ! /toroidfield/ncoils - Number of packets of coils
   type (type_exp0D)  :: current  ! /toroidfield/current - Current in the toroidal field coils [A]; Time-dependent. Scalar.
   type (type_exp0D)  :: bvac_r  ! /toroidfield/bvac_r - Vacuum field times radius in the toroidal field magnet [T.m]. Positive sign means anti-clockwise when viewed from above. Time-depe
  real(DP)  :: r0=-9.0D40       ! /toroidfield/r0 - Characteristic major radius of the device (used in publications, usually middle of the vessel at the equatorial midplane) [m]. Sca
  real(DP)  :: time=-9.0D40       ! /toroidfield/time - Time [s]; Time-dependent. Scalar.
endtype

! ***********  Include sawteeth.xsd
type type_sawteeth_profiles1d  !    Core profiles after sawtooth crash
  real(DP),pointer  :: ne(:) => null()     ! /ne - Electron density [m^-3]. Time-dependent. Vector (nrho).
  real(DP),pointer  :: ni(:,:) => null()     ! /ni - Ion density [m^-3]. Time-dependent. Matrix (nrho,nion).
  real(DP),pointer  :: te(:) => null()     ! /te - Electron temperature [eV]. Time-dependent. Vector (nrho).
  real(DP),pointer  :: ti(:,:) => null()     ! /ti - Ion temperature [eV]. Time-dependent. Matrix (nrho,nion).
  real(DP),pointer  :: psi(:) => null()     ! /psi - Poloidal flux [Wb], without 1/2pi and such that Bp=|grad psi| /R/2/pi. Time-dependent. Vector (nrho).
  real(DP),pointer  :: phi(:) => null()     ! /phi - Toroidal flux [Wb]. Time-dependent. Vector (nrho).
  real(DP),pointer  :: psistar(:) => null()     ! /psistar - Psi* = psi - phi [Wb]. Time-dependent. Vector (nrho).
  real(DP),pointer  :: volume(:) => null()     ! /volume - Volume enclosed in the flux surface [m^3]. Required to ensure particle and energy conservation during reconnection process (ndV an
  real(DP),pointer  :: q(:) => null()     ! /q - Safety factor = dphi/dpsi [-]. Time-dependent. Vector (nrho).
endtype

type type_sawteeth_diags  !    Inversion and mixing radii
  real(DP)  :: shear1=-9.0D40       ! /shear1 - Magnetic shear at q = 1 [-]. Time-dependent. Real scalar.
  real(DP)  :: rhotorn_q1=-9.0D40       ! /rhotorn_q1 - Rho_tor_norm at q=1 radius [-]. Time-dependent. Real scalar.
  real(DP)  :: rhotorn_inv=-9.0D40       ! /rhotorn_inv - Rho_tor_norm at inversion radius [-]. Time-dependent. Real scalar.
  real(DP)  :: rhotorn_mix=-9.0D40       ! /rhotorn_mix - Rho_tor_norm at mixing radius [-]. Time-dependent. Real scalar.
endtype

type type_sawteeth  !    
  type (type_datainfo) :: datainfo  ! /sawteeth/datainfo - 
  integer  :: crash_trig=-999999999       ! /sawteeth/crash_trig - Flag indicating whether a crash condition has been satisfied : 0 = no crash. N(>0) = crash triggered due to condition ii=N. Intege
  type (type_composition) :: composition  ! /sawteeth/composition - 
  real(DP),pointer  :: rho_tor_norm(:) => null()     ! /sawteeth/rho_tor_norm - Normalised toroidal flux coordinate values (= rho_tor normalised to the value at the last grid point); Vector (nrho)
  real(DP),pointer  :: rho_tor(:) => null()     ! /sawteeth/rho_tor - Toroidal flux coordinate [m] given by sqrt(phi/B0/pi), where B0 = toroidfield%bvac_r%value / toroidfield%r0. Vector (nrho). Time-d
   type (type_sawteeth_profiles1d)  :: profiles1d  ! /sawteeth/profiles1d - Core profiles after sawtooth crash
   type (type_sawteeth_diags)  :: diags  ! /sawteeth/diags - 
  type (type_codeparam) :: codeparam  ! /sawteeth/codeparam - 
  real(DP)  :: time=-9.0D40       ! /sawteeth/time - Time [s]; Time-dependent; Scalar
endtype

! ***********  Include scenario.xsd
type type_scenario_ref  !    Structure for scenario reference; Time-dependent
  real(DP)  :: value=-9.0D40       ! /value - Signal value; Time-dependent; Scalar
  character(len=132), dimension(:), pointer ::source => null()       ! /source - Source of the signal (any comment describing the origin of the signal : code, path to diagnostic signals, massaging, ...); String
endtype

type type_scenario_int  !    Structure for scenario integer flag; Time-dependent
  integer  :: value=-999999999       ! /value - Signal value; Time-dependent; Scalar Integer.
  character(len=132), dimension(:), pointer ::source => null()       ! /source - Source of the signal (any comment describing the origin of the signal : code, path to diagnostic signals, massaging, ...); String
endtype

type type_scenario_reactor  !    reactor data (such as electricity cost ...)
  real(DP)  :: pnetwork=-9.0D40       ! /pnetwork - reactor electric power provide to the network [W].
endtype

type type_scenario_vol_ave  !    volume averaged values
   type (type_scenario_ref)  :: te_ave  ! /te_ave - volume averaged electron temperature [eV]. Time-dependent.
   type (type_scenario_ref)  :: ti_ave  ! /ti_ave - volume averaged ion temperature [eV]. Time-dependent.
   type (type_scenario_ref)  :: ne_ave  ! /ne_ave - volume averaged electron density [m^-3]. Time-dependent.
   type (type_scenario_ref)  :: dne_ave_dt  ! /dne_ave_dt - time derivative of volume averaged electron density [m^-3/s]. Time-dependent.
   type (type_scenario_ref)  :: ni_ave  ! /ni_ave - volume averaged ion density (<sum(n_k)>, k in species) [m^-3]. Time-dependent.
   type (type_scenario_ref)  :: zeff_ave  ! /zeff_ave - volume averaged effective charge. Time-dependent.
   type (type_scenario_ref)  :: ti_o_te_ave  ! /ti_o_te_ave - volume averaged ion temperature over electron temperature  (<Ti/Te>) []. Time-dependent.
   type (type_scenario_ref)  :: meff_ave  ! /meff_ave - volume averaged effectice mass  (<sum(n_k * m_k)  > /  < sum(n_k)> ) []. Time-dependent.
   type (type_scenario_ref)  :: pellet_flux  ! /pellet_flux - number of electrons fuelling  the plasma every second coming from pellet injection [s^-1]. Time-dependent.
  real(DP),pointer  :: nions_ave(:) => null()     ! /nions_ave - volume averaged ions densities (vector, one element per ion species) [m^-3]. Time-dependent.
   type (type_scenario_ref)  :: omega_ave  ! /omega_ave - bulk volume average toroidal rotation velocity (whole plasma) [rad/s]. Time-dependent.
endtype

type type_scenario_references  !    References
   type (type_scenario_ref)  :: plh  ! /plh - Lower hybrid power [W]. Time-dependent.
   type (type_scenario_ref)  :: picrh  ! /picrh - Ion cyclotron resonnance heating power [W]. Time-dependent.
   type (type_scenario_ref)  :: pecrh  ! /pecrh - electron cyclotron resonnance heating power [W]. Time-dependent.
   type (type_scenario_ref)  :: pnbi  ! /pnbi - neutral beam injection power [W]. Time-dependent.
   type (type_scenario_ref)  :: ip  ! /ip - Plasma current [A]. Time-dependent.
   type (type_scenario_ref)  :: bvac_r  ! /bvac_r - Vacuum field times radius in the toroidal field magnet [T.m]. Time-dependent.
   type (type_scenario_ref)  :: zeffl  ! /zeffl - line averaged effective charge []. Time-dependent.
   type (type_scenario_ref)  :: nbar  ! /nbar - line averaged electron density [m^-3]. Time-dependent.
   type (type_scenario_ref)  :: xecrh  ! /xecrh - position of maximum  (normalized rho coordinate) of electron cyclotron resonnance heating power []. Time-dependent.
   type (type_scenario_ref)  :: pol_flux  ! /pol_flux - separatrix poloidal flux [Wb]. Time-dependent.
   type (type_scenario_ref)  :: enhancement  ! /enhancement - energy enhancement factor []. Time-dependent.
   type (type_scenario_ref)  :: isotopic  ! /isotopic - ratio between tritium  and deuterium density (for burning plasma)  []. Time-dependent.
   type (type_scenario_ref)  :: nbi_td_ratio  ! /nbi_td_ratio - ratio between tritium  and deuterium power in neutral beam injection  []. Time-dependent.
endtype

type type_scenario_sol  !    SOL characteristic  (@ LCMS)
   type (type_scenario_ref)  :: l_te_sol  ! /l_te_sol - electron temperature radial decay length [m]. Time-dependent.
   type (type_scenario_ref)  :: l_ti_sol  ! /l_ti_sol - ion temperature  radial decay length [m]. Time-dependent.
   type (type_scenario_ref)  :: l_ne_sol  ! /l_ne_sol - electron density radial decay length [m]. Time-dependent.
   type (type_scenario_ref)  :: l_ni_sol  ! /l_ni_sol - ion density  radial decay length [m]. Time-dependent.
   type (type_scenario_ref)  :: l_qe_sol  ! /l_qe_sol - electron heat flux radial decay length [m]. Time-dependent.
   type (type_scenario_ref)  :: l_qi_sol  ! /l_qi_sol - ion  heat flux radial decay length [m]. Time-dependent.
   type (type_scenario_ref)  :: p_rad_sol  ! /p_rad_sol - radiative power of the SOL [W]. Time-dependent.
  real(DP),pointer  :: gaz_puff(:) => null()     ! /gaz_puff - gaz puff flux for each ion species [s^-1]. Time-dependent.
endtype

type type_scenario_pedestal  !    Values at the top of the H-mode pedestal
   type (type_scenario_ref)  :: te_ped  ! /te_ped - pedestal electron temperature [eV]. Time-dependent.
   type (type_scenario_ref)  :: ti_ped  ! /ti_ped - pedestal ion temperature [eV]. Time-dependent.
   type (type_scenario_ref)  :: ne_ped  ! /ne_ped - pedestal electron density [m^-3]. Time-dependent.
   type (type_scenario_ref)  :: ni_ped  ! /ni_ped - pedestal ion density [m^-3]. Time-dependent.
   type (type_scenario_ref)  :: psi_ped  ! /psi_ped - pedestal poloidal flux [Wb]. Time-dependent.
   type (type_scenario_ref)  :: phi_ped  ! /phi_ped - pedestal toroidal flux [Wb]. Time-dependent.
   type (type_scenario_ref)  :: rho_ped  ! /rho_ped - top pedestal value of internal simulator coordinate [m]. Time-dependent.
   type (type_scenario_ref)  :: q_ped  ! /q_ped - top pedestal safety factor value []. Time-dependent.
   type (type_scenario_ref)  :: pressure_ped  ! /pressure_ped - top pedestal thermal pressure (n_e * T_e  + n_i * T_i) [Pa]. Time-dependent.
   type (type_scenario_ref)  :: vtor_ped  ! /vtor_ped - top pedestal value of rotation velocity of selected impurity [m/s]. Time-dependent.
endtype

type type_scenario_ninety_five  !    values at 95% of poloidal flux
   type (type_scenario_ref)  :: q_95  ! /q_95 - safety factor value  @ 95 % of poloidal flux span []. Time-dependent.
   type (type_scenario_ref)  :: elong_95  ! /elong_95 - plasma elongation  @ 95 % of poloidal flux span []. Time-dependent.
   type (type_scenario_ref)  :: tria_95  ! /tria_95 - averaged plasma triangularity  @ 95 % of poloidal flux span []. Time-dependent.
   type (type_scenario_ref)  :: tria_up_95  ! /tria_up_95 - upper plasma triangularity  @ 95 % of poloidal flux span []. Time-dependent.
   type (type_scenario_ref)  :: tria_lo_95  ! /tria_lo_95 - lower plasma triangularity  @ 95 % of poloidal flux span []. Time-dependent.
   type (type_scenario_ref)  :: te_95  ! /te_95 - electron temperature  @ 95 % of poloidal flux [eV]. Time-dependent.
   type (type_scenario_ref)  :: ti_95  ! /ti_95 - ion temperature  @ 95 % of poloidal flux [eV]. Time-dependent.
   type (type_scenario_ref)  :: ne_95  ! /ne_95 - electron density  @ 95 % of poloidal flux [m^-3]. Time-dependent.
   type (type_scenario_ref)  :: ni_95  ! /ni_95 - ion density  @ 95 % of poloidal flux [m^-3]. Time-dependent.
   type (type_scenario_ref)  :: phi_95  ! /phi_95 - toroidal flux  @ 95 % of poloidal flux [Wb]. Time-dependent.
   type (type_scenario_ref)  :: rho_95  ! /rho_95 -  value of internal simulator coordinate  @ 95 % of poloidal flux [m]. Time-dependent.
   type (type_scenario_ref)  :: vtor_95  ! /vtor_95 - rotation velocity of selected impurity   @ 95 % of poloidal flux [m/s]. Time-dependent.
endtype

type type_scenario_neutron  !    neutron flux for DD and DT reactions
   type (type_scenario_ref)  :: ndd_tot  ! /ndd_tot - total neutron flux coming  from DD reactions [Hz]. Time-dependent.
   type (type_scenario_ref)  :: ndd_th  ! /ndd_th - neutron flux coming  from thermal  DD reactions [Hz]. Time-dependent.
   type (type_scenario_ref)  :: ndd_nbi_th  ! /ndd_nbi_th - neutron flux coming  from beam/plasma  DD reactions [Hz]. Time-dependent.
   type (type_scenario_ref)  :: ndd_nbi_nbi  ! /ndd_nbi_nbi - neutron flux coming  from beam/beam  DD reactions [Hz]. Time-dependent.
   type (type_scenario_ref)  :: ndt_tot  ! /ndt_tot - total neutron flux coming  from DT reactions [Hz]. Time-dependent.
   type (type_scenario_ref)  :: ndt_th  ! /ndt_th - neutron flux coming  from thermal DT reactions [Hz]. Time-dependent.
endtype

type type_scenario_line_ave  !    line averaged value
   type (type_scenario_ref)  :: ne_line  ! /ne_line - line averaged electron density [m^-3]. Time-dependent.
   type (type_scenario_ref)  :: zeff_line  ! /zeff_line - line averaged effective charge. Time-dependent.
   type (type_scenario_ref)  :: ne_zeff_line  ! /ne_zeff_line - line averaged electron density * Zeff . Time-dependent.
   type (type_scenario_ref)  :: dne_line_dt  ! /dne_line_dt - time derivative of line averaged electron density [m^-3/s]. Time-dependent.
endtype

type type_scenario_lim_div_wall  !    values on the plate of divertor or on the limitor or on the wall (@ LCMS)
   type (type_scenario_ref)  :: te_lim_div  ! /te_lim_div - limiter/divertor electron temperature [eV]. Time-dependent.
   type (type_scenario_ref)  :: ti_lim_div  ! /ti_lim_div - limiter/divertor ion temperature [eV]. Time-dependent.
   type (type_scenario_ref)  :: ne_lim_div  ! /ne_lim_div - limiter/divertor electron density [m^-3]. Time-dependent.
   type (type_scenario_ref)  :: ni_lim_div  ! /ni_lim_div - limiter/divertor ion density [m^-3]. Time-dependent.
   type (type_scenario_ref)  :: p_peak_div  ! /p_peak_div - peak power on  divertor [W]. Time-dependent.
   type (type_scenario_ref)  :: surf_temp  ! /surf_temp - limiter surface or divertor plate temperature [K]. Time-dependent.
   type (type_scenario_ref)  :: p_lim_div  ! /p_lim_div - Power flux on limiter or divertor plate [W]. Time-dependent.
   type (type_scenario_ref)  :: p_rad_div  ! /p_rad_div - radiative power in the divertor zone [W]. Time-dependent.
   type (type_scenario_ref)  :: wall_temp  ! /wall_temp - wall temperature [K]. Time-dependent.
   type (type_scenario_ref)  :: wall_state  ! /wall_state - saturation state of the wall (0 = completly pumping wall, 1 = competely saturate wall) []. Time-dependent.
   type (type_scenario_ref)  :: detach_state  ! /detach_state - plasma detachement state (0= attach plasma, 1 = completely detach plasma) []. Time-dependent.
  real(DP),pointer  :: pump_flux(:) => null()     ! /pump_flux - flux pump out for each ion species [s^-1]. Time-dependent.
endtype

type type_scenario_itb  !    Values characteristics of the Internal Transport Barrier
   type (type_scenario_ref)  :: q_min  ! /q_min - minimal value of safety factor []. Time-dependent.
   type (type_scenario_ref)  :: te_itb  ! /te_itb - electron temperature @ q = q_min [eV]. Time-dependent.
   type (type_scenario_ref)  :: ti_itb  ! /ti_itb - ion temperature @ q = q_min [eV]. Time-dependent.
   type (type_scenario_ref)  :: ne_itb  ! /ne_itb - electron density  @ q = q_min [m^-3]. Time-dependent.
   type (type_scenario_ref)  :: ni_itb  ! /ni_itb - ion density  @ q = q_min [m^-3]. Time-dependent.
   type (type_scenario_ref)  :: psi_itb  ! /psi_itb - poloidal flux @ q = q_min [Wb]. Time-dependent.
   type (type_scenario_ref)  :: phi_itb  ! /phi_itb - toroidal flux @ q = q_min [Wb]. Time-dependent.
   type (type_scenario_ref)  :: rho_itb  ! /rho_itb - value of internal simulator coordinate @ q = q_min [m]. Time-dependent.
   type (type_scenario_ref)  :: h_itb  ! /h_itb - energy enhancement ITB factor [m]. Time-dependent.
   type (type_scenario_ref)  :: width_itb  ! /width_itb - width of the high pressure gradient region (on scale of rho_itb) [m]. Time-dependent.
   type (type_scenario_ref)  :: vtor_itb  ! /vtor_itb - rotation velocity of selected impurity @ rho_itb [m/s]. Time-dependent.
   type (type_scenario_int)  :: itb_type  ! /itb_type - itb type []. Time-dependent. Any combinaison of :0 = none; 1 = on T_i; 2 = on T_e; 4  = on n_e; 8 = reverse shear triggered; 16 = 
endtype

type type_scenario_heat_power  !    Power delivred to plasma (thermal an non thermal)
   type (type_scenario_ref)  :: plh  ! /plh - Lower hybrid power [W]. Time-dependent.
   type (type_scenario_ref)  :: pohmic  ! /pohmic - ohmic power (thermal species contribution only) [W]. Time-dependent.
   type (type_scenario_ref)  :: picrh  ! /picrh - Ion cyclotron resonnance heating power [W]. Time-dependent.
   type (type_scenario_ref)  :: pecrh  ! /pecrh - electron cyclotron resonnance heating power [W]. Time-dependent.
   type (type_scenario_ref)  :: pnbi  ! /pnbi - neutral beam injection power [W]. Time-dependent.
   type (type_scenario_ref)  :: pnbi_co_cur  ! /pnbi_co_cur - neutral beam injection power injeted in co-current direction [W]. Time-dependent.
   type (type_scenario_ref)  :: pnbi_counter  ! /pnbi_counter - neutral beam injection power injeted in counter-current direction [W]. Time-dependent.
   type (type_scenario_ref)  :: plh_th  ! /plh_th - lower hybrid power deposited on thermal electrons [W]. Time-dependent.
   type (type_scenario_ref)  :: picrh_th  ! /picrh_th - ion cyclotron resonnance heating power deposited on thermal species [W]. Time-dependent.
   type (type_scenario_ref)  :: pecrh_th  ! /pecrh_th - electron cyclotron resonnance heating power deposited on thermal electrons [W]. Time-dependent.
   type (type_scenario_ref)  :: pnbi_th  ! /pnbi_th - neutral beam injection power deposited on thermal species [W]. Time-dependent.
   type (type_scenario_ref)  :: ploss_icrh  ! /ploss_icrh - Ion cyclotron resonnance heating power losses [W]. Time-dependent.
   type (type_scenario_ref)  :: ploss_nbi  ! /ploss_nbi - neutral beam injection power losses (including shine-through) [W]. Time-dependent.
   type (type_scenario_ref)  :: pbrem  ! /pbrem - Bremsstrahlung radition losses [W]. Time-dependent.
   type (type_scenario_ref)  :: pcyclo  ! /pcyclo - cyclotron radiation losses [W]. Time-dependent.
   type (type_scenario_ref)  :: prad  ! /prad - impurity radition losses in core plamsa , without Bremsstrahlung [W]. Time-dependent.
   type (type_scenario_ref)  :: pdd_fus  ! /pdd_fus - fusion power due to DD reactions [W]. Time-dependent.
   type (type_scenario_ref)  :: pei  ! /pei - power exchange between eletron and ion (equipartition) [W]. Time-dependent.
   type (type_scenario_ref)  :: pel_tot  ! /pel_tot - total thermal electron power deposition without equipartition [W]. Time-dependent.
   type (type_scenario_ref)  :: pel_fus  ! /pel_fus - fusion electron power deposition [W]. Time-dependent.
   type (type_scenario_ref)  :: pel_icrh  ! /pel_icrh - ICRH  electron power deposition [W]. Time-dependent.
   type (type_scenario_ref)  :: pel_nbi  ! /pel_nbi - NBI electron power deposition [W]. Time-dependent.
   type (type_scenario_ref)  :: pfus_dt  ! /pfus_dt - total D-T fusion power of alpha [W]. Time-dependent.
   type (type_scenario_ref)  :: ploss_fus  ! /ploss_fus - D-T fusion power of alpha losses  [W]. Time-dependent.
   type (type_scenario_ref)  :: pfus_nbi  ! /pfus_nbi - NBI induce D-T fusion power of alpha  [W]. Time-dependent.
   type (type_scenario_ref)  :: pfus_th  ! /pfus_th - alpha (from DT fusion reaction)  power deposited on thermal species [W]. Time-dependent.
   type (type_scenario_ref)  :: padd_tot  ! /padd_tot - total additional power input including ohmic power  [W]. Time-dependent.
   type (type_scenario_ref)  :: pion_tot  ! /pion_tot - total thermal ion power deposition without equipartition [W]. Time-dependent.
   type (type_scenario_ref)  :: pion_fus  ! /pion_fus - fusion ion power deposition [W]. Time-dependent.
   type (type_scenario_ref)  :: pion_icrh  ! /pion_icrh - ICRH  ion power deposition [W]. Time-dependent.
   type (type_scenario_ref)  :: pion_nbi  ! /pion_nbi - NBI  ion power deposition [W]. Time-dependent.
   type (type_scenario_ref)  :: pioniz  ! /pioniz - power losses due to cold neutral ionization [W]. Time-dependent.
   type (type_scenario_ref)  :: ploss  ! /ploss - plasma losses power, as define in ITER basis [W]. Time-dependent.
   type (type_scenario_ref)  :: p_wth  ! /p_wth - thermal power input, define as tau_E * P_th = Wth [W]. Time-dependent.
   type (type_scenario_ref)  :: p_w  ! /p_w - effective power define as tau_E  * P_w = W_tot [W]. Time-dependent.
   type (type_scenario_ref)  :: p_l2h_thr  ! /p_l2h_thr - additionnal power crossing the LCMS; must be compare to  L->H threshold power (Ryter PPCF 2002) [W]. Time-dependent.
   type (type_scenario_ref)  :: p_l2h_sc  ! /p_l2h_sc - threshold power given by the choosen scaling law for transition from L-mode to H-mode  [W]. Time-dependent.
   type (type_scenario_ref)  :: p_nbi_icrh  ! /p_nbi_icrh - beam power increase due to  ICRH effects  [W]. Time-dependent.
endtype

type type_scenario_global  !     global scalar value 
   type (type_scenario_ref)  :: ip  ! /ip - Plasma current [A]. Time-dependent.
   type (type_scenario_ref)  :: dip_dt  ! /dip_dt - time derivative of plasma current [A/s]. Time-dependent.
   type (type_scenario_ref)  :: beta_pol  ! /beta_pol - poloidal beta []. Time-dependent.
   type (type_scenario_ref)  :: beta_tor  ! /beta_tor - toroidal beta []. Time-dependent.
   type (type_scenario_ref)  :: beta_normal  ! /beta_normal - normalised beta []. Time-dependent.
   type (type_scenario_ref)  :: li  ! /li - internal inductance (definition 3). Time-dependent.
   type (type_scenario_ref)  :: volume  ! /volume - total plasma volume [m^3]. Time-dependent.
   type (type_scenario_ref)  :: area_pol  ! /area_pol - area poloidal cross section [m^2]. Time-dependent.
   type (type_scenario_ref)  :: area_ext  ! /area_ext - external plasma surface [m^2]. Time-dependent.
   type (type_scenario_ref)  :: len_sepa  ! /len_sepa - length of the separatrix [m]. Time-dependent.
   type (type_scenario_ref)  :: beta_pol_th  ! /beta_pol_th - poloidal beta, thermal contribution []. Time-dependent.
   type (type_scenario_ref)  :: beta_tor_th  ! /beta_tor_th - toroidal beta, thermal contribution []. Time-dependent.
   type (type_scenario_ref)  :: beta_n_th  ! /beta_n_th - normalised beta, thermal contribution []. Time-dependent.
   type (type_scenario_ref)  :: disruption  ! /disruption - flag for disruption (set to 1 for disruption, oterwise equal 0) []. Time-dependent.
   type (type_scenario_ref)  :: mode_h  ! /mode_h - confinement mode verus time:  0 = L-mode et 1 = H-mode []. Time-dependent.
   type (type_scenario_ref)  :: s_alpha  ! /s_alpha - total number of alpha fusion  particules from D-T ractions  per second [s^-1]. Time-dependent.
endtype

type type_scenario_energy  !    plasma energy content
   type (type_scenario_ref)  :: w_tot  ! /w_tot - total plasma energy [J]. Time-dependent.
   type (type_scenario_ref)  :: w_b_pol  ! /w_b_pol - poloidal field energy of  the plasma [J]. Time-dependent.
   type (type_scenario_ref)  :: w_dia  ! /w_dia - 3/2 perpendicular plasma energy [J]. Time-dependent.
   type (type_scenario_ref)  :: dwdia_dt  ! /dwdia_dt - time derivative of Wdia [W]. Time-dependent.
   type (type_scenario_ref)  :: w_b_tor_pla  ! /w_b_tor_pla - toroidal magnetic plasma energy  [J]. Time-dependent.
   type (type_scenario_ref)  :: w_th  ! /w_th - thermal plasma energy [J]. Time-dependent.
   type (type_scenario_ref)  :: dwtot_dt  ! /dwtot_dt - time derivative of total plasma energy [W]. Time-dependent.
   type (type_scenario_ref)  :: dwbpol_dt  ! /dwbpol_dt - time derivative of plasma poloidal field energy [W]. Time-dependent.
   type (type_scenario_ref)  :: dwbtorpla_dt  ! /dwbtorpla_dt - time derivative of toroidal magnetic plasma energy  [W]. Time-dependent.
   type (type_scenario_ref)  :: dwth_dt  ! /dwth_dt - time derivative of thermal plasma energy [W]. Time-dependent.
   type (type_scenario_ref)  :: esup_icrhtot  ! /esup_icrhtot - total suprathermal energy of fast ions accelerated  by ICRH [J]. Time-dependent.
   type (type_scenario_ref)  :: esup_icrhper  ! /esup_icrhper - perpendicular part of suprathermal energy of fast ions accelerated  by ICRH [J]. Time-dependent.
   type (type_scenario_ref)  :: esup_nbitot  ! /esup_nbitot - total suprathermal energy of fast ions from NBI ionisation [J]. Time-dependent.
   type (type_scenario_ref)  :: esup_nbiperp  ! /esup_nbiperp - perpendicular part of suprathermal energy of fast ions from NBI ionisation [J]. Time-dependent.
   type (type_scenario_ref)  :: esup_lhcd  ! /esup_lhcd - total suprathermal energy of fast electron from LHCD [J]. Time-dependent.
   type (type_scenario_ref)  :: esup_alpha  ! /esup_alpha - total suprathermal energy of fast alpha particules [J]. Time-dependent.
endtype

type type_scenario_edge  !    edge value (@ LCMS)
   type (type_scenario_ref)  :: te_edge  ! /te_edge - edge electron temperature [eV]. Time-dependent.
   type (type_scenario_ref)  :: ti_edge  ! /ti_edge - edge ion temperature [eV]. Time-dependent.
   type (type_scenario_ref)  :: ne_edge  ! /ne_edge - edge electron density [m^-3]. Time-dependent.
   type (type_scenario_ref)  :: ni_edge  ! /ni_edge - edge ion density [m^-3]. Time-dependent.
   type (type_scenario_ref)  :: psi_edge  ! /psi_edge - edge  poloidal flux [Wb]. Time-dependent.
   type (type_scenario_ref)  :: phi_edge  ! /phi_edge - edge  toroidal flux [Wb]. Time-dependent.
   type (type_scenario_ref)  :: rho_edge  ! /rho_edge - edge value of internal simulator coordinate [m]. Time-dependent.
   type (type_scenario_ref)  :: drho_edge_dt  ! /drho_edge_dt - time derivative of edge value of internal simulator coordinate [m/s]. Time-dependent.
   type (type_scenario_ref)  :: q_edge  ! /q_edge - edge or effective  safety factor value []. Time-dependent.
   type (type_scenario_ref)  :: neutral_flux  ! /neutral_flux - number of cold neutral (in equivalent electron for Z >1) that input in  plasma at the edge every second coming from recycling and 
   type (type_scenario_ref)  :: phi_plasma  ! /phi_plasma - contribution of the plasma to the toroidal flux (used for toroidal coils heat load computation) [Wb]. Time-dependent.
   type (type_scenario_ref)  :: vtor_edge  ! /vtor_edge - rotation velocity of selected impurity on the separatrix [m/s]. Time-dependent.
endtype

type type_scenario_currents  !    data related to current sources and current diffusion
   type (type_scenario_ref)  :: RR  ! /RR - plasma resistivity [ohm]. Time-dependent.
   type (type_scenario_ref)  :: i_align  ! /i_align - current drive alignment quality parameter (1 = good , 0 = bad). Time-dependent.
   type (type_scenario_ref)  :: i_boot  ! /i_boot - bootstrap current [A]. Time-dependent.
   type (type_scenario_ref)  :: i_cd_tot  ! /i_cd_tot - total current drive [A]. Time-dependent.
   type (type_scenario_ref)  :: i_eccd  ! /i_eccd - Electron Cyclotron current drive [A]. Time-dependent.
   type (type_scenario_ref)  :: i_fast_ion  ! /i_fast_ion - fast ions bootstrap like current drive  (i.e. fast alpha) [A]. Time-dependent.
   type (type_scenario_ref)  :: i_fwcd  ! /i_fwcd - Fast Wave current drive [A]. Time-dependent.
   type (type_scenario_ref)  :: i_lhcd  ! /i_lhcd - Lower Hybrid current drive [A]. Time-dependent.
   type (type_scenario_ref)  :: i_nbicd  ! /i_nbicd - Neutral Beam Injection current drive  [A]. Time-dependent.
   type (type_scenario_ref)  :: i_ni_tot  ! /i_ni_tot - total non inductive current  [A]. Time-dependent.
   type (type_scenario_ref)  :: i_ohm  ! /i_ohm - ohmic current  [A]. Time-dependent.
   type (type_scenario_ref)  :: i_par  ! /i_par - total plasma current (projected on B : <J.B>/B0)   [A]. Time-dependent.
   type (type_scenario_ref)  :: i_runaway  ! /i_runaway - runaway current  [A]. Time-dependent.
   type (type_scenario_ref)  :: v_loop  ! /v_loop - loop voltage @ LCMS / LFS , equatorial point  [V]. Time-dependent.
   type (type_scenario_ref)  :: v_meas  ! /v_meas - loop voltage measured on a  coil   [V]. Time-dependent.
endtype

type type_scenario_confinement  !    characteristic confinement times
   type (type_scenario_ref)  :: tau_e  ! /tau_e - thermal energy confinement time [s]. Time-dependent.
   type (type_scenario_ref)  :: tau_l_sc  ! /tau_l_sc - confinement time given by the selected L-mode scaling law [s]. Time-dependent.
   type (type_scenario_ref)  :: tau_h_sc  ! /tau_h_sc - confinement time given by the selected H-mode scaling law [s]. Time-dependent.
   type (type_scenario_ref)  :: tau_he  ! /tau_he - Helium ashes confinement time [s]. Time-dependent.
   type (type_scenario_ref)  :: tau_e_ee  ! /tau_e_ee - electron energy confimenent time [s]. Time-dependent.
   type (type_scenario_ref)  :: tau_e_ii  ! /tau_e_ii - ion energy confinement time [s]. Time-dependent.
   type (type_scenario_ref)  :: tau_e_ei  ! /tau_e_ei - energy equipartition characteristic time [s]. Time-dependent.
   type (type_scenario_ref)  :: tau_cur_diff  ! /tau_cur_diff - characteristic time for current diffusion  [s]. Time-dependent.
   type (type_scenario_ref)  :: tau_i_rol  ! /tau_i_rol - characteristic time for current decrease in tokamak equivalent R/L circuit [s]. Time-dependent.
endtype

type type_scenario_configuration  !    Strings describing the tokamak configuration
   type (type_scenario_int)  :: config  ! /config - plasma configuration (limiter/divertor ...) []. Time-dependent. Possible values : 0 = undetermined; 1 = poloidal limiter (ring); 2
  character(len=132), dimension(:), pointer ::lmode_sc => null()       ! /lmode_sc - name of the L-mode scaling law. String.
  character(len=132), dimension(:), pointer ::hmode_sc => null()       ! /hmode_sc - name of the H-mode scaling law. String.
  character(len=132), dimension(:), pointer ::core_sc => null()       ! /core_sc - name of the core plasma  energy scaling law. String.
  character(len=132), dimension(:), pointer ::pedestal_sc => null()       ! /pedestal_sc - name of the  pedestal energy scaling law. String.
  character(len=132), dimension(:), pointer ::helium_sc => null()       ! /helium_sc - name of the  helium confinement time scaling law. String.
  character(len=132), dimension(:), pointer ::impurity_sc => null()       ! /impurity_sc - name of the impurities confinement time scaling law
  character(len=132), dimension(:), pointer ::l2h_sc => null()       ! /l2h_sc - name of the  L-mode to H-mode power threshold scaling law. String.
  character(len=132), dimension(:), pointer ::tor_rot_sc => null()       ! /tor_rot_sc - name of the toroidal spontaneous rotation  scaling law. String.
  character(len=132), dimension(:), pointer ::wall_mat => null()       ! /wall_mat - chemical compostion of the wall. String.
  character(len=132), dimension(:), pointer ::evap_mat => null()       ! /evap_mat - chemical compostion evaporated wall conditioning material. String.
  character(len=132), dimension(:), pointer ::lim_mat => null()       ! /lim_mat - chemical compostion of the limiter. String.
  character(len=132), dimension(:), pointer ::div_mat => null()       ! /div_mat - chemical compostion of the divertor
  character(len=132), dimension(:), pointer ::coordinate => null()       ! /coordinate - name/definition of the internal coordinate of the simulator that are given by the data named rho
   type (type_scenario_ref)  :: ecrh_freq  ! /ecrh_freq - ECRH frequency [Hz]. Time-dependent.
   type (type_scenario_ref)  :: ecrh_loc  ! /ecrh_loc - position of maximum ECRH deposition on scale of rho [rho]. Time-dependent.
   type (type_scenario_int)  :: ecrh_mode  ! /ecrh_mode - polarisation of ecrh wave (0 = O mode, 1 = X mode) []. Time-dependent.
   type (type_scenario_ref)  :: ecrh_tor_ang  ! /ecrh_tor_ang - toroidal angle of ECRH at resonance  [rad] Time-dependent.
   type (type_scenario_ref)  :: ecrh_pol_ang  ! /ecrh_pol_ang - poloidal angle of ECRH resonance positon (0= LFS, pi/2 = top, -pi/2 = down, pi = HFS)  [rad]. Time-dependent.
   type (type_scenario_int)  :: ecrh_harm  ! /ecrh_harm - harmonic number of the apsorbed ecrh wave []. Time-dependent.
   type (type_scenario_ref)  :: enbi  ! /enbi - energy of the neutral beam [eV]. Time-dependent.
   type (type_scenario_ref)  :: r_nbi  ! /r_nbi - Major radius of tengance of NBI [m]. Time-dependent.
   type (type_scenario_int)  :: grad_b_drift  ! /grad_b_drift - direction of ion grad-B drift (1= to lower divertor, -1 = from lower divertor)  []. Time-dependent.
   type (type_scenario_ref)  :: icrh_freq  ! /icrh_freq - ICRH frequency [Hz]. Time-dependent.
  character(len=132), dimension(:), pointer ::icrh_scheme => null()       ! /icrh_scheme - icrh scheme either : H_min_1; He3_min; T_harm_2; FW; FW_CD; FW_CCD
   type (type_scenario_ref)  :: icrh_phase  ! /icrh_phase - ICRH antenna phasing [rad]. Time-dependent.
   type (type_scenario_ref)  :: LH_freq  ! /LH_freq - LHCD frequency [Hz]. Time-dependent.
   type (type_scenario_ref)  :: LH_npar  ! /LH_npar - LHCD parallel indice []. Time-dependent.
   type (type_scenario_ref)  :: pellet_ang  ! /pellet_ang - pellet injection positon (0= LFS, pi/2 = top, -pi/2 = down, pi = HFS)  [rad]. Time-dependent.
   type (type_scenario_ref)  :: pellet_v  ! /pellet_v - pellet injection velocity [m/s]. Time-dependent.
   type (type_scenario_ref)  :: pellet_nba  ! /pellet_nba - initial number of atoms in pellet  []. Time-dependent.
endtype

type type_scenario_composition  !    Plasma composition (description of ion species).
  real(DP),pointer  :: amn(:) => null()     ! /amn - Atomic mass number (lumped ions are allowed); Vector (nion)
  real(DP),pointer  :: zn(:) => null()     ! /zn - Nuclear charge (lumped ions are allowed); Vector (nion)
  real(DP),pointer  :: zion(:) => null()     ! /zion - Ion charge (of the dominant ionisation state; lumped ions are allowed); Vector (nion)
  integer,pointer  :: imp_flag(:) => null()      ! /imp_flag - Multiple charge state calculation flag : 0-Only one charge state is considered; 1-Multiple charge state are considered and are des
  integer,pointer  :: rot_imp_flag(:) => null()      ! /rot_imp_flag - set to 1 for the impurity corresponding at the given toroidal rotation, otherwise = 0
  real(DP),pointer  :: pellet_amn(:) => null()     ! /pellet_amn - Atomic mass number (for pellet injector); Vector (nion)
  real(DP),pointer  :: pellet_zn(:) => null()     ! /pellet_zn - Nuclear charge (pellet injector); Vector (nion)
  real(DP),pointer  :: nbi_amn(:) => null()     ! /nbi_amn - Atomic mass number (for neutral beam injection); Vector (nion)
  real(DP),pointer  :: nbi_zn(:) => null()     ! /nbi_zn - Nuclear charge (for neutral beam injection); Vector (nion)
endtype

type type_scenario_centre  !    central values of the profiles (at magnetic axis)
   type (type_scenario_ref)  :: te0  ! /te0 - central electron temperature [eV]. Time-dependent.
   type (type_scenario_ref)  :: ti0  ! /ti0 - central ion temperature [eV]. Time-dependent.
   type (type_scenario_ref)  :: ne0  ! /ne0 - central electron density [m^-3]. Time-dependent.
   type (type_scenario_ref)  :: ni0  ! /ni0 - central ion density [m^-3]. Time-dependent.
   type (type_scenario_ref)  :: shift0  ! /shift0 - central value of Shafranov shift [m]. Time-dependent.
   type (type_scenario_ref)  :: psi0  ! /psi0 - pedestal poloidal flux [Wb]. Time-dependent.
   type (type_scenario_ref)  :: phi0  ! /phi0 - central toroidal flux [Wb]. Time-dependent.
   type (type_scenario_ref)  :: q0  ! /q0 - central safety factor value []. Time-dependent.
   type (type_scenario_ref)  :: Rmag  ! /Rmag - radius of magnetic axis [R]. Time-dependent.
   type (type_scenario_ref)  :: Zmag  ! /Zmag - Z coordinate of magnetic axis [R]. Time-dependent.
   type (type_scenario_ref)  :: vtor_0  ! /vtor_0 - central rotation velocity of selected impurity [m/s]. Time-dependent.
endtype

type type_scenario  !    
  type (type_datainfo) :: datainfo  ! /scenario/datainfo - 
   type (type_scenario_centre)  :: centre  ! /scenario/centre - central values of the profiles (at magnetic axis)
   type (type_scenario_composition)  :: composition  ! /scenario/composition - Plasma composition (description of ion species).
   type (type_scenario_configuration)  :: configs  ! /scenario/configs - Strings describing the tokamak configuration
   type (type_scenario_confinement)  :: confinement  ! /scenario/confinement - characteristic confinement times
   type (type_scenario_currents)  :: currents  ! /scenario/currents - data related to current sources and current diffusion
   type (type_scenario_edge)  :: edge  ! /scenario/edge - edge value (@ LCMS)
   type (type_scenario_energy)  :: energy  ! /scenario/energy - plasma energy content
  type (type_eqgeometry) :: eqgeometry  ! /scenario/eqgeometry - 
   type (type_scenario_global)  :: global_param  ! /scenario/global_param - Global scalar values
   type (type_scenario_heat_power)  :: heat_power  ! /scenario/heat_power - Power delivred to plasma (thermal and non thermal)
   type (type_scenario_itb)  :: itb  ! /scenario/itb - Values characteristics of the Internal Transport Barrier
   type (type_scenario_lim_div_wall)  :: lim_div_wall  ! /scenario/lim_div_wall - values on the plate of divertor or on the limitor or on the wall (@ LCMS)
   type (type_scenario_line_ave)  :: line_ave  ! /scenario/line_ave - line averaged value
   type (type_scenario_neutron)  :: neutron  ! /scenario/neutron - neutron flux for DD and DT reactions
   type (type_scenario_ninety_five)  :: ninety_five  ! /scenario/ninety_five - values at 95% of poloidal flux
   type (type_scenario_pedestal)  :: pedestal  ! /scenario/pedestal - Values at the top of the H-mode pedestal
   type (type_scenario_references)  :: references  ! /scenario/references - References
   type (type_scenario_reactor)  :: reactor  ! /scenario/reactor - reactor data (such as electricity cost ...)
   type (type_scenario_sol)  :: sol  ! /scenario/sol - SOL characteristic  (@ LCMS)
   type (type_scenario_vol_ave)  :: vol_ave  ! /scenario/vol_ave - volume averaged value
  type (type_codeparam) :: codeparam  ! /scenario/codeparam - 
  real(DP)  :: time=-9.0D40       ! /scenario/time - Time [s]; Time-dependent; Scalar
endtype

! ***********  Include summary.xsd
type type_summary  !    
  type (type_datainfo) :: datainfo  ! /summary/datainfo - 
   type (type_reduced)  :: ip  ! /summary/ip - Plasma current [A]
   type (type_reduced)  :: bvac_r  ! /summary/bvac_r - Vacuum field times radius in the toroidal field magnet [T.m];
   type (type_reduced)  :: geom_axis_r  ! /summary/geom_axis_r - Major radius of the geometric axis [m]
   type (type_reduced)  :: a_minor  ! /summary/a_minor - Minor radius of the plasma boundary [m]
   type (type_reduced)  :: elongation  ! /summary/elongation - Elongation of the plasma boundary [m]
   type (type_reduced)  :: tria_lower  ! /summary/tria_lower - Lower triangularity of the plasma boundary [m]
   type (type_reduced)  :: tria_upper  ! /summary/tria_upper - Upper triangularity of the plasma boundary [m]
   type (type_reduced)  :: tev  ! /summary/tev - volume averaged electron temperature [eV]
   type (type_reduced)  :: tiv  ! /summary/tiv - volume averaged ion temperature [eV]
   type (type_reduced)  :: nev  ! /summary/nev - volume averaged electron density [m^-3]
   type (type_reduced)  :: zeffv  ! /summary/zeffv - volume averaged effective charge
   type (type_reduced)  :: beta_pol  ! /summary/beta_pol - poloidal beta
   type (type_reduced)  :: beta_tor  ! /summary/beta_tor - toroidal beta
   type (type_reduced)  :: beta_normal  ! /summary/beta_normal - normalised beta
   type (type_reduced)  :: li  ! /summary/li - internal inductance
   type (type_reduced)  :: volume  ! /summary/volume - total plasma volume [m^3]
   type (type_reduced)  :: area  ! /summary/area - area poloidal cross section [m^2]
   type (type_reduced)  :: main_ion1_z  ! /summary/main_ion1_z - Atomic number of the main ion #1 [a.m.u.]
   type (type_reduced)  :: main_ion1_a  ! /summary/main_ion1_a - Atomic mass of the main ion #1 [a.m.u.]
   type (type_reduced)  :: main_ion2_z  ! /summary/main_ion2_z - Atomic number of the main ion #2 [a.m.u.]
   type (type_reduced)  :: main_ion2_a  ! /summary/main_ion2_a - Atomic mass of the main ion #2 [a.m.u.]
   type (type_reduced)  :: impur1_z  ! /summary/impur1_z - Atomic number of the impurity #1 [a.m.u.]
   type (type_reduced)  :: impur1_a  ! /summary/impur1_a - Atomic mass of the impurity #1 [a.m.u.]
  real(DP)  :: time=-9.0D40       ! /summary/time - Time at which the 0D variables of the summary are taken [s]. Scalar
endtype

! ***********  Include vessel.xsd
type type_vessel  !    
  type (type_datainfo) :: datainfo  ! /vessel/datainfo - 
   type (type_rz1D)  :: position  ! /vessel/position - Vessel wall outline (list of R,Z co-ordinates) [m]; Vector (npoints)
endtype

! ***********  Include waves.xsd
type type_theta_info  !    
  integer,pointer  :: angl_type(:) => null()      ! /theta_info/angl_type - Type of poloidal angle: 1 : same as the poloidal angle in the equlibrium cpo; 2 : normal polar angle; 3 : other. If option three a
  real(DP),pointer  :: th2th_pol(:,:) => null()     ! /theta_info/th2th_pol - Transformation from theta to a ploar theta; Matrix (nfreq, max_ntheta)
endtype

type type_waves_2dgrid  !    Grid points in 2D
  real(DP),pointer  :: psin(:,:) => null()     ! /psin - Grid points in normalized poloidal flux function; Matrix(nfreq, max_npsin)
  real(DP),pointer  :: theta(:,:) => null()     ! /theta - Grid points of the ploidal angle; Matrix(nfreq, max_ntheta)
  integer,pointer  :: npsin(:) => null()      ! /npsin - Number of psin points in the grid for each frequency. Vector (nfreq).
  integer,pointer  :: ntheta(:) => null()      ! /ntheta - Number of theta points in the grid for each frequency. Vector (nfreq).
   type (type_rz3D)  :: rz_position  ! /rz_position - R (major radius) and Z (altitude) of grid points; Array 3D (nfreq, max_npsin, max_ntheta)
  type (type_theta_info) :: theta_info  ! /theta_info - Information on the poloidal angle theta.
endtype

type type_waves_global_param  !    Global wave deposition parameters
  real(DP),pointer  :: frequency(:) => null()     ! /frequency - Wave frequency [Hz]; Time-dependent; Vector (nfreq)
  character(len=132), dimension(:), pointer ::name => null()       ! /name - Antenna name, Vector of strings (nfreq)
  character(len=132), dimension(:), pointer ::type => null()       ! /type - Wave type (LH, EC, IC, ...), Vector of strings (nfreq)
  integer,pointer  :: nntor(:) => null()      ! /nntor - Number of toroidal mode numbers for each frequency; Vector (nfreq)
  integer,pointer  :: ntor(:,:) => null()     ! /ntor - Toroidal mode numbers; Time dependent; Matrix (nfreq, max_nntor)
  integer,pointer  :: f_assumption(:,:) => null()     ! /f_assumption - Assumption on the functions distribution used by the wave solver to calculate the power deposition : 0 = Maxwellian (linear absorp
  real(DP),pointer  :: power_tot(:) => null()     ! /power_tot - Total absorbed wave power for each frequency [W]; Time-dependent; Vector (nfreq)
  real(DP),pointer  :: p_frac_ntor(:,:) => null()     ! /p_frac_ntor - Fraction of wave power per toroidal mode number; Time-dependent; Matrix (nfreq, max_nntor)
  real(DP),pointer  :: power_i(:,:) => null()     ! /power_i - Wave power absorbed by an ion species [W]; Time-dependent; Matrix (nfreq,nion)
  real(DP),pointer  :: power_e(:) => null()     ! /power_e - Wave power absorbed by the electrons [W]; Time-dependent; Vector (nfreq)
  real(DP),pointer  :: power_i_ntor(:,:,:) => null()     ! /power_i_ntor - Wave power absorbed by an ion species per toroidal mode number [W]; Time-dependent; Array 3D (nfreq,max_nntor,nion)
  real(DP),pointer  :: power_e_ntor(:,:) => null()     ! /power_e_ntor - Wave power absorbed by the electrons per toroidal mode number [W]; Time-dependent; Matrix (nfreq, max_nntor)
  integer,pointer  :: code_type(:) => null()      ! /code_type - Type of wave deposition code for a given frequency: 1=beam/ray tracing; 2=full wave; Vector(nfreq)
  integer,pointer  :: freq_point(:) => null()      ! /freq_point - Pointer to the frequency position in either the beamtracing or full wave branch for each frequency (the branch depends on code_typ
endtype

type type_waves_dep_profiles_flux_surf_avr  !    Flux surface averaged power deposition
  real(DP),pointer  :: powerd_tot(:,:) => null()     ! /powerd_tot - Total flux surface averaged wave power density [W/m^3]; Time-dependent; Matrix (nfreq, max_npsin)
  real(DP),pointer  :: powerd_e(:,:) => null()     ! /powerd_e - Flux surface averaged absorbed wave power density on electrons [W/m^3]; Time-dependent; Matrix (nfreq, max_npsin) 
  real(DP),pointer  :: powerd_i(:,:,:) => null()     ! /powerd_i - Flux surface averaged absorbed wave power density on ion species [W/m^3]; Time-dependent; Array3D (nfreq, max_npsin, nion) 
  real(DP),pointer  :: powerd_ntor(:,:,:) => null()     ! /powerd_ntor - Flux surface averaged power density for each toroidal mode number [W/m^3]; Time-dependent; Array 3D (nfreq, max_npsin, max_nntor) 
  real(DP),pointer  :: powerd_entor(:,:,:) => null()     ! /powerd_entor - Flux surface averaged absorbed power density for each toroidal mode number on electrons [W/m^3]; Time-dependent; Array 3D (nfreq, 
  real(DP),pointer  :: powerd_intor(:,:,:,:) => null()     ! /powerd_intor - Flux surface averaged power density for each toroidal mode number on each ions species [W/m^3]; Time-dependent; Array4D (nfreq, ma
endtype

type type_waves_dep_profiles_volume_intgr  !    Volume integrated power deposition (power deposited inside a given flux surface)
  real(DP),pointer  :: power_tot(:,:) => null()     ! /power_tot - Volume integrated absorbed wave power density [W]; Time-dependent; Matrix (nfreq, max_npsin) 
  real(DP),pointer  :: power_e(:,:) => null()     ! /power_e - Volume integrated absorbed wave power density on electrons [W]; Time-dependent; Matrix (nfreq, max_npsin) 
  real(DP),pointer  :: power_i(:,:,:) => null()     ! /power_i - Volume integrated absorbed wave power density on ion species [W]; Time-dependent; Array3D (nfreq, max_npsin, nion) 
  real(DP),pointer  :: power_ntor(:,:,:) => null()     ! /power_ntor - Volume integrated power density for each toroidal mode number [W]; Time-dependent; Array 3D (nfreq, max_npsin, max_nntor) 
  real(DP),pointer  :: power_e_ntor(:,:,:) => null()     ! /power_e_ntor - Volume integrated power density for each toroidal mode number [W]; Time-dependent; Array 3D (nfreq, max_npsin, max_nntor) 
  real(DP),pointer  :: power_i_ntor(:,:,:,:) => null()     ! /power_i_ntor - Volume integrated power density for each toroidal mode number on each ions species [W]; Time-dependent; Array4D (nfreq, max_npsin,
  real(DP),pointer  :: lin_curr_dr(:,:,:) => null()     ! /lin_curr_dr - Result of linear electron current drive calculation for each toroidal mode number (note: not consistent with other sources of curr
endtype

type type_waves_dep_profiles_two_dim  !    2D power deposition
  real(DP),pointer  :: powerd_tot(:,:,:) => null()     ! /powerd_tot - Total wave power density; Time-dependent [W/m^3]; Array 3D (nfreq, max_npsin, max_ntheta)
  real(DP),pointer  :: powerd_e(:,:,:) => null()     ! /powerd_e - Absorbed wave power density on electrons [W/m^3]; Time-dependent; Array3D (nfreq, max_npsin, max_ntheta) 
  real(DP),pointer  :: powerd_i(:,:,:,:) => null()     ! /powerd_i - Absorbed wave power density on ion species [W/m^3]; Time-dependent; Array4D (nfreq, max_npsin, max_ntheta, nion) 
  real(DP),pointer  :: powerd_ntor(:,:,:,:) => null()     ! /powerd_ntor - Absorbed power density for each toroidal mode number [W/m^3]; Time-dependent; Array 4D (nfreq, max_npsin, max_ntheta, max_nntor) 
  real(DP),pointer  :: powerd_entor(:,:,:,:) => null()     ! /powerd_entor - Absorbed power density for each toroidal mode number on electrons [W/m^3]; Time-dependent; Array 4D (nfreq, max_npsin, max_ntheta,
  real(DP),pointer  :: powerd_intor(:,:,:,:,:) => null()     ! /powerd_intor - Absorbed power density for each toroidal mode number on each ions species [W/m^3]; Time-dependent; Array5D (nfreq, max_npsin, max_
  real(DP),pointer  :: powerd_iharm(:,:,:,:,:) => null()     ! /powerd_iharm - Power density absorbed by an ion species for each toroidal mode numer at a given harmonic resonance ; Time-dependent (W/m^3); Arra
endtype

type type_waves_dep_profiles  !    Profiles of wave deposition quantities
   type (type_waves_2dgrid)  :: grid  ! /grid - Grid points in 2D
   type (type_waves_dep_profiles_volume_intgr)  :: volume_intgr  ! /volume_intgr - Volume integrated quantities.
   type (type_waves_dep_profiles_flux_surf_avr)  :: flux_surf_av  ! /flux_surf_av - Flux surface averaged quantities
   type (type_waves_dep_profiles_two_dim)  :: profiles_2d  ! /profiles_2d - 2D (in space) quantities
endtype

type type_waves_rtwavevector  !    Ray/beam wave vector
  real(DP),pointer  :: kr(:,:,:) => null()     ! /kr - Ray/beam wave vector in the major radius direction [m-1], Array (3D) of double precision real (nfreq_beam, max_nbeams, max_npoints
  real(DP),pointer  :: kz(:,:,:) => null()     ! /kz - Ray/beam wave vector in the vertical direction [m], Array (3D) of double precision real (nfreq_beam, max_nbeams, max_npoints). Tim
  real(DP),pointer  :: npar(:,:,:) => null()     ! /npar - Ray/beam parallel refractive index, Array (3D) of double precision real (nfreq_beam, max_nbeams, max_npoints). Time-dependent
  real(DP),pointer  :: nperp(:,:,:) => null()     ! /nperp - Ray/beam perpendicular refractive index, Array (3D) of double precision real (nfreq_beam, max_nbeams, max_npoints). Time-dependent
  real(DP),pointer  :: ntor(:,:,:) => null()     ! /ntor - Ray/beam toroidal wave number, Array (3D) of double precision real (nfreq_beam, max_nbeams, max_npoints/1). If var_ntor(nfreq_beam
  integer,pointer  :: var_ntor(:) => null()      ! /var_ntor - Flag telling whether ntor is constant along the ray path (0) or varying (1). Vector if integer (nfreq_beam).
endtype

type type_waves_rtposition  !    Ray/beam position
  real(DP),pointer  :: r(:,:,:) => null()     ! /r - Ray/beam major radius location [m], Array (3D) of double precision real (nfreq_beam, max_nbeams, max_npoints). Time-dependent
  real(DP),pointer  :: z(:,:,:) => null()     ! /z - Ray/beam vertical location [m], Array (3D) of double precision real (nfreq_beam, max_nbeams, max_npoints). Time-dependent
  real(DP),pointer  :: psin(:,:,:) => null()     ! /psin - Ray/beam normalized poloidal magnetic flux location, Array (3D) of double precision real (nfreq_beam, max_nbeams, max_npoints). Ti
  real(DP),pointer  :: theta(:,:,:) => null()     ! /theta - Ray/beam poloidal angle location [rd], Array (3D) of double precision real (nfreq_beam, max_nbeams, max_npoints). PRECISE THE DEFI
  real(DP),pointer  :: phi(:,:,:) => null()     ! /phi - Ray/beam toroidal angle location [rd], Array (3D) of double precision real (nfreq_beam, max_nbeams, max_npoints). Time-dependent
endtype

type type_polarization  !    
  real(DP),pointer  :: epol_p(:,:,:) => null()     ! /waves/beamtracing/polarization/epol_p - Electric field polarization vector in the p rotating coordinates, Array (3D) of double precision real (nfreq_beam, max_nbeams, max
  real(DP),pointer  :: epol_m(:,:,:) => null()     ! /waves/beamtracing/polarization/epol_m - Electric field polarization vector in the m rotating coordinates, Array (3D) of double precision real (nfreq_beam, max_nbeams, max
  real(DP),pointer  :: epol_par(:,:,:) => null()     ! /waves/beamtracing/polarization/epol_par - Electric field polarization vector in the magnetic field direction, Array (3D) of double precision real (nfreq_beam, max_nbeams, m
endtype

type type_powerflow  !    
  real(DP),pointer  :: phi_perp(:,:,:) => null()     ! /waves/beamtracing/powerflow/phi_perp - Normalized power flow in the direction perpendicular to the magnetic field, Array (3D) of double precision real (nfreq_beam, max_n
  real(DP),pointer  :: phi_par(:,:,:) => null()     ! /waves/beamtracing/powerflow/phi_par - Normalized power flow in the direction parallel to the magnetic field, Array (3D) of double precision real (nfreq_beam, max_nbeams
  real(DP),pointer  :: power_e(:,:,:) => null()     ! /waves/beamtracing/powerflow/power_e - Power absorbed along the beam by electrons. Array (3D) of double precision real (nfreq_beam, max_nbeams, max_npoints). Time-depend
  real(DP),pointer  :: power_i(:,:,:,:) => null()     ! /waves/beamtracing/powerflow/power_i - Power absorbed along the beam by an ion species [W]; Array (4D) of double precision real (nfreq_beam, max_nbeams, max_npoints, nio
endtype

type type_poloidal_decomp  !    
  integer,pointer  :: mpol(:,:) => null()     ! /waves/fullwave/poloidal_decomp/mpol - Poloidal mode numbers; Matrix (nfreq_fw, max_nmpol)
  integer,pointer  :: nmpol(:) => null()      ! /waves/fullwave/poloidal_decomp/nmpol - Number of poloidal mode numbers for each frequency; Vector (nfreq_fw)
  real(DP),pointer  :: e_plus_m(:,:,:,:) => null()     ! /waves/fullwave/poloidal_decomp/e_plus_m - Poloidal Fourier decomposition of left hand polarised component of the wave electric field; Time-dependent (V/m); Array 4D (nfreq_
  real(DP),pointer  :: e_minus_m(:,:,:,:) => null()     ! /waves/fullwave/poloidal_decomp/e_minus_m - Poloidal Fourier decomposition of right hand polarised component of the wave electric field; Time-dependent (V/m); Array 4D (nfreq
  real(DP),pointer  :: e_norm_m(:,:,:,:) => null()     ! /waves/fullwave/poloidal_decomp/e_norm_m - Poloidal Fourier decomposition of  wave electric field normal to a flux surface [V/m]; Time dependent; Array 4D (nfreq_fw, max_nto
  real(DP),pointer  :: e_bi_norm_m(:,:,:,:) => null()     ! /waves/fullwave/poloidal_decomp/e_bi_norm_m - Poloidal Fourier decomposition of wave electric field tangent to a flux surface [V/m]; Time dependent; Array 4D (nfreq_fw, max_nto
  real(DP),pointer  :: e_parallel_m(:,:,:,:) => null()     ! /waves/fullwave/poloidal_decomp/e_parallel_m - Poloidal Fourier decomposition of parallel wave electric field [V/m]; Time dependent; Array 4D (nfreq_fw, max_ntor, max_npsin, max
  real(DP),pointer  :: b_norm_m(:,:,:,:) => null()     ! /waves/fullwave/poloidal_decomp/b_norm_m - Poloidal Fourier decomposition of wave magnetic field normal to a flux surface [T]; Time dependent; Array 4D (nfreq_fw, max_ntor, 
  real(DP),pointer  :: b_bi_norm_m(:,:,:,:) => null()     ! /waves/fullwave/poloidal_decomp/b_bi_norm_m - Poloidal Fourier decomposition of wave magnetic field tangent to a flux surface [T]; Time dependent; Array 4D (nfreq_fw, max_ntor,
  real(DP),pointer  :: b_parallel_m(:,:,:,:) => null()     ! /waves/fullwave/poloidal_decomp/b_parallel_m - Wave magnetic field parallel to the equilibrium magnetic field [T]; Time dependent; Array 4D (nfreq_fw, max_ntor, max_npsin, max_n
endtype

type type_local  !    
  real(DP),pointer  :: e_plus(:,:,:,:) => null()     ! /waves/fullwave/local/e_plus - Left hand polarised component of the wave electric field; Time-dependent (V/m); Array 4D (nfreq_fw, max_ntor, max_npsin, max_nthet
  real(DP),pointer  :: e_minus(:,:,:,:) => null()     ! /waves/fullwave/local/e_minus - Right hand polarised component of the wave electric field; Time-dependent (V/m); Array 4D (nfreq_fw, max_ntor, max_npsin, max_nthe
  real(DP),pointer  :: e_norm(:,:,:,:) => null()     ! /waves/fullwave/local/e_norm - Wave electric field normal to a flux surface [V/m]; Time dependent; 4D (nfreq_fw, max_ntor, max_npsin, max_ntheta)
  real(DP),pointer  :: e_bi_norm(:,:,:,:) => null()     ! /waves/fullwave/local/e_bi_norm - Wave electric field tangent to a flux surface [V/m]; Time dependent; 4D (nfreq_fw, max_ntor, max_npsin, max_ntheta)
  real(DP),pointer  :: e_parallel(:,:,:,:) => null()     ! /waves/fullwave/local/e_parallel - Parallel wave electric field [V/m]; Time dependent; Array 4D (nfreq_fw, max_ntor, max_npsin, max_ntheta)
  real(DP),pointer  :: b_norm(:,:,:,:) => null()     ! /waves/fullwave/local/b_norm - Wave magnetic field normal to a flux surface [T]; Time dependent; Array 4D (nfreq_fw, max_ntor, max_npsin, max_ntheta)
  real(DP),pointer  :: b_bi_norm(:,:,:,:) => null()     ! /waves/fullwave/local/b_bi_norm - Wave magnetic field tangent to a flux surface [T]; Time dependent; Array 4D (nfreq_fw, max_ntor, max_npsin, max_ntheta)
  real(DP),pointer  :: b_parallel(:,:,:,:) => null()     ! /waves/fullwave/local/b_parallel - Wave magnetic field parallel to the equilibrium magnetic field [T]; Time dependent; Array 4D (nfreq_fw, max_ntor, max_npsin, max_n
endtype

type type_beamtracing  !    
  integer,pointer  :: nbeams(:) => null()      ! /waves/beamtracing/nbeams - Number of rays/beams for each antenna. Vector of integers (nfreq_beam)
  integer,pointer  :: npoints(:,:) => null()     ! /waves/beamtracing/npoints - Number of points along each ray/beam. Matrix of integers (nfreq_beam, max_nbeams)
  real(DP),pointer  :: power(:,:) => null()     ! /waves/beamtracing/power - Initial power in each ray/beam [W], Matrix (nfreq_beam, max_nbeams). Time-dependent
  real(DP),pointer  :: dnpar(:,:,:) => null()     ! /waves/beamtracing/dnpar - Spectral width associated with each ray/beam, Array (3D) of double precision real (nfreq_beam, max_nbeams, max_npoints). Time-depe
  real(DP),pointer  :: length(:,:,:) => null()     ! /waves/beamtracing/length - Ray/beam curvilinear length [m], Array (3D) of double precision real (nfreq_beam, max_nbeams, max_npoints). Time-dependent
   type (type_waves_rtposition)  :: position  ! /waves/beamtracing/position - Ray/beam position
   type (type_waves_rtwavevector)  :: wavevector  ! /waves/beamtracing/wavevector - Ray/beam wave vector.
  type (type_polarization) :: polarization  ! /waves/beamtracing/polarization - Wave field polarization along the ray/beam.
  type (type_powerflow) :: powerflow  ! /waves/beamtracing/powerflow - Power flow along the ray/beam.
endtype

type type_fullwave  !    
  type (type_poloidal_decomp) :: poloidal_decomp  ! /waves/fullwave/poloidal_decomp - Poloidal decomposition of the wave fields
  type (type_local) :: local  ! /waves/fullwave/local - Local description of the wave fields
endtype

type type_waves  !    
  type (type_datainfo) :: datainfo  ! /waves/datainfo - 
  type (type_composition) :: composition  ! /waves/composition - 
   type (type_waves_global_param)  :: global_param  ! /waves/global_param - Global wave deposition parameters
   type (type_waves_dep_profiles)  :: profiles  ! /waves/profiles - Profiles of wave deposition quantities
  type (type_beamtracing) :: beamtracing  ! /waves/beamtracing - Beam-tracing or ray-tracing solver
  type (type_fullwave) :: fullwave  ! /waves/fullwave - Solution by full wave code
  type (type_codeparam) :: codeparam  ! /waves/codeparam - 
  real(DP)  :: time=-9.0D40       ! /waves/time - Time [s]; Time-dependent; Scalar
endtype

type type_mdinfo  !    
  integer  :: shot_min=-999999999       ! /top/topinfo/mdinfo/shot_min - Minimum shot number to which the machine description applies
  integer  :: shot_max=-999999999       ! /top/topinfo/mdinfo/shot_max - Maximum shot number to which the machine description applies
   type (type_entry_def)  :: md_entry  ! /top/topinfo/mdinfo/md_entry - Entry of the machine description used. NB : just for information : for the moment, no guarantee that machine description data have
endtype

type type_topinfo  !    
  character(len=132), dimension(:), pointer ::dataprovider => null()       ! /top/topinfo/dataprovider - Name of the main data provider (the person who filled the original data)
  character(len=132), dimension(:), pointer ::description => null()       ! /top/topinfo/description - Pulse/Entry description
  character(len=132), dimension(:), pointer ::firstputdate => null()       ! /top/topinfo/firstputdate - Date of the original data submission
  character(len=132), dimension(:), pointer ::lastupdate => null()       ! /top/topinfo/lastupdate - Date of the last data addition in the tree
  character(len=132), dimension(:), pointer ::source => null()       ! /top/topinfo/source - Exact reference of the data source (e.g. original reference in the native machine data base)
  character(len=132), dimension(:), pointer ::comment => null()       ! /top/topinfo/comment - Any additional comment
  character(len=132), dimension(:), pointer ::dataversion => null()       ! /top/topinfo/dataversion - Version of the data structure
  character(len=132), dimension(:), pointer ::workflow => null()       ! /top/topinfo/workflow - Workflow which has been used to produce the present entry. Exact format to be defined with the platform group. User-specific input
   type (type_entry_def)  :: entry  ! /top/topinfo/entry - Definition of this database entry
   type (type_entry_def)  :: parent_entry  ! /top/topinfo/parent_entry - Definition of the entry of the direct parent (if any)
  type (type_mdinfo) :: mdinfo  ! /top/topinfo/mdinfo - Information related to machine description for this entry
endtype
 
type type_interfdiag  ! Special type CPO (lineintegraldiag) 
  type (type_datainfo) :: datainfo  ! /datainfo - 
  character(len=132), dimension(:), pointer ::expression => null()       ! /expression - Formal expression for the line integral to be evaluated as a function of ne, ni, Te, Ti, Zeff, Br, Bz
   type (type_setup_line)  :: setup_line  ! /setup_line - Geometric description of the lines of sight
   type (type_exp1D)  :: measure  ! /measure - Measured value. Time-dependent; Vector (nchords)
  real(DP)  :: time=-9.0D40       ! /time - Time [s]; Time-dependent; Scalar
end type
 
type type_polardiag  ! Special type CPO (lineintegraldiag) 
  type (type_datainfo) :: datainfo  ! /datainfo - 
  character(len=132), dimension(:), pointer ::expression => null()       ! /expression - Formal expression for the line integral to be evaluated as a function of ne, ni, Te, Ti, Zeff, Br, Bz
   type (type_setup_line)  :: setup_line  ! /setup_line - Geometric description of the lines of sight
   type (type_exp1D)  :: measure  ! /measure - Measured value. Time-dependent; Vector (nchords)
  real(DP)  :: time=-9.0D40       ! /time - Time [s]; Time-dependent; Scalar
end type

end module

