! ITM FORTRAN 90 type definitions

! Version phase 4.08b,  generated September 2010

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

type type_desc_impur  !    
  real(DP),pointer  :: amn(:) => null()     ! /desc_impur/amn - Atomic mass number of the impurity; Vector (nimp)
  integer,pointer  :: zn(:) => null()      ! /desc_impur/zn - Nuclear charge of the impurity; Vector (nimp)
  integer,pointer  :: i_ion(:) => null()      ! /desc_impur/i_ion - Index of the impurity species in the coreprof ion species ordering. Vector (nimp)
  integer,pointer  :: nzimp(:) => null()      ! /desc_impur/nzimp - Number of charge states (or bundles) considered for each impurity species. Vector (nimp)
  integer,pointer  :: zmin(:,:) => null()     ! /desc_impur/zmin - Minimum Z of impurity ionisation state bundle. Matrix (nimp,max_nzimp)
  integer,pointer  :: zmax(:,:) => null()     ! /desc_impur/zmax - Maximum Z of impurity ionisation state bundle. If no bundle, zmax=zmin. Matrix (nimp,max_nzimp)
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

type type_rzphidrdzdphi1D  !    Structure for list of R,Z,phi positions and width dR dZ dphi (1D)
  real(DP),pointer  :: r(:) => null()     ! /r - Position : major radius [m]
  real(DP),pointer  :: z(:) => null()     ! /z - Position : altitude [m]
  real(DP),pointer  :: phi(:) => null()     ! /phi - Position : toroidal angle [rad]
  real(DP),pointer  :: dr(:) => null()     ! /dr - Width : major radius [m]
  real(DP),pointer  :: dz(:) => null()     ! /dz - Width : altitude [m]
  real(DP),pointer  :: dphi(:) => null()     ! /dphi - Width : toroidal angle [rad]
endtype

type type_rzphi1Dexp  !    Structure for list of R,Z,phi positions (1D)
  type (type_exp1D) :: r  ! /r - Major radius [m]
  type (type_exp1D) :: z  ! /z - Altitude [m]
  type (type_exp1D) :: phi  ! /phi - Toroidal angle [rad]
endtype

type type_rzphi2D  !    Structure for list of R,Z,phi positions (2D)
  real(DP),pointer  :: r(:,:) => null()     ! /r - Major radius [m]
  real(DP),pointer  :: z(:,:) => null()     ! /z - Altitude [m]
  real(DP),pointer  :: phi(:,:) => null()     ! /phi - Toroidal angle [rad]
endtype

type type_rzphi3D  !    Structure for list of R,Z,phi positions (3D)
  real(DP),pointer  :: r(:,:,:) => null()     ! /r - Major radius [m]
  real(DP),pointer  :: z(:,:,:) => null()     ! /z - Altitude [m]
  real(DP),pointer  :: phi(:,:,:) => null()     ! /phi - Toroidal angle [rad]
endtype

type type_setup_line  !    Geometric description of the lines of sight for line integral diagnostic
  type (type_rzphi1D) :: pivot_point  ! /pivot_point - Pivot point of each line of sight; Vector (nchords)
  real(DP),pointer  :: horchordang1(:) => null()     ! /horchordang1 - Angle [rad] of horizontal projection of l.o.s. with poloidal cross section (0 for HFS to LFS chord - see Convention_angles_interfd
  real(DP),pointer  :: verchordang1(:) => null()     ! /verchordang1 - Angle of chord with vertical axis (0 for bottom-top chord, Pi for top-bottom chord - see Convention_angles_interfdiag.pdf) [rad]; 
  real(DP),pointer  :: width(:) => null()     ! /width - Width of the laser beam (1/e) [m]; Vector (nchords)
  type (type_rzphi1D) :: second_point  ! /second_point - Second point defining the line of sight together with the pivot_point. In case the probing wave is reflected, this should be the p
  real(DP),pointer  :: horchordang2(:) => null()     ! /horchordang2 - For reflected l.o.s. only (undefined otherwise) : Angle [rad] of horizontal projection of reflected l.o.s. with poloidal cross sec
  real(DP),pointer  :: verchordang2(:) => null()     ! /verchordang2 - For reflected l.o.s. only (undefined otherwise) : Angle of reflected chord with vertical axis (0 for bottom-top chord, Pi for top-
  type (type_rzphi1D) :: third_point  ! /third_point - Third point defining the reflected line of sight together with the second_point (undefined if the probing wave is not reflected). 
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
  type (type_offdiagion) :: off_diagonal  ! /off_diagonal - Details of the transport matrix, just for diagnostic (not used in transport equations). Time-dependent.
  integer  :: flag=-999999999       ! /flag - Flag describing the form of transport produced by the original model : 0- not calculated, 1- D and V, 2- flux, 3- full transport m
endtype

type type_transcoefel  !    Subtree containing transport coefficients from a transport model, for the electrons
  real(DP),pointer  :: diff_eff(:) => null()     ! /diff_eff - Effective diffusivity [m^2.s^-1]. Time-dependent. Vector (nrho)
  real(DP),pointer  :: vconv_eff(:) => null()     ! /vconv_eff - Effective convection [m.s^-1]. Time-dependent. Vector (nrho)
  real(DP),pointer  :: flux(:) => null()     ! /flux - Flux. Not used in transport equations [field.m.s^-1,.m^-3 if field is not a density itself]. Time-dependent. Vector (nrho)
  type (type_offdiagel) :: off_diagonal  ! /off_diagonal - Details of the transport matrix, just for diagnostic (not used in transport equations). Time-dependent.
  integer  :: flag=-999999999       ! /flag - Flag describing the form of transport produced by the original model : 0- not calculated, 1- D and V, 2- flux, 3- full transport m
endtype

type type_transcoefvtor  !    Subtree containing transport coefficients from a transport model, for the various ion species
  real(DP),pointer  :: diff_eff(:,:) => null()     ! /diff_eff - Effective diffusivity [m^2.s^-1]. Time-dependent. Matrix (nrho,nion)
  real(DP),pointer  :: vconv_eff(:,:) => null()     ! /vconv_eff - Effective convection [m.s^-1]. Time-dependent. Matrix (nrho,nion)
  real(DP),pointer  :: flux(:,:) => null()     ! /flux - Flux. Not used in transport equations [field.m.s^-1,.m^-3 if field is not a density itself]. Time-dependent. Matrix (nrho,nion)
  type (type_offdiagion) :: off_diagonal  ! /off_diagonal - Details of the transport matrix, just for diagnostic (not used in transport equations). Time-dependent.
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
  type (type_setup_line) :: setup_line  ! /setup_line - Geometric description of the lines of sight
  type (type_exp1D) :: measure  ! /measure - Measured value. Time-dependent; Vector (nchords)
  real(DP)  :: time=-9.0D40       ! /time - Time [s]; Time-dependent; Scalar
endtype

end module ! end of the utilities module

module     euitm_schemas       ! declaration of all CPOs

use euITM_utilities

integer, parameter, private :: DP=kind(1.0D0)

! ***********  Include amns.xsd
type type_table_info1  !    Information on the amns table
  integer,pointer  :: coord_extrap(:,:) => null()     ! /coord_extrap - 0 : none, report error; 1 : boundary value; 2: simple; Array(nproc1d, 2)
  integer  :: interp_type=-999999999       ! /interp_type - 1: linear; ... ; Vector(nproc1d)
  character(len=132), dimension(:), pointer ::coord_label => null()       ! /coord_label - description of the coordinate, string.
  character(len=132), dimension(:), pointer ::coord_unit => null()       ! /coord_unit - units of coordinate; string
  integer  :: coord_trans=-999999999       ! /coord_trans - 0 : none; 1 : log10; 2 : ln; Integer
  integer  :: unif_spacing=-999999999       ! /unif_spacing - for optimization purposes
endtype

type type_table_info2  !    Information on the amns table
  integer,pointer  :: coord_extrap(:,:,:) => null()     ! /coord_extrap - 0 : none, report error; 1 : boundary value; 2: simple; Array(nproc2d, 2, 2)
  integer,pointer  :: interp_type(:) => null()      ! /interp_type - 1: linear; ...  Vector(nproc2d)
  character(len=132), dimension(:), pointer ::coord_label => null()       ! /coord_label - description of each coordinate, Vector(2).
  character(len=132), dimension(:), pointer ::coord_unit => null()       ! /coord_unit - units of coordinate; Vector(2)
  integer,pointer  :: coord_trans(:) => null()      ! /coord_trans - 0 : none; 1 : log10; 2 : ln; Vector(2)
  integer  :: unif_spacing=-999999999       ! /unif_spacing - for optimization purposes
endtype

type type_table_info3  !    Information on the amns table
  integer,pointer  :: coord_extrap(:,:,:) => null()     ! /coord_extrap - 0 : none, report error; 1 : boundary value; 2: simple; Array(nproc3d, 2, 3)
  integer,pointer  :: interp_type(:) => null()      ! /interp_type - 1: linear; ... ; Vector(nproc3d)
  character(len=132), dimension(:), pointer ::coord_label => null()       ! /coord_label - description of each coordinate, Vector(3).
  character(len=132), dimension(:), pointer ::coord_unit => null()       ! /coord_unit - units of coordinate; Vector(3)
  integer,pointer  :: coord_trans(:) => null()      ! /coord_trans - 0 : none; 1 : log10; 2 : ln; Vector(3)
  integer  :: unif_spacing=-999999999       ! /unif_spacing - for optimization purposes
endtype

type type_table_info4  !    Information on the amns table
  integer,pointer  :: coord_extrap(:,:,:) => null()     ! /coord_extrap - 0 : none, report error; 1 : boundary value; 2: simple; Array(nproc4d, 2, 5)
  integer,pointer  :: interp_type(:) => null()      ! /interp_type - 1: linear; ... ; Vector(nproc4d)
  character(len=132), dimension(:), pointer ::coord_label => null()       ! /coord_label - description of each coordinate, Vector(4).
  character(len=132), dimension(:), pointer ::coord_unit => null()       ! /coord_unit - units of coordinate; Vector(4)
  integer,pointer  :: coord_trans(:) => null()      ! /coord_trans - 0 : none; 1 : log10; 2 : ln; Vector(4)
  integer  :: unif_spacing=-999999999       ! /unif_spacing - for optimization purposes
endtype

type type_table_info5  !    Information on the amns table
  integer,pointer  :: coord_extrap(:,:,:) => null()     ! /coord_extrap - 0 : none, report error; 1 : boundary value; 2: simple; Array(nproc5d, 2, 5)
  integer,pointer  :: interp_type(:) => null()      ! /interp_type - 1: linear; ... ; Vector(nproc5d)
  character(len=132), dimension(:), pointer ::coord_label => null()       ! /coord_label - description of each coordinate, Vector(5).
  character(len=132), dimension(:), pointer ::coord_unit => null()       ! /coord_unit - units of coordinate; Vector(5)
  integer,pointer  :: coord_trans(:) => null()      ! /coord_trans - 0 : none; 1 : log10; 2 : ln; Vector(5)
  integer  :: unif_spacing=-999999999       ! /unif_spacing - for optimization purposes
endtype

type type_table_0d  !    
  real(DP),pointer  :: table(:,:) => null()     ! /amns/tables/table_0d/table - interpolation data, Array(nz,nproc0d)
endtype

type type_table_1d  !    
  type (type_table_info1) :: table_prop  ! /amns/tables/table_1d/table_prop - Information on the properties of the table and the coordinates.
  real(DP),pointer  :: coord1(:) => null()     ! /amns/tables/table_1d/coord1 - value of coordinate;  Vector(ncoord1)
  real(DP),pointer  :: table(:,:,:) => null()     ! /amns/tables/table_1d/table - interpolation data, Array(ncoord1, nz, nproc1d)
endtype

type type_table_2d  !    
  type (type_table_info2) :: table_prop  ! /amns/tables/table_2d/table_prop - Information on the properties of the table and the coordinates.
  real(DP),pointer  :: coord1(:) => null()     ! /amns/tables/table_2d/coord1 - value of coordinate;  Vector(ncoord1)
  real(DP),pointer  :: coord2(:) => null()     ! /amns/tables/table_2d/coord2 - value of coordinate;  Vector(ncoord2)
  real(DP),pointer  :: table(:,:,:,:) => null()     ! /amns/tables/table_2d/table - Interpolation data , Array(ncoord1,ncoord2, nz, nproc2d)
endtype

type type_table_3d  !    
  type (type_table_info3) :: table_prop  ! /amns/tables/table_3d/table_prop - Information on the properties of the table and the coordinates.
  real(DP),pointer  :: coord1(:) => null()     ! /amns/tables/table_3d/coord1 - value of coordinate;  Vector(ncoord1)
  real(DP),pointer  :: coord2(:) => null()     ! /amns/tables/table_3d/coord2 - value of coordinate;  Vector(ncoord2)
  real(DP),pointer  :: coord3(:) => null()     ! /amns/tables/table_3d/coord3 - value of coordinate;  Vector(ncoord3)
  real(DP),pointer  :: table(:,:,:,:,:) => null()     ! /amns/tables/table_3d/table - interpolation data , Array(ncoord1,ncoord2,ncoord3, nz, nproc3d)
endtype

type type_table_4d  !    
  type (type_table_info4) :: table_prop  ! /amns/tables/table_4d/table_prop - Information on the properties of the table and the coordinates.
  real(DP),pointer  :: coord1(:) => null()     ! /amns/tables/table_4d/coord1 - value of coordinate;  Vector(ncoord1)
  real(DP),pointer  :: coord2(:) => null()     ! /amns/tables/table_4d/coord2 - value of coordinate;  Vector(ncoord2)
  real(DP),pointer  :: coord3(:) => null()     ! /amns/tables/table_4d/coord3 - value of coordinate;  Vector(ncoord3)
  real(DP),pointer  :: coord4(:) => null()     ! /amns/tables/table_4d/coord4 - value of coordinate;  Vector(ncoord4)
  real(DP),pointer  :: table(:,:,:,:,:,:) => null()     ! /amns/tables/table_4d/table - interpolation data , Array(ncoord1,ncoord2,ncoord3,ncoord4, nz, nproc4d)
endtype

type type_table_5d  !    
  type (type_table_info5) :: table_prop  ! /amns/tables/table_5d/table_prop - Information on the properties of the table and the coordinates.
  real(DP),pointer  :: coord1(:) => null()     ! /amns/tables/table_5d/coord1 - value of coordinate;  Vector(ncoord1)
  real(DP),pointer  :: coord2(:) => null()     ! /amns/tables/table_5d/coord2 - value of coordinate;  Vector(ncoord2)
  real(DP),pointer  :: coord3(:) => null()     ! /amns/tables/table_5d/coord3 - value of coordinate;  Vector(ncoord3)
  real(DP),pointer  :: coord4(:) => null()     ! /amns/tables/table_5d/coord4 - value of coordinate;  Vector(ncoord4)
  real(DP),pointer  :: coord5(:) => null()     ! /amns/tables/table_5d/coord5 - value of coordinate;  Vector(ncoord5)
  real(DP),pointer  :: table(:,:,:,:,:,:) => null()     ! /amns/tables/table_5d/table - interpolation data , Array(ncoord1,ncoord2,ncoord3,ncoord4,ncoord5, nz, nproc5d). DECLARED AS 6D ARRAY FOR THE MOMENT UNTIL WE UPD
endtype

type type_tables  !    
  integer,pointer  :: id(:,:) => null()     ! /amns/tables/id - Pointer to table: (1,jproc) indicates table dimensionality for process jproc; (2,jproc) indicates position in that table (index of
  type (type_table_0d) :: table_0d  ! /amns/tables/table_0d - 
  type (type_table_1d) :: table_1d  ! /amns/tables/table_1d - 
  type (type_table_2d) :: table_2d  ! /amns/tables/table_2d - 
  type (type_table_3d) :: table_3d  ! /amns/tables/table_3d - 
  type (type_table_4d) :: table_4d  ! /amns/tables/table_4d - 
  type (type_table_5d) :: table_5d  ! /amns/tables/table_5d - 
endtype

type type_amns  !    
  type (type_datainfo) :: datainfo  ! /amns/datainfo - 
  character(len=132), dimension(:), pointer ::version => null()       ! /amns/version - Version of the data.
  character(len=132), dimension(:), pointer ::source => null()       ! /amns/source - Source of the data.
  integer  :: zn=-999999999       ! /amns/zn - Nuclear charge [units of elementary charge];
  integer,pointer  :: zion(:) => null()      ! /amns/zion - Ion charge [units of elementary charge]. If negative value, means it is a bundle of charge state which cannot be described as sing
  real(DP)  :: amn=-9.0D40       ! /amns/amn - Mass of atom [amu]
  character(len=132), dimension(:), pointer ::state_label => null()       ! /amns/state_label - label for charge state (e.g. D0, D1+, ...); Vector(nz)
  character(len=132), dimension(:), pointer ::result_label => null()       ! /amns/result_label - description of each result; Vector(nprocs)
  character(len=132), dimension(:), pointer ::result_unit => null()       ! /amns/result_unit - units of result; Vector(nprocs)
  integer,pointer  :: result_trans(:) => null()      ! /amns/result_trans - 0 : none; 1 : 10**; 2 : exp; Vector(nprocs)
  integer  :: bundled=-999999999       ! /amns/bundled - 0 : none.
  character(len=132), dimension(:), pointer ::proc_label => null()       ! /amns/proc_label - Label for process (e.g. EI, RC; could also include error estimates); Vector(nprocs)
  type (type_tables) :: tables  ! /amns/tables - 
endtype

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
  integer,pointer  :: ima_theta(:,:) => null()     ! /modules/ima_theta - Position index of the module in the poloidal direction (from low theta to high theta, i.e. from bottom to top if the antenna is on
  integer,pointer  :: ima_phi(:,:) => null()     ! /modules/ima_phi - Position index of the module in the toroidal direction (from low phi to high phi, counter-clockwise when seen from above). Matrix 
  real(DP),pointer  :: sm_theta(:) => null()     ! /modules/sm_theta - Spacing between poloidally neighboring modules [m], Vector (nantenna_lh)
  type (type_exp2D) :: amplitude  ! /modules/amplitude - Amplitude of the TE10 mode injected in the module [W], Matrix (nantenna_lh,max_nmodules). Time-dependent
  type (type_exp2D) :: phase  ! /modules/phase - Phase of the TE10 mode injected in the module [rd], Matrix (nantenna_lh, max_nmodules). Time-dependent
  type (type_waveguides) :: waveguides  ! /modules/waveguides - Waveguides description
endtype

type type_straps  !    
  integer,pointer  :: nstraps(:) => null()      ! /straps/nstraps - Number of straps in each antenna; Vector (nantenna_ic)
  type (type_exp2D) :: phase  ! /straps/phase - Phase of strap current [rad];  Time-dependent; Matrix (nantenna_ic, max_nstraps)
  real(DP),pointer  :: phi_centre(:,:) => null()     ! /straps/phi_centre - Toroidal angle at the centre of the strap [rad]; Matrix (nantenna_ic, max_nstraps)
  real(DP),pointer  :: width(:,:) => null()     ! /straps/width - Width of strap in the toroidal direction [m]; Matrix (nantenna_ic, max_nstraps)
  real(DP),pointer  :: dist2wall(:,:) => null()     ! /straps/dist2wall - Distance to conducting wall or other conducter behind the antenna straps [m]; Matrix (nantenna_ic, max_nstraps)
  integer,pointer  :: ncoord_strap(:,:) => null()     ! /straps/ncoord_strap - Number of point in the polygon describing the antenna in the poloidal plane; Matrix (nantenna_ic, max_nstraps)
  type (type_rz3D) :: coord_strap  ! /straps/coord_strap - Coordinates (R,z) of polygon (of length ncoord_strap) describing the antenna in the poloidal plane;  rz3d array (nantenna_ic, max_
endtype

type type_antennalh_setup  !    Detailed description of LH antennas
  type (type_modules) :: modules  ! /modules - Modules description. NB there are nmodules per antenna, distributed among nma_phi toroidal positions and nma_theta poloidal positi
endtype

type type_antennaic_setup  !    Detailed description of ICRH antennas
  type (type_straps) :: straps  ! /straps - Properties of each IC antenna strap
endtype

type type_launchangles  !    
  real(DP),pointer  :: alpha(:) => null()     ! /antennas/antenna_ec/launchangles/alpha - Poloidal launching angle between the horizontal plane and the poloidal component of the nominal beam centerline (trigonometric con
  real(DP),pointer  :: beta(:) => null()     ! /antennas/antenna_ec/launchangles/beta - Toroidal launching angle between the horizontal plane and the poloidal component of the nominal beam centerline (trigonometric con
endtype

type type_plasmaedge  !    
  integer,pointer  :: npoints(:) => null()      ! /antennas/antenna_lh/plasmaedge/npoints - Number of points in the distance grid. Vector of integers (nantenna_lh).
  real(DP),pointer  :: distance(:,:) => null()     ! /antennas/antenna_lh/plasmaedge/distance - Grid for electron density, defined as the perpendicular distance to the antenna waveguide plane (the origin being described in the
  real(DP),pointer  :: density(:,:) => null()     ! /antennas/antenna_lh/plasmaedge/density - Electron density in front of the antenna [m^-3]. Matrix (nantenna_lh,max_npoints). Time-dependent.
endtype

type type_antenna_ec  !    
  character(len=132), dimension(:), pointer ::name => null()       ! /antennas/antenna_ec/name - Antenna name, Vector of strings (nantenna_ec)
  real(DP),pointer  :: frequency(:) => null()     ! /antennas/antenna_ec/frequency - Frequency [Hz], Vector (nantenna_ec)
  type (type_exp1D) :: power  ! /antennas/antenna_ec/power - Power [W], Vector (nantenna_ec). Time-dependent
  integer,pointer  :: mode(:) => null()      ! /antennas/antenna_ec/mode - Incoming wave mode (+ or -1 for O/X mode), Vector of integers (nantenna_ec). Time-dependent
  type (type_rzphi1D) :: position  ! /antennas/antenna_ec/position - Reference global position of the last mirror. Vectors (nantenna_ec). Time-dependent
  type (type_launchangles) :: launchangles  ! /antennas/antenna_ec/launchangles - Launching angles of the beam
  type (type_rf_beam) :: beam  ! /antennas/antenna_ec/beam - Beam characteristics
endtype

type type_antenna_ic  !    
  character(len=132), dimension(:), pointer ::name => null()       ! /antennas/antenna_ic/name - Antenna name; Vector of strings (nantenna_ic)
  type (type_exp1D) :: frequency  ! /antennas/antenna_ic/frequency - Frequency [Hz]; Time-dependent; Vector (nantenna_ic)
  type (type_exp1D) :: power  ! /antennas/antenna_ic/power - Power [W]; Time-dependent; Vector (nantenna_ic)
  type (type_antennaic_setup) :: setup  ! /antennas/antenna_ic/setup - Detailed description of IC antennas
endtype

type type_antenna_lh  !    
  character(len=132), dimension(:), pointer ::name => null()       ! /antennas/antenna_lh/name - Antenna name, Vector of strings (nantenna_lh)
  real(DP),pointer  :: frequency(:) => null()     ! /antennas/antenna_lh/frequency - Frequency [Hz], Vector (nantenna_lh)
  type (type_exp1D) :: power  ! /antennas/antenna_lh/power - Power [W], Vector (nantenna_lh). Time-dependent
  real(DP),pointer  :: n_par(:) => null()     ! /antennas/antenna_lh/n_par - Main parallel refractive index of the launched spectrum, for multi-junction antennas. Vectors (nantenna_lh). Time-dependent
  type (type_rzphi1Dexp) :: position  ! /antennas/antenna_lh/position - Reference global antenna position. Vectors (nantenna_lh). Time-dependent
  type (type_antennalh_setup) :: setup  ! /antennas/antenna_lh/setup - Detailed description of LH antennas.
  type (type_plasmaedge) :: plasmaedge  ! /antennas/antenna_lh/plasmaedge - Plasma edge characteristics in front of the antenna.
  type (type_rf_beam) :: beam  ! /antennas/antenna_lh/beam - Beam characteristics
endtype

type type_antennas  !    
  type (type_datainfo) :: datainfo  ! /antennas/datainfo - 
  type (type_antenna_ec) :: antenna_ec  ! /antennas/antenna_ec - Electron Cyclotron antennas
  type (type_antenna_ic) :: antenna_ic  ! /antennas/antenna_ic - Ion Cyclotron antennas
  type (type_antenna_lh) :: antenna_lh  ! /antennas/antenna_lh - Lower Hybrid antennas
  type (type_codeparam) :: codeparam  ! /antennas/codeparam - 
  real(DP)  :: time=-9.0D40       ! /antennas/time - Time [s]; Time-dependent; Scalar
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

type type_coreimpur  !    
  type (type_datainfo) :: datainfo  ! /coreimpur/datainfo - 
  real(DP),pointer  :: rho_tor_norm(:) => null()     ! /coreimpur/rho_tor_norm - Normalised toroidal flux coordinate values (= rho_tor normalised to the value at the last grid point); Vector (nrho)
  real(DP),pointer  :: rho_tor(:) => null()     ! /coreimpur/rho_tor - Toroidal flux coordinate (not normalised, equivalent to rho_tor_norm) [m];  Vector (nrho). Time-dependent.
  character(len=132), dimension(:), pointer ::source => null()       ! /coreimpur/source - Source of the profile (any comment describing the origin of the impurity profiles : code, path to diagnostic signals, massaging, .
  integer,pointer  :: flag(:) => null()      ! /coreimpur/flag - Flag describing how the profile has been processed : 0-not calculated; 1-interpretative; 2-calculated by the transport solver; 3-c
  type (type_desc_impur) :: desc_impur  ! /coreimpur/desc_impur - 
  real(DP),pointer  :: z(:,:,:) => null()     ! /coreimpur/z - Impurity ionisation state (averaged for bundle); Time-dependent; Array3D (nrho,nimp,max_nzimp)
  real(DP),pointer  :: zsq(:,:,:) => null()     ! /coreimpur/zsq - Z^2, Square of impurity ionisation state (averaged for bundle); Time-dependent; Array3D (nrho,nimp,max_nzimp)
  real(DP),pointer  :: nz(:,:,:) => null()     ! /coreimpur/nz - Density of impurity in a given charge state [m^-3]. Time-dependent; Array3D (nrho,nimp,max_nzimp)
  type (type_sourceimp) :: source_term  ! /coreimpur/source_term - Source term for each charge state. Time-dependent.
  type (type_boundaryimp) :: boundary  ! /coreimpur/boundary - Boundary condition for each charge state. Time-dependent
  type (type_coretransimp) :: transp_coef  ! /coreimpur/transp_coef - Transport coefficients for each charge state
  type (type_fluximp) :: flux  ! /coreimpur/flux - Fluxes of impurity particles, two definitions [m^-2.s^-1]. Time-dependent.
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
  type (type_boundary_neutrals) :: boundary  ! /boundary - Boundary condition for the transport equation. Time-dependent.
endtype

type type_corefieldneutrale  !    Structure for a main field of core neutral transport equations, (Temperature, with flux as energy); Time-dependent;
  real(DP),pointer  :: value(:,:,:) => null()     ! /value - Signal value; Array3D(nrho,nneut,max_ntype). Time-dependent
  real(DP),pointer  :: flux(:,:,:) => null()     ! /flux - Net flux of the kinetic energy through the magnetic surface (3/2*E*n*V), positive values correspond to the direction from the cent
  type (type_boundary_neutrals) :: boundary  ! /boundary - Boundary condition for the transport equation. Time-dependent.
endtype

type type_corefieldneutralv  !    Structure for a main field of core neutral transport equations (without flux variable); Time-dependent;
  real(DP),pointer  :: value(:,:,:) => null()     ! /value - Signal value; Array3D(nrho,nneut,max_ntype)Time-dependent; 
  type (type_boundary_neutrals) :: boundary  ! /boundary - Boundary condition for the transport equation. Time-dependent.
endtype

type type_corefieldneutralv0  !    Neutral velocity
  type (type_corefieldneutralv) :: toroidal  ! /toroidal - Neutral velocity in the toroidal direction [m.s^-1]. Positive is anti-clockwise when viewed from above. Time-dependent;
  type (type_corefieldneutralv) :: poloidal  ! /poloidal - Velocity of neutrals in the poloidal direction. 0 is directed towards low field side, pi is towards high field side. Positive is a
  type (type_corefieldneutralv) :: radial  ! /radial - Neutral velocity in the radial direction (perpendicular to the magnetic surface), positive is from the centre to the edge [m.s^-1]
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
  type (type_recycling_neutrals) :: recycling  ! /recycling - Recycling coefficients
  type (type_sputtering_neutrals) :: sputtering  ! /sputtering - Sputtering coefficients
endtype

type type_profiles_neutrals  !    Profiles derived from the fields solved in the transport equations, or from experiment.
  type (type_corefieldneutral) :: n0  ! /n0 - Neutral density [m^-3].  Time-dependent;
  type (type_corefieldneutrale) :: t0  ! /t0 - Neutral temperature [eV]. Time-dependent;
  type (type_corefieldneutralv0) :: v0  ! /v0 - Neutral velocity
  real(DP),pointer  :: prad0(:,:) => null()     ! /prad0 - Power radiated by neutrals [W.m^-3]. Matrix (nrho,nneut). Time-dependent.
endtype

type type_coreneutrals  !    
  type (type_datainfo) :: datainfo  ! /coreneutrals/datainfo - 
  real(DP),pointer  :: rho_tor(:) => null()     ! /coreneutrals/rho_tor - Toroidal flux coordinate (not normalised, equivalent to rho_tor_norm) [m];  Vector (nrho). Time-dependent.
  real(DP),pointer  :: rho_tor_norm(:) => null()     ! /coreneutrals/rho_tor_norm - Normalised toroidal flux coordinate values (= rho_tor normalised to the value at the last grid point); Vector (nrho)
  type (type_composition_neutrals) :: composition  ! /coreneutrals/composition - Description of neutrals species
  type (type_profiles_neutrals) :: profiles  ! /coreneutrals/profiles - Profiles derived from the fields solved in the transport equations, or from experiment.
  type (type_coefficients_neutrals) :: coefficients  ! /coreneutrals/coefficients - Recycling and sputtering coefficients used by the neutral solver. The nion index refers to the various ions (and charge states) co
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
  real(DP),pointer  :: derivative(:) => null()     ! /derivative - Radial derivative (dvalue/drho_tor) [signal_value_unit.m^-1]; Time-dependent; Vector (nrho)
  character(len=132), dimension(:), pointer ::source => null()       ! /source - Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); Strin
  integer  :: flag=-999999999       ! /flag - Flag describing how the profile has been processed : 0-not calculated 1-interpretative; 2-calculated by the transport solver; 3-ca
  type (type_boundaryel) :: boundary  ! /boundary - Boundary condition for the transport equation. Time-dependent.
  type (type_sourceel) :: source_term  ! /source_term - Total source term for the transport equation. Time-dependent.
  type (type_coretransel) :: transp_coef  ! /transp_coef - Total transport coefficients. Time-dependent.
  type (type_fluxel) :: flux  ! /flux - Fluxes of the quantity, two definitions. Time-dependent.
  real(DP),pointer  :: time_deriv(:) => null()     ! /time_deriv - Integral of the time derivative term of the transport equation. Time-dependent. Vector (nrho)
  type (type_codeparam) :: codeparam  ! /codeparam - 
endtype

type type_corefieldion  !    Structure for an ion field of core transport equations; Time-dependent;
  real(DP),pointer  :: value(:,:) => null()     ! /value - Signal value; Time-dependent; Matrix (nrho,nion)
  real(DP),pointer  :: derivative(:,:) => null()     ! /derivative - Radial derivative (dvalue/drho_tor) [signal_value_unit.m^-1]; Time-dependent; Matrix (nrho,nion)
  character(len=132), dimension(:), pointer ::source => null()       ! /source - Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); Array
  integer,pointer  :: flag(:) => null()      ! /flag - Flag describing how the profile has been processed : 0-not calculated 1-interpretative; 2-calculated by the transport solver; 3-ca
  type (type_boundaryion) :: boundary  ! /boundary - Boundary condition for the transport equation
  type (type_sourceion) :: source_term  ! /source_term - Total source term for the transport equation. Time-dependent.
  type (type_coretransion) :: transp_coef  ! /transp_coef - Total transport coefficients. Time-dependent.
  type (type_fluxion) :: flux  ! /flux - Fluxes of the quantity, two definitions. Time-dependent.
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
  real(DP),pointer  :: derivative(:) => null()     ! /coreprof/psi/derivative - Radial derivative (dvalue/drho_tor) [Wb.m^-1]; Time-dependent; Vector (nrho)
  character(len=132), dimension(:), pointer ::source => null()       ! /coreprof/psi/source - Source of the profile (any comment describing the origin of the profile : code, path to diagnostic signals, massaging, ...); Strin
  integer  :: flag=-999999999       ! /coreprof/psi/flag - Flag describing how the profile has been processed : 0-not calculated; 1-interpretative; 2-calculated by the transport solver; 3-c
  type (type_boundary) :: boundary  ! /coreprof/psi/boundary - Boundary condition for the transport equation. Time-dependent.
  type (type_jni) :: jni  ! /coreprof/psi/jni - Non-inductive parallel current density [A/m^2]; Time-dependent;
  type (type_coreprofile) :: sigma_par  ! /coreprof/psi/sigma_par - Parallel conductivity [ohm^-1.m^-1]. Time-dependent
  type (type_codeparam) :: codeparam  ! /coreprof/psi/codeparam - 
endtype

type type_profiles1d  !    
  type (type_coreprofile) :: pe  ! /coreprof/profiles1d/pe - Electron pressure [Pa]; Time-dependent;
  type (type_coreprofion) :: pi  ! /coreprof/profiles1d/pi - Ion pressure [Pa]; Time-dependent;
  type (type_coreprofile) :: pr_th  ! /coreprof/profiles1d/pr_th - Thermal pressure (electrons+ions) [Pa]; Time-dependent;
  type (type_coreprofile) :: pr_perp  ! /coreprof/profiles1d/pr_perp - Total perpendicular pressure (electrons+ions, thermal+non-thermal) [Pa]; Time-dependent;
  type (type_coreprofile) :: pr_parallel  ! /coreprof/profiles1d/pr_parallel - Total parallel pressure (electrons+ions, thermal+non-thermal) [Pa]; Time-dependent;
  type (type_coreprofile) :: jtot  ! /coreprof/profiles1d/jtot - total parallel current density = average(jtot.B) / B0, where B0 = coreprof/toroid_field/b0 [A/m^2]; Time-dependent;
  type (type_coreprofile) :: jni  ! /coreprof/profiles1d/jni - non-inductive parallel current density = average(jni.B) / B0, where B0 = coreprof/toroid_field/b0 [A/m^2]; Time-dependent;
  type (type_coreprofile) :: joh  ! /coreprof/profiles1d/joh - ohmic parallel current density = average(joh.B) / B0, where B0 = coreprof/toroid_field/b0 [A/m^2]; Time-dependent;
  type (type_coreprofile) :: vloop  ! /coreprof/profiles1d/vloop - Toroidal loop voltage [V].  Time-dependent.
  type (type_coreprofile) :: sigmapar  ! /coreprof/profiles1d/sigmapar - Parallel conductivity [ohm^-1.m^-1].  Time-dependent. 
  type (type_coreprofile) :: qoh  ! /coreprof/profiles1d/qoh - ohmic heating [W/m^3]; Time-dependent;
  type (type_coreprofile) :: eparallel  ! /coreprof/profiles1d/eparallel - Parallel electric field = average(E.B) / B0, where B0 = coreprof/toroid_field/b0 [V.m^-1].  Time-dependent. 
  type (type_coreprofile) :: e_b  ! /coreprof/profiles1d/e_b - Average(E.B) [V.T.m^-1].  Time-dependent. 
  type (type_coreprofile) :: q  ! /coreprof/profiles1d/q - Safety factor profile; Time-dependent;
  type (type_coreprofile) :: shear  ! /coreprof/profiles1d/shear - Magnetic shear profile; Time-dependent;
  type (type_coreprofion) :: ns  ! /coreprof/profiles1d/ns - Density of fast ions, for the various ion species [m^-3]; Time-dependent;
  type (type_coreprofion) :: mtor  ! /coreprof/profiles1d/mtor - Toroidal momentum of the various ion species [UNITS?]; Time-dependent;
  type (type_coreprofion) :: wtor  ! /coreprof/profiles1d/wtor - Angular toroidal rotation frequency of the various ion species [s^-1]; Time-dependent;
  type (type_coreprofile) :: zeff  ! /coreprof/profiles1d/zeff - Effective charge profile; Time-dependent;
  type (type_coreprofile) :: bpol  ! /coreprof/profiles1d/bpol - Average poloidal magnetic field, defined as sqrt(ave(grad rho^2/R^2)).dpsi/drho [T].  Time-dependent.
  type (type_coreprofile) :: dpsidt  ! /coreprof/profiles1d/dpsidt - Time derivative of the  poloidal flux at constant rho_tor_norm [V].  Time-dependent.
  type (type_coreprofile) :: dpsidt_phi  ! /coreprof/profiles1d/dpsidt_phi - Time derivative of the  poloidal flux at constant toroidal flux [V].  Time-dependent.
  type (type_coreprofile) :: dvprimedt  ! /coreprof/profiles1d/dvprimedt - Time derivative of the radial derivative of the volume enclosed in the flux surface, i.e. d/dt(dV/drho_tor) [m^2.s^-1]; Time-depen
endtype

type type_globalparam  !    
  real(DP)  :: current_tot=-9.0D40       ! /coreprof/globalparam/current_tot - Total plasma current [A]; Time-dependent; Scalar
  real(DP)  :: current_bnd=-9.0D40       ! /coreprof/globalparam/current_bnd - Plasma current inside transport solver boundary rho_tor_bnd [A]; Time-dependent; Scalar
  real(DP)  :: current_ni=-9.0D40       ! /coreprof/globalparam/current_ni - Total non-inductive parallel current [A]; Time-dependent; Scalar
  real(DP)  :: vloop=-9.0D40       ! /coreprof/globalparam/vloop - Toroidal loop voltage [V]; Time-dependent; Scalar
  real(DP)  :: li=-9.0D40       ! /coreprof/globalparam/li - Internal inductance; Time-dependent; Scalar
  real(DP)  :: beta_tor=-9.0D40       ! /coreprof/globalparam/beta_tor - toroidal beta; Time-dependent; Scalar
  real(DP)  :: beta_normal=-9.0D40       ! /coreprof/globalparam/beta_normal - normalised beta; Time-dependent; Scalar
  real(DP)  :: beta_pol=-9.0D40       ! /coreprof/globalparam/beta_pol - poloidal beta; Time-dependent; Scalar
  real(DP)  :: w_dia=-9.0D40       ! /coreprof/globalparam/w_dia - Plasma energy content = 3/2 * int(p,dV) with p being the total pressure (pr_th + pr_perp). Time-dependent; Scalar
endtype

type type_coreprof  !    
  type (type_datainfo) :: datainfo  ! /coreprof/datainfo - 
  real(DP),pointer  :: rho_tor_norm(:) => null()     ! /coreprof/rho_tor_norm - Normalised toroidal flux coordinate values (= rho_tor normalised to the value at the last grid point); Vector (nrho)
  real(DP),pointer  :: rho_tor(:) => null()     ! /coreprof/rho_tor - Toroidal flux coordinate (not normalised, equivalent to rho_tor_norm) [m];  Vector (nrho). Time-dependent.
  real(DP),pointer  :: drho_dt(:) => null()     ! /coreprof/drho_dt - Time derivative of rho_tor  [m/s];  Vector (nrho). Time-dependent.
  type (type_toroid_field) :: toroid_field  ! /coreprof/toroid_field - Toroidal field information  entering the definition of rho_tor, for reference only. The physical value of the toroidal field shoul
  type (type_composition) :: composition  ! /coreprof/composition - 
  type (type_psi) :: psi  ! /coreprof/psi - Poloidal magnetic flux [Wb]; Time-dependent;
  type (type_corefield) :: te  ! /coreprof/te - Electron temperature [eV]; (source term in [W.m^-3]). Time-dependent;
  type (type_corefieldion) :: ti  ! /coreprof/ti - Ion temperature [eV]; (source term in [W.m^-3]). Time-dependent;
  type (type_corefield) :: ne  ! /coreprof/ne - Electron density [m^-3]; (source term in [m^-3]).Time-dependent;
  type (type_corefieldion) :: ni  ! /coreprof/ni - Ion density [m^-3]; (source term in [m^-3]). Time-dependent;
  type (type_corefieldion) :: vtor  ! /coreprof/vtor - Toroidal velocity of the various ion species [m.s^-1]; Time-dependent;
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
  type (type_b0r0) :: toroid_field  ! /coresource/toroid_field - Characteristics of the vacuum toroidal field, redundant with the toroidfield CPO, to document the normalisation of j in this CPO.
  real(DP),pointer  :: j(:) => null()     ! /coresource/j - Parallel current source for psi transport equation, = average(j.B) / B0, where B0  = coresource/toroid_field/b0 [A.m^-2]. Vector(n
  real(DP),pointer  :: sigma(:) => null()     ! /coresource/sigma - Induced parallel conductivity [ohm^-1.m^-1]. EXACT DEFINITION PENDING. Vector(nrho). Time-dependent.
  type (type_source_ion) :: si  ! /coresource/si - Particle source for ion density transport equation [m^-3.s^-1]. Time-dependent.
  type (type_source_el) :: se  ! /coresource/se - Particle source for electron density transport equation [m^-3.s^-1]. Time-dependent.
  type (type_source_imp) :: sz  ! /coresource/sz - Particle source for impurity density transport equation [m^-3.s^-1]. Time-dependent.
  type (type_source_ion) :: qi  ! /coresource/qi - Heat source for ion heat transport equations [W.m^-3]. Time-dependent.
  type (type_source_el) :: qe  ! /coresource/qe - Heat source for electron heat transport equation [W.m^-3]. Time-dependent.
  type (type_source_imp) :: qz  ! /coresource/qz - Heat source for impurity heat transport equations [W.m^-3].  Time-dependent.
  type (type_source_ion) :: ui  ! /coresource/ui - Velocity source for toroidal velocity transport equation [kg.m^-1.s^-2]. Vector(nrho). Time-dependent.
  type (type_codeparam) :: codeparam  ! /coresource/codeparam - 
  real(DP)  :: time=-9.0D40       ! /coresource/time - Time [s]; Time-dependent; Scalar
endtype

! ***********  Include coretransp.xsd
type type_ni_transp  !    
  real(DP),pointer  :: diff_eff(:,:,:) => null()     ! /coretransp/ni_transp/diff_eff - Effective diffusivity [m^2.s^-1]. The last index of the array describes which multiplier should be applied to the particule flux w
  real(DP),pointer  :: vconv_eff(:,:,:) => null()     ! /coretransp/ni_transp/vconv_eff - Effective convection [m.s^-1]. The last index of the array describes which multiplier should be applied to the particule flux when
  real(DP),pointer  :: flux(:,:) => null()     ! /coretransp/ni_transp/flux - Flux. Not used in transport equations [field.m.s^-1,.m^-3 if field is not a density itself]. Time-dependent. Matrix (nrho,nion)
  type (type_offdiagion) :: off_diagonal  ! /coretransp/ni_transp/off_diagonal - Details of the transport matrix, just for diagnostic (not used in transport equations). Time-dependent.
  integer  :: flag=-999999999       ! /coretransp/ni_transp/flag - Flag describing the form of transport produced by the original model : 0- not calculated, 1- D and V, 2- flux, 3- full transport m
endtype

type type_ne_transp  !    
  real(DP),pointer  :: diff_eff(:,:) => null()     ! /coretransp/ne_transp/diff_eff - Effective diffusivity [m^2.s^-1]. The last index of the array describes which multiplier should be applied to the particule flux w
  real(DP),pointer  :: vconv_eff(:,:) => null()     ! /coretransp/ne_transp/vconv_eff - Effective convection [m.s^-1]. The last index of the array describes which multiplier should be applied to the particule flux when
  real(DP),pointer  :: flux(:) => null()     ! /coretransp/ne_transp/flux - Flux. Not used in transport equations [field.m.s^-1,.m^-3 if field is not a density itself]. Time-dependent. Vector (nrho)
  type (type_offdiagel) :: off_diagonal  ! /coretransp/ne_transp/off_diagonal - Details of the transport matrix, just for diagnostic (not used in transport equations). Time-dependent.
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
  type (type_transcoefimp) :: nz_transp  ! /coretransp/nz_transp - Transport coefficients for impurity (multiple charge state) density equation. Time-dependent.
  type (type_transcoefion) :: ti_transp  ! /coretransp/ti_transp - Transport coefficients for ion temperature equation. Time-dependent.
  type (type_transcoefel) :: te_transp  ! /coretransp/te_transp - Transport coefficients for electron temperature equation. Time-dependent.
  type (type_transcoefimp) :: tz_transp  ! /coretransp/tz_transp - Transport coefficients for impurity  (multiple charge state) temperature equation. Time-dependent.
  type (type_transcoefvtor) :: vtor_transp  ! /coretransp/vtor_transp - Transport coefficients for toroidal velocity equation. Time-dependent.
  type (type_codeparam) :: codeparam  ! /coretransp/codeparam - 
  real(DP)  :: time=-9.0D40       ! /coretransp/time - Time [s]; Time-dependent; Scalar
endtype

! ***********  Include cxdiag.xsd
type type_cxsetup  !    diagnostic setup information
  type (type_rzphi1Dexp) :: position  ! /position - Position of the measurement. Time-dependent. Vector (nchannels)
endtype

type type_cxmeasure  !    Measured values
  type (type_exp1D) :: ti  ! /ti - Ion temperature [eV]. Vector (nchannels)
  type (type_exp1D) :: vtor  ! /vtor - Toroidal velocity [m/s]. Vector (nchannels)
  type (type_exp1D) :: vpol  ! /vpol - Poloidal velocity [m/s]. Vector (nchannels)
endtype

type type_cxdiag  !    
  type (type_datainfo) :: datainfo  ! /cxdiag/datainfo - 
  type (type_cxsetup) :: setup  ! /cxdiag/setup - diagnostic setup information
  type (type_cxmeasure) :: measure  ! /cxdiag/measure - Measured values
  real(DP)  :: time=-9.0D40       ! /cxdiag/time - Time [s]; Time-dependent; Scalar
endtype

! ***********  Include distribution.xsd
type type_dist_src_snk_vol  !    
  real(DP),pointer  :: particles(:,:) => null()     ! /particles - Source/sink particles [1/s]; Time-dependedent; Matrix(ndist_spec, max_npsi)
  real(DP),pointer  :: power(:,:) => null()     ! /power - Power associated with the source/sink of particles [W]; Time-dependent; Matrix(ndist_spec, max_npsi)
  real(DP),pointer  :: torque(:,:) => null()     ! /torque - Torque due to the source/sink of particles [N.m]; Time-dependent; Matrix (ndist_spec, max_npsi)
endtype

type type_dist_src_snk_surf  !    
  real(DP),pointer  :: particlesd(:,:) => null()     ! /particlesd - Source/sink  particles [s^-1 m^-3]; Time-dependedent; Matrix(ndist_spec, max_npsi)
  real(DP),pointer  :: powerd(:,:) => null()     ! /powerd - Power density associated with the source/sink of particles [W.m^-3]; Time-dependent; Matrix(ndist_spec, max_npsi)
  real(DP),pointer  :: torqued(:,:) => null()     ! /torqued - Torque density due to the source/sink of particles [N.m^-2]; Time-dependent; Matrix(ndist_spec, max_npsi)
endtype

type type_dist_src_snk_tot  !    
  real(DP),pointer  :: particles(:) => null()     ! /particles - Source/sink  particles [1/s]; Time-dependedent; Vector(ndist_spec)
  real(DP),pointer  :: power(:) => null()     ! /power - Power associated with the source/sink of particles [W]; Time-dependent; Vector(ndist_spec)
  real(DP),pointer  :: torque(:) => null()     ! /torque - Torque due to the source/sink of particles [N.m]; Time-dependent; Vector (ndist_spec).
endtype

type type_dist_test_part  !    Distribution given as a set of test particles.
  real(DP),pointer  :: nvar(:) => null()     ! /nvar - Number of variables associated with a test particle; Vector (ndist_spec)
  integer,pointer  :: var_id(:,:) => null()     ! /var_id - Identification of a variable; Matrix (ndist_spec, 5)
  real(DP),pointer  :: var1(:,:) => null()     ! /var1 - Phase space variables one characterising a test particle; Time-dependent; Matrix (ndist_spec, ntpart)
  real(DP),pointer  :: var2(:,:) => null()     ! /var2 - Phase space variables two characterising a test particle; Time-dependent; Matrix  (ndist_spec, ntpart)
  real(DP),pointer  :: var3(:,:) => null()     ! /var3 - Phase space variables three characterising a test particle; Time-dependent; Matrix (ndist_spec, ntpart)
  real(DP),pointer  :: var4(:,:) => null()     ! /var4 - Phase space variables four characterising a test particle; Time-dependent; Matrix  (ndist_spec, ntpart)
  real(DP),pointer  :: var5(:,:) => null()     ! /var5 - Phase space variables five characterising a test particle; Time-dependent; Matrix  (ndist_spec, ntpart)
  real(DP),pointer  :: var6(:,:) => null()     ! /var6 - Phase space variables six characterising a test particle; Time-dependent; Matrix  (ndist_spec, ntpart)
  real(DP),pointer  :: weight(:,:) => null()     ! /weight - Weight of a test particle; Time-dependent; Matrix (ndist_spec, ntpart)
endtype

type type_dist_grid  !    Grid on which the distribution function is calculated.
  real(DP),pointer  :: dim1(:,:) => null()     ! /dim1 - First dimension in phase space; Time-dependent; Matrix (ndist_spec, max_ndim1).
  integer,pointer  :: ndim1(:) => null()      ! /ndim1 - Size of the first dimension in phase space, for each species; Vector (ndist_spec).
  real(DP),pointer  :: dim2(:,:) => null()     ! /dim2 - Second dimension in phase space; Time-dependent; Matrix (ndist_spec, max_ndim2).
  integer,pointer  :: ndim2(:) => null()      ! /ndim2 - Size of the second dimension in phase space, for each species; Vector (ndist_spec).
  real(DP),pointer  :: dim3(:,:) => null()     ! /dim3 - Third dimension in phase space; Time-dependent; Matrix (ndist_spec, max_ndim3).
  integer,pointer  :: ndim3(:) => null()      ! /ndim3 - Size of the third dimension in phase space, for each species; Vector (ndist_spec).
  real(DP),pointer  :: jacobian(:,:,:,:) => null()     ! /jacobian - Jacobian of the transformation of the phase space grid variables; Time-dependent; Array4d(ndist_spec, max_ndim1, max_ndim2, max_nd
endtype

type type_dist_ff  !    Orbit averaged (or Bounce averaged) zero order distribution function.
  integer,pointer  :: grid_type(:) => null()      ! /grid_type - Type of grid. Vector (ndist_spec).
  type (type_dist_grid) :: grid  ! /grid - Grid on which the distribution function is calculated.
  real(DP),pointer  :: value(:,:,:,:) => null()     ! /value - Orbit (or bounce) averaged distribution function given on a grid [1/m^3 (m/s)^-3]; Time-dependent; array 4d(ndist_spec, max_ndim1,
endtype

type type_dist_particle_src  !    Particle source
  type (type_dist_src_snk_tot) :: total  ! /total - Total source of particles and power (NBI, fusion products, pellets etc.)
  type (type_dist_src_snk_vol) :: volume_intgr  ! /volume_intgr - Volume integrated source of particles and power (NBI, fusion products, pellets etc.)
  type (type_dist_src_snk_surf) :: flux_surf_av  ! /flux_surf_av - Flux surface averaged source of particles and power (NBI, fusion products, pellets etc.)
endtype

type type_dist_wave_src  !    Auxiliary wave absorbed by the distribution species
  character(len=132), dimension(:), pointer ::type => null()       ! /type - Wave type (LH, EC, IC, ...), can be a combination of these if several wave types are absorbed by this species. Vector of strings (
  real(DP),pointer  :: wave_power(:) => null()     ! /wave_power - Auxiliary wave power absorbed by the distribution species [W]; Time-dependent; Vector (ndist_spec).
  real(DP),pointer  :: wave_powerd(:,:) => null()     ! /wave_powerd - Auxiliary flux surface averaged wave power density absorbed by the distribution species [W/m^3]; Time-dependent; Matrix (ndist_spe
endtype

type type_dist_input_src  !    Input sources of particles and power for the distribution species (to aid diagnosing the code output).
  type (type_dist_particle_src) :: particle_src  ! /particle_src - Particle source
  type (type_dist_wave_src) :: wave_src  ! /wave_src - Auxiliary wave absorbed by the distribution species
endtype

type type_dist_prof_vol_dist_losses  !    Losses of the distribution species.
  type (type_dist_src_snk_vol) :: orb_loss  ! /orb_loss - Losses due to orbits intersecting a material surface.
  type (type_dist_src_snk_vol) :: neutr_loss  ! /neutr_loss - Losses due to neutralised ions, e.g. due to charge exchange events.
endtype

type type_dist_prof_surf_dist_losses  !    Losses of the distribution species.
  type (type_dist_src_snk_surf) :: orb_loss  ! /orb_loss - Losses due to orbits intersecting a material surface.
  type (type_dist_src_snk_surf) :: neutr_loss  ! /neutr_loss - Losses due to neutralised ions, e.g. due to charge exchange events.
endtype

type type_dist_prof_surf_nucl_reac_th  !    Nuclear reactions between the cacluated species and oher species assumed to have thermal distributions.
  real(DP),pointer  :: rated(:,:,:) => null()     ! /rated - Reaction rate [s^-1.m^-3]; Time dependent; Array3d(ndist_spec, nreac_max, max_npsi)
  real(DP),pointer  :: powerd(:,:,:) => null()     ! /powerd - Nuclear reaction power density [W.m^-3]; Time dependent; Array3d(ndist_spec, nreac_max, max_npsi)
endtype

type type_dist_prof_surf_nucl_reac_sf  !    Nuclear reactions of the calculated species with itself (thermal + non-thermal).
  real(DP),pointer  :: rate(:,:) => null()     ! /rate - Reaction rate [s^-1.m^-3]; Time-dependent; Matrix (ndist_spec, max_npsi)
  real(DP),pointer  :: power(:,:) => null()     ! /power - Fusion reaction power [W.m^-3]; Time-dependent; Matrix (ndist_spec, max_npsi)
endtype

type type_dist_nucl_reac_sf  !    Nuclear reactions of the calculated species with itself (thermal + non-thermal).
  real(DP),pointer  :: rate(:) => null()     ! /rate - Reaction rate [1/s]; Time-dependent; Vector (ndist_spec)
  real(DP),pointer  :: power(:) => null()     ! /power - Fusion reaction power[W]; Time-dependent; Vector (ndist_spec)
endtype

type type_dist_prof_vol_nucl_reac_sf  !    Nuclear reactions of the calculated species with itself (thermal + non-thermal).
  real(DP),pointer  :: rate(:,:) => null()     ! /rate - Reaction rate [1/s]; Time-dependent; Matrix (ndist_spec, max_npsi)
  real(DP),pointer  :: power(:,:) => null()     ! /power - Fusion reaction power[W]; Time-dependent; Matrxi (ndist_spec, max_npsi)
endtype

type type_dist_prof_vol_nucl_reac_th  !    Nuclear reactions between the calculated species and other species assumed to have thermal distributions.
  real(DP),pointer  :: rate(:,:,:) => null()     ! /rate - Reaction rate [1/s]; Time-dependent; Array3D (ndist_spec, max_nreac, max_npsi)
  real(DP),pointer  :: power(:,:,:) => null()     ! /power - Fusion reaction power[W]; Time-dependent; Array3D(ndist_spec, max_nreac, max_npsi)
endtype

type type_dist_nucl_reac_th  !    Nuclear reactions between the calculated species and other species assumed to have thermal distributions.
  real(DP),pointer  :: rate(:,:) => null()     ! /rate - Reaction rate [1/s]; Time-dependent; Matrix (ndist_spec, max_nreac)
  real(DP),pointer  :: power(:,:) => null()     ! /power - Fusion reaction power[W]; Time-dependent; Matrix(ndist_spec, max_nreac)
endtype

type type_dist_glob_dist_losses  !    Losses of the distribution species (orbit losses and neutralisation losses).
  type (type_dist_src_snk_tot) :: orb_loss  ! /orb_loss - Losses due to orbits intersecting a material surface.
  type (type_dist_src_snk_tot) :: neutr_loss  ! /neutr_loss - Losses due to neutralisation of distribution ions (charge exchange etc.)
endtype

type type_dist_glob  !    Global parameters (in most cases volume integrated and surface averaged quanatities).
  real(DP),pointer  :: enrg(:) => null()     ! /enrg - Energy content of of a distribution species [J]; Time-dependent; Vector(ndist_spec)
  real(DP),pointer  :: enrg_para(:) => null()     ! /enrg_para - Parallel energy content of of a distribution species [J] Time-dependent; Vector(ndist_spec)
  real(DP),pointer  :: pow_coll_i(:,:) => null()     ! /pow_coll_i - Collisional power to ions [W]; Time-dependent; Matrix(ndist_spec, nion)
  real(DP),pointer  :: pow_coll_e(:) => null()     ! /pow_coll_e - Collisional power to the electrons [W]; Time-dependent; Vector(ndist_spec)
  type (type_dist_src_snk_tot) :: therm_src  ! /therm_src - Source particles and power due to particles of the distribution species being thermalised (merging into the thermal plasma).
  type (type_dist_glob_dist_losses) :: losses  ! /losses - Losses of the distribution species (orbit losses and neutralisation losses).
  real(DP),pointer  :: cur_dr_tor(:) => null()     ! /cur_dr_tor - Toroidal current of non-thermal particles (excluding electron back current for fast ions) [A]; Time-dependent; Vector(ndist_spec).
  real(DP),pointer  :: trq_i(:,:) => null()     ! /trq_i - Collisional torque to background ions [N.m]; Time dependent; Matrix (ndist_spec, nion)
  real(DP),pointer  :: trq_e(:) => null()     ! /trq_e - Collisional torque to electrons [N.m]; Time dependent; Vector(ndist_spec)
  real(DP),pointer  :: trq_j_rxb(:) => null()     ! /trq_j_rxb - Torque due to radial currents of non-thermal particles [N.m]; Time-dependent; Vector(ndist_spec).
  type (type_dist_nucl_reac_th) :: nucl_reac_th  ! /nucl_reac_th - Nuclear reactions between the calculated species and other species assumed to have thermal distributions.
  type (type_dist_nucl_reac_sf) :: nucl_reac_sf  ! /nucl_reac_sf - Nuclear reactions of the calculated species with itself (thermal + non-thermal).
endtype

type type_dist_nucl_reac  !    Information on nuclear reactions involving the calculated species.
  integer,pointer  :: nreacs(:) => null()      ! /nreacs - Number of possible nuclear reactions (with background species and for different branches); Vector(ndist_spec)
  integer,pointer  :: point_reac(:,:) => null()     ! /point_reac - Pointer to a species in composition who can undergo a nuclear reaction with the calculated species; Matrix(ndist_spec, max_nreac)
  integer,pointer  :: id_reac(:,:) => null()     ! /id_reac - Identification of the reaction between the calculated species and a background species (including which branch if applicable); Tim
endtype

type type_dist_profiles  !    Profiles (volume integrated and flux surface averaged)
  integer,pointer  :: npsi(:) => null()      ! /npsi - Number of points of the radial grid for each species. Vector(ndist_spec)
  real(DP),pointer  :: rho_tor_norm(:,:) => null()     ! /rho_tor_norm - Normalised toroidal flux coordinate at the grid points for 1D profiles; Time-dependent; matrix (ndist_spec, max_npsi)
  real(DP),pointer  :: rho_tor(:,:) => null()     ! /rho_tor - Toroidal flux coordinate at the grid points for 1D profiles [m]. Defined as sqrt(phi/pi/B0), where B0 = equilibrium/global_param/t
  real(DP),pointer  :: psi(:,:) => null()     ! /psi - Poloidal flux at the grid points for 1D profiles [Wb], without 1/2pi and such that Bp=|grad psi| /R/2/pi. Time-dependent; matrix (
  real(DP),pointer  :: enrgd_tot(:,:) => null()     ! /enrgd_tot - Flux surface averaged energy density of a distribution species [J/m^3]; Time-dependent; Matrix(ndist_spec, max_npsi)
  real(DP),pointer  :: enrgd_para(:,:) => null()     ! /enrgd_para - Flux surface averaged parallel energy density of a distribution species [J/m^3] Time-dependent; Matrix(ndist_spec, max_npsi).
  real(DP),pointer  :: powd_coll_i(:,:,:) => null()     ! /powd_coll_i - Flux surface averaged collisional power to ions [W.m^-3]; Time-dependent; Array3d(ndist_spec, nion, max_npsi)
  real(DP),pointer  :: powd_coll_e(:,:) => null()     ! /powd_coll_e - Flux surface averaged collisional power to the electrons [W.m^-3]; Time-dependent; Matrix(ndist_spec, max_npsi)
  type (type_dist_src_snk_surf) :: therm_srcd  ! /therm_srcd - Flux surface averaged source of particles and power due to particles of the distribution species being thermalised (merging into t
  type (type_dist_prof_surf_dist_losses) :: lossesd  ! /lossesd - Particle loss densities due to charge exchange events with neutrals or orbits intersecting material surfaces.
  real(DP),pointer  :: curd_fp(:,:) => null()     ! /curd_fp - Flux surface averaged toroidal current density of non-thermal (fast) particles of the distribution species (excluding electron bac
  real(DP),pointer  :: curd_dr(:) => null()     ! /curd_dr - Total toroidal driven current density (including electron back current in the presence of fast ions) [A]; Time-dependent; Matrix(n
  real(DP),pointer  :: trqd_i(:,:,:) => null()     ! /trqd_i - Flux surface averaged collisional toroidal torque to background ions [N.m^-2]; Time dependent; Array3d (ndist_spec, nion, max_npsi
  real(DP),pointer  :: trqd_e(:,:) => null()     ! /trqd_e - Flux surface averaged collisional toroidal torque density to electrons [N.m^-2]; Time dependent; Matrix(ndist_spec, max_npsi)
  real(DP),pointer  :: trqd_jrxb(:,:) => null()     ! /trqd_jrxb - Toroidal torque density due to radial currents of non-thermal particles of the distribution species [N.m^-2]; Time-dependent; Matr
  type (type_dist_prof_surf_nucl_reac_th) :: nucl_rd_th  ! /nucl_rd_th - Nuclear reaction rate densities for reactions between the cacluated species and other species assumed to have thermal distribution
  type (type_dist_prof_surf_nucl_reac_sf) :: nucl_rd_sf  ! /nucl_rd_sf - Nuclear reaction rate densities for reactions of the calculated species with itself (thermal + non-thermal).
  real(DP),pointer  :: enrg_tot(:,:) => null()     ! /enrg_tot - Energy content of of a distribution species [J] inside a flux surface; Time-dependent; Matrix(ndist_spec, max_npsi)
  real(DP),pointer  :: enrg_para(:,:) => null()     ! /enrg_para - Parallel energy content of a distribution species [J] inside a flux surface; Time-dependent; Matrix(ndist_spec, max_npsi)
  real(DP),pointer  :: pow_coll_i(:,:,:) => null()     ! /pow_coll_i - Collisional power to ions inside a flux surface [W]; Time-dependent; Array3d(ndist_spec, nion, max_npsi)
  real(DP),pointer  :: pow_coll_e(:,:) => null()     ! /pow_coll_e - Collisional power to the electrons inside a flux surface [W]; Time-dependent; Matrix(ndist_spec, max_npsi)
  type (type_dist_src_snk_vol) :: therm_src  ! /therm_src - Source particles and power inside a flux surface due to particles of the distribution species being thermalised (merging into the 
  type (type_dist_prof_vol_dist_losses) :: losses  ! /losses - Particle loss inside flux surface due to charge exchange events.
  real(DP),pointer  :: cur_fp(:,:) => null()     ! /cur_fp - Toroidal current of non-thermal (fast) particles driven inside a flux surface (does not include electron back current for fast ion
  real(DP),pointer  :: cur_dr(:,:) => null()     ! /cur_dr - Total toroidal current driven inside a flux surface (including electron back current in the presence of fast ions) [A]; Time-depen
  real(DP),pointer  :: trq_i(:,:,:) => null()     ! /trq_i - Collisional toroidal torque to background ions inside a flux surface [N.m]; Time dependent; Array3d (ndist_spec, nion, max_npsi)
  real(DP),pointer  :: trq_e(:,:) => null()     ! /trq_e - Collisional toroidal torque to electrons inside a flux surface [N.m]; Time dependent; Matrix(ndist_spec, max_npsi)
  real(DP),pointer  :: trq_j_rxb(:,:) => null()     ! /trq_j_rxb - Toroidal torque due to radial currents of non-thermal particles of the distribution species [N.m]; Time-dependent; Matrix(ndist_sp
  type (type_dist_prof_vol_nucl_reac_th) :: nucl_reac_th  ! /nucl_reac_th - Nuclear reactions inside a flux surface involving the distribution species and other species assumed to be thermal.
  type (type_dist_prof_vol_nucl_reac_sf) :: nucl_reac_sf  ! /nucl_reac_sf - Nuclear reactions inside a flux surface of the calculated species with itself (thermal + non-thermal).
endtype

type type_dist_func  !    
  integer,pointer  :: sol_type(:) => null()      ! /distribution/dist_func/sol_type - Solution type: 1 - full-f; 2 - delta-f. For the latter case delta-f is given by the test particles and the unperturbed distributio
  type (type_dist_test_part) :: test_part  ! /distribution/dist_func/test_part - Distribution given as a set of test particles.
  type (type_dist_ff) :: f0  ! /distribution/dist_func/f0 - Orbit averaged (or Bounce averaged) zero order distribution function.
  type (type_dist_ff) :: fullf  ! /distribution/dist_func/fullf - Orbit averaged (or Bounce averaged) full-f distribution function.
endtype

type type_distribution  !    
  type (type_datainfo) :: datainfo  ! /distribution/datainfo - 
  type (type_composition) :: composition  ! /distribution/composition - 
  integer,pointer  :: calc_spec(:) => null()      ! /distribution/calc_spec - Pointer to the species for which the distribution function(s) is/are calculated and whose characteristics are given in composition
  type (type_dist_nucl_reac) :: nucl_reac  ! /distribution/nucl_reac - Information on nuclear reactions involving the calculated species.
  type (type_dist_glob) :: global_param  ! /distribution/global_param - Global parameters (in most cases volume integrated and surface averaged quanatities).
  type (type_dist_profiles) :: profiles_1d  ! /distribution/profiles_1d - Profiles (volume integrated and flux surface averaged)
  type (type_dist_func) :: dist_func  ! /distribution/dist_func - Distribution functions
  type (type_dist_input_src) :: input_src  ! /distribution/input_src - Input sources of particles and power for the distribution species (to aid diagnosing the code output).
  type (type_codeparam) :: codeparam  ! /distribution/codeparam - 
  real(DP)  :: time=-9.0D40       ! /distribution/time - Time [s]; Time-dependent; Scalar
endtype

! ***********  Include distsource.xsd
type type_distsource_global_param  !    Global parameters (volume integrated).
  real(DP),pointer  :: src_pow(:) => null()     ! /src_pow - Total power source [W]; Time-dependent. Vector(nsrc_spec)
  real(DP),pointer  :: src_rate(:) => null()     ! /src_rate - Particle source rate [1/s]; Time-dependent; Vector(nsrc_spec)
endtype

type type_distsource_profiles_1d  !    
  integer,pointer  :: npsi(:) => null()      ! /npsi - Number of points of the radial grid for each species. Vector(nsrc_spec)
  real(DP),pointer  :: rho_tor_norm(:,:) => null()     ! /rho_tor_norm - Normalised toroidal flux coordinate at the grid points for 1D profiles; Time-dependent; matrix(nsrc_spec, max_npsi)
  real(DP),pointer  :: rho_tor(:,:) => null()     ! /rho_tor - Toroidal flux coordinate at the grid points for 1D profiles [m]. Defined as sqrt(phi/pi/B0), where B0 = equilibrium/global_param/t
  real(DP),pointer  :: psi(:,:) => null()     ! /psi - Poloidal flux at the grid points for 1D profiles [Wb], without 1/2pi and such that Bp=|grad psi| /R/2/pi. Time-dependent; matrix(n
  real(DP),pointer  :: pow_den(:,:) => null()     ! /pow_den - Flux surface averaged power density [W/m^3]; Time-dependent; Matrix(nsrc_spec, max_npsi)
  real(DP),pointer  :: src_rate(:,:) => null()     ! /src_rate - Flux surface averaged total source density of particles [m^-3 s^-1]; Time-dependent; Matrix(nsrc_spec, max_npsi)
endtype

type type_distsource_rect_grid  !    Details of rectangular grids.
  integer,pointer  :: ndim1(:) => null()      ! /ndim1 - Number of grid points in the first dimension in phase space; vector (nsrc_spec)
  integer,pointer  :: ndim2(:) => null()      ! /ndim2 - Number of grid points in the second dimension in phase space; vector (nsrc_spec)
  integer,pointer  :: ndim3(:) => null()      ! /ndim3 - Number of grid points in the third dimension in phase space; vector (nsrc_spec)
  integer,pointer  :: ndim4(:) => null()      ! /ndim4 - Number of grid points in the fourth dimension in phase space; vector (nsrc_spec)
  real(DP),pointer  :: dim1(:,:) => null()     ! /dim1 - Grid in the first dimension in phase space; Time-dependent; matrix(nsrc_spec, max_ndim1)
  real(DP),pointer  :: dim2(:,:) => null()     ! /dim2 - Grid in the second dimension in phase space; Time-dependent; matrix(nsrc_spec, max_ndim2)
  real(DP),pointer  :: dim3(:,:) => null()     ! /dim3 - Grid in the third dimension in phase space; Time-dependent; Matrix (nsrc_spec, max_ndim3)
  real(DP),pointer  :: dim4(:,:) => null()     ! /dim4 - Grid in the fourth dimension in phase space; Time-dependent; Matrix (nsrc_spec, max_ndim4)
  real(DP),pointer  :: jacobian(:,:,:,:,:) => null()     ! /jacobian - Jacobian of the transformation of the phase space grid variables; Time-dependent; array5d (nsrc_spec, max_ndim1, max_ndim2, max_nd
endtype

type type_src_snk_tot  !    
  real(DP),pointer  :: particles(:) => null()     ! /particles - Source/sink  particles [1/s]; Time-dependedent; Vector(nsrc_spec)
  real(DP),pointer  :: power(:) => null()     ! /power - Power associated with the source/sink of particles [W]; Time-dependent; Vector(nsrc_spec)
  real(DP),pointer  :: torque(:) => null()     ! /torque - Torque due to the source/sink of particles [Nm]; Time dependent; Vector (nsrc_spec)
endtype

type type_src_snk_fav  !    
  real(DP),pointer  :: particles(:,:) => null()     ! /particles - Source/sink  particles [s^1 m^-3]; Time-dependedent; Matrix(nsrc_spec, max_npsi)
  real(DP),pointer  :: power(:,:) => null()     ! /power - Power density associated with the source/sink of particles [W/m^3]; Time-dependent; Matrix(nsrc_spec, max_npsi)
  real(DP),pointer  :: torque(:,:) => null()     ! /torque - Torque density due to the source/sink of particles [Nm/m^3]; Time dependent; Matrix (nsrc_spec, max_npsi)
endtype

type type_src_snk_int  !    
  real(DP),pointer  :: particles(:,:) => null()     ! /particles - Source/sink  particles [s^1 m^-3]; Time-dependedent; Vector(nsrc_spec, max_npsi)
  real(DP),pointer  :: power(:,:) => null()     ! /power - Power associated with the source/sink of particles [MW/m^3]; Time-dependent; Vector(nsrc_spec, max_npsi)
  real(DP),pointer  :: torque(:,:) => null()     ! /torque - Torque due to the source/sink of particles [Nm/m^3]; Time dependent; Vector (nsrc_spec, max_npsi)
endtype

type type_source_4d  !    
  integer,pointer  :: gyrosrc_type(:) => null()      ! /distsource/source_4d/gyrosrc_type - Defines how  to interpret the source: 1 = the source is calulated at the particle birth point;  2 = the source is calulated at the
  integer,pointer  :: grid_type(:) => null()      ! /distsource/source_4d/grid_type - Defines the four grid variables and the grid structure (rectangular, unstructured...): 1 = { R(c), z(c), ksi(c), E(d), rectangular
  type (type_distsource_rect_grid) :: rect_grid  ! /distsource/source_4d/rect_grid - Details of rectangular grids.
  real(DP),pointer  :: source(:,:,:,:,:) => null()     ! /distsource/source_4d/source - Phase space source of particles; the units depend on the grid_type:  [m^-3 s^-1] if the grid is discrete in energy/velocity and [(
endtype

type type_source_tp  !    
  integer,pointer  :: n_particles(:) => null()      ! /distsource/source_tp/n_particles - Number of test particle for each species; Time-dependent; Vector (nsrc_spec)
  integer  :: var_type=-999999999       ! /distsource/source_tp/var_type - Identification of variables: 1 = { R, z, phi, v, ksi, R*v_phi } ; 2 = { R, z, phi, Energy, ksi, R*v_phi } ; 3 = { Energy, magnetic
  real(DP),pointer  :: var1(:,:) => null()     ! /distsource/source_tp/var1 - Phase space variable number one characterising a test particle; Time-dependent; Matrix(nsrc_spec, max_n_particles)
  real(DP),pointer  :: var2(:,:) => null()     ! /distsource/source_tp/var2 - Phase space variable number two characterising a test particle; Time-dependent; Matrix(nsrc_spec, max_n_particles)
  real(DP),pointer  :: var3(:,:) => null()     ! /distsource/source_tp/var3 - Phase space variable number three characterising a test particle; Time-dependent; Matrix (nsrc_spec, max_n_particles)
  real(DP),pointer  :: var4(:,:) => null()     ! /distsource/source_tp/var4 - Phase space variable number four characterising a test particle; Time-dependent; Matrix(nsrc_spec, max_n_particles)
  real(DP),pointer  :: var5(:,:) => null()     ! /distsource/source_tp/var5 - Phase space variable number five characterising a test particle; Time-dependent; Matrix(nsrc_spec, max_n_particles)
  real(DP),pointer  :: var6(:,:) => null()     ! /distsource/source_tp/var6 - Phase space variable number six characterising a test particle; Time-dependent; Matrix(nsrc_spec, max_n_particles)
  real(DP),pointer  :: weight(:,:) => null()     ! /distsource/source_tp/weight - Weight of test particle; Time-dependent; Matrix(nsrc_spec, max_n_particles)
endtype

type type_distsource  !    
  type (type_datainfo) :: datainfo  ! /distsource/datainfo - 
  type (type_composition) :: composition  ! /distsource/composition - Plasma composition.
  integer,pointer  :: src_spec(:) => null()      ! /distsource/src_spec - Pointer to the source species whose characteristics are given in composition. Vector(nsrc_spec)
  type (type_distsource_global_param) :: global_param  ! /distsource/global_param - Global parameters (volume integrated).
  type (type_distsource_profiles_1d) :: profiles_1d  ! /distsource/profiles_1d - 1D radial profiles
  type (type_source_4d) :: source_4d  ! /distsource/source_4d - Source of particles in phase space.
  type (type_source_tp) :: source_tp  ! /distsource/source_tp - Source given as a set of test particles. Note that the test particles are given at the source location and not at the gyrocentre. 
  type (type_codeparam) :: codeparam  ! /distsource/codeparam - Code parameters
  real(DP)  :: time=-9.0D40       ! /distsource/time - Time [s]; Time-dependent; scalar
endtype

! ***********  Include ecediag.xsd
type type_ecesetup  !    diagnostic setup information
  real(DP),pointer  :: frequency(:) => null()     ! /frequency - Frequency of the ECE channels. Vector (nchannels)
  type (type_rzphi1Dexp) :: position  ! /position - Position of the measurement. Time-dependent. Vector (nchannels)
endtype

type type_ecemeasure  !    Measured values
  type (type_exp1D) :: te  ! /te - Electron temperature [eV]. Vector (nchannels)
endtype

type type_ecediag  !    
  type (type_datainfo) :: datainfo  ! /ecediag/datainfo - 
  type (type_ecesetup) :: setup  ! /ecediag/setup - diagnostic setup information
  type (type_ecemeasure) :: measure  ! /ecediag/measure - Measured values
  real(DP)  :: time=-9.0D40       ! /ecediag/time - Time [s]; Time-dependent; Scalar
endtype

! ***********  Include edge.xsd
type type_bezier  !    
  real(DP),pointer  :: u(:,:) => null()     ! /properties/bezier/u - First Bezier vector components. Matrix(nnode,2)
  real(DP),pointer  :: v(:,:) => null()     ! /properties/bezier/v - Second Bezier vector components. Matrix(nnode,2)
  real(DP),pointer  :: w(:,:) => null()     ! /properties/bezier/w - Third Bezier vector components. Matrix(nnode,2)
endtype

type type_alter_coord  !    
  integer,pointer  :: type_coord(:) => null()      ! /alter_coord/type_coord - Type of coordinates describing the space. Vector of integers (ncoord)
  real(DP),pointer  :: node_value(:,:) => null()     ! /alter_coord/node_value - Numerical value of the node coordinates. Matrix (nnode,ncoord)
endtype

type type_properties  !    
  integer,pointer  :: alias(:) => null()      ! /properties/alias - Describes the links among grid nodes, primarily in case of periodic grids. If nodes i and j are two instances of the same node, lo
  integer,pointer  :: type(:) => null()      ! /properties/type - General purpose signal allowing the user grouping the space nodes according to his/her needs. Vector of integers (nnode).
  integer,pointer  :: is_x(:) => null()      ! /properties/is_x - Location of X points. Vector of integers (nnode).
  character(len=132), dimension(:), pointer ::node_connect => null()       ! /properties/node_connect - Lconnection type between two nodes. If its value is STRAIGHT, then two nodes are connected with a straight line (where "straight" 
  type (type_bezier) :: bezier  ! /properties/bezier - Components of the Bezier vectors associated to a node. I WONDER IF THIS IS GENERAL ENOUGH ... WHAT DO WE DO IF A DIFFERENT TYPE OF
endtype

type type_grid_transp_coef_el  !    Transport coefficients for electron quantities
  integer,pointer  :: gridlink(:) => null()      ! /gridlink - For each space, dimension of the objects that is used to form the grid for this physical quantity. Vector of integers (nspace).
  integer,pointer  :: pointer(:,:) => null()     ! /pointer - For each value, points to the object index in each space. Matrix of integers (nspace, nvalue)
  real(DP),pointer  :: diff_dia(:) => null()     ! /diff_dia - Diamagnetic diffusivity [m^2/s]. Time-dependent. Vector(nvalue). 
  real(DP),pointer  :: diff_rad(:) => null()     ! /diff_rad - Radial diffusivity [m^2/s]. Time-dependent. Vector(nvalue).
endtype

type type_grid_transp_coef_ion  !    Transport coefficients for ion quantities
  integer,pointer  :: gridlink(:) => null()      ! /gridlink - For each space, dimension of the objects that is used to form the grid for this physical quantity. Vector of integers (nspace).
  integer,pointer  :: pointer(:,:) => null()     ! /pointer - For each value, points to the object index in each space. Matrix of integers (nspace, nvalue)
  real(DP),pointer  :: diff_dia(:,:) => null()     ! /diff_dia - Diamagnetic diffusivity [m^2/s]. Time-dependent. Matrix(nvalue, nimp).
  real(DP),pointer  :: diff_rad(:,:) => null()     ! /diff_rad - Radial diffusivity [m^2/s]. Time-dependent. Matrix(nvalue, nimp).
endtype

type type_grid_fluxes_heat_el  !    Fluxes
  integer,pointer  :: gridlink(:) => null()      ! /gridlink - For each space, dimension of the objects that is used to form the grid for this physical quantity. Vector of integers (nspace).
  integer,pointer  :: pointer(:,:) => null()     ! /pointer - For each value, points to the object index in each space. Matrix of integers (nspace, nvalue)
  real(DP),pointer  :: heat_par(:) => null()     ! /heat_par - Parallel heat flux. Time-dependent. Vector(nvalue)
  real(DP),pointer  :: heat_dia(:) => null()     ! /heat_dia - Diamagnetic heat flux. Time-dependent. Vector (nvalue)
  real(DP),pointer  :: heat_rad(:) => null()     ! /heat_rad - Radial heat flux. Time-dependent. Vector(nvalue)
endtype

type type_grid_fluxes_heat_ion  !    Fluxes, heat and energy, ion
  integer,pointer  :: gridlink(:) => null()      ! /gridlink - For each space, dimension of the objects that is used to form the grid for this physical quantity. Vector of integers (nspace).
  integer,pointer  :: pointer(:,:) => null()     ! /pointer - For each value, points to the object index in each space. Matrix of integers (nspace, nvalue)
  real(DP),pointer  :: heat_par(:,:) => null()     ! /heat_par - Parallel heat flux. Matrix(nvalue,nimp)
  real(DP),pointer  :: heat_dia(:,:) => null()     ! /heat_dia - Diamagnetic heat flux. Matrix(nvalue,nimp)
  real(DP),pointer  :: heat_rad(:,:) => null()     ! /heat_rad - Radial heat flux. Matrix(nvalue,nimp)
endtype

type type_grid_fluxes_part_el  !    Fluxes
  integer,pointer  :: gridlink(:) => null()      ! /gridlink - For each space, dimension of the objects that is used to form the grid for this physical quantity. Vector of integers (nspace).
  integer,pointer  :: pointer(:,:) => null()     ! /pointer - For each value, points to the object index in each space. Matrix of integers (nspace, nvalue)
  real(DP),pointer  :: flux_par(:) => null()     ! /flux_par - Parallel flux. Time-dependent. Vector(nvalue)
  real(DP),pointer  :: flux_dia(:) => null()     ! /flux_dia - Diamagnetic flux. Time-dependent. Vector (nvalue)
  real(DP),pointer  :: flux_rad(:) => null()     ! /flux_rad - Radial flux. Time-dependent. Vector(nvalue)
endtype

type type_grid_fluxes_part_ion  !    Fluxes, heat and energy, ion
  integer,pointer  :: gridlink(:) => null()      ! /gridlink - For each space, dimension of the objects that is used to form the grid for this physical quantity. Vector of integers (nspace).
  integer,pointer  :: pointer(:,:) => null()     ! /pointer - For each value, points to the object index in each space. Matrix of integers (nspace, nvalue)
  real(DP),pointer  :: flux_par(:,:) => null()     ! /flux_par - Parallel flux. Time-dependent. Matrix(nvalue,nimp)
  real(DP),pointer  :: flux_dia(:,:) => null()     ! /flux_dia - Diamagnetic flux. Time-dependent. Matrix(nvalue,nimp)
  real(DP),pointer  :: flux_rad(:,:) => null()     ! /flux_rad - Radial flux. Time-dependent. Matrix(nvalue,nimp)
endtype

type type_grid_field_el  !    Physical field defined on a complex grid, for electron quantities 
  integer,pointer  :: gridlink(:) => null()      ! /gridlink - For each space, dimension of the objects that is used to form the grid for this physical quantity. Vector of integers (nspace).
  integer,pointer  :: pointer(:,:) => null()     ! /pointer - For each value, points to the object index in each space. Matrix of integers (nspace, nvalue)
  real(DP),pointer  :: value(:) => null()     ! /value - Value of the physical quantity given on each super-object (tensorial product of all objects in spaces defined by gridlink). Vector
endtype

type type_grid_field_ion  !    Physical field defined on a complex grid, for ion quantities 
  integer,pointer  :: gridlink(:) => null()      ! /gridlink - For each space, dimension of the objects that is used to form the grid for this physical quantity. Vector of integers (nspace).
  integer,pointer  :: pointer(:,:) => null()     ! /pointer - For each value, points to the object index in each space. Matrix of integers (nspace, nvalue)
  real(DP),pointer  :: value(:,:) => null()     ! /value - Value of the physical quantity given on each super-object (tensorial product of all objects in spaces defined by gridlink), for ea
endtype

type type_grid_te  !    Electron temperature [eV], heat fluxes
  type (type_grid_field_el) :: main_field  ! /main_field - Main physical quantity
  type (type_grid_fluxes_heat_el) :: fluxes  ! /fluxes - Fluxes
  type (type_grid_transp_coef_el) :: transp_coef  ! /transp_coef - Transport coefficients
endtype

type type_grid_ti  !    Ion temperature [eV], heat fluxes
  type (type_grid_field_ion) :: main_field  ! /main_field - Main physical quantity
  type (type_grid_fluxes_heat_ion) :: fluxes  ! /fluxes - Fluxes
  type (type_grid_transp_coef_ion) :: transp_coef  ! /transp_coef - Transport coefficients
endtype

type type_grid_ni  !    Ion density
  type (type_grid_field_ion) :: main_field  ! /main_field - Main physical quantity
  type (type_grid_fluxes_part_ion) :: fluxes  ! /fluxes - Fluxes
  type (type_grid_transp_coef_ion) :: transp_coef  ! /transp_coef - Transport coefficients
endtype

type type_grid_ne  !    Electron density
  type (type_grid_field_el) :: main_field  ! /main_field - Main physical quantity
  type (type_grid_fluxes_part_el) :: fluxes  ! /fluxes - Fluxes
  type (type_grid_transp_coef_el) :: transp_coef  ! /transp_coef - Transport coefficients
endtype

type type_grid_space  !    Description of a space in the grid
  integer,pointer  :: type_coord(:) => null()      ! /type_coord - Type of coordinates describing the space. Vector of integers (ncoord)
  real(DP),pointer  :: node_value(:,:) => null()     ! /node_value - Numerical value of the node coordinates. Matrix (nnode,ncoord)
  type (type_alter_coord) :: alter_coord  ! /alter_coord - Alternative coordinate system possibly used to describe the space (e.g. rho_tor versus rho_tor_norm). NB : when specifying straigh
  integer,pointer  :: nobject(:) => null()      ! /nobject - Number of object defined in the space, for each dimension. Vector of integers (ncoord)
  integer,pointer  :: nobject_bou(:) => null()      ! /nobject_bou - Maximum number of boundaries ("faces") of an object, for each dimension. Vector of integers (ncoord)
  integer,pointer  :: neighborside(:) => null()      ! /neighborside - Maximum number of neighbors lying on a "face" of objects for each dimension. Vector of integers (ncoord)
  integer,pointer  :: objdef(:,:,:) => null()     ! /objdef - Object definition for each dimensionality (last index). Each object is defined recursively by listing its boundaries, which are ob
  integer,pointer  :: neighbors(:,:,:) => null()     ! /neighbors - Neighbors of a given object, specified only for the highest dimensionality. Unused slots of the matrix should be set as UNDEFINED.
  type (type_properties) :: properties  ! /properties - Space properties
endtype

type type_grid_fluid  !    Fluid quantities
  type (type_grid_ne) :: ne  ! /ne - Electron density [m^-3]
  type (type_grid_te) :: te  ! /te - Electron temperature [eV]
  type (type_grid_te) :: te_perp  ! /te_perp - Electron perpendicular temperature [eV]
  type (type_grid_ne) :: ve_dia  ! /ve_dia - Electron diamagnetic velocity [m/s]
  type (type_grid_ne) :: ve_par  ! /ve_par - Electron parallel velocity [m/s]
  type (type_grid_ne) :: ve_rad  ! /ve_rad - Electron radial velocity [m/s]
  type (type_grid_ni) :: ni  ! /ni - Ion density [m^-3]
  type (type_grid_ti) :: ti  ! /ti - Ion temperature [eV]
  type (type_grid_ti) :: ti_perp  ! /ti_perp - Ion perpendicular temperature [eV]
  type (type_grid_ni) :: vi_dia  ! /vi_dia - Ion diamagnetic velocity [m/s]
  type (type_grid_ni) :: vi_par  ! /vi_par - Ion parallel velocity [m/s]
  type (type_grid_ni) :: vi_rad  ! /vi_rad - Ion radial velocity [m/s]
  type (type_grid_field_el) :: potential  ! /potential - Electric potential [V]
endtype

type type_grid_full  !    Generic definition of a complex grid
  type (type_grid_space),pointer :: spaces(:) => null()  ! /spaces(i) - Definition of the grid spaces. Structure array(nspace).
  real(DP),pointer  :: metric(:) => null()     ! /metric - Grid metric. INSERT HERE CLARIFIED DEFINITION Vector. DIMENSIONALITY ?
endtype

type type_edge  !    
  type (type_datainfo) :: datainfo  ! /edge/datainfo - 
  type (type_grid_full) :: grid  ! /edge/grid - Grid definition
  type (type_desc_impur) :: desc_impur  ! /edge/desc_impur - 
  type (type_grid_fluid) :: fluid  ! /edge/fluid - Fluid quantities
  real(DP)  :: time=-9.0D40       ! /edge/time - Time [s]; Time-dependent; Scalar.
  type (type_codeparam) :: codeparam  ! /edge/codeparam - 
endtype

! ***********  Include eqcoord_sys.xsd
type type_coord_sys  !    
  character(len=132), dimension(:), pointer ::grid_type => null()       ! /coord_sys/grid_type - Type of coordinate system
  type (type_reggrid) :: grid  ! /coord_sys/grid - Regular grid definition; Time-dependent
  real(DP),pointer  :: jacobian(:,:) => null()     ! /coord_sys/jacobian - Jacobian of the coordinate system; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: g_11(:,:) => null()     ! /coord_sys/g_11 - metric coefficients g_11; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: g_12(:,:) => null()     ! /coord_sys/g_12 - metric coefficients g_12; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: g_13(:,:) => null()     ! /coord_sys/g_13 - metric coefficients g_13; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: g_22(:,:) => null()     ! /coord_sys/g_22 - metric coefficients g_22; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: g_23(:,:) => null()     ! /coord_sys/g_23 - metric coefficients g_23; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: g_33(:,:) => null()     ! /coord_sys/g_33 - metric coefficients g_33; Time-dependent; Matrix (ndim1, ndim2)
  type (type_rz2D) :: position  ! /coord_sys/position - R and Z position of grid points; Time-dependent; Matrix (ndim1, ndim2)
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
  type (type_rz1D) :: position  ! /eqconstraint/isoflux/position - Position of the points at which the flux is considered the same; Time-dependent; Vector (nmeas)
  character(len=132), dimension(:), pointer ::source => null()       ! /eqconstraint/isoflux/source - Description or path to the source signal (diagnostic or genprof, from which to read all info on the signal), e.g. 'magdiag/bpol_pr
  real(DP),pointer  :: weight(:) => null()     ! /eqconstraint/isoflux/weight - weight given to the measurement (>= 0); -1 if exact data; Time-dependent; Vector (nmeas)
  real(DP),pointer  :: sigma(:) => null()     ! /eqconstraint/isoflux/sigma - standard deviation of the measurement; Time-dependent; Vector (nmeas)
  real(DP),pointer  :: calculated(:) => null()     ! /eqconstraint/isoflux/calculated - Signal as recalculated by the equilibrium code; Time-dependent; Vector (nmeas)
  real(DP),pointer  :: chi2(:) => null()     ! /eqconstraint/isoflux/chi2 - chi^2 of (calculated-measured); Time-dependent; Vector (nmeas)
endtype

type type_magnet_iron  !    
  type (type_eqmes1D) :: mr  ! /eqconstraint/magnet_iron/mr - Magnetisation along the R axis [T];
  type (type_eqmes1D) :: mz  ! /eqconstraint/magnet_iron/mz - Magnetisation along the Z axis [T];
endtype

type type_q  !    
  real(DP),pointer  :: qvalue(:) => null()     ! /eqconstraint/q/qvalue - Safety factor values; Time-dependent; Vector (nmeas)
  type (type_rz1D) :: position  ! /eqconstraint/q/position - Major radius of the given safety factor values [m]; Time-dependent; Vector (nmeas)
  character(len=132), dimension(:), pointer ::source => null()       ! /eqconstraint/q/source - Description or path to the source signal (diagnostic or genprof, from which to read all info on the signal), e.g. 'magdiag/bpol_pr
  integer  :: exact=-999999999       ! /eqconstraint/q/exact - 1 means exact data, is not fitted; 0 means the equilibrium code does a least square fit; scalar integer
  real(DP),pointer  :: weight(:) => null()     ! /eqconstraint/q/weight - weight given to the measurement (>= 0); Time-dependent; Vector (nmeas)
  real(DP),pointer  :: sigma(:) => null()     ! /eqconstraint/q/sigma - standard deviation of the measurement; Time-dependent; Vector (nmeas)
  real(DP),pointer  :: calculated(:) => null()     ! /eqconstraint/q/calculated - Signal as recalculated by the equilibrium code; Time-dependent; Vector (nmeas)
  real(DP),pointer  :: chi2(:) => null()     ! /eqconstraint/q/chi2 - chi^2 of (calculated-measured); Time-dependent; Vector (nmeas)
endtype

type type_xpts  !    
  type (type_rz1D) :: position  ! /eqconstraint/xpts/position - Position of the X-point(s); Time-dependent; Vector (nmeas)
  character(len=132), dimension(:), pointer ::source => null()       ! /eqconstraint/xpts/source - Description or path to the source signal (diagnostic or genprof, from which to read all info on the signal), e.g. 'magdiag/bpol_pr
  real(DP),pointer  :: weight(:) => null()     ! /eqconstraint/xpts/weight - weight given to the measurement (>= 0); -1 if exact data; Time-dependent; Vector (nmeas)
  real(DP),pointer  :: sigma(:) => null()     ! /eqconstraint/xpts/sigma - standard deviation of the measurement; Time-dependent; Vector (nmeas)
  real(DP),pointer  :: calculated(:) => null()     ! /eqconstraint/xpts/calculated - Signal as recalculated by the equilibrium code; Time-dependent; Vector (nmeas)
  real(DP),pointer  :: chi2(:) => null()     ! /eqconstraint/xpts/chi2 - chi^2 of (calculated-measured); Time-dependent; Vector (nmeas)
endtype

type type_eqconstraint  !    
  type (type_eqmes1D) :: bpol  ! /eqconstraint/bpol - poloidal pickup coils [T]
  type (type_eqmes0D) :: bvac_r  ! /eqconstraint/bvac_r - Vacuum field times radius in the toroidal field magnet [T.m];
  type (type_eqmes1D) :: faraday  ! /eqconstraint/faraday - Faraday rotation angles [rad]
  type (type_eqmes1D) :: flux  ! /eqconstraint/flux - Poloidal flux loops [Wb]
  type (type_eqmes0D) :: i_plasma  ! /eqconstraint/i_plasma - Plasma current [A];
  type (type_isoflux) :: isoflux  ! /eqconstraint/isoflux - Point series at which the flux is considered the same
  type (type_eqmes1D) :: jsurf  ! /eqconstraint/jsurf - Average of current density on the flux surface [A/m^2]
  type (type_magnet_iron) :: magnet_iron  ! /eqconstraint/magnet_iron - Magnetisation in iron segments [T]
  type (type_eqmes1D) :: mse  ! /eqconstraint/mse - MSE angles [rad]
  type (type_eqmes1D) :: ne  ! /eqconstraint/ne - Electron density [m^-3 for local measurement, m^-2 if line integrated]
  type (type_eqmes1D) :: pfcurrent  ! /eqconstraint/pfcurrent - Current in poloidal field coils [A]
  type (type_eqmes1D) :: pressure  ! /eqconstraint/pressure - Total pressure [Pa]
  type (type_q) :: q  ! /eqconstraint/q - Safety factor
  type (type_xpts) :: xpts  ! /eqconstraint/xpts - Position of the X-point(s)
endtype

! ***********  Include flush.xsd
type type_flush  !    
  type (type_datainfo) :: datainfo  ! /flush/datainfo - 
  type (type_rz1D) :: position  ! /flush/position - Major radius and altitude of the FLUSH grid [m]; Time-dependent; Vectors resp. (nR) and (nZ)
  real(DP),pointer  :: coef(:,:) => null()     ! /flush/coef - Coefficients of the fit; Time-dependent; Matrix 2D (nR,nZ)
  type (type_codeparam) :: codeparam  ! /flush/codeparam - 
endtype

! ***********  Include eqgeometry.xsd
type type_eqgeometry  !    
  character(len=132), dimension(:), pointer ::source => null()       ! /eqgeometry/source - Comment describing the origin of the eqgeometry data; String
  integer  :: boundarytype=-999999999       ! /eqgeometry/boundarytype - 0 (limiter) or 1 (separatrix); Integer; Time-dependent
  type (type_rz1D_npoints) :: boundary  ! /eqgeometry/boundary - RZ description of the plasma boundary; Time-dependent;
  type (type_rz0D) :: geom_axis  ! /eqgeometry/geom_axis - position of the geometric axis [m]; Time-dependent; Scalar
  real(DP)  :: a_minor=-9.0D40       ! /eqgeometry/a_minor - Minor radius of the plasma boundary [m]; Time-dependent; Scalar
  real(DP)  :: elongation=-9.0D40       ! /eqgeometry/elongation - Elongation of the plasma boundary; Time-dependent; Scalar
  real(DP)  :: tria_upper=-9.0D40       ! /eqgeometry/tria_upper - Upper triangularity of the plasma boundary; Time-dependent; Scalar
  real(DP)  :: tria_lower=-9.0D40       ! /eqgeometry/tria_lower - Lower triangularity of the plasma boundary; Time-dependent; Scalar
  type (type_rz1D) :: xpts  ! /eqgeometry/xpts - Position of the Xpoints, first is the active xpoint if diverted [m]; Time-dependent; Vector (npoint)
  type (type_rz0D) :: left_low_st  ! /eqgeometry/left_low_st - Position of the lower left strike point [m]; Time-dependent; Scalar
  type (type_rz0D) :: right_low_st  ! /eqgeometry/right_low_st - Position of the lower right strike point [m]; Time-dependent; Scalar
  type (type_rz0D) :: left_up_st  ! /eqgeometry/left_up_st - Position of the upper left strike point [m]; Time-dependent; Scalar
  type (type_rz0D) :: right_up_st  ! /eqgeometry/right_up_st - Position of the upper right strike point [m]; Time-dependent; Scalar
  type (type_rz0D) :: active_limit  ! /eqgeometry/active_limit - Position of the active limiter point (point of the plasma boundary in contact with the limiter) [m]; Set R = 0 for X-point plasma;
endtype

! ***********  Include eqglobal.xsd
type type_mag_axis  !    
  type (type_rz0D) :: position  ! /global_param/mag_axis/position - Position of the magnetic axis [m]; Time-dependent; Scalar; 
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
  type (type_b0r0) :: toroid_field  ! /global_param/toroid_field - Characteristics of the vacuum toroidal field, redundant with the toroidfield CPO, to be used by the ETS
  real(DP)  :: w_mhd=-9.0D40       ! /global_param/w_mhd - Plasma energy content = 3/2 * int(p,dV) with p being the total pressure (thermal + fast particles) [J]. Time-dependent; Scalar
  real(DP)  :: gamma=-9.0D40       ! /global_param/gamma - Adiabatic index. Time-dependent; Scalar
endtype

! ***********  Include eqprofiles2d.xsd
type type_grid  !    
  real(DP),pointer  :: dim1(:) => null()     ! /profiles_2d/grid/dim1 - First dimension values; Time-dependent; Vector (ndim1) 
  real(DP),pointer  :: dim2(:) => null()     ! /profiles_2d/grid/dim2 - Second dimension values; Time-dependent; Vector (ndim2) 
  integer,pointer  :: connect(:,:) => null()     ! /profiles_2d/grid/connect - In case of a finite elemnt representation, lists the points (3 for triangles, 4 for quadrangles) which define a finite element. In
endtype

type type_profiles_2d  !    
  character(len=132), dimension(:), pointer ::grid_type => null()       ! /profiles_2d/grid_type - Selection of one of a set of grid types. 1-rectangular (R,Z) grid, in this case the position arrays should not be filled since the
  type (type_grid) :: grid  ! /profiles_2d/grid - definition of the 2D grid
  real(DP),pointer  :: r(:,:) => null()     ! /profiles_2d/r - values of the major radius on the grid [m]; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: z(:,:) => null()     ! /profiles_2d/z - values of the altitude on the grid [m]; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: psi(:,:) => null()     ! /profiles_2d/psi - values of the poloidal flux at the grid in the poloidal plane [Wb]; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: theta(:,:) => null()     ! /profiles_2d/theta - values of the poloidal angle on the grid [rad]; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: jphi(:,:) => null()     ! /profiles_2d/jphi - toroidal plasma current density [A m-2]; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: jpar(:,:) => null()     ! /profiles_2d/jpar - parallel (to magnetic field) plasma current density [A m-2]; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: br(:,:) => null()     ! /profiles_2d/br - R component of the poloidal magnetic field at the specified grid [T]; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: bz(:,:) => null()     ! /profiles_2d/bz - Z component of the poloidal magnetic field at the specified grid [T]; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: bphi(:,:) => null()     ! /profiles_2d/bphi - toroidal component of the magnetic field at the specified grid [T]; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: vphi(:,:) => null()     ! /profiles_2d/vphi - toroidal flow velocity [m/s]; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: vtheta(:,:) => null()     ! /profiles_2d/vtheta - Poloidal flow velocity [m/s]; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: rho_mass(:,:) => null()     ! /profiles_2d/rho_mass - Mass density [kg/m^3]; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: pressure(:,:) => null()     ! /profiles_2d/pressure - Pressure [Pa]; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: temperature(:,:) => null()     ! /profiles_2d/temperature - Temperature [eV]; Time-dependent; Matrix (ndim1, ndim2)
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
  real(DP),pointer  :: dpsidrho_tor(:) => null()     ! /profiles_1d/dpsidrho_tor - dpsi/drho_tor [Wb/m]; Time-dependent; Vector (npsi)
  real(DP),pointer  :: rho_vol(:) => null()     ! /profiles_1d/rho_vol - Normalised radial coordinate related to the plasma volume. Defined as sqrt(volume / volume[LCFS]). Time-dependent; Vector (npsi)
  real(DP),pointer  :: beta_pol(:) => null()     ! /profiles_1d/beta_pol - poloidal beta (inside the magnetic surface); Time-dependent; Vector (npsi)
  real(DP),pointer  :: li(:) => null()     ! /profiles_1d/li - internal inductance (inside the magnetic surface); Time-dependent; Vector (npsi)
  real(DP),pointer  :: elongation(:) => null()     ! /profiles_1d/elongation - Elongation; Time-dependent; Vector (npsi)
  real(DP),pointer  :: tria_upper(:) => null()     ! /profiles_1d/tria_upper - Upper triangularity profile; Time-dependent; Vector (npsi)
  real(DP),pointer  :: tria_lower(:) => null()     ! /profiles_1d/tria_lower - Lower triangularity profile; Time-dependent; Vector (npsi)
  real(DP),pointer  :: volume(:) => null()     ! /profiles_1d/volume - Volume enclosed in the flux surface [m^3]; Time-dependent; Vector (npsi)
  real(DP),pointer  :: vprime(:) => null()     ! /profiles_1d/vprime - Radial derivative of the volume enclosed in the flux surface with respect to psi, i.e. dV/dpsi [m^3/Wb]; Time-dependent; Vector (n
  real(DP),pointer  :: area(:) => null()     ! /profiles_1d/area - Cross-sectional area of the flux surface [m^2]; Time-dependent; Vector (npsi)
  real(DP),pointer  :: aprime(:) => null()     ! /profiles_1d/aprime - Radial derivative of the cross-sectional area of the flux surface with respect to psi, i.e. darea/dpsi [m^2/Wb]; Time-dependent; V
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
  real(DP),pointer  :: b_av(:) => null()     ! /profiles_1d/b_av - average(B); Time-dependent; Vector (npsi)
  real(DP),pointer  :: b_min(:) => null()     ! /profiles_1d/b_min - minimum(B) on the flux surface; Time-dependent; Vector (npsi)
  real(DP),pointer  :: b_max(:) => null()     ! /profiles_1d/b_max - maximum(B) on the flux surface; Time-dependent; Vector (npsi)
  real(DP),pointer  :: omega(:) => null()     ! /profiles_1d/omega - Toroidal rotation angular frequency (assumed constant on the flux surface)  [rad/s]; Time-dependent; Vector (npsi)
  real(DP),pointer  :: omegaprime(:) => null()     ! /profiles_1d/omegaprime - Psi derivative of the toroidal rotation angular frequency (assumed constant on the flux surface)  [rad/(s.Wb)]; Time-dependent; Ve
  real(DP),pointer  :: mach_a(:) => null()     ! /profiles_1d/mach_a - Alfvenic Mach number; Time-dependent; Vector (npsi)
  real(DP),pointer  :: phi_flow(:) => null()     ! /profiles_1d/phi_flow - Definition to be provided; Time-dependent; Vector (npsi)
  real(DP),pointer  :: s_flow(:) => null()     ! /profiles_1d/s_flow - Definition to be provided; Time-dependent; Vector (npsi)
  real(DP),pointer  :: h_flow(:) => null()     ! /profiles_1d/h_flow - Definition to be provided; Time-dependent; Vector (npsi)
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
  type (type_rz2D) :: rzcoordinate  ! /ironmodel/desc_iron/geom_iron/rzcoordinate - Irregular outline [m]; 2D arrays (nsegment,max_npoints)
endtype

type type_desc_iron  !    
  character(len=132), dimension(:), pointer ::name => null()       ! /ironmodel/desc_iron/name - Name of circuit. Array of strings (ncircuit).
  character(len=132), dimension(:), pointer ::id => null()       ! /ironmodel/desc_iron/id - ID of circuit.  Array of strings (ncircuit).
  type (type_permeability) :: permeability  ! /ironmodel/desc_iron/permeability - Permeability model (can be different for each iron segment)
  type (type_geom_iron) :: geom_iron  ! /ironmodel/desc_iron/geom_iron - Geometry of the iron segments
endtype

type type_magnetise  !    
  type (type_exp1D) :: mr  ! /ironmodel/magnetise/mr - Magnetisation along the R axis [T]; Time-dependent; Vector (nsegment)
  type (type_exp1D) :: mz  ! /ironmodel/magnetise/mz - Magnetisation along the Z axis [T]; Time-dependent; Vector (nsegment)
endtype

type type_ironmodel  !    
  type (type_datainfo) :: datainfo  ! /ironmodel/datainfo - 
  type (type_desc_iron) :: desc_iron  ! /ironmodel/desc_iron - Description of the iron segments
  type (type_magnetise) :: magnetise  ! /ironmodel/magnetise - Magnetisation M of the iron segment, assumed to be constant inside a given iron segment. Reminder : H = 1/mu0 * B - mur * M; [A/m]
  real(DP)  :: time=-9.0D40       ! /ironmodel/time - Time [s]; Time-dependent; Scalar
endtype

! ***********  Include launchs.xsd
type type_launchs_parallel  !    Power spectrum as a function of the parallel refractive index.
  integer,pointer  :: nn_par(:) => null()      ! /nn_par - Number of points for the discretization of the spectrum in the poloidal direction, Vector of integers (nantenna).
  real(DP),pointer  :: n_par(:,:) => null()     ! /n_par - Refraction index in the parallel direction, Matrix (nantenna,max_nn_par).
  real(DP),pointer  :: power(:) => null()     ! /power - W/dN_par [W], Matrix(nantenna, max_nn_par). Time-dependent
endtype

type type_launchs_phi_theta  !    Power spectrum as a function of the refractive index in the toroidal and poloidal directions.
  integer,pointer  :: nn_phi(:) => null()      ! /nn_phi - Number of points for the discretization of the spectrum in the toroidal direction, Vector of integers (nantenna).
  integer,pointer  :: nn_theta(:) => null()      ! /nn_theta - Number of points for the discretization of the spectrum in the poloidal direction, Vector of integers (nantenna).
  real(DP),pointer  :: n_phi(:,:) => null()     ! /n_phi - Refraction index in the toroidal direction, Matrix (nantenna,max_nn_phi).
  real(DP),pointer  :: n_theta(:,:) => null()     ! /n_theta - Refraction index in poloidal direction, Matrix (nantenna,max_nn_theta).
  real(DP),pointer  :: power(:,:,:) => null()     ! /power - W/dNphi/dNtheta [W], Array (nantenna, max_nn_phi, max_nn_theta). Time-dependent
endtype

type type_spectrum  !    
  type (type_launchs_phi_theta) :: phi_theta  ! /launchs/spectrum/phi_theta - Power spectrum as a function of the refractive index in the toroidal and poloidal directions.
  type (type_launchs_parallel) :: parallel  ! /launchs/spectrum/parallel - Power spectrum as a function of the parallel refractive index.
endtype

type type_launchs  !    
  type (type_datainfo) :: datainfo  ! /launchs/datainfo - 
  character(len=132), dimension(:), pointer ::name => null()       ! /launchs/name - Antenna name, Vector of strings (nantenna)
  character(len=132), dimension(:), pointer ::type => null()       ! /launchs/type - Wave type (LH, EC, IC, ...), Vector of strings (nantenna)
  real(DP),pointer  :: frequency(:) => null()     ! /launchs/frequency - Wave frequency [Hz], Vector (nantenna).
  integer,pointer  :: mode(:) => null()      ! /launchs/mode - Incoming wave mode (+ 1 : slow wave only; -1 both slow and fast wave modes). Vector of integers (nantenna). Time-dependent
  type (type_rzphi1D) :: position  ! /launchs/position - Reference global position of the antenna. Time-dependent
  type (type_spectrum) :: spectrum  ! /launchs/spectrum - Spectral properties of the wave.
  type (type_rf_beam) :: beam  ! /launchs/beam - Beam characteristics
  type (type_codeparam) :: codeparam  ! /launchs/codeparam - 
  real(DP)  :: time=-9.0D40       ! /launchs/time - Time [s]; Time-dependent; Scalar
endtype

! ***********  Include limiter.xsd
type type_limiter  !    
  type (type_datainfo) :: datainfo  ! /limiter/datainfo - 
  type (type_rz1D) :: position  ! /limiter/position - Position (R,Z coordinates) of the limiter [m]; Vector(npoints)
endtype

! ***********  Include magdiag.xsd
type type_setup_floops  !    
  character(len=132), dimension(:), pointer ::name => null()       ! /magdiag/flux_loops/setup_floops/name - Name of loop. Array of strings (nloops).
  character(len=132), dimension(:), pointer ::id => null()       ! /magdiag/flux_loops/setup_floops/id - ID of loop. Array of strings (nloops).
  type (type_rzphi2D) :: position  ! /magdiag/flux_loops/setup_floops/position - List of (R,Z,phi) points defining the position of the loop (see data structure documentation FLUXLOOPposition.pdf); Matrices (nloo
  integer,pointer  :: npoints(:) => null()      ! /magdiag/flux_loops/setup_floops/npoints - Number of points describing each loop in the "position" matrices. Vector (nloops)
endtype

type type_setup_bprobe  !    
  character(len=132), dimension(:), pointer ::name => null()       ! /magdiag/bpol_probes/setup_bprobe/name - Name of the probe.  Array of strings (nprobes).
  character(len=132), dimension(:), pointer ::id => null()       ! /magdiag/bpol_probes/setup_bprobe/id - ID of the probe.  Array of strings (nprobes).
  type (type_rz1D) :: position  ! /magdiag/bpol_probes/setup_bprobe/position - RZ of coil centre [m]; Vector (nprobes)
  real(DP),pointer  :: polangle(:) => null()     ! /magdiag/bpol_probes/setup_bprobe/polangle - Poloidal angle of coil orientation (w.r.t. horizontal ?? to be checked) [rad]; Vector (nprobes)
  real(DP),pointer  :: torangle(:) => null()     ! /magdiag/bpol_probes/setup_bprobe/torangle - Toroidal angle of coil orientation (0 if fully in the poloidal plane) [rad] ; Vector (nprobes)
  real(DP),pointer  :: area(:) => null()     ! /magdiag/bpol_probes/setup_bprobe/area - Area of coil [m^2]; Vector (nprobes)
  real(DP),pointer  :: length(:) => null()     ! /magdiag/bpol_probes/setup_bprobe/length - Length of coil [m]; Vector (nprobes)
  integer,pointer  :: turns(:) => null()      ! /magdiag/bpol_probes/setup_bprobe/turns - Turns in the coil; Vector (nprobes)
endtype

type type_flux_loops  !    
  type (type_setup_floops) :: setup_floops  ! /magdiag/flux_loops/setup_floops - diagnostic setup information
  type (type_exp1D) :: measure  ! /magdiag/flux_loops/measure - Measured flux [Wb]; Time-dependent; Vector (nloops)
endtype

type type_bpol_probes  !    
  type (type_setup_bprobe) :: setup_bprobe  ! /magdiag/bpol_probes/setup_bprobe - diagnostic setup information
  type (type_exp1D) :: measure  ! /magdiag/bpol_probes/measure - Measured value [T]; Time-dependent; Vector (nprobes)
endtype

type type_magdiag  !    
  type (type_datainfo) :: datainfo  ! /magdiag/datainfo - 
  type (type_exp0D) :: ip  ! /magdiag/ip - Plasma current [A]. Positive sign means anti-clockwise when viewed from above.  Time-dependent. Scalar
  type (type_exp0D) :: diamagflux  ! /magdiag/diamagflux - Diamagnetic flux [Wb]; Time-dependent; Scalar
  type (type_flux_loops) :: flux_loops  ! /magdiag/flux_loops - Poloidal flux loops RZ coordinates have 1 component for the full loop and two if there is a negative reference loop
  type (type_bpol_probes) :: bpol_probes  ! /magdiag/bpol_probes - Poloidal field probes
  real(DP)  :: time=-9.0D40       ! /magdiag/time - Time [s]; Time-dependent; Scalar
endtype

! ***********  Include msediag.xsd
type type_setup_mse  !    
  type (type_rzphidrdzdphi1D) :: rzgamma  ! /msediag/setup_mse/rzgamma - Position and width of the intersection between beam and line of sight. Vectors (nchords)
  real(DP),pointer  :: geom_coef(:,:) => null()     ! /msediag/setup_mse/geom_coef - Geometric coefficients (9) describing the angle between beam and line of sight; The first dimension contains succesively : numerat
endtype

type type_msediag  !    
  type (type_datainfo) :: datainfo  ! /msediag/datainfo - 
  type (type_setup_mse) :: setup_mse  ! /msediag/setup_mse - diagnostic setup information
  type (type_exp1D) :: measure  ! /msediag/measure - Measured value (MSE angle gamma [rad]). Time-dependent; Vector (nchords)
  real(DP)  :: time=-9.0D40       ! /msediag/time - Time [s]; Time-dependent; Scalar
endtype

! ***********  Include mhd.xsd
type type_mhd_vector  !    Vector structure for MHD CPO
  real(DP),pointer  :: coord1(:,:,:) => null()     ! /coord1 - First coordinate; Time-dependent; Array 3D (npsi,nn,nm)
  real(DP),pointer  :: coord2(:,:,:) => null()     ! /coord2 - Second coordinate; Time-dependent; Array 3D (npsi,nn,nm)
  real(DP),pointer  :: coord3(:,:,:) => null()     ! /coord3 - Third coordinate; Time-dependent; Array 3D (npsi,nn,nm)
endtype

type type_mhd_vaccum  !    External modes
  real(DP),pointer  :: m(:,:,:) => null()     ! /m - Poloidal mode number; Time-dependent; Array3D (npsi,nn,nm)
  type (type_coord_sys) :: coord_sys  ! /coord_sys - 
  type (type_mhd_vector) :: a_pert  ! /a_pert - Pertubed vector potential
  type (type_mhd_vector) :: b_pert  ! /b_pert - Perturbed magnetic field [T]
endtype

type type_mhd_plasma  !    MHD modes in the confined plasma
  real(DP),pointer  :: psi(:) => null()     ! /psi - Position in poloidal flux [Wb] (without 1/2pi and such that Bp=|grad psi| /R/2/pi). Time-dependent; Vector (npsi)
  real(DP),pointer  :: m(:,:,:) => null()     ! /m - Poloidal mode number; Time-dependent; Array3D (npsi,nn,nm)
  real(DP),pointer  :: disp_perp(:,:,:) => null()     ! /disp_perp - Perpendicular displacement of the mode [m]; Time-dependent; Array 3D (npsi,nn,nm)
  real(DP),pointer  :: disp_par(:,:,:) => null()     ! /disp_par - Parallel displacement of the mode [m]; Time-dependent; Array 3D (npsi,nn,nm)
  real(DP),pointer  :: tau_alfven(:) => null()     ! /tau_alfven - Alven time=R/vA=R0 sqrt(mi ni(rho))/B0 [s]; Definitions of R0, BO, mi, ni to be clarified. rho grid should be included in the MHD 
  real(DP),pointer  :: tau_resistive(:) => null()     ! /tau_resistive - Resistive time = mu_0 rho*rho/1.22/eta_neo [s]; Source of eta_neo to be clarified. Time-dependent; Vector (npsi)
  type (type_coord_sys) :: coord_sys  ! /coord_sys - 
  type (type_mhd_vector) :: a_pert  ! /a_pert - Pertubed vector potential
  type (type_mhd_vector) :: b_pert  ! /b_pert - Perturbed magnetic field [T]
  type (type_mhd_vector) :: v_pert  ! /v_pert - Perturbed velocity [m/s]
  real(DP),pointer  :: rho_masspert(:,:,:) => null()     ! /rho_masspert - Perturbed mass density [kg/m^3]; Time-dependent; Array 3D (npsi,nn,nm)
  real(DP),pointer  :: temp_pert(:,:,:) => null()     ! /temp_pert - Perturbed temperature [eV]; Time-dependent; Array 3D (npsi,nn,nm)
endtype

type type_mhd  !    
  type (type_datainfo) :: datainfo  ! /mhd/datainfo - 
  integer,pointer  :: n(:) => null()      ! /mhd/n - Toroidal mode number; Time-dependent; Vector (nn)
  real(DP),pointer  :: frequency(:) => null()     ! /mhd/frequency - Frequency of the mode [Hz]; Time-dependent; Vector (nn)
  real(DP),pointer  :: growthrate(:) => null()     ! /mhd/growthrate - Linear growthrate of the mode [Hz]; Time-dependent; Vector (nn)
  type (type_mhd_plasma) :: plasma  ! /mhd/plasma - MHD modes in the confined plasma
  type (type_mhd_vaccum) :: vaccum  ! /mhd/vaccum - External modes
  real(DP)  :: time=-9.0D40       ! /mhd/time - Time [s]; Time-dependent; Scalar.
  type (type_codeparam) :: codeparam  ! /mhd/codeparam - 
endtype

! ***********  Include nbi.xsd
type type_beamlets  !    
  integer,pointer  :: nbeamlets(:) => null()      ! /nbi/setup_inject/beamlets/nbeamlets - Number of beamlets of a unit; Vector(nunits)
  type (type_rzphi2D) :: position  ! /nbi/setup_inject/beamlets/position - Position of beamlets. Matrices(nunits, max_nbeamlets)
  real(DP),pointer  :: tang_rad_blt(:,:) => null()     ! /nbi/setup_inject/beamlets/tang_rad_blt - Tangency radius (major radius where the central line of a beamlet is tangent to a circle around the torus) [m]; Matrix(nunits, max
  real(DP),pointer  :: angle_blt(:,:) => null()     ! /nbi/setup_inject/beamlets/angle_blt - Angle of inclination between a line at the centre of a beamlet and the horiontal plane [rad]; Matrix(nunits, max_nbeamlets)
  real(DP),pointer  :: pow_frc_blt(:,:) => null()     ! /nbi/setup_inject/beamlets/pow_frc_blt - Fraction of power of a unit injected by a beamlet; Matrix(nunits, max_nbeamlets)
endtype

type type_inj_spec  !    
  real(DP),pointer  :: amn(:) => null()     ! /nbi/inj_spec/amn - Atomic mass number; Vector (nunits)
  real(DP),pointer  :: zn(:) => null()     ! /nbi/inj_spec/zn - Nuclear charge; Vector (nunits)
  real(DP),pointer  :: zion(:) => null()     ! /nbi/inj_spec/zion - Ion charge; Vector (nunits)
endtype

type type_setup_inject  !    
  type (type_rzphi1D) :: position  ! /nbi/setup_inject/position - Position of centre of injection unit surface. Vectors(nunits).
  real(DP),pointer  :: tang_rad(:) => null()     ! /nbi/setup_inject/tang_rad - Tagency radius (major radius where the central line of a NBI unit is tangent to a circle around the torus) [m]; Vector(nunits)
  real(DP),pointer  :: angle(:) => null()     ! /nbi/setup_inject/angle - Angle of inclination between a line at the centre of the injection unit surface and the horiontal plane [rad];  Vector(nunits)
  integer,pointer  :: direction(:) => null()      ! /nbi/setup_inject/direction - Direction of the beam seen from above the torus: -1 = clockwise; 1 = counter clockwise; Vector(nunits)
  real(DP),pointer  :: div_vert(:) => null()     ! /nbi/setup_inject/div_vert - Beam divergence for a unit in the vertical direction[rad]; Vector(nunits)
  real(DP),pointer  :: div_horiz(:) => null()     ! /nbi/setup_inject/div_horiz - Beam divergence for a unit in the horizontal direction[rad]; Vector(nunits)
  real(DP),pointer  :: focal_len_hz(:) => null()     ! /nbi/setup_inject/focal_len_hz - Horizontal focal length along the beam line [m], Vector(nunits)
  real(DP),pointer  :: focal_len_vc(:) => null()     ! /nbi/setup_inject/focal_len_vc - Vertical focal length along the beam line [m], Vector(nunits)
  type (type_beamlets) :: beamlets  ! /nbi/setup_inject/beamlets - Detailed information on beamlets.
endtype

type type_nbi  !    
  type (type_datainfo) :: datainfo  ! /nbi/datainfo - 
  type (type_inj_spec) :: inj_spec  ! /nbi/inj_spec - Injected species
  type (type_exp1D) :: pow_unit  ! /nbi/pow_unit - Power delivered by an NBI unit [W]; Time-dependent; Vector(nunits)
  type (type_exp1D) :: inj_eng_unit  ! /nbi/inj_eng_unit - Full injection energy of a unit [ev]; Time-dependent; Vector(nunits)
  type (type_exp1D) :: halfe_cfr  ! /nbi/halfe_cfr - Beam current fraction (of total) for half energy component; Time-dependent; Vector(nunits)
  type (type_exp1D) :: thirde_cfr  ! /nbi/thirde_cfr - Beam current fraction (of total) for the one third energy component. Time-dependent; Vector(nunits)
  type (type_setup_inject) :: setup_inject  ! /nbi/setup_inject - Detailed information on an injection unit.
  type (type_codeparam) :: codeparam  ! /nbi/codeparam - Code parameters
  real(DP)  :: time=-9.0D40       ! /nbi/time - Time [s]; Time-dependent; Scalar
endtype

! ***********  Include neoclassic.xsd
type type_neoclassic  !    
  type (type_datainfo) :: datainfo  ! /neoclassic/datainfo - 
  real(DP),pointer  :: rho_tor_norm(:) => null()     ! /neoclassic/rho_tor_norm - Normalised toroidal flux coordinate values (= rho_tor normalised to the value at the last grid point); Vector (nrho)
  real(DP),pointer  :: rho_tor(:) => null()     ! /neoclassic/rho_tor - Toroidal flux coordinate (not normalised, equivalent to rho_tor_norm) [m];  Vector (nrho). Time-dependent.
  type (type_composition) :: composition  ! /neoclassic/composition - 
  type (type_transcoefion) :: ni_neo  ! /neoclassic/ni_neo - Neoclassical transport coefficients for ion density equation. Time-dependent.
  type (type_transcoefel) :: ne_neo  ! /neoclassic/ne_neo - Neoclassical transport coefficients for electron density equation. Time-dependent.
  type (type_transcoefimp) :: nz_neo  ! /neoclassic/nz_neo - Neoclassical transport coefficients for impurity (multiple charge state) density equation. Time-dependent.
  type (type_transcoefion) :: ti_neo  ! /neoclassic/ti_neo - Neoclassical transport coefficients for ion temperature equation. Time-dependent.
  type (type_transcoefel) :: te_neo  ! /neoclassic/te_neo - Neoclassical transport coefficients for electron temperature equation. Time-dependent.
  type (type_transcoefimp) :: tz_neo  ! /neoclassic/tz_neo - Neoclassical transport coefficients for impurity  (multiple charge state) temperature equation. Time-dependent.
  type (type_transcoefel) :: mtor_neo  ! /neoclassic/mtor_neo - Neoclassical transport coefficients for total toroidal momentum equation. Time-dependent.
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
  type (type_orbit_pos) :: outer  ! /orbit/orb_glob_dat/special_pos/midplane/outer - Position at outer mid-plane
  type (type_orbit_pos) :: inner  ! /orbit/orb_glob_dat/special_pos/midplane/inner - Position at inner mid-plane
endtype

type type_turning_pts  !    
  type (type_orbit_pos) :: upper  ! /orbit/orb_glob_dat/special_pos/turning_pts/upper - Position at upper turning point
  type (type_orbit_pos) :: lower  ! /orbit/orb_glob_dat/special_pos/turning_pts/lower - Position at lower turning point
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
  type (type_rz3D) :: rzcoordinate  ! /pfgeometry/rzcoordinate - Irregular outline [m]; 3D arrays (ncoils,max_nelements,max_npoints)
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
  type (type_exp1D) :: coilcurrent  ! /pfcoils/coilcurrent - Circuit feed current in the coil , defined positive if it flows from point 1 to point 2 of the component in the pfcircuit descript
  type (type_exp1D) :: coilvoltage  ! /pfcoils/coilvoltage - Voltage on the full coil [V]; Time-dependent; Vector (ncoils)
endtype

! ***********  Include pfpassive.xsd
type type_pfpageometry  !    
  integer,pointer  :: type(:) => null()      ! /pfpassive/pfpageometry/type - Type used to describe the shape (0 for 'rzcoordinates' or 1 for 'rzdrdz'); Vector of integers (nelements)
  integer,pointer  :: npoints(:) => null()      ! /pfpassive/pfpageometry/npoints - Number of points describing an element (irregular outline rzcoordinates); Vector of integers (nelements)
  type (type_rz2D) :: rzcoordinate  ! /pfpassive/pfpageometry/rzcoordinate - Irregular outline [m]; Matrix (nelements,max_npoints)
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
  type (type_exp1D) :: voltage  ! /pfsupplies/voltage - Voltage at the supply output [V]; Time-dependent; Vector  (nsupplies)
  type (type_exp1D) :: current  ! /pfsupplies/current - Current at the supply output, defined positive if it flows from point 1 to point 2 of the component in the pfcircuit description [
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

! ***********  Include reference.xsd
type type_ref_nt_0dr_ref  !    a non-timed reference of real type
  real(DP)  :: value=-9.0D40       ! /value - Value of the reference. Real scalar.
  character(len=132), dimension(:), pointer ::description => null()       ! /description - Description of the reference. String.
endtype

type type_ref_nt_0di_ref  !    a non-timed reference of integer type
  integer  :: value=-999999999       ! /value - Value of the reference. Integer scalar.
  character(len=132), dimension(:), pointer ::description => null()       ! /description - Description of the reference. String.
endtype

type type_ref_nt_0ds_ref  !    a non-timed reference of string type
  character(len=132), dimension(:), pointer ::value => null()       ! /value - Value of the reference. String
  character(len=132), dimension(:), pointer ::description => null()       ! /description - Description of the reference. String.
endtype

type type_ref_nt_1dr_ref  !    a non-timed reference of vecflt type
  real(DP),pointer  :: value(:) => null()     ! /value - Value of the reference. Vector.
  character(len=132), dimension(:), pointer ::description => null()       ! /description - Description of the reference. String.
endtype

type type_ref_nt_1di_ref  !    a non-timed reference of vecint type
  integer,pointer  :: value(:) => null()      ! /value - Value of the reference. Vector of integers.
  character(len=132), dimension(:), pointer ::description => null()       ! /description - Description of the reference. String.
endtype

type type_ref_nt_0dr  !    set of non-timed references of real type
  type (type_ref_nt_0dr_ref) :: ref1  ! /ref1 - Reference signal #1
  type (type_ref_nt_0dr_ref) :: ref2  ! /ref2 - Reference signal #2
  type (type_ref_nt_0dr_ref) :: ref3  ! /ref3 - Reference signal #3
  type (type_ref_nt_0dr_ref) :: ref4  ! /ref4 - Reference signal #4
  type (type_ref_nt_0dr_ref) :: ref5  ! /ref5 - Reference signal #5
  type (type_ref_nt_0dr_ref) :: ref6  ! /ref6 - Reference signal #6
  type (type_ref_nt_0dr_ref) :: ref7  ! /ref7 - Reference signal #7
endtype

type type_ref_nt_0di  !    set of non-timed references of integer type
  type (type_ref_nt_0di_ref) :: ref1  ! /ref1 - Reference signal #1
  type (type_ref_nt_0di_ref) :: ref2  ! /ref2 - Reference signal #2
  type (type_ref_nt_0di_ref) :: ref3  ! /ref3 - Reference signal #3
  type (type_ref_nt_0di_ref) :: ref4  ! /ref4 - Reference signal #4
endtype

type type_ref_nt_0ds  !    set of non-timed references of string type
  type (type_ref_nt_0ds_ref) :: ref1  ! /ref1 - Reference signal #1
  type (type_ref_nt_0ds_ref) :: ref2  ! /ref2 - Reference signal #2
endtype

type type_ref_nt_1dr  !    set of non-timed references of vecflt type
  type (type_ref_nt_1dr_ref) :: ref1  ! /ref1 - Reference signal #1
  type (type_ref_nt_1dr_ref) :: ref2  ! /ref2 - Reference signal #2
  type (type_ref_nt_1dr_ref) :: ref3  ! /ref3 - Reference signal #3
  type (type_ref_nt_1dr_ref) :: ref4  ! /ref4 - Reference signal #4
  type (type_ref_nt_1dr_ref) :: ref5  ! /ref5 - Reference signal #5
endtype

type type_ref_nt_1di  !    set of non-timed references of vecint type
  type (type_ref_nt_1di_ref) :: ref1  ! /ref1 - Reference signal #1
  type (type_ref_nt_1di_ref) :: ref2  ! /ref2 - Reference signal #2
  type (type_ref_nt_1di_ref) :: ref3  ! /ref3 - Reference signal #3
  type (type_ref_nt_1di_ref) :: ref4  ! /ref4 - Reference signal #4
endtype

type type_ref_nt  !    set of non-timed references
  type (type_ref_nt_0dr) :: zerod_real  ! /zerod_real - 0d reference of real type
  type (type_ref_nt_0di) :: zerod_int  ! /zerod_int - 0d reference of integer type
  type (type_ref_nt_0ds) :: zerod_string  ! /zerod_string - 0d reference of string type
  type (type_ref_nt_1dr) :: oned_real  ! /oned_real - 1d reference of real type
  type (type_ref_nt_1di) :: oned_int  ! /oned_int - 1d reference of integer type
endtype

type type_ref_t_0dr_ref  !    a timed reference of real type
  real(DP)  :: value=-9.0D40       ! /value - Value of the reference. Real scalar. Time-dependent.
  character(len=132), dimension(:), pointer ::description => null()       ! /description - Description of the reference. String.
endtype

type type_ref_t_0di_ref  !    a timed reference of integer type
  integer  :: value=-999999999       ! /value - Value of the reference. Integer scalar. Time-dependent.
  character(len=132), dimension(:), pointer ::description => null()       ! /description - Description of the reference. String.
endtype

type type_ref_t_1dr_ref  !    a timed reference of vecflt type
  real(DP),pointer  :: value(:) => null()     ! /value - Value of the reference. Vector. Time-dependent.
  character(len=132), dimension(:), pointer ::description => null()       ! /description - Description of the reference. String.
endtype

type type_ref_t_1di_ref  !    a timed reference of vecint type
  integer,pointer  :: value(:) => null()      ! /value - Value of the reference. Vector of integers. Time-dependent.
  character(len=132), dimension(:), pointer ::description => null()       ! /description - Description of the reference. String.
endtype

type type_ref_t_0dr  !    set of timed references of real type
  type (type_ref_t_0dr_ref) :: ref1  ! /ref1 - Reference signal #1
  type (type_ref_t_0dr_ref) :: ref2  ! /ref2 - Reference signal #2
  type (type_ref_t_0dr_ref) :: ref3  ! /ref3 - Reference signal #3
  type (type_ref_t_0dr_ref) :: ref4  ! /ref4 - Reference signal #4
  type (type_ref_t_0dr_ref) :: ref5  ! /ref5 - Reference signal #5
  type (type_ref_t_0dr_ref) :: ref6  ! /ref6 - Reference signal #6
  type (type_ref_t_0dr_ref) :: ref7  ! /ref7 - Reference signal #7
  type (type_ref_t_0dr_ref) :: ref8  ! /ref8 - Reference signal #8
  type (type_ref_t_0dr_ref) :: ref9  ! /ref9 - Reference signal #9
  type (type_ref_t_0dr_ref) :: ref10  ! /ref10 - Reference signal #10
endtype

type type_ref_t_0di  !    set of timed references of integer type
  type (type_ref_t_0di_ref) :: ref1  ! /ref1 - Reference signal #1
  type (type_ref_t_0di_ref) :: ref2  ! /ref2 - Reference signal #2
  type (type_ref_t_0di_ref) :: ref3  ! /ref3 - Reference signal #3
  type (type_ref_t_0di_ref) :: ref4  ! /ref4 - Reference signal #4
endtype

type type_ref_t_1dr  !    set of timed references of vecflt type
  type (type_ref_t_1dr_ref) :: ref1  ! /ref1 - Reference signal #1
  type (type_ref_t_1dr_ref) :: ref2  ! /ref2 - Reference signal #2
  type (type_ref_t_1dr_ref) :: ref3  ! /ref3 - Reference signal #3
  type (type_ref_t_1dr_ref) :: ref4  ! /ref4 - Reference signal #4
  type (type_ref_t_1dr_ref) :: ref5  ! /ref5 - Reference signal #5
endtype

type type_ref_t_1di  !    set of timed references of vecint type
  type (type_ref_t_1di_ref) :: ref1  ! /ref1 - Reference signal #1
  type (type_ref_t_1di_ref) :: ref2  ! /ref2 - Reference signal #2
  type (type_ref_t_1di_ref) :: ref3  ! /ref3 - Reference signal #3
  type (type_ref_t_1di_ref) :: ref4  ! /ref4 - Reference signal #4
endtype

type type_ref_t  !    set of timed references
  type (type_ref_t_0dr) :: zerod_real  ! /zerod_real - 0d reference of real type
  type (type_ref_t_0di) :: zerod_int  ! /zerod_int - 0d reference of integer type
  type (type_ref_t_1dr) :: oned_real  ! /oned_real - 1d reference of real type
  type (type_ref_t_1di) :: oned_int  ! /oned_int - 1d reference of integer type
endtype

type type_reference  !    
  type (type_datainfo) :: datainfo  ! /reference/datainfo - 
  type (type_ref_nt) :: non_timed  ! /reference/non_timed - Time-independent references (parameters)
  type (type_ref_t) :: timed  ! /reference/timed - Time-dependent references
  real(DP)  :: time=-9.0D40       ! /reference/time - Time [s]; Time-dependent; Scalar
endtype

! ***********  Include toroidalfield.xsd
type type_toroidfield  !    
  type (type_datainfo) :: datainfo  ! /toroidfield/datainfo - 
  integer  :: nturns=-999999999       ! /toroidfield/nturns - Number of total turns in the toroidal field coil
  integer  :: ncoils=-999999999       ! /toroidfield/ncoils - Number of packets of coils
  type (type_exp0D) :: current  ! /toroidfield/current - Current in the toroidal field coils [A]; Time-dependent. Scalar.
  type (type_exp0D) :: bvac_r  ! /toroidfield/bvac_r - Vacuum field times radius in the toroidal field magnet [T.m]. Positive sign means anti-clockwise when viewed from above. Time-depe
  real(DP)  :: r0=-9.0D40       ! /toroidfield/r0 - Characteristic major radius of the device (used in publications, usually middle of the vessel at the equatorial midplane) [m]. Sca
  real(DP)  :: time=-9.0D40       ! /toroidfield/time - Time [s]; Time-dependent. Scalar.
endtype

! ***********  Include tsdiag.xsd
type type_tssetup  !    diagnostic setup information
  type (type_rz1D) :: position  ! /position - RZ of intersection between laser and line of sight [m]; Vector (nchords)
endtype

type type_tsmeasure  !    Measured values (Thomson scattering)
  type (type_exp1D) :: te  ! /te - Electron temperature [eV]. Vector (nchords)
  type (type_exp1D) :: ne  ! /ne - Electron density [m^-3]. Vector (nchords)
endtype

type type_tsdiag  !    
  type (type_datainfo) :: datainfo  ! /tsdiag/datainfo - 
  type (type_tssetup) :: setup  ! /tsdiag/setup - diagnostic setup information
  type (type_tsmeasure) :: measure  ! /tsdiag/measure - Measured values
  real(DP)  :: time=-9.0D40       ! /tsdiag/time - Time [s]; Time-dependent; Scalar
endtype

! ***********  Include turbulence.xsd
type type_turbgrid  !    Generic structure for a turbulence grid.
  real(DP),pointer  :: dim1(:) => null()     ! /dim1 - First dimension values; Vector (ndim1).
  real(DP),pointer  :: dim2(:) => null()     ! /dim2 - Second dimension values; Vector (ndim2).
  real(DP),pointer  :: dim3(:) => null()     ! /dim3 - Third dimension values; Vector (ndim3).
  real(DP),pointer  :: dim_v1(:) => null()     ! /dim_v1 - First v-space dimension values; Vector (ndim_v1).
  real(DP),pointer  :: dim_v2(:) => null()     ! /dim_v2 - Second v-space dimension values; Vector (ndim_v2).
endtype

type type_turbcoordsys  !    Decription of the coordinates and metric.
  character(len=132), dimension(:), pointer ::grid_type => null()       ! /grid_type - Type of coordinate system.
  type (type_turbgrid) :: turbgrid  ! /turbgrid - Turbulence grid used by the codes; Time-dependent.
  real(DP),pointer  :: jacobian(:,:) => null()     ! /jacobian - Jacobian of the coordinate system; Time-dependent; Matrix (ndim1, ndim2).
  real(DP),pointer  :: g_11(:,:) => null()     ! /g_11 - metric coefficients g_11; Time-dependent; Matrix (ndim1, ndim2).
  real(DP),pointer  :: g_12(:,:) => null()     ! /g_12 - metric coefficients g_12; Time-dependent; Matrix (ndim1, ndim2).
  real(DP),pointer  :: g_13(:,:) => null()     ! /g_13 - metric coefficients g_13; Time-dependent; Matrix (ndim1, ndim2).
  real(DP),pointer  :: g_22(:,:) => null()     ! /g_22 - metric coefficients g_22; Time-dependent; Matrix (ndim1, ndim2).
  real(DP),pointer  :: g_33(:,:) => null()     ! /g_33 - metric coefficients g_33; Time-dependent; Matrix (ndim1, ndim2).
  type (type_rzphi3D) :: position  ! /position - R Z phi positions of grid points; Time-dependent; Array3D (ndim1, ndim2, ndim3).
endtype

type type_turbvar0d  !    Time traces.
  character(len=132), dimension(:), pointer ::dtime_type => null()       ! /dtime_type - Description of time trace e.g. last ndtime points.
  real(DP),pointer  :: dtime(:) => null()     ! /dtime - Fast diagnostic time [s]; Time-dependent; Vector (ndtime).
  real(DP),pointer  :: en_exb(:) => null()     ! /en_exb - ExB energy [J/m^3]; Time-dependent; Vector (ndtime).
  real(DP),pointer  :: en_mag(:) => null()     ! /en_mag - Magnetic energy [J/m^3]; Time-dependent; Vector (ndtime).
  real(DP),pointer  :: en_el_th(:) => null()     ! /en_el_th - electron thermal energy or free energy [J/m^3]; Time-dependent.
  real(DP),pointer  :: en_ion_th(:,:) => null()     ! /en_ion_th - Ion thermal energy or free energy [J/m^3]; Time-dependent; Matrix (ndtime, nion).
  real(DP),pointer  :: en_el_par(:) => null()     ! /en_el_par - Electron parallel energy [J/m^3]; Time-dependent; Vector (ndtime).
  real(DP),pointer  :: en_ion_par(:,:) => null()     ! /en_ion_par - Ion parallel energy [J/m^3]; Time-dependent; Matrix (ndtime,nion).
  real(DP),pointer  :: en_tot(:) => null()     ! /en_tot - Total energy or free energy [J/m^3]; Time-dependent; Vector (ndtime).
  real(DP),pointer  :: fl_el(:) => null()     ! /fl_el - Electron flux [m^-2 s^-1]; Time-dependent; Vector (ndtime).
  real(DP),pointer  :: fl_heatel(:) => null()     ! /fl_heatel - Conductive electron heat flux [eV m^-2 s^-1]; Time-dependent; Vector (ndtime).
  real(DP),pointer  :: fl_ion(:,:) => null()     ! /fl_ion - Ion flux [m^-2 s^-1]; Time-dependent; Matrix (ndtime, nion).
  real(DP),pointer  :: fl_heation(:,:) => null()     ! /fl_heation - Conductive ion heat flux [eV m^-2 s^-1]; Time-dependent; Matrix (ndtime, nion).
  real(DP),pointer  :: fl_magel(:) => null()     ! /fl_magel - Electron flux [m^-2 s^-1]; Time-dependent; Vector (ndtime).
  real(DP),pointer  :: fl_magheatel(:) => null()     ! /fl_magheatel - Conductive electron heat flux [eV m^-2 s^-1]; Time-dependent; Vector (ndtime).
  real(DP),pointer  :: fl_magion(:,:) => null()     ! /fl_magion - Ion flux [m^-2 s^-1]; Time-dependent; Matrix (ndtime, nion).
  real(DP),pointer  :: flmagheation(:,:) => null()     ! /flmagheation - Conductive ion heat flux [eV m^-2 s^-1]; Time-dependent; Matrix (ndtime, nion).
endtype

type type_turbvar1d  !    Dependent variable radial profile.
  real(DP),pointer  :: rho_tor_norm(:) => null()     ! /rho_tor_norm - Normalised toroidal flux  coordinate for the var1d structure. Vector(nrho1d)
  real(DP),pointer  :: phi(:) => null()     ! /phi - Electrostatic potential [V]; Time-dependent; Vector (nrho1d).
  real(DP),pointer  :: er(:) => null()     ! /er - Radial electric field [V/m]; Time-dependent; Vector (nrho1d).
  real(DP),pointer  :: vor(:) => null()     ! /vor - Vorticity [s^-1]; Time-dependent; Vector (nrho1d).
  real(DP),pointer  :: apl(:) => null()     ! /apl - Parallel magnetic potential divided by B [m]; Time-dependent; Vector (nrho1d).
  real(DP),pointer  :: jpl(:) => null()     ! /jpl - Parallel current divided by B [A/m^2 per T]; Time-dependent; Vector (nrho1d).
  real(DP),pointer  :: ne(:) => null()     ! /ne - Electron density [m^-3]; Time-dependent; Vector (nrho1d).
  real(DP),pointer  :: te(:) => null()     ! /te - Electron temperature [eV]; Time-dependent; Vector (nrho1d).
  real(DP),pointer  :: ni(:,:) => null()     ! /ni - Ion density [m^-3]; Time-dependent; Matrix (nrho1d,nion).
  real(DP),pointer  :: ti(:,:) => null()     ! /ti - Ion temperature [eV]; Time-dependent; Matrix (nrho1d,nion).
  real(DP),pointer  :: ui(:,:) => null()     ! /ui - Ion parallel velocity divided by B [m/s per T]; Time-dependent; Matrix (nrho1d,nion).
endtype

type type_turbvar2d  !    Dependent variable axisymmetric.
  real(DP),pointer  :: rho_tor_norm(:) => null()     ! /rho_tor_norm - Normalised toroidal flux  coordinate for the var2d structure. Vector(nrho2d)
  real(DP),pointer  :: theta(:) => null()     ! /theta - Straight field line poloidal angle for the var2d structure. Vector(ntheta2d)
  real(DP),pointer  :: phi(:,:) => null()     ! /phi - Electrostatic potential [V]; Time-dependent; Matrix (nrho2d,ntheta2d).
  real(DP),pointer  :: apl(:,:) => null()     ! /apl - Parallel magnetic potential divided by B [m]; Time-dependent; Matrix(nrho2d,ntheta2d).
  real(DP),pointer  :: jpl(:,:) => null()     ! /jpl - Parallel current divided by B [A/m^2 per T]; Time-dependent; Matrix (nrho2d,ntheta2d).
  real(DP),pointer  :: vor(:,:) => null()     ! /vor - Vorticity [s^-1]; Time-dependent; Matrix(nrho2d,ntheta2d).
  real(DP),pointer  :: ne(:,:) => null()     ! /ne - Electron density [m^-3]; Time-dependent; Matrix (nrho2d,ntheta2d).
  real(DP),pointer  :: te(:,:) => null()     ! /te - Electron temperature [eV]; Time-dependent; Matrix (nrho2d,ntheta2d).
  real(DP),pointer  :: ni(:,:,:) => null()     ! /ni - Ion density [m^-3]; Time-dependent; Array3D (nrho2d,ntheta2d,nion).
  real(DP),pointer  :: ti(:,:,:) => null()     ! /ti - Ion temperature [eV]; Time-dependent; Array3D (nrho2d,ntheta2d,nion).
  real(DP),pointer  :: ui(:,:,:) => null()     ! /ui - Ion parallel velocity divided by B [m/s per T]; Time-dependent; Array3D(nrho2d,ntheta2d,nion).
endtype

type type_turbvar3d  !    Dependent variable morphology (on the internal grid code coord_sys/turbgrid).
  real(DP),pointer  :: phi(:,:,:) => null()     ! /phi - Electrostatic potential [V]; Time-dependent; Array3D(ndim1,ndim2,ndim3).
  real(DP),pointer  :: vor(:,:,:) => null()     ! /vor - Vorticity [s^-1]; Time-dependent; Array3D(ndim1,ndim2,ndim3).
  real(DP),pointer  :: jpl(:,:,:) => null()     ! /jpl - Parallel current [A/m^2]; Time-dependent; Array3D(ndim1,ndim2,ndim3).
  real(DP),pointer  :: ne(:,:,:) => null()     ! /ne - Electron density [m^-3]; Time-dependent; Array3D(ndim1,ndim2,ndim3).
endtype

type type_turbspec1d  !    Toroidal mode number spectra.
  real(DP),pointer  :: dim_spec(:) => null()     ! /dim_spec - Perp Wavenumber Spectrum values; Vector (ndim_spec).
  real(DP),pointer  :: phi(:) => null()     ! /phi - Electrostatic potential [V^2 per mode]; Time-dependent; Vector (ndim_spec).
  real(DP),pointer  :: vor(:) => null()     ! /vor - Vorticity [s^-2 per mode]; Time-dependent; Vector (ndim_spec).
  real(DP),pointer  :: b(:) => null()     ! /b - Magnetic energy [T^2 per mode]; Time-dependent; Vector (ndim_spec).
  real(DP),pointer  :: jpl(:) => null()     ! /jpl - Current [A^2/m^4 per mode]; Time-dependent; Vector (ndim_spec).
  real(DP),pointer  :: ne(:) => null()     ! /ne - Electron density [m^-6 per mode]; Time-dependent; Vector (ndim_spec).
  real(DP),pointer  :: te(:) => null()     ! /te - Electron temperature [eV^2 per mode]; Time-dependent; Vector (ndim_spec).
  real(DP),pointer  :: ti(:,:) => null()     ! /ti - Ion temperature [eV^2 per mode]; Time-dependent; Matrix (ndim_spec,nion).
  real(DP),pointer  :: fe(:) => null()     ! /fe - Electron particle flux [m^-2/s per mode]; Time-dependent; Vector (ndim_spec).
  real(DP),pointer  :: qe(:) => null()     ! /qe - Electron conductive heat flux [eV m/s per mode]; Time-dependent; Vector (ndim_spec).
  real(DP),pointer  :: qi(:,:) => null()     ! /qi - Ion conductive heat flux [eV m/s per mode]; Time-dependent; Matrix(ndim_spec,nion).
  real(DP),pointer  :: me(:) => null()     ! /me - Magnetic electron heat flux [eV m/s per mode]; Time-dependent; Matrix (ndim_spec).
  real(DP),pointer  :: mi(:,:) => null()     ! /mi - Magnetic ion heat flux [eV m/s per mode]; Time-dependent; Matrix (ndim_spec,nion).
endtype

type type_turbenv1d  !    Parallel fluctuation envelope.
  real(DP),pointer  :: theta(:) => null()     ! /theta - Straight field line poloidal angle; Vector (ntheta_env).
  real(DP),pointer  :: phi(:) => null()     ! /phi - Electrostatic potential [V^2]; Time-dependent; Vector (ntheta_env).
  real(DP),pointer  :: vor(:) => null()     ! /vor - Vorticity [coulomb^2/m^6]; Time-dependent; Vector (ntheta_env).
  real(DP),pointer  :: jpl(:) => null()     ! /jpl - Parallel current [A^2/m^4]; Time-dependent; Vector (ntheta_env).
  real(DP),pointer  :: ne(:) => null()     ! /ne - Electron density [m^-6]; Time-dependent; Vector (ntheta_env).
  real(DP),pointer  :: he(:) => null()     ! /he - Nonadiabatic electron density [m^-6]; Time-dependent; Vector (ntheta_env).
  real(DP),pointer  :: te(:) => null()     ! /te - Electron temperature [eV^2]; Time-dependent; Vector (ntheta_env).
  real(DP),pointer  :: ni(:,:) => null()     ! /ni - Ion density [m^-6]; Time-dependent; Matrix(ntheta_env,nion).
  real(DP),pointer  :: ti(:,:) => null()     ! /ti - Ion temperature [eV^2]; Time-dependent; Matrix(ntheta_env,nion).
  real(DP),pointer  :: ui(:,:) => null()     ! /ui - Ion parallel velocity [m^2/s^2]; Time-dependent; Matrix (ntheta_env,nion).
  real(DP),pointer  :: fe(:) => null()     ! /fe - Electron particle flux [m^-2/s per mode]; Time-dependent; Vector (ntheta_env).
  real(DP),pointer  :: qe(:) => null()     ! /qe - Electron conductive heat flux [eV m^-2/s per mode]; Time-dependent; Vector (ntheta_env).
  real(DP),pointer  :: qi(:,:) => null()     ! /qi - Ion conductive heat flux [eV m^-2/s per mode]; Time-dependent; Matrix(ntheta_env,nion).
  real(DP),pointer  :: me(:) => null()     ! /me - Magnetic electron heat flux [eV m^-2/s per mode]; Time-dependent; Vector (ntheta_env).
  real(DP),pointer  :: mi(:,:) => null()     ! /mi - Magnetic ion heat flux [eV m^-2/s per mode]; Time-dependent; Matrix(ntheta_env,nion).
endtype

type type_turbulence  !    
  type (type_datainfo) :: datainfo  ! /turbulence/datainfo - 
  type (type_composition) :: composition  ! /turbulence/composition - 
  type (type_turbcoordsys) :: coordsys  ! /turbulence/coordsys - Decription of the coordinates and metric used by the codes.
  type (type_turbvar0d) :: var0d  ! /turbulence/var0d - Diagnostic fast time traces.
  type (type_turbvar1d) :: var1d  ! /turbulence/var1d - Dependent variable radial profile.
  type (type_turbvar2d) :: var2d  ! /turbulence/var2d - Dependent variable axisymmetric.
  type (type_turbvar3d) :: var3d  ! /turbulence/var3d - Dependent variable morphology. Grid is defined in coord_sys/turbgrid.
  type (type_turbspec1d) :: spec1d  ! /turbulence/spec1d - Toroidal mode number spectra.
  type (type_turbenv1d) :: env1d  ! /turbulence/env1d - Parallel fluctuation envelope.
  type (type_codeparam) :: codeparam  ! /turbulence/codeparam - 
  real(DP)  :: time=-9.0D40       ! /turbulence/time - Time [s]; Time-dependent; Scalar.
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
  type (type_sawteeth_profiles1d) :: profiles1d  ! /sawteeth/profiles1d - Core profiles after sawtooth crash
  type (type_sawteeth_diags) :: diags  ! /sawteeth/diags - 
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
  type (type_scenario_ref) :: te_ave  ! /te_ave - volume averaged electron temperature [eV]. Time-dependent.
  type (type_scenario_ref) :: ti_ave  ! /ti_ave - volume averaged ion temperature [eV]. Time-dependent.
  type (type_scenario_ref) :: ne_ave  ! /ne_ave - volume averaged electron density [m^-3]. Time-dependent.
  type (type_scenario_ref) :: dne_ave_dt  ! /dne_ave_dt - time derivative of volume averaged electron density [m^-3/s]. Time-dependent.
  type (type_scenario_ref) :: ni_ave  ! /ni_ave - volume averaged ion density (<sum(n_k)>, k in species) [m^-3]. Time-dependent.
  type (type_scenario_ref) :: zeff_ave  ! /zeff_ave - volume averaged effective charge. Time-dependent.
  type (type_scenario_ref) :: ti_o_te_ave  ! /ti_o_te_ave - volume averaged ion temperature over electron temperature  (<Ti/Te>) []. Time-dependent.
  type (type_scenario_ref) :: meff_ave  ! /meff_ave - volume averaged effectice mass  (<sum(n_k * m_k)  > /  < sum(n_k)> ) []. Time-dependent.
  type (type_scenario_ref) :: pellet_flux  ! /pellet_flux - number of electrons fuelling  the plasma every second coming from pellet injection [s^-1]. Time-dependent.
  real(DP),pointer  :: nions_ave(:) => null()     ! /nions_ave - volume averaged ions densities (vector, one element per ion species) [m^-3]. Time-dependent.
  type (type_scenario_ref) :: omega_ave  ! /omega_ave - bulk volume average toroidal rotation velocity (whole plasma) [rad/s]. Time-dependent.
endtype

type type_scenario_references  !    References
  type (type_scenario_ref) :: plh  ! /plh - Lower hybrid power [W]. Time-dependent.
  type (type_scenario_ref) :: picrh  ! /picrh - Ion cyclotron resonnance heating power [W]. Time-dependent.
  type (type_scenario_ref) :: pecrh  ! /pecrh - electron cyclotron resonnance heating power [W]. Time-dependent.
  type (type_scenario_ref) :: pnbi  ! /pnbi - neutral beam injection power [W]. Time-dependent.
  type (type_scenario_ref) :: ip  ! /ip - Plasma current [A]. Time-dependent.
  type (type_scenario_ref) :: bvac_r  ! /bvac_r - Vacuum field times radius in the toroidal field magnet [T.m]. Time-dependent.
  type (type_scenario_ref) :: zeffl  ! /zeffl - line averaged effective charge []. Time-dependent.
  type (type_scenario_ref) :: nbar  ! /nbar - line averaged electron density [m^-3]. Time-dependent.
  type (type_scenario_ref) :: xecrh  ! /xecrh - position of maximum  (normalized rho coordinate) of electron cyclotron resonnance heating power []. Time-dependent.
  type (type_scenario_ref) :: pol_flux  ! /pol_flux - separatrix poloidal flux [Wb]. Time-dependent.
  type (type_scenario_ref) :: enhancement  ! /enhancement - energy enhancement factor []. Time-dependent.
  type (type_scenario_ref) :: isotopic  ! /isotopic - ratio between tritium  and deuterium density (for burning plasma)  []. Time-dependent.
  type (type_scenario_ref) :: nbi_td_ratio  ! /nbi_td_ratio - ratio between tritium  and deuterium power in neutral beam injection  []. Time-dependent.
endtype

type type_scenario_sol  !    SOL characteristic  (@ LCMS)
  type (type_scenario_ref) :: l_te_sol  ! /l_te_sol - electron temperature radial decay length [m]. Time-dependent.
  type (type_scenario_ref) :: l_ti_sol  ! /l_ti_sol - ion temperature  radial decay length [m]. Time-dependent.
  type (type_scenario_ref) :: l_ne_sol  ! /l_ne_sol - electron density radial decay length [m]. Time-dependent.
  type (type_scenario_ref) :: l_ni_sol  ! /l_ni_sol - ion density  radial decay length [m]. Time-dependent.
  type (type_scenario_ref) :: l_qe_sol  ! /l_qe_sol - electron heat flux radial decay length [m]. Time-dependent.
  type (type_scenario_ref) :: l_qi_sol  ! /l_qi_sol - ion  heat flux radial decay length [m]. Time-dependent.
  type (type_scenario_ref) :: p_rad_sol  ! /p_rad_sol - radiative power of the SOL [W]. Time-dependent.
  real(DP),pointer  :: gaz_puff(:) => null()     ! /gaz_puff - gaz puff flux for each ion species [s^-1]. Time-dependent.
endtype

type type_scenario_pedestal  !    Values at the top of the H-mode pedestal
  type (type_scenario_ref) :: te_ped  ! /te_ped - pedestal electron temperature [eV]. Time-dependent.
  type (type_scenario_ref) :: ti_ped  ! /ti_ped - pedestal ion temperature [eV]. Time-dependent.
  type (type_scenario_ref) :: ne_ped  ! /ne_ped - pedestal electron density [m^-3]. Time-dependent.
  type (type_scenario_ref) :: ni_ped  ! /ni_ped - pedestal ion density [m^-3]. Time-dependent.
  type (type_scenario_ref) :: psi_ped  ! /psi_ped - pedestal poloidal flux [Wb]. Time-dependent.
  type (type_scenario_ref) :: phi_ped  ! /phi_ped - pedestal toroidal flux [Wb]. Time-dependent.
  type (type_scenario_ref) :: rho_ped  ! /rho_ped - top pedestal value of internal simulator coordinate [m]. Time-dependent.
  type (type_scenario_ref) :: q_ped  ! /q_ped - top pedestal safety factor value []. Time-dependent.
  type (type_scenario_ref) :: pressure_ped  ! /pressure_ped - top pedestal thermal pressure (n_e * T_e  + n_i * T_i) [Pa]. Time-dependent.
  type (type_scenario_ref) :: vtor_ped  ! /vtor_ped - top pedestal value of rotation velocity of selected impurity [m/s]. Time-dependent.
endtype

type type_scenario_ninety_five  !    values at 95% of poloidal flux
  type (type_scenario_ref) :: q_95  ! /q_95 - safety factor value  @ 95 % of poloidal flux span []. Time-dependent.
  type (type_scenario_ref) :: elong_95  ! /elong_95 - plasma elongation  @ 95 % of poloidal flux span []. Time-dependent.
  type (type_scenario_ref) :: tria_95  ! /tria_95 - averaged plasma triangularity  @ 95 % of poloidal flux span []. Time-dependent.
  type (type_scenario_ref) :: tria_up_95  ! /tria_up_95 - upper plasma triangularity  @ 95 % of poloidal flux span []. Time-dependent.
  type (type_scenario_ref) :: tria_lo_95  ! /tria_lo_95 - lower plasma triangularity  @ 95 % of poloidal flux span []. Time-dependent.
  type (type_scenario_ref) :: te_95  ! /te_95 - electron temperature  @ 95 % of poloidal flux [eV]. Time-dependent.
  type (type_scenario_ref) :: ti_95  ! /ti_95 - ion temperature  @ 95 % of poloidal flux [eV]. Time-dependent.
  type (type_scenario_ref) :: ne_95  ! /ne_95 - electron density  @ 95 % of poloidal flux [m^-3]. Time-dependent.
  type (type_scenario_ref) :: ni_95  ! /ni_95 - ion density  @ 95 % of poloidal flux [m^-3]. Time-dependent.
  type (type_scenario_ref) :: phi_95  ! /phi_95 - toroidal flux  @ 95 % of poloidal flux [Wb]. Time-dependent.
  type (type_scenario_ref) :: rho_95  ! /rho_95 -  value of internal simulator coordinate  @ 95 % of poloidal flux [m]. Time-dependent.
  type (type_scenario_ref) :: vtor_95  ! /vtor_95 - rotation velocity of selected impurity   @ 95 % of poloidal flux [m/s]. Time-dependent.
endtype

type type_scenario_neutron  !    neutron flux for DD and DT reactions
  type (type_scenario_ref) :: ndd_tot  ! /ndd_tot - total neutron flux coming  from DD reactions [Hz]. Time-dependent.
  type (type_scenario_ref) :: ndd_th  ! /ndd_th - neutron flux coming  from thermal  DD reactions [Hz]. Time-dependent.
  type (type_scenario_ref) :: ndd_nbi_th  ! /ndd_nbi_th - neutron flux coming  from beam/plasma  DD reactions [Hz]. Time-dependent.
  type (type_scenario_ref) :: ndd_nbi_nbi  ! /ndd_nbi_nbi - neutron flux coming  from beam/beam  DD reactions [Hz]. Time-dependent.
  type (type_scenario_ref) :: ndt_tot  ! /ndt_tot - total neutron flux coming  from DT reactions [Hz]. Time-dependent.
  type (type_scenario_ref) :: ndt_th  ! /ndt_th - neutron flux coming  from thermal DT reactions [Hz]. Time-dependent.
endtype

type type_scenario_line_ave  !    line averaged value
  type (type_scenario_ref) :: ne_line  ! /ne_line - line averaged electron density [m^-3]. Time-dependent.
  type (type_scenario_ref) :: zeff_line  ! /zeff_line - line averaged effective charge. Time-dependent.
  type (type_scenario_ref) :: ne_zeff_line  ! /ne_zeff_line - line averaged electron density * Zeff . Time-dependent.
  type (type_scenario_ref) :: dne_line_dt  ! /dne_line_dt - time derivative of line averaged electron density [m^-3/s]. Time-dependent.
endtype

type type_scenario_lim_div_wall  !    values on the plate of divertor or on the limitor or on the wall (@ LCMS)
  type (type_scenario_ref) :: te_lim_div  ! /te_lim_div - limiter/divertor electron temperature [eV]. Time-dependent.
  type (type_scenario_ref) :: ti_lim_div  ! /ti_lim_div - limiter/divertor ion temperature [eV]. Time-dependent.
  type (type_scenario_ref) :: ne_lim_div  ! /ne_lim_div - limiter/divertor electron density [m^-3]. Time-dependent.
  type (type_scenario_ref) :: ni_lim_div  ! /ni_lim_div - limiter/divertor ion density [m^-3]. Time-dependent.
  type (type_scenario_ref) :: p_peak_div  ! /p_peak_div - peak power on  divertor [W]. Time-dependent.
  type (type_scenario_ref) :: surf_temp  ! /surf_temp - limiter surface or divertor plate temperature [K]. Time-dependent.
  type (type_scenario_ref) :: p_lim_div  ! /p_lim_div - Power flux on limiter or divertor plate [W]. Time-dependent.
  type (type_scenario_ref) :: p_rad_div  ! /p_rad_div - radiative power in the divertor zone [W]. Time-dependent.
  type (type_scenario_ref) :: wall_temp  ! /wall_temp - wall temperature [K]. Time-dependent.
  type (type_scenario_ref) :: wall_state  ! /wall_state - saturation state of the wall (0 = completly pumping wall, 1 = competely saturate wall) []. Time-dependent.
  type (type_scenario_ref) :: detach_state  ! /detach_state - plasma detachement state (0= attach plasma, 1 = completely detach plasma) []. Time-dependent.
  real(DP),pointer  :: pump_flux(:) => null()     ! /pump_flux - flux pump out for each ion species [s^-1]. Time-dependent.
endtype

type type_scenario_itb  !    Values characteristics of the Internal Transport Barrier
  type (type_scenario_ref) :: q_min  ! /q_min - minimal value of safety factor []. Time-dependent.
  type (type_scenario_ref) :: te_itb  ! /te_itb - electron temperature @ q = q_min [eV]. Time-dependent.
  type (type_scenario_ref) :: ti_itb  ! /ti_itb - ion temperature @ q = q_min [eV]. Time-dependent.
  type (type_scenario_ref) :: ne_itb  ! /ne_itb - electron density  @ q = q_min [m^-3]. Time-dependent.
  type (type_scenario_ref) :: ni_itb  ! /ni_itb - ion density  @ q = q_min [m^-3]. Time-dependent.
  type (type_scenario_ref) :: psi_itb  ! /psi_itb - poloidal flux @ q = q_min [Wb]. Time-dependent.
  type (type_scenario_ref) :: phi_itb  ! /phi_itb - toroidal flux @ q = q_min [Wb]. Time-dependent.
  type (type_scenario_ref) :: rho_itb  ! /rho_itb - value of internal simulator coordinate @ q = q_min [m]. Time-dependent.
  type (type_scenario_ref) :: h_itb  ! /h_itb - energy enhancement ITB factor [m]. Time-dependent.
  type (type_scenario_ref) :: width_itb  ! /width_itb - width of the high pressure gradient region (on scale of rho_itb) [m]. Time-dependent.
  type (type_scenario_ref) :: vtor_itb  ! /vtor_itb - rotation velocity of selected impurity @ rho_itb [m/s]. Time-dependent.
  type (type_scenario_int) :: itb_type  ! /itb_type - itb type []. Time-dependent. Any combinaison of :0 = none; 1 = on T_i; 2 = on T_e; 4  = on n_e; 8 = reverse shear triggered; 16 = 
endtype

type type_scenario_heat_power  !    Power delivred to plasma (thermal an non thermal)
  type (type_scenario_ref) :: plh  ! /plh - Lower hybrid power [W]. Time-dependent.
  type (type_scenario_ref) :: pohmic  ! /pohmic - ohmic power (thermal species contribution only) [W]. Time-dependent.
  type (type_scenario_ref) :: picrh  ! /picrh - Ion cyclotron resonnance heating power [W]. Time-dependent.
  type (type_scenario_ref) :: pecrh  ! /pecrh - electron cyclotron resonnance heating power [W]. Time-dependent.
  type (type_scenario_ref) :: pnbi  ! /pnbi - neutral beam injection power [W]. Time-dependent.
  type (type_scenario_ref) :: pnbi_co_cur  ! /pnbi_co_cur - neutral beam injection power injeted in co-current direction [W]. Time-dependent.
  type (type_scenario_ref) :: pnbi_counter  ! /pnbi_counter - neutral beam injection power injeted in counter-current direction [W]. Time-dependent.
  type (type_scenario_ref) :: plh_th  ! /plh_th - lower hybrid power deposited on thermal electrons [W]. Time-dependent.
  type (type_scenario_ref) :: picrh_th  ! /picrh_th - ion cyclotron resonnance heating power deposited on thermal species [W]. Time-dependent.
  type (type_scenario_ref) :: pecrh_th  ! /pecrh_th - electron cyclotron resonnance heating power deposited on thermal electrons [W]. Time-dependent.
  type (type_scenario_ref) :: pnbi_th  ! /pnbi_th - neutral beam injection power deposited on thermal species [W]. Time-dependent.
  type (type_scenario_ref) :: ploss_icrh  ! /ploss_icrh - Ion cyclotron resonnance heating power losses [W]. Time-dependent.
  type (type_scenario_ref) :: ploss_nbi  ! /ploss_nbi - neutral beam injection power losses (including shine-through) [W]. Time-dependent.
  type (type_scenario_ref) :: pbrem  ! /pbrem - Bremsstrahlung radition losses [W]. Time-dependent.
  type (type_scenario_ref) :: pcyclo  ! /pcyclo - cyclotron radiation losses [W]. Time-dependent.
  type (type_scenario_ref) :: prad  ! /prad - impurity radition losses in core plamsa , without Bremsstrahlung [W]. Time-dependent.
  type (type_scenario_ref) :: pdd_fus  ! /pdd_fus - fusion power due to DD reactions [W]. Time-dependent.
  type (type_scenario_ref) :: pei  ! /pei - power exchange between eletron and ion (equipartition) [W]. Time-dependent.
  type (type_scenario_ref) :: pel_tot  ! /pel_tot - total thermal electron power deposition without equipartition [W]. Time-dependent.
  type (type_scenario_ref) :: pel_fus  ! /pel_fus - fusion electron power deposition [W]. Time-dependent.
  type (type_scenario_ref) :: pel_icrh  ! /pel_icrh - ICRH  electron power deposition [W]. Time-dependent.
  type (type_scenario_ref) :: pel_nbi  ! /pel_nbi - NBI electron power deposition [W]. Time-dependent.
  type (type_scenario_ref) :: pfus_dt  ! /pfus_dt - total D-T fusion power of alpha [W]. Time-dependent.
  type (type_scenario_ref) :: ploss_fus  ! /ploss_fus - D-T fusion power of alpha losses  [W]. Time-dependent.
  type (type_scenario_ref) :: pfus_nbi  ! /pfus_nbi - NBI induce D-T fusion power of alpha  [W]. Time-dependent.
  type (type_scenario_ref) :: pfus_th  ! /pfus_th - alpha (from DT fusion reaction)  power deposited on thermal species [W]. Time-dependent.
  type (type_scenario_ref) :: padd_tot  ! /padd_tot - total additional power input including ohmic power  [W]. Time-dependent.
  type (type_scenario_ref) :: pion_tot  ! /pion_tot - total thermal ion power deposition without equipartition [W]. Time-dependent.
  type (type_scenario_ref) :: pion_fus  ! /pion_fus - fusion ion power deposition [W]. Time-dependent.
  type (type_scenario_ref) :: pion_icrh  ! /pion_icrh - ICRH  ion power deposition [W]. Time-dependent.
  type (type_scenario_ref) :: pion_nbi  ! /pion_nbi - NBI  ion power deposition [W]. Time-dependent.
  type (type_scenario_ref) :: pioniz  ! /pioniz - power losses due to cold neutral ionization [W]. Time-dependent.
  type (type_scenario_ref) :: ploss  ! /ploss - plasma losses power, as define in ITER basis [W]. Time-dependent.
  type (type_scenario_ref) :: p_wth  ! /p_wth - thermal power input, define as tau_E * P_th = Wth [W]. Time-dependent.
  type (type_scenario_ref) :: p_w  ! /p_w - effective power define as tau_E  * P_w = W_tot [W]. Time-dependent.
  type (type_scenario_ref) :: p_l2h_thr  ! /p_l2h_thr - additionnal power crossing the LCMS; must be compare to  L->H threshold power (Ryter PPCF 2002) [W]. Time-dependent.
  type (type_scenario_ref) :: p_l2h_sc  ! /p_l2h_sc - threshold power given by the choosen scaling law for transition from L-mode to H-mode  [W]. Time-dependent.
  type (type_scenario_ref) :: p_nbi_icrh  ! /p_nbi_icrh - beam power increase due to  ICRH effects  [W]. Time-dependent.
endtype

type type_scenario_global  !     global scalar value 
  type (type_scenario_ref) :: ip  ! /ip - Plasma current [A]. Time-dependent.
  type (type_scenario_ref) :: dip_dt  ! /dip_dt - time derivative of plasma current [A/s]. Time-dependent.
  type (type_scenario_ref) :: beta_pol  ! /beta_pol - poloidal beta []. Time-dependent.
  type (type_scenario_ref) :: beta_tor  ! /beta_tor - toroidal beta []. Time-dependent.
  type (type_scenario_ref) :: beta_normal  ! /beta_normal - normalised beta []. Time-dependent.
  type (type_scenario_ref) :: li  ! /li - internal inductance (definition 3). Time-dependent.
  type (type_scenario_ref) :: volume  ! /volume - total plasma volume [m^3]. Time-dependent.
  type (type_scenario_ref) :: area_pol  ! /area_pol - area poloidal cross section [m^2]. Time-dependent.
  type (type_scenario_ref) :: area_ext  ! /area_ext - external plasma surface [m^2]. Time-dependent.
  type (type_scenario_ref) :: len_sepa  ! /len_sepa - length of the separatrix [m]. Time-dependent.
  type (type_scenario_ref) :: beta_pol_th  ! /beta_pol_th - poloidal beta, thermal contribution []. Time-dependent.
  type (type_scenario_ref) :: beta_tor_th  ! /beta_tor_th - toroidal beta, thermal contribution []. Time-dependent.
  type (type_scenario_ref) :: beta_n_th  ! /beta_n_th - normalised beta, thermal contribution []. Time-dependent.
  type (type_scenario_ref) :: disruption  ! /disruption - flag for disruption (set to 1 for disruption, oterwise equal 0) []. Time-dependent.
  type (type_scenario_ref) :: mode_h  ! /mode_h - confinement mode verus time:  0 = L-mode et 1 = H-mode []. Time-dependent.
  type (type_scenario_ref) :: s_alpha  ! /s_alpha - total number of alpha fusion  particules from D-T ractions  per second [s^-1]. Time-dependent.
endtype

type type_scenario_energy  !    plasma energy content
  type (type_scenario_ref) :: w_tot  ! /w_tot - total plasma energy [J]. Time-dependent.
  type (type_scenario_ref) :: w_b_pol  ! /w_b_pol - poloidal field energy of  the plasma [J]. Time-dependent.
  type (type_scenario_ref) :: w_dia  ! /w_dia - 3/2 perpendicular plasma energy [J]. Time-dependent.
  type (type_scenario_ref) :: dwdia_dt  ! /dwdia_dt - time derivative of Wdia [W]. Time-dependent.
  type (type_scenario_ref) :: w_b_tor_pla  ! /w_b_tor_pla - toroidal magnetic plasma energy  [J]. Time-dependent.
  type (type_scenario_ref) :: w_th  ! /w_th - thermal plasma energy [J]. Time-dependent.
  type (type_scenario_ref) :: dwtot_dt  ! /dwtot_dt - time derivative of total plasma energy [W]. Time-dependent.
  type (type_scenario_ref) :: dwbpol_dt  ! /dwbpol_dt - time derivative of plasma poloidal field energy [W]. Time-dependent.
  type (type_scenario_ref) :: dwbtorpla_dt  ! /dwbtorpla_dt - time derivative of toroidal magnetic plasma energy  [W]. Time-dependent.
  type (type_scenario_ref) :: dwth_dt  ! /dwth_dt - time derivative of thermal plasma energy [W]. Time-dependent.
  type (type_scenario_ref) :: esup_icrhtot  ! /esup_icrhtot - total suprathermal energy of fast ions accelerated  by ICRH [J]. Time-dependent.
  type (type_scenario_ref) :: esup_icrhper  ! /esup_icrhper - perpendicular part of suprathermal energy of fast ions accelerated  by ICRH [J]. Time-dependent.
  type (type_scenario_ref) :: esup_nbitot  ! /esup_nbitot - total suprathermal energy of fast ions from NBI ionisation [J]. Time-dependent.
  type (type_scenario_ref) :: esup_nbiperp  ! /esup_nbiperp - perpendicular part of suprathermal energy of fast ions from NBI ionisation [J]. Time-dependent.
  type (type_scenario_ref) :: esup_lhcd  ! /esup_lhcd - total suprathermal energy of fast electron from LHCD [J]. Time-dependent.
  type (type_scenario_ref) :: esup_alpha  ! /esup_alpha - total suprathermal energy of fast alpha particules [J]. Time-dependent.
endtype

type type_scenario_edge  !    edge value (@ LCMS)
  type (type_scenario_ref) :: te_edge  ! /te_edge - edge electron temperature [eV]. Time-dependent.
  type (type_scenario_ref) :: ti_edge  ! /ti_edge - edge ion temperature [eV]. Time-dependent.
  type (type_scenario_ref) :: ne_edge  ! /ne_edge - edge electron density [m^-3]. Time-dependent.
  type (type_scenario_ref) :: ni_edge  ! /ni_edge - edge ion density [m^-3]. Time-dependent.
  type (type_scenario_ref) :: psi_edge  ! /psi_edge - edge  poloidal flux [Wb]. Time-dependent.
  type (type_scenario_ref) :: phi_edge  ! /phi_edge - edge  toroidal flux [Wb]. Time-dependent.
  type (type_scenario_ref) :: rho_edge  ! /rho_edge - edge value of internal simulator coordinate [m]. Time-dependent.
  type (type_scenario_ref) :: drho_edge_dt  ! /drho_edge_dt - time derivative of edge value of internal simulator coordinate [m/s]. Time-dependent.
  type (type_scenario_ref) :: q_edge  ! /q_edge - edge or effective  safety factor value []. Time-dependent.
  type (type_scenario_ref) :: neutral_flux  ! /neutral_flux - number of cold neutral (in equivalent electron for Z >1) that input in  plasma at the edge every second coming from recycling and 
  type (type_scenario_ref) :: phi_plasma  ! /phi_plasma - contribution of the plasma to the toroidal flux (used for toroidal coils heat load computation) [Wb]. Time-dependent.
  type (type_scenario_ref) :: vtor_edge  ! /vtor_edge - rotation velocity of selected impurity on the separatrix [m/s]. Time-dependent.
endtype

type type_scenario_currents  !    data related to current sources and current diffusion
  type (type_scenario_ref) :: RR  ! /RR - plasma resistivity [ohm]. Time-dependent.
  type (type_scenario_ref) :: i_align  ! /i_align - current drive alignment quality parameter (1 = good , 0 = bad). Time-dependent.
  type (type_scenario_ref) :: i_boot  ! /i_boot - bootstrap current [A]. Time-dependent.
  type (type_scenario_ref) :: i_cd_tot  ! /i_cd_tot - total current drive [A]. Time-dependent.
  type (type_scenario_ref) :: i_eccd  ! /i_eccd - Electron Cyclotron current drive [A]. Time-dependent.
  type (type_scenario_ref) :: i_fast_ion  ! /i_fast_ion - fast ions bootstrap like current drive  (i.e. fast alpha) [A]. Time-dependent.
  type (type_scenario_ref) :: i_fwcd  ! /i_fwcd - Fast Wave current drive [A]. Time-dependent.
  type (type_scenario_ref) :: i_lhcd  ! /i_lhcd - Lower Hybrid current drive [A]. Time-dependent.
  type (type_scenario_ref) :: i_nbicd  ! /i_nbicd - Neutral Beam Injection current drive  [A]. Time-dependent.
  type (type_scenario_ref) :: i_ni_tot  ! /i_ni_tot - total non inductive current  [A]. Time-dependent.
  type (type_scenario_ref) :: i_ohm  ! /i_ohm - ohmic current  [A]. Time-dependent.
  type (type_scenario_ref) :: i_par  ! /i_par - total plasma current (projected on B : <J.B>/B0)   [A]. Time-dependent.
  type (type_scenario_ref) :: i_runaway  ! /i_runaway - runaway current  [A]. Time-dependent.
  type (type_scenario_ref) :: v_loop  ! /v_loop - loop voltage @ LCMS / LFS , equatorial point  [V]. Time-dependent.
  type (type_scenario_ref) :: v_meas  ! /v_meas - loop voltage measured on a  coil   [V]. Time-dependent.
endtype

type type_scenario_confinement  !    characteristic confinement times
  type (type_scenario_ref) :: tau_e  ! /tau_e - thermal energy confinement time [s]. Time-dependent.
  type (type_scenario_ref) :: tau_l_sc  ! /tau_l_sc - confinement time given by the selected L-mode scaling law [s]. Time-dependent.
  type (type_scenario_ref) :: tau_h_sc  ! /tau_h_sc - confinement time given by the selected H-mode scaling law [s]. Time-dependent.
  type (type_scenario_ref) :: tau_he  ! /tau_he - Helium ashes confinement time [s]. Time-dependent.
  type (type_scenario_ref) :: tau_e_ee  ! /tau_e_ee - electron energy confimenent time [s]. Time-dependent.
  type (type_scenario_ref) :: tau_e_ii  ! /tau_e_ii - ion energy confinement time [s]. Time-dependent.
  type (type_scenario_ref) :: tau_e_ei  ! /tau_e_ei - energy equipartition characteristic time [s]. Time-dependent.
  type (type_scenario_ref) :: tau_cur_diff  ! /tau_cur_diff - characteristic time for current diffusion  [s]. Time-dependent.
  type (type_scenario_ref) :: tau_i_rol  ! /tau_i_rol - characteristic time for current decrease in tokamak equivalent R/L circuit [s]. Time-dependent.
endtype

type type_scenario_configuration  !    Strings describing the tokamak configuration
  type (type_scenario_int) :: config  ! /config - plasma configuration (limiter/divertor ...) []. Time-dependent. Possible values : 0 = undetermined; 1 = poloidal limiter (ring); 2
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
  type (type_scenario_ref) :: ecrh_freq  ! /ecrh_freq - ECRH frequency [Hz]. Time-dependent.
  type (type_scenario_ref) :: ecrh_loc  ! /ecrh_loc - position of maximum ECRH deposition on scale of rho [rho]. Time-dependent.
  type (type_scenario_int) :: ecrh_mode  ! /ecrh_mode - polarisation of ecrh wave (0 = O mode, 1 = X mode) []. Time-dependent.
  type (type_scenario_ref) :: ecrh_tor_ang  ! /ecrh_tor_ang - toroidal angle of ECRH at resonance  [rad] Time-dependent.
  type (type_scenario_ref) :: ecrh_pol_ang  ! /ecrh_pol_ang - poloidal angle of ECRH resonance positon (0= LFS, pi/2 = top, -pi/2 = down, pi = HFS)  [rad]. Time-dependent.
  type (type_scenario_int) :: ecrh_harm  ! /ecrh_harm - harmonic number of the apsorbed ecrh wave []. Time-dependent.
  type (type_scenario_ref) :: enbi  ! /enbi - energy of the neutral beam [eV]. Time-dependent.
  type (type_scenario_ref) :: r_nbi  ! /r_nbi - Major radius of tengance of NBI [m]. Time-dependent.
  type (type_scenario_int) :: grad_b_drift  ! /grad_b_drift - direction of ion grad-B drift (1= to lower divertor, -1 = from lower divertor)  []. Time-dependent.
  type (type_scenario_ref) :: icrh_freq  ! /icrh_freq - ICRH frequency [Hz]. Time-dependent.
  character(len=132), dimension(:), pointer ::icrh_scheme => null()       ! /icrh_scheme - icrh scheme either : H_min_1; He3_min; T_harm_2; FW; FW_CD; FW_CCD
  type (type_scenario_ref) :: icrh_phase  ! /icrh_phase - ICRH antenna phasing [rad]. Time-dependent.
  type (type_scenario_ref) :: LH_freq  ! /LH_freq - LHCD frequency [Hz]. Time-dependent.
  type (type_scenario_ref) :: LH_npar  ! /LH_npar - LHCD parallel indice []. Time-dependent.
  type (type_scenario_ref) :: pellet_ang  ! /pellet_ang - pellet injection positon (0= LFS, pi/2 = top, -pi/2 = down, pi = HFS)  [rad]. Time-dependent.
  type (type_scenario_ref) :: pellet_v  ! /pellet_v - pellet injection velocity [m/s]. Time-dependent.
  type (type_scenario_ref) :: pellet_nba  ! /pellet_nba - initial number of atoms in pellet  []. Time-dependent.
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
  type (type_scenario_ref) :: te0  ! /te0 - central electron temperature [eV]. Time-dependent.
  type (type_scenario_ref) :: ti0  ! /ti0 - central ion temperature [eV]. Time-dependent.
  type (type_scenario_ref) :: ne0  ! /ne0 - central electron density [m^-3]. Time-dependent.
  type (type_scenario_ref) :: ni0  ! /ni0 - central ion density [m^-3]. Time-dependent.
  type (type_scenario_ref) :: shift0  ! /shift0 - central value of Shafranov shift [m]. Time-dependent.
  type (type_scenario_ref) :: psi0  ! /psi0 - pedestal poloidal flux [Wb]. Time-dependent.
  type (type_scenario_ref) :: phi0  ! /phi0 - central toroidal flux [Wb]. Time-dependent.
  type (type_scenario_ref) :: q0  ! /q0 - central safety factor value []. Time-dependent.
  type (type_scenario_ref) :: Rmag  ! /Rmag - radius of magnetic axis [R]. Time-dependent.
  type (type_scenario_ref) :: Zmag  ! /Zmag - Z coordinate of magnetic axis [R]. Time-dependent.
  type (type_scenario_ref) :: vtor_0  ! /vtor_0 - central rotation velocity of selected impurity [m/s]. Time-dependent.
endtype

type type_scenario  !    
  type (type_datainfo) :: datainfo  ! /scenario/datainfo - 
  type (type_scenario_centre) :: centre  ! /scenario/centre - central values of the profiles (at magnetic axis)
  type (type_scenario_composition) :: composition  ! /scenario/composition - Plasma composition (description of ion species).
  type (type_scenario_configuration) :: configs  ! /scenario/configs - Strings describing the tokamak configuration
  type (type_scenario_confinement) :: confinement  ! /scenario/confinement - characteristic confinement times
  type (type_scenario_currents) :: currents  ! /scenario/currents - data related to current sources and current diffusion
  type (type_scenario_edge) :: edge  ! /scenario/edge - edge value (@ LCMS)
  type (type_scenario_energy) :: energy  ! /scenario/energy - plasma energy content
  type (type_eqgeometry) :: eqgeometry  ! /scenario/eqgeometry - 
  type (type_scenario_global) :: global_param  ! /scenario/global_param - Global scalar values
  type (type_scenario_heat_power) :: heat_power  ! /scenario/heat_power - Power delivred to plasma (thermal and non thermal)
  type (type_scenario_itb) :: itb  ! /scenario/itb - Values characteristics of the Internal Transport Barrier
  type (type_scenario_lim_div_wall) :: lim_div_wall  ! /scenario/lim_div_wall - values on the plate of divertor or on the limitor or on the wall (@ LCMS)
  type (type_scenario_line_ave) :: line_ave  ! /scenario/line_ave - line averaged value
  type (type_scenario_neutron) :: neutron  ! /scenario/neutron - neutron flux for DD and DT reactions
  type (type_scenario_ninety_five) :: ninety_five  ! /scenario/ninety_five - values at 95% of poloidal flux
  type (type_scenario_pedestal) :: pedestal  ! /scenario/pedestal - Values at the top of the H-mode pedestal
  type (type_scenario_references) :: references  ! /scenario/references - References
  type (type_scenario_reactor) :: reactor  ! /scenario/reactor - reactor data (such as electricity cost ...)
  type (type_scenario_sol) :: sol  ! /scenario/sol - SOL characteristic  (@ LCMS)
  type (type_scenario_vol_ave) :: vol_ave  ! /scenario/vol_ave - volume averaged value
  type (type_codeparam) :: codeparam  ! /scenario/codeparam - 
  real(DP)  :: time=-9.0D40       ! /scenario/time - Time [s]; Time-dependent; Scalar
endtype

! ***********  Include summary.xsd
type type_summary  !    
  type (type_datainfo) :: datainfo  ! /summary/datainfo - 
  type (type_reduced) :: ip  ! /summary/ip - Plasma current [A]
  type (type_reduced) :: bvac_r  ! /summary/bvac_r - Vacuum field times radius in the toroidal field magnet [T.m];
  type (type_reduced) :: geom_axis_r  ! /summary/geom_axis_r - Major radius of the geometric axis [m]
  type (type_reduced) :: a_minor  ! /summary/a_minor - Minor radius of the plasma boundary [m]
  type (type_reduced) :: elongation  ! /summary/elongation - Elongation of the plasma boundary [m]
  type (type_reduced) :: tria_lower  ! /summary/tria_lower - Lower triangularity of the plasma boundary [m]
  type (type_reduced) :: tria_upper  ! /summary/tria_upper - Upper triangularity of the plasma boundary [m]
  type (type_reduced) :: tev  ! /summary/tev - volume averaged electron temperature [eV]
  type (type_reduced) :: tiv  ! /summary/tiv - volume averaged ion temperature [eV]
  type (type_reduced) :: nev  ! /summary/nev - volume averaged electron density [m^-3]
  type (type_reduced) :: zeffv  ! /summary/zeffv - volume averaged effective charge
  type (type_reduced) :: beta_pol  ! /summary/beta_pol - poloidal beta
  type (type_reduced) :: beta_tor  ! /summary/beta_tor - toroidal beta
  type (type_reduced) :: beta_normal  ! /summary/beta_normal - normalised beta
  type (type_reduced) :: li  ! /summary/li - internal inductance
  type (type_reduced) :: volume  ! /summary/volume - total plasma volume [m^3]
  type (type_reduced) :: area  ! /summary/area - area poloidal cross section [m^2]
  type (type_reduced) :: main_ion1_z  ! /summary/main_ion1_z - Atomic number of the main ion #1 [a.m.u.]
  type (type_reduced) :: main_ion1_a  ! /summary/main_ion1_a - Atomic mass of the main ion #1 [a.m.u.]
  type (type_reduced) :: main_ion2_z  ! /summary/main_ion2_z - Atomic number of the main ion #2 [a.m.u.]
  type (type_reduced) :: main_ion2_a  ! /summary/main_ion2_a - Atomic mass of the main ion #2 [a.m.u.]
  type (type_reduced) :: impur1_z  ! /summary/impur1_z - Atomic number of the impurity #1 [a.m.u.]
  type (type_reduced) :: impur1_a  ! /summary/impur1_a - Atomic mass of the impurity #1 [a.m.u.]
  real(DP)  :: time=-9.0D40       ! /summary/time - Time at which the 0D variables of the summary are taken [s]. Scalar
endtype

! ***********  Include vessel.xsd
type type_vessel  !    
  type (type_datainfo) :: datainfo  ! /vessel/datainfo - 
  type (type_rz1D) :: position  ! /vessel/position - Vessel wall outline (list of R,Z co-ordinates) [m]; Vector (npoints)
endtype

! ***********  Include waves.xsd
type type_theta_info  !    
  integer  :: angl_type=-999999999       ! /theta_info/angl_type - Type of poloidal angle: 1 : same as the poloidal angle in the equlibrium cpo; 2 : normal (geometrical) polar angle; 3 : other. If 
  real(DP),pointer  :: th2th_pol(:,:) => null()     ! /theta_info/th2th_pol - Polar (geometrical) poloidal angle at grid points in theta, i.e. the transformation from theta to the polar poloidal angle; used o
endtype

type type_waves_global_param  !    Global wave deposition parameters
  real(DP)  :: frequency=-9.0D40       ! /frequency - Wave frequency [Hz]; Time-dependent, floating
  character(len=132), dimension(:), pointer ::name => null()       ! /name - Antenna name, String
  character(len=132), dimension(:), pointer ::type => null()       ! /type - Wave type (LH, EC, IC, ...), String
  integer,pointer  :: ntor(:) => null()      ! /ntor - Toroidal mode numbers; Time-dependent; Vector (ntor)
  integer,pointer  :: f_assumption(:) => null()      ! /f_assumption - Assumption on the functions distribution used by the wave solver to calculate the power deposition : 0 = Maxwellian (linear absorp
  real(DP)  :: power_tot=-9.0D40       ! /power_tot - Total absorbed wave power [W]; Time-dependent
  real(DP),pointer  :: p_frac_ntor(:) => null()     ! /p_frac_ntor - Fraction of wave power per toroidal mode number; Time-dependent; Vector (ntor)
  real(DP),pointer  :: pow_i(:) => null()     ! /pow_i - Wave power absorbed by an ion species [W]; Time-dependent; Vector (nion)
  real(DP)  :: pow_e=-9.0D40       ! /pow_e - Wave power absorbed by the electrons [W]; Time-dependent; Float
  real(DP),pointer  :: pow_ntor_i(:,:) => null()     ! /pow_ntor_i - Wave power absorbed by an ion species per toroidal mode number [W]; Time-dependent; Matrix (ntor,nion)
  real(DP),pointer  :: pow_ntor_e(:) => null()     ! /pow_ntor_e - Wave power absorbed by the electrons per toroidal mode number [W]; Time-dependent; Vector (ntor)
  real(DP)  :: cur_tor=-9.0D40       ! /cur_tor - Wave driven toroidal current from a stand alone calculation (not consistent with other sources) [A]; Time-dependent, Float
  real(DP),pointer  :: cur_tor_ntor(:) => null()     ! /cur_tor_ntor - Wave driven toroidal current for each toroidal mode number from a stand alone calculation (not consistent with other sources)  [A]
  integer  :: code_type=-999999999       ! /code_type - Type of wave deposition code for a given frequency: 1=beam/ray tracing; 2=full wave; Integer
  type (type_b0r0) :: toroid_field  ! /toroid_field - Characteristics of the vacuum toroidal field, redundant with the toroidfield CPO, to document the normalisation of parallel curren
endtype

type type_waves_grid_1d  !    Grid points for profiles
  real(DP),pointer  :: rho_tor_norm(:) => null()     ! /rho_tor_norm - Normalised toroidal flux coordinate at the grid points for 1D profiles; Time-dependent; Vector (npsi)
  real(DP),pointer  :: rho_tor(:) => null()     ! /rho_tor - Toroidal flux coordinate at the grid points for 1D profiles [m]; Time-dependent; Vector (npsi)
  real(DP),pointer  :: psi(:) => null()     ! /psi - Grid points in poloidal flux function [Wb], without 1/2pi and such that Bp=|grad psi| /R/2/pi. Time-dependent; Vector (npsi)
endtype

type type_waves_grid_2d  !    Grid points for 2D profiles
  integer  :: grid_type=-999999999       ! /grid_type - Grid type. 1: rectangular grid in R,Z. 2: rectnagular grid in psi, theta. 3: unstructured grid. Integer.
  real(DP),pointer  :: rho_tor_norm(:,:) => null()     ! /rho_tor_norm - Normalised toroidal flux coordinate at the grid points for 1D and 2D profiles; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: rho_tor(:,:) => null()     ! /rho_tor - Toroidal flux coordinate at the grid points for 1D and 2D profiles [m]; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: psi(:,:) => null()     ! /psi - Grid points in poloidal flux function [Wb], without 1/2pi and such that Bp=|grad psi| /R/2/pi. Time-dependent; Matrix (ndim1, ndim
  real(DP),pointer  :: theta(:,:) => null()     ! /theta - Grid points of the ploidal angle; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: r(:,:) => null()     ! /r - R (major radius) of grid points; Time-dependent; Matrix(ndim1, ndim2)
  real(DP),pointer  :: z(:,:) => null()     ! /z - Z (altitude) of grid points; Time-dependent; Matrix (ndim1, ndim2)
  type (type_theta_info) :: theta_info  ! /theta_info - Information on the poloidal angle theta.
endtype

type type_waves_profiles_1d  !    waves 1D radial profiles
  real(DP),pointer  :: powd_tot(:) => null()     ! /powd_tot - Total flux surface averaged wave power density [W/m^3]; Time-dependent; Vector (npsi)
  real(DP),pointer  :: powd_e(:) => null()     ! /powd_e - Flux surface averaged absorbed wave power density on electrons [W/m^3]; Time-dependent; Vector (npsi)
  real(DP),pointer  :: powd_i(:,:) => null()     ! /powd_i - Flux surface averaged absorbed wave power density on ion species [W/m^3]; Time-dependent; Matrix (npsi, nion)
  real(DP),pointer  :: powd_ntor(:,:) => null()     ! /powd_ntor - Flux surface averaged power density for each toroidal mode number [W/m^3]; Time-dependent; Matrix(npsi, ntor)
  real(DP),pointer  :: powd_ntor_e(:,:) => null()     ! /powd_ntor_e - Flux surface averaged absorbed power density for each toroidal mode number on electrons [W/m^3]; Time-dependent; Matrix (npsi, nto
  real(DP),pointer  :: powd_ntor_i(:,:,:) => null()     ! /powd_ntor_i - Flux surface averaged power density for each toroidal mode number on each ions species [W/m^3]; Time-dependent; Array3D (npsi, nto
  real(DP),pointer  :: curd_tor(:) => null()     ! /curd_tor - Flux surface averaged wave driven toroidal current density = average(jphi/R) / average(1/R) [A/m^2]; Time-dependent; Vector (npsi)
  real(DP),pointer  :: curd_torntor(:,:) => null()     ! /curd_torntor - Flux surface averaged wave driven toroidal current density for each toroidal mode number = average(jphi/R) / average(1/R) [A/m^2];
  real(DP),pointer  :: pow_tot(:) => null()     ! /pow_tot - Volume integrated absorbed wave power density [W]; Time-dependent; Vector (npsi)
  real(DP),pointer  :: pow_e(:) => null()     ! /pow_e - Volume integrated absorbed wave power density on electrons [W]; Time-dependent; Vector (npsi)
  real(DP),pointer  :: pow_i(:,:) => null()     ! /pow_i - Volume integrated absorbed wave power density on ion species [W]; Time-dependent; Matrix (npsi, nion)
  real(DP),pointer  :: pow_ntor(:,:,:) => null()     ! /pow_ntor - Volume integrated power density for each toroidal mode number [W]; Time-dependent; Matrix (npsi, ntor)
  real(DP),pointer  :: pow_ntor_e(:,:) => null()     ! /pow_ntor_e - Volume integrated power density for each toroidal mode number on the electrons  [W]; Time-dependent; Matrix (npsi, ntor)
  real(DP),pointer  :: pow_ntor_i(:,:,:) => null()     ! /pow_ntor_i - Volume integrated power density for each toroidal mode number on each ions species [W]; Time-dependent; Array3D (npsi, ntor, nion)
  real(DP),pointer  :: curd_par(:) => null()     ! /curd_par - Flux surface averaged wave driven parallel current density = average(j.B) / B0, where B0 is in  global_param/toroid_field/b0, from
  real(DP),pointer  :: curd_parntor(:,:) => null()     ! /curd_parntor - Flux surface averaged wave driven parallel current density for each toroidal mode number = average(j.B) / B0, where B0 is in globa
  real(DP),pointer  :: cur_tor(:) => null()     ! /cur_tor - Wave driven toroidal current inside a flux surface from stand alone calculation (not consistent with other sources) [A]; Time-depe
  real(DP),pointer  :: cur_tor_ntor(:,:) => null()     ! /cur_tor_ntor - Wave driven toroidal current inside a flux surface for each toroidal mode number from a stand alone calculation (not consistent wi
endtype

type type_waves_profiles_2d  !    waves 2D profiles in poloidal cross-section
  real(DP),pointer  :: powd_tot(:,:) => null()     ! /powd_tot - Total wave power density; Time-dependent [W/m^3]; Matrix (ndim1, ndim2)
  real(DP),pointer  :: powd_e(:,:) => null()     ! /powd_e - Absorbed wave power density on electrons [W/m^3]; Time-dependent; Matrix (ndim1, ndim2)
  real(DP),pointer  :: powd_i(:,:,:) => null()     ! /powd_i - Absorbed wave power density on ion species [W/m^3]; Time-dependent; Array3D (ndim1, ndim2, nion)
  real(DP),pointer  :: powd_ntor(:,:,:) => null()     ! /powd_ntor - Absorbed power density for each toroidal mode number [W/m^3]; Time-dependent; Array 3D (ndim1, ndim2, ntor)
  real(DP),pointer  :: powd_ntor_e(:,:,:) => null()     ! /powd_ntor_e - Absorbed power density for each toroidal mode number on electrons [W/m^3]; Time-dependent; Array 3D (ndim1, ndim2, ntor)
  real(DP),pointer  :: powd_ntor_i(:,:,:,:) => null()     ! /powd_ntor_i - Absorbed power density for each toroidal mode number on each ions species [W/m^3]; Time-dependent; Array4D (ndim1, ndim2, ntor, ni
  real(DP),pointer  :: powd_iharm(:,:,:,:,:) => null()     ! /powd_iharm - Power density absorbed by an ion species for each toroidal mode numer at a given harmonic cyclotron resonance ; Time-dependent (W/
endtype

type type_waves_rtposition  !    Ray/beam position
  real(DP),pointer  :: r(:,:) => null()     ! /r - Ray/beam major radius location [m]; Time-dependent; Matrix of double precision real (nbeams, max_npoints)
  real(DP),pointer  :: z(:,:) => null()     ! /z - Ray/beam vertical location [m]; Time-dependent; Matrix of double precision real (nbeams, max_npoints)
  real(DP),pointer  :: psi(:,:) => null()     ! /psi - Poloidal magnetic flux coordinate of the ray/beam position [Wb], without 1/2pi and such that Bp=|grad psi| /R/2/pi; Time-dependent
  real(DP),pointer  :: theta(:,:) => null()     ! /theta - Ray/beam poloidal angle location [rad]; Time-dependent; Matrix of double precision real (nbeams, max_npoints). PRECISE THE DEFINIT
  real(DP),pointer  :: phi(:,:) => null()     ! /phi - Ray/beam toroidal angle location [rad]; Time-dependent; Matrix of double precision real (nbeams, max_npoints)
endtype

type type_waves_rtwavevector  !    Ray/beam wave vector
  real(DP),pointer  :: kr(:,:) => null()     ! /kr - Ray/beam wave vector in the major radius direction [m-1], Matrix of double precision real (nbeams, max_npoints). Time-dependent
  real(DP),pointer  :: kz(:,:) => null()     ! /kz - Ray/beam wave vector in the vertical direction [m], Matrix of double precision real (nbeams, max_npoints). Time-dependent
  real(DP),pointer  :: npar(:,:) => null()     ! /npar - Ray/beam parallel refractive index, Matrix of double precision real (nbeams, max_npoints). Time-dependent
  real(DP),pointer  :: nperp(:,:) => null()     ! /nperp - Ray/beam perpendicular refractive index, Matrix of double precision real (nbeams, max_npoints). Time-dependent
  real(DP),pointer  :: ntor(:,:) => null()     ! /ntor - Ray/beam toroidal wave number, Matrix of double precision real (nbeams, max_npoints/1). If var_ntor=0, ntor is constant along the 
  integer  :: var_ntor=-999999999       ! /var_ntor - Flag telling whether ntor is constant along the ray path (0) or varying (1). Integer
endtype

type type_polarization  !    
  real(DP),pointer  :: epol_p(:,:) => null()     ! /waves/coherentwave(i)/beamtracing/polarization/epol_p - Electric field polarization vector in the p rotating coordinates, Matrix of double precision real (nbeams, max_npoints). Time-depe
  real(DP),pointer  :: epol_m(:,:) => null()     ! /waves/coherentwave(i)/beamtracing/polarization/epol_m - Electric field polarization vector in the m rotating coordinates, Matrix of double precision real (nbeams, max_npoints). Time-depe
  real(DP),pointer  :: epol_par(:,:) => null()     ! /waves/coherentwave(i)/beamtracing/polarization/epol_par - Electric field polarization vector in the magnetic field direction, Matrix of double precision real (nbeams, max_npoints). Time-de
endtype

type type_powerflow  !    
  real(DP),pointer  :: phi_perp(:,:) => null()     ! /waves/coherentwave(i)/beamtracing/powerflow/phi_perp - Normalized power flow in the direction perpendicular to the magnetic field; Matrix of double precision real (nbeams, max_npoints).
  real(DP),pointer  :: phi_par(:,:) => null()     ! /waves/coherentwave(i)/beamtracing/powerflow/phi_par - Normalized power flow in the direction parallel to the magnetic field; Matrix of double precision real (nbeams, max_npoints). Time
  real(DP),pointer  :: power_e(:,:) => null()     ! /waves/coherentwave(i)/beamtracing/powerflow/power_e - Power absorbed along the beam by electrons [W]; Matrix of double precision real (nbeams, max_npoints). Time-dependent
  real(DP),pointer  :: power_i(:,:,:) => null()     ! /waves/coherentwave(i)/beamtracing/powerflow/power_i - Power absorbed along the beam by an ion species [W]; Array (3D) of double precision real (nbeams, max_npoints, nion). Time-depende
endtype

type type_pol_decomp  !    
  integer,pointer  :: mpol(:) => null()      ! /waves/coherentwave(i)/fullwave/pol_decomp/mpol - Poloidal mode numbers; Vector (nmpol)
  real(DP),pointer  :: e_plus(:,:,:) => null()     ! /waves/coherentwave(i)/fullwave/pol_decomp/e_plus - Magnitude of poloidal Fourier decomposition of left hand polarised component of the wave electric field [V/m]; Time-dependent; Arr
  real(DP),pointer  :: e_plus_ph(:,:,:) => null()     ! /waves/coherentwave(i)/fullwave/pol_decomp/e_plus_ph - Phase of poloidal Fourier decomposition of left hand polarised component of the wave electric field [rad]; Time-dependent; Array 3
  real(DP),pointer  :: e_minus(:,:,:) => null()     ! /waves/coherentwave(i)/fullwave/pol_decomp/e_minus - Magnitude of poloidal Fourier decomposition of right hand polarised component of the wave electric field; Time-dependent (V/m); Ar
  real(DP),pointer  :: e_minus_ph(:,:,:) => null()     ! /waves/coherentwave(i)/fullwave/pol_decomp/e_minus_ph - Phase of poloidal Fourier decomposition of right hand polarised component of the wave electric field [rad]; Time-dependent; Array 
  real(DP),pointer  :: e_norm(:,:,:) => null()     ! /waves/coherentwave(i)/fullwave/pol_decomp/e_norm - Magnitude of poloidal Fourier decomposition of  wave electric field normal to a flux surface [V/m]; Time dependent; Array 3D (ntor
  real(DP),pointer  :: e_norm_ph(:,:,:) => null()     ! /waves/coherentwave(i)/fullwave/pol_decomp/e_norm_ph - Phase of poloidal Fourier decomposition of  wave electric field normal to a flux surface [rad]; Time dependent; Array 3D (ntor, np
  real(DP),pointer  :: e_binorm(:,:,:) => null()     ! /waves/coherentwave(i)/fullwave/pol_decomp/e_binorm - Magnitude of poloidal Fourier decomposition of wave electric field tangent to a flux surface [V/m]; Time dependent; Array 3D (ntor
  real(DP),pointer  :: e_binorm_ph(:,:,:) => null()     ! /waves/coherentwave(i)/fullwave/pol_decomp/e_binorm_ph - Phase of poloidal Fourier decomposition of wave electric field tangent to a flux surface [rad]; Time dependent; Array 3D (ntor, np
  real(DP),pointer  :: e_para(:,:,:) => null()     ! /waves/coherentwave(i)/fullwave/pol_decomp/e_para - Magnitude of poloidal Fourier decomposition of parallel wave electric field [V/m]; Time dependent; Array 3D (ntor, npsi, nmpol)
  real(DP),pointer  :: e_para_ph(:,:,:) => null()     ! /waves/coherentwave(i)/fullwave/pol_decomp/e_para_ph - Phase of poloidal Fourier decomposition of parallel wave electric field [rad]; Time dependent; Array 3D (ntor, npsi, nmpol)
  real(DP),pointer  :: b_norm(:,:,:) => null()     ! /waves/coherentwave(i)/fullwave/pol_decomp/b_norm - Magnitude of poloidal Fourier decomposition of wave magnetic field normal to a flux surface [T]; Time dependent; Array 3D (ntor, n
  real(DP),pointer  :: b_norm_ph(:,:,:) => null()     ! /waves/coherentwave(i)/fullwave/pol_decomp/b_norm_ph - Phase of poloidal Fourier decomposition of parallel wave electric field [rad]; Time dependent; Array 3D (ntor, npsi, nmpol)
  real(DP),pointer  :: b_binorm(:,:,:) => null()     ! /waves/coherentwave(i)/fullwave/pol_decomp/b_binorm - Magnitude of poloidal Fourier decomposition of wave magnetic field tangent to a flux surface [T]; Time dependent; Array 3D (ntor, 
  real(DP),pointer  :: b_binorm_ph(:,:,:,:) => null()     ! /waves/coherentwave(i)/fullwave/pol_decomp/b_binorm_ph - Phase of poloidal Fourier decomposition of wave magnetic field tangent to a flux surface [rad]; Time dependent; Array 3D (ntor, np
  real(DP),pointer  :: b_para(:,:,:) => null()     ! /waves/coherentwave(i)/fullwave/pol_decomp/b_para - Magnitude of Fourier decomposition of wave magnetic field parallel to the equilibrium magnetic field [T]; Time dependent; Array 3D
  real(DP),pointer  :: b_para_ph(:,:,:) => null()     ! /waves/coherentwave(i)/fullwave/pol_decomp/b_para_ph - Phase of Fourier decomposition of wave magnetic field parallel to the equilibrium magnetic field [T]; Time dependent; Array 3D (nt
endtype

type type_local  !    
  real(DP),pointer  :: e_plus(:,:,:) => null()     ! /waves/coherentwave(i)/fullwave/local/e_plus - Magnitude of left hand polarised component of the wave electric field [V/m]; Time-dependent; Array 3D (ntor, ndim1, ndim2)
  real(DP),pointer  :: e_plus_ph(:,:,:) => null()     ! /waves/coherentwave(i)/fullwave/local/e_plus_ph - Phase of left hand polarised component of the wave electric field [rad]; Time-dependent; Array 3D (ntor, ndim1, ndim2)
  real(DP),pointer  :: e_minus(:,:,:) => null()     ! /waves/coherentwave(i)/fullwave/local/e_minus - Magnitude of right hand polarised component of the wave electric field [v/m]; Time-dependent; Array 3D (ntor, ndim1, ndim2)
  real(DP),pointer  :: e_minus_ph(:,:,:) => null()     ! /waves/coherentwave(i)/fullwave/local/e_minus_ph - Phase of right hand polarised component of the wave electric field [rad]; Time-dependent; Array 3D (ntor, ndim1, ndim2)
  integer,pointer  :: e_norm(:,:,:) => null()     ! /waves/coherentwave(i)/fullwave/local/e_norm - Magnitude of wave electric field normal to a flux surface [V/m]; Time dependent; 3D (ntor, ndim1, ndim2)
  real(DP),pointer  :: enorm_ph(:,:,:) => null()     ! /waves/coherentwave(i)/fullwave/local/enorm_ph - Phase of wave electric field normal to a flux surface [rad]; Time dependent; 3D (ntor, ndim1, ndim2)
  real(DP),pointer  :: e_binorm(:,:,:) => null()     ! /waves/coherentwave(i)/fullwave/local/e_binorm - Magnitude of wave electric field tangent to a flux surface [V/m]; Time dependent; 3D (ntor, ndim1, ndim2)
  real(DP),pointer  :: e_binorm_ph(:,:,:) => null()     ! /waves/coherentwave(i)/fullwave/local/e_binorm_ph - Phase of wave electric field tangent to a flux surface [rad]; Time dependent; 3D (ntor, ndim1, ndim2)
  real(DP),pointer  :: e_para(:,:,:) => null()     ! /waves/coherentwave(i)/fullwave/local/e_para - Magnitude of parallel wave electric field [V/m]; Time dependent; Array 3D (ntor, ndim1, ndim2)
  real(DP),pointer  :: e_para_ph(:,:,:) => null()     ! /waves/coherentwave(i)/fullwave/local/e_para_ph - Phase of parallel wave electric field [rad]; Time dependent; Array 3D (ntor, ndim1, ndim2)
  real(DP),pointer  :: b_norm(:,:,:) => null()     ! /waves/coherentwave(i)/fullwave/local/b_norm - Magnitude of wave magnetic field normal to a flux surface [T]; Time dependent; Array 3D (ntor, ndim1, ndim2)
  real(DP),pointer  :: b_norm_ph(:,:,:) => null()     ! /waves/coherentwave(i)/fullwave/local/b_norm_ph - Phase of wave magnetic field normal to a flux surface [rad]; Time dependent; Array 3D (ntor, ndim1, ndim2)
  real(DP),pointer  :: b_binorm(:,:,:) => null()     ! /waves/coherentwave(i)/fullwave/local/b_binorm - Magnitude of wave magnetic field tangent to a flux surface [T]; Time dependent; Array 3D (ntor, ndim1, ndim2)
  real(DP),pointer  :: b_binorm_ph(:,:,:) => null()     ! /waves/coherentwave(i)/fullwave/local/b_binorm_ph - Phase of wave magnetic field tangent to a flux surface [rad]; Time dependent; Array 3D (ntor, ndim1, ndim2)
  real(DP),pointer  :: b_para(:,:,:) => null()     ! /waves/coherentwave(i)/fullwave/local/b_para - Magnitude of wave magnetic field parallel to the equilibrium magnetic field [T]; Time dependent; Array 3D (ntor, ndim1, ndim2)
  real(DP),pointer  :: b_para_ph(:,:,:) => null()     ! /waves/coherentwave(i)/fullwave/local/b_para_ph - Phase of wave magnetic field parallel to the equilibrium magnetic field [rad]; Time dependent; Array 3D (ntor, ndim1, ndim2)
endtype

type type_beamtracing  !    
  integer,pointer  :: npoints(:) => null()      ! /waves/coherentwave(i)/beamtracing/npoints - Number of points along each ray/beam. Vector of integers (nbeams)
  real(DP),pointer  :: power(:) => null()     ! /waves/coherentwave(i)/beamtracing/power - Initial power in each ray/beam [W], Vector (nbeams). Time-dependent
  real(DP),pointer  :: dnpar(:,:) => null()     ! /waves/coherentwave(i)/beamtracing/dnpar - Spectral width in refractive index associated with each ray/beam, Matrix of double precision real (nbeams, max_npoints). Time-depe
  real(DP),pointer  :: length(:,:) => null()     ! /waves/coherentwave(i)/beamtracing/length - Ray/beam curvilinear length [m], Matrix of double precision real (nbeams, max_npoints). Time-dependent
  type (type_waves_rtposition) :: position  ! /waves/coherentwave(i)/beamtracing/position - Ray/beam position
  type (type_waves_rtwavevector) :: wavevector  ! /waves/coherentwave(i)/beamtracing/wavevector - Ray/beam wave vector.
  type (type_polarization) :: polarization  ! /waves/coherentwave(i)/beamtracing/polarization - Wave field polarization along the ray/beam.
  type (type_powerflow) :: powerflow  ! /waves/coherentwave(i)/beamtracing/powerflow - Power flow along the ray/beam.
endtype

type type_fullwave  !    
  type (type_pol_decomp) :: pol_decomp  ! /waves/coherentwave(i)/fullwave/pol_decomp - Poloidal decomposition of the wave fields. Uses the flux surface grid in grid_1d.
  type (type_local) :: local  ! /waves/coherentwave(i)/fullwave/local - Local description of the wave fields. Uses the grid in grid_2d.
endtype

type type_coherentwave  !    
  type (type_composition) :: composition  ! /waves/coherentwave(i)/composition - 
  type (type_waves_global_param) :: global_param  ! /waves/coherentwave(i)/global_param - Global wave deposition parameters
  type (type_waves_grid_1d) :: grid_1d  ! /waves/coherentwave(i)/grid_1d - Grid points for 1D profiles.
  type (type_waves_grid_2d) :: grid_2d  ! /waves/coherentwave(i)/grid_2d - Grid points for 2D profiles and for full wave solutions.
  type (type_waves_profiles_1d) :: profiles_1d  ! /waves/coherentwave(i)/profiles_1d - 1D radial profiles
  type (type_waves_profiles_2d) :: profiles_2d  ! /waves/coherentwave(i)/profiles_2d - 2D profiles in poloidal cross-section
  type (type_beamtracing) :: beamtracing  ! /waves/coherentwave(i)/beamtracing - Beam-tracing or ray-tracing solver
  type (type_fullwave) :: fullwave  ! /waves/coherentwave(i)/fullwave - Solution by full wave code
  type (type_codeparam) :: codeparam  ! /waves/coherentwave(i)/codeparam - 
endtype

type type_waves  !    
  type (type_datainfo) :: datainfo  ! /waves/datainfo - 
  type (type_coherentwave),pointer :: coherentwave(:) => null()  ! /waves/coherentwave(i) - Wave description for each frequency. Time-dependent. Structure array(nfreq)
  type (type_codeparam) :: codeparam  ! /waves/codeparam - 
  real(DP)  :: time=-9.0D40       ! /waves/time - Time [s]; Time-dependent; Scalar
endtype

type type_mdinfo  !    
  integer  :: shot_min=-999999999       ! /top/topinfo/mdinfo/shot_min - Minimum shot number to which the machine description applies
  integer  :: shot_max=-999999999       ! /top/topinfo/mdinfo/shot_max - Maximum shot number to which the machine description applies
  type (type_entry_def) :: md_entry  ! /top/topinfo/mdinfo/md_entry - Entry of the machine description used. NB : just for information : for the moment, no guarantee that machine description data have
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
  type (type_entry_def) :: entry  ! /top/topinfo/entry - Definition of this database entry
  type (type_entry_def) :: parent_entry  ! /top/topinfo/parent_entry - Definition of the entry of the direct parent (if any)
  type (type_mdinfo) :: mdinfo  ! /top/topinfo/mdinfo - Information related to machine description for this entry
endtype
 
type type_interfdiag  ! Special type CPO (lineintegraldiag) 
  type (type_datainfo) :: datainfo  ! /datainfo - 
  character(len=132), dimension(:), pointer ::expression => null()       ! /expression - Formal expression for the line integral to be evaluated as a function of ne, ni, Te, Ti, Zeff, Br, Bz
  type (type_setup_line) :: setup_line  ! /setup_line - Geometric description of the lines of sight
  type (type_exp1D) :: measure  ! /measure - Measured value. Time-dependent; Vector (nchords)
  real(DP)  :: time=-9.0D40       ! /time - Time [s]; Time-dependent; Scalar
end type
 
type type_polardiag  ! Special type CPO (lineintegraldiag) 
  type (type_datainfo) :: datainfo  ! /datainfo - 
  character(len=132), dimension(:), pointer ::expression => null()       ! /expression - Formal expression for the line integral to be evaluated as a function of ne, ni, Te, Ti, Zeff, Br, Bz
  type (type_setup_line) :: setup_line  ! /setup_line - Geometric description of the lines of sight
  type (type_exp1D) :: measure  ! /measure - Measured value. Time-dependent; Vector (nchords)
  real(DP)  :: time=-9.0D40       ! /time - Time [s]; Time-dependent; Scalar
end type

end module

