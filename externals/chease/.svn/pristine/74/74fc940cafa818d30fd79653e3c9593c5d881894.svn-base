! IDS FORTRAN 90 type definitions
! Contains the type definition of all IDSs


module ids_utilities    ! declare the set of types common to all sub-trees

integer, parameter, public :: DP = kind(1.0d0)

type ids_Plasma_Composition_Charge_States  !    Array of charge states considered   
  real(DP)  :: Z_Min=-9.0D40       ! /Z_Min - Minimum Z of the charge state bundle   
  real(DP)  :: Z_Max=-9.0D40       ! /Z_Max - Maximum Z of the charge state bundle
  character(len=132), dimension(:), pointer ::Label => null()       ! /Label - String identifying charge state (e.g. C+, C+2 , C+3, C+4, C+5, C+6, ...)
endtype

type ids_Plasma_Composition_Ions  !    Array of plasma ions.   
  real(DP)  :: A=-9.0D40       ! /A - Mass of atom   
  real(DP)  :: Zion=-9.0D40       ! /Zion - Ion charge (of the dominant ionisation state; lumped ions are allowed)   
  real(DP)  :: Zn=-9.0D40       ! /Zn - Nuclear charge
  character(len=132), dimension(:), pointer ::Label => null()       ! /Label - String identifying ion (e.g. H+, D+, T+, He+2, C+, ...)
  integer  :: Multiple_Charge_State_Flag=-999999999       ! /Multiple_Charge_State_Flag - Multiple charge state calculation flag : 0-Only one charge state is considered; 1-Multiple charge st
  type (ids_Plasma_Composition_Charge_States),pointer :: Charge_States(:) => null()  ! /Charge_States(i) - Array of charge states considered for this ion (non-empty if Multiple_Charge_State_Flag = 1)
endtype

type ids_Plasma_Composition_Neutrals_Components  !    Components to the atom or molecule   
  real(DP)  :: A=-9.0D40       ! /A - Mass of atom   
  real(DP)  :: Zn=-9.0D40       ! /Zn - Nuclear charge   
  real(DP)  :: Multiplicity=-9.0D40       ! /Multiplicity - Multiplicity of the atom
endtype

type ids_Plasma_Composition_Neutrals  !    Array of neutral atoms or molecules
  type (ids_Plasma_Composition_Neutrals_Components),pointer :: Components(:) => null()  ! /Components(i) - Array of components to the atom or molecule
  integer,pointer  :: Energies(:) => null()      ! /Energies - Types energies considered for this neutral species : 0=cold, 1=thermal, 2= fast, 3=NBI
  character(len=132), dimension(:), pointer ::Label => null()       ! /Label - String identifying the atom or molecule (e.g. D2, DT, CD4, ...)
endtype

type ids_Errorbar  !    Structure for 1D signal errorbar
  character(len=132), dimension(:), pointer ::Type => null()       ! /Type - Type of errorbar description which is used. Refers to the name of the sibling fields.
  real(DP),pointer  :: Signal(:) => null()     ! /Signal - Absolute error provided as a Signal of same dimension as the related field.   
  real(DP)  :: Constant=-9.0D40       ! /Constant - Relative error in % provided as a constant
  character(len=132), dimension(:), pointer ::Expression => null()       ! /Expression - Absolute error provided as an Expression.
endtype

type ids_Signal_Type  !    Structure for 1D signal (prototype to be evaluated)
  real(DP), pointer  :: Timebase(:) => null()  ! Timebase
  real(DP),pointer  :: Value(:) => null()     ! /Value - Value of the signal. Value/Data is ugly. Moreover the metadata are not at the right place. 
endtype

type ids_B_Tor_Vacuum_1  !    Characteristics of the vacuum toroidal field. Timebase assumed to be one level above   
  real(DP)  :: R0=-9.0D40       ! /R0 - Reference major radius where the vacuum toroidal magnetic field is given (usually a fixed position s
  real(DP),pointer  :: B0(:) => null()     ! /B0 - Vacuum toroidal field at R0 [T]; Positive sign means anti-clockwise when viewed from above. The prod
endtype

type ids_B_Tor_Vacuum_2  !    Characteristics of the vacuum toroidal field. Timebase assumed to be two levels above   
  real(DP)  :: R0=-9.0D40       ! /R0 - Reference major radius where the vacuum toroidal magnetic field is given (usually a fixed position s
  real(DP),pointer  :: B0(:) => null()     ! /B0 - Vacuum toroidal field at R0 [T]; Positive sign means anti-clockwise when viewed from above. The prod
endtype

type ids_RZPhi0D_STATIC  !    Structure for R, Z, Phi positions (0D, STATIC)   
  real(DP)  :: R=-9.0D40       ! /R - Major radius   
  real(DP)  :: Z=-9.0D40       ! /Z - Height   
  real(DP)  :: Phi=-9.0D40       ! /Phi - Toroidal angle
endtype

type ids_RZPhi1D_STATIC  !    Structure for list of R, Z, Phi positions (1D, STATIC)
  real(DP),pointer  :: R(:) => null()     ! /R - Major radius
  real(DP),pointer  :: Z(:) => null()     ! /Z - Height
  real(DP),pointer  :: Phi(:) => null()     ! /Phi - Toroidal angle
endtype

type ids_RZ1D_CONSTANT  !    Structure for list of R, Z positions (1D, CONSTANT)
  real(DP),pointer  :: R(:) => null()     ! /R - Major radius
  real(DP),pointer  :: Z(:) => null()     ! /Z - Height
endtype

type ids_RZ1D_SIGNAL  !    Structure for list of R, Z positions (1D, SIGNAL), Timebase assumed to be 2 levels above
  real(DP),pointer  :: R(:) => null()     ! /R - Major radius
  real(DP),pointer  :: Z(:) => null()     ! /Z - Height
endtype

type ids_RZ2D_SIGNAL_Npoints  !    Structure for list of R, Z positions (2D, SIGNAL), with an N_Points indicator of the number of active points in each list. This co
  integer,pointer  :: N_Points(:) => null()      ! /N_Points - Number of meaningful points in R and Z at a given time. R and Z are matrices oversized on their firs
  real(DP),pointer  :: R(:,:) => null()     ! /R - Major radius
  real(DP),pointer  :: Z(:,:) => null()     ! /Z - Height
endtype

type ids_Outline_2D_Geometry_STATIC  !    Description of 2D geometric outline
  integer  :: Description_Type=-999999999       ! /Description_Type - Type used to describe the element shape (0 for 'Coordinates_RZ' or 1 for 'RZDRDZ') 
  real(DP),pointer  :: Coordinates_RZ(:,:) => null()     ! /Coordinates_RZ - Irregular outline of the element
  real(DP),pointer  :: RZDRDZ(:) => null()     ! /RZDRDZ - 4-Vector defining Centre R,Z and full extents dR, dZ 
endtype

type ids_Plasma_Composition  !    Generic declaration of Plasma Composition for a simulation
  type (ids_Plasma_Composition_Ions),pointer :: Ions(:) => null()  ! /Plasma_Composition/Ions(i) - Array of plasma ions
  type (ids_Plasma_Composition_Neutrals),pointer :: Neutrals(:) => null()  ! /Plasma_Composition/Neutrals(i) - Array of neutrals
endtype

type ids_Code_Parameters  !    Generic decription of the code specific parameters for the code that has produced this IDS
  character(len=132), dimension(:), pointer ::Code_Name => null()       ! /Code_Parameters/Code_Name - Name of the code
  character(len=132), dimension(:), pointer ::Code_Version => null()       ! /Code_Parameters/Code_Version - Version of the code
  character(len=132), dimension(:), pointer ::Parameters => null()       ! /Code_Parameters/Parameters - List of the code specific parameters in XML format
  character(len=132), dimension(:), pointer ::Output_Diagnostics => null()       ! /Code_Parameters/Output_Diagnostics - List of the code specific diagnostic/output in XML format
  integer,pointer  :: Output_Flag(:) => null()      ! /Code_Parameters/Output_Flag - Output flag : 0 means the run is successful, other values mean some difficulty has been encountered,
endtype

type ids_Parameters_Input  !    Code parameters block passed from the wrapper to the subroutine. Does not appear as such in the data structure. This is inserted i
  character(len=132), dimension(:), pointer ::Parameters_Value => null()       ! /Parameters_Input/Parameters_Value - Actual value of the code parameters (instance of Code_Parameters/Parameters in XML format)
  character(len=132), dimension(:), pointer ::Parameters_Default => null()       ! /Parameters_Input/Parameters_Default - Default value of the code parameters (instance of Code_Parameters/Parameters in XML format)
  character(len=132), dimension(:), pointer ::Schema => null()       ! /Parameters_Input/Schema - Code parameters schema
endtype

type ids_IDS_Properties  !    Interface Data Structure properties. This element identifies the node above as an IDS
  character(len=132), dimension(:), pointer ::Status_of => null()       ! /IDS_Properties/Status_of - Status of this structure 'OK' if the structure was created consistently, 'PARTIAL' if not complete, 
  character(len=132), dimension(:), pointer ::Comment_of => null()       ! /IDS_Properties/Comment_of - Any additional comment from the source application
  character(len=132), dimension(:), pointer ::Creator_of => null()       ! /IDS_Properties/Creator_of - Creator of this consistent IDS
  character(len=132), dimension(:), pointer ::Date_of => null()       ! /IDS_Properties/Date_of - Date of consistent creation of this IDS
  character(len=132), dimension(:), pointer ::Source_of => null()       ! /IDS_Properties/Source_of - Where do the data for this IDS come from (a procedure, a set of codes)
  character(len=132), dimension(:), pointer ::Reference_of => null()       ! /IDS_Properties/Reference_of - Documented reference to this IDS data
  integer  :: Homogeneous_Timebase=-999999999       ! /IDS_Properties/Homogeneous_Timebase - 1 if the Timebase of this IDS is homogeneous. In this case, the Timebase values for this IDS are sto
  integer  :: cocos=-999999999       ! /IDS_Properties/cocos - Coordinate conventions of this IDS
endtype


end module ! end of the utilities module

module ids_schemas       ! declaration of all IDSs

use ids_utilities


! ***********  Include Actuator/DD_Actuator.xsd
type ids_Actuator  !    Generic simple description of a heating/current drive actuator, for a first simplified version of the Plasma Simulator component
  type (ids_IDS_Properties) :: IDS_Properties  ! /Actuator/IDS_Properties - 
  character(len=132), dimension(:), pointer ::Name => null()       ! /Actuator/Name - Name of the actuator (IC, EC, NBI, LH)
  character(len=132), dimension(:), pointer ::Channels => null()       ! /Actuator/Channels - ID of the multiple channels of the actuator: Beam boxes for NBI, EC or IC launcher, ...
  real(DP),pointer  :: Power(:,:) => null()     ! /Actuator/Power - Power delivered at the output of the actuator, in the vessel (NB this is before the coupling / beam 
  real(DP),pointer  :: Generic_Signal(:,:) => null()     ! /Actuator/Generic_Signal - Generic 2D signal slot for storing an actuator control parameter (e.g. the angle of an ECRH mirror)
  type (ids_Code_Parameters) :: Code_Parameters  ! /Actuator/Code_Parameters - 
  real(DP), pointer  :: Timebase(:) => null()  ! Timebase
endtype

! ***********  Include Core_Profiles/DD_Core_Profiles.xsd
type ids_Core_Profiles_Ions_Charge_States  !    Quantities related to the a given charge state of the ion species   
  real(DP)  :: Z_Min=-9.0D40       ! /Z_Min - Minimum Z of the charge state bundle   
  real(DP)  :: Z_Max=-9.0D40       ! /Z_Max - Maximum Z of the charge state bundle
  character(len=132), dimension(:), pointer ::Label => null()       ! /Label - String identifying charge state (e.g. C+, C+2 , C+3, C+4, C+5, C+6, ...)
  real(DP),pointer  :: Nz(:,:) => null()     ! /Nz - Density of the charge state considered (thermal+non-thermal)
  real(DP),pointer  :: Nz_Fast(:,:) => null()     ! /Nz_Fast - Density of fast (non-thermal) ions, for the charge state considered
  real(DP),pointer  :: Tz(:,:) => null()     ! /Tz - Temperature of the charge state considered
  real(DP),pointer  :: V_Tor_z(:,:) => null()     ! /V_Tor_z - Toroidal velocity of the charge state considered
  real(DP),pointer  :: V_Pol_z(:,:) => null()     ! /V_Pol_z - Poloidal velocity of the charge state considered
endtype

type ids_Core_Profile_Ions  !    Quantities related to a given ion species   
  real(DP)  :: A=-9.0D40       ! /A - Mass of atom   
  real(DP)  :: Zion=-9.0D40       ! /Zion - Ion charge (of the dominant ionisation state; lumped ions are allowed)   
  real(DP)  :: Zn=-9.0D40       ! /Zn - Nuclear charge
  character(len=132), dimension(:), pointer ::Label => null()       ! /Label - String identifying ion (e.g. H+, D+, T+, He+2, C+, ...)
  real(DP),pointer  :: Ni(:,:) => null()     ! /Ni - Ion density (thermal+non-thermal)
  real(DP),pointer  :: Ni_Fast(:,:) => null()     ! /Ni_Fast - Density of fast (non-thermal) ions, for the ion species considered
  real(DP),pointer  :: Ti(:,:) => null()     ! /Ti - Ion temperature of that ion species
  real(DP),pointer  :: Pi(:,:) => null()     ! /Pi - Ion pressure for that ion species
  real(DP),pointer  :: V_Tor_i(:,:) => null()     ! /V_Tor_i - Toroidal velocity of that ion species
  real(DP),pointer  :: V_Pol_i(:,:) => null()     ! /V_Pol_i - Poloidal velocity of that ion species
  integer  :: Multiple_Charge_State_Flag=-999999999       ! /Multiple_Charge_State_Flag - Multiple charge state calculation flag : 0-Only one charge state is considered; 1-Multiple charge st
  type (ids_Core_Profiles_Ions_Charge_States),pointer :: Charge_States(:) => null()  ! /Charge_States(i) - Quantities related to the different charge states of the ion species
endtype

type ids_Core_Profiles_Global  !    Various global quantities calculated from the fields solved in the transport equations and from the Derived Profiles
  real(DP),pointer  :: Ip(:) => null()     ! /Ip - Total plasma current
  real(DP),pointer  :: Current_Non_Inductive(:) => null()     ! /Current_Non_Inductive - Total non-inductive parallel current
  real(DP),pointer  :: Current_Bootstrap(:) => null()     ! /Current_Bootstrap - Bootstrap parallel current
  real(DP),pointer  :: V_Loop(:) => null()     ! /V_Loop - LCFS loop voltage
  real(DP),pointer  :: li_3(:) => null()     ! /li_3 - Internal inductance
  real(DP),pointer  :: Beta_Tor(:) => null()     ! /Beta_Tor - Toroidal beta (ratio of kinetic to magnetic pressure)
  real(DP),pointer  :: Beta_Tor_Norm(:) => null()     ! /Beta_Tor_Norm - Normalised toroidal beta
  real(DP),pointer  :: Beta_Pol(:) => null()     ! /Beta_Pol - Poloidal beta
  real(DP),pointer  :: Energy_Diamagnetic(:) => null()     ! /Energy_Diamagnetic - Plasma energy content = 3/2 * int(p,dV) with p being the total pressure (Pressure_Thermal + Pressure
endtype

type ids_Core_Profiles  !    Core plasma radial profiles
  type (ids_IDS_Properties) :: IDS_Properties  ! /Core_Profiles/IDS_Properties - 
  real(DP),pointer  :: Rho_Tor_Norm(:,:) => null()     ! /Core_Profiles/Rho_Tor_Norm - Normalised toroidal flux coordinate. The normalizing value for Rho_Tor_Norm, is the toroidal flux co
  type (ids_B_Tor_Vacuum_1) :: Vacuum_Toroidal_Field  ! /Core_Profiles/Vacuum_Toroidal_Field - Characteristics of the vacuum toroidal field (used in Rho_Tor definition and in the normalization of
  real(DP),pointer  :: Psi(:,:) => null()     ! /Core_Profiles/Psi - Poloidal magnetic flux
  real(DP),pointer  :: Te(:,:) => null()     ! /Core_Profiles/Te - Electron temperature
  real(DP),pointer  :: Ti_Average(:,:) => null()     ! /Core_Profiles/Ti_Average - Ion temperature (averaged on charge states and ion species)
  real(DP),pointer  :: Ne(:,:) => null()     ! /Core_Profiles/Ne - Electron density (thermal+non-thermal)
  real(DP),pointer  :: Ne_Fast(:,:) => null()     ! /Core_Profiles/Ne_Fast - Density of fast (non-thermal) electrons
  type (ids_Core_Profile_Ions),pointer :: Ions(:) => null()  ! /Core_Profiles/Ions(i) - Quantities related to the different ion species
  real(DP),pointer  :: Ni_Total_Over_Ne(:,:) => null()     ! /Core_Profiles/Ni_Total_Over_Ne - Ratio of total ion density (sum over species and charge states) over electron density. (thermal+non-
  real(DP),pointer  :: Momentum_Tor(:,:) => null()     ! /Core_Profiles/Momentum_Tor - Total plasma toroidal momentum, summed over ion species and electrons 
  real(DP),pointer  :: Zeff(:,:) => null()     ! /Core_Profiles/Zeff - Effective charge
  real(DP),pointer  :: Pe(:,:) => null()     ! /Core_Profiles/Pe - Electron pressure
  real(DP),pointer  :: Pi_Total(:,:) => null()     ! /Core_Profiles/Pi_Total - Total ion pressure (sum over the ion species)
  real(DP),pointer  :: Pressure_Thermal(:,:) => null()     ! /Core_Profiles/Pressure_Thermal - Thermal pressure (electrons+ions)
  real(DP),pointer  :: Pressure_Perpendicular(:,:) => null()     ! /Core_Profiles/Pressure_Perpendicular - Total perpendicular pressure (electrons+ions, thermal+non-thermal)
  real(DP),pointer  :: Pressure_Parallel(:,:) => null()     ! /Core_Profiles/Pressure_Parallel - Total parallel pressure (electrons+ions, thermal+non-thermal)
  real(DP),pointer  :: J_Total(:,:) => null()     ! /Core_Profiles/J_Total - Total parallel current density = average(jtot.B) / B0, where B0 = Core_Profiles/Vacuum_Toroidal_Fiel
  real(DP),pointer  :: J_Tor(:,:) => null()     ! /Core_Profiles/J_Tor - Total toroidal current density = average(J_Tor/R) / average(1/R)
  real(DP),pointer  :: J_Ohmic(:,:) => null()     ! /Core_Profiles/J_Ohmic - Ohmic parallel current density = average(J_Ohmic.B) / B0, where B0 = Core_Profiles/Vacuum_Toroidal_F
  real(DP),pointer  :: J_Non_Inductive(:,:) => null()     ! /Core_Profiles/J_Non_Inductive - Non-inductive (includes bootstrap) parallel current density = average(jni.B) / B0, where B0 = Core_P
  real(DP),pointer  :: J_Bootstrap(:,:) => null()     ! /Core_Profiles/J_Bootstrap - Bootstrap parallel current density = average(J_Bootstrap.B) / B0, where B0 = Core_Profiles/Vacuum_To
  real(DP),pointer  :: Conductivity_Parallel(:,:) => null()     ! /Core_Profiles/Conductivity_Parallel - Parallel conductivity
  real(DP),pointer  :: E_Parallel(:,:) => null()     ! /Core_Profiles/E_Parallel - Parallel electric field = average(E.B) / B0, where Core_Profiles/Vacuum_Toroidal_Field/ B0
  real(DP),pointer  :: q(:,:) => null()     ! /Core_Profiles/q - Safety factor
  real(DP),pointer  :: Magnetic_Shear(:,:) => null()     ! /Core_Profiles/Magnetic_Shear - Magnetic shear
  type (ids_Core_Profiles_Global) :: Global  ! /Core_Profiles/Global - Various global quantities derived from the Core_Profiles
  type (ids_Code_Parameters) :: Code_Parameters  ! /Core_Profiles/Code_Parameters - 
  real(DP), pointer  :: Timebase(:) => null()  ! Timebase
endtype

! ***********  Include Core_Sources/DD_Core_Sources.xsd
type ids_Core_Sources_Actuators_Ions_Charge_States  !    Source terms related to the a given charge state of the ion species   
  real(DP)  :: Z_Min=-9.0D40       ! /Z_Min - Minimum Z of the charge state bundle   
  real(DP)  :: Z_Max=-9.0D40       ! /Z_Max - Maximum Z of the charge state bundle
  character(len=132), dimension(:), pointer ::Label => null()       ! /Label - String identifying charge state (e.g. C+, C+2 , C+3, C+4, C+5, C+6, ...)
  real(DP),pointer  :: Nz_Source(:,:) => null()     ! /Nz_Source - Density of the charge state considered (thermal+non-thermal)
  real(DP),pointer  :: Tz_Source(:,:) => null()     ! /Tz_Source - Temperature of the charge state considered
endtype

type ids_Core_Sources_Actuators_Ions  !    Source terms related to a given ion species   
  real(DP)  :: A=-9.0D40       ! /A - Mass of atom   
  real(DP)  :: Zion=-9.0D40       ! /Zion - Ion charge (of the dominant ionisation state; lumped ions are allowed)   
  real(DP)  :: Zn=-9.0D40       ! /Zn - Nuclear charge
  character(len=132), dimension(:), pointer ::Label => null()       ! /Label - String identifying ion (e.g. H+, D+, T+, He+2, C+, ...)
  real(DP),pointer  :: Ni_Source(:,:) => null()     ! /Ni_Source - Source term for ion density equation
  real(DP),pointer  :: Ti_Source(:,:) => null()     ! /Ti_Source - Source term for the ion energy transport equation.
  integer  :: Multiple_Charge_State_Flag=-999999999       ! /Multiple_Charge_State_Flag - Multiple charge state calculation flag : 0-Only one charge state is considered; 1-Multiple charge st
  type (ids_Core_Sources_Actuators_Ions_Charge_States),pointer :: Charge_States(:) => null()  ! /Charge_States(i) - Source terms related to the different charge states of the ion species
endtype

type ids_Core_Sources_Actuators  !    Source terms for a given model
  character(len=132), dimension(:), pointer ::Name => null()       ! /Name - Name of the actuator
  real(DP),pointer  :: Conductivity_Parallel(:,:) => null()     ! /Conductivity_Parallel - Parallel conductivity induced by this actuator
  real(DP),pointer  :: Ne_Source(:,:) => null()     ! /Ne_Source - Source term for electron density equation
  real(DP),pointer  :: Te_Source(:,:) => null()     ! /Te_Source - Source term for electron energy equation
  real(DP),pointer  :: Ti_Average_Source(:,:) => null()     ! /Ti_Average_Source - Source term for the total ion energy equation
  real(DP),pointer  :: Momentum_Tor_Source(:,:) => null()     ! /Momentum_Tor_Source - Source term for total toroidal momentum equation
  type (ids_Core_Sources_Actuators_Ions),pointer :: Ions(:) => null()  ! /Ions(i) - Source terms related to the different ions species
endtype

type ids_Core_Sources  !    Core plasma source terms (for the transport equations)
  type (ids_IDS_Properties) :: IDS_Properties  ! /Core_Sources/IDS_Properties - 
  real(DP),pointer  :: Rho_Tor_Norm(:,:) => null()     ! /Core_Sources/Rho_Tor_Norm - Normalised toroidal flux coordinate. The normalizing value for Rho_Tor_Norm, is the toroidal flux co
  type (ids_B_Tor_Vacuum_1) :: Vacuum_Toroidal_Field  ! /Core_Sources/Vacuum_Toroidal_Field - Characteristics of the vacuum toroidal field (used in Rho_Tor definition and in the normalization of
  type (ids_Core_Sources_Actuators),pointer :: Actuators(:) => null()  ! /Core_Sources/Actuators(i) - Source is described by a combination of various actuators. All actuators provide their values on the
  type (ids_Code_Parameters) :: Code_Parameters  ! /Core_Sources/Code_Parameters - 
  real(DP), pointer  :: Timebase(:) => null()  ! Timebase
endtype

! ***********  Include Core_Transport/DD_Core_Transport.xsd
type ids_Core_Transport_Models_Density_1  !    Transport coefficients for density equations. Axes one level above.
  real(DP),pointer  :: D(:,:,:) => null()     ! /D - Effective diffusivity. The 2nd index of the array describes which multiplier should be applied to th
  real(DP),pointer  :: V(:,:,:) => null()     ! /V - Effective convection. The 2nd index of the array describes which multiplier should be applied to the
endtype

type ids_Core_Transport_Models_Density_2  !    Transport coefficients for density equations. Axes two levels above.
  real(DP),pointer  :: D(:,:,:) => null()     ! /D - Effective diffusivity. The 2nd index of the array describes which multiplier should be applied to th
  real(DP),pointer  :: V(:,:,:) => null()     ! /V - Effective convection. The 2nd index of the array describes which multiplier should be applied to the
endtype

type ids_Core_Transport_Models_Density_3  !    Transport coefficients for density equations. Axes three levels above.
  real(DP),pointer  :: D(:,:,:) => null()     ! /D - Effective diffusivity. The 2nd index of the array describes which multiplier should be applied to th
  real(DP),pointer  :: V(:,:,:) => null()     ! /V - Effective convection. The 2nd index of the array describes which multiplier should be applied to the
endtype

type ids_Core_Transport_Models_Density_4  !    Transport coefficients for density equations. Axes four levels above.
  real(DP),pointer  :: D(:,:,:) => null()     ! /D - Effective diffusivity. The 2nd index of the array describes which multiplier should be applied to th
  real(DP),pointer  :: V(:,:,:) => null()     ! /V - Effective convection. The 2nd index of the array describes which multiplier should be applied to the
endtype

type ids_Core_Transport_Models_Energy_TorMom_1  !    Transport coefficients for energy or total toroidal momentum equations. Axes one level above.
  real(DP),pointer  :: D(:,:) => null()     ! /D - Effective diffusivity
  real(DP),pointer  :: V(:,:) => null()     ! /V - Effective convection
endtype

type ids_Core_Transport_Models_Energy_TorMom_2  !    Transport coefficients for energy or total toroidal momentum equations. Axes two levels above.
  real(DP),pointer  :: D(:,:) => null()     ! /D - Effective diffusivity
  real(DP),pointer  :: V(:,:) => null()     ! /V - Effective convection
endtype

type ids_Core_Transport_Models_Energy_TorMom_3  !    Transport coefficients for energy or total toroidal momentum equations. Axes three levels above.
  real(DP),pointer  :: D(:,:) => null()     ! /D - Effective diffusivity
  real(DP),pointer  :: V(:,:) => null()     ! /V - Effective convection
endtype

type ids_Core_Transport_Models_Energy_TorMom_4  !    Transport coefficients for energy or total toroidal momentum equations. Axes four levels above.
  real(DP),pointer  :: D(:,:) => null()     ! /D - Effective diffusivity
  real(DP),pointer  :: V(:,:) => null()     ! /V - Effective convection
endtype

type ids_Core_Transport_Models_Ions_Charge_States  !    Transport coefficients related to the a given charge state of the ion species   
  real(DP)  :: Z_Min=-9.0D40       ! /Z_Min - Minimum Z of the charge state bundle   
  real(DP)  :: Z_Max=-9.0D40       ! /Z_Max - Maximum Z of the charge state bundle
  character(len=132), dimension(:), pointer ::Label => null()       ! /Label - String identifying charge state (e.g. C+, C+2 , C+3, C+4, C+5, C+6, ...)
  type (ids_Core_Transport_Models_Density_4) :: Nz_Transport  ! /Nz_Transport - Transport coefficients of for the density of the charge state considered (thermal+non-thermal)
  type (ids_Core_Transport_Models_Energy_TorMom_4) :: Tz_Transport  ! /Tz_Transport - Transport coefficients of for the energy of the charge state considered 
endtype

type ids_Core_Transport_Models_Ions  !    Transport coefficients related to a given ion species   
  real(DP)  :: A=-9.0D40       ! /A - Mass of atom   
  real(DP)  :: Zion=-9.0D40       ! /Zion - Ion charge (of the dominant ionisation state; lumped ions are allowed)   
  real(DP)  :: Zn=-9.0D40       ! /Zn - Nuclear charge
  character(len=132), dimension(:), pointer ::Label => null()       ! /Label - String identifying ion (e.g. H+, D+, T+, He+2, C+, ...)
  type (ids_Core_Transport_Models_Density_3) :: Ni_Transport  ! /Ni_Transport - Transport coefficients for ion density equation
  type (ids_Core_Transport_Models_Energy_TorMom_3) :: Ti_Transport  ! /Ti_Transport - Transport coefficients for ion energy equation
  integer  :: Multiple_Charge_State_Flag=-999999999       ! /Multiple_Charge_State_Flag - Multiple charge state calculation flag : 0-Only one charge state is considered; 1-Multiple charge st
  type (ids_Core_Transport_Models_Ions_Charge_States),pointer :: Charge_States(:) => null()  ! /Charge_States(i) - Transport coefficients related to the different charge states of the ion species
endtype

type ids_Core_Transport_Models  !    Transport coefficients for a given model
  character(len=132), dimension(:), pointer ::Name => null()       ! /Name - Name of the transport model
  real(DP),pointer  :: Conductivity_Parallel(:,:) => null()     ! /Conductivity_Parallel - Parallel conductivity
  type (ids_Core_Transport_Models_Density_2) :: Ne_Transport  ! /Ne_Transport - Transport coefficients for electron density equation
  type (ids_Core_Transport_Models_Energy_TorMom_2) :: Te_Transport  ! /Te_Transport - Transport coefficients for electron energy equation
  type (ids_Core_Transport_Models_Energy_TorMom_2) :: Ti_Average_Transport  ! /Ti_Average_Transport - Transport coefficients for averaged (over ion species) ion energy equation
  type (ids_Core_Transport_Models_Energy_TorMom_2) :: Momentum_Tor_Transport  ! /Momentum_Tor_Transport - Transport coefficients for total toroidal momentum equation
  type (ids_Core_Transport_Models_Ions),pointer :: Ions(:) => null()  ! /Ions(i) - Transport coefficients related to the different ions species
endtype

type ids_Core_Transport  !    Core plasma transport
  type (ids_IDS_Properties) :: IDS_Properties  ! /Core_Transport/IDS_Properties - 
  real(DP),pointer  :: Rho_Tor_Norm(:,:) => null()     ! /Core_Transport/Rho_Tor_Norm - Normalised toroidal flux coordinate. The normalizing value for Rho_Tor_Norm, is the toroidal flux co
  type (ids_B_Tor_Vacuum_1) :: Vacuum_Toroidal_Field  ! /Core_Transport/Vacuum_Toroidal_Field - Characteristics of the vacuum toroidal field (used in Rho_Tor definition and in the normalization of
  type (ids_Core_Transport_Models),pointer :: Models(:) => null()  ! /Core_Transport/Models(i) - Transport is described by a combination of various transport models. All models provide their values
  type (ids_Code_Parameters) :: Code_Parameters  ! /Core_Transport/Code_Parameters - 
  real(DP), pointer  :: Timebase(:) => null()  ! Timebase
endtype

! ***********  Include Equilibrium/DD_Equilibrium.xsd
type ids_Equilibrium_Boundary  !    Geometry of the plasma boundary
  integer,pointer  :: Type(:) => null()      ! /Type - 0 (limiter) or 1 (diverted)
  integer,pointer  :: N_Outline_Points(:) => null()      ! /N_Outline_Points - Number of meaningful Outline points in R and Z at a given time.
  real(DP),pointer  :: Outline_RZ(:,:,:) => null()     ! /Outline_RZ - RZ description of the plasma boundary. The second dimension is oversized to fit the maximum number o
  real(DP),pointer  :: Geometric_Axis_RZ(:,:) => null()     ! /Geometric_Axis_RZ - RZ position of the geometric axis (defined as (Rmin+Rmax) / 2 of the boundary)
  real(DP),pointer  :: a_minor(:) => null()     ! /a_minor - Minor radius of the plasma boundary (defined as (Rmax-Rmin) / 2 of the boundary)
  real(DP),pointer  :: Elongation(:) => null()     ! /Elongation - Elongation of the plasma boundary
  real(DP),pointer  :: Elongation_Upper(:) => null()     ! /Elongation_Upper - Elongation (upper half w.r.t. geometric axis) of the plasma boundary
  real(DP),pointer  :: Elongation_Lower(:) => null()     ! /Elongation_Lower - Elongation (lower half w.r.t. geometric axis) of the plasma boundary
  real(DP),pointer  :: Triangularity_Upper(:) => null()     ! /Triangularity_Upper - Upper triangularity of the plasma boundary
  real(DP),pointer  :: Triangularity_Lower(:) => null()     ! /Triangularity_Lower - Lower triangularity of the plasma boundary
  integer,pointer  :: N_X_Points(:) => null()      ! /N_X_Points - Number of X points at a given time
  real(DP),pointer  :: X_Points_RZ(:,:,:) => null()     ! /X_Points_RZ - RZ position of the X points, first is the active X point if diverted. The second dimension is oversi
  real(DP),pointer  :: Strike_Point_Lower_Left_RZ(:,:) => null()     ! /Strike_Point_Lower_Left_RZ - RZ position of the lower left strike point
  real(DP),pointer  :: Strike_Point_Upper_Left_RZ(:,:) => null()     ! /Strike_Point_Upper_Left_RZ - RZ position of the upper left strike point
  real(DP),pointer  :: Strike_Point_Lower_Right_RZ(:,:) => null()     ! /Strike_Point_Lower_Right_RZ - RZ position of the lower right strike point
  real(DP),pointer  :: Strike_Point_Upper_Right_RZ(:,:) => null()     ! /Strike_Point_Upper_Right_RZ - RZ position of the upper right strike point
  real(DP),pointer  :: Active_Limiter_Point_RZ(:,:) => null()     ! /Active_Limiter_Point_RZ - RZ position of the active limiter point (point of the plasma boundary in contact with the limiter)
endtype

type ids_Equlibrium_Global  !    0D parameters of the equilibrium
  real(DP),pointer  :: Beta_Pol(:) => null()     ! /Beta_Pol - Poloidal beta
  real(DP),pointer  :: Beta_Tor(:) => null()     ! /Beta_Tor - Toroidal beta
  real(DP),pointer  :: Beta_Normal(:) => null()     ! /Beta_Normal - Normalised toroidal beta
  real(DP),pointer  :: Ip(:) => null()     ! /Ip - Plasma current. Positive sign means anti-clockwise when viewed from above.
  real(DP),pointer  :: li_3(:) => null()     ! /li_3 - Internal inductance
  real(DP),pointer  :: Volume(:) => null()     ! /Volume - Total plasma volume
  real(DP),pointer  :: Area(:) => null()     ! /Area - Area of the poloidal cross section
  real(DP),pointer  :: Psi_Axis(:) => null()     ! /Psi_Axis - Poloidal flux at the magnetic axis
  real(DP),pointer  :: Psi_Boundary(:) => null()     ! /Psi_Boundary - Poloidal flux at the selected plasma boundary
  type (ids_RZ1D_SIGNAL) :: Magnetic_Axis  ! /Magnetic_Axis - Magnetic axis position
  real(DP),pointer  :: B_Tor_Axis(:) => null()     ! /B_Tor_Axis - Total toroidal magnetic field at the magnetic axis
  real(DP),pointer  :: q_Axis(:) => null()     ! /q_Axis - q at the magnetic axis
  real(DP),pointer  :: q_95(:) => null()     ! /q_95 - q at the 95% poloidal flux surface
  real(DP),pointer  :: q_Min(:) => null()     ! /q_Min - Minimum q value in the plasma
  type (ids_B_Tor_Vacuum_2) :: Vacuum_Toroidal_Field  ! /Vacuum_Toroidal_Field - Characteristics of the vacuum toroidal field
  real(DP),pointer  :: W_MHD(:) => null()     ! /W_MHD - Plasma energy content = 3/2 * int(p,dV) with p being the total pressure (thermal + fast particles) [
  real(DP),pointer  :: Gamma(:) => null()     ! /Gamma - Adiabatic index
endtype

type ids_Equilibrium_Profiles_1D  !    Equilibrium profiles (1D radial grid) as a function of the poloidal flux
  real(DP),pointer  :: Psi(:,:) => null()     ! /Psi - Poloidal flux, such that Bp=|grad psi| /R/2/pi
  real(DP),pointer  :: Phi(:,:) => null()     ! /Phi - Toroidal flux
  real(DP),pointer  :: Pressure(:,:) => null()     ! /Pressure - Pressure
  real(DP),pointer  :: F(:,:) => null()     ! /F - Diamagnetic function (F=R B_Phi)
  real(DP),pointer  :: dPressure_dPsi(:,:) => null()     ! /dPressure_dPsi - Derivative of Pressure w.r.t. Psi
  real(DP),pointer  :: F_dF_dPsi(:,:) => null()     ! /F_dF_dPsi - Derivative of F w.r.t. Psi, multiplied with F
  real(DP),pointer  :: J_Tor(:,:) => null()     ! /J_Tor - Flux surface averaged toroidal current density = average(J_TorR) / average(1/R)
  real(DP),pointer  :: J_Parallel(:,:) => null()     ! /J_Parallel - Flux surface averaged parallel current density = average(j.B) / B0, where B0 = Equilibrium/Global/To
  real(DP),pointer  :: q(:,:) => null()     ! /q - Safety factor
  real(DP),pointer  :: Magnetic_Shear(:,:) => null()     ! /Magnetic_Shear - Magnetic shear
  real(DP),pointer  :: R_Inboard(:,:) => null()     ! /R_Inboard - Radial coordinate (major radius) on the inboard side of the magnetic axis
  real(DP),pointer  :: R_Outboard(:,:) => null()     ! /R_Outboard - Radial coordinate (major radius) on the outboard side of the magnetic axis
  real(DP),pointer  :: Rho_Tor(:,:) => null()     ! /Rho_Tor - Toroidal flux coordinate, defined as sqrt(Phi/pi/B0), where B0 = Equilibrium/Global/Toroidal_Field/B
  real(DP),pointer  :: dPsi_dRho_Tor(:,:) => null()     ! /dPsi_dRho_Tor - Derivative of Psi with respect to Rho_Tor
  real(DP),pointer  :: Rho_Vol_Norm(:,:) => null()     ! /Rho_Vol_Norm - Normalised radial coordinate related to the plasma volume. Defined as sqrt(volume / volume[LCFS])
  real(DP),pointer  :: Elongation(:,:) => null()     ! /Elongation - Elongation
  real(DP),pointer  :: Triangularity_Upper(:,:) => null()     ! /Triangularity_Upper - Upper triangularity w.r.t. magnetic axis
  real(DP),pointer  :: Triangularity_Lower(:,:) => null()     ! /Triangularity_Lower - Lower triangularity w.r.t. magnetic axis
  real(DP),pointer  :: Volume(:,:) => null()     ! /Volume - Volume enclosed in the flux surface
  real(DP),pointer  :: dVolume_dPsi(:,:) => null()     ! /dVolume_dPsi - Radial derivative of the volume enclosed in the flux surface with respect to Psi
  real(DP),pointer  :: dVolume_dRho_Tor(:,:) => null()     ! /dVolume_dRho_Tor - Radial derivative of the volume enclosed in the flux surface with respect to Rho_Tor
  real(DP),pointer  :: Area(:,:) => null()     ! /Area - Cross-sectional area of the flux surface
  real(DP),pointer  :: dArea_dPsi(:,:) => null()     ! /dArea_dPsi - Radial derivative of the cross-sectional area of the flux surface with respect to Psi
  real(DP),pointer  :: Surface(:,:) => null()     ! /Surface - Surface area of the toroidal flux surface
  real(DP),pointer  :: Trapped_Fraction(:,:) => null()     ! /Trapped_Fraction - Trapped particle fraction
  real(DP),pointer  :: gm1(:,:) => null()     ! /gm1 - Flux surface averaged 1/R^2
  real(DP),pointer  :: gm2(:,:) => null()     ! /gm2 - Flux surface averaged grad_rho^2/R^2
  real(DP),pointer  :: gm3(:,:) => null()     ! /gm3 - Flux surface averaged grad_rho^2
  real(DP),pointer  :: gm4(:,:) => null()     ! /gm4 - Flux surface averaged 1/B^2
  real(DP),pointer  :: gm5(:,:) => null()     ! /gm5 - Flux surface averaged B^2
  real(DP),pointer  :: gm6(:,:) => null()     ! /gm6 - Flux surface averaged grad_rho^2/B^2
  real(DP),pointer  :: gm7(:,:) => null()     ! /gm7 - Flux surface averaged grad_rho
  real(DP),pointer  :: gm8(:,:) => null()     ! /gm8 - Flux surface averaged R
  real(DP),pointer  :: gm9(:,:) => null()     ! /gm9 - Flux surface averaged 1/R
  real(DP),pointer  :: B_Average(:,:) => null()     ! /B_Average - Flux surface averaged B
  real(DP),pointer  :: B_Min(:,:) => null()     ! /B_Min - Minimum(B) on the flux surface
  real(DP),pointer  :: B_Max(:,:) => null()     ! /B_Max - Maximum(B) on the flux surface
endtype

type ids_Equilibrium_Profiles_2D_Grid  !    Definition of the 2D grid
  real(DP),pointer  :: Dim1(:,:) => null()     ! /Dim1 - First dimension values
  real(DP),pointer  :: Dim2(:,:) => null()     ! /Dim2 - Second dimension values
endtype

type ids_Equilibrium_Profiles_2D  !    Equilibrium 2D profiles in the poloidal plane
  character(len=132), dimension(:), pointer ::Grid_Type => null()       ! /Grid_Type - Selection of one of a set of grid types. 1-rectangular (R,Z) grid, in this case the position arrays 
  type (ids_Equilibrium_Profiles_2D_Grid) :: Grid  ! /Grid - Definition of the 2D grid
  real(DP),pointer  :: R(:,:,:) => null()     ! /R - Values of the major radius on the grid
  real(DP),pointer  :: Z(:,:,:) => null()     ! /Z - Values of the Height on the grid
  real(DP),pointer  :: Psi(:,:,:) => null()     ! /Psi - Values of the poloidal flux at the grid in the poloidal plane
  real(DP),pointer  :: Theta(:,:,:) => null()     ! /Theta - Values of the poloidal angle on the grid
  real(DP),pointer  :: Phi(:,:,:) => null()     ! /Phi - Toroidal flux
  real(DP),pointer  :: J_Tor(:,:,:) => null()     ! /J_Tor - Toroidal plasma current density
  real(DP),pointer  :: J_Parallel(:,:,:) => null()     ! /J_Parallel - Parallel (to magnetic field) plasma current density
  real(DP),pointer  :: B_R(:,:,:) => null()     ! /B_R - R component of the poloidal magnetic field
  real(DP),pointer  :: B_Z(:,:,:) => null()     ! /B_Z - Z component of the poloidal magnetic field
  real(DP),pointer  :: B_Tor(:,:,:) => null()     ! /B_Tor - Toroidal component of the magnetic field
endtype

type ids_Equilibrium_Coordinate_System  !    Flux surface coordinate system on a square grid of flux and poloidal angle
  character(len=132), dimension(:), pointer ::Grid_Type => null()       ! /Grid_Type - Type of coordinate system
  type (ids_Equilibrium_Profiles_2D_Grid) :: Grid  ! /Grid - Definition of the 2D grid
  real(DP),pointer  :: R(:,:,:) => null()     ! /R - Values of the major radius on the grid
  real(DP),pointer  :: Z(:,:,:) => null()     ! /Z - Values of the Height on the grid
  real(DP),pointer  :: Jacobian(:,:,:) => null()     ! /Jacobian - Jacobian of the coordinate system
  real(DP),pointer  :: g_11(:,:,:) => null()     ! /g_11 - metric coefficients g_11; g_ij=g^ij are contravariant metric tensor for the grid described by grid_t
  real(DP),pointer  :: g_12(:,:,:) => null()     ! /g_12 - metric coefficients g_12; g_ij=g^ij are contravariant metric tensor for the grid described by grid_t
  real(DP),pointer  :: g_13(:,:,:) => null()     ! /g_13 - metric coefficients g_13; g_ij=g^ij are contravariant metric tensor for the grid described by grid_t
  real(DP),pointer  :: g_22(:,:,:) => null()     ! /g_22 - metric coefficients g_22; g_ij=g^ij are contravariant metric tensor for the grid described by grid_t
  real(DP),pointer  :: g_23(:,:,:) => null()     ! /g_23 - metric coefficients g_23; g_ij=g^ij are contravariant metric tensor for the grid described by grid_t
  real(DP),pointer  :: g_33(:,:,:) => null()     ! /g_33 - metric coefficients g_33; g_ij=g^ij are contravariant metric tensor for the grid described by grid_t
endtype

type ids_Equilibrium  !    Description of a 2D, axi-symmetric, tokamak equilibrium; result of an equilibrium code.
  type (ids_IDS_Properties) :: IDS_Properties  ! /Equilibrium/IDS_Properties - 
  type (ids_Equilibrium_Boundary) :: Boundary  ! /Equilibrium/Boundary - Description of the plasma boundary
  type (ids_Equlibrium_Global) :: Global  ! /Equilibrium/Global - 0D parameters of the equilibrium
  type (ids_Equilibrium_Profiles_1D) :: Profiles_1D  ! /Equilibrium/Profiles_1D - Equilibrium profiles (1D radial grid) as a function of the poloidal flux
  type (ids_Equilibrium_Profiles_2D),pointer :: Profiles_2D(:) => null()  ! /Equilibrium/Profiles_2D(i) - Equilibrium 2D profiles in the poloidal plane. Multiple 2D representations of the equilibrium can be
  type (ids_Equilibrium_Coordinate_System) :: Coordinate_System  ! /Equilibrium/Coordinate_System - Flux surface coordinate system on a square grid of flux and poloidal angle
  type (ids_Code_Parameters) :: Code_Parameters  ! /Equilibrium/Code_Parameters - 
  real(DP), pointer  :: Timebase(:) => null()  ! Timebase
endtype

! ***********  Include Magnetics/DD_Magnetics.xsd
! SPECIAL STRUCTURE Data / Timebase
type ids_Magnetics_Flux_Loops_Flux  !    Measured flux.
  real(DP), pointer  :: Data(:) => null()     ! /Flux - Measured flux.
  real(DP), pointer  :: Timebase(:) => null()  ! Timebase
endtype
 
type ids_Magnetics_Flux_Loops  !    Flux loops
  character(len=132), dimension(:), pointer ::Name => null()       ! /Name - Name of the flux loop
  character(len=132), dimension(:), pointer ::Identifier => null()       ! /Identifier - ID of the flux loop
  real(DP),pointer  :: Position_RZPhi(:,:) => null()     ! /Position_RZPhi - List of (R,Z,phi) points defining the position of the loop (see data structure documentation FLUXLOO
  type (ids_Magnetics_Flux_Loops_Flux) :: Flux  ! /Flux - Measured flux.
endtype

! SPECIAL STRUCTURE Data / Timebase
type ids_Magnetics_Bpol_Probes_Field  !    Measured magnetic field
  real(DP), pointer  :: Data(:) => null()     ! /Field - Measured magnetic field
  real(DP), pointer  :: Timebase(:) => null()  ! Timebase
endtype
 
type ids_Magnetics_Bpol_Probes  !    Poloidal field probes
  character(len=132), dimension(:), pointer ::Name => null()       ! /Name - Name of the probe
  character(len=132), dimension(:), pointer ::Identifier => null()       ! /Identifier - ID of the probe
  real(DP),pointer  :: Centre_Position_RZPhi(:) => null()     ! /Centre_Position_RZPhi - R, Z, Phi position of coil centre   
  real(DP)  :: Poloidal_Angle=-9.0D40       ! /Poloidal_Angle - Poloidal angle of coil orientation   
  real(DP)  :: Toroidal_Angle=-9.0D40       ! /Toroidal_Angle - Toroidal angle of coil orientation (0 if fully in the poloidal plane)    
  real(DP)  :: Area=-9.0D40       ! /Area - Area of the coil   
  real(DP)  :: Length=-9.0D40       ! /Length - Length of the coil
  integer  :: Turns=-999999999       ! /Turns - Turns in the coil
  type (ids_Magnetics_Bpol_Probes_Field) :: Field  ! /Field - Measured magnetic field
endtype

! SPECIAL STRUCTURE Data / Timebase
type ids_Magnetics_Processed_Data_Ip  !    Plasma current. Positive sign means anti-clockwise when viewed from above
  real(DP), pointer  :: Data(:) => null()     ! /Ip - Plasma current. Positive sign means anti-clockwise when viewed from above
  real(DP), pointer  :: Timebase(:) => null()  ! Timebase
endtype
 
! SPECIAL STRUCTURE Data / Timebase
type ids_Magnetics_Processed_Data_Diamagnetic_Flux  !    Diamagnetic flux
  real(DP), pointer  :: Data(:) => null()     ! /Diamagnetic_Flux - Diamagnetic flux
  real(DP), pointer  :: Timebase(:) => null()  ! Timebase
endtype
 
type ids_Magnetics_Processed_Data  !    Processed quantities derived from the magnetic measurements.
  character(len=132), dimension(:), pointer ::Method => null()       ! /Method - Name of the data processing method
  type (ids_Magnetics_Processed_Data_Ip) :: Ip  ! /Ip - Plasma current. Positive sign means anti-clockwise when viewed from above
  type (ids_Magnetics_Processed_Data_Diamagnetic_Flux) :: Diamagnetic_Flux  ! /Diamagnetic_Flux - Diamagnetic flux
endtype

type ids_Magnetics  !    Magnetic diagnostics for equilibrium identification and plasma shape control.
  type (ids_IDS_Properties) :: IDS_Properties  ! /Magnetics/IDS_Properties - 
  type (ids_Magnetics_Flux_Loops),pointer :: Flux_Loops(:) => null()  ! /Magnetics/Flux_Loops(i) - Flux loops
  type (ids_Magnetics_Bpol_Probes),pointer :: Bpol_Probes(:) => null()  ! /Magnetics/Bpol_Probes(i) - Poloidal field probes
  type (ids_Magnetics_Processed_Data),pointer :: Processed_Data(:) => null()  ! /Magnetics/Processed_Data(i) - Processed quantities derived from the magnetic measurements.
  type (ids_Code_Parameters) :: Code_Parameters  ! /Magnetics/Code_Parameters - 
  real(DP), pointer  :: Timebase(:) => null()  ! Timebase
endtype

! ***********  Include PF/DD_PF.xsd
type ids_PF_Vacuum_Model  !    Electromagnetic Vacuum Model of the PF System, including both active and passive coils
  character(len=132), dimension(:), pointer ::Names => null()       ! /Names - Names of circuits [Active;Passive]
  real(DP),pointer  :: Inductance_Matrix(:,:) => null()     ! /Inductance_Matrix - Inductance matrix between circuits
  real(DP),pointer  :: Resistance_Vector(:) => null()     ! /Resistance_Vector - Resistances of PF coils
endtype

type ids_PF_Passive_Loops  !    Passive axisymmetric conductor description in the form of non-connected loops; any connected loops are expressed as active coil ci
  character(len=132), dimension(:), pointer ::Names => null()       ! /Names - Names of the loops
  real(DP),pointer  :: Areas(:) => null()     ! /Areas - Surface areas of the passive loops
  real(DP),pointer  :: Resistances(:) => null()     ! /Resistances - Passive elements resistances
  type (ids_Outline_2D_Geometry_STATIC),pointer :: Geometries(:) => null()  ! /Geometries(i) - Shapes of the Passive Loops
  real(DP),pointer  :: Currents(:,:) => null()     ! /Currents - Passive Loops currents (all Passive Loops are assumed to share the same timebase)
  real(DP), pointer  :: Timebase(:) => null()  ! Timebase
endtype

! SPECIAL STRUCTURE Data / Timebase
type ids_PF_Supplies_Voltage  !    Voltages at the supply output
  real(DP), pointer  :: Data(:) => null()     ! /Voltage - Voltages at the supply output
  real(DP), pointer  :: Timebase(:) => null()  ! Timebase
endtype
 
! SPECIAL STRUCTURE Data / Timebase
type ids_PF_Supplies_Current  !    Current at the supply output, defined positive if it flows from point 1 to point 2 of the component in the pfcircuit description
  real(DP), pointer  :: Data(:) => null()     ! /Current - Current at the supply output, defined positive if it flows from point 1 to point 2 of the component 
  real(DP), pointer  :: Timebase(:) => null()  ! Timebase
endtype
 
type ids_PF_Supplies  !    PF power supplies
  character(len=132), dimension(:), pointer ::Name => null()       ! /Name - Name of the PF supply
  character(len=132), dimension(:), pointer ::Identifier => null()       ! /Identifier - Identifier of the supply
  integer  :: Type=-999999999       ! /Type - Type of the supply; TBD add free description of non-linear power supplies   
  real(DP)  :: Resistance=-9.0D40       ! /Resistance - Power supply internal resistance   
  real(DP)  :: Delay=-9.0D40       ! /Delay - Pure delay in the supply
  real(DP),pointer  :: Filter_Numerator(:) => null()     ! /Filter_Numerator - Coefficients of the numerator, in increasing order : a0 + a1*s + ... + an*s^n; used for a linear sup
  real(DP),pointer  :: Filter_Denominator(:) => null()     ! /Filter_Denominator - Coefficients of the denominator, in increasing order : b0 + b1*s + ... + bm*s^m; used for a linear s   
  real(DP)  :: Current_Limit_Max=-9.0D40       ! /Current_Limit_Max - Maximum current in the supply   
  real(DP)  :: Current_Limit_Min=-9.0D40       ! /Current_Limit_Min - Minimum current in the supply   
  real(DP)  :: Voltage_Limit_Max=-9.0D40       ! /Voltage_Limit_Max - Maximum voltage from the supply   
  real(DP)  :: Voltage_Limit_Min=-9.0D40       ! /Voltage_Limit_Min - Minimum voltage from the supply   
  real(DP)  :: Current_Limiter_Gain=-9.0D40       ! /Current_Limiter_Gain - Gain to prevent overcurrent in a linear model of the supply   
  real(DP)  :: Energy_Limit_Max=-9.0D40       ! /Energy_Limit_Max - Maximum energy to be dissipated in the supply during a pulse
  character(len=132), dimension(:), pointer ::Nonlinear_Model => null()       ! /Nonlinear_Model - Description of the nonlinear transfer function of the supply
  type (ids_PF_Supplies_Voltage) :: Voltage  ! /Voltage - Voltages at the supply output
  type (ids_PF_Supplies_Current) :: Current  ! /Current - Current at the supply output, defined positive if it flows from point 1 to point 2 of the component 
endtype

! SPECIAL STRUCTURE Data / Timebase
type ids_PF_Circuits_Voltage  !    Voltage on the circuit
  real(DP), pointer  :: Data(:) => null()     ! /Voltage - Voltage on the circuit
  real(DP), pointer  :: Timebase(:) => null()  ! Timebase
endtype
 
! SPECIAL STRUCTURE Data / Timebase
type ids_PF_Circuits_Current  !    Current in the circuit
  real(DP), pointer  :: Data(:) => null()     ! /Current - Current in the circuit
  real(DP), pointer  :: Timebase(:) => null()  ! Timebase
endtype
 
type ids_PF_Circuits  !    Circuits, connecting multiple PF coils to multiple supplies, defining the current and voltage relationships in the system
  character(len=132), dimension(:), pointer ::Name => null()       ! /Name - Name of the circuit
  character(len=132), dimension(:), pointer ::Identifier => null()       ! /Identifier - ID of the circuit
  character(len=132), dimension(:), pointer ::Type => null()       ! /Type - Type of the circuit
  real(DP),pointer  :: Connections(:,:) => null()     ! /Connections - Description of the supplies and coils connections (nodes) across the circuit. The matrix describing 
  type (ids_PF_Circuits_Voltage) :: Voltage  ! /Voltage - Voltage on the circuit
  type (ids_PF_Circuits_Current) :: Current  ! /Current - Current in the circuit
endtype

type ids_PF_Coils_Elements  !    Each PF coil is comprised of a number of cross-section elements described  individually
  character(len=132), dimension(:), pointer ::Name => null()       ! /Name - Name of this element of this coil
  character(len=132), dimension(:), pointer ::Identifier => null()       ! /Identifier - Identifier of this element of this coil
  integer  :: Turns_With_Sign=-999999999       ! /Turns_With_Sign - Number of effective turns in the element for calculating magnetic fields of the coil; includes the s   
  real(DP)  :: Area=-9.0D40       ! /Area - Cross-sectional areas of the element
  type (ids_Outline_2D_Geometry_STATIC) :: Geometry  ! /Geometry - Cross-sectional shape of the element
endtype

! SPECIAL STRUCTURE Data / Timebase
type ids_PF_Coils_Current  !    Current in the coil
  real(DP), pointer  :: Data(:) => null()     ! /Current - Current in the coil
  real(DP), pointer  :: Timebase(:) => null()  ! Timebase
endtype
 
! SPECIAL STRUCTURE Data / Timebase
type ids_PF_Coils_Voltage  !    Voltage on the coil terminals
  real(DP), pointer  :: Data(:) => null()     ! /Voltage - Voltage on the coil terminals
  real(DP), pointer  :: Timebase(:) => null()  ! Timebase
endtype
 
type ids_PF_Coils  !    Active PF coils
  character(len=132), dimension(:), pointer ::Name => null()       ! /Name - Name of the coil
  character(len=132), dimension(:), pointer ::Identifier => null()       ! /Identifier - Alphanumeric identifier of coils used for convenience   
  real(DP)  :: Resistance=-9.0D40       ! /Resistance - Coil resistance   
  real(DP)  :: Energy_Limit_Max=-9.0D40       ! /Energy_Limit_Max - Maximum Energy to be dissipated in the coil
  type (ids_PF_Coils_Elements),pointer :: Elements(:) => null()  ! /Elements(i) - Each PF coil is comprised of a number of cross-section elements described  individually
  type (ids_PF_Coils_Current) :: Current  ! /Current - Current in the coil
  type (ids_PF_Coils_Voltage) :: Voltage  ! /Voltage - Voltage on the coil terminals
endtype

type ids_PF_Vertical_Forces  !    Vertical forces on the axisymmetric PF coil system
  character(len=132), dimension(:), pointer ::Names => null()       ! /Names - Names of the Force combinations
  real(DP),pointer  :: Combinations(:,:) => null()     ! /Combinations - Coils involved in the Force combinations
  real(DP),pointer  :: Limits_Max(:) => null()     ! /Limits_Max - Vertical Force combinations limits
  real(DP),pointer  :: Limits_Min(:) => null()     ! /Limits_Min - Vertical Force combination limit
  real(DP),pointer  :: Forces(:,:) => null()     ! /Forces - Force combinations. All forces are assumed to be on the same timebase
  real(DP), pointer  :: Timebase(:) => null()  ! Timebase
endtype

type ids_PF  !    Description of the axisymmetric active poloidal field (PF) coils, passive conductors, currents flowing in those and mutual electro
  type (ids_IDS_Properties) :: IDS_Properties  ! /PF/IDS_Properties - 
  type (ids_PF_Coils),pointer :: Coils(:) => null()  ! /PF/Coils(i) - Active PF coils
  type (ids_PF_Vertical_Forces) :: Vertical_Forces  ! /PF/Vertical_Forces - Vertical forces on the axisymmetric PF coil system
  type (ids_PF_Circuits),pointer :: Circuits(:) => null()  ! /PF/Circuits(i) - Circuits, connecting multiple PF coils to multiple supplies, defining the current and voltage relati
  type (ids_PF_Supplies),pointer :: Supplies(:) => null()  ! /PF/Supplies(i) - PF power supplies
  type (ids_PF_Passive_Loops) :: Passive_Loops  ! /PF/Passive_Loops - Passive axisymmetric conductor description in the form of non-connected loops; any connected loops a
  type (ids_PF_Vacuum_Model) :: Vacuum_Model  ! /PF/Vacuum_Model - Electromagnetic Vacuum Model of the PF System, including both active and passive coils
  type (ids_Code_Parameters) :: Code_Parameters  ! /PF/Code_Parameters - 
  real(DP), pointer  :: Timebase(:) => null()  ! Timebase
endtype

! ***********  Include SDN/DD_SDN.xsd
type ids_SDN_Topic_List  !    List of the groups of signals used at different reading and writing points
  character(len=132), dimension(:), pointer ::Names => null()       ! /Names - Names of the group of SDN signals
  integer,pointer  :: Indices(:) => null()      ! /Indices - Indices into the current SDN allocated list; it must be updated when the allocated list is changed
endtype

type ids_SDN_Allocatable_Signals  !    Dictionary of the signal names and definitions which can be allocated to the SDN
  character(len=132), dimension(:), pointer ::Names => null()       ! /Names - Names of all the allocatable signals
  character(len=132), dimension(:), pointer ::Definitions  => null()       ! /Definitions  - Definitions of the allocatable signals
  integer,pointer  :: Ip_Normalise(:) => null()      ! /Ip_Normalise - Is this signal multiplied by Ip to generate a control variable; this usage is specific to magnetic c
endtype

type ids_SDN  !    Description of the Synchronous Data Network parameters and the signals on it
  type (ids_IDS_Properties) :: IDS_Properties  ! /SDN/IDS_Properties - 
  type (ids_SDN_Allocatable_Signals) :: Allocatable_Signals  ! /SDN/Allocatable_Signals - Dictionary of the signal names and definitions which can be allocated to the SDN
  character(len=132), dimension(:), pointer ::Allocated_Signals => null()       ! /SDN/Allocated_Signals - Allocation of signal names to positions in the SDN
  real(DP), pointer  :: Timebase(:) => null()  ! Timebase
  real(DP),pointer  :: Signals(:,:) => null()     ! /SDN/Signals - Data on the SDN
  integer  :: N_Topic_List=-999999999       ! /SDN/N_Topic_List - Number items in the vector of structures
  type (ids_SDN_Topic_List),pointer :: Topic_List(:) => null()  ! /SDN/Topic_List(i) - List of the groups of signals used at different reading and writing points
endtype

! ***********  Include TF/DD_TF.xsd
! SPECIAL STRUCTURE Data / Timebase
type ids_TF_Current  !    Current in the coil
  real(DP), pointer  :: Data(:) => null()     ! /TF/Current - Current in the coil
  real(DP), pointer  :: Timebase(:) => null()  ! Timebase
endtype
 
! SPECIAL STRUCTURE Data / Timebase
type ids_TF_Voltage  !    Voltage on the coil terminals
  real(DP), pointer  :: Data(:) => null()     ! /TF/Voltage - Voltage on the coil terminals
  real(DP), pointer  :: Timebase(:) => null()  ! Timebase
endtype
 
! SPECIAL STRUCTURE Data / Timebase
type ids_TF_B_Tor_Vacuum_R  !    Vacuum field times major radius in the toroidal field magnet. Positive sign means anti-clockwise when viewed from above
  real(DP), pointer  :: Data(:) => null()     ! /TF/B_Tor_Vacuum_R - Vacuum field times major radius in the toroidal field magnet. Positive sign means anti-clockwise whe
  real(DP), pointer  :: Timebase(:) => null()  ! Timebase
endtype
 
type ids_TF  !    Toroidal field coils (all assumed to be identical). If individual description of coil is needed, use the same description as in PF
  type (ids_IDS_Properties) :: IDS_Properties  ! /TF/IDS_Properties - 
  integer  :: N_Turns=-999999999       ! /TF/N_Turns - Number of total turns in a toroidal field coil
  integer  :: N_Coils=-999999999       ! /TF/N_Coils - Number of coils around the tokamak
  type (ids_TF_Current) :: Current  ! /TF/Current - Current in the coil
  type (ids_TF_Voltage) :: Voltage  ! /TF/Voltage - Voltage on the coil terminals
  type (ids_TF_B_Tor_Vacuum_R) :: B_Tor_Vacuum_R  ! /TF/B_Tor_Vacuum_R - Vacuum field times major radius in the toroidal field magnet. Positive sign means anti-clockwise whe
  real(DP), pointer  :: Timebase(:) => null()  ! Timebase
endtype

end module

