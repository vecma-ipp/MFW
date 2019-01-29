MODULE GAUSIAN_SRC


CONTAINS
!-------------------------------------------------------!
!-------------------------------------------------------!

  SUBROUTINE GAUSIAN_SOURCES    (COREPROF, EQUILIBRIUM, CORESOURCE, code_parameters) 

!-------------------------------------------------------!
!     This routine provides dummy source for the        !
!     ETS workflow.                                     !
!-------------------------------------------------------!
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     Denis.Kalupin@efda.org              !
!                                                       !
!     Comments:     input parameter list is specified   !
!                   in "source_dummy.xml" file.         !
!                                                       !
!                   output CORESOURCE CPO is            !
!                   allocated inside the module         !
!                                                       !
!-------------------------------------------------------!


      USE  EUITM_SCHEMAS
      USE  EUITM_ROUTINES
      USE  ALLOCATE_DEALLOCATE
      USE  ITM_TYPES
      USE  COPY_STRUCTURES
      USE  DEALLOCATE_STRUCTURES

      IMPLICIT NONE


      INTEGER                           :: ifail
 

! +++ CPO derived types:
      TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM(:)      !input CPO with geometry quantities from previous time
      TYPE (TYPE_COREPROF),    POINTER  :: COREPROF(:)         !input CPO with internal ETS parameters profiles from previous time
      TYPE (TYPE_CORESOURCE),  POINTER  :: CORESOURCE(:)       !output CPO with sources
      TYPE (TYPE_PARAM)                 :: code_parameters


! +++ Local variables:
      REAL(R8)                          :: TIME
      REAL(R8),            ALLOCATABLE  :: AMN(:)
      REAL(R8),            ALLOCATABLE  :: ZN(:)
      REAL(R8),            ALLOCATABLE  :: ZION(:)
      REAL(R8),            ALLOCATABLE  :: RHO(:)
      REAL(R8),            ALLOCATABLE  :: VPRIME(:)
      REAL(R8),            ALLOCATABLE  :: JNI(:)
      REAL(R8),            ALLOCATABLE  :: QEL(:)
      REAL(R8),            ALLOCATABLE  :: QION(:,:)
      REAL(R8),            ALLOCATABLE  :: SEL(:)
      REAL(R8),            ALLOCATABLE  :: SION(:,:)
      REAL(R8),            ALLOCATABLE  :: UION(:,:)
      REAL(R8)                          :: R0                  


! +++ Dimensions:
      INTEGER,               PARAMETER  :: NSLICE = 1           
      INTEGER                           :: NPSI                 
      INTEGER                           :: NRHO                
      INTEGER                           :: NNUCL              
      INTEGER                           :: NION               
      INTEGER                           :: NIMP                
      INTEGER,             ALLOCATABLE  :: NZIMP(:)           
      INTEGER                           :: NNEUT                 
      INTEGER,             ALLOCATABLE  :: NCOMP(:)           
      INTEGER,             ALLOCATABLE  :: NTYPE(:)           

      INTEGER                           :: NION2               
      INTEGER                           :: INUCL               
      INTEGER                           :: IION1               
      INTEGER                           :: IION2               


! +++ Set dimensions:
      NRHO                   = SIZE (COREPROF(1)%rho_tor, DIM=1)
      NPSI                   = SIZE (EQUILIBRIUM(1)%profiles_1d%rho_tor, DIM=1)
      NION2                  = 100 
      CALL GET_COMP_DIMENSIONS      (COREPROF(1)%COMPOSITIONS, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP)

! +++ Allocate output CPO:
      CALL ALLOCATE_CORESOURCE_CPO  (NSLICE,      NRHO, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, CORESOURCE)
      CALL DEALLOCATE_CPO           (CORESOURCE(1)%COMPOSITIONS)
      CALL COPY_CPO                 (COREPROF(1)%COMPOSITIONS, CORESOURCE(1)%COMPOSITIONS)

 
! +++ Allocate local variables:
      ALLOCATE      (       AMN(NION2)     )
      ALLOCATE      (        ZN(NION2)     )
      ALLOCATE      (      ZION(NION2)     )
      ALLOCATE      (       RHO(NRHO)      )
      ALLOCATE      (    VPRIME(NRHO)      )
      ALLOCATE      (       JNI(NRHO)      )
      ALLOCATE      (       QEL(NRHO)      )
      ALLOCATE      (      QION(NRHO,NION2))
      ALLOCATE      (       SEL(NRHO)      )
      ALLOCATE      (      SION(NRHO,NION2))
      ALLOCATE      (      UION(NRHO,NION2))
      

      AMN                               =  0.0_R8
      ZN                                =  0.0_R8
      ZION                              =  0.0_R8
      RHO                               =  0.0_R8
      VPRIME                            =  0.0_R8
      JNI                               =  0.0_R8
      QEL                               =  0.0_R8
      QION                              =  0.0_R8
      SEL                               =  0.0_R8
      SION                              =  0.0_R8
      UION                              =  0.0_R8



! +++ Save output in CPO:
      TIME                              =  COREPROF(1)%time     !time    [s]

      RHO                               =  COREPROF(1)%rho_tor  !rho     [m]

      R0                                =  EQUILIBRIUM(1)%global_param%toroid_field%r0

      CALL L3DERIV                  (EQUILIBRIUM(1)%profiles_1d%volume,   EQUILIBRIUM(1)%profiles_1d%rho_tor,  NPSI,    &
                                     VPRIME,                              RHO,                                 NRHO)


! +++ Calculate sources:
!                                    <<        input                >>  <<      output          >>
      CALL ADDITIONAL_SOURCE        (AMN, ZN, ZION, NRHO, RHO, R0, VPRIME, NION2, QEL, QION, SEL, SION, UION, JNI, code_parameters)

      

      NION2                         =  MIN(SIZE(AMN),SIZE(ZN),SIZE(ZION)) 


! +++ Save output in CPO:
      CORESOURCE(1)%time                                         =  TIME                 !time    [s]

      CORESOURCE(1)%VALUES(1)%rho_tor                            =  RHO                  !rho     [m]

      CORESOURCE(1)%VALUES(1)%j                                  =  JNI                  !j_ni    [A/m^2]

      CORESOURCE(1)%VALUES(1)%qe%exp                             =  QEL                  !Qe_exp  [W/m^3]
      CORESOURCE(1)%VALUES(1)%qe%imp                             =  0.0E0_R8             !Qe_imp  [1/m^3/s]

      CORESOURCE(1)%VALUES(1)%se%exp                             =  SEL                  !Se_exp  [1/m^3/s]
      CORESOURCE(1)%VALUES(1)%se%imp                             =  0.0E0_R8             !Se_imp  [1/s]

      DO IION1 = 1, NION
         INUCL    = CORESOURCE(1)%COMPOSITIONS%IONS(IION1)%nucindex
         DO IION2 = 1, NION2
          IF (ABS(AMN(IION2)   - CORESOURCE(1)%COMPOSITIONS%NUCLEI(INUCL)%amn)  .LE. 0.25  .AND.  &
              ABS(ZN(IION2)    - CORESOURCE(1)%COMPOSITIONS%NUCLEI(INUCL)%zn )  .LE. 0.25  .AND.  &
              ABS(ZION(IION2)  - CORESOURCE(1)%COMPOSITIONS%IONS(IION1)%zion )  .LE. 0.25) THEN

                CORESOURCE(1)%VALUES(1)%si%exp(:,IION1)          =  SION(:,IION2)        !Si_exp  [1/m^3/s]
                CORESOURCE(1)%VALUES(1)%si%imp(:,IION1)          =  0.0E0_R8             !Si_imp  [1/s]

                CORESOURCE(1)%VALUES(1)%qi%exp(:,IION1)          =  QION(:,IION2)        !Qi_exp  [W/m^3]
                CORESOURCE(1)%VALUES(1)%qi%imp(:,IION1)          =  0.0E0_R8             !Qi_imp  [1/m^3/s]
 
                CORESOURCE(1)%VALUES(1)%ui%exp(:,IION1)          =  UION(:,IION2)        !Ui_exp  [kg/m/s^2]
                CORESOURCE(1)%VALUES(1)%ui%imp(:,IION1)          =  0.0E0_R8             !Ui_imp  [kg/m^2/s]

            END IF
         END DO
      END DO



! +++ ADD IDENTIFIER TO OUTPUT CPO VALUES(1):
    ALLOCATE            (CORESOURCE(1)%VALUES(1)%sourceid%id(1))
    ALLOCATE            (CORESOURCE(1)%VALUES(1)%sourceid%description(1))
    CORESOURCE(1)%VALUES(1)%sourceid%id          = 'gaussian'
    CORESOURCE(1)%VALUES(1)%sourceid%flag        = 33
    CORESOURCE(1)%VALUES(1)%sourceid%description = 'Gaussian'



! +++ Deallocate local variables:
    IF(ALLOCATED(AMN))    DEALLOCATE    (AMN)
    IF(ALLOCATED(RHO))    DEALLOCATE    (RHO)
    IF(ALLOCATED(VPRIME)) DEALLOCATE    (VPRIME)
    IF(ALLOCATED(JNI))    DEALLOCATE    (JNI)
    IF(ALLOCATED(QEL))    DEALLOCATE    (QEL)
    IF(ALLOCATED(QION))   DEALLOCATE    (QION)
    IF(ALLOCATED(SEL))    DEALLOCATE    (SEL)
    IF(ALLOCATED(SION))   DEALLOCATE    (SION)
    IF(ALLOCATED(UION))   DEALLOCATE    (UION)
    IF(ALLOCATED(NZIMP))  DEALLOCATE    (NZIMP)
    IF(ALLOCATED(NCOMP))  DEALLOCATE    (NCOMP)
    IF(ALLOCATED(NTYPE))  DEALLOCATE    (NTYPE)


! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  



      RETURN


      END SUBROUTINE GAUSIAN_SOURCES  

!-------------------------------------------------------!
!-------------------------------------------------------!






!-------------------------------------------------------!
!-------------------------------------------------------!

      SUBROUTINE ADDITIONAL_SOURCE (AMN, ZN, ZION, NRHO, RHO, R0, VPRIME, NION, QEL, QION, SEL, SION, UION, JNI, code_parameters)

      USE ITM_CONSTANTS
      USE ITM_TYPES

      USE EUITM_ROUTINES
      USE EUITM_SCHEMAS
      USE EUITM_XML_PARSER  
      USE XML_FILE_READER  

      IMPLICIT NONE


! +++ Dimensions:
      INTEGER              :: I
      INTEGER              :: NRHO, IRHO
      INTEGER              :: NION


! +++ Local variables:
      REAL(R8)             :: RHO(NRHO)
      REAL(R8)             :: VPRIME(NRHO)

      REAL(R8)             :: AMN(NION)             
      REAL(R8)             :: ZN(NION)             
      REAL(R8)             :: ZION(NION)             

      REAL(R8)             :: JNI(NRHO)
      REAL(R8)             :: QEL(NRHO)
      REAL(R8)             :: QION(NRHO,NION)
      REAL(R8)             :: SEL(NRHO)
      REAL(R8)             :: SION(NRHO,NION)
      REAL(R8)             :: UION(NRHO,NION)
      REAL(R8)             :: R0                  

      REAL(R8)             :: GFUN(NRHO)
      REAL(R8)             :: A, C, W, INTFUN
      REAL(R8)             :: FTOT


! +++ Parameters from XML:

!     CURRENT:
      REAL(R8)             :: JNITOT                     !total noninductive current [A]
      REAL(R8)             :: RCURR                      !rho position of current profile maximum [m]
      REAL(R8)             :: FWCURR                     !A full width at half maximum of the current profiles [m]

!     ELECTRON HEATING:
      REAL(R8)             :: WTOT_EL                    !total energy input  [W]
      REAL(R8)             :: RHEAT_EL                   !rho position of heating profile maximum [m]
      REAL(R8)             :: FWHEAT_EL                  !A full width at half maximum of the heating profiles [m]

!     ION HEATING:
      REAL(R8)             :: WTOT(NION)                 !total energy input  [W]
      REAL(R8)             :: RHEAT(NION)                !rho position of heating profile maximum [m]
      REAL(R8)             :: FWHEAT(NION)               !A full width at half maximum of the heating profiles [m]

!     ELECTRONS:
      REAL(R8)             :: STOT_EL                    !total electron particle input [s^-1]
      REAL(R8)             :: RPART_EL                   !rho position of electron particle source profile maximum [m]
      REAL(R8)             :: FWPART_EL                  !A full width at half maximum of the electron particle source profiles [m]

!     IONS:
      REAL(R8)             :: STOT(NION)                 !total ion particle input [s^-1]
      REAL(R8)             :: RPART(NION)                !rho position of ion particle source profile maximum [m]
      REAL(R8)             :: FWPART(NION)               !A full width at half maximum of the ion particle source profiles [m]

!     MOMENTUM:
      REAL(R8)             :: UTOT(NION)                 !Total momentum [kg*m^2*s^-1]
      REAL(R8)             :: RMOM(NION)                 !rho position of momentum source profile maximum [m]
      REAL(R8)             :: FWMOM(NION)                !A full width at half maximum of the momentum source profiles [m]


! +++ Other
      LOGICAL,  SAVE       :: first = .TRUE.
      INTEGER              :: return_status
      TYPE (TYPE_PARAM)    :: code_parameters


!     CALL FILL_PARAM (code_parameters, 'XML/source_dummy.xml', '', 'XML/source_dummy.xsd')

! choose some defaults
      JNITOT       = 0.0_R8
      RCURR        = 0.0_R8  
      FWCURR       = 0.5_R8 * RHO(NRHO)
               
      WTOT_EL      = 0.0_R8
      RHEAT_EL     = 0.0_R8  
      FWHEAT_EL    = 0.5_R8 * RHO(NRHO)
               
      WTOT         = 0.0_R8
      RHEAT        = 0.0_R8
      FWHEAT       = 0.5_R8 * RHO(NRHO)
               
      STOT_EL      = 0.0_R8
      RPART_EL     = 0.0_R8  
      FWPART_EL    = 0.5_R8 * RHO(NRHO) 
               
      STOT         = 0.0_R8
      RPART        = 0.0_R8
      FWPART       = 0.5_R8 * RHO(NRHO)
               
      UTOT         = 0.0_R8
      RMOM         = 0.0_R8
      FWMOM        = 0.5_R8 * RHO(NRHO)

     CALL ASSIGN_DUMMY_SOURCES(code_parameters, return_status)
    
     IF (return_status /= 0) THEN
        WRITE(*,*) 'ERROR: Could not assign source multipliers.'
     END IF



!-------------------------------------------------------!
! +++ Ambipolarity check:

     IF (STOT_EL.NE.SUM(STOT*ZION))THEN
        WRITE(*,*)'=================================='
        WRITE(*,*)'=================================='
        WRITE(*,*)'== Your Gaussian sources are    =='
        WRITE(*,*)'== not ambipolar!               =='
        WRITE(*,*)'== This can break your worflow. =='
        WRITE(*,*)'=================================='
        WRITE(*,*)'=================================='
     END IF

!-------------------------------------------------------!
! +++ Current sources:

     IF(JNITOT .NE. 0.0_R8) THEN
        A        = RCURR
        C        = FWCURR/2.35482_R8
        W        = JNITOT 

        GFUN     = 1.0_R8 / (2.0_R8*ITM_PI)**0.5/ C   &
                   * EXP(-(RHO-A)**2 / 2.0_R8 / C**2) &
                   * VPRIME / 2.0_R8 / ITM_PI / RHO

        IF (RHO(1).EQ.0.0_R8)                         &
             GFUN(1)  = 1.0_R8 / (2.0_R8*ITM_PI)**0.5/ C   &
                        * EXP(-A**2 / 2.0_R8 / C**2)       &
                        * 2.0_R8 * ITM_PI * R0
 
        CALL INTEG(NRHO, RHO, GFUN, INTFUN)

        JNI      = 1.0_R8 / (2.0_R8*ITM_PI)**0.5/ C   &
                   * EXP(-(RHO-A)**2 / 2.0_R8 / C**2) &
                   * W / INTFUN
     ELSE
        JNI = 0.0_R8
     ENDIF

!-------------------------------------------------------!
! +++ Electron heating sources:

     IF(WTOT_EL .NE. 0.0_R8) THEN
        A        = RHEAT_EL
        C        = FWHEAT_EL/2.35482_R8
        W        = WTOT_EL 

        GFUN     = 1.0_R8 / (2.0_R8*ITM_PI)**0.5/ C   &
                   * EXP(-(RHO-A)**2 / 2.0_R8 / C**2) &
                   * VPRIME
 
        CALL INTEG(NRHO, RHO, GFUN, INTFUN)

        QEL      = 1.0_R8 / (2.0_R8*ITM_PI)**0.5/ C   &
                   * EXP(-(RHO-A)**2 / 2.0_R8 / C**2) &
                   * W / INTFUN 
     ELSE
        QEL = 0.0_R8
     ENDIF

!-------------------------------------------------------!
! +++ Individual ion heating sources:

     DO I = 1,NION

        IF(WTOT(I) .NE. 0.0_R8) THEN
           A        = RHEAT(I)
           C        = FWHEAT(I)/2.35482_R8
           W        = WTOT(I) 

           GFUN     = 1.0_R8 / (2.0_R8*ITM_PI)**0.5/ C   &
                      * EXP(-(RHO-A)**2 / 2.0_R8 / C**2) &
                      * VPRIME
 
           CALL INTEG(NRHO, RHO, GFUN, INTFUN)

           QION(:,I)= 1.0_R8 / (2.0_R8*ITM_PI)**0.5/ C   &
                      * EXP(-(RHO-A)**2 / 2.0_R8 / C**2) &
                      * W / INTFUN 
        ELSE
           QION(:,I) = 0.0_R8
        ENDIF

      END DO

!-------------------------------------------------------!
! +++ Individual electron sources:

      IF(RPART_EL .NE. 0.0) THEN
         A          = RPART_EL
         C          = FWPART_EL/2.35482_R8
         W          = STOT_EL 

         GFUN       = 1.0_R8 / (2.0_R8*ITM_PI)**0.5/ C   &
                      * EXP(-(RHO-A)**2 / 2.0_R8 / C**2) &
                      * VPRIME
 
         CALL INTEG(NRHO, RHO, GFUN, INTFUN)

         SEL        = 1.0_R8 / (2.0_R8*ITM_PI)**0.5/ C   &
                      * EXP(-(RHO-A)**2 / 2.0_R8 / C**2) &
                      * W / INTFUN
      ELSE
         SEL = 0.0_R8
      ENDIF

!-------------------------------------------------------!
! +++ Individual ion particle sources:

      DO I = 1,NION

         IF(STOT(I) .NE. 0.0_R8) THEN
            A        = RPART(I)
            C        = FWPART(I)/2.35482_R8
            W        = STOT(I) 

            GFUN     = 1.0_R8 / (2.0_R8*ITM_PI)**0.5/ C   &
                       * EXP(-(RHO-A)**2 / 2.0_R8 / C**2) &
                       * VPRIME
 
            CALL INTEG(NRHO, RHO, GFUN, INTFUN)

            SION(:,I)= 1.0_R8 / (2.0_R8*ITM_PI)**0.5/ C   &
                       * EXP(-(RHO-A)**2 / 2.0_R8 / C**2) &
                       * W / INTFUN
         ELSE
            SION(:,I) = 0.0_R8
         ENDIF

      END DO

!-------------------------------------------------------!
! +++ Individual momentum sources:

      DO I = 1,NION

         IF(RMOM(I) .NE. 0.0) THEN
            A        = RMOM(I)
            C        = FWMOM(I)/2.35482_R8
            W        = UTOT(I) 

            GFUN     = 1.0_R8 / (2.0_R8*ITM_PI)**0.5/ C   &
                       * EXP(-(RHO-A)**2 / 2.0_R8 / C**2) &
                       * VPRIME
 
            CALL INTEG(NRHO, RHO, GFUN, INTFUN)

            UION(:,I)= 1.0_R8 / (2.0_R8*ITM_PI)**0.5/ C   &
                       * EXP(-(RHO-A)**2 / 2.0_R8 / C**2) &
                       * W / INTFUN
         ELSE
            UION(:,I) = 0.0_R8
         ENDIF

      END DO


      RETURN


!-------------------------------------------------------!
!-------------------------------------------------------!



      CONTAINS
!-------------------------------------------------------!
!-------------------------------------------------------!
      SUBROUTINE ASSIGN_DUMMY_SOURCES(codeparameters, return_status)

!-------------------------------------------------------!
!     This subroutine calls the XML parser for          !
!     the dummy sources and assign the                  !
!     resulting values to the corresponding variables   !
!-------------------------------------------------------!
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     Denis.Kalupin@efda.org              !
!                                                       !
!     Comments:     ---                                 !
!                                                       !
!-------------------------------------------------------!
  
      USE ITM_TYPES
      USE EUITM_SCHEMAS
      USE EUITM_XML_PARSER  

      IMPLICIT NONE


      TYPE(type_param)                  :: codeparameters
      INTEGER(ITM_I4)                   :: return_status 

      TYPE(tree)                        :: parameter_list
      TYPE(element),        POINTER     :: temp_pointer
      INTEGER(ITM_I4)                   :: i, nparm, n_values
      CHARACTER(len = 132)              :: cname

      INTEGER                           :: n_data

      return_status          = 0      

!-- parse xml-string codeparameters%parameters

      WRITE(6,*) 'Calling euitm_xml_parse'
      CALL EUITM_XML_PARSE  (codeparameters, nparm, parameter_list)
      WRITE(6,*) 'Called euitm_xml_parse'

!-- assign variables

      temp_pointer => parameter_list%first

      outer: DO
         cname = char2str(temp_pointer%cname)   ! necessary for AIX
         SELECT CASE (cname)


!--   parameters overall
         CASE ("parameters")
            temp_pointer => temp_pointer%child
            CYCLE

!-----------------------------------------------------------
!--   Parameters for current source
         CASE ("currents")
            temp_pointer => temp_pointer%child
            CYCLE

         CASE ("JNITOT")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, JNITOT)

         CASE ("RCURR")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, RCURR)
  
         CASE ("FWCURR")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, FWCURR)


 
!-----------------------------------------------------------
!--   Parameters for electrons 
         CASE ("electrons")
            temp_pointer => temp_pointer%child
            CYCLE

!--   heating source
         CASE ("heating_el")
            temp_pointer => temp_pointer%child
            CYCLE

         CASE ("WTOT_el")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, WTOT_EL)
  
         CASE ("RHEAT_el")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, RHEAT_EL)

         CASE ("FWHEAT_el")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, FWHEAT_EL)

            
 
!--   Parameters for particle source
         CASE ("particles_el")
            temp_pointer => temp_pointer%child
            CYCLE

         CASE ("STOT_el")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, STOT_EL)
            
         CASE ("RPART_el")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, RPART_EL)

         CASE ("FWPART_el")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, FWPART_EL)
            
 
            
 
!-----------------------------------------------------------
!--   Parameters for ions 
         CASE ("ions")
            temp_pointer => temp_pointer%child
            CYCLE

!--   ion composition
         CASE ("composition")
            temp_pointer => temp_pointer%child
            CYCLE

         CASE ("AMN")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2real(char2str(temp_pointer%cvalue), AMN, n_data)
            
         CASE ("ZN")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2real(char2str(temp_pointer%cvalue), ZN, n_data)

         CASE ("ZION")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2real(char2str(temp_pointer%cvalue), ZION, n_data)
            

!--   heating source
         CASE ("heating")
            temp_pointer => temp_pointer%child
            CYCLE

         CASE ("WTOT")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2real(char2str(temp_pointer%cvalue), WTOT, n_data)
            
         CASE ("RHEAT")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2real(char2str(temp_pointer%cvalue), RHEAT, n_data)

         CASE ("FWHEAT")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2real(char2str(temp_pointer%cvalue), FWHEAT, n_data)
            
 
!--   Parameters for particle source
         CASE ("particles")
            temp_pointer => temp_pointer%child
            CYCLE

         CASE ("STOT")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2real(char2str(temp_pointer%cvalue), STOT, n_data)
            
         CASE ("RPART")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2real(char2str(temp_pointer%cvalue), RPART, n_data)

         CASE ("FWPART")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2real(char2str(temp_pointer%cvalue), FWPART, n_data)
            
 
!--   Parameters for momentum source
         CASE ("momentum")
            temp_pointer => temp_pointer%child
            CYCLE

         CASE ("UTOT")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2real(char2str(temp_pointer%cvalue), UTOT, n_data)
            
         CASE ("RMOM")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2real(char2str(temp_pointer%cvalue), RMOM, n_data)

         CASE ("FWMOM")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2real(char2str(temp_pointer%cvalue), FWMOM, n_data)
            
 
        CASE default
            WRITE(*, *) 'ERROR: invalid parameter', cname
            return_status = 1
            EXIT
         END SELECT


         DO
            IF (ASSOCIATED(temp_pointer%sibling)) THEN
               temp_pointer => temp_pointer%sibling
               EXIT
            END IF
            IF (ASSOCIATED(temp_pointer%parent, parameter_list%first )) &
                 EXIT outer
            IF (ASSOCIATED(temp_pointer%parent)) THEN
               temp_pointer => temp_pointer%parent
            ELSE
               WRITE(*, *) 'ERROR: broken list.'
               RETURN
            END IF
         END DO
      END DO outer
      

!  -- destroy tree
      CALL DESTROY_XML_TREE(parameter_list)


      RETURN


      END SUBROUTINE ASSIGN_DUMMY_SOURCES
!-------------------------------------------------------!
!-------------------------------------------------------!


      END  SUBROUTINE ADDITIONAL_SOURCE
!-------------------------------------------------------!
!-------------------------------------------------------!






!-------------------------------------------------------!
!-------------------------------------------------------!
!  This subroutine calculates integral of a function y(x)
!  from X=0
      SUBROUTINE INTEG(N,X,Y,INTY)


      USE ITM_TYPES
  
   
      IMPLICIT NONE
  
      INTEGER :: N                                          ! number of radial points (input)
      INTEGER :: I
  
      REAL (R8) :: X(N), &                                  ! argument array (input)
                   Y(N), &                                  ! function array (input)
                   INTY                                     ! integral value (output)
  
      INTY      = 0.0_R8
      DO i=2,N
        INTY    = INTY + (Y(I)+Y(I-1))*(X(I)-X(I-1))/2.E0_R8
      END DO
  
      RETURN

      END SUBROUTINE INTEG
!-------------------------------------------------------!
!-------------------------------------------------------!




!-------------------------------------------------------!
!-------------------------------------------------------!
  SUBROUTINE INTERPOLATE(ninput,rinput,finput,nout,rout,fout)

    ! +++ this subroutine interpolates profile Finput(rinput) to the new greed Fout(rout)

    USE itm_types

    IMPLICIT NONE

    INTEGER              :: i,ii,jj, ninput,nout
    ! Input: radii, function
    REAL (R8)            :: rinput(ninput),finput(ninput)
    ! Output: radii, function
    REAL (R8)            :: rout(nout),fout(nout)
    REAL (R8)            :: f(2),r,h1,h2,y1,y2,y3,a,b,c,z


    DO i=1,nout
       r=rout(i)
       IF(r.LE.rinput(2)) THEN
          h1=rinput(2)-rinput(1)
          h2=rinput(3)-rinput(2)
          y1=finput(1)
          y2=finput(2)
          y3=finput(3)
          a=((y1-y2)*h2+(y3-y2)*h1)/h1/h2/(h1+h2)
          b=((y3-y2)*h1*h1-(y1-y2)*h2*h2)/h1/h2/(h1+h2)
          c=y2
          z=a*(r-rinput(2))**2+b*(r-rinput(2))+c
          GOTO 100
       ENDIF
       IF(r.GT.rinput(2).AND.r.LE.rinput(ninput-1)) THEN
          DO ii=3,ninput-1
             IF(r.GT.rinput(ii-1).AND.r.LE.rinput(ii)) THEN
                DO jj=1,2
                   h1=rinput(ii-2+jj)-rinput(ii-3+jj)
                   h2=rinput(ii-1+jj)-rinput(ii-2+jj)
                   y1=finput(ii-3+jj)
                   y2=finput(ii-2+jj)
                   y3=finput(ii-1+jj)
                   a=((y1-y2)*h2+(y3-y2)*h1)/h1/h2/(h1+h2)
                   b=((y3-y2)*h1*h1-(y1-y2)*h2*h2)/h1/h2/(h1+h2)
                   c=y2
                   f(jj)=a*(r-rinput(ii-2+jj))**2+b*(r-rinput(ii-2+jj))+c
                ENDDO
                z=(f(1)*(rinput(ii)-r)+f(2)*(r-rinput(ii-1)))/       &
                     (rinput(ii)-rinput(ii-1))
             ENDIF
          ENDDO
          GOTO 100
       ENDIF
       IF(r.GE.rinput(ninput-1)) THEN
          h1=rinput(ninput-1)-rinput(ninput-2)
          h2=rinput(ninput)-rinput(ninput-1)
          y1=finput(ninput-2)
          y2=finput(ninput-1)
          y3=finput(ninput)
          a=((y1-y2)*h2+(y3-y2)*h1)/h1/h2/(h1+h2)
          b=((y3-y2)*h1*h1-(y1-y2)*h2*h2)/h1/h2/(h1+h2)
          c=y2
          z=a*(r-rinput(ninput-1))**2+b*(r-rinput(ninput-1))+c
          GOTO 100
       ENDIF
100    fout(i)=z
    ENDDO

!DPC
    IF(rinput(1).EQ.rout(1)) THEN
       IF(finput(1).NE.fout(1)) THEN
          WRITE(*,*) 'INTERPOLATION corrected for ',rinput(1),finput(1),fout(1)
          fout(1)=finput(1)
       ENDIF
    ENDIF
    RETURN

  END SUBROUTINE INTERPOLATE
!-------------------------------------------------------!
!-------------------------------------------------------!




!-------------------------------------------------------!
!-------------------------------------------------------!

END MODULE GAUSIAN_SRC







