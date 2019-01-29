MODULE PLASMA_COMPOSITION

CONTAINS
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
  SUBROUTINE SET_PLASMA_COMPOSITION (COREPROF_OUT,                          &
                                     NION,      NIMP,      NNEUT,           &
                                     AMN_ION,   ZN_ION,    Z_ION,           &
                                     AMN_IMP,   ZN_IMP,    MAXZ_IMP,        &
                                     NCOMP_IN,  NTYPE_IN,                   &
                                     NCOLD,     NTHERMAL,  NFAST,   NNBI) 


! +++ Declaration of variables: 
    use itm_types
    USE EUITM_SCHEMAS
    USE ALLOCATE_DEALLOCATE
    USE COPY_STRUCTURES
    USE DEALLOCATE_STRUCTURES



    IMPLICIT NONE

! +++ CPO derived types:
    TYPE (TYPE_COREPROF),     POINTER   :: COREPROF_OUT(:)        !output CPO slice 
    TYPE (TYPE_COMPOSITIONC), POINTER   :: COMPOSITIONC(:)

! +++ Input:
    INTEGER,                  PARAMETER  :: NOCUR = 1             !number of CPO ocurancies in the work flow
    INTEGER                              :: NNUCL,    INUCL       !number of nuclei species
    INTEGER                              :: NION,     IION        !number of ion species
    INTEGER                              :: NIMP,     IIMP        !number of impurity species
    INTEGER,                 ALLOCATABLE :: NZIMP(:)              !number of ionization states for each impurity
    INTEGER                              ::           IZIMP       
    INTEGER                              :: NNEUT,    INEUT       !number of neutrals species
    INTEGER,                 ALLOCATABLE :: NCOMP(:)              !number of components for each neutral
    INTEGER                              ::           ICOMP       
    INTEGER,                 ALLOCATABLE :: NTYPE(:)              !number of types for each neutral
    INTEGER                              ::           ITYPE       

    INTEGER                              :: NCOLD, NTHERMAL, NFAST, NNBI     

    REAL (R8),               ALLOCATABLE :: AMN_ION(:),  ZN_ION(:),  Z_ION(:)
    REAL (R8),               ALLOCATABLE :: AMN_IMP(:),  ZN_IMP(:),  MAXZ_IMP(:)

    INTEGER                              :: NCOMP_IN(:)
    INTEGER                              :: NTYPE_IN(:)

    INTEGER,                 ALLOCATABLE :: NUCINDEX_ION(:)
    INTEGER,                 ALLOCATABLE :: NUCINDEX_IMP(:)
    INTEGER,                 ALLOCATABLE :: NUCINDEX_NEUT(:)
    INTEGER                              :: NEUT_FLAG(4)
    CHARACTER                            :: NEUT_ID(4)
    CHARACTER                            :: NEUT_DESC(4)

    CHARACTER                            :: NEUTRALS(4)
    DATA NEUTRALS                           /'cold','thermal','fast','NBI'/
    INTEGER                              :: INTYPE(4), I, I_ION

! +++ Local:
    REAL (R8),              ALLOCATABLE  :: AMN(:), ZN(:)




! +++ Check consistency:
    IF(MIN(SIZE(AMN_ION), SIZE(ZN_ION),SIZE(Z_ION)).LT.NION) THEN
       WRITE (*,*) 'COMPOSITION INFORMATION IS NOT COMPETE FOR {1:NION}'
       STOP
       END IF
    IF(MIN(SIZE(AMN_IMP), SIZE(ZN_IMP),SIZE(MAXZ_IMP)).LT.NIMP) THEN
       WRITE (*,*) 'COMPOSITION INFORMATION IS NOT COMPETE FOR {1:NIMP}'
       STOP
       END IF



! +++ Define nuclei:
    ALLOCATE (AMN(NION+NIMP))
    ALLOCATE (ZN(NION+NIMP))
    ALLOCATE (NUCINDEX_ION(NION))
    ALLOCATE (NUCINDEX_IMP(NIMP))
    ALLOCATE (NUCINDEX_NEUT(NION+NIMP))


    NNUCL   = 0
    DO IION = 1,NION
       IF (AMN_ION(IION).GT.0._R8.AND.ZN_ION(IION).GT.0._R8) THEN
          IF (NNUCL.GE.1) THEN
             DO INUCL = 1, NNUCL
                IF (ABS(AMN(INUCL)-AMN_ION(IION)) .LE. 0.25 .AND. &
                    ABS(ZN(INUCL)-ZN_ION(IION)) .LE. 0.25)           THEN   !Nuclei already exist
                    NUCINDEX_ION(IION)  = INUCL
                    NUCINDEX_NEUT(IION) = INUCL
                    GOTO 10                                              
                END IF
             ENDDO
          ENDIF
          NNUCL               =  NNUCL + 1                                   !Add one more nuclei
          AMN(NNUCL)          =  AMN_ION(IION)
          ZN(NNUCL)           =  ZN_ION(IION)
          NUCINDEX_ION(IION)  =  NNUCL
          NUCINDEX_NEUT(IION) =  NNUCL
 10       CONTINUE
       END IF 
    END DO
    DO IIMP = 1,NIMP
       IF (AMN_IMP(IIMP).GT.0._R8.AND.ZN_IMP(IIMP).GT.0._R8) THEN
          IF (NNUCL.GE.1) THEN
             DO INUCL = 1, NNUCL
                IF (ABS(AMN(INUCL)-AMN_IMP(IIMP)) .LE. 0.25 .AND. &
                    ABS(ZN(INUCL)-ZN_IMP(IIMP)) .LE. 0.25)           THEN   !Nuclei already exist
                    NUCINDEX_IMP(IIMP)       = INUCL
                    NUCINDEX_NEUT(NION+IIMP) = INUCL
                    GOTO 12                                              
                END IF
             ENDDO
          ENDIF
          NNUCL                    =  NNUCL + 1                             !Add one more nuclei
          AMN(NNUCL)               =  AMN_IMP(IIMP)
          ZN(NNUCL)                =  ZN_IMP(IIMP)
          NUCINDEX_IMP(IIMP)       =  NNUCL
          NUCINDEX_NEUT(NION+IIMP) =  NNUCL
 12       CONTINUE
       END IF 
    END DO



! +++ Define impurity:
    ALLOCATE (NZIMP(NIMP))


    DO IIMP = 1,NIMP
       NZIMP(IIMP)   =  INT(MAXZ_IMP(IIMP))
    END DO



! +++ Define neutrals:
    if(nneut.gt.0) then
       ALLOCATE (NCOMP(NNEUT))
       ALLOCATE (NTYPE(NNEUT))
       NCOMP(1:NNEUT) = NCOMP_IN(1:NNEUT) 
       NTYPE(1:NNEUT) = NTYPE_IN(1:NNEUT)
       INTYPE (1) = NCOLD
       INTYPE (2) = NTHERMAL
       INTYPE (3) = NFAST
       INTYPE (4) = NNBI

       ITYPE =0
       DO I = 1, 4
          IF (INTYPE(I).EQ.1) THEN
             ITYPE = ITYPE +1
             NEUT_ID(ITYPE)        = NEUTRALS(I)
             NEUT_FLAG(ITYPE)      = I-1
             NEUT_DESC(ITYPE)      = NEUTRALS(I)
          END IF
       END DO
    end if



! +++ Allocate output CPO and internal derived types:
    CALL ALLOCATE_COMPOSITIONC_CPO (NOCUR, NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, COMPOSITIONC)



! +++ Fill oou new allocated CPO:
! +++ Nuclei:
         DO INUCL = 1, NNUCL
            COMPOSITIONC(1)%compositions%NUCLEI(INUCL)%zn                    = ZN(INUCL)
            COMPOSITIONC(1)%compositions%NUCLEI(INUCL)%amn                   = AMN(INUCL)
            COMPOSITIONC(1)%compositions%NUCLEI(INUCL)%label                 = " "
         END DO



! +++ Ions:
         DO IION = 1, NION
            COMPOSITIONC(1)%compositions%IONS(IION)%nucindex                 = NUCINDEX_ION(IION)
            COMPOSITIONC(1)%compositions%IONS(IION)%zion                     = Z_ION(IION)
            COMPOSITIONC(1)%compositions%IONS(IION)%imp_flag                 = 0
            COMPOSITIONC(1)%compositions%IONS(IION)%label                    = " "
         END DO



! +++ Impurities:
         DO IIMP = 1, NIMP
            COMPOSITIONC(1)%compositions%IMPURITIES(IIMP)%nucindex           = NUCINDEX_IMP(IIMP)
            I_ION                                                            = 0
            DO IION = 1, NION
               IF (NUCINDEX_IMP(IIMP).EQ.NUCINDEX_ION(IION)) I_ION           = IION
            END DO
            COMPOSITIONC(1)%compositions%IMPURITIES(IIMP)%i_ion              = I_ION
            COMPOSITIONC(1)%compositions%IMPURITIES(IIMP)%nzimp              = NZIMP(IIMP)
            DO IZIMP = 1, NZIMP(IIMP)
               COMPOSITIONC(1)%compositions%IMPURITIES(IIMP)%zmin(IZIMP)     = IZIMP
               COMPOSITIONC(1)%compositions%IMPURITIES(IIMP)%zmax(IZIMP)     = IZIMP
               COMPOSITIONC(1)%compositions%IMPURITIES(IIMP)%label(IZIMP)    = " "
            END DO
         END DO



! +++ Neutrals:
         DO INEUT = 1, NNEUT
            DO ICOMP = 1, NCOMP(INEUT)
               COMPOSITIONC(1)%compositions%NEUTRALSCOMP(INEUT)%NEUTCOMP(ICOMP)%nucindex     = NUCINDEX_NEUT(INEUT)
               COMPOSITIONC(1)%compositions%NEUTRALSCOMP(INEUT)%NEUTCOMP(ICOMP)%multiplicity = 1
            END DO
            DO ITYPE = 1, NTYPE(INEUT)
               COMPOSITIONC(1)%compositions%NEUTRALSCOMP(INEUT)%TYPE(ITYPE)%id               = NEUT_ID(ITYPE)
               COMPOSITIONC(1)%compositions%NEUTRALSCOMP(INEUT)%TYPE(ITYPE)%flag             = NEUT_FLAG(ITYPE)
               COMPOSITIONC(1)%compositions%NEUTRALSCOMP(INEUT)%TYPE(ITYPE)%description      = NEUT_DESC(ITYPE)
            END DO
         END DO



! +++ Copy composition to COREPROF
    ALLOCATE      (COREPROF_OUT(1))
    CALL COPY_CPO (COMPOSITIONC(1)%compositions, COREPROF_OUT(1)%compositions)



! +++ Deallocation
    CALL DEALLOCATE_CPO (COMPOSITIONC)

    DEALLOCATE    (AMN)
    DEALLOCATE    (ZN)
    DEALLOCATE    (NZIMP)
    if(nneut.gt.0) then
       DEALLOCATE    (NCOMP)
       DEALLOCATE    (NTYPE)
    endif
    DEALLOCATE    (NUCINDEX_ION)
    DEALLOCATE    (NUCINDEX_IMP)
    DEALLOCATE    (NUCINDEX_NEUT)


    RETURN 


    END SUBROUTINE SET_PLASMA_COMPOSITION
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  


END MODULE PLASMA_COMPOSITION
