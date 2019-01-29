MODULE euITM_ual_defs

USE ISO_C_BINDING

implicit none

integer, parameter, public :: EUITM_R8 = c_double

! Type definitions

 ! TYPE, BIND(C) :: type_UalComplex
 !      REAL(EUITM_R8) :: re=-9.0D40       ! real part
 !      REAL(EUITM_R8) :: im=-9.0D40       ! imaginary part
 ! END TYPE type_UalComplex


type type_UalComplex  !    
 real(EUITM_R8)  :: re=-9.0D40       ! real part
 real(EUITM_R8)  :: im=-9.0D40       ! imaginary part
endtype
       

CONTAINS

FUNCTION convertToUalComplex(stdComplex) result (ualComplex)  
    COMPLEX(EUITM_R8) :: stdComplex
    TYPE (type_UalComplex):: ualComplex

    ualComplex%re=REAL(stdComplex)
    ualComplex%im= AIMAG(stdComplex)
    RETURN
 END FUNCTION convertToUalComplex
 
FUNCTION convertArrayToUalComplex(inArray) RESULT (outArray)
    COMPLEX(EUITM_R8), DIMENSION(:),  ALLOCATABLE, TARGET, INTENT(in) :: inArray
    TYPE (type_UalComplex),DIMENSION(:), ALLOCATABLE, TARGET:: outArray
    
    INTEGER::sizeOfArray
    INTEGER::I
    
    sizeOfArray = SIZE(inArray)
    ALLOCATE(outArray(sizeofArray))
    do I = 1, sizeOfArray
        outArray(I)%re = REAL(inArray(I)) 
	outArray(I)%im = AIMAG(inArray(I))
    end do     
    RETURN
END FUNCTION convertArrayToUalComplex

FUNCTION convert1DArrayToUalComplex(inArray) result (outArray)
    COMPLEX(EUITM_R8), pointer:: inArray(:)
    TYPE (type_UalComplex), pointer :: outArray(:)

    INTEGER::sizeOfArray
    INTEGER, DIMENSION(1) :: shapeOfArray
    COMPLEX(EUITM_R8),                DIMENSION(:), ALLOCATABLE, TARGET:: tmp1DStdComplexArray
    TYPE (type_UalComplex), DIMENSION(:), ALLOCATABLE, TARGET:: tmp1DUalComplexArray

    sizeOfArray = SIZE(inArray)
    shapeOfArray = SHAPE(inArray)

    allocate(tmp1DStdComplexArray(sizeOfArray))
    allocate(tmp1DUalComplexArray(sizeOfArray))

    tmp1DStdComplexArray =  RESHAPE(inArray, (/sizeOfArray/))
    tmp1DUalComplexArray = convertArrayToUalComplex(tmp1DStdComplexArray)

    allocate(outArray(shapeOfArray(1)))
    
    outArray = RESHAPE(tmp1DUalComplexArray, shapeOfArray)

    deallocate(tmp1DStdComplexArray)
    deallocate(tmp1DUalComplexArray)

    RETURN
END FUNCTION convert1DArrayToUalComplex

FUNCTION convert2DArrayToUalComplex(inArray) result (outArray)
    COMPLEX(EUITM_R8), pointer:: inArray(:,:)
    TYPE (type_UalComplex), pointer :: outArray(:,:)

    INTEGER::sizeOfArray
    INTEGER, DIMENSION(2) :: shapeOfArray
    COMPLEX(EUITM_R8),                DIMENSION(:), ALLOCATABLE, TARGET:: tmp1DStdComplexArray
    TYPE (type_UalComplex), DIMENSION(:), ALLOCATABLE, TARGET:: tmp1DUalComplexArray

    sizeOfArray = SIZE(inArray)
    shapeOfArray = SHAPE(inArray)

    allocate(tmp1DStdComplexArray(sizeOfArray))
    allocate(tmp1DUalComplexArray(sizeOfArray))

    tmp1DStdComplexArray =  RESHAPE(inArray, (/sizeOfArray/))
    tmp1DUalComplexArray = convertArrayToUalComplex(tmp1DStdComplexArray)

    allocate(outArray(shapeOfArray(1),shapeOfArray(2)))
    
    outArray = RESHAPE(tmp1DUalComplexArray, shapeOfArray)


    deallocate(tmp1DStdComplexArray)
    deallocate(tmp1DUalComplexArray)


    RETURN
END FUNCTION convert2DArrayToUalComplex


FUNCTION convert3DArrayToUalComplex(inArray) result (outArray)
    COMPLEX(EUITM_R8), pointer:: inArray(:,:,:)
    TYPE (type_UalComplex), pointer :: outArray(:,:,:)

    INTEGER::sizeOfArray
    INTEGER, DIMENSION(3) :: shapeOfArray
    COMPLEX(EUITM_R8),                DIMENSION(:), ALLOCATABLE, TARGET:: tmp1DStdComplexArray
    TYPE (type_UalComplex), DIMENSION(:), ALLOCATABLE, TARGET:: tmp1DUalComplexArray

    sizeOfArray = SIZE(inArray)
    shapeOfArray = SHAPE(inArray)

    allocate(tmp1DStdComplexArray(sizeOfArray))
    allocate(tmp1DUalComplexArray(sizeOfArray))

    tmp1DStdComplexArray =  RESHAPE(inArray, (/sizeOfArray/))
    tmp1DUalComplexArray = convertArrayToUalComplex(tmp1DStdComplexArray)

    allocate(outArray(shapeOfArray(1),shapeOfArray(2),shapeOfArray(3)))
    
    outArray = RESHAPE(tmp1DUalComplexArray, shapeOfArray)

    deallocate(tmp1DStdComplexArray)
    deallocate(tmp1DUalComplexArray)

    RETURN
END FUNCTION convert3DArrayToUalComplex

FUNCTION convert4DArrayToUalComplex(inArray) result (outArray)
    COMPLEX(EUITM_R8), pointer:: inArray(:,:,:,:)
    TYPE (type_UalComplex), pointer :: outArray(:,:,:,:)

    INTEGER::sizeOfArray
    INTEGER, DIMENSION(4) :: shapeOfArray
    COMPLEX(EUITM_R8),                DIMENSION(:), ALLOCATABLE, TARGET:: tmp1DStdComplexArray
    TYPE (type_UalComplex), DIMENSION(:), ALLOCATABLE, TARGET:: tmp1DUalComplexArray

    sizeOfArray = SIZE(inArray)
    shapeOfArray = SHAPE(inArray)

    allocate(tmp1DStdComplexArray(sizeOfArray))
    allocate(tmp1DUalComplexArray(sizeOfArray))

    tmp1DStdComplexArray =  RESHAPE(inArray, (/sizeOfArray/))
    tmp1DUalComplexArray = convertArrayToUalComplex(tmp1DStdComplexArray)

    allocate(outArray(shapeOfArray(1),shapeOfArray(2),shapeOfArray(3), shapeOfArray(4)))
    
    outArray = RESHAPE(tmp1DUalComplexArray, shapeOfArray)

    deallocate(tmp1DStdComplexArray)
    deallocate(tmp1DUalComplexArray)

    RETURN
END FUNCTION convert4DArrayToUalComplex

FUNCTION convert5DArrayToUalComplex(inArray) result (outArray)
    COMPLEX(EUITM_R8),                POINTER:: inArray(:,:,:,:,:)
    TYPE (type_UalComplex), POINTER :: outArray(:,:,:,:,:)

    INTEGER::sizeOfArray
    INTEGER, DIMENSION(5) :: shapeOfArray
    COMPLEX(EUITM_R8),                DIMENSION(:), ALLOCATABLE, TARGET:: tmp1DStdComplexArray
    TYPE (type_UalComplex), DIMENSION(:), ALLOCATABLE, TARGET:: tmp1DUalComplexArray

    sizeOfArray = SIZE(inArray)
    shapeOfArray = SHAPE(inArray)

    allocate(tmp1DStdComplexArray(sizeOfArray))
    allocate(tmp1DUalComplexArray(sizeOfArray))

    tmp1DStdComplexArray =  RESHAPE(inArray, (/sizeOfArray/))
    tmp1DUalComplexArray = convertArrayToUalComplex(tmp1DStdComplexArray)

    allocate(outArray(shapeOfArray(1),shapeOfArray(2),shapeOfArray(3), shapeOfArray(4), shapeOfArray(5)))
    
    outArray = RESHAPE(tmp1DUalComplexArray, shapeOfArray)

    deallocate(tmp1DStdComplexArray)
    deallocate(tmp1DUalComplexArray)

    RETURN
END FUNCTION convert5DArrayToUalComplex

FUNCTION convert6DArrayToUalComplex(inArray) result (outArray)
    COMPLEX(EUITM_R8),                POINTER :: inArray(:,:,:,:,:,:)
    TYPE (type_UalComplex), POINTER :: outArray(:,:,:,:,:,:)

    INTEGER::sizeOfArray
    INTEGER, DIMENSION(6) :: shapeOfArray
    COMPLEX(EUITM_R8),                DIMENSION(:), ALLOCATABLE, TARGET:: tmp1DStdComplexArray
    TYPE (type_UalComplex), DIMENSION(:), ALLOCATABLE, TARGET:: tmp1DUalComplexArray

    sizeOfArray = SIZE(inArray)
    shapeOfArray = SHAPE(inArray)


    allocate(tmp1DStdComplexArray(sizeOfArray))
    allocate(tmp1DUalComplexArray(sizeOfArray))

    tmp1DStdComplexArray =  RESHAPE(inArray, (/sizeOfArray/))
    tmp1DUalComplexArray = convertArrayToUalComplex(tmp1DStdComplexArray)

    allocate(outArray(shapeOfArray(1),shapeOfArray(2),shapeOfArray(3), shapeOfArray(4), &
    shapeOfArray(5), shapeOfArray(6)))
    
    outArray = RESHAPE(tmp1DUalComplexArray, shapeOfArray)

    deallocate(tmp1DStdComplexArray)
    deallocate(tmp1DUalComplexArray)

    RETURN
END FUNCTION convert6DArrayToUalComplex

FUNCTION convert7DArrayToUalComplex(inArray) result (outArray)
    COMPLEX(EUITM_R8),                POINTER :: inArray(:,:,:,:,:,:,:)
    TYPE (type_UalComplex), POINTER :: outArray(:,:,:,:,:,:,:)

    INTEGER::sizeOfArray
    INTEGER, DIMENSION(7) :: shapeOfArray
    COMPLEX(EUITM_R8),                DIMENSION(:), ALLOCATABLE, TARGET:: tmp1DStdComplexArray
    TYPE (type_UalComplex), DIMENSION(:), ALLOCATABLE, TARGET:: tmp1DUalComplexArray

    sizeOfArray = SIZE(inArray)
    shapeOfArray = SHAPE(inArray)

    allocate(tmp1DStdComplexArray(sizeOfArray))
    allocate(tmp1DUalComplexArray(sizeOfArray))

    tmp1DStdComplexArray =  RESHAPE(inArray, (/sizeOfArray/))
    tmp1DUalComplexArray = convertArrayToUalComplex(tmp1DStdComplexArray)

    allocate(outArray(shapeOfArray(1),shapeOfArray(2),shapeOfArray(3), shapeOfArray(4), &
    shapeOfArray(5), shapeOfArray(6), shapeOfArray(7) ))
    
    outArray = RESHAPE(tmp1DUalComplexArray, shapeOfArray)

    deallocate(tmp1DStdComplexArray)
    deallocate(tmp1DUalComplexArray)

    RETURN
END FUNCTION convert7DArrayToUalComplex

FUNCTION convertToStdComplex(ualComplex) result (stdComplex)
    type (type_UalComplex), intent(in):: ualComplex
    COMPLEX(EUITM_R8) :: stdComplex
    stdComplex = CMPLX(ualComplex%re, ualcomplex%im, EUITM_R8)
    RETURN
 END FUNCTION convertToStdComplex
 
FUNCTION convertArrayToStdComplex(inArray) result (outArray)
    TYPE (type_UalComplex), DIMENSION(:), ALLOCATABLE, TARGET, INTENT(in) :: inArray
    COMPLEX(EUITM_R8),                DIMENSION(:),  ALLOCATABLE, TARGET :: outArray


    INTEGER::sizeOfArray
    INTEGER::I

    sizeOfArray = SIZE(inArray)
    ALLOCATE(outArray(sizeofArray))

    do I = 1, sizeOfArray
        outArray(I) = CMPLX( inArray(I)%re, inArray(I)%im, EUITM_R8)
    end do
    RETURN
END FUNCTION convertArrayToStdComplex

FUNCTION convert1DArrayToStdComplex(inArray) result (outArray)
    TYPE (type_UalComplex), POINTER :: inArray(:)
    COMPLEX(EUITM_R8),                POINTER:: outArray(:)
       
    INTEGER::sizeOfArray
    INTEGER, DIMENSION(1) :: shapeOfArray
 
    COMPLEX(EUITM_R8),                DIMENSION(:), ALLOCATABLE, TARGET:: tmp1DStdComplexArray
    TYPE (type_UalComplex), DIMENSION(:), ALLOCATABLE, TARGET:: tmp1DUalComplexArray

    sizeOfArray = SIZE(inArray)
    shapeOfArray = SHAPE(inArray)

    allocate(tmp1DStdComplexArray(sizeOfArray))
    allocate(tmp1DUalComplexArray(sizeOfArray))

    tmp1DUalComplexArray = RESHAPE(inArray, (/sizeOfArray/))
    tmp1DStdComplexArray = convertArrayToStdComplex(tmp1DUalComplexArray)
    
    ALLOCATE(outArray(shapeOfArray(1)))
    outArray = RESHAPE(tmp1DStdComplexArray, shapeOfArray)

    deallocate(tmp1DStdComplexArray)
    deallocate(tmp1DUalComplexArray)
            
    RETURN
END FUNCTION convert1DArrayToStdComplex

FUNCTION convert2DArrayToStdComplex(inArray) result (outArray)
    TYPE (type_UalComplex), POINTER :: inArray(:,:)
    COMPLEX(EUITM_R8),                POINTER:: outArray(:,:)
       
    INTEGER::sizeOfArray
    INTEGER, DIMENSION(2) :: shapeOfArray
 
    COMPLEX(EUITM_R8),                DIMENSION(:), ALLOCATABLE, TARGET:: tmp1DStdComplexArray
    TYPE (type_UalComplex), DIMENSION(:), ALLOCATABLE, TARGET:: tmp1DUalComplexArray

    sizeOfArray = SIZE(inArray)
    shapeOfArray = SHAPE(inArray)

    allocate(tmp1DStdComplexArray(sizeOfArray))
    allocate(tmp1DUalComplexArray(sizeOfArray))

    tmp1DUalComplexArray = RESHAPE(inArray, (/sizeOfArray/))
    tmp1DStdComplexArray = convertArrayToStdComplex(tmp1DUalComplexArray)
    
    ALLOCATE(outArray(shapeOfArray(1),shapeOfArray(2)))
    outArray = RESHAPE(tmp1DStdComplexArray, shapeOfArray)

    deallocate(tmp1DStdComplexArray)
    deallocate(tmp1DUalComplexArray)
            
    RETURN
END FUNCTION convert2DArrayToStdComplex

FUNCTION convert3DArrayToStdComplex(inArray) result (outArray)
    TYPE (type_UalComplex), POINTER :: inArray(:,:, :)
    COMPLEX(EUITM_R8),                POINTER:: outArray(:,:, :)
       
    INTEGER::sizeOfArray
    INTEGER, DIMENSION(3) :: shapeOfArray
 
    COMPLEX(EUITM_R8),                DIMENSION(:), ALLOCATABLE, TARGET:: tmp1DStdComplexArray
    TYPE (type_UalComplex), DIMENSION(:), ALLOCATABLE, TARGET:: tmp1DUalComplexArray

    sizeOfArray = SIZE(inArray)
    shapeOfArray = SHAPE(inArray)

    allocate(tmp1DStdComplexArray(sizeOfArray))
    allocate(tmp1DUalComplexArray(sizeOfArray))

    tmp1DUalComplexArray = RESHAPE(inArray, (/sizeOfArray/))
    tmp1DStdComplexArray = convertArrayToStdComplex(tmp1DUalComplexArray)
    
    ALLOCATE(outArray(shapeOfArray(1),shapeOfArray(2), shapeOfArray(3)))
    outArray = RESHAPE(tmp1DStdComplexArray, shapeOfArray)

    deallocate(tmp1DStdComplexArray)
    deallocate(tmp1DUalComplexArray)
            
    RETURN
END FUNCTION convert3DArrayToStdComplex

FUNCTION convert4DArrayToStdComplex(inArray) result (outArray)
    TYPE (type_UalComplex), POINTER :: inArray(:,:, :, :)
    COMPLEX(EUITM_R8),                POINTER:: outArray(:,:, :, :)
       
    INTEGER::sizeOfArray
    INTEGER, DIMENSION(4) :: shapeOfArray
 
    COMPLEX(EUITM_R8),                DIMENSION(:), ALLOCATABLE, TARGET:: tmp1DStdComplexArray
    TYPE (type_UalComplex), DIMENSION(:), ALLOCATABLE, TARGET:: tmp1DUalComplexArray

    sizeOfArray = SIZE(inArray)
    shapeOfArray = SHAPE(inArray)

    allocate(tmp1DStdComplexArray(sizeOfArray))
    allocate(tmp1DUalComplexArray(sizeOfArray))

    tmp1DUalComplexArray = RESHAPE(inArray, (/sizeOfArray/))
    tmp1DStdComplexArray = convertArrayToStdComplex(tmp1DUalComplexArray)
    
    ALLOCATE(outArray(shapeOfArray(1),shapeOfArray(2), shapeOfArray(3), shapeOfArray(4)))
    outArray = RESHAPE(tmp1DStdComplexArray, shapeOfArray)

    deallocate(tmp1DStdComplexArray)
    deallocate(tmp1DUalComplexArray)
            
    RETURN
END FUNCTION convert4DArrayToStdComplex


FUNCTION convert5DArrayToStdComplex(inArray) result (outArray)
    TYPE (type_UalComplex), POINTER :: inArray(:,:, :, :, :)
    COMPLEX(EUITM_R8),                POINTER:: outArray(:,:, :, :, :)
       
    INTEGER::sizeOfArray
    INTEGER, DIMENSION(5) :: shapeOfArray
 
    COMPLEX(EUITM_R8),                DIMENSION(:), ALLOCATABLE, TARGET:: tmp1DStdComplexArray
    TYPE (type_UalComplex), DIMENSION(:), ALLOCATABLE, TARGET:: tmp1DUalComplexArray

    sizeOfArray = SIZE(inArray)
    shapeOfArray = SHAPE(inArray)


    allocate(tmp1DStdComplexArray(sizeOfArray))
    allocate(tmp1DUalComplexArray(sizeOfArray))

    tmp1DUalComplexArray = RESHAPE(inArray, (/sizeOfArray/))
    tmp1DStdComplexArray = convertArrayToStdComplex(tmp1DUalComplexArray)
    
    ALLOCATE(outArray(shapeOfArray(1),shapeOfArray(2), shapeOfArray(3), shapeOfArray(4), shapeOfArray(5)))
    outArray = RESHAPE(tmp1DStdComplexArray, shapeOfArray)

    deallocate(tmp1DStdComplexArray)
    deallocate(tmp1DUalComplexArray)

         
    RETURN
END FUNCTION convert5DArrayToStdComplex

FUNCTION convert6DArrayToStdComplex(inArray) result (outArray)
    TYPE (type_UalComplex), POINTER :: inArray(:,:, :, :, :, :)
    COMPLEX(EUITM_R8),                POINTER:: outArray(:,:, :, :, :, :)
       
    INTEGER::sizeOfArray
    INTEGER, DIMENSION(6) :: shapeOfArray
 
    COMPLEX(EUITM_R8),                DIMENSION(:), ALLOCATABLE, TARGET:: tmp1DStdComplexArray
    TYPE (type_UalComplex), DIMENSION(:), ALLOCATABLE, TARGET:: tmp1DUalComplexArray

    sizeOfArray = SIZE(inArray)
    shapeOfArray = SHAPE(inArray)

    allocate(tmp1DStdComplexArray(sizeOfArray))
    allocate(tmp1DUalComplexArray(sizeOfArray))

    tmp1DUalComplexArray = RESHAPE(inArray, (/sizeOfArray/))
    tmp1DStdComplexArray = convertArrayToStdComplex(tmp1DUalComplexArray)
    
    ALLOCATE(outArray(shapeOfArray(1),shapeOfArray(2), shapeOfArray(3), shapeOfArray(4), shapeOfArray(5), shapeOfArray(6)))
    outArray = RESHAPE(tmp1DStdComplexArray, shapeOfArray)

    deallocate(tmp1DStdComplexArray)
    deallocate(tmp1DUalComplexArray)
            
    RETURN
END FUNCTION convert6DArrayToStdComplex

FUNCTION convert7DArrayToStdComplex(inArray) result (outArray)
    TYPE (type_UalComplex), POINTER :: inArray(:,:, :, :, :, :, :)
    COMPLEX(EUITM_R8),                POINTER:: outArray(:,:, :, :, :, :, :)
       
    INTEGER::sizeOfArray
    INTEGER, DIMENSION(7) :: shapeOfArray
 
    COMPLEX(EUITM_R8),                DIMENSION(:), ALLOCATABLE, TARGET:: tmp1DStdComplexArray
    TYPE (type_UalComplex), DIMENSION(:), ALLOCATABLE, TARGET:: tmp1DUalComplexArray

    sizeOfArray = SIZE(inArray)
    shapeOfArray = SHAPE(inArray)

    allocate(tmp1DStdComplexArray(sizeOfArray))
    allocate(tmp1DUalComplexArray(sizeOfArray))

    tmp1DUalComplexArray = RESHAPE(inArray, (/sizeOfArray/))
    tmp1DStdComplexArray = convertArrayToStdComplex(tmp1DUalComplexArray)
    
    ALLOCATE(outArray(shapeOfArray(1),shapeOfArray(2), shapeOfArray(3), shapeOfArray(4), shapeOfArray(5), &
                      shapeOfArray(6), shapeOfArray(7)))
    outArray = RESHAPE(tmp1DStdComplexArray, shapeOfArray)
 
    deallocate(tmp1DStdComplexArray)
    deallocate(tmp1DUalComplexArray)
            
    RETURN
END FUNCTION convert7DArrayToStdComplex


END MODULE euITM_ual_defs

