! *
! **********************************************************************************
! *                                                                                *
! * INTEL CORPORATION                                                              *
! * Copyright 2006-2011 Intel Corporation All Rights Reserved.                     *
! *                                                                                *
! * The source code contained or described herein and all documents related to     *
! * the source code ("Material") are owned by Intel Corporation or its suppliers   *
! * or licensors. Title to the Material remains with Intel Corporation or its      *
! * suppliers and licensors. The Material contains trade secrets and proprietary   *
! * and confidential information of Intel or its suppliers and licensors. The      *
! * Material is protected by worldwide copyright and trade secret laws and         *
! * treaty provisions. No part of the Material may be used, copied, reproduced,    *
! * modified, published, uploaded, posted, transmitted, distributed, or            *
! * disclosed in any way without Intel's prior express written permission.         *
! *                                                                                *
! * No license under any patent, copyright, trade secret or other intellectual     *
! * property right is granted to or conferred upon you by disclosure or delivery   *
! * of the Materials, either expressly, by implication, inducement, estoppel or    *
! * otherwise. Any license under such intellectual property rights must be         *
! * express and approved by Intel in writing.                                      *
! *                                                                                *
! **********************************************************************************
! *

!
! **************************** Edit History *******************************
!
! Sep-07-2006  Initial creation based on the Fortran 2003 standard
!              (J3/04-007) 15.1
! Feb-02-2008  Fix C_INT_FAST16_T, C_INT_FAST32_T, and C_LONG_DOUBLE for MAC
! Feb-06-2008  Use attribute directive C_PTR for C_PTR and C_FUNPTR
! Oct-22-2008  Add BIND(C) for C_PTR and C_FUNPTR
! Sep-29-2008  Fix C_ASSOCIATED(C_NULL_PTR) returns true in mixed-language. This 
!              .obj file is moved from libifcore builds to a new ifmodintr.lib library.
! Dec-03-2008  C_ASSOCIATED with /Gm does not work.
! Jan-14-2009  The -assume underscore and 2underscores decorate C_LOC and C_FUNLOC
!              (ISO_C_BINDING) intrinsic module inquiry functions
! Jan-15-2009  The -assume underscore and 2underscores decorate 
!              C_F_PROCPOINTER, C_NULL_FUNPTR and C_NULL_PTR (ISO_C_BINDING) 
!              intrinsic module inquiry functions and constants
! Jan-17-2009  Removed BIND(C) attribute from global constant definition.
! Mar-30-2010  Added DEFAULT attribute to global constant definition for portability.
! Sep-29-2010  Allow deallocation of fortran array pointers coming out of C_F_POINTER
! Dec-21-2010  Require correct type of C pointer argument for C_F_PROCPOINTER and scalar
!              C_F_POINTER. Add DEFAULT attribute to all routines to prevent stack
!              corruption should caller build with STDCALL conventions.
! Apr-27-2011  Added support for C types on Windows. -pakuber
!
! *************************************************************************

! *************************************************************************
!
! Notes:
!
! 1) This module is created with explicit integer and real types and is
!    independent of default integer and default real.
!
! 2) Preset Defines that are used in this module:
!      Windows: _WIN32
!      Linux: __x86_64__  is defined for Intel(R) 64 architecture
!             __ia64__ is defined for Intel(R) IA-64 architecture
!
! 3) On Windows, the type "_Bool", and the complex types "float _Complex",
!    "double _Complex", and "long double _Complex" are only supported in C
!    with the option "/Qc99".  We assume that this option is specified for C.
!    (See C_FLOAT_COMPLEX, C_DOUBLE_COMPLEX, C_LONG_DOUBLE_COMPLEX, C_BOOL)
!
! *************************************************************************


MODULE ISO_C_BINDING

!DEC$ IF DEFINED(_WIN32) .OR. DEFINED(_WIN64)
!DEC$ OBJCOMMENT LIB:"ifmodintr.lib"
!DEC$ ENDIF

    IMPLICIT NONE

    PRIVATE
    PUBLIC :: &
! Derived types and constants
        C_INT, C_SHORT, C_LONG, C_LONG_LONG, C_SIGNED_CHAR, &
        C_SIZE_T, C_INT8_T, C_INT16_T, C_INT32_T, C_INT64_T, &
        C_INT_LEAST8_T, C_INT_LEAST16_T, C_INT_LEAST32_T, C_INT_LEAST64_T, &
        C_INT_FAST8_T, C_INT_FAST16_T, C_INT_FAST32_T, C_INT_FAST64_T, &
        C_INTMAX_T, C_INTPTR_T, &
        C_FLOAT, C_DOUBLE, C_LONG_DOUBLE, &
        C_FLOAT_COMPLEX, C_DOUBLE_COMPLEX, C_LONG_DOUBLE_COMPLEX, &
        C_BOOL, C_CHAR, &
        C_NULL_CHAR, C_ALERT, C_BACKSPACE, C_FORM_FEED, &
        C_NEW_LINE, C_CARRIAGE_RETURN, C_HORIZONTAL_TAB, C_VERTICAL_TAB, &
        C_PTR, C_FUNPTR, C_NULL_PTR, C_NULL_FUNPTR, &
! Procedures
        c_associated, c_f_pointer, c_f_procpointer, c_funloc, c_loc

    ! Pointer length is 4/8 for 32/64 bit architecture
    INTEGER, PARAMETER :: POINTER_LEN = INT_PTR_KIND() 

! -------------------------------------------------------------------------
! integer types
! -------------------------------------------------------------------------
    INTEGER (KIND=4), PARAMETER :: C_INT = 4
    INTEGER (KIND=4), PARAMETER :: C_SHORT = 2

!DEC$ IF DEFINED(_WIN32)
    INTEGER (KIND=4), PARAMETER :: C_LONG = 4
!DEC$ ELSE 
    INTEGER (KIND=4), PARAMETER :: C_LONG = POINTER_LEN
!DEC$ ENDIF

    INTEGER (KIND=4), PARAMETER :: C_LONG_LONG = 8
    INTEGER (KIND=4), PARAMETER :: C_SIGNED_CHAR = 1
! On linux the C type size_t is defined in stddef.h
    INTEGER (KIND=4), PARAMETER :: C_SIZE_T = POINTER_LEN

! On linux the following C types (int8_t .. intmax_t) are defined in stdint.h
! Support for C types on Windows.
!DEC$ IF DEFINED(_WIN32)
    INTEGER (KIND=4), PARAMETER :: C_INT8_T = 1
    INTEGER (KIND=4), PARAMETER :: C_INT16_T = 2
    INTEGER (KIND=4), PARAMETER :: C_INT32_T = 4
    INTEGER (KIND=4), PARAMETER :: C_INT64_T = 8
    INTEGER (KIND=4), PARAMETER :: C_INT_LEAST8_T = 1
    INTEGER (KIND=4), PARAMETER :: C_INT_LEAST16_T = 2
    INTEGER (KIND=4), PARAMETER :: C_INT_LEAST32_T = 4
    INTEGER (KIND=4), PARAMETER :: C_INT_LEAST64_T = 8
    INTEGER (KIND=4), PARAMETER :: C_INT_FAST8_T = 1
    INTEGER (KIND=4), PARAMETER :: C_INT_FAST16_T = 2
    INTEGER (KIND=4), PARAMETER :: C_INT_FAST32_T = 4
    INTEGER (KIND=4), PARAMETER :: C_INT_FAST64_T = 8
    INTEGER (KIND=4), PARAMETER :: C_INTMAX_T = 8
!DEC$ ELSEIF DEFINED(__MACH__)
    INTEGER (KIND=4), PARAMETER :: C_INT8_T = 1
    INTEGER (KIND=4), PARAMETER :: C_INT16_T = 2
    INTEGER (KIND=4), PARAMETER :: C_INT32_T = 4
    INTEGER (KIND=4), PARAMETER :: C_INT64_T = 8
    INTEGER (KIND=4), PARAMETER :: C_INT_LEAST8_T = 1
    INTEGER (KIND=4), PARAMETER :: C_INT_LEAST16_T = 2
    INTEGER (KIND=4), PARAMETER :: C_INT_LEAST32_T = 4
    INTEGER (KIND=4), PARAMETER :: C_INT_LEAST64_T = 8
    INTEGER (KIND=4), PARAMETER :: C_INT_FAST8_T = 1
    INTEGER (KIND=4), PARAMETER :: C_INT_FAST16_T = 2
    INTEGER (KIND=4), PARAMETER :: C_INT_FAST32_T = 4
    INTEGER (KIND=4), PARAMETER :: C_INT_FAST64_T = 8
    INTEGER (KIND=4), PARAMETER :: C_INTMAX_T = 8
!DEC$ ELSE
    INTEGER (KIND=4), PARAMETER :: C_INT8_T = 1
    INTEGER (KIND=4), PARAMETER :: C_INT16_T = 2
    INTEGER (KIND=4), PARAMETER :: C_INT32_T = 4
    INTEGER (KIND=4), PARAMETER :: C_INT64_T = 8
    INTEGER (KIND=4), PARAMETER :: C_INT_LEAST8_T = 1
    INTEGER (KIND=4), PARAMETER :: C_INT_LEAST16_T = 2
    INTEGER (KIND=4), PARAMETER :: C_INT_LEAST32_T = 4
    INTEGER (KIND=4), PARAMETER :: C_INT_LEAST64_T = 8
    INTEGER (KIND=4), PARAMETER :: C_INT_FAST8_T = 1
    INTEGER (KIND=4), PARAMETER :: C_INT_FAST16_T = POINTER_LEN
    INTEGER (KIND=4), PARAMETER :: C_INT_FAST32_T = POINTER_LEN
    INTEGER (KIND=4), PARAMETER :: C_INT_FAST64_T = 8
    INTEGER (KIND=4), PARAMETER :: C_INTMAX_T = 8
!DEC$ ENDIF

    ! On linux the C type intptr_t is defined in stdint.h
    ! On Windows intptr_t is defined in stddef.h
    INTEGER (KIND=4), PARAMETER :: C_INTPTR_T = POINTER_LEN

! -------------------------------------------------------------------------
! real types
! -------------------------------------------------------------------------
    INTEGER (KIND=4), PARAMETER :: C_FLOAT = 4
    INTEGER (KIND=4), PARAMETER :: C_DOUBLE = 8

!DEC$ IF DEFINED(__linux__) .AND. DEFINED(__i386__ )
    ! On Linux IA32, the size of "long double" in C is 12 by default
    ! and it is not supported by Fortran.  (The size of "long double" is 16
    ! with -Qoption,cpp,-_l).

      INTEGER (KIND=4), PARAMETER :: C_LONG_DOUBLE = -1

!DEC$ ELSEIF DEFINED(__MACH__)
    INTEGER (KIND=4), PARAMETER :: C_LONG_DOUBLE = 16

!DEC$ ELSE 
    INTEGER (KIND=4), PARAMETER :: C_LONG_DOUBLE = C_LONG *2

!DEC$ ENDIF

! -------------------------------------------------------------------------
! complex types 
! -------------------------------------------------------------------------
    ! On Windows, these complex types are only supported in C with /Qc99
    INTEGER (KIND=4), PARAMETER :: C_FLOAT_COMPLEX = C_FLOAT
    INTEGER (KIND=4), PARAMETER :: C_DOUBLE_COMPLEX = C_DOUBLE
    INTEGER (KIND=4), PARAMETER :: C_LONG_DOUBLE_COMPLEX = C_LONG_DOUBLE

! -------------------------------------------------------------------------
! logical type
! -------------------------------------------------------------------------
    ! On Windows, _Bool is only supported in C with /Qc99
    INTEGER (KIND=4), PARAMETER :: C_BOOL = 1

! -------------------------------------------------------------------------
! character type
! -------------------------------------------------------------------------
    INTEGER (KIND=4), PARAMETER :: C_CHAR = 1
      
! -------------------------------------------------------------------------
! special characters
! -------------------------------------------------------------------------
    CHARACTER (KIND=1, LEN=1), PARAMETER :: C_NULL_CHAR = ACHAR(0)
    CHARACTER (KIND=1, LEN=1), PARAMETER :: C_ALERT = ACHAR(7)
    CHARACTER (KIND=1, LEN=1), PARAMETER :: C_BACKSPACE = ACHAR(8)
    CHARACTER (KIND=1, LEN=1), PARAMETER :: C_FORM_FEED = ACHAR(12)
    CHARACTER (KIND=1, LEN=1), PARAMETER :: C_NEW_LINE = ACHAR(10)
    CHARACTER (KIND=1, LEN=1), PARAMETER :: C_CARRIAGE_RETURN = ACHAR(13)
    CHARACTER (KIND=1, LEN=1), PARAMETER :: C_HORIZONTAL_TAB = ACHAR(9)
    CHARACTER (KIND=1, LEN=1), PARAMETER :: C_VERTICAL_TAB = ACHAR(11)
 
! -------------------------------------------------------------------------
! C_PTR, C_FUNPTR, C_NULL_PTR, C_NULL_FUNPTR
! -------------------------------------------------------------------------
    TYPE, BIND(C) :: C_PTR
        PRIVATE
        INTEGER(C_INTPTR_T) :: ptr
    END TYPE C_PTR

    TYPE, BIND(C) :: C_FUNPTR
        PRIVATE
        INTEGER(C_INTPTR_T) :: ptr
    END TYPE C_FUNPTR
!DEC$ ATTRIBUTES C_PTR :: C_PTR, C_FUNPTR

    TYPE(C_PTR), PARAMETER :: C_NULL_PTR = C_PTR(0)
    !DEC$ ATTRIBUTES DEFAULT :: C_NULL_PTR
    TYPE(C_FUNPTR), PARAMETER :: C_NULL_FUNPTR = C_FUNPTR(0)
    !DEC$ ATTRIBUTES DEFAULT :: C_NULL_FUNPTR

! -------------------------------------------------------------------------
! private types for fortran descriptors
! -------------------------------------------------------------------------
    INTEGER(4), PARAMETER :: for_desc_max_rank = 7    
    INTEGER(C_INTPTR_T), PARAMETER :: for_desc_array_defined= 1
    INTEGER(C_INTPTR_T), PARAMETER :: for_desc_array_contiguous = 4
    INTEGER(C_INTPTR_T), PARAMETER :: &
            for_desc_flags = for_desc_array_defined + &
                             for_desc_array_contiguous

    TYPE :: for_desc_triplet
        INTEGER(C_INTPTR_T) :: extent
        INTEGER(C_INTPTR_T) :: mult  ! multiplier for this dimension
        INTEGER(C_INTPTR_T) :: lowerbound
    END TYPE for_desc_triplet

    TYPE :: for_array_descriptor
        INTEGER(C_INTPTR_T) :: base
        INTEGER(C_INTPTR_T) :: len  ! len of data type
        INTEGER(C_INTPTR_T) :: offset
        INTEGER(C_INTPTR_T) :: flags
        INTEGER(C_INTPTR_T) :: rank
        INTEGER(C_INTPTR_T) :: reserved1
        TYPE(for_desc_triplet) :: diminfo(for_desc_max_rank)
    END TYPE for_array_descriptor

! -------------------------------------------------------------------------
! procedure interfaces
! -------------------------------------------------------------------------

    ! ---------------------------------------------------------------------
    ! Generic interface for indicating association status of pointers
    ! ---------------------------------------------------------------------
    INTERFACE c_associated
        MODULE PROCEDURE c_associated_ptr, c_associated_funptr
    END INTERFACE

    ! ---------------------------------------------------------------------
    ! Generic interface for associating a data pointer with the target of
    ! a C pointer and specifying its shape when the pointer is an array.
    ! The specific procedure c_f_pointer_scalar is for scalar pointer argument.
    ! The four specific procedures c_f_pointer_array1, etc. are for array
    ! pointer argument and only differ by the type of the shape argument.
    !
    ! The specific procedures c_f_pointer_array1, etc. are implemented by
    ! differently named module procedures c_f_pointer_private1, etc.
    ! The names need to be different since their argument declarations do not
    ! match (c_f_pointer_private1, etc. declare the pointer argument directly
    ! as array descriptors).  The ALIAS attribute is used to associate the
    ! specific procedures with their implementation procedures via internal
    ! names.
    !
    ! In the specific procedures c_f_pointer_array1, etc. the NO_ARG_CHECK
    ! attribute is used to allow various types and shapes for the Fortran
    ! pointer.  The NULLIFY attribute is used to nullify the pointer argument
    ! before the call and to ensure that the rank and length fields of the
    ! descriptor are properly initialized.
    ! ---------------------------------------------------------------------
    INTERFACE c_f_pointer
        SUBROUTINE c_f_pointer_scalar_new (cptr, fptr)
!DEC$ ATTRIBUTES DEFAULT,ALIAS: 'c_f_pointer_set_scalar' :: c_f_pointer_scalar_new
            IMPORT c_ptr
            IMPLICIT NONE
            TYPE(c_ptr), INTENT(IN) :: cptr
!DEC$ ATTRIBUTES NO_ARG_CHECK:: fptr
            INTEGER, POINTER, INTENT(OUT) :: fptr
        END SUBROUTINE c_f_pointer_scalar_new

        SUBROUTINE c_f_pointer_array1 (cptr, fptr, shape)
!DEC$ ATTRIBUTES DEFAULT,ALIAS: 'c_f_pointer_set_desc1' :: c_f_pointer_array1
            IMPORT c_ptr
            IMPLICIT NONE
            TYPE(c_ptr), INTENT(IN) :: cptr
!DEC$ ATTRIBUTES NO_ARG_CHECK, NULLIFY :: fptr
            INTEGER, POINTER, INTENT(OUT) :: fptr(:)
            INTEGER(1), INTENT(IN) :: shape(:)
        END SUBROUTINE c_f_pointer_array1

        SUBROUTINE c_f_pointer_array2 (cptr, fptr, shape)
!DEC$ ATTRIBUTES DEFAULT,ALIAS: 'c_f_pointer_set_desc2' :: c_f_pointer_array2
            IMPORT c_ptr
            IMPLICIT NONE
            TYPE(c_ptr), INTENT(IN) :: cptr
!DEC$ ATTRIBUTES NO_ARG_CHECK, NULLIFY :: fptr
            INTEGER, POINTER, INTENT(OUT) :: fptr(:)
            INTEGER(2), INTENT(IN) :: shape(:)
        END SUBROUTINE c_f_pointer_array2

        SUBROUTINE c_f_pointer_array4 (cptr, fptr, shape)
!DEC$ ATTRIBUTES DEFAULT,ALIAS: 'c_f_pointer_set_desc4' :: c_f_pointer_array4
            IMPORT c_ptr
            IMPLICIT NONE
            TYPE(c_ptr), INTENT(IN) :: cptr
!DEC$ ATTRIBUTES NO_ARG_CHECK, NULLIFY :: fptr
            INTEGER, POINTER, INTENT(OUT) :: fptr(:)
            INTEGER(4), INTENT(IN) :: shape(:)
        END SUBROUTINE c_f_pointer_array4

        SUBROUTINE c_f_pointer_array8 (cptr, fptr, shape)
!DEC$ ATTRIBUTES DEFAULT,ALIAS: 'c_f_pointer_set_desc8' :: c_f_pointer_array8
            IMPORT c_ptr
            IMPLICIT NONE
            TYPE(c_ptr), INTENT(IN) :: cptr
!DEC$ ATTRIBUTES NO_ARG_CHECK, NULLIFY :: fptr
            INTEGER, POINTER, INTENT(OUT) :: fptr(:)
            INTEGER(8), INTENT(IN) :: shape(:)
        END SUBROUTINE c_f_pointer_array8

    END INTERFACE

    INTERFACE c_f_procpointer
        SUBROUTINE c_f_procpointer_private_new (cptr, fptr)
!DEC$ ATTRIBUTES DEFAULT,ALIAS: 'c_f_pointer_set_scalar' :: c_f_procpointer_private_new
            IMPORT c_funptr
            IMPLICIT NONE
            TYPE(c_funptr), INTENT(IN) :: cptr
!DEC$ ATTRIBUTES NO_ARG_CHECK:: fptr
            INTEGER, POINTER, INTENT(OUT) :: fptr
        END SUBROUTINE c_f_procpointer_private_new
    END INTERFACE c_f_procpointer

    INTERFACE c_funloc
        MODULE PROCEDURE c_funloc_private
    END INTERFACE c_funloc

    INTERFACE c_loc
        MODULE PROCEDURE c_loc_private
    END INTERFACE c_loc

! -------------------------------------------------------------------------
! procedures
! -------------------------------------------------------------------------
CONTAINS

    ! ---------------------------------------------------------------------
    ! Indicates the association status of c_ptr_1 or indicates whether
    ! c_ptr_1 and c_ptr_2 are associated with the same entity.
    ! Both c_ptr_1 and c_ptr_2 are of type C_PTR
    ! ---------------------------------------------------------------------
    FUNCTION c_associated_ptr (c_ptr_1, c_ptr_2)
        !DEC$ ATTRIBUTES DEFAULT :: c_associated_ptr
        LOGICAL(4) :: c_associated_ptr
        TYPE(c_ptr) :: c_ptr_1
        TYPE(c_ptr), OPTIONAL :: c_ptr_2

        IF (.NOT. PRESENT(c_ptr_2)) THEN
            IF (c_ptr_1%ptr == 0) THEN   ! c_ptr_1 is a C null pointer
                c_associated_ptr = .FALSE.
            ELSE
                c_associated_ptr = .TRUE.
            END IF

        ELSE  ! c_ptr_2 is present
            IF (c_ptr_1%ptr == 0) THEN   ! c_ptr_1 is a C null pointer
                c_associated_ptr = .FALSE.
            ELSE IF (c_ptr_1%ptr == c_ptr_2%ptr) THEN
                c_associated_ptr = .TRUE.
            ELSE 
                c_associated_ptr = .FALSE.
            END IF
        END IF
    END FUNCTION c_associated_ptr

    ! ---------------------------------------------------------------------
    ! Indicates the association status of c_ptr_1 or indicates whether
    ! c_ptr_1 and c_ptr_2 are associated with the same entity.
    ! Both c_ptr_1 and c_ptr_2 are of type C_FUNPTR
    ! ---------------------------------------------------------------------
    FUNCTION c_associated_funptr (c_ptr_1, c_ptr_2)
        !DEC$ ATTRIBUTES DEFAULT :: c_associated_funptr
        LOGICAL(4) :: c_associated_funptr
        TYPE(c_funptr) :: c_ptr_1
        TYPE(c_funptr), OPTIONAL :: c_ptr_2

        IF (.NOT. PRESENT(c_ptr_2)) THEN
            IF (c_ptr_1%ptr == 0) THEN   ! c_ptr_1 is a C null pointer
                c_associated_funptr = .FALSE.
            ELSE
                c_associated_funptr = .TRUE.
            END IF

        ELSE  ! c_ptr_2 is present
            IF (c_ptr_1%ptr == 0) THEN   ! c_ptr_1 is a C null pointer
                c_associated_funptr = .FALSE.
            ELSE IF (c_ptr_1%ptr == c_ptr_2%ptr) THEN
                c_associated_funptr = .TRUE.
            ELSE 
                c_associated_funptr = .FALSE.
            END IF
        END IF
    END FUNCTION c_associated_funptr

    ! ---------------------------------------------------------------------
    ! Associates a scalar pointer with the target of a C pointer.
    ! This routine is also used for C_F_PROCPOINTER
    ! ---------------------------------------------------------------------

    SUBROUTINE c_f_pointer_private0 (caddr, faddr)
!DEC$ ATTRIBUTES DEFAULT,ALIAS:'c_f_pointer_set_scalar' :: c_f_pointer_private0
        INTEGER(C_INTPTR_T), INTENT(IN) :: caddr
        INTEGER(C_INTPTR_T), INTENT(OUT) :: faddr
        faddr = caddr
        
    END SUBROUTINE c_f_pointer_private0
    
    ! ---------------------------------------------------------------------
    ! Associates an array pointer with the target of a C pointer and
    ! specifies its shape.  The argument "shape" is of type INTEGER(1).
    ! ---------------------------------------------------------------------
    SUBROUTINE c_f_pointer_private1 (caddr, fdesc, shape)
!DEC$ ATTRIBUTES DEFAULT,ALIAS:'c_f_pointer_set_desc1' :: c_f_pointer_private1
        INTEGER(C_INTPTR_T), INTENT(IN) :: caddr
        TYPE(for_array_descriptor), INTENT(INOUT) :: fdesc
        INTEGER(1), INTENT(IN) :: shape(:)
        INTEGER(4) :: i
        INTEGER(C_INTPTR_T) :: multiplier

        fdesc%base = caddr
        fdesc%flags = for_desc_flags

        multiplier = fdesc%len
        do i = 1, fdesc%rank
            fdesc%diminfo(i)%extent = shape(i)
            fdesc%diminfo(i)%mult = multiplier
            fdesc%diminfo(i)%lowerbound = 1
            multiplier = multiplier * shape(i)
        enddo

    END SUBROUTINE c_f_pointer_private1

    ! ---------------------------------------------------------------------
    ! Associates an array pointer with the target of a C pointer and
    ! specifies its shape.  The argument "shape" is of type INTEGER(2).
    ! ---------------------------------------------------------------------
    SUBROUTINE c_f_pointer_private2 (caddr, fdesc, shape)
!DEC$ ATTRIBUTES DEFAULT,ALIAS:'c_f_pointer_set_desc2' :: c_f_pointer_private2
        INTEGER(C_INTPTR_T), INTENT(IN) :: caddr
        TYPE(for_array_descriptor), INTENT(INOUT) :: fdesc
        INTEGER(2), INTENT(IN) :: shape(:)
        INTEGER(4) :: i
        INTEGER(C_INTPTR_T) :: multiplier

        fdesc%base = caddr
        fdesc%flags = for_desc_flags

        multiplier = fdesc%len
        do i = 1, fdesc%rank
            fdesc%diminfo(i)%extent = shape(i)
            fdesc%diminfo(i)%mult = multiplier
            fdesc%diminfo(i)%lowerbound = 1
            multiplier = multiplier * shape(i)
        enddo
    END SUBROUTINE c_f_pointer_private2

    ! ---------------------------------------------------------------------
    ! Associates an array pointer with the target of a C pointer and
    ! specifies its shape.  The argument "shape" is of type INTEGER(4).
    ! ---------------------------------------------------------------------
    SUBROUTINE c_f_pointer_private4 (caddr, fdesc, shape)
!DEC$ ATTRIBUTES DEFAULT,ALIAS:'c_f_pointer_set_desc4' :: c_f_pointer_private4
        INTEGER(C_INTPTR_T), INTENT(IN) :: caddr
        TYPE(for_array_descriptor), INTENT(INOUT) :: fdesc
        INTEGER(4), INTENT(IN) :: shape(:)
        INTEGER(4) :: i
        INTEGER(C_INTPTR_T) :: multiplier

        fdesc%base = caddr
        fdesc%flags = for_desc_flags

        multiplier = fdesc%len
        do i = 1, fdesc%rank
            fdesc%diminfo(i)%extent = shape(i)
            fdesc%diminfo(i)%mult = multiplier
            fdesc%diminfo(i)%lowerbound = 1
            multiplier = multiplier * shape(i)
        enddo
    END SUBROUTINE c_f_pointer_private4

    ! ---------------------------------------------------------------------
    ! Associates an array pointer with the target of a C pointer and
    ! specifies its shape.  The argument "shape" is of type INTEGER(8).
    ! ---------------------------------------------------------------------
    SUBROUTINE c_f_pointer_private8 (caddr, fdesc, shape)
!DEC$ ATTRIBUTES DEFAULT,ALIAS:'c_f_pointer_set_desc8' :: c_f_pointer_private8
        INTEGER(C_INTPTR_T), INTENT(IN) :: caddr
        TYPE(for_array_descriptor), INTENT(INOUT) :: fdesc
        INTEGER(8), INTENT(IN) :: shape(:)
        INTEGER(4) :: i
        INTEGER(C_INTPTR_T) :: multiplier

        fdesc%base = caddr
        fdesc%flags = for_desc_flags

        multiplier = fdesc%len
        do i = 1, fdesc%rank
            fdesc%diminfo(i)%extent = shape(i)
            fdesc%diminfo(i)%mult = multiplier
            fdesc%diminfo(i)%lowerbound = 1
            multiplier = multiplier * shape(i)
        enddo
    END SUBROUTINE c_f_pointer_private8

    ! ---------------------------------------------------------------------
    ! Returns the C address of the procedure argument.
    ! ---------------------------------------------------------------------
    FUNCTION c_funloc_private (x)
        TYPE(c_funptr) :: c_funloc_private
!DEC$ ATTRIBUTES DEFAULT :: c_funloc_private
!DEC$ ATTRIBUTES NO_ARG_CHECK :: x
        INTEGER :: x   ! use any type here, since the procedure pointer is
                       ! not yet supported
        INTRINSIC :: LOC

        ! Since x has the NO_ARG_CHECK attribute, the actual address (not
        ! any descriptor address) is passed in.
        c_funloc_private%ptr = LOC(x)
    END FUNCTION c_funloc_private

    ! ---------------------------------------------------------------------
    ! Returns the C address of the argument.
    ! ---------------------------------------------------------------------
    FUNCTION c_loc_private (x)
        TYPE(c_ptr) :: c_loc_private
!DEC$ ATTRIBUTES DEFAULT :: c_loc_private
!DEC$ ATTRIBUTES NO_ARG_CHECK :: x
        INTEGER :: x   ! use any type here
        INTRINSIC :: LOC

        ! Since x has the NO_ARG_CHECK attribute, the actual address (not
        ! any descriptor address) is passed in.
        c_loc_private%ptr = LOC(x)
    END FUNCTION c_loc_private
    

    ! The following two routines are no longer used by this module, but they must
    ! remain for compatibility with previously compiled sources.
    !
    
    SUBROUTINE c_f_pointer_scalar (cptr, fptr)
!DEC$ ATTRIBUTES DEFAULT :: c_f_pointer_scalar
!DEC$ ATTRIBUTES NO_ARG_CHECK :: cptr, fptr
        INTEGER, POINTER , INTENT(IN):: cptr  ! use pointer of any type here
        INTEGER, POINTER , INTENT(OUT):: fptr  ! use pointer of any type here

        fptr => cptr
    END SUBROUTINE c_f_pointer_scalar

    SUBROUTINE c_f_procpointer_private (cptr, fptr)
!DEC$ ATTRIBUTES DEFAULT :: c_f_procpointer_private
!DEC$ ATTRIBUTES NO_ARG_CHECK :: cptr, fptr
        INTEGER, POINTER , INTENT(IN):: cptr  ! use pointer of any type here
        !PROCEDURE(), POINTER , INTENT(OUT):: fptr   ! not yet supported
        INTEGER, POINTER , INTENT(OUT):: fptr  ! use pointer of any type here

        fptr => cptr
    END SUBROUTINE c_f_procpointer_private
    
END MODULE ISO_C_BINDING
