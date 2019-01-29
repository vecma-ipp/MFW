! Storage/Buffer for 0D diagnostic quantities.
!
!   B. McMillan, CRPP/EPFL.
!   December 2007
!
! Uses a hashtable to store the data. The key is a string
! containing the name of the diagnostic. 
MODULE hashtable
  USE futils
  IMPLICIT NONE
  INCLUDE 'mpif.h'
    
  PRIVATE
  PUBLIC :: ref_htable, add_record, htable_to_hdf5_int, htable_init, htable_endstep
  PUBLIC :: htable_hdf5_flush,set_htable_fileid

  INTEGER, PARAMETER :: maxnamelen=32
  INTEGER, PARAMETER :: maxdesclen=256

  TYPE, PUBLIC ::  mpi_reduce_pars
     INTEGER :: communicator
     INTEGER :: mpiop
     LOGICAL :: do_nothing
  END TYPE mpi_reduce_pars

  ! Datatype for storing a 0D diagnostic in a list.
  TYPE, PUBLIC :: lel
     ! Name of the diagnostic (HDF5 datagroup).
     INTEGER :: name_length
     CHARACTER(len=maxnamelen)       :: name
     ! Description of the diagnostic. 
     INTEGER :: description_length
     CHARACTER(len=maxdesclen)       :: description
     ! Buffer of values.
     DOUBLE PRECISION, POINTER, DIMENSION(:) :: value => NULL()
     ! Next element in list
     TYPE(lel), POINTER          :: next => NULL()
     ! Collective operation MPI
     TYPE(mpi_reduce_pars)       :: mpiparams
     INTEGER                     :: mpiparamno
  END type lel
  
  TYPE, PUBLIC :: lelptr
     TYPE(lel), POINTER :: pntr => NULL()
  END TYPE lelptr

  ! Iterator for the hashtable
  TYPE leliter
     INTEGER :: num                ! Position in Hashtable array
     TYPE(lelptr), DIMENSION(:), POINTER :: root ! Iterator's Hashtable
     TYPE(lel), POINTER    :: ptr  ! Current element in chained list.
  END TYPE leliter


  TYPE, PUBLIC :: BUFFER_TYPE

     ! HDF5 file handle
     INTEGER         :: fresid                  
     ! Name of group to place 0D variables in.
     CHARACTER(len=maxdesclen) :: groupname
     ! node which performs IO via futils/HDF5
     INTEGER        :: ionode

     ! Communicators for parallel sum
     TYPE(mpi_reduce_pars), DIMENSION(:), POINTER :: mpireducepars   => null()
     INTEGER,               DIMENSION(:), POINTER :: mpireducenum    => null()
     INTEGER,               DIMENSION(:), POINTER :: mpireduceindex  => null()
     INTEGER               :: nmpipars

     ! Length of buffer
     INTEGER     :: buffer_length = 1
     INTEGER     :: current_buffer_pos = 1

     ! Mask for mapping the hash value into the array 
     INTEGER     :: tablelength_mask
     ! Number of items in hashtable
     INTEGER     :: numels = 0
     ! The hashtable: an array of lists
     TYPE(lelptr), POINTER, DIMENSION(:) :: htable => null()

     ! Temporary arrays for MPI sum
     DOUBLE PRECISION, DIMENSION(:,:), POINTER :: psum_arr  => null()
     DOUBLE PRECISION, DIMENSION(:,:), POINTER :: psum_arr2 => null()

  END TYPE BUFFER_TYPE

  ! We want the pointer members of TYPE(lel) to behave differently under
  ! assignment: the 'value' element should be copied across into 
  ! freshly allocated storage.
  INTERFACE ASSIGNMENT (=)
     MODULE PROCEDURE ASSIGN_LEL_TO_LEL
  END INTERFACE

  INTERFACE OPERATOR (==)
     MODULE PROCEDURE EQUALITY_TYPE_MPARAMS
  END INTERFACE

CONTAINS
  SUBROUTINE ASSIGN_LEL_TO_LEL(el1,el2)
    TYPE(lel), INTENT(OUT)  :: el1
    TYPE(lel), INTENT(IN)   :: el2
    el1%name = el2%name
    el1%mpiparamno = el2%mpiparamno
    el1%mpiparams = el2%mpiparams
    el1%name_length = el2%name_length
    el1%description = el2%description
    el1%description_length = el2%description_length
    IF(ASSOCIATED(el1%value)) THEN
       DEALLOCATE(el1%value)
    END IF
    ALLOCATE(el1%value(SIZE(el2%value)))
    el1%value = el2%value
  END SUBROUTINE ASSIGN_LEL_TO_LEL

  FUNCTION EQUALITY_TYPE_MPARAMS(mp1,mp2)
    TYPE(mpi_reduce_pars), INTENT(IN) :: mp1,mp2
    LOGICAL :: EQUALITY_TYPE_MPARAMS
    EQUALITY_TYPE_MPARAMS= &
         & ((mp1%communicator==mp2%communicator).AND.(mp1%mpiop==mp2%mpiop)).OR.(mp1%do_nothing.AND.mp2%do_nothing)
  END FUNCTION  EQUALITY_TYPE_MPARAMS

  SUBROUTINE set_htable_fileid(buf,fresid_in,groupname_in)
    TYPE(BUFFER_TYPE), INTENT(INOUT) :: buf 
    INTEGER, INTENT(IN) :: fresid_in
    CHARACTER(len=*), INTENT(IN), OPTIONAL :: groupname_in
    CHARACTER(len=*), parameter :: default_groupname = '/data/var0d/'
    buf%fresid = fresid_in
    IF(PRESENT(groupname_in)) THEN
       buf%groupname = groupname_in
    ELSE
       buf%groupname = default_groupname
    END IF
  END SUBROUTINE SET_HTABLE_FILEID

  SUBROUTINE htable_init(buf,buffer_length_in,ionode_in)
    TYPE(BUFFER_TYPE), INTENT(INOUT) :: buf 
    INTEGER, INTENT(IN), OPTIONAL :: buffer_length_in
    INTEGER, INTENT(IN), OPTIONAL :: ionode_in
    ! Default ionode is zero.
    IF(PRESENT(ionode_in)) THEN
       buf%ionode = ionode_in
    ELSE
       buf%ionode = 0
    END IF

    IF(PRESENT(buffer_length_in)) THEN
       buf%buffer_length = buffer_length_in
    ELSE
       buf%buffer_length = 1
    END IF

  END SUBROUTINE htable_init

  ! Add element to list (interface to recursive function `add')
  SUBROUTINE add_element(el,lptr)
    TYPE(lel), POINTER ::  el
    TYPE(lelptr) :: lptr
    lptr%pntr => add(el,lptr%pntr)
  END SUBROUTINE add_element

  RECURSIVE FUNCTION add(el,list) RESULT(add_res)
    TYPE(lel), POINTER ::  list,el,add_res
    IF (.not.associated(list)) THEN
       el%next => list
       add_res => el
       RETURN
    END IF
    IF (list%name<el%name) THEN
       el%next => list
       add_res => el
       RETURN
    END IF
    list%next => add(el,list%next)
    add_res => list
  END FUNCTION add
  
  ! Resize hashtable to power of 2 >= length_hint.
  SUBROUTINE set_htable_size(buf,length_hint)
    TYPE(BUFFER_TYPE), INTENT(INOUT) :: buf 
    INTEGER, INTENT(IN) :: length_hint
    INTEGER             :: log2size,htable_size,one
    TYPE(lelptr), DIMENSION(:), POINTER :: old_htable
    TYPE(lel), POINTER  :: curr,new_el
    TYPE(leliter)       :: iter

    one = 1 
    log2size =  CEILING(LOG(1.0*length_hint)/LOG(2.0))
    htable_size = 2**log2size
    buf%tablelength_mask = ISHFTC(one,log2size)-1

    IF(ASSOCIATED(buf%htable)) THEN
       ! Copy elements into new hashtable.
       old_htable => buf%htable
       ALLOCATE(buf%htable(0:htable_size-1))
       CALL restart(old_htable,iter)
       DO WHILE(.NOT.finished(iter))
          curr => iter%ptr
          ALLOCATE(new_el)
          new_el = curr
          CALL add_element(new_el,buf%htable(hash(buf,curr%name)))
          CALL increment(iter)
          IF(ASSOCIATED(curr)) THEN
             DEALLOCATE(curr)
          END IF
       END DO
       DEALLOCATE(old_htable)      
    ELSE
       ALLOCATE(buf%htable(0:htable_size-1))
    END IF
  END SUBROUTINE SET_HTABLE_SIZE

  ! Place iterator at first element in 
  ! hashtable
  SUBROUTINE restart(hashtable,iter)
    TYPE(lelptr), POINTER, DIMENSION(:) :: hashtable
    TYPE(leliter)       :: iter
    iter%num=0
    iter%root=>hashtable
    iter%ptr=>hashtable(iter%num)%pntr
    IF(.NOT.ASSOCIATED(iter%ptr)) THEN
       CALL increment(iter)
    END IF
  END SUBROUTINE restart

  ! Move to next entry in hash table
  SUBROUTINE increment(iter)
    TYPE(leliter)       :: iter 
    ! Move along the list.
    IF (ASSOCIATED(iter%ptr)) THEN
       iter%ptr => iter%ptr%next
    END IF
    ! We have fallen off the chain.
    IF(.NOT.ASSOCIATED(iter%ptr)) THEN
       ! End of the chained list.
       ! Find next nonempty list and position iterator at start 
       DO 
          iter%num =iter%num+1
          IF(iter%num>=size(iter%root)) EXIT
          IF(ASSOCIATED(iter%root(iter%num)%pntr)) EXIT
       END DO
       IF( iter%num < size(iter%root) ) THEN
          iter%ptr => iter%root(iter%num)%pntr
       ELSE
          iter%ptr => null()
       END IF
    END IF
  END SUBROUTINE increment

  ! Check whether iterator is finished
  LOGICAL FUNCTION finished(iter)
    TYPE(leliter)       :: iter    
    finished = (iter%num.GE.size(iter%root))
  END FUNCTION finished

  INTEGER FUNCTION hash(buf,in)
    TYPE(BUFFER_TYPE), INTENT(INOUT) :: buf 
    CHARACTER(len=*) :: in
    hash = hash_int(in,buf%tablelength_mask)
  END FUNCTION hash

  ! hash function based on 
  ! Jenkin's one at a time hash
  INTEGER FUNCTION hash_int(in,lengthmask)
    CHARACTER(len=*), INTENT(IN) :: in
    INTEGER, INTENT(IN)          :: lengthmask
    INTEGER                      :: i, ch

    hash_int = 0
    DO i=1,len(in)
       ch = ICHAR(in(i:i))
       hash_int = hash_int + ch
       hash_int = hash_int + ISHFT(hash_int, 10)
       hash_int = IEOR(hash_int, ISHFT(hash_int,-6))
    END DO
    hash_int = hash_int + ISHFT(hash_int,3)
    hash_int = IEOR(hash_int,ISHFT(hash_int,-11))
    hash_int = hash_int + ISHFT(hash_int,15)
    hash_int = IAND(hash_int,lengthmask)
  END FUNCTION hash_int

  ! Add value to hashtable, creating new record if necessary 
  SUBROUTINE add_record(buf,name,description,value,parallel_comm, mpi_operation)
    TYPE(BUFFER_TYPE), INTENT(INOUT) :: buf 
    CHARACTER(len=*), INTENT(IN) :: name
    CHARACTER(len=*), INTENT(IN) :: description
    DOUBLE PRECISION, INTENT(IN) :: value
    INTEGER, OPTIONAL,INTENT(IN) :: parallel_comm
    INTEGER, OPTIONAL,INTENT(IN) :: mpi_operation
    TYPE(lel), POINTER  :: record 
    INTEGER             :: pcomm, mpiop

    record => ref_htable(buf,name)

    IF (.NOT.ASSOCIATED(record)) THEN
       ! Not already in table
       CALL add_to_htable(buf,name,description,parallel_comm,mpi_operation)
       record => ref_htable(buf,name)
    END IF
    ! May be safer to explicitly allocate memory
    ! for the buffer here rather than elsewhere
    IF (.NOT.ASSOCIATED(record%value)) THEN
       ALLOCATE(record%value(buf%buffer_length))
       record%value = 0.0 
    END IF

    record%value(buf%current_buffer_pos) = value
    
  END SUBROUTINE add_record

  SUBROUTINE add_to_htable(buf,name,description,pcomm, mpiop)
    TYPE(BUFFER_TYPE) :: buf 
    CHARACTER(len=*), INTENT(IN) :: name,description
    INTEGER, INTENT(IN), OPTIONAL :: pcomm
    INTEGER, INTENT(IN), OPTIONAL :: mpiop
    TYPE(lel), POINTER :: newel
    LOGICAL            :: resize

    buf%numels = buf%numels + 1

    ! resize hashtable if necessary.
    IF(.NOT.ASSOCIATED(buf%htable)) THEN
       resize=.true.
    ELSE 
       resize=(buf%numels>SIZE(buf%htable))
    END IF
    IF (resize) CALL set_htable_size(buf,buf%numels)

    ALLOCATE(newel)
    IF(PRESENT(pcomm)) newel%mpiparams%communicator = pcomm     
    IF(PRESENT(mpiop)) THEN
       newel%mpiparams%mpiop        = mpiop
    ELSE
       newel%mpiparams%mpiop        = MPI_SUM
    END IF
    newel%mpiparams%do_nothing = .NOT.PRESENT(pcomm)
    newel%name  = name
    newel%name_length = min(len(newel%name),len(name))
    newel%description  = description
    newel%description_length = min(len(newel%description),len(description))
    CALL add_element(newel,buf%htable(hash(buf,newel%name)))
  END SUBROUTINE add_to_htable
  
  ! Print out the hashes, and the elements
  SUBROUTINE print_htable(buf)
    TYPE(BUFFER_TYPE) :: buf 
    INTEGER :: i
    TYPE(lel), POINTER :: curr

    DO i=0,size(buf%htable)-1
       print *,'Hash: ',i
       curr => buf%htable(i)%pntr
       DO WHILE(ASSOCIATED(curr))
          print *,'el: "',curr%name, '"'
          curr => curr%next 
       END DO
    END DO

  END SUBROUTINE print_htable

  ! After all the data has been placed in the buffer,
  ! advance along the buffer and flush if necessary
  SUBROUTINE htable_endstep(buf)
    TYPE(BUFFER_TYPE), INTENT(INOUT) :: buf 
    buf%current_buffer_pos = buf%current_buffer_pos + 1
    IF (buf%current_buffer_pos > buf%buffer_length) THEN
       buf%current_buffer_pos = 1
       CALL htable_to_hdf5_int(buf,buf%buffer_length)       
    END IF
  END SUBROUTINE htable_endstep

  ! Clear buffer without flushing
  !
  SUBROUTINE htable_clear(buf)
    TYPE(BUFFER_TYPE), INTENT(INOUT) :: buf 
    buf%current_buffer_pos = 1    
  END SUBROUTINE htable_clear

  ! Flush previously written data
  ! after a time step
  SUBROUTINE htable_hdf5_flush(buf)
    TYPE(BUFFER_TYPE), INTENT(INOUT) :: buf 
    CALL htable_to_hdf5_int(buf,buf%current_buffer_pos - 1)
    buf%current_buffer_pos = 1
  END SUBROUTINE htable_hdf5_flush

  ! Output the contents to the HDF5 file, creating 
  ! descriptors if necessary
  SUBROUTINE htable_to_hdf5_int(buf,num_elements)
    TYPE(BUFFER_TYPE), INTENT(INOUT) :: buf 
    INCLUDE 'mpif.h'
    TYPE(leliter)       :: iter
    TYPE(lel), POINTER  :: curr
    INTEGER, DIMENSION(1) :: dims = (/0/)
    INTEGER, INTENT(IN)  :: num_elements
    INTEGER :: num,me_world,ierr

    IF(num_elements==0) RETURN

    ! Do parallel sum
    CALL psum_htable(buf)

    CALL restart(buf%htable,iter)
    
    CALL MPI_COMM_RANK(MPI_COMM_WORLD,me_world,ierr)
    IF(me_world/=buf%ionode) RETURN
    num = 1

    DO WHILE(.NOT.finished(iter))
       curr => iter%ptr
       IF (.NOT.isdataset(buf%fresid, TRIM(buf%groupname)//'/'//curr%name(1:curr%name_length))) THEN
          !PRINT *,'Create dataset: ',TRIM(groupname)//'/'//curr%name(1:curr%name_length),'::', &
          !     &curr%description(1:curr%description_length)
          !PRINT *,'fresid: ',fresid
          CALL creatd(buf%fresid, 0, dims, TRIM(buf%groupname)//'/'//curr%name(1:curr%name_length), &
               & curr%description(1:curr%description_length))             
          CALL attach(buf%fresid,TRIM(buf%groupname)//'/'//curr%name(1:curr%name_length), &
               & 'PlotOrder',num)             
       END IF
       !PRINT *,'Append value: ', curr%name(1:curr%name_length), num_elements, curr%value(1:num_elements)
       CALL append(buf%fresid,TRIM(buf%groupname)//'/'//curr%name(1:curr%name_length),curr%value(1:num_elements))
       !PRINT *,'~Append value: '
       CALL increment(iter)
       num = num + 1
    END DO
  END SUBROUTINE htable_to_hdf5_int

  FUNCTION ref_htable(buf,name)
    TYPE(BUFFER_TYPE), INTENT(INOUT) :: buf 
    CHARACTER(len=*) :: name
    TYPE(lel), POINTER :: ref_htable
    TYPE(lel), POINTER :: curr
    TYPE(lel) :: del 

    IF(.NOT.ASSOCIATED(buf%htable)) THEN
       ref_htable => null()
       RETURN
    END IF

    del%name = name
    curr => buf%htable(hash(buf,del%name))%pntr
    DO WHILE(ASSOCIATED(curr))
       IF (curr%name==name) THEN
          ref_htable => curr
          RETURN
       END IF
       curr => curr%next 
    END DO
    ref_htable => null()
  END FUNCTION ref_htable

  ! Do a parallel sum over some values in the hashtable,
  ! using an array to consolidate it into a single MPI operation.
  SUBROUTINE psum_htable(buf)
    TYPE(BUFFER_TYPE), INTENT(INOUT) :: buf 

    CALL find_reduce_pars(buf)
    CALL psum_htable_int(buf)

  END SUBROUTINE psum_htable

  ! Find all the different combinations of (MPI_COMMUNICATOR,MPI_OPERATION)
  ! and put them in a list.
  ! This could be optimised by using a sorting algorithm, but probably
  ! not useful except for very large cases.
  SUBROUTINE find_reduce_pars(buf)
    TYPE(BUFFER_TYPE), INTENT(INOUT) :: buf 
    TYPE(lel), POINTER  :: curr
    TYPE(leliter)       :: iter
    INTEGER             :: paramno, i, cnum

    IF(.NOT.ASSOCIATED(buf%mpireducenum)) THEN
       ALLOCATE(buf%mpireducepars(buf%numels))
       ALLOCATE(buf%mpireduceindex(buf%numels))
       ALLOCATE(buf%mpireducenum(buf%numels))
    END IF
    IF(SIZE(buf%mpireducepars)<buf%numels) THEN
       DEALLOCATE(buf%mpireducepars)
       DEALLOCATE(buf%mpireducenum)
       DEALLOCATE(buf%mpireduceindex)
       ALLOCATE(buf%mpireducepars(buf%numels))
       ALLOCATE(buf%mpireduceindex(buf%numels))
       ALLOCATE(buf%mpireducenum(buf%numels))
    END IF

    buf%nmpipars = 0
    buf%mpireducenum(:) = 0

    CALL restart(buf%htable,iter)
    DO WHILE(.NOT.finished(iter))
       curr => iter%ptr
       paramno = 0 
       DO i=1, buf%nmpipars 
          if (buf%mpireducepars(i) ==  curr%mpiparams) THEN
             paramno = i
          END IF
       END DO
       IF (paramno==0) THEN
          buf%nmpipars = buf%nmpipars +1
          buf%mpireducepars(buf%nmpipars) = curr%mpiparams
          paramno = buf%nmpipars 
       END IF
       curr % mpiparamno = paramno
       buf%mpireducenum(paramno) = buf%mpireducenum(paramno) + 1
       CALL increment(iter)
    END DO

    cnum = 1
    buf%mpireduceindex(1) = cnum
    DO i=2,buf%nmpipars
       buf%mpireduceindex(i) = buf%mpireducenum(i-1) + cnum
       cnum = buf%mpireduceindex(i)
    END DO
  END SUBROUTINE find_reduce_pars

  ! For each communicator group.
  SUBROUTINE psum_htable_int(buf)
    INCLUDE 'mpif.h'
    TYPE(BUFFER_TYPE), INTENT(INOUT)   :: buf 
    INTEGER             :: ierr,curr_num
    TYPE(leliter)       :: iter
    TYPE(lel), POINTER  :: curr
    INTEGER             :: me_world,num_send,group,mpiop
    INTEGER             :: i

    IF(ASSOCIATED(buf%psum_arr)) THEN
       IF(SIZE(buf%psum_arr,2)<buf%numels) THEN
          DEALLOCATE(buf%psum_arr)
          DEALLOCATE(buf%psum_arr2)
       END IF
    END IF
    IF(.NOT.ASSOCIATED(buf%psum_arr)) THEN
       ALLOCATE(buf%psum_arr( buf%buffer_length,buf%numels))
       ALLOCATE(buf%psum_arr2(buf%buffer_length,buf%numels))
    END IF

    CALL MPI_COMM_RANK(MPI_COMM_WORLD,me_world,ierr)
    
    ! Loop through hashtable, putting elements which need to be
    ! summed into an array.
    buf%mpireducenum(:) = 0
    CALL restart(buf%htable,iter)
    DO WHILE(.NOT.finished(iter))
       curr => iter%ptr
       buf%psum_arr(:, buf%mpireducenum(curr%mpiparamno)+buf%mpireduceindex(curr%mpiparamno) ) = curr%value
       buf%mpireducenum(curr%mpiparamno) = buf%mpireducenum(curr%mpiparamno) + 1
       CALL increment(iter)
   END DO

    num_send = SIZE(buf%psum_arr,1)*buf%numels 

    DO i=1, buf%nmpipars
       group    = buf%mpireducepars(i)%communicator
       mpiop    = buf%mpireducepars(i)%mpiop
       IF(.NOT.buf%mpireducepars(i)%do_nothing) THEN
          CALL MPI_ALLREDUCE(buf%psum_arr( 1,buf%mpireduceindex(i)),   &
                          &  buf%psum_arr2(1,buf%mpireduceindex(i)),   & 
                          &  buf%mpireducenum(i)*buf%buffer_length,                      &
                          &  MPI_DOUBLE_PRECISION, mpiop, group, ierr)
       ELSE
          buf%psum_arr2( :,buf%mpireduceindex(i):buf%mpireduceindex(i)+buf%mpireducenum(i)-1) &
               & = buf%psum_arr( :,buf%mpireduceindex(i):buf%mpireduceindex(i)+buf%mpireducenum(i)-1)
       END IF
    END DO

    ! Put values back into hashtable
    buf%mpireducenum(:) = 0
    CALL restart(buf%htable,iter)
    DO WHILE(.NOT.finished(iter))
       curr => iter%ptr
       curr%value = buf%psum_arr2(:, buf%mpireducenum(curr%mpiparamno)+buf%mpireduceindex(curr%mpiparamno) )
       buf%mpireducenum(curr%mpiparamno) = buf%mpireducenum(curr%mpiparamno) + 1
       CALL increment(iter)
    END DO

  END SUBROUTINE psum_htable_int

END MODULE hashtable

