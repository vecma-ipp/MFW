module euITM_access
!----------------------------------------------------------------
! module containing euITM access routines to the MDSplus database
!     euITMopen(server,tree,shot_number,ifail)
!     euITMclose(ifail)
! and generic subroutines to read and write  (euitmget, euitmput)
! that can be used for 0d, 1d and 2d data for real*8s.
!                      0d and 1d          for integers
!                      and strings
!
! writing data : 0d, 1d, 2d : euitmput(signal_name,value,ifail)
!
! reading data :
!     0d : eutimget(signal_name,value,ifail)
!     1d : eutimget(signal_name,values(:),ifail)
!     2d : eutimget(signal_name,values(:,:),ifail)
! string : eutimget(signal_name,a_string,ifail)
!
! values(:) and values(:,:) are allocatable arrays!
!
! Guido Huysmans 16/11/2005
!--------------------------------------------------------------
  implicit none

  interface euitmget  ! defines the generic names for the reading routines
     module procedure euitmget0, euitmget0i, euitmget1,  euitmget1i, euitmget2 ,euitmget2i, euitmgets
  endinterface

  interface euitmput  ! defines the generic routines for the writing routines
     module procedure euitmput0, euitmput0i, euitmput1,  euitmput1i, euitmput2, euitmput2i, euITMputs
  endinterface

contains

  subroutine euITMopen(server,tree,ishot,ifail)
    implicit none
    character*(*)     :: server, tree
    integer           :: ishot, istatus, ifail
    integer, external :: MdsConnect, MdsOpen

!------------------------------- connect to MDS server
    ifail = 999
    istatus = MdsConnect(trim(server)//char(0))
    if (istatus/2*2 .eq. istatus) then
       write(6,'(A)')    '  Mdsconnected to server ',trim(server)
       write(6,'(A,i8)') '    Mdsconnect returns status= ',istatus
       ifail = 1
    else
       write(6,'(A,A)') '  Mds SUCCESSFULLY connected to server : ',trim(server)
       ifail = 0
    endif

!------------------------------- open tree and shot
    istatus = MdsOpen(trim(tree)//char(0), ishot)
    if (istatus/2*2 .eq. istatus) then
       write(6,'(A,A,i12)') '  MdsOpen FAILS for (tree, ishot)   :',trim(tree),ishot
       write(6,'(A,i12)')   '  MdsOpen returns status            : ',istatus
       ifail = 2
    else
       write (6,'(A,A,i8)') '  MdsOpen SUCCESS for (tree, ishot) : ',trim(tree),ishot
       ifail = 0
    endif

    return
  end subroutine euITMopen

  subroutine euITMclose(ifail)
!------------------------------- close the connection to the MDS server
    implicit none
    integer           :: ifail,istatus
    integer, external :: MdsDisconnect

    istatus = MdsDisconnect()
    ifail   = 0

    return
  end subroutine euITMclose

  subroutine euITMget0(path,value,ifail)
!----------------------------------------------------------------------
! reads one value from the MDSserver
!----------------------------------------------------------------------
    implicit none
    include 'mdslib.inc'
    integer           :: istatus, iretlen, ifail
    character*(*)     :: path
    real*8              :: value

    write(*,'(A,A)') ' ITMget(0) : ',trim(path)

!      istatus = MdsValue2(trim(path)//CHAR(0),descr2(IDTYPE_DOUBLE,0),value,0,iretlen)
    istatus = MdsValue(trim(path)//CHAR(0),descr(IDTYPE_DOUBLE,value,1,0), 0, iretlen)

    write(*,*) ' euitmget0 : ',value

    if (istatus/2*2 .eq. istatus) then
       write(*,'(A,i12)') 'ITMget(0) problem : ',istatus
       ifail = 1
       value = 0.
       return
    endif

    ifail = 0
    return
  endsubroutine euITMget0

  subroutine euITMget0i(path,ivalue,ifail)
!----------------------------------------------------------------------
! reads one value from the MDSserver
!----------------------------------------------------------------------
    implicit none
    include 'mdslib.inc'
    integer           :: istatus, iretlen, ifail
    character*(*)     :: path
    integer           :: ivalue

    write(*,'(A,A)') ' ITMget(0i) : ',trim(path)

!      istatus = MdsValue2(trim(path)//CHAR(0),descr2(IDTYPE_DOUBLE,0),ivalue,0,iretlen)
    istatus = MdsValue(trim(path)//CHAR(0),descr(IDTYPE_LONG,ivalue,1,0), 0, iretlen)

    if (istatus/2*2 .eq. istatus) then
       write(*,'(A,i12)') 'ITMget(0i) problem : ',istatus
       ifail  = 1
       ivalue = 0
       return
    endif

    write (*,*) ' euitmget0(0i) : ',ivalue
    ifail = 0
    return
  endsubroutine euITMget0i

  subroutine euITMget1(path,values,ifail)
!----------------------------------------------------------------------
! reads an 1d allocatable array from the MDStree
!----------------------------------------------------------------------
    implicit none
    include 'mdslib.inc'
    integer           :: ndim1, istatus, iretlen, ifail, i
    character*(*)     :: path
    real*8,pointer      :: values(:)

    write(*,'(A,A)') ' ITMget(1) : ',trim(path)

    istatus= MdsValue('_x='//trim(path)//',size(_x,0)'//CHAR(0),descr(IDTYPE_LONG,ndim1,0),0,1)

    if (istatus/2*2 .eq. istatus) then
       write(*,'(A,i12)') 'ITMget(1a) problem : ',istatus
       ifail  = 1
       return
    endif

    write (*,'(A,i5)') ' number of points: ',ndim1

!     if (allocated(values)) deallocate(values)
    if (associated(values)) deallocate(values)

    allocate(values(ndim1))
    values = 0.

    istatus =  MdsValue('_x'//CHAR(0),descr(IDTYPE_DOUBLE,values,ndim1,0), 0, iretlen)

    if (istatus/2*2 .eq. istatus) then
       write(*,'(A,i12)') 'ITMget(1b) problem : ',istatus
       ifail  = 1
       values = 0.
       return
    endif

    write (*,'(A,i8)') ' array length: ',iretlen
    write (*,*) ' array: ',(values(i),i=1,ndim1)
    ifail = 0
    return
  endsubroutine euITMget1

  subroutine euITMget1i(path,ivalues,ifail)
!----------------------------------------------------------------------
! reads an 1d allocatable array from the MDStree
!----------------------------------------------------------------------
    implicit none
    include 'mdslib.inc'
    integer           :: ndim1, istatus, iretlen, ifail, i
    character*(*)     :: path
    integer,pointer   :: ivalues(:)

    write(*,'(A,A)') ' ITMget(1i) : ',path

    istatus= MdsValue('_x='//trim(path)//',size(_x,0)'//CHAR(0),descr(IDTYPE_LONG,ndim1,0),0,1)

    if (istatus/2*2 .eq. istatus) then
       write(*,'(A,i12)') 'ITMget(1ia) problem : ',istatus
       ifail  = 1
       return
    endif

    write (*,'(A,i5)') ' number of points: ',ndim1

!     if (allocated(ivalues)) deallocate(ivalues)
    if (associated(ivalues)) deallocate(ivalues)
    
    allocate(ivalues(ndim1))
    ivalues = 0.
    
    istatus =  MdsValue('_x'//CHAR(0),descr(IDTYPE_LONG,ivalues,ndim1,0), 0, iretlen)

    if (istatus/2*2 .eq. istatus) then
       write(*,'(A,i12)') 'ITMget(1i) problem : ',istatus
       ifail    = 1
       ivalues = 0
       return
    endif
    
    write (*,'(A,i8)') ' array length: ',iretlen
    write (*,*) ' array: ',(ivalues(i),i=1,ndim1)
    ifail = 0
    return
  endsubroutine euITMget1i

  subroutine euITMget2(path,values,ifail)
!----------------------------------------------------------------------
! reads an 2d allocatable array from the MDStree
!----------------------------------------------------------------------
    implicit none
    include 'mdslib.inc'
    integer           :: ndim1, ndim2, istatus, iretlen, ifail,i,j
    character*(*)     :: path
    real*8,pointer      :: values(:,:)

    write(*,'(A,A)') ' ITMget(2) : ',path
    
    istatus= MdsValue('_x='//trim(path)//',size(_x,0)'//CHAR(0),descr(IDTYPE_LONG,ndim1,0),0,1)

    if (istatus/2*2 .eq. istatus) then
       write(*,'(A,i12)') 'ITMget(2a) problem : ',istatus
       ifail  = 1
       return
    endif

    istatus= MdsValue('size(_x,1)'//CHAR(0),descr(IDTYPE_LONG,ndim2,0),0,1)

    if (istatus/2*2 .eq. istatus) then
       write(*,'(A,i12)') 'ITMget(2b) problem : ',istatus
       ifail  = 1
       return
    endif

    write (*,'(A,2i5)') ' ndim1, ndim2: ',ndim1,ndim2

!     if (allocated(values)) deallocate(values)
    if (associated(values)) deallocate(values)

    allocate(values(ndim1,ndim2))
    values = 0.

    istatus =  MdsValue('_x'//CHAR(0),descr(IDTYPE_DOUBLE,values,ndim1,ndim2,0), 0, iretlen)

    if (istatus/2*2 .eq. istatus) then
       write(*,'(A,i12)') 'ITMget(2c) problem : ',istatus
       ifail = 1
       values = 0.
       return
    endif

    do j=1,ndim2
       write (*,*) j,(values(i,j),i=1,ndim1)
    enddo

    ifail = 0
    return
  endsubroutine euITMget2

  subroutine euITMget2i(path,values,ifail)
!----------------------------------------------------------------------
! reads an 2d allocatable array from the MDStree
!----------------------------------------------------------------------
    implicit none
    include 'mdslib.inc'
    integer           :: ndim1, ndim2, istatus, iretlen, ifail,i,j
    character*(*)     :: path
    integer,pointer   :: values(:,:)

    write(*,'(A,A)') ' ITMget(2) : ',path

    istatus= MdsValue('_x='//trim(path)//',size(_x,0)'//CHAR(0),descr(IDTYPE_LONG,ndim1,0),0,1)

    if (istatus/2*2 .eq. istatus) then
       write(*,'(A,i12)') 'ITMget(2a) problem : ',istatus
       ifail  = 1
       return
    endif

    istatus= MdsValue('size(_x,1)'//CHAR(0),descr(IDTYPE_LONG,ndim2,0),0,1)

    if (istatus/2*2 .eq. istatus) then
       write(*,'(A,i12)') 'ITMget(2b) problem : ',istatus
       ifail  = 1
       return
    endif

    write (*,'(A,2i5)') ' ndim1, ndim2: ',ndim1,ndim2

!     if (allocated(values)) deallocate(values)
    if (associated(values)) deallocate(values)

    allocate(values(ndim1,ndim2))
    values = 0.

    istatus =  MdsValue('_x'//CHAR(0),descr(IDTYPE_DOUBLE,values,ndim1,ndim2,0), 0, iretlen)
    
    if (istatus/2*2 .eq. istatus) then
       write(*,'(A,i12)') 'ITMget(2c) problem : ',istatus
       ifail = 1
       values = 0.
       return
    endif

    do j=1,ndim2
       write (*,*) j,(values(i,j),i=1,ndim1)
    enddo

    ifail = 0
    return
  endsubroutine euITMget2i


  subroutine euITMgets(path,astring,ifail)
!----------------------------------------------------------------------
! reads a string from the MDStree
!----------------------------------------------------------------------
    implicit none
    include 'mdslib.inc'
    integer           :: ndim1, istatus, iretlen, ifail, i,itest
    character*(*)     :: path, astring
    character(100)    :: bstring

    write(*,'(A,A)') ' ITMget(s) : ',path
    istatus= MdsValue('_x='//trim(path)//',size(_x,0)'//CHAR(0),descr(IDTYPE_SHORT,ndim1,0),0,1)
    if (istatus/2*2 .eq. istatus) then
       write(*,'(A,i12)') 'ITMget(sa) problem : ',istatus
       ifail  = 1
       return
    endif

    astring = ''
    ndim1=len(astring)

    istatus = MdsValue('_x'//CHAR(0),descr(IDTYPE_CSTRING,astring,0,ndim1), 0, iretlen)

    if (istatus/2*2 .eq. istatus) then
       write(*,'(A,i12)') 'ITMget(sb) problem : ',istatus
       ifail  = 1
       return
    endif

    write (*,'(A,A)') ' string : ',astring

    ifail = 0
    return
  endsubroutine euITMgets

  subroutine euITMput0(path,value,ifail)
!----------------------------------------------------------------------
! writes one value to the MDStree
!----------------------------------------------------------------------
    implicit none
    include 'mdslib.inc'
    integer           :: ianswer, istatus, iretlen, ifail, ndim, i
    character*(*)     :: path
    real*8              :: value
    
    write(*,'(A,A)')  ' ITMput(0) : ',trim(path)
    write(*,*) ' value : ',value

    istatus = MdsPut2(trim(path)//CHAR(0),'$'//char(0),descr2(IDTYPE_DOUBLE,1,0),value,0)

    if (istatus/2*2 .eq. istatus) then
       write(*,'(A,i12)') 'ITMput(0) problem : ',istatus
       ifail = 1
       return
    endif
    ifail = 0
    return
  endsubroutine euITMput0

  subroutine euITMput1(path,values,ifail)
!----------------------------------------------------------------------
! writes a 1d array to the MDSserver
!----------------------------------------------------------------------
    implicit none
    include 'mdslib.inc'
    integer           :: ndim1, istatus, iretlen, ifail,i
    character*(*)     :: path
    real*8,pointer      :: values(:)

    write(*,'(A,A)')  ' ITMput(1) : ',trim(path)

    if (.not. associated(values)) then
       write(*,*) ' values not allocated, returning'
       return
    endif

    ndim1 = size(values)
    
    if (ndim1 .le. 0) return

    write(*,'(A,i8)') ' ITMput(1) : ',ndim1

    istatus = MdsPut2(path//char(0),'$'//char(0),descr2(IDTYPE_DOUBLE,ndim1,0),values,0)

    if (istatus/2*2 .eq. istatus) then
       write(*,'(A,i12)') 'ITMput(1) problem : ',istatus
       ifail = 1
       return
    endif
    write (*,'(A,i8)') ' array length: ',iretlen
    write (*,*) ' array: ',(values(i),i=1,ndim1)
    ifail = 0
    return
  endsubroutine euITMput1


  subroutine euITMput2(path,values,ifail)
!----------------------------------------------------------------------
! writes a 2d array to the MDStree
!----------------------------------------------------------------------
    implicit none
    include 'mdslib.inc'
    integer             :: ndim1, ndim2, ianswer, istatus, iretlen, ifail, ndim, i
    character*(*)       :: path
    real*8,pointer        :: values(:,:)

    write(*,'(A,A)')  ' ITMput(2) : ',trim(path)

    if (.not. associated(values)) then
       write(*,*) ' values not allocated, returning'
       return
    endif

    ndim1 = size(values,1)
    ndim2 = size(values,2)

    write(*,'(A,2i8)') ' ITMput(2) : ',ndim1,ndim2

    istatus = MdsPut2(trim(path)//CHAR(0),'$'//char(0),descr2(IDTYPE_DOUBLE,ndim1,ndim2,0),values,0)

    if (istatus/2*2 .eq. istatus) then
       write(*,'(A,i12)') 'ITMput(2) problem : ',istatus
       ifail = 1
       return
    endif
    ifail = 0
    
    return
  endsubroutine euITMput2

  subroutine euITMput2i(path,values,ifail)
!----------------------------------------------------------------------
! writes a 2d array to the MDStree
!----------------------------------------------------------------------
    implicit none
    include 'mdslib.inc'
    integer             :: ndim1, ndim2, ianswer, istatus, iretlen, ifail, ndim, i
    character*(*)       :: path
    integer,pointer     :: values(:,:)

    write(*,'(A,A)')  ' ITMput(2) : ',trim(path)
   
    if (.not. associated(values)) then
       write(*,*) ' values not allocated, returning'
       return
    endif

    ndim1 = size(values,1)
    ndim2 = size(values,2)

    write(*,'(A,2i8)') ' ITMput(2) : ',ndim1,ndim2

    istatus = MdsPut2(trim(path)//CHAR(0),'$'//char(0),descr2(IDTYPE_DOUBLE,ndim1,ndim2,0),values,0)

    if (istatus/2*2 .eq. istatus) then
       write(*,'(A,i12)') 'ITMput(2) problem : ',istatus
       ifail = 1
       return
    endif
    ifail = 0
    return
  endsubroutine euITMput2i

  subroutine euITMput0i(path,ivalue,ifail)
!----------------------------------------------------------------------
! writes one value to the MDStree
!----------------------------------------------------------------------
    implicit none
    include 'mdslib.inc'
    integer           :: ianswer, istatus, iretlen, ifail, ndim, i
    character*(*)     :: path
    integer           :: ivalue

    write(*,'(A,A)')  ' ITMput(0i) : ',trim(path)
    write(*,*) ' value : ',ivalue

    istatus = MdsPut2(trim(path)//CHAR(0),'$'//char(0),descr2(IDTYPE_LONG,1,0),ivalue,0)

    if (istatus/2*2 .eq. istatus) then
       write(*,'(A,i12)') 'ITMput(0i) problem : ',istatus
       ifail = 1
       return
    endif
    ifail = 0
    return
  endsubroutine euITMput0i

  subroutine euITMput1i(path,ivalues,ifail)
!----------------------------------------------------------------------
! writes a 1d array to the MDSserver
!----------------------------------------------------------------------
    implicit none
    include 'mdslib.inc'
    integer              :: ndim1, istatus, iretlen, ifail
    character*(*)        :: path
    integer,dimension(:) :: ivalues

    write(*,'(A,A)')  ' ITMput(1i) : ',trim(path)

    ndim1 = size(ivalues)

    write(*,'(A,i8)') ' ITMput(1i) : ',ndim1

    istatus = MdsPut2(path//char(0),'$'//char(0),descr2(IDTYPE_LONG,ndim1,0),ivalues,0)

    if (istatus/2*2 .eq. istatus) then
       write(*,'(A,i12)') 'ITMput(1i) problem : ',istatus
       ifail = 1
       return
    endif
    ifail = 0
    return
  endsubroutine euITMput1i
  
  subroutine euITMputs(path,astring,ifail)
!----------------------------------------------------------------------
! writes one value to the MDStree
!----------------------------------------------------------------------
    implicit none
    include 'mdslib.inc'
    integer           :: ianswer, istatus, iretlen, ifail, ndim1, i
    character*(*)     :: path, astring

    write(*,'(A,A)')  ' ITMput(s) : ',trim(path)
    write(*,'(A,A)')  ' value     : ',trim(astring)

    ndim1 = LEN(trim(astring))

    write(*,'(A,i5)') ' length    : ',ndim1
 
    if (ndim1 .eq. 0) return

    istatus = MdsPut2(trim(path)//CHAR(0),'$'//char(0),descr2(IDTYPE_CSTRING,0,ndim1),trim(astring),0)

    if (istatus/2*2 .eq. istatus) then
       write(*,'(A,i12)') 'ITMput(s) problem : ',istatus
       ifail = 1
       return
    endif
    ifail = 0
    return
  endsubroutine euITMputs
  
end module euITM_access
