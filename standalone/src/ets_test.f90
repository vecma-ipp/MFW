!> This test shows the usage of PEC method for the ETS+UQ with MPI parallezation 
! ...............................................................................
program ets_test

  use iso_fortran_env
  use mod_numrec
  use mod_index
  use read_structures
  use write_structures
  use ets_standalone
  use deallocate_structures
!#ifdef MPI
  use mpi
!#endif

  implicit none

  double precision                              :: a,sum1,sum2

  double precision, allocatable, dimension(:)   :: apar,sigpar,     &
                                                   hpol,xi,w,       &
                                                   germ,            &
                                                   aknorm,sk

  double precision, allocatable, dimension(:,:) :: aspeicher,hpolxi,&
                                                   akt, yfunc 
  
  double precision, allocatable, dimension(:, :) :: akt_glo
  
  integer  :: np, nrun, ipar, npar, ntar
  ! need to double integer range
  integer  :: iflag_64            
  ! big numbers
  integer(int64) :: j64, k64, l64, ihelp64, jtest64, mmax64, np64, nall64  
  integer(int64), dimension(:), allocatable  :: hnorm64

  integer, allocatable, dimension(:)   :: jterme, ifunchelp
  integer, allocatable, dimension(:,:) :: ihe, ifunc

  integer :: i, j, k, l, m, mmax, ihelp, ios
  integer :: isum, npterme, nfterme, nall, nsk

  character*13 nstr
  
  ! timestp final
  real(R8)  :: tau = 0.01
  
  ! ... input structure and path for input files directory
  type (type_coretransp), pointer :: coret(:) => NULL()
  character(len=*), parameter :: data_path = 'standalone/data/ETS/gem_spread_4/'
  
  ! ... For MPI 
  integer :: ibeg, iend, nloc
  double precision   :: t1, t2
  integer :: comm, ierr, rank, nbprocs

  ! ... 
!#ifdef MPI
  comm =  MPI_COMM_WORLD
  call MPI_INIT(ierr)
  call MPI_COMM_RANK(comm, rank, ierr)
  call MPI_COMM_SIZE(comm, nbprocs, ierr)
  t1 = MPI_WTIME()
!#else
!  nbprocs = 1
!  rank = 0
!#endif
  
  if (rank==0)  write(6,*)'>>> Nb PROCS: ',nbprocs

  !
  nfterme = 0
  npterme = 0

  np   = 3   ! Ordnung P der Hermiteschen Polynome fuer Gauss Quadratur
  npar = 4   ! number of parameters
  ntar = 100 ! number of target values
  
  ! ... Arrays to collect  parameter values and uncertainty
  allocate(apar(npar))
  allocate(sigpar(npar))
  allocate(coret(1))
  
  !...  Read parameter values
  open (unit = 10, file = data_path // "ets_coretransp_in.cpo", &
        status = 'old', form = 'formatted', &
        action = 'read', iostat = ios)
  if (ios == 0) then
    close (10)
    call open_read_file(10, data_path // "ets_coretransp_in.cpo")
    call read_cpo(coret(1), 'coretransp' )
    call close_read_file
    
    apar(1:npar)   = coret(1)%values(1)%te_transp%diff_eff       
    sigpar(1:npar) = 0.2d0*apar(:) ! parameter uncertainty
    
    else
      print *,"ERROR: ets.coretransp_in.cpo file." 
      STOP
  end if
  call deallocate_cpo(coret)

  if (rank==0) write(6,*)'>>> np Gauss-Hermite quadrature formula:', np

  ! Aufruf der NumRec-Routinen mit  1,...,P+1, wenn P die hoechste Ordnung ist
  np = np+1

  allocate(hpol(np))
  allocate(hnorm64(np))
  allocate(xi(np+1))
  allocate(w(np+1))

  ! Groesse random field
  mmax = npar

  ! ... Indexgenerierung fuer Ordnung der He-Polynome   Anfang
  !     (N) Dimension: mmax (Zahl der Zufallsvariablen)
  !   (P) Polynomordnung: np

  allocate(jterme(0:np))

  call number_of_terms(nfterme, npterme, np, mmax, jterme)
  
  ! ... Indexgenerierung fuer Gausquadratur

  nall = (np+1)**mmax

  if (rank==0) write(6,*)'>>> nall = ', nall, mmax, nfterme, npterme

  np64   = np
  mmax64 = mmax
  nall64 = (np64+1)**mmax64
  
  !write(6,*)'>>> nall64 = ', nall64
  !write(6,*)

  iflag_64 = 0
  !iflag_64 = 1
  
  if (nall64.ne.nall) then
    iflag_64 = 1
    write(6,*)'>>> doppelter Integerbereich, iflag_64 = ', iflag_64
    write(6,*)'>>> ungetestet -> stop'
    write(6,*)'>>> weniger Terme!'
    stop
  endif
  
  ! ... local sizes
  ibeg = (rank*nall)/nbprocs + 1
  iend = ((rank+1)*nall)/nbprocs
  nloc = iend - ibeg + 1 
  
  allocate(aknorm(npterme))

  ! ... Dimensionierung: s_i  : mmax
  !                      s_ij : (mmax ueber 2)
  !                      s_ijk: (mmax ueber 3)   bis (mmax ueber mmax)
  !                             --------------------------------------
  !                              Summe = 2^mmax - 1 
 
  nsk = 2**mmax-1
  allocate(sk(nsk))

  allocate(akt(npterme, ntar))
  allocate(akt_glo(npterme, ntar))

  allocate(ihe(npterme, mmax))
  ihe = 0

  allocate(ifunchelp(mmax))
  allocate(ifunc(nloc, mmax))
  ifunc = 0

  call index_gen_nall(npterme, np, mmax, nloc, ibeg, ifunc, ihe)

  ! ... Indexgenerierung   Ende
  !     Zur Berechnung der ak benoetigte Abszissen und Gewichte
  call gauherprob(xi,w,np+1)
  call polnorm64(hnorm64,np)

  do l= 1, npterme
    sum1 = 1.d0
    do m = 1,mmax
      sum1 = sum1*hnorm64(ihe(l,m) + 1)
    enddo
    aknorm(l) = sum1
  enddo

  allocate(hpolxi(np+1, np))

  ! write(6,*)"alle Hermite-Polynome"
  do j= 1, np+1
    call polher(xi(j),hpol,np)
    hpolxi(j,:)=hpol(:)
  enddo
  
  ! ... Bestimmung der Modellantwort auf gewaehlte Kombination von Variablenbelegungen
  !  -> npterme-Moeglichkeiten
  allocate(germ(mmax))
  allocate(aspeicher(nloc, mmax))
  allocate(yfunc(nloc, ntar))
  
  do l = 1, nloc
    ifunchelp = ifunc(l,:)
    
    do m = 1, mmax
      germ(m) = xi(ifunchelp(m))
    enddo
    
    do i = 1, mmax
      aspeicher(l,i) = apar(i) + germ(i)*sigpar(i)
    enddo
  enddo
  ! ...

  ! ... CALL ETS_STANDALONE
  do i=1, nloc
    call ets2file(data_path // "ets_coreprof_in.cpo", &
                  data_path // "ets_equilibrium_in.cpo", &
                  data_path // "ets_coretransp_in.cpo", &
                  data_path // "ets_coresource_in.cpo", &
                  data_path // "ets_coreimpur_in.cpo", &
                  data_path // "ets_toroidfield_in.cpo", &
                  tau, &
                  npar, &
                  aspeicher(i,:), &
                  yfunc(i,:))
  enddo
  ! ...

  ! ...
  do l = 1, nloc
    ! weights
    sum2 = 1.d0
    do m = 1, mmax
      sum2 = sum2*w(ifunc(l,m))
    enddo
    
    ! Funktion * Gewicht
    !yfunc(l,:)=yfunc(l,:)*sum2
    yfunc(l,:)=log(yfunc(l,:))*sum2
  enddo
  
  ! ... akt contain
  akt     = 0.d0
  akt_glo = 0.d0
  do k=1,npterme
    do j = 1, nloc
      sum2 = 1.d0
      do m = 1, mmax
        ! welcher xi-Wert, Ordnung
        sum2 = sum2*hpolxi(ifunc(j,m), ihe(k,m)+1)
      enddo
      akt(k,:) = akt(k,:) + yfunc(j,:)*sum2
    enddo
  enddo

  ! ... 
!#ifdef MPI
  call MPI_ALLREDUCE(akt, akt_glo, ntar*npterme, MPI_DOUBLE_PRECISION, MPI_SUM, comm, ierr)
  t2 = MPI_WTIME()
!#else
!  akt_glo = akt
!#endif

!  write (str_rank,'(I0)')  rank 
!  open(5,file='standalone/logs/p_value_'//Trim(str_rank)//'.dat',status='unknown')
!  do k = 1, ntar
!    write(5,*) k, akt(1,k), akt_glo(1,k)
!  enddo
!  close(4)
  
  ! ... Results:
  ! ============ 
!  do k=1,ntar
!    call pictures(k, gstr)
!    nstr='logs/akoef' // gstr // '.dat'
!    
!    open(4,file=nstr,status='unknown')
!      l=0
!      do i=0, np-1
!        do j=1,jterme(i)
!          l=l+1
!          write(4,*)i,j,akt(l,k)/aknorm(l)
!        enddo
!        write(4,*)
!      enddo
!      close(4)
!   enddo
  
  if (rank == 0) then
    print*, '>>> Elapsed time= ',  t2 - t1, npterme,nfterme, npar, mmax
    
    open(4,file='standalone/logs/te_values_tr.dat',status='unknown')
    !open(5,file='standalone/logs/ak_values_tr.dat',status='unknown')
    
    do k = 1, ntar
      write(5,*)k, akt_glo(1,k)
      
      sum1 = 0.d0
      do i = 2, npterme
        sum1 = sum1 + akt_glo(i,k)*akt_glo(i,k)/aknorm(i)
        !write(5,*) k, akt_glo(i,k)
      enddo
      ! ...  Expectation value and  Standard deviation
      write(4,*)exp(akt_glo(1,k)), exp(akt_glo(1,k))*sqrt(sum1)
    enddo
    close(4)
    !close(5)
  endif
  ! ...

  deallocate(aspeicher)
  deallocate(apar)
  deallocate(sigpar)
  deallocate(hpol)
  deallocate(jterme)
  deallocate(aknorm)
  deallocate(akt)
  deallocate(akt_glo)
  deallocate(yfunc)
  deallocate(ifunc)
  deallocate(ihe)
  deallocate(hnorm64)
  deallocate(hpolxi)
  deallocate(germ)
  
  ! TODO delete
  9990 format(20e16.8)
  9980 format(i4,1x,i3,1x,20i2)
  9974 format(4i3,4e12.4)
  9972 format(2i3,4e12.4)
  9970 format(i4,3i3,4e12.4)
  9960 format(5i3,4e12.4)
  9950 format(10i3)
  9940 format(100e12.4)
  
  ! ...
!#ifdef MPI
  call MPI_FINALIZE(ierr)
!#endif
  
end program ets_test
!...............................................................................
