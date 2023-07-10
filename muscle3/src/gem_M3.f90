program gem_M3
  use gem_standalone
  use ymmsl
  use libmuscle_mpi
  use mpi
  implicit none

  ! MUSCLE3 specific
  type(LIBMUSCLE_PortsDescription) :: ports
  type(LIBMUSCLE_Instance) :: instance

  type(LIBMUSCLE_Message) :: rmsg
  type(LIBMUSCLE_DataConstRef) :: rdata, item

  type(LIBMUSCLE_Message) :: smsg
  type(LIBMUSCLE_Data) :: sdata

  integer, parameter :: root_rank = 0
  integer :: err_code, ierr, npes, irank, ios
  character(:), allocatable :: err_msg
  integer :: equil_buf_size, coreprof_buf_size

  real (selected_real_kind(15)) :: t_cur !, t_max, dt, k

  ! code specific
  character(kind=c_char), pointer :: coreprof_in_buf(:)
  character(kind=c_char), pointer :: equilibrium_in_buf(:)
  character(kind=c_char), pointer :: coretransp_out_buf(:)

  call MPI_Init(ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, npes, ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, irank, ierr)

  if (irank == root_rank) then
    call system('cp ../../../../gem.xml gem.xml')
    call system('cp ../../../../gem.xsd gem.xsd')
  end if

  ports = LIBMUSCLE_PortsDescription_create()
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_F_INIT, 'equilibrium_in')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_F_INIT, 'coreprof_in')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_O_F, 'coretransp_out')
  instance = LIBMUSCLE_Instance_create(ports, MPI_COMM_WORLD, root_rank)
  call LIBMUSCLE_PortsDescription_free(ports)
  
  print *, "before entering the run iteration loop" !!!DEBUG
  !print *, "npes= ", npes  !!!DEBUG
  !print *, "irank= ", irank !!!DEBUG

  ! main loop
  do while (LIBMUSCLE_Instance_reuse_instance(instance))

     print *, "starting a new iteration"

     ! receive equilibrium data
     rmsg = LIBMUSCLE_Instance_receive(instance, 'equilibrium_in')
     if (irank == root_rank) then
        print *, "from rank=", irank, " reading equilibrium_in" !!!DEBUG
        rdata = LIBMUSCLE_Message_get_data(rmsg)
        equil_buf_size = LIBMUSCLE_DataConstRef_size(rdata)
        allocate (equilibrium_in_buf(equil_buf_size))
        call LIBMUSCLE_DataConstRef_as_byte_array(rdata, equilibrium_in_buf)
        call LIBMUSCLE_DataConstRef_free(rdata)
        call LIBMUSCLE_Message_free(rmsg)
     end if

     print *, "received equilibrium_in, now broadcasting" !!!DEBUG
     call MPI_Bcast(equil_buf_size, 1, MPI_INT, root_rank, MPI_COMM_WORLD, ierr)

     if (irank /= root_rank) then
        allocate (equilibrium_in_buf(equil_buf_size))
     end if

     call MPI_Bcast(equilibrium_in_buf, equil_buf_size, MPI_BYTE, root_rank, MPI_COMM_WORLD, ierr)
     
     print *, "broadcasted equilibrium_in, now receiving coreprof_in" !!!DEBUG
     print *, "equilibrium_in_buf associated: ", associated(equilibrium_in_buf) !!!DEBUG
     print *, "first ", 64, " symbols of equilibrium_in: ", equilibrium_in_buf(1:64) !!!DEBUG 

     ! receive coreprof data
     rmsg = LIBMUSCLE_Instance_receive(instance, 'coreprof_in')
     if (irank == root_rank) then
        print *, "from rank=", irank, " reading coreprof_in" !!!DEBUG
        rdata = LIBMUSCLE_Message_get_data(rmsg)
        coreprof_buf_size = LIBMUSCLE_DataConstRef_size(rdata)
        allocate (coreprof_in_buf(coreprof_buf_size))
        call LIBMUSCLE_DataConstRef_as_byte_array(rdata, coreprof_in_buf)
        call LIBMUSCLE_DataConstRef_free(rdata)
        t_cur = LIBMUSCLE_Message_timestamp(rmsg)
        call LIBMUSCLE_Message_free(rmsg)
     end if

     print *, "received coreprof_in, now broadcasting" !!!DEBUG
     call MPI_Bcast(coreprof_buf_size, 1, MPI_INT, root_rank, MPI_COMM_WORLD, ierr)

     if (irank /= root_rank) then
        allocate (coreprof_in_buf(coreprof_buf_size))
     end if

     call MPI_Bcast(coreprof_in_buf, coreprof_buf_size, MPI_BYTE, root_rank, MPI_COMM_WORLD, ierr)
     print *, "broadcasted coreprof_in, now barier before the GEM call" !!!DEBUG
     print *, "coreprof_in_buf associated: ", associated(coreprof_in_buf) !!!DEBUG
     print *, "first ", 64, " symbols of coreprof_in: ", coreprof_in_buf(1:64) !!!DEBUG 

     call MPI_Barrier(MPI_COMM_WORLD, ierr)

     print *, "after barier, calling gem2buf" !!!DEBUG

     call gem2buf( &
          equilibrium_in_buf, &
          coreprof_in_buf, &
          coretransp_out_buf)

     call MPI_Barrier(MPI_COMM_WORLD, ierr)

     if (irank == root_rank) then

        print *, "from rank=", irank, " sending coretransp_out" !!!DEBUG
        print *, "coretransp_out_buf associated: ", associated(coretransp_out_buf) !!!DEBUG
        print *, "first ", 64, " symbols of coretransp_out: ", coretransp_out_buf(1:64) !!!DEBUG 
        sdata = LIBMUSCLE_Data_create_byte_array(coretransp_out_buf)
        print *, "byte array created for coretransp_out" !!! DEBUG
        smsg = LIBMUSCLE_Message_create(t_cur, sdata)
        print *, "muscle coretransp_out message created" !!! DEBUG
        call LIBMUSCLE_Instance_send(instance, 'coretransp_out', smsg)
        print *, "muscle coretransp_out message sent" !!! DEBUG
        call LIBMUSCLE_Message_free(smsg)
        print *, "muscle coretransp_out message structures destroyed" !!! DEBUG

        call LIBMUSCLE_Data_free(sdata)
        print *, "muscle coretransp_out data structures destroyed" !!! DEBUG

     end if
     
     deallocate(equilibrium_in_buf)
     deallocate(coreprof_in_buf)
     deallocate(coretransp_out_buf)

     print *, "end of muscle iteration" !!! DEBUG

  end do
  
  call LIBMUSCLE_Instance_free(instance)
  print *, "freed libmuscle instance" !!! DEBUG
  
  call MPI_Finalize(ierr)
  print *, "finilised mpi" !!! DEBUG

end program gem_M3
