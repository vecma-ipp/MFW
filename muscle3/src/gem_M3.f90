program gem_M3
  use gem_standalone
  use imp4dv_standalone
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
  character(kind=c_char), pointer :: coretransp_flux_buf(:)
  character(kind=c_char), pointer :: coretransp_out_buf(:)

  call MPI_Init(ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, npes, ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, irank, ierr)

  ports = LIBMUSCLE_PortsDescription_create()
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_F_INIT, 'equilibrium_in')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_F_INIT, 'coreprof_in')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_O_F, 'coretransp_out')
  instance = LIBMUSCLE_Instance_create(ports, MPI_COMM_WORLD, root_rank)
  call LIBMUSCLE_PortsDescription_free(ports)
  
  ! main loop
  do while (LIBMUSCLE_Instance_reuse_instance(instance))

     ! receive equilibrium data
     rmsg = LIBMUSCLE_Instance_receive(instance, 'equilibrium_in')
     if (irank == root_rank) then
        rdata = LIBMUSCLE_Message_get_data(rmsg)
        equil_buf_size = LIBMUSCLE_DataConstRef_size(rdata)
        allocate (equilibrium_in_buf(equil_buf_size))
        call LIBMUSCLE_DataConstRef_as_byte_array(rdata, equilibrium_in_buf)
        call LIBMUSCLE_DataConstRef_free(rdata)
        call LIBMUSCLE_Message_free(rmsg)
     end if

     call MPI_Bcast(equil_buf_size, 1, MPI_INT, root_rank, MPI_COMM_WORLD, ierr)

     if (irank /= root_rank) then
        allocate (equilibrium_in_buf(equil_buf_size))
     end if

     call MPI_Bcast(equilibrium_in_buf, equil_buf_size, MPI_BYTE, root_rank, MPI_COMM_WORLD, ierr)

     ! receive coreprof data
     rmsg = LIBMUSCLE_Instance_receive(instance, 'coreprof_in')
     if (irank == root_rank) then
        rdata = LIBMUSCLE_Message_get_data(rmsg)
        coreprof_buf_size = LIBMUSCLE_DataConstRef_size(rdata)
        allocate (coreprof_in_buf(coreprof_buf_size))
        call LIBMUSCLE_DataConstRef_as_byte_array(rdata, coreprof_in_buf)
        call LIBMUSCLE_DataConstRef_free(rdata)
        t_cur = LIBMUSCLE_Message_timestamp(rmsg)
        call LIBMUSCLE_Message_free(rmsg)
     end if

     call MPI_Bcast(coreprof_buf_size, 1, MPI_INT, root_rank, MPI_COMM_WORLD, ierr)

     if (irank /= root_rank) then
        allocate (coreprof_in_buf(coreprof_buf_size))
     end if

     call MPI_Bcast(coreprof_in_buf, coreprof_buf_size, MPI_BYTE, root_rank, MPI_COMM_WORLD, ierr)

     call MPI_Barrier(MPI_COMM_WORLD, ierr)

     call gem2buf( &
          equilibrium_in_buf, &
          coreprof_in_buf, &
          coretransp_flux_buf)

     print *,"calling imp4dv"

     call imp4dv2buf( &
          equilibrium_in_buf, &
          coreprof_in_buf, &
          coretransp_flux_buf, &
          coretransp_out_buf)

     sdata = LIBMUSCLE_Data_create_byte_array(coretransp_out_buf)
     smsg = LIBMUSCLE_Message_create(t_cur, sdata)
     call LIBMUSCLE_Instance_send(instance, 'coretransp_out', smsg)
     call LIBMUSCLE_Message_free(smsg)
     call LIBMUSCLE_Data_free(sdata)
     
     deallocate(equilibrium_in_buf)
     deallocate(coreprof_in_buf)
     deallocate(coretransp_flux_buf)
     deallocate(coretransp_out_buf)

  end do
  
  call LIBMUSCLE_Instance_free(instance)
  
  call MPI_Finalize(ierr)

end program gem_M3
