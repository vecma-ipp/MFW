program imp4dv_M3
  use imp4dv_standalone
  use ymmsl
  use libmuscle
  implicit none

  ! MUSCLE3 specific
  type(LIBMUSCLE_PortsDescription) :: ports
  type(LIBMUSCLE_Instance) :: instance

  type(LIBMUSCLE_Message) :: rmsg
  type(LIBMUSCLE_DataConstRef) :: rdata, item

  type(LIBMUSCLE_Message) :: smsg
  type(LIBMUSCLE_Data) :: sdata

  integer :: err_code
  character(:), allocatable :: err_msg

  real (selected_real_kind(15)) :: t_cur !, t_max, dt, k

  ! code specific
  character(kind=c_char), pointer :: coreprof_in_buf(:)
  character(kind=c_char), pointer :: equilibrium_in_buf(:)
  character(kind=c_char), pointer :: coretransp_in_buf(:)
  character(kind=c_char), pointer :: coretransp_out_buf(:)

  ports = LIBMUSCLE_PortsDescription_create()
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_F_INIT, 'equilibrium_in')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_F_INIT, 'coreprof_in')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_F_INIT, 'coretransp_in')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_O_F, 'coretransp_out')
  instance = LIBMUSCLE_Instance_create(ports)
  call LIBMUSCLE_PortsDescription_free(ports)
  
  ! main loop
  do while (LIBMUSCLE_Instance_reuse_instance(instance))

     ! receive equilibrium data
     rmsg = LIBMUSCLE_Instance_receive(instance, 'equilibrium_in')
     rdata = LIBMUSCLE_Message_get_data(rmsg)
     if (.not. LIBMUSCLE_DataConstRef_is_a_byte_array(rdata)) STOP 'wrong data type received'
     allocate (equilibrium_in_buf(LIBMUSCLE_DataConstRef_size(rdata)))
     call LIBMUSCLE_DataConstRef_as_byte_array(rdata, equilibrium_in_buf, err_code, err_msg)
     call LIBMUSCLE_DataConstRef_free(rdata)
     call LIBMUSCLE_Message_free(rmsg)

     ! receive coreprof data
     rmsg = LIBMUSCLE_Instance_receive(instance, 'coreprof_in')
     rdata = LIBMUSCLE_Message_get_data(rmsg)
     if (.not. LIBMUSCLE_DataConstRef_is_a_byte_array(rdata)) STOP 'wrong data type received'
     allocate (coreprof_in_buf(LIBMUSCLE_DataConstRef_size(rdata)))
     call LIBMUSCLE_DataConstRef_as_byte_array(rdata, coreprof_in_buf, err_code, err_msg)
     call LIBMUSCLE_DataConstRef_free(rdata)
     t_cur = LIBMUSCLE_Message_timestamp(rmsg)
     !t_max = LIBMUSCLE_Message_timestamp(rmsg) + t_max
     call LIBMUSCLE_Message_free(rmsg)

     ! receive coreprof data
     rmsg = LIBMUSCLE_Instance_receive(instance, 'coretransp_in')
     rdata = LIBMUSCLE_Message_get_data(rmsg)
     if (.not. LIBMUSCLE_DataConstRef_is_a_byte_array(rdata)) STOP 'wrong data type received'
     allocate (coretransp_in_buf(LIBMUSCLE_DataConstRef_size(rdata)))
     call LIBMUSCLE_DataConstRef_as_byte_array(rdata, coretransp_in_buf, err_code, err_msg)
     call LIBMUSCLE_DataConstRef_free(rdata)
     t_cur = LIBMUSCLE_Message_timestamp(rmsg)
     !t_max = LIBMUSCLE_Message_timestamp(rmsg) + t_max
     call LIBMUSCLE_Message_free(rmsg)

     call imp4dv2buf( &
          equilibrium_in_buf, &
          coreprof_in_buf, &
          coretransp_in_buf, &
          coretransp_out_buf)

     sdata = LIBMUSCLE_Data_create_byte_array(coretransp_out_buf)
     smsg = LIBMUSCLE_Message_create(t_cur, sdata)
     call LIBMUSCLE_Instance_send(instance, 'coretransp_out', smsg)
     call LIBMUSCLE_Message_free(smsg)
     call LIBMUSCLE_Data_free(sdata)
     
     deallocate(equilibrium_in_buf)
     deallocate(coreprof_in_buf)
     deallocate(coretransp_in_buf)
     deallocate(coretransp_out_buf)

  end do
  
  call LIBMUSCLE_Instance_free(instance)
  
end program imp4dv_M3
