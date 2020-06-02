program ets_M3
  use ets_standalone
  use equilupdate_standalone
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

  real (selected_real_kind(15)) :: t_init, t_current, t_duration, dt_max

  ! code specific
  character(kind=c_char), pointer :: coreprof_in_buf(:)
  character(kind=c_char), pointer :: equilibrium_in_buf(:)
  character(kind=c_char), pointer :: coretransp_in_buf(:)
  character(kind=c_char), pointer :: coresource_in_buf(:)
  character(kind=c_char), pointer :: coreimpur_in_buf(:)
  character(kind=c_char), pointer :: toroidfield_in_buf(:)

  character(kind=c_char), pointer :: coreprof_out_buf(:)
  character(kind=c_char), pointer :: equilibrium_out_buf(:)

  ports = LIBMUSCLE_PortsDescription_create()
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_F_INIT, 'coreprof_init')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_F_INIT, 'equilibrium_init')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_F_INIT, 'coresource_init')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_F_INIT, 'coreimpur_init')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_F_INIT, 'toroidfield_init')

  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_S, 'equilibrium_in')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_S, 'coretransp_in')

  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_O_I, 'coreprof_out')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_O_I, 'equilibrium_out')

  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_O_F, 'coreprof_final')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_O_F, 'equilibrium_final')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_O_F, 'coresource_final')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_O_F, 'coreimpur_final')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_O_F, 'toroidfield_final')

  instance = LIBMUSCLE_Instance_create(ports)
  call LIBMUSCLE_PortsDescription_free(ports)
  
  ! main loop
  do while (LIBMUSCLE_Instance_reuse_instance(instance))

     !###  INIT (F_INIT)  ########################!

     ! get params
     dt_max = LIBMUSCLE_Instance_get_setting_as_real8(instance, 'dt')
     t_duration = LIBMUSCLE_Instance_get_setting_as_real8(instance, 'duration')
     ! recv init equilibrium
     rmsg = LIBMUSCLE_Instance_receive(instance, 'equilibrium_init')
     rdata = LIBMUSCLE_Message_get_data(rmsg)
     if (.not. LIBMUSCLE_DataConstRef_is_a_byte_array(rdata)) STOP 'wrong data type received'
     allocate (equilibrium_in_buf(LIBMUSCLE_DataConstRef_size(rdata)))
     call LIBMUSCLE_DataConstRef_as_byte_array(rdata, equilibrium_in_buf, err_code, err_msg)
     call LIBMUSCLE_DataConstRef_free(rdata)
     call LIBMUSCLE_Message_free(rmsg)
     ! recv init coreprof
     rmsg = LIBMUSCLE_Instance_receive(instance, 'coreprof_init')
     rdata = LIBMUSCLE_Message_get_data(rmsg)
     if (.not. LIBMUSCLE_DataConstRef_is_a_byte_array(rdata)) STOP 'wrong data type received'
     allocate (coreprof_in_buf(LIBMUSCLE_DataConstRef_size(rdata)))
     call LIBMUSCLE_DataConstRef_as_byte_array(rdata, coreprof_in_buf, err_code, err_msg)
     call LIBMUSCLE_DataConstRef_free(rdata)
     t_init = LIBMUSCLE_Message_timestamp(rmsg)
     call LIBMUSCLE_Message_free(rmsg)
     ! recv init coresource
     rmsg = LIBMUSCLE_Instance_receive(instance, 'coresource_init')
     rdata = LIBMUSCLE_Message_get_data(rmsg)
     if (.not. LIBMUSCLE_DataConstRef_is_a_byte_array(rdata)) STOP 'wrong data type received'
     allocate (coresource_in_buf(LIBMUSCLE_DataConstRef_size(rdata)))
     call LIBMUSCLE_DataConstRef_as_byte_array(rdata, coresource_in_buf, err_code, err_msg)
     call LIBMUSCLE_DataConstRef_free(rdata)
     call LIBMUSCLE_Message_free(rmsg)
     ! recv init coreimpur
     rmsg = LIBMUSCLE_Instance_receive(instance, 'coreimpur_init')
     rdata = LIBMUSCLE_Message_get_data(rmsg)
     if (.not. LIBMUSCLE_DataConstRef_is_a_byte_array(rdata)) STOP 'wrong data type received'
     allocate (coreimpur_in_buf(LIBMUSCLE_DataConstRef_size(rdata)))
     call LIBMUSCLE_DataConstRef_as_byte_array(rdata, coreimpur_in_buf, err_code, err_msg)
     call LIBMUSCLE_DataConstRef_free(rdata)
     call LIBMUSCLE_Message_free(rmsg)
     ! recv init toroidfield
     rmsg = LIBMUSCLE_Instance_receive(instance, 'toroidfield_init')
     rdata = LIBMUSCLE_Message_get_data(rmsg)
     if (.not. LIBMUSCLE_DataConstRef_is_a_byte_array(rdata)) STOP 'wrong data type received'
     allocate (toroidfield_in_buf(LIBMUSCLE_DataConstRef_size(rdata)))
     call LIBMUSCLE_DataConstRef_as_byte_array(rdata, toroidfield_in_buf, err_code, err_msg)
     call LIBMUSCLE_DataConstRef_free(rdata)
     call LIBMUSCLE_Message_free(rmsg)

     allocate(coreprof_out_buf, source=coreprof_in_buf)
     allocate(equilibrium_out_buf, source=equilibrium_in_buf)

     t_current = t_init
     !###  TIME LOOP  ############################!
     do while (t_current+dt_max .lt. t_init+t_duration)
        
        !###  O_I  ###############################!
        ! send coreprof
        sdata = LIBMUSCLE_Data_create_byte_array(coreprof_out_buf)
        smsg = LIBMUSCLE_Message_create(t_current, sdata)
        call LIBMUSCLE_Instance_send(instance, 'coreprof_out', smsg)
        call LIBMUSCLE_Message_free(smsg)
        call LIBMUSCLE_Data_free(sdata)
        ! send equilibrium
        sdata = LIBMUSCLE_Data_create_byte_array(equilibrium_out_buf)
        smsg = LIBMUSCLE_Message_create(t_current, sdata)
        call LIBMUSCLE_Instance_send(instance, 'equilibrium_out', smsg)
        call LIBMUSCLE_Message_free(smsg)
        call LIBMUSCLE_Data_free(sdata)
             
        deallocate(equilibrium_out_buf)
        deallocate(coreprof_out_buf)


        !###  S  #################################!
        deallocate(equilibrium_in_buf)
        deallocate(coretransp_in_buf)
        ! recv equilibrium
        rmsg = LIBMUSCLE_Instance_receive(instance, 'equilibrium_in')
        rdata = LIBMUSCLE_Message_get_data(rmsg)
        if (.not. LIBMUSCLE_DataConstRef_is_a_byte_array(rdata)) STOP 'wrong data type received'
        allocate (equilibrium_in_buf(LIBMUSCLE_DataConstRef_size(rdata)))
        call LIBMUSCLE_DataConstRef_as_byte_array(rdata, equilibrium_in_buf, err_code, err_msg)
        call LIBMUSCLE_DataConstRef_free(rdata)
        call LIBMUSCLE_Message_free(rmsg)
        ! recv coretransp
        rmsg = LIBMUSCLE_Instance_receive(instance, 'coretransp_in')
        rdata = LIBMUSCLE_Message_get_data(rmsg)
        if (.not. LIBMUSCLE_DataConstRef_is_a_byte_array(rdata)) STOP 'wrong data type received'
        allocate (coretransp_in_buf(LIBMUSCLE_DataConstRef_size(rdata)))
        call LIBMUSCLE_DataConstRef_as_byte_array(rdata, coretransp_in_buf, err_code, err_msg)
        call LIBMUSCLE_DataConstRef_free(rdata)
        call LIBMUSCLE_Message_free(rmsg)

        call ets2buf( &
             coreprof_in_buf, &
             equilibrium_in_buf, &
             coretransp_in_buf, &
             coresource_in_buf, &
             coreimpur_in_buf, &
             coreprof_out_buf, &
             t_current)

        deallocate(coreprof_in_buf)
        allocate(coreprof_in_buf, source=coreprof_out_buf)

        call equilupdate2buf( &
             coreprof_in_buf, &
             toroidfield_in_buf, &
             equilibrium_in_buf, &
             equilibrium_out_buf)

     end do


     !###  O_F  #################################!
     ! send equilibrium
     sdata = LIBMUSCLE_Data_create_byte_array(equilibrium_out_buf)
     smsg = LIBMUSCLE_Message_create(t_current, sdata)
     call LIBMUSCLE_Instance_send(instance, 'equilibrium_final', smsg)
     call LIBMUSCLE_Message_free(smsg)
     call LIBMUSCLE_Data_free(sdata)
     ! send coreprof
     sdata = LIBMUSCLE_Data_create_byte_array(coreprof_out_buf)
     smsg = LIBMUSCLE_Message_create(t_current, sdata)
     call LIBMUSCLE_Instance_send(instance, 'coreprof_final', smsg)
     call LIBMUSCLE_Message_free(smsg)
     call LIBMUSCLE_Data_free(sdata)
     ! send coresource
     sdata = LIBMUSCLE_Data_create_byte_array(coresource_in_buf)
     smsg = LIBMUSCLE_Message_create(t_current, sdata)
     call LIBMUSCLE_Instance_send(instance, 'coresource_final', smsg)
     call LIBMUSCLE_Message_free(smsg)
     call LIBMUSCLE_Data_free(sdata)
     ! send coreimpur
     sdata = LIBMUSCLE_Data_create_byte_array(coreimpur_in_buf)
     smsg = LIBMUSCLE_Message_create(t_current, sdata)
     call LIBMUSCLE_Instance_send(instance, 'coreimpur_final', smsg)
     call LIBMUSCLE_Message_free(smsg)
     call LIBMUSCLE_Data_free(sdata)
     ! send toroidfield
     sdata = LIBMUSCLE_Data_create_byte_array(toroidfield_in_buf)
     smsg = LIBMUSCLE_Message_create(t_current, sdata)
     call LIBMUSCLE_Instance_send(instance, 'toroidfield_final', smsg)
     call LIBMUSCLE_Message_free(smsg)
     call LIBMUSCLE_Data_free(sdata)

     deallocate(equilibrium_out_buf)
     deallocate(coreprof_out_buf)
     deallocate(coresource_in_buf)
     deallocate(coreimpur_in_buf)
     deallocate(toroidfield_in_buf)
        
  end do
  
  call LIBMUSCLE_Instance_free(instance)

end program ets_M3
