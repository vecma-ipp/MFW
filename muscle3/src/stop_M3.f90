program stop_M3
  use euitm_schemas, only: type_coreprof
  use read_structures
  use deallocate_structures
  use c_tools
  use ymmsl
  use libmuscle
  implicit none

  ! MUSCLE3 specific
  type(LIBMUSCLE_PortsDescription) :: ports
  type(LIBMUSCLE_Instance) :: instance

  type(LIBMUSCLE_Message) :: rmsg
  type(LIBMUSCLE_DataConstRef) :: rdata, item

  real (selected_real_kind(15)) :: t_current

  character(256) :: final_cpos

  ! code specific
  character(kind=c_char), pointer :: equilibrium_in_buf(:)
  character(kind=c_char), pointer :: coreprof_in_buf(:)
  character(kind=c_char), pointer :: coresource_in_buf(:)
  character(kind=c_char), pointer :: coreimpur_in_buf(:)
  character(kind=c_char), pointer :: toroidfield_in_buf(:)

  type(type_coreprof), pointer :: coreprof(:)
  
  ports = LIBMUSCLE_PortsDescription_create()
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_F_INIT, 'equilibrium_in')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_F_INIT, 'coreprof_in')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_F_INIT, 'coresource_in')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_F_INIT, 'coreimpur_in')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_F_INIT, 'toroidfield_in')
  instance = LIBMUSCLE_Instance_create(ports)
  call LIBMUSCLE_PortsDescription_free(ports)
  
  ! main loop
  do while (LIBMUSCLE_Instance_reuse_instance(instance))

     ! get params
     final_cpos = LIBMUSCLE_Instance_get_setting_as_character(instance, 'final_cpo_dir')

     ! recv and write final CPO to file into specified dir
     ! equilibrium
     rmsg = LIBMUSCLE_Instance_receive(instance, 'equilibrium_in')
     rdata = LIBMUSCLE_Message_get_data(rmsg)
     allocate (equilibrium_in_buf(LIBMUSCLE_DataConstRef_size(rdata)))
     call LIBMUSCLE_DataConstRef_as_byte_array(rdata, equilibrium_in_buf)
     call LIBMUSCLE_DataConstRef_free(rdata)
     call LIBMUSCLE_Message_free(rmsg)
     call byte2file(trim(final_cpos)//'/ets_equilibrium_in.cpo', equilibrium_in_buf, size(equilibrium_in_buf))
     deallocate(equilibrium_in_buf)
     ! coreprof
     rmsg = LIBMUSCLE_Instance_receive(instance, 'coreprof_in')
     rdata = LIBMUSCLE_Message_get_data(rmsg)
     allocate (coreprof_in_buf(LIBMUSCLE_DataConstRef_size(rdata)))
     call LIBMUSCLE_DataConstRef_as_byte_array(rdata, coreprof_in_buf)
     call LIBMUSCLE_DataConstRef_free(rdata)
     !t_init = LIBMUSCLE_Message_timestamp(rmsg)
     call LIBMUSCLE_Message_free(rmsg)
     call byte2file(trim(final_cpos)//'/ets_coreprof_in.cpo', coreprof_in_buf, size(coreprof_in_buf))
     deallocate(coreprof_in_buf)
     ! recv init coresource
     rmsg = LIBMUSCLE_Instance_receive(instance, 'coresource_in')
     rdata = LIBMUSCLE_Message_get_data(rmsg)
     allocate (coresource_in_buf(LIBMUSCLE_DataConstRef_size(rdata)))
     call LIBMUSCLE_DataConstRef_as_byte_array(rdata, coresource_in_buf)
     call LIBMUSCLE_DataConstRef_free(rdata)
     call LIBMUSCLE_Message_free(rmsg)
     call byte2file(trim(final_cpos)//'/ets_coresource_in.cpo', coresource_in_buf, size(coresource_in_buf))
     deallocate(coresource_in_buf)
     ! recv init coreimpur
     rmsg = LIBMUSCLE_Instance_receive(instance, 'coreimpur_in')
     rdata = LIBMUSCLE_Message_get_data(rmsg)
     allocate (coreimpur_in_buf(LIBMUSCLE_DataConstRef_size(rdata)))
     call LIBMUSCLE_DataConstRef_as_byte_array(rdata, coreimpur_in_buf)
     call LIBMUSCLE_DataConstRef_free(rdata)
     call LIBMUSCLE_Message_free(rmsg)
     call byte2file(trim(final_cpos)//'/ets_coreimpur_in.cpo', coreimpur_in_buf, size(coreimpur_in_buf))
     deallocate(coreimpur_in_buf)
     ! recv init toroidfield
     rmsg = LIBMUSCLE_Instance_receive(instance, 'toroidfield_in')
     rdata = LIBMUSCLE_Message_get_data(rmsg)
     allocate (toroidfield_in_buf(LIBMUSCLE_DataConstRef_size(rdata)))
     call LIBMUSCLE_DataConstRef_as_byte_array(rdata, toroidfield_in_buf)
     call LIBMUSCLE_DataConstRef_free(rdata)
     call LIBMUSCLE_Message_free(rmsg)
     call byte2file(trim(final_cpos)//'/ets_toroidfield_in.cpo', toroidfield_in_buf, size(toroidfield_in_buf))
     deallocate(toroidfield_in_buf)

  end do
  
  call LIBMUSCLE_Instance_free(instance)

end program stop_M3
