program init_M3
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

  type(LIBMUSCLE_Message) :: smsg
  type(LIBMUSCLE_Data) :: sdata

  integer :: err_code
  character(:), allocatable :: err_msg

  real (selected_real_kind(15)) :: t_cur 

  character(256) :: init_cpos

  ! code specific
  character(kind=c_char), pointer :: equilibrium_init_buf(:)
  character(kind=c_char), pointer :: coreprof_init_buf(:)
  character(kind=c_char), pointer :: coresource_init_buf(:)
  character(kind=c_char), pointer :: coreimpur_init_buf(:)
  character(kind=c_char), pointer :: toroidfield_init_buf(:)

  character(kind=c_char), pointer :: tmpbuf(:)
  integer :: tmpsize, ios

  type(type_coreprof), pointer :: coreprof(:)
  
  ports = LIBMUSCLE_PortsDescription_create()
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_O_F, 'equilibrium_init')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_O_F, 'coreprof_init')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_O_F, 'coresource_init')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_O_F, 'coreimpur_init')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_O_F, 'toroidfield_init')
  instance = LIBMUSCLE_Instance_create(ports)
  call LIBMUSCLE_PortsDescription_free(ports)
  
  ! main loop
  do while (LIBMUSCLE_Instance_reuse_instance(instance))

     ! get params
     init_cpos = LIBMUSCLE_Instance_get_setting_as_character(instance, 'init_cpo_dir')

     ! read init CPO files from specified dir
     call file2byte(trim(init_cpos)//"/ets_equilibrium_in.cpo", &
          tmpbuf, tmpsize)
     allocate(equilibrium_init_buf(tmpsize))
     equilibrium_init_buf(1:tmpsize) = tmpbuf(1:tmpsize)
     call dealloc_cbytebuf(tmpbuf)

     call file2byte(trim(init_cpos)//"/ets_coreprof_in.cpo", &
          tmpbuf, tmpsize)
     allocate(coreprof_init_buf(tmpsize))
     coreprof_init_buf(1:tmpsize) = tmpbuf(1:tmpsize)
     call dealloc_cbytebuf(tmpbuf)
  
     allocate(coreprof(1))
     ! Read coreprof CPO to get main time   
     open (unit = 10, file = trim(init_cpos)//"/ets_coreprof_in.cpo", &
          status = 'old', form = 'formatted', &
          action = 'read', iostat = ios)
     if (ios == 0) then
        close (10)
        call open_read_file(10, trim(init_cpos)//"/ets_coreprof_in.cpo")
        call read_cpo(coreprof(1), 'coreprof')
        call close_read_file
     else
        print *,"ERROR. CPO file not found:", &
             trim(init_cpos)//"/ets_coreprof_in.cpo"
        STOP
     end if
     t_cur = coreprof(1)%time
     call deallocate_cpo(coreprof)

     call file2byte(trim(init_cpos)//"/ets_coresource_in.cpo", &
          tmpbuf, tmpsize)
     allocate(coresource_init_buf(tmpsize))
     coresource_init_buf(1:tmpsize) = tmpbuf(1:tmpsize)
     call dealloc_cbytebuf(tmpbuf)

     call file2byte(trim(init_cpos)//"/ets_coreimpur_in.cpo", &
          tmpbuf, tmpsize)
     allocate(coreimpur_init_buf(tmpsize))
     coreimpur_init_buf(1:tmpsize) = tmpbuf(1:tmpsize)
     call dealloc_cbytebuf(tmpbuf)

     call file2byte(trim(init_cpos)//"/ets_toroidfield_in.cpo", &
          tmpbuf, tmpsize)
     allocate(toroidfield_init_buf(tmpsize))
     toroidfield_init_buf(1:tmpsize) = tmpbuf(1:tmpsize)
     call dealloc_cbytebuf(tmpbuf)

     ! send data
     sdata = LIBMUSCLE_Data_create_byte_array(equilibrium_init_buf)
     smsg = LIBMUSCLE_Message_create(t_cur, sdata)
     call LIBMUSCLE_Instance_send(instance, 'equilibrium_init', smsg)
     call LIBMUSCLE_Message_free(smsg)
     call LIBMUSCLE_Data_free(sdata)

     sdata = LIBMUSCLE_Data_create_byte_array(coreprof_init_buf)
     smsg = LIBMUSCLE_Message_create(t_cur, sdata)
     call LIBMUSCLE_Instance_send(instance, 'coreprof_init', smsg)
     call LIBMUSCLE_Message_free(smsg)
     call LIBMUSCLE_Data_free(sdata)

     sdata = LIBMUSCLE_Data_create_byte_array(coresource_init_buf)
     smsg = LIBMUSCLE_Message_create(t_cur, sdata)
     call LIBMUSCLE_Instance_send(instance, 'coresource_init', smsg)
     call LIBMUSCLE_Message_free(smsg)
     call LIBMUSCLE_Data_free(sdata)

     sdata = LIBMUSCLE_Data_create_byte_array(coreimpur_init_buf)
     smsg = LIBMUSCLE_Message_create(t_cur, sdata)
     call LIBMUSCLE_Instance_send(instance, 'coreimpur_init', smsg)
     call LIBMUSCLE_Message_free(smsg)
     call LIBMUSCLE_Data_free(sdata)
     
     sdata = LIBMUSCLE_Data_create_byte_array(toroidfield_init_buf)
     smsg = LIBMUSCLE_Message_create(t_cur, sdata)
     call LIBMUSCLE_Instance_send(instance, 'toroidfield_init', smsg)
     call LIBMUSCLE_Message_free(smsg)
     call LIBMUSCLE_Data_free(sdata)

     deallocate(equilibrium_init_buf)
     deallocate(coreprof_init_buf)
     deallocate(coresource_init_buf)
     deallocate(coreimpur_init_buf)
     deallocate(toroidfield_init_buf)

  end do
  
  call LIBMUSCLE_Instance_free(instance)

end program init_M3
