program ets_M3
  use ets_standalone
  use equilupdate_standalone
  use c_tools
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

  real(kind=LIBMUSCLE_real8) :: t_init, t_current, t_duration, dt_max
  logical :: save_slice
  !logical :: adaptive_timestep
  integer(kind=LIBMUSCLE_int8) :: slice_init

  ! code specific
  character(kind=c_char), pointer :: coreprof_in_buf(:)
  character(kind=c_char), pointer :: equilibrium_in_buf(:)
  character(kind=c_char), pointer :: coretransp_in_buf(:)
  character(kind=c_char), pointer :: coresource_in_buf(:)
  character(kind=c_char), pointer :: coreimpur_in_buf(:)
  character(kind=c_char), pointer :: toroidfield_in_buf(:)

  character(kind=c_char), pointer :: coreprof_out_buf(:)
  character(kind=c_char), pointer :: equilibrium_out_buf(:)

  integer :: step
  character(5) :: stepstr

  call system('cp ../../../../ets.xml ets.xml')
  call system('cp ../../../../ets.xsd ets.xsd')

  ports = LIBMUSCLE_PortsDescription_create()
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_F_INIT, 'coreprof_init')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_F_INIT, 'equilibrium_init')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_F_INIT, 'coresource_init')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_F_INIT, 'coreimpur_init')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_F_INIT, 'toroidfield_init')

  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_S, 'equilibrium_in')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_S, 'coretransp_in')

  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_O_I, 'coreprof_out')
  !call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_O_I, 'coreprof_out_flux2tcoeff')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_O_I, 'equilibrium_out')

  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_O_F, 'coreprof_final')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_O_F, 'equilibrium_final')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_O_F, 'coresource_final')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_O_F, 'coreimpur_final')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_O_F, 'toroidfield_final')

  instance = LIBMUSCLE_Instance_create(ports)
  call LIBMUSCLE_PortsDescription_free(ports)

  coretransp_in_buf => null()
  
  !!!TODO DEBUG - this fails first now
  print *, ">before entering the loop" !!!DEBUG
  ! main loop
  do while (LIBMUSCLE_Instance_reuse_instance(instance))

     !###  INIT (F_INIT)  ########################!

     print *, ">entering the loop and getting params" !!!DEBUG
     ! get params
     dt_max = LIBMUSCLE_Instance_get_setting_as_real8(instance, 'dt')
     tau = dt_max ! tau is from ets_standalone module
     t_duration = LIBMUSCLE_Instance_get_setting_as_real8(instance, 'duration')
     slice_init = LIBMUSCLE_Instance_get_setting_as_int8(instance, 'slice_initial_number')
     save_slice = LIBMUSCLE_Instance_get_setting_as_logical(instance, 'save_slice')
     adaptive_timestep = LIBMUSCLE_Instance_get_setting_as_logical(instance, 'adaptive_timestep')
     
     print *, ">receiving equilibrium_init" !!!DEBUG
     ! recv init equilibrium
     rmsg = LIBMUSCLE_Instance_receive(instance, 'equilibrium_init')
     rdata = LIBMUSCLE_Message_get_data(rmsg)
     allocate (equilibrium_in_buf(LIBMUSCLE_DataConstRef_size(rdata)))
     call LIBMUSCLE_DataConstRef_as_byte_array(rdata, equilibrium_in_buf)
     call LIBMUSCLE_DataConstRef_free(rdata)
     call LIBMUSCLE_Message_free(rmsg)
     ! recv init coreprof
     rmsg = LIBMUSCLE_Instance_receive(instance, 'coreprof_init')
     rdata = LIBMUSCLE_Message_get_data(rmsg)
     allocate (coreprof_in_buf(LIBMUSCLE_DataConstRef_size(rdata)))
     call LIBMUSCLE_DataConstRef_as_byte_array(rdata, coreprof_in_buf)
     call LIBMUSCLE_DataConstRef_free(rdata)
     t_init = LIBMUSCLE_Message_timestamp(rmsg)
     call LIBMUSCLE_Message_free(rmsg)
     ! recv init coresource
     rmsg = LIBMUSCLE_Instance_receive(instance, 'coresource_init')
     rdata = LIBMUSCLE_Message_get_data(rmsg)
     allocate (coresource_in_buf(LIBMUSCLE_DataConstRef_size(rdata)))
     call LIBMUSCLE_DataConstRef_as_byte_array(rdata, coresource_in_buf)
     call LIBMUSCLE_DataConstRef_free(rdata)
     call LIBMUSCLE_Message_free(rmsg)
     ! recv init coreimpur
     rmsg = LIBMUSCLE_Instance_receive(instance, 'coreimpur_init')
     rdata = LIBMUSCLE_Message_get_data(rmsg)
     allocate (coreimpur_in_buf(LIBMUSCLE_DataConstRef_size(rdata)))
     call LIBMUSCLE_DataConstRef_as_byte_array(rdata, coreimpur_in_buf)
     call LIBMUSCLE_DataConstRef_free(rdata)
     call LIBMUSCLE_Message_free(rmsg)
     ! recv init toroidfield
     rmsg = LIBMUSCLE_Instance_receive(instance, 'toroidfield_init')
     rdata = LIBMUSCLE_Message_get_data(rmsg)
     allocate (toroidfield_in_buf(LIBMUSCLE_DataConstRef_size(rdata)))
     call LIBMUSCLE_DataConstRef_as_byte_array(rdata, toroidfield_in_buf)
     call LIBMUSCLE_DataConstRef_free(rdata)
     call LIBMUSCLE_Message_free(rmsg)

     allocate(coreprof_out_buf, source=coreprof_in_buf)
     allocate(equilibrium_out_buf, source=equilibrium_in_buf)

     t_current = t_init
     step = 0
     
     print *, '>ets: t_init=', t_current, ' ;step=', step, ';dt_max=', dt_max, ' ;t_duration= ', t_duration !DEBUG

     !###  TIME LOOP  ############################!
     do while (t_current+dt_max .lt. t_init+t_duration)
        
        print *, '>ets: new iteration with t_current=', t_current, ' at step=', step !DEBUG
        !###  O_I  ###############################!
        
        ! send coreprof
     !    !print *, '>ets: printing first ', 64, ' symbols of coreprof_out_buf', coreprof_out_buf(1:64)  ! DEBUG
     !    print *, 'sending coreprof_out_flux2tcoeff'
     !    sdata = LIBMUSCLE_Data_create_byte_array(coreprof_out_buf)
     !    smsg = LIBMUSCLE_Message_create(t_current, sdata)
     !    call LIBMUSCLE_Instance_send(instance, 'coreprof_out_flux2tcoeff', smsg)
     !    call LIBMUSCLE_Message_free(smsg)
     !    call LIBMUSCLE_Data_free(sdata)

        print *, 'sending coreprof_out'
        sdata = LIBMUSCLE_Data_create_byte_array(coreprof_out_buf)
        smsg = LIBMUSCLE_Message_create(t_current, sdata)
        call LIBMUSCLE_Instance_send(instance, 'coreprof_out', smsg)
        call LIBMUSCLE_Message_free(smsg)
        call LIBMUSCLE_Data_free(sdata)
        
        ! send equilibrium
        !print *, '>ets: printing first ', 64, ' symbols of equilibrium_out_buf', equilibrium_out_buf(1:64)  ! DEBUG
        print *, 'sending equilibrium_out'
        sdata = LIBMUSCLE_Data_create_byte_array(equilibrium_out_buf)
        smsg = LIBMUSCLE_Message_create(t_current, sdata)
        call LIBMUSCLE_Instance_send(instance, 'equilibrium_out', smsg)
        call LIBMUSCLE_Message_free(smsg)
        call LIBMUSCLE_Data_free(sdata)
             
        deallocate(equilibrium_out_buf)
        nullify(equilibrium_out_buf)
        deallocate(coreprof_out_buf)
        nullify(coreprof_out_buf)

        !###  S  #################################!
        if (associated(equilibrium_in_buf)) deallocate(equilibrium_in_buf)
        if (associated(coretransp_in_buf)) deallocate(coretransp_in_buf)
        ! recv equilibrium
        print *, 'receiving equilibrium_in'
        rmsg = LIBMUSCLE_Instance_receive(instance, 'equilibrium_in')
        rdata = LIBMUSCLE_Message_get_data(rmsg)
        allocate (equilibrium_in_buf(LIBMUSCLE_DataConstRef_size(rdata)))
        call LIBMUSCLE_DataConstRef_as_byte_array(rdata, equilibrium_in_buf)
        call LIBMUSCLE_DataConstRef_free(rdata)
        call LIBMUSCLE_Message_free(rmsg)
        ! recv coretransp
        print *, 'receiving coretransp_in'
        rmsg = LIBMUSCLE_Instance_receive(instance, 'coretransp_in')
        rdata = LIBMUSCLE_Message_get_data(rmsg)
        allocate (coretransp_in_buf(LIBMUSCLE_DataConstRef_size(rdata)))
        call LIBMUSCLE_DataConstRef_as_byte_array(rdata, coretransp_in_buf)
        call LIBMUSCLE_DataConstRef_free(rdata)
        call LIBMUSCLE_Message_free(rmsg)

        if (save_slice) then
           write(stepstr,'(I5.5)') slice_init+step
           call byte2file("equilibrium_"//stepstr//".cpo", &
                equilibrium_in_buf, &
                size(equilibrium_in_buf))
           call byte2file("coreprof_"//stepstr//".cpo", &
                coreprof_in_buf, &
                size(coreprof_in_buf))
           call byte2file("coretransp_"//stepstr//".cpo", &
                coretransp_in_buf, &
                size(coretransp_in_buf))           
        end if

        call ets2buf( &
             coreprof_in_buf, &
             equilibrium_in_buf, &
             coretransp_in_buf, &
             coresource_in_buf, &
             coreimpur_in_buf, &
             coreprof_out_buf, &
             t_current)

        print *,"ETS CALCULATED PROFILES AT TIME = ",t_current

        deallocate(coreprof_in_buf)
        allocate(coreprof_in_buf, source=coreprof_out_buf)

        call equilupdate2buf( &
             coreprof_in_buf, &
             toroidfield_in_buf, &
             equilibrium_in_buf, &
             equilibrium_out_buf)

        step = step+1
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
