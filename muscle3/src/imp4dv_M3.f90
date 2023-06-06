program imp4dv_M3
  use imp4dv_standalone
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

  ! code specific
  character(kind=c_char), pointer :: coreprof_in_buf(:)
  character(kind=c_char), pointer :: equilibrium_in_buf(:)
  character(kind=c_char), pointer :: coretransp_in_buf(:)
  character(kind=c_char), pointer :: coretransp_out_buf(:)

  ports = LIBMUSCLE_PortsDescription_create()

  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_F_INIT, 'coreprof_in')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_F_INIT, 'equilibrium_in')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_F_INIT, 'coretransp_in')

  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_O_F, 'coretransp_out')  

  instance = LIBMUSCLE_Instance_create(ports)
  call LIBMUSCLE_PortsDescription_free(ports)

  !coretransp_in_buf => null()
  
  !print *, ">before entering the loop" !DEBUG
  ! main loop
  do while (LIBMUSCLE_Instance_reuse_instance(instance))

     !###  INIT (F_INIT)  ########################!

     print *, ">entering new iteration"

     !###  S  #################################!
     if (associated(coreprof_in_buf)) deallocate(coreprof_in_buf)
     if (associated(equilibrium_in_buf)) deallocate(equilibrium_in_buf)
     if (associated(coretransp_in_buf)) deallocate(coretransp_in_buf)

     ! recv coreprof
     print *, ">receiving coreprof"
     rmsg = LIBMUSCLE_Instance_receive(instance, 'coreprof_in')
     rdata = LIBMUSCLE_Message_get_data(rmsg)
     allocate (coreprof_in_buf(LIBMUSCLE_DataConstRef_size(rdata)))
     call LIBMUSCLE_DataConstRef_as_byte_array(rdata, coreprof_in_buf)
     call LIBMUSCLE_DataConstRef_free(rdata)
     t_current = LIBMUSCLE_Message_timestamp(rmsg)
     call LIBMUSCLE_Message_free(rmsg)

     ! recv equilibrium
     print *, ">receiving equilibrium"
     rmsg = LIBMUSCLE_Instance_receive(instance, 'equilibrium_in')
     rdata = LIBMUSCLE_Message_get_data(rmsg)
     allocate (equilibrium_in_buf(LIBMUSCLE_DataConstRef_size(rdata)))
     call LIBMUSCLE_DataConstRef_as_byte_array(rdata, equilibrium_in_buf)
     call LIBMUSCLE_DataConstRef_free(rdata)
     call LIBMUSCLE_Message_free(rmsg)

     ! recv coretransp
     print *, ">receiving coretransp"
     rmsg = LIBMUSCLE_Instance_receive(instance, 'coretransp_in')
     rdata = LIBMUSCLE_Message_get_data(rmsg)
     allocate (coretransp_in_buf(LIBMUSCLE_DataConstRef_size(rdata)))
     call LIBMUSCLE_DataConstRef_as_byte_array(rdata, coretransp_in_buf)
     call LIBMUSCLE_DataConstRef_free(rdata)
     call LIBMUSCLE_Message_free(rmsg)

     ! calling imp4dv
     allocate(coretransp_out_buf, source=coretransp_in_buf)

     !print *, ">printing first ", 64, " of equilibrium_in_buf of size ", size(equilibrium_in_buf) !DEBUG
     !print *, equilibrium_in_buf(1:64) !DEBUG

     !print *, ">printing first ", 64, " of coreprof_in_buf of size ", size(coreprof_in_buf) !DEBUG
     !print *, coreprof_in_buf(1:64) !DEBUG

     !print *, ">printing first ", 64, " of coretransp_out_buf of size ", size(coretransp_out_buf) !DEBUG
     !print *, coretransp_out_buf(1:64) !DEBUG

     print *, ">calling imp4dv2buf"
     call imp4dv2buf( &
          equilibrium_in_buf, &
          coreprof_in_buf, &
          coretransp_in_buf, &
          coretransp_out_buf)

     print *,"IMP4DV CALCULATED Ds AND Vs"

     deallocate(equilibrium_in_buf)
     deallocate(coreprof_in_buf)
     !allocate(coreprof_in_buf, source=coretransp_in_buf)

     !###  O_I  ###############################!
     ! send coretransp
     print *, ">sending coretransp_out"
     sdata = LIBMUSCLE_Data_create_byte_array(coretransp_out_buf)
     smsg = LIBMUSCLE_Message_create(t_current, sdata)
     call LIBMUSCLE_Instance_send(instance, 'coretransp_out', smsg)
     call LIBMUSCLE_Message_free(smsg)
     call LIBMUSCLE_Data_free(sdata)
     print *, ">sent coretransp_out"
  
     deallocate(coretransp_out_buf)
     nullify(coretransp_out_buf)

  end do
  
  call LIBMUSCLE_Instance_free(instance)

end program imp4dv_M3
