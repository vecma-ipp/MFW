program dupplicate_M3
  use ymmsl
  use libmuscle
  implicit none

  ! MUSCLE3 specific
  type(LIBMUSCLE_PortsDescription) :: ports
  type(LIBMUSCLE_Instance) :: instance

  type(LIBMUSCLE_Message) :: rmsg
  type(LIBMUSCLE_DataConstRef) :: rdata

  ports = LIBMUSCLE_PortsDescription_create()
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_F_INIT, 'input')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_O_F, 'copy_A')
  call LIBMUSCLE_PortsDescription_add(ports, YMMSL_Operator_O_F, 'copy_B')
  instance = LIBMUSCLE_Instance_create(ports)
  call LIBMUSCLE_PortsDescription_free(ports)
  
  ! main loop
  do while (LIBMUSCLE_Instance_reuse_instance(instance))

     ! receive data
     rmsg = LIBMUSCLE_Instance_receive(instance, 'input')

     ! send data
     call LIBMUSCLE_Instance_send(instance, 'copy_A', rmsg)
     call LIBMUSCLE_Instance_send(instance, 'copy_B', rmsg)

     call LIBMUSCLE_Message_free(rmsg)
     
  end do
  
  call LIBMUSCLE_Instance_free(instance)

end program dupplicate_M3
