c
c MPI fortran routine stubs
c
      subroutine mpi_abort
      print*,'mpi_abort: your are using a non-parallel HDF5!'
      end
c--------------------------------------------------------------------- 
      subroutine mpi_allreduce
      print*,'mpi_allreduce: your are using a non-parallel HDF5!'
      end
c--------------------------------------------------------------------- 
      subroutine mpi_cart_sub
      print*,'mpi_cart_sub: your are using a non-parallel HDF5!'
      end
c--------------------------------------------------------------------- 
      subroutine mpi_cartdim_get
      print*,'mpi_cartdim_get: your are using a non-parallel HDF5!'
      end
c--------------------------------------------------------------------- 
      subroutine mpi_comm_free
      print*,'mpi_comm_free: your are using a non-parallel HDF5!'
      end
c--------------------------------------------------------------------- 
      subroutine mpi_comm_rank(comm, rank, ierr)
      integer comm, rank, ierr
      rank = 0
c$$$      print*,'mpi_comm_rank: your are using a non-parallel HDF5!'
      end
c--------------------------------------------------------------------- 
      subroutine mpi_scan
      print*,'mpi_scan: your are using a non-parallel HDF5!'
      end
c--------------------------------------------------------------------- 
      subroutine mpi_topo_test
      print*,'mpi_scan: your are using a non-parallel HDF5!'
      end
c--------------------------------------------------------------------- 
