#ifdef POWER_XL
#define use_device_addr use_device_ptr
#endif

program MPI_Send_Recv_Example

  use mpi
  use omp_lib
  use iso_c_binding
  use iso_fortran_env
  
  implicit none
  
  integer :: &
    nV = 10, &
    MPI_Rank, &
    MPI_Size, &
    Error
  real ( real64 ), allocatable, dimension ( : ) :: &
    Buffer
  type ( c_ptr ) :: &
    D_Buffer
    
  allocate ( Buffer ( nV ) )
  !$OMP target enter data map ( to: Buffer )
  
  call MPI_INIT ( Error ) 
  call MPI_COMM_SIZE ( MPI_COMM_WORLD, MPI_Size, Error )
  call MPI_COMM_RANK ( MPI_COMM_WORLD, MPI_Rank, Error )
  
  if ( MPI_Size /= 2 ) then
    print*, 'This example is to be run with 2 MPI processes only.'
    call MPI_ABORT ( MPI_COMM_WORLD, -1, Error ) 
  end if

  if ( MPI_Rank == 0 ) then
    
    Buffer = huge ( 1.0_Real64)  !-- Set value on host
    
    !$OMP target
    Buffer = 2.0_Real64  !-- Set value on device
    !$OMP end target
    
    !$OMP target data use_device_addr ( Buffer )
    call MPI_SEND &
           ( Buffer, nV, MPI_DOUBLE_PRECISION, 1, 0, MPI_COMM_WORLD, &
             Error )
    !$OMP end target data

  end if
  
  if ( MPI_Rank == 1 ) then
    Buffer = - huge ( 1.0_Real64 )
    call MPI_RECV &
           ( Buffer, nV, MPI_DOUBLE_PRECISION, 0, 0, MPI_COMM_WORLD, &
             MPI_STATUS_IGNORE, Error )
    print*, 'Buffer on Rank 1', Buffer
  end if
  
end program MPI_Send_Recv_Example
