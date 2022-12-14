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
  real ( real64 ), allocatable, dimension ( : ), target :: &
    Buffer
  real ( real64 ), dimension ( : ), pointer :: &
    F_Buffer
  type ( c_ptr ) :: &
    D_Buffer
    
  allocate ( Buffer ( nV ) )
  
  
  !-- allocate memory on GPU, then associate it with host variable
  associate ( Size => c_sizeof ( 1.0_c_double ) * nV )
  D_Buffer = omp_target_alloc ( c_sizeof ( 1.0_c_double ) * nV, 0 )
  Error = omp_target_associate_ptr ( c_loc ( Buffer ), D_Buffer, Size, &
          0_c_size_t, 0 )
  end associate
  
  
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
    
    call c_f_pointer ( D_Buffer, F_Buffer, [ nV ] )
    
    !-- !$OMP target data use_device_addr ( Buffer )
    !call MPI_SEND &
    !       ( Buffer, nV, MPI_DOUBLE_PRECISION, 1, 0, MPI_COMM_WORLD, &
    !         Error )
    !-- !$OMP end target data

    call MPI_SEND &
           ( F_Buffer, nV, MPI_DOUBLE_PRECISION, 1, 0, MPI_COMM_WORLD, &
             Error )

  end if
  
  if ( MPI_Rank == 1 ) then
    Buffer = - huge ( 1.0_Real64 )
    call MPI_RECV &
           ( Buffer, nV, MPI_DOUBLE_PRECISION, 0, 0, MPI_COMM_WORLD, &
             MPI_STATUS_IGNORE, Error )
    print*, 'Buffer on Rank 1', Buffer
  end if
  
end program MPI_Send_Recv_Example
