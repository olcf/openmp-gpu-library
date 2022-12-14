program MatrixMultiply_Test

  use OMP_LIB
  use iso_fortran_env
  use iso_c_binding
  
  use MM_Utilities
  use MM_OpenMP
  use MM_HIP
  use MM_GPU_Library
  
  implicit none
  
  integer :: &
    N_VALUES = 1024
    
  real ( real64 ) :: &
    Error
  real ( real64 ), dimension ( :, : ), allocatable :: &
    Matrix_A, Matrix_B, Matrix_C, &
    Matrix_C_Ref
  
  !-- Initialize matrices
  allocate ( Matrix_A ( N_VALUES, N_VALUES ), &
             Matrix_B ( N_VALUES, N_VALUES ), &
             Matrix_C ( N_VALUES, N_VALUES ) )
  
  call random_number ( Matrix_A ) 
  call random_number ( Matrix_B )
  Matrix_C = huge ( 1.0_real64 )  !-- make error more obvious
  
  !$OMP target enter data map ( to: Matrix_A, Matrix_B, Matrix_C )
  

  !-- Calculate reference results on CPU 
  Matrix_C_Ref = matmul ( Matrix_A, Matrix_B )
  
  !-- Calculate with OpenMP Offload
  call MatrixMultiply_OpenMP ( Matrix_A, Matrix_B, Matrix_C )

  !-- Get results and verify
  !$OMP target update from ( Matrix_C )
  call Verify ( Matrix_C_Ref, Matrix_C, 'OpenMP', Error )

  !-- Reset the results matrix
  Matrix_C = huge ( 1.0_real64 )  !-- make error more obvious
  !$OMP target update to ( Matrix_C )
  

  
  !-- Calculate with HIP kernel
  call MatrixMultiply_HIP ( Matrix_A, Matrix_B, Matrix_C )
  
  !-- Get results and verify
  !$OMP target update from ( Matrix_C )
  call Verify ( Matrix_C_Ref, Matrix_C, 'HIP', Error )
  
  !-- Reset the results matrix
  Matrix_C = huge ( 1.0_real64 )  !-- make error more obvious
  !$OMP target update to ( Matrix_C )
  
  
  
  !-- Calculate with GPU Library (e.g. hipblas or cublas)
  call MatrixMultiply_GPU_Library ( Matrix_A, Matrix_B, Matrix_C )
  
  !-- Get results and verify
  !$OMP target update from ( Matrix_C )
  call Verify ( Matrix_C_Ref, Matrix_C, 'GPU Library', Error )
  
  
  
  !-- cleanup
  !$OMP target exit data map ( delete: Matrix_C, Matrix_B, Matrix_A )

end program MatrixMultiply_Test
