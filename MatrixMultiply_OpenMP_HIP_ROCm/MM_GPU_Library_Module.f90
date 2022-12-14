#ifdef POWER_XL
#define use_device_addr use_device_ptr
#define __mm_lib_create__  'cublasCreate_v2'
#define __mm_lib_dgemm__   'cublasDgemm_v2'
#define __mm_lib_destroy__ 'cublasDestroy_v2'
#define __blas_op_n__ 0
#define __blas_op_t__ 1
#define __blas_op_c__ 2
#endif

#ifdef Cray_CCE
#define __mm_lib_create__  'hipblasCreate'
#define __mm_lib_dgemm__   'hipblasDgemm'
#define __mm_lib_destroy__ 'hipblasDestroy'
#define __blas_op_n__ 111
#define __blas_op_t__ 112
#define __blas_op_c__ 113

#endif

module MM_GPU_Library

  use iso_fortran_env
  use iso_c_binding
  
  implicit none
  private 

  public :: &
    MatrixMultiply_GPU_Library!, &
!--    MatrixMultiply_GPU_LibraryWithHelper
    
  enum, bind ( c )  !-- From cublas_api.h
    enumerator :: BLAS_OP_N = __blas_op_n__
    enumerator :: BLAS_OP_T = __blas_op_t__
    enumerator :: BLAS_OP_C = __blas_op_c__
  end enum
  
  interface
  
    function MM_LibCreate ( Handle ) &
                 bind ( C, name = __mm_lib_create__ ) result ( Status )
      use iso_c_binding
      implicit none
      type ( c_ptr ) :: &
        Handle
      integer ( c_int ) :: &
        Status
    end function MM_LibCreate
    
    function MM_LibDgemm ( Handle, Trans_A, Trans_B, M, N, K, Alpha, &
                           d_A, LDA, d_B, LDB, Beta, d_C, LDC ) &
               bind ( c, name = __mm_lib_dgemm__ ) result ( Status )
      use iso_c_binding
      implicit none
      type ( c_ptr ), value :: &
        Handle
      integer ( c_int ), value :: &
        Trans_A, Trans_B, &
        M, N, K, &
        LDA, LDB, LDC
      real ( c_double ) :: &
        Alpha, Beta
      type ( c_ptr ), value :: &
        d_A, d_B, d_C
      integer ( c_int ) :: &
        Status
    end function MM_LibDgemm
  
    function MM_LibDestroy ( Handle ) &
                 bind ( C, name = __mm_lib_destroy__ ) result ( Status )
      use iso_c_binding
      implicit none
      type ( c_ptr ), value :: &
        Handle
      integer ( c_int ) :: &
        Status
    end function MM_LibDestroy

    
!--    subroutine MatrixMultiply_BLAS ( A, B, C, nV ) &
!--                 bind ( c, name = 'MatrixMultiply_BLAS' )
!--      use iso_c_binding
!--      type ( c_ptr ), value :: &
!--        A, B, C
!--      integer ( c_int ), value :: &
!--        nV 
!--    end subroutine MatrixMultiply_BLAS

  
  end interface 
  

contains


  subroutine MatrixMultiply_GPU_Library ( A, B, C )

    real ( real64 ), dimension ( :, : ), intent ( in ), target :: &
      A, B
    real ( real64 ), dimension ( :, : ), intent ( inout ), target :: &
      C
    
    integer ( c_int ) :: &
      nV, &
      Status
    real ( c_double ) :: &
      Alpha = 1.0_c_double, &
      Beta  = 0.0_c_double
    type ( c_ptr ) :: &
      Handle
      
    !write ( *, fmt = '(z64)') transfer ( Handle, 1 )
    Status = MM_LibCreate ( Handle )
    !write ( *, fmt = '(z64)') transfer ( Handle, 1 )
    
    nV = size ( A, dim = 1 )
    !$OMP target data use_device_addr ( A, B, C )
    Status = MM_LibDgemm ( Handle, BLAS_OP_N, BLAS_OP_N, nV, nV, nV, &
                           Alpha, c_loc ( A ), nV, c_loc ( B ), nV, &
                           Beta, c_loc ( C ), nV ) 
    !$OMP end target data

    Status = MM_LibDestroy ( Handle )
      
  end subroutine MatrixMultiply_GPU_Library


!--  subroutine MatrixMultiply_GPU_LibraryWithHelper ( A, B, C )
!--
!--    real ( real64 ), dimension ( :, : ), intent ( in ), target :: &
!--      A, B
!--    real ( real64 ), dimension ( :, : ), intent ( inout ), target :: &
!--      C
!--    
!--    integer ( c_int ) :: &
!--      nV
!--    
!--    print*, 'Here'
!--    nV = size ( A, dim = 1 )
!--    !$OMP target data use_device_addr ( A, B, C )
!--    call MatrixMultiply_BLAS ( c_loc ( A ), c_loc ( B ), c_loc ( C ), nV )
!--    !$OMP end target data
!--    
!--  end subroutine MatrixMultiply_GPU_LibraryWithHelper


end module MM_GPU_Library
