#ifdef POWER_XL
#define use_device_addr use_device_ptr
#endif

module MM_HIP

  use iso_fortran_env
  use iso_c_binding
  
  implicit none
  private 

  public :: &
    MatrixMultiply_HIP
    
  
  interface
  
    subroutine MatrixMultiply_HIP_Launch ( A, B, C, nValues ) &
               bind ( C, name = 'MatrixMultiply_HIP_Launch' )
      use iso_c_binding
      implicit none
      type ( c_ptr ), value :: &
        A, B, C
      integer ( c_int ), value :: &
        nValues
    end subroutine MatrixMultiply_HIP_Launch
  
  end interface 
  

contains


  subroutine MatrixMultiply_HIP ( A, B, C )

    real ( real64 ), dimension ( :, : ), intent ( in ), target :: &
      A, B
    real ( real64 ), dimension ( :, : ), intent ( inout ), target :: &
      C
      
    !$OMP target data use_device_addr ( A, B, C )
    call MatrixMultiply_HIP_Launch &
           ( c_loc ( A ), c_loc ( B ), c_loc ( C ), size ( A, dim = 1 ) )
    !$OMP end target data
    
  end subroutine MatrixMultiply_HIP

end module MM_HIP
