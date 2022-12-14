module MM_OpenMP

  use iso_fortran_env
  
  implicit none
  private 

  public :: &
    MatrixMultiply_OpenMP
  
contains

  subroutine MatrixMultiply_OpenMP ( A, B, C )

    real ( real64 ), dimension ( :, : ), intent ( in ) :: &
      A, B
    real ( real64 ), dimension ( :, : ), intent ( out ) :: &
      C
    
    integer :: &
      iV, jV, kV
    
    !$OMP target teams distribute parallel do collapse ( 2 )
    do iV = 1, size ( A, dim = 1 ) 
      do jV = 1, size ( B, dim = 2 )
        C ( iV, jV ) = 0.0_real64
        do kV = 1, size ( A, dim = 2 )
          C ( iV, jV ) = C ( iV, jV )  +  A ( iV, kV ) * B ( kV, jV )
        end do
      end do
    end do
    
  end subroutine MatrixMultiply_OpenMP

end module MM_OpenMP
