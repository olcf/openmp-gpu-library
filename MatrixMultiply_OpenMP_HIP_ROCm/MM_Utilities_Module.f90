module MM_Utilities

  use iso_fortran_env
  
  implicit none
  private 
  
  public :: &
    Verify
  
contains

  function L1_Error ( A_Start, A_End ) result ( L1 )
  
    real ( real64 ), dimension ( :, : ), intent ( in ) :: &
      A_Start, &
      A_End
    real ( real64 ) :: &
      L1

    real ( real64 ) :: &
      MyDistanceSum, &
      MyOriginSum

    MyDistanceSum = sum ( abs ( A_End - A_Start ) )
    MyOriginSum = sum ( abs ( A_Start ) )

    if ( MyOriginSum /= 0.0_real64 ) then
      L1 = MyDistanceSum / MyOriginSum
    else
      L1 = huge ( 1.0_real64 )   
      print*, 'WARNING: Cannot compute L1 error for vanishing variable'
    end if

  end function L1_Error


  subroutine Verify ( A_Ref, A, Label, Error, V_Option )

    real ( real64 ), dimension ( :, : ), intent ( in ) :: &
      A_Ref, &  
      A
    character ( * ), intent ( in ) :: &
      Label
    real ( real64 ), intent ( out ) :: &
      Error
    logical, intent ( out ), optional :: &
      V_Option

    Error = L1_Error ( A_Ref, A )
    if ( Error <= 1.0e-14_real64 ) then
      if ( present ( V_Option ) ) V_Option = .true.
      write ( *, fmt =  '(a31, a7)' ) trim ( Label ) // ' Verification : ',  'PASSED'
    else 
      if ( present ( V_Option ) ) V_Option = .false.
      write ( *, fmt =  '(a31, a7)' ) trim ( Label ) // ' Verification : ',  'FAILED'
      write ( *, fmt =  '(a31, es15.6e3)' ) 'Error : ', Error
      associate ( mV => min ( 5, size ( A, dim = 1 ) ) )
      print*, 'A_Ref ( 1 : 5, 1 : 5)', A_Ref ( 1 : mV, 1 : mV )
      print*, 'A     ( 1 : 5, 1 : 5)', A     ( 1 : mV, 1 : mV )
      end associate
    endif

  end subroutine Verify
  

end module MM_Utilities
