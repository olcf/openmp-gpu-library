module Timer_Form

  use OMP_LIB
  use iso_fortran_env

  implicit none
  private
  
  integer, parameter :: &
    KDR = real64, &
    LDL = 31

  type, public :: TimerForm
    integer :: &
      iStart = 0, &
      Handle = -1
    real ( real64 ) :: &
      StartTime, &
      StopTime, &
      TimeInterval, &
      TotalTime
    character ( LDL ) :: &
      Name = ''
  contains
    procedure, private, pass :: &
      InitializeNameLevel
    generic, public :: &
      Initialize => InitializeNameLevel
    procedure, public, pass :: &
      Start
    procedure, public, pass :: &
      Stop
    procedure, public, pass :: &
      ShowInterval
    procedure, public, pass :: &
      ShowTotal
  end type TimerForm

    character ( 10 ), private, parameter :: &
      Suffix = '::::::::::'
    
contains


  subroutine InitializeNameLevel ( T, Name )

    class ( TimerForm ), intent ( inout ) :: &
      T
    character ( * ), intent ( in ) :: &
      Name

    T % Name = Name

    T % StartTime    = 0.0_KDR
    T % StopTime     = 0.0_KDR
    T % TimeInterval = 0.0_KDR
    T % TotalTime    = 0.0_KDR
    
  end subroutine InitializeNameLevel


  subroutine Start ( T )

    class ( TimerForm ), intent ( inout ) :: &
      T
    
    if ( T % iStart  ==  0 ) &
      T % StartTime  =  omp_get_wtime ( )

    T % iStart  =  T % iStart + 1
    
  end subroutine Start


  subroutine Stop ( T )

    class ( TimerForm ), intent ( inout ) :: &
      T

    T % iStart  =  max ( T % iStart - 1, 0 )

    if ( T % iStart == 0 ) then

      T % StopTime = omp_get_wtime ( )

      T % TimeInterval  =  T % StopTime   -  T % StartTime
      T % TotalTime     =  T % TotalTime  +  T % TimeInterval

    end if
    
  end subroutine Stop


  impure elemental subroutine ShowInterval ( T )

    class ( TimerForm ), intent ( inout ) :: &
      T

    integer :: &
      Ignorability

    if ( T % Name == '' ) return

    print*, trim ( T % Name ) // ' Interval ', T % TimeInterval

    !call Show ( T % TimeInterval, &
    !            trim ( T % Name ) // ' Interval ' // Suffix ( 1 : T % Level ), &
    !            Ignorability )

  end subroutine ShowInterval


  impure elemental subroutine ShowTotal ( T )

    class ( TimerForm ), intent ( inout ) :: &
      T

    if ( T % Name == '' ) return
    
    !print*, trim ( T % Name ) // ' TotalTime ', T % TotalTime
    print '(a31,a3,es15.6e3, a2)', &
      trim ( T % Name ) // ' time', ' :', T % TotalTime, ' s'
    
    !call Show ( T % TotalTime, &
    !            trim ( T % Name ) // ' ' // Suffix ( 1 : T % Level ), &
    !            Ignorability )

  end subroutine ShowTotal


end module Timer_Form
