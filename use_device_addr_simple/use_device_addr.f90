#ifdef POWER_XL
#define use_device_addr use_device_ptr
#endif

program use_device_addr_example

  real, dimension ( 10 ) :: &
    A
    
  call print_addr ( A )  !-- print host addr
  !$OMP target enter data map ( to: A )
  
  !$OMP target data use_device_addr ( A )
  call print_addr ( A )  !-- print device addr
  !$OMP end target data
  
  call print_addr ( A )  !-- print host addr
  
contains

  subroutine print_addr ( A )
    use iso_c_binding
    real, dimension ( : ), intent ( in ), target :: &
      A
    
    type ( c_ptr ) :: &
      Ptr
    integer, parameter  :: &
      KBI = selected_int_kind ( 15 )
    integer ( KBI ) :: &
      Addr
    character ( 1024 ) :: &
      Buffer
    
    Ptr = c_loc ( A )
    Addr = transfer ( Ptr, 1_KBI )
    write ( Buffer, fmt = ' ( z64 ) ' ) Addr
    Buffer = '0x' // adjustl ( Buffer ) 
    print*, trim ( Buffer )
  end subroutine print_addr

end program use_device_addr_example
