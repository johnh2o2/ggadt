module constants
  ! use, intrinsic :: iso_c_binding
  implicit none

  save
  integer, parameter                            ::  ISIGN                  = 1
  integer, parameter                            ::  dp_real                = SELECTED_REAL_KIND(8,8)
  ! integer, parameter          ::  dp_complex             = c_double_complex
   integer, parameter            ::  dp_complex             = dp_real
  integer, parameter                            ::  ip                     = 4
  real(8), parameter                            ::  pi                     = 3.1415926535897932384626433832795028841971693993
  real(8), parameter                            ::  twopi                  = 2*3.1415926535897932384626433832795028841971693993
  real(8), parameter                            ::  HUGE_NUMBER            = 1.0D50
  real(8), parameter                            ::  ARCSECONDS_PER_RADIAN  = (360.0D0*60.0D0*60.0D0)/(2.0D0*pi)
  integer, parameter                            ::  MAX_NGRID              = 2048

end module constants