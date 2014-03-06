module constants
  
  implicit none

  save
  integer, parameter              ::  ISIGN       = 1
  integer, parameter              ::  dp_real     = SELECTED_REAL_KIND(8,8)
    integer, parameter            ::  ip          = 4
  real(8), parameter   ::  pi          = 4.0D0*datan(1.0D0)
  real(8), parameter   ::  twopi       = 8.0D0*datan(1.0D0)
  real(8), parameter   ::  HUGE_NUMBER       = 1.0D50
  real(8), parameter   ::  arcseconds_per_radian = (360.0D0*60.0D0*60.0D0)/(2.0D0*pi)
  integer, parameter              ::  max_ngrid   = 2048

end module constants