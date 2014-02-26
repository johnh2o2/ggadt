module constants
  
  implicit none

  save
  integer, parameter            ::  ISIGN       = 1
  real, parameter               ::  pi          = 4.*atan(1.)
  real, parameter               ::  twopi       = 8.*atan(1.)
  real, parameter				::  arcseconds_per_radian = (360.*60.*60.)/(2*pi)
  integer, parameter            ::  max_ngrid   = 2048

end module constants