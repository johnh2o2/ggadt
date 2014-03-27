module constants
  ! use, intrinsic :: iso_c_binding
  implicit none

  save
  integer, parameter                            ::  stdout                 = 6
  integer, parameter                            ::  stderr                 = 0
  integer, parameter                            ::  input_unit             = 1

  integer, parameter                            ::  max_str_len            = 300
  
  integer, parameter                            ::  ISIGN                  = 1
  integer, parameter                            ::  dp_real                = SELECTED_REAL_KIND(8,8)
  ! integer, parameter          ::  dp_complex             = c_double_complex
   integer, parameter            ::  dp_complex             = dp_real
  integer, parameter                            ::  ip                     = 4
  real(kind=dp_real), parameter                 ::  pi                     = 3.1415926535897932384626433832795028841971693993
  real(kind=dp_real), parameter                 ::  twopi                  = 2*3.1415926535897932384626433832795028841971693993
  real(kind=dp_real), parameter                 ::  HUGE_NUMBER            = 1.0D50
  real(kind=dp_real), parameter                 ::  ARCSECONDS_PER_RADIAN  = (360.0D0*60.0D0*60.0D0)/(2.0D0*pi)
  integer, parameter                            ::  MAX_NGRID              = 2048


  real(kind=dp_real), parameter                 ::  default_aeff           = 0.2
  real(kind=dp_real), parameter                 ::  default_ephot          = 2.0
  real(kind=dp_real), parameter                 ::  default_grain_a1       = 1.0
  real(kind=dp_real), parameter                 ::  default_grain_a2       = 1.0
  real(kind=dp_real), parameter                 ::  default_grain_a3       = 1.0
  real(kind=dp_real), parameter                 ::  default_ior_re         = -1.920E-4
  real(kind=dp_real), parameter                 ::  default_ior_im         = 2.807E-5
  character(len=200), parameter                 ::  default_grain_geometry = 'sphere'
  character(len=50), parameter                  ::  default_euler_angle_mode = 'random'
  character(len=200), parameter                 ::  default_euler_angle_file = ''
  integer, parameter                            ::  default_ngrain         = 256
  integer, parameter                            ::  default_nscatter       = 100
  integer, parameter                            ::  default_norientations  = 100
  real(kind=dp_real), parameter                 ::  default_dtheta         = 10.0
  real(kind=dp_real), parameter                 ::  default_max_angle      = 3000.
  character(len=200), parameter                 ::  default_cluster_file_name = ''
  character(len=100), parameter                 ::  default_fftw_opt_name = 'estimate'


! New defaults for sed variables
  integer, parameter                            ::  default_nephots                = 100
  real(kind=dp_real), parameter                 ::  default_ephot_min              = 0.1
  real(kind=dp_real), parameter                 ::  default_ephot_max              = 5.0
  character(len=200), parameter                 ::  default_material               = "custom"
  character(len=200), parameter                 ::  default_material_file          = ""
  real(kind=dp_real), parameter                 ::  default_dephot                 = 0.1
     


end module constants
