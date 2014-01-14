module common_mod
  use, intrinsic :: iso_c_binding
  use options
  implicit none
  save

  type(options_t) :: opts 

  character(len=100), parameter ::  VERSION="GGADT (General Geometry Anomalous Diffraction Theory) v0.9.0"
  character(len=100), parameter ::  INFO = "Written by John Hoffman (jah5@princeton.edu)&
    & and Michael Tarczon (mtarczon@princeton.edu)"

  
  real, parameter               ::  pi          = 3.14159
  complex(c_double_complex)     ::  delm 
 
  real                          ::  a_eff     
  real                          ::  ephot     
  character(len=100)            ::  geometry  
  character(len=100)            ::  euler_angle_mode


  integer                       ::  ngrid 
  integer                       ::  nangle 
  integer                       ::  ierr

  real                          ::  box_width  
  real                          ::  ior_im        
  real                          ::  ior_re     
  real, dimension(3)            ::  grain_a 
  logical                       ::  use_experimental_fft
  logical                       ::  asked_for_version
  character(len=200)            ::  parameter_file_name
  character(len=100)            ::  fftw_optimization_mode_name
  character(len=100)            ::  sphlist_fname

  contains

    subroutine set_parameter_values()
      implicit none
      ! define help flag
      call define_help_flag(opts,print_help)
      call define_flag(opts, "version",abbrev='v',&
        description="Print version information")
      call define_option_string(opts,"parameter-file-name","",required=.false.,&
        description="Path to parameter file")

      ! Define command line options

      call define_option_string(opts,"grain-geometry","sphere",required=.false.,&
        description="SPHERES, ELLIPSOID, SPHERE")
      call define_option_string(opts,"cluster-file-name","",required=.false.,&
        description="Path to file that defines a grain made up of a cluster of spheres")
      call define_option_real(opts,"aeff",0.2,required=.false.,description="Effective radius of grain")
      call define_option_real(opts,"ephot",0.5,required=.false.,description="Energy of X-ray photon in keV")
      call define_option_real(opts,"RE-index-of-refraction",-2.079E-3,required=.false., &
        description="Real component of the index of refraction, minus 1")
      call define_option_real(opts,"IM-index-of-refraction",3.201E-3,required=.false., &
        description="Imaginary component of the index of refraction")
      call define_option_real(opts,"grid-width",64.0,required=.false.,&
        description="Width of numerical grid as a fraction of a_eff.&
          & Higher values (>2) give you more angular resolution when calculating the scattering cross section,&
          & at the cost of reducing the angular resolution of the grain on the grid.")

      call define_option_integer(opts,"ngrid",512,required=.false.,&
        description="Number of grid points along 1 dimension")
      call define_option_integer(opts,"nangle",100,required=.false.,&
        description="Number of orientations to average over")
      call define_option_string(opts,"euler-angle-mode","random",required=.false.,&
        description="How to choose Euler angles (sequential, random)")
     
      
      call define_option_string(opts,"fftw-optimization","estimate",required=.false.,&
        description="[estimate,measure,patient,exhaustive]")
      call define_flag(opts,"use-experimental-fft",abbrev='s',&
        description="Possible speedup? Experimental!")

      ! Now process command line options and read any specified parameter file

      call process_command_line(opts,ierr)
      if (ierr .ne. 0) stop
      call check_required_options(opts,ierr)
      if (ierr .ne. 0) stop

      ! Set values
      call get_option_string(opts,"parameter-file-name",parameter_file_name)
      call get_option_real(opts,"aeff",a_eff)
      call get_option_real(opts,"ephot",ephot)
      call get_flag(opts,"use-experimental-fft",use_experimental_fft)
      call get_option_real(opts,"RE-index-of-refraction",ior_re)
      call get_option_real(opts,"IM-index-of-refraction",ior_im)
      delm = cmplx(ior_re,ior_im)
      call get_option_string(opts,"grain-geometry",geometry)
      call get_option_string(opts,"euler-angle-mode",euler_angle_mode)
      call get_option_integer(opts,"ngrid",ngrid)
      call get_option_integer(opts,"nangle",nangle)
      call get_option_real(opts,"grid-width",box_width)
      call get_option_string(opts,"cluster-file-name",sphlist_fname)
      call get_option_string(opts,"fftw-optimization",fftw_optimization_mode_name)
      call get_flag(opts,"version",asked_for_version)

      if (asked_for_version) then
        print *, VERSION
        print *, INFO 
        stop
      end if 

      ! Set to lower case
      fftw_optimization_mode_name = strlowcase(fftw_optimization_mode_name)
      euler_angle_mode = strlowcase(euler_angle_mode)
      geometry = strlowcase(geometry)


      ! Read parameter file if one is given (don't overwrite command-line options if already given)
      if (parameter_file_name .ne. "") then
        call process_input_file(opts,parameter_file_name,ierr,overwrite="NO",delim_char="=")
        if (ierr .ne. 0) then
          print *,"Cannot read ",parameter_file_name,". Quitting."
          stop
        end if 
      end if
    end subroutine set_parameter_values

    subroutine print_help(opts)
      type(options_t), intent(in) :: opts
      write (*,'(a)') "ggadt: Compute orientationally &
      &averaged scattering cross section of ISM grain as a function of the scattering angle."
      write (*,'(a)') "Usage: ggadt [options]"
      write (*,'(a)') ""
      write (*,'(a)') "Options:"
      call print_options(opts)
    end subroutine print_help

    subroutine print_parameters()
      implicit none
      call print_option_values(opts,0)


    end subroutine print_parameters


    function rot_matrix(eul_ang)
      implicit none
      real, dimension(3), intent(in) :: eul_ang
      real, dimension(3,3) :: rot_matrix
      rot_matrix(1,1) =  cos(eul_ang(2))*cos(eul_ang(3))
      rot_matrix(1,2) = -cos(eul_ang(1))*sin(eul_ang(3))-sin(eul_ang(1))*sin(eul_ang(2))*cos(eul_ang(3))
      rot_matrix(1,3) =  sin(eul_ang(1))*sin(eul_ang(3))-cos(eul_ang(1))*sin(eul_ang(2))*sin(eul_ang(3))

      rot_matrix(2,1) =  cos(eul_ang(2))*sin(eul_ang(3))
      rot_matrix(2,2) =  cos(eul_ang(1))*cos(eul_ang(3))-sin(eul_ang(1))*sin(eul_ang(2))*sin(eul_ang(3))
      rot_matrix(2,3) = -sin(eul_ang(1))*cos(eul_ang(3))-cos(eul_ang(1))*sin(eul_ang(2))*sin(eul_ang(3))

      rot_matrix(3,1) =  sin(eul_ang(2))
      rot_matrix(3,2) =  sin(eul_ang(1))*cos(eul_ang(2))
      rot_matrix(3,3) =  cos(eul_ang(1))*cos(eul_ang(2))
    end function rot_matrix

    function rot_x(eul_ang)
      implicit none
      real, intent(in) :: eul_ang
      real, dimension(3,3) :: rot_x
      rot_x(1,1) = 1.0
      rot_x(1,2) = 0.0
      rot_x(1,3) = 0.0

      rot_x(2,1) =  0.0
      rot_x(2,2) =  cos(eul_ang)
      rot_x(2,3) = -sin(eul_ang)

      rot_x(3,1) =  0.0
      rot_x(3,2) =  sin(eul_ang)
      rot_x(3,3) =  cos(eul_ang)
    end function rot_x 

    function rot_y(eul_ang)
      implicit none
      real, intent(in) :: eul_ang
      real, dimension(3,3) :: rot_y
      rot_y(1,1) = cos(eul_ang)
      rot_y(1,2) = 0.0
      rot_y(1,3) = sin(eul_ang)

      rot_y(2,1) =  0.0
      rot_y(2,2) =  1.0
      rot_y(2,3) =  0.0

      rot_y(3,1) = -sin(eul_ang)
      rot_y(3,2) =  0.0
      rot_y(3,3) =  cos(eul_ang)
    end function rot_y

    function rot_z(eul_ang)
      implicit none
      real, intent(in) :: eul_ang
      real, dimension(3,3) :: rot_z
      rot_z(1,1) = cos(eul_ang)
      rot_z(1,2) = sin(eul_ang)
      rot_z(1,3) = 0.0

      rot_z(2,1) = -sin(eul_ang)
      rot_z(2,2) =  cos(eul_ang)
      rot_z(2,3) =  0.0

      rot_z(3,1) = 0.0
      rot_z(3,2) = 0.0
      rot_z(3,3) = 1.0
    end function rot_z

    function strlowcase(input_string) 
        character(*),parameter :: lower_case = 'abcdefghijklmnopqrstuvwxyz' 
        character(*),parameter :: upper_case =  'ABCDEFGHIJKLMNOPQRSTUVWXYZ'  
       ! -- argument and result
       character( * ), intent( in ) :: input_string
       character( len( input_string ) ) :: strlowcase
       ! -- local variables
       integer :: i, n

       ! -- copy input string
       strlowcase = input_string
       ! -- loop over string elements
       do i = 1, len(strlowcase)
         ! -- find location of letter in upper case constant string
         n = index( upper_case, strlowcase( i:i ) )
         ! -- if current substring is an upper case letter, make it lower case
         if ( n /= 0 ) strlowcase( i:i ) = lower_case( n:n )
       end do
    end function strlowcase 

end module common_mod
