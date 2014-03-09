module common_mod
  ! use, intrinsic :: iso_c_binding
  use options
  use constants
  implicit none

  public


  save

  type(options_t) :: opts 

  character(len=100), parameter ::  VERSION="GGADT (General Geometry Anomalous Diffraction Theory) v0.9.7"
  character(len=100), parameter ::  INFO = "Written by John Hoffman (jah5@princeton.edu)&
    & and Michael Tarczon (mtarczon@princeton.edu)"

  complex(kind=dp_complex)                    ::  delm 
  
  real(kind=dp_real)                          ::  a_eff     
  real(kind=dp_real)                          ::  ephot     
  character(len=50)                           ::  geometry  
  character(len=50)                           ::  euler_angle_mode


  integer                                     ::  ngrain
  integer                                     ::  nscatter_min 
  integer                                     ::  ngrid
  integer                                     ::  nscatter
  integer                                     ::  norientations 
  integer                                     ::  ierr


  real(kind=dp_real)                          ::  grid_width  
  real(kind=dp_real)                          ::  dtheta
  real(kind=dp_real)                          ::  ior_im        
  real(kind=dp_real)                          ::  ior_re     
  real(kind=dp_real), dimension(3)            ::  grain_a 
  real(kind=dp_real)                          ::  max_angle

  logical                                     ::  use_padded_fft
  logical                                     ::  asked_for_version
  logical                                     ::  verbose_mode
  logical                                     ::  timing_mode
  character(len=200)                          ::  parameter_file_name
  character(len=200)                          ::  euler_angle_file
  character(len=100)                          ::  fftw_optimization_mode_name
  character(len=200)                          ::  cluster_file_name

  contains

    subroutine set_parameter_values()
      implicit none
      
     
      ! define help flag

      call define_help_flag(opts,print_help)
      call define_flag(opts, &
        "version",abbrev='v',&
        description="Print version information")
      call define_flag(opts, &
        "verbose",abbrev='d',&
        description="prints variable values and other status messages")
      call define_flag(opts, &
        "timing",abbrev='t',&
        description="suppresses output for timing purposes")
      call define_option_string(opts,&
        "parameter-file-name","",&
        description="Path to parameter file")

      ! Define command line options

      call define_option_string(opts,&
        "grain-geometry","sphere",&
        description="SPHERES, ELLIPSOID, SPHERE")
      call define_option_string(opts,&
        "cluster-file-name","",&
        description="Path to file that defines a grain made up of a cluster of spheres")
      call define_option_real(opts,&
        "aeff",real(0.2,kind=dp_real),&
        description="Effective radius of grain")
      call define_option_real(opts,&
        "grain-axis-x",real(1.0,kind=dp_real),&
        description="For ELLIPSOID grains: x-axis length")
      call define_option_real(opts,&
        "grain-axis-y",real(1.0,kind=dp_real),&
        description="For ELLIPSOID grains: y-axis length")
      call define_option_real(opts,&
        "grain-axis-z",real(1.0,kind=dp_real),&
        description="For ELLIPSOID grains: z-axis length")
      call define_option_real(opts,&
        "ephot",real(0.5,kind=dp_real),&
        description="Energy of X-ray photon in keV")
      call define_option_real(opts,&
        "max-angle",real(6000.,kind=dp_real),&
        description="Maximum scattering angle in arcseconds")
      call define_option_real(opts,&
        "ior-re",real(-2.079E-3,kind=dp_real),&
        description="real(kind=dp_real) component of the index of refraction, minus 1")
      call define_option_real(opts,&
        "ior-im",real(3.201E-3,kind=dp_real),&
        description="Imaginary component of the index of refraction")
      call define_option_real(opts,&
        "dtheta",real(10.0,kind=dp_real), &
        description="Angular resolution for dQscatter/dOmega calculation")
      call define_option_integer(opts,&
        "ngrain",64,&
        description="Number of grid points along 1 dimension with which we should resolve the grain")
      call define_option_integer(opts,&
        "nscatter",64,&
        description="Number of angles along 1 dimension with which we should calculate the scattering cross section")
      call define_option_integer(opts,&
        "norientations",100,&
        description="Number of orientations to average over")
      call define_option_string(opts,&
        "euler-angle-mode","random",&
        description="How to choose Euler angles (sequential, random, file)")
      call define_option_string(opts,&
        "euler-angle-file","",&
        description="Path to a text file containing a list of orientations")
      

      call define_option_string(opts,"fftw-optimization","estimate",required=.false.,&
        description="[estimate,measure,patient,exhaustive]")
      call define_flag(opts,"use-padded-fft",&
        description="Use a slower FFT method that pads a &
        &2d grid to obtain more points in the desired range of scattering angles. &
        &This should only be used for consistency tests.")

      ! Now process command line options and read any specified parameter file

      call process_command_line(opts,ierr)
      if (ierr .ne. 0) then
        write (0,*) "ERROR in processing command line arguments." 
        stop
      end if 
      

    
      ! Read parameter file if one is given (don't overwrite command-line options if already given)
      call get_option_string(opts,"parameter-file-name",parameter_file_name)
      if (parameter_file_name .ne. "") then
        call process_input_file(opts,parameter_file_name,ierr,delim_char="=")
        if (ierr .ne. 0) then
          print *,"Cannot read ",parameter_file_name,". Quitting."
          stop
        end if 
      end if

      ! Set values
      call get_option_real(opts,"aeff",a_eff)
      call get_option_real(opts,"grain-axis-x",grain_a(1))
      call get_option_real(opts,"grain-axis-y",grain_a(2))
      call get_option_real(opts,"grain-axis-z",grain_a(3))
      call get_option_real(opts,"ephot",ephot)
      call get_flag(opts,"use-padded-fft",use_padded_fft)
      call get_flag(opts,"verbose",verbose_mode)
      call get_flag(opts,"timing",timing_mode)
      call get_option_real(opts,"ior-re",ior_re)
      call get_option_real(opts,"ior-im",ior_im)
      call get_option_string(opts,"grain-geometry",geometry)
      call get_option_string(opts,"euler-angle-mode",euler_angle_mode)
      call get_option_string(opts,"euler-angle-file",euler_angle_file)
      call get_option_integer(opts,"ngrain",ngrain)
      call get_option_integer(opts,"nscatter",nscatter)
      call get_option_integer(opts,"norientations",norientations)
      call get_option_real(opts, "dtheta", dtheta)
      call get_option_real(opts,"max-angle",max_angle)
      call get_option_string(opts,"cluster-file-name",cluster_file_name)
      call get_option_string(opts,"fftw-optimization",fftw_optimization_mode_name)
      call get_flag(opts,"version",asked_for_version)

      if (asked_for_version) then
        print *, VERSION
        print *, INFO 
        stop
      end if 

      delm = cmplx(ior_re,ior_im)

      ! Set to lower case
      fftw_optimization_mode_name = strlowcase(fftw_optimization_mode_name)
      euler_angle_mode = strlowcase(euler_angle_mode)
      geometry = strlowcase(geometry)



      if (option_found(opts,"nscatter") .and. option_found(opts, "dtheta")) then
          write(0,*) "Warning: You have specified BOTH the 'nscatter' and 'dtheta' options."
          write(0,*) "We will use your specified value of nscatter:", nscatter
      else if (option_found(opts,"dtheta")) then
          write(0,*) "Specified dtheta = ",dtheta
          nscatter = int(ceiling(real(2*max_angle,kind=dp_real)/dtheta))
          write(0,*) "Nscatter --> ", nscatter
      else if (option_found(opts,"nscatter")) then
          write(0,*) "nscatter specified", nscatter
      end if 

      write(0,*) "Nscatter = ", nscatter
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


    function rot_matrix(eul_ang)
      implicit none
      real(kind=dp_real), dimension(3), intent(in) :: eul_ang
      real(kind=dp_real), dimension(3,3) :: rot_matrix
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
      real(kind=dp_real), intent(in) :: eul_ang
      real(kind=dp_real), dimension(3,3) :: rot_x
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
      real(kind=dp_real), intent(in) :: eul_ang
      real(kind=dp_real), dimension(3,3) :: rot_y
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
      real(kind=dp_real), intent(in) :: eul_ang
      real(kind=dp_real), dimension(3,3) :: rot_z
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
