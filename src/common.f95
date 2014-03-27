module common_mod
  ! use, intrinsic :: iso_c_binding

  
  use gpfa
  use options
  use constants
  implicit none

  public


  save

  type(options_t) :: opts 

  character(len=100), parameter ::  VERSION="GGADT (General Geometry Anomalous Diffraction Theory) v0.9.8"
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
  logical                                     ::  asked_for_help
  logical                                     ::  verbose_mode
  logical                                     ::  timing_mode
  character(len=200)                          ::  parameter_file_name


! new variables
  integer                                     ::  nephots
  real(kind=dp_real)                          ::  ephot_min
  real(kind=dp_real)                          ::  dephot
  real(kind=dp_real)                          ::  ephot_max 
  character(len=200)                          ::  material          ! name of predefined material
  character(len=200)                          ::  material_file     ! file that defines ior_re and ior_im for a range of energies
  logical                                     ::  sed_mode          ! are we doing the spectral energy distribution? Maybe
                                                                    !   it's better to define a global "mode" variable
                                                                    !   with a default value and then the user only has 
                                                                    !   to specify "--mode='sed'" or something
!-------
  character(len=200)                          ::  euler_angle_file
  character(len=100)                          ::  fftw_optimization_mode_name
  character(len=200)                          ::  cluster_file_name

  contains

    subroutine set_parameter_values()
      implicit none
      
     
      ! define help flag

      !call define_help_flag(opts,print_help)

      call define_flag(opts,"help",abbrev='h',description="Print this help message.")
      call define_flag(opts, &
        "version",abbrev='v',&
        description="Print version information")
      call define_flag(opts, &
        "verbose",abbrev='d',&
        description="prints variable values and other status messages")
      call define_flag(opts, &
        "timing",abbrev='t',&
        description="suppresses output for timing purposes")
      call define_flag(opts, &
        "sed",&
        description="Generates an SED of the scattering, absorption and extinction cross sections.")
      call define_option_string(opts,&
        "parameter-file-name","",&
        description="Path to parameter file")

      ! Define command line options

      call define_option_string(opts,&
        "grain-geometry",default_grain_geometry,&
        description="SPHERES, ELLIPSOID, SPHERE")
      call define_option_string(opts,&
        "cluster-file-name",default_cluster_file_name,&
        description="Path to file that defines a grain made up of a cluster of spheres")
      call define_option_real(opts,&
        "aeff",default_aeff,&
        description="Effective radius of grain")
      call define_option_real(opts,&
        "grain-axis-x",default_grain_a1,&
        description="For ELLIPSOID grains: x-axis length")
      call define_option_real(opts,&
        "grain-axis-y", default_grain_a2,&
        description="For ELLIPSOID grains: y-axis length")
      call define_option_real(opts,&
        "grain-axis-z", default_grain_a3,&
        description="For ELLIPSOID grains: z-axis length")
      call define_option_real(opts,&
        "ephot",default_ephot,&
        description="Energy of X-ray photon in keV")
      call define_option_real(opts,&
        "max-angle",default_max_angle,&
        description="Maximum scattering angle in arcseconds")
      call define_option_real(opts,&
        "ior-re",default_ior_re,&
        description="real(kind=dp_real) component of the index of refraction, minus 1")
      call define_option_real(opts,&
        "ior-im",default_ior_im,&
        description="Imaginary component of the index of refraction")
      call define_option_real(opts,&
        "dtheta",default_dtheta, &
        description="Angular resolution for dQscatter/dOmega calculation")
      call define_option_integer(opts,&
        "ngrain",default_ngrain,&
        description="Number of grid points along 1 dimension with which we should resolve the grain")
      call define_option_integer(opts,&
        "nscatter",default_nscatter,&
        description="Number of angles along 1 dimension with which we should calculate the scattering cross section")
      call define_option_integer(opts,&
        "norientations",default_norientations,&
        description="Number of orientations to average over")
      call define_option_string(opts,&
        "euler-angle-mode",default_euler_angle_mode,&
        description="How to choose Euler angles (sequential, random, file)")
      call define_option_string(opts,&
        "euler-angle-file",default_euler_angle_file,&
        description="Path to a text file containing a list of orientations")
      
      call define_option_real(opts,&
        "ephot-min", default_ephot_min,&
        description="Minimum photon energy for which to calculate the cross sections")
      call define_option_real(opts,&
        "ephot-max", default_ephot_max,&
        description="Maximum photon energy for which to calculate the cross sections")
      call define_option_integer(opts,&
        "nephots", default_nephots,&
        description="Number of photon energies to calculate SED")
      call define_option_real(opts,&
        "dephot", default_dephot,&
        description="Spacing between photon energies")
      
      call define_option_string(opts,&
        "material", default_material,&
        description="Name of grain material &
            &(say 'custom' and use 'material-file=/path/to/file' &
            &to use your own material)")
      call define_option_string(opts,&
        "material-file", default_material_file,&
        description="Name of custom material file")

      call define_option_string(opts,"fftw-optimization",default_fftw_opt_name,required=.false.,&
        description="[estimate,measure,patient,exhaustive]")
      call define_flag(opts,"use-padded-fft",&
        description="Use a slower FFT method that pads a &
        &2d grid to obtain more points in the desired range of scattering angles. &
        &This should only be used for consistency tests.")

      ! Now process command line options and read any specified parameter file

      call process_command_line(opts,ierr)
      if (ierr .ne. 0) then
        write (stderr,*) "ERROR in processing command line arguments." 
        stop
      end if 
      

    
      ! Read parameter file if one is given (don't overwrite command-line options if already given)
      call get_option_string(opts,"parameter-file-name",parameter_file_name)
      if (parameter_file_name .ne. "") then
        call process_input_file(opts,parameter_file_name,ierr,delim_char="=")
        if (ierr .ne. 0) then
          write(stderr,*) "ERROR: Cannot read ",parameter_file_name,". Quitting."
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
      call get_flag(opts,"sed",sed_mode)  
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

      call get_option_real(opts,"ephot-min", ephot_min)
      call get_option_real(opts,"ephot-max", ephot_max)
      call get_option_integer(opts,"nephots", nephots)
      call get_option_real(opts,"dephot", dephot)
      
      call get_option_string(opts,"material",material)
      call get_option_string(opts,"material-file",material_file)
     
      call get_flag(opts,"version",asked_for_version)
      call get_flag(opts,"help",asked_for_help)


      if (asked_for_version) then
        write(stdout,*) VERSION
        write(stdout,*) INFO 
        stop
      end if
      if (asked_for_help) then
        call print_help(opts) 
      end if 

      delm = cmplx(ior_re,ior_im)

      ! Set to lower case
      fftw_optimization_mode_name = strlowcase(fftw_optimization_mode_name)
      euler_angle_mode = strlowcase(euler_angle_mode)
      geometry = strlowcase(geometry)



      if (option_found(opts,"nscatter") .and. option_found(opts, "dtheta")) then
          write(stderr,*) "Warning: You have specified BOTH the 'nscatter' and 'dtheta' options."
          write(stderr,*) "We will use your specified value of nscatter:", nscatter
      else if (option_found(opts,"dtheta")) then
          nscatter = int(ceiling(real(2*max_angle,kind=dp_real)/dtheta))
      end if 

      if (option_found(opts,"dephot") .and. option_found(opts, "nephots")) then
          write(stderr,*) "Warning: You have specified BOTH the 'nephots' and 'dephot' options."
          write(stderr,*) "We will use your specified value of nephots:", nephots
      else if (option_found(opts,"dephot")) then
          nephots = int(ceiling((ephot_max - ephot_min)/dephot))
      else 
          dephot = (ephot_max - ephot_min)/(nephots - 1)
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
      stop
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
      rot_y(1,1) =  cos(eul_ang)
      rot_y(1,2) =  0.0
      rot_y(1,3) =  sin(eul_ang)

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


    subroutine fft_faster(f,kmin,kmax,enhancement, fft_output)
        
        integer, intent(in) :: kmin, kmax, enhancement
        
        complex(kind=dp_complex), intent(inout) :: f(:,:)
        complex(kind=dp_complex), intent(out) :: fft_output(:,:)
        complex(kind=dp_complex), allocatable :: working_fft(:,:)
        complex(kind=dp_complex), dimension(size(f(:,1)), size(f(1,:))) :: new_grid
        integer :: i,j,a,b,norig, fsize, fftsize, ix, iy
        real(kind=dp_real) :: deltax, deltay

        f = kspace_shift(f,real(kmin,kind=dp_real),real(kmin,kind=dp_real)) ! shift the input function so that the k-space origin is at (kmin,kmin)
        norig = kmax - kmin 
        fsize = size(f(:,1))
        fftsize = norig*enhancement + 1
        
        if (fftsize .ne. size(fft_output(1,:))) then
          write(stderr,*) "ERROR: (fft_faster) fft_output size is ",size(fft_output(1,:)),&
                          ", but fftsize = norig*enhancement + 1 = ",fftsize
          stop
        end if 

        if (fsize < norig) then
            write(stderr,*) "ERROR"
            write(stderr,*) "Your chosen value for ngrain is too low to obtain &
                    &high resolution features within the scattering region &
                    &that you've specified. &
                    &Choose an ngrain value of at least",norig
            stop
        end if 
        ! allocate memory
        allocate( working_fft(  norig   ,   norig   ))
        !allocate( fft_output(   fftsize ,   fftsize ))
        

        do i=1,enhancement+1
            deltax = real(i-1)/real(enhancement)
            do j=1,enhancement+1
                deltay = real(j-1)/real(enhancement)
                new_grid = kspace_shift(f,deltax,deltay)
                working_fft = fft_firstk(new_grid,norig)
                do a=1,norig
                    do b=1,norig
                        ix = i + (a-1)*enhancement
                        iy = j + (b-1)*enhancement
                        if ((ix <= fftsize) .and. (iy <= fftsize)) then
                            fft_output(ix, iy) = working_fft(a,b)
                        end if 
                    end do
                end do 
            end do
        end do 
          
        f = kspace_shift(f,real(-kmin,kind=dp_real),real(-kmin,kind=dp_real)) ! shift the input function back.
    end subroutine fft_faster

    function fft_center(f)
        complex(kind=dp_complex), intent(in) :: f(:,:)
        complex(kind=dp_complex), dimension(size(f(:,1)),size(f(1,:))) :: fft_center
        integer :: i, j, n

        n = size(f(:,1))
        do i=1,n
            do j=1,n
                fft_center(i,j) = f(i,j)*(-1)**(i+j)
            end do
        end do 
    end function fft_center

    function kspace_shift(f,shiftx,shifty)
        real(kind=dp_real),  intent(in) :: shiftx,shifty
        complex(kind=dp_complex), intent(in) :: f(:,:)
        complex(kind=dp_complex), dimension(size(f(:,1)),size(f(1,:))) :: kspace_shift
        integer :: i, j, n

        n = size(f(:,1))

        do i=1,n
            do j=1,n
                kspace_shift(i,j) = f(i,j)*exp(ISIGN*CMPLX(0,1)*twopi*(shiftx*(i-1)+shifty*(j-1))/real(n))
            end do
        end do
    end function kspace_shift
    function linterp(x,y,xval)
        implicit none 
        real(kind=dp_real), intent(in) :: x(:), y(:), xval
        real(kind=dp_real) :: linterp
        integer :: i 
        real(kind=dp_real) :: a,b

        if ( xval < x(1) ) then
            write(stderr,*) "ERROR: (linterp) requested value at x=",xval," however, &
                & this is below the minimum value of x=",x(1)
            stop 
        else if ( xval > x(size(x)) ) then 
            write(stderr,*) "ERROR: (linterp) requested value at x=",xval," however, &
                & this is above the maximum value of x=",x(size(x))
            stop 
        end if 


        i=1
        do while( (x(i) < xval) )
            i = i + 1
        end do 

    
        a = xval - x(i-1)
        b = x(i) - x(i-1)

        linterp = y(i-1) + (a/b)*( y(i) - y(i-1) )


    end function linterp


end module common_mod
