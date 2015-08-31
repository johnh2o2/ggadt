module common_mod

  
  use gpfa
  use options
  use constants


  implicit none

  public

  save

  type(options_t) :: opts 

  character(len=100), parameter ::  VERSION="GGADT (General Geometry Anomalous Diffraction Theory) v1.1.2"
  character(len=100), parameter ::  INFO = "Written by John Hoffman (jah5@princeton.edu)&
    & and Michael Tarczon (mtarczon@princeton.edu)"

  complex(kind=dp_complex)               ::  delm 
  
  real(dp_real)                          ::  a_eff     
  real(dp_real)                          ::  ephot 

  character(len=50)                      ::  geometry  
  character(len=50)                      ::  angle_mode
  !character(len=50)                      ::  rotation_axis

  integer                                ::  ngrain
  integer                                ::  nscatter_min 
  integer                                ::  ngrid
  integer                                ::  nscatter
  integer                                ::  norientations 
  integer                                ::  nthreads
  integer                                ::  nphi
  integer                                ::  ierr

  integer, allocatable                   ::  allowed_ngrids(:)

  real(dp_real)                          ::  grid_width  
  real(dp_real)                          ::  dtheta
  real(dp_real)                          ::  ior_im        
  real(dp_real)                          ::  ior_re     
  real(dp_real), dimension(3)            ::  grain_a 
  real(dp_real)                          ::  max_angle

  logical                                ::  use_padded_fft
  logical                                ::  use_efficiencies
  logical                                ::  sang_correction
  logical                                ::  asked_for_version
  logical                                ::  asked_for_help
  logical                                ::  verbose_mode
  logical                                ::  timing_mode
  logical                                ::  material_file_specified
  logical                                ::  force_numerical
  logical                                ::  do_full_2d_fft
  logical                                ::  quiet_mode
  logical                                ::  do_phi_averaging
  logical                                ::  save_shadow_function
  logical                                ::  save_phi_grid
  logical                                ::  grid_width_manually_set


  character(len=200)                     ::  parameter_file_name


! new variables
  integer                                ::  nephots
  real(dp_real)                          ::  ephot_min
  real(dp_real)                          ::  dephot
  real(dp_real)                          ::  ephot_max 
 !character(len=200)                     ::  material          ! name of predefined material
  character(len=200)                     ::  material_file     ! file that defines ior_re and ior_im for a range of energies
  logical                                ::  integrated_mode          
                                                              
!-------

  character(len=200), dimension(max_num_mats)           ::  material_files
  character(len=200), dimension(max_num_mats)           ::  material_tags
  character(len=200)                                    ::  agglom_composition_file
  character(len=200)                                    ::  save_file_root, phi_grid_file, shadow_function_file
  character(len=200)                                    ::  axes_convention
  integer, allocatable                                  ::  agglom_material_tags(:)
  integer                                               ::  nmaterials
  real(dp_real), dimension(max_num_mats)                ::  matfracs
  real(dp_real), dimension(max_num_mats)                ::  ior_res_single_en
  real(dp_real), dimension(max_num_mats)                ::  ior_ims_single_en



  character(len=200)                                    ::  angle_file
  character(len=10)                                     ::  fftw_optimization_mode_name
  character(len=200)                                    ::  agglom_file_name

  integer :: rank_aeff, rank_grain_a1, rank_grain_a2, rank_grain_a3, &
         rank_ephot, rank_use_padded_fft, rank_force_numerical, &
         rank_integrated_mode, rank_use_efficiencies, rank_verbose_mode, &
         rank_timing_mode, rank_ior_re, rank_ior_im, rank_geometry, rank_angle_mode, &
         !rank_rotation_axis, 
         rank_angle_file, rank_ngrain, rank_nscatter, &
         rank_norientations, rank_dtheta, rank_max_angle, rank_agglom_file_name, &
         rank_fftw_optimization_mode_name, rank_ephot_min, rank_ephot_max, rank_nephots, &
         rank_dephot, rank_material_files(max_num_mats), rank_material_tags(max_num_mats), &
         rank_agglom_composition_file, rank_material_file, rank_asked_for_version, &
         rank_asked_for_help, rank_parameter_file_name, rank_axes_convention, rank_do_full_2d_fft, &
         rank_quiet_mode, rank_sang_correction, rank_nthreads, rank_do_phi_averaging, rank_save_shadow_function, &
         rank_save_phi_grid, rank_save_file_root, rank_nphi, rank_grid_width

  contains

    subroutine set_allowed_ngrids()
      implicit none
      integer i, nallowed_gridpoints, allocate_status

      open(unit=file_unit,file=allowed_ngrid_values_filename)

      read(file_unit, *) nallowed_gridpoints

      allocate(allowed_ngrids(nallowed_gridpoints), stat=allocate_status)

      do i=1, nallowed_gridpoints
        read(file_unit, *) allowed_ngrids(i)
      end do

    end subroutine set_allowed_ngrids

    subroutine set_parameter_values()
      implicit none
      character(len=100) :: optname
      integer :: i, p, q, r

      !write(stderr, *) "in set_parameter_values"
      call set_allowed_ngrids()
     
      !write(stderr, *) "called set_allowed_ngrids"

      call define_flag(opts,"help",abbrev='h',description="Print this help message.")
      call define_flag(opts, &
        "version",abbrev='v',&
        description="Print version information")
      call define_flag(opts, &
        "verbose",abbrev='d',&
        description="prints variable values and other status messages")
      call define_flag(opts, &
        "use-efficiencies",abbrev='e',&
        description="use dimensionless cross sections (efficiencies): Q = sigma/(pi*aeff^2)")
      call define_flag(opts, &
        "timing",abbrev='t',&
        description="suppresses output for timing purposes")
      call define_flag(opts, &
        "integrated",&
        description="Generates integrated cross sections (scattering, absorption and extinction) as a function of photon energy")
      call define_flag(opts, &
        "force-numerical",&
        description="Forces GGADT to use a numerical, rather than an analytical, solution for spherical grains.")
      call define_flag(opts, &
        "quiet",abbrev='q',&
        description="Quiet mode; do not print progress")
      call define_flag(opts, &
        "do-full-2d-fft",&
        description="Do not assume axisymmetry (dQ/dOmega is a function of phi)")
      call define_flag(opts, &
        "sang-correction",&
        description="Use a heurstic 1/ngrid correction to the scattering angles")

      !call define_flag(opts, &
      !  "save-phi-grid",&
      !  description="Save phi grid (use save-file-root option)")
      call define_flag(opts, &
        "save-shadow-function",&
        description="Save shadow function (use save-file-root option)")

      call define_flag(opts, &
        "do-phi-averaging",&
        description="Assumes --do-full-2d-fft flag is specified; averages over phi to get 1d dsigma_sca/dOmega")
      

      call define_option_string(opts,&
        "parameter-file","",&
        description="Path to parameter file")
      call define_option_string(opts,&
        "save-file-root","output",&
        description="Root name of output files (if --save-shadow-function flag is used)")

      ! Allow user to specify up to max_num_mats materials:

      do i=1,max_num_mats
        if (i .gt. 10) then 
          write(optname,'(i2)') i 
        else
          write(optname,'(i1)') i 
        end if 
        
        call define_option_string(opts,&
          "material-file"//optname,"",&
          description="Path to material file for material "//optname)
        call define_option_string(opts,&
          "material-tag"//optname,"",&
           description="Tag to associate with material "//optname)
        !call define_option_real(opts,&
       !  "material-frac"//optname,"",&
       !  description="Volume fraction of the grain composed of material "//optname)
      end do 
      

      call define_option_string(opts,&
        "agglom-composition-file","",&
        description="File matching each agglomerate with a material tag")

      call define_option_string(opts,&
        "grain-geometry",default_grain_geometry,&
        description="SPHERES (or AGGLOMERATION), ELLIPSOID, SPHERE, or CUSTOM")
      call define_option_string(opts,&
        "agglom-file",default_agglom_file_name,&
        description="Path to file that defines agglomeration of spheres")

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
        description="real(dp_real) component of the index of refraction, minus 1")
      call define_option_real(opts,&
        "ior-im",default_ior_im,&
        description="Imaginary component of the index of refraction")
      call define_option_real(opts,&
        "dtheta",default_dtheta, &
        description="Angular resolution for dQscatter/dOmega calculation")
      call define_option_real(opts,&
        "grid-width",default_grid_width, &
        description="Parameter to set the width of the grid (in microns). This is automatically set by GGADT otherwise.")

      call define_option_integer(opts,&
        "ngrain",default_ngrain,&
        description="Number of grid points along 1 dimension with which we should resolve the grain")
      call define_option_integer(opts,&
        "nscatter",default_nscatter,&
        description="Number of angles along 1 dimension with which we should calculate the scattering cross section")
      call define_option_integer(opts,&
        "norientations",default_norientations,&
        description="Number of orientations to average over")
      call define_option_integer(opts,&
        "nphi",default_nphi,&
        description="Number of phi angles to do phi averaging over (see --do-phi-averaging)")
      call define_option_integer(opts,&
        "nthreads",default_nthreads,&
        description="For OpenMP: how many threads are used (-1 is default; uses system-specified number of threads)")

      call define_option_string(opts,&
        "angle-mode",default_angle_mode,&
        description="How to choose Euler angles (sequential, random, file)")
      !call define_option_string(opts,&
      !  "rotation-axis",default_rotation_axis,&
      !  description="Sampled orientations will be rotations about this axis (none, xaxis, yaxis, zaxis)")

      call define_option_string(opts,&
        "axes-convention",default_axes_convention,&
        description="'ddscat' (beta,theta,phi) or 'mstm' (alpha,beta,gamma). See documentation for more details.")


      call define_option_string(opts,&
        "angle-file",default_angle_file,&
        description="Path to a text file containing a list of orientations")
      


      call define_option_real(opts,&
        "ephot-min", default_ephot_min,&
        description="Minimum photon energy for which to calculate the cross sections")
      call define_option_real(opts,&
        "ephot-max", default_ephot_max,&
        description="Maximum photon energy for which to calculate the cross sections")
      call define_option_integer(opts,&
        "nephots", default_nephots,&
        description="Number of photon energies for which to calculate integrated cross sections")
      call define_option_real(opts,&
        "dephot", default_dephot,&
        description="Spacing between photon energies")
      
      call define_option_string(opts,&
        "material", default_material,&
        description="Name of material (this will be obselete in future versions). Must be --material='custom'")
      call define_option_string(opts,&
        "material-file", default_material_file,&
        description="Path to file providing energy dependent indices of refraction and &
                  &dielectric constant for the grain material")

      call define_option_string(opts,"fftw-optimization",default_fftw_opt_name,required=.false.,&
        description="[estimate,measure,patient,exhaustive]")
      call define_flag(opts,"use-padded-fft",&
        description="Use a slower FFT method that pads a &
        &2d grid to obtain more points in the desired range of scattering angles. &
        &This should only be used for consistency tests.")

      ! First read in defaults
      call process_input_file(opts,defaults_file_name,ierr,delim_char="=",rank=3)
      if (ierr .ne. 0) then
        write(stderr,*) "ERROR: Cannot read '",trim(defaults_file_name),"'. Quitting."
        stop
      end if 

      ! write(0,*) "Loaded defaults."

      ! Now process command line options in order to read
      call process_command_line(opts,ierr,rank=1)
      if (ierr .ne. 0) then
        write (stderr,*) "ERROR in processing command line arguments." 
        stop
      end if
      ! write(stderr,*) "Processed command line arguments (round 1 of 2)."
      
      ! Read parameter file if one is given (overwrite everything)
      call get_option_string(opts,"parameter-file",parameter_file_name,rank_parameter_file_name)
      if (parameter_file_name .ne. "") then
        call process_input_file(opts,parameter_file_name,ierr,delim_char="=",overwrite="YES",rank=2)
        ! write(stderr,*) "Processed parameter file."
        if (ierr .ne. 0) then
          write(stderr,*) "ERROR: Cannot read ",parameter_file_name,". Quitting."
          stop
        end if 
      end if

      ! re-read command line arguments (to overwrite parameter file arguments)
      call process_command_line(opts,ierr,rank=1)
      ! write(stderr,*) "Processed command line arguments (round 2 of 2)."
      if (ierr .ne. 0) then
        write (stderr,*) "ERROR in processing command line arguments." 
        stop
      end if
      



      !write(stderr, *) "about to read all of the command line"
      ! Set values
      call get_option_real(opts,"aeff",a_eff,rank_aeff)
      call get_option_real(opts,"grain-axis-x",grain_a(1),rank_grain_a1)
      call get_option_real(opts,"grain-axis-y",grain_a(2),rank_grain_a2)
      call get_option_real(opts,"grain-axis-z",grain_a(3),rank_grain_a3)
      call get_option_real(opts,"ephot",ephot,rank_ephot)
      call get_option_real(opts, "dtheta", dtheta, rank_dtheta)
      call get_option_real(opts,"max-angle",max_angle, rank_max_angle)
      call get_option_real(opts,"ior-re",ior_re, rank_ior_re)
      call get_option_real(opts,"ior-im",ior_im, rank_ior_im)
      call get_option_real(opts,"ephot-min", ephot_min, rank_ephot_min)
      call get_option_real(opts,"ephot-max", ephot_max, rank_ephot_max)
      call get_option_real(opts,"dephot", dephot, rank_dephot)
      call get_option_real(opts,"grid-width", grid_width, rank_grid_width)

      call get_flag(opts,"use-padded-fft",use_padded_fft,rank_use_padded_fft)
      call get_flag(opts,"force-numerical",force_numerical,rank_force_numerical)
      call get_flag(opts,"integrated",integrated_mode, rank_integrated_mode) 
      call get_flag(opts,"use-efficiencies",use_efficiencies, rank_use_efficiencies)  
      call get_flag(opts,"verbose",verbose_mode, rank_verbose_mode)
      call get_flag(opts,"timing",timing_mode, rank_timing_mode)
      call get_flag(opts,"do-full-2d-fft",do_full_2d_fft, rank_do_full_2d_fft)
      call get_flag(opts,"quiet",quiet_mode, rank_quiet_mode)
      call get_flag(opts,"sang-correction",sang_correction, rank_sang_correction)
      call get_flag(opts,"do-phi-averaging",do_phi_averaging, rank_do_phi_averaging)
      !call get_flag(opts,"save-phi-grid",save_phi_grid, rank_save_phi_grid)
      call get_flag(opts,"save-shadow-function",save_shadow_function, rank_save_shadow_function)


      call get_option_string(opts,"grain-geometry",geometry, rank_geometry)
      call get_option_string(opts,"axes-convention",axes_convention, rank_axes_convention)
      call get_option_string(opts,"angle-mode",angle_mode, rank_angle_mode)
      !call get_option_string(opts,"rotation-axis",rotation_axis, rank_rotation_axis)
      call get_option_string(opts,"angle-file",angle_file, rank_angle_file)
      call get_option_string(opts,"agglom-file",agglom_file_name, rank_agglom_file_name)
      call get_option_string(opts,"fftw-optimization",fftw_optimization_mode_name, rank_fftw_optimization_mode_name)
      call get_option_string(opts,"save-file-root",save_file_root, rank_save_file_root)


      call get_option_integer(opts,"ngrain",ngrain, rank_ngrain)
      call get_option_integer(opts,"nthreads",nthreads, rank_nthreads)
      call get_option_integer(opts,"nphi",nphi, rank_nphi)
      call get_option_integer(opts,"nscatter",nscatter, rank_nscatter)
      call get_option_integer(opts,"norientations",norientations, rank_norientations)
      call get_option_integer(opts,"nephots", nephots, rank_nephots)

      
      save_phi_grid = .FALSE.

      do i=1,max_num_mats
        if (i .gt. 10) then
          write(optname,'(i2)') i 
        else
          write(optname,'(i1)') i 
        end if 
        
        call get_option_string(opts,"material-file"//optname,material_files(i), rank_material_files(i))
        call get_option_string(opts,"material-tag"//optname,material_tags(i), rank_material_tags(i))

      end do 

      call get_option_string(opts,"agglom-composition-file",agglom_composition_file, rank_agglom_composition_file)
      call get_option_string(opts,"material-file",material_file, rank_material_file)
     
      call get_flag(opts,"version",asked_for_version,rank_asked_for_version)
      call get_flag(opts,"help",asked_for_help,rank_asked_for_help)


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
      angle_mode = strlowcase(angle_mode)
      !rotation_axis = strlowcase(rotation_axis)
      geometry = strlowcase(geometry)
      axes_convention = strlowcase(axes_convention)

      write(phi_grid_file, *) trim(save_file_root)//"_phigrid_o"
      write(shadow_function_file, *) trim(save_file_root)//"_shadow_function_o"

      grid_width_manually_set = option_found(opts, 'grid-width')
      if (grid_width_manually_set) then
        grid_width = grid_width/a_eff
      end if
      ! multiply max_angle by sqrt(2) if using super_fast_fft_mode 
      !   we want to calculate out to (thetax, thetay) = (max_angle, max_angle).
      ! if (super_fast_fft_mode) max_angle = max_angle * sqrt(2.0)


      material_file_specified = option_found(opts,"material-file")
      ! make sure all material files have a corresponding material tag and count number of materials
      nmaterials = 0
      
      do i=1,max_num_mats
        write(optname,'(I1)') i
        if (option_found(opts,"material-file"//optname) .and. .not. &
            option_found(opts,"material-tag"//optname) ) then
          write(stderr,*) "ERROR: material_tags(",i,") defined as '",&
              material_tags(i),"', but no corresponding material file defined."
          stop
        else if (option_found(opts,"material-tag"//optname) .and. .not. &
            option_found(opts,"material-file"//optname) ) then
          write(stderr,*) "ERROR: material_files(",i,") defined as '",&
              material_files(i),"', but no corresponding material tag defined."
          stop
        end if 
        
        if (option_found(opts,"material-file"//optname)) then
          material_file_specified = .TRUE.
          ! can't mix material-file and material-file1,2,... parameters
          if (option_found(opts,"material-file") .and. (rank_material_file .LE. rank_material_files(i))) then 
            write(stderr,*) "ERROR: Cannot use BOTH --material-file and --material-file1,2,.. options"
            stop
          end if 
          nmaterials = nmaterials + 1
        end if 
      end do

      ! if(material_file_specified) write(stderr,*) "material_file_specified..."

      if ( (option_found(opts,"ior-re") .or. option_found(opts,"ior-im")) .and. material_file_specified) then
        if (min(rank_ior_re,rank_ior_im) == min(rank_material_file,minval(rank_material_files))) then
          write(stderr,*) "Warning: you have specified BOTH a manual index of refraction value and "
          write(stderr,*) "   a material file. We will use the material file by default."
        else if (min(rank_ior_re,rank_ior_im) < min(rank_material_file,minval(rank_material_files))) then 
          material_file_specified = .FALSE.
        end if 
      end if 
     

      ! If multiple compositions specified, but the grain isn't an agglomerate
      if ( nmaterials .gt. 0 .and. .not. (geometry == "spheres" .or. geometry == "agglomerate")) then
        write(stderr,*) "Warning: you have specified more than one material file, but the grain"
        write(stderr,*) "  geometry is ",geometry," not 'spheres'/'agglomerate'. Multiple compositions is"
        write(stderr,*) "  currently only supported for agglomerate grains."
      end if 
      ! check that the user didn't forget to provide the agglom-composition-file
      if ( nmaterials .gt. 0 .and. .not. option_found(opts,"agglom-composition-file")) then
        write(stderr, *) "ERROR: agglom-composition-file parameter is not set, but material-file1 and material-tag1 are."
        stop
      end if 
      
      ! Some book keeping if we're just using a single composition
      if (nmaterials == 0 .and. material_file_specified) then 
        material_files(1) = material_file 
        nmaterials = 1
      else if (nmaterials == 0) then
        ! Otherwise use manually supplied index of refraction
        ! write(stderr,*) "manual ior..."
        material_file_specified = .FALSE.
        material_files(1) = ""
        material_file = ""
        nmaterials = 1

      end if 




      ! Resolve parameter conflicts

      ! irrelevant nphi specification
      if (do_phi_averaging .and. (.not. do_full_2d_fft .or. integrated_mode)) then
        write(stderr, *) "ERROR: --do-phi-averaging is specified, but not --do-full-2d-fft"
        stop
      end if

      ! irrelevant nphi specification
      if (option_found(opts,'nphi') .and. (.not. do_full_2d_fft .or. integrated_mode)) then
        if (.not. do_full_2d_fft) then
          write(stderr, *) "Warning: nphi specified, but --do-full-2d-fft is not. Ignoring nphi."
        else
          write(stderr, *) "Warning: nphi specified, but we're doing integrated cross sections! Ignoring nphi."
        end if
      end if

      ! output where you're saving the phi_grid and shadow function
      if (save_shadow_function) then
        write(stderr, *) "Note: Will be saving shadow function grid to ", trim(shadow_function_file)//"<orientation>.dat"
      end if


      ! nscatter and dtheta
      if (option_found(opts,"nscatter") .and. option_found(opts, "dtheta")) then
        if (rank_nscatter == rank_dtheta) then
          write(stderr,*) "Warning: You have specified BOTH the 'nscatter' and 'dtheta' options."
          write(stderr,*) "We will use your specified value of nscatter:", nscatter
        else if (rank_dtheta < rank_nscatter .or. rank_nscatter == 4) then
          nscatter = int(ceiling(real(2*max_angle,dp_real)/dtheta))
        end if
      else if (option_found(opts,"dtheta")) then
          nscatter = int(ceiling(real(2*max_angle,dp_real)/dtheta))
      end if 

      ! nephots and dephot
      if (option_found(opts,"dephot") .and. option_found(opts, "nephots")) then
        if (rank_dephot == rank_nephots) then
          write(stderr,*) "Warning: You have specified BOTH the 'nephots' and 'dephot' options."
          write(stderr,*) "We will use your specified value of nephots:", nephots
        else if (rank_dephot < rank_nephots .or. rank_nephots == 4) then 
          nephots = int(ceiling((ephot_max - ephot_min)/dephot))
        else 
          dephot = (ephot_max - ephot_min)/(nephots - 1)
        end if 
      else if (option_found(opts,"dephot")) then
          nephots = int(ceiling((ephot_max - ephot_min)/dephot))
      else 
          dephot = (ephot_max - ephot_min)/(nephots - 1)
      end if 

      ! If index of refraction is defined manually (ior-re and ior-im), and ephot/material-file
      ! options are also specified, go with the material file.
      if ((option_found(opts,"ephot") .and. option_found(opts,"material-file")) .and. &
            & (option_found(opts, "ior-re") .or. option_found(opts,"ior-im") )) then

          if (min(rank_ephot, rank_material_file) == min(rank_ior_re, rank_ior_im) .and. rank_ephot .lt. 4) then
            write(stderr,*) "Warning: You have specified BOTH 'ephot'/'material-file' and 'ior-re'/'ior-im' parameters."
            write(stderr,*) "We will pull the index of refraction value from material-file=",material_file
            write(stderr,*) "at a photon energy of ephot=",ephot,"keV."
          else if (min(rank_ior_re,rank_ior_im) < min(rank_ephot,rank_material_file)) then
            material_file_specified = .FALSE.
            material_file=""
            material_files(1)=""
          else 
            ior_re = 0.0
            ior_im = 0.0
          end if 
      end if 

      ! make sure the axes_convention is an allowable value

      if (.not. (axes_convention == 'ddscat' .or. axes_convention == 'mstm')) then
        write(stderr, *) "ERROR: '",axes_convention,"' is not one of 'ddscat' or 'mstm'"
        stop
      end if 

      !if (.not. (rotation_axis == 'none')) then
      !  write(stderr, *) "WARNING: the current implementation of the rotation-axis option is faulty."
      !  write(stderr, *) "    Future versions will have rotation and precession axis options."
      !end if 
      if (.not. integrated_mode) then
          nephots = 1
          dephot = 0.0

          if (verbose_mode) then 
            write(stderr,*) "GGADT will now calculate the DIFFERENTIAL SCATTERING CROSS SECTION"
            write(stderr,*) "at a single energy for a single dust grain with a geometry of ", geometry
            write(stderr,*) ""
            write(stderr,*) "If instead you wish to calculate the energy dependent integrated cross sections,"
            write(stderr,*) "pass the '--integrated' flag (or set integrated=T in your parameter file)."
          end if
      else
         if (verbose_mode) then 
            write(stderr,*) "GGADT will now calculate the INTEGRATED (ABS,SCA,EXT) CROSS SECTIONS"
            write(stderr,*) "for a single dust grain with a geometry of ", geometry
            write(stderr,*) "over the energy range ", ephot_min," - ",ephot_max, " keV in ",dephot," keV increments"
            write(stderr,*) "for a dust grain composed of ",nmaterials," materials:"
            do i=1,nmaterials
              write(stderr,*) " + ", material_files(i)

            end do
            write(stderr,*) 
            write(stderr,*) ""
            write(stderr,*) "If instead you wish to calculate the differential scattering cross section,"
            write(stderr,*) "do not pass the '--integrated' flag (or set integrated=F in your parameter file)."
          end if

      end if 

    end subroutine set_parameter_values

    subroutine print_help(opts)
      type(options_t), intent(in) :: opts
      write (*,'(a)') "ggadt: Computes orientationally averaged scattering and absorption&
        &quantities for a user-defined dust grain."
      write (*,'(a)') "Usage: ggadt [options]"
      write (*,'(a)') ""
      write (*,'(a)') "Options:"
      call print_options(opts)
      stop
    end subroutine print_help


    function rotation_matrix(beta,theta,phi)
      ! Rotation matrix for a set of Euler angles -- for some reason this function does not work!
      ! (We use rot_x*rot_y*rot_z instead.) It's a low-priority bug.
      ! 8.28.14 -- jah updated this function. Should work now! Has not been tested.
      implicit none
      real(dp_real), intent(in) :: beta,theta,phi
      real(dp_real), dimension(3,3) :: rotation_matrix

      rotation_matrix(1,1) = cos(theta)
      rotation_matrix(1,2) = -cos(beta)*sin(theta)
      rotation_matrix(1,3) =  sin(beta)*sin(theta)

      rotation_matrix(2,1) =  cos(phi)*sin(theta)
      rotation_matrix(2,2) =  cos(beta)*cos(phi)*cos(theta) - sin(beta)*sin(phi)
      rotation_matrix(2,3) = -cos(phi)*cos(theta)*sin(beta) - cos(beta)*sin(phi)

      rotation_matrix(3,1) = sin(phi)*sin(theta)
      rotation_matrix(3,2) = cos(phi)*sin(beta) + cos(beta)*cos(theta)*sin(phi)
      rotation_matrix(3,3) = cos(beta)*cos(phi) - cos(theta)*sin(beta)*sin(phi)
    end function rotation_matrix

    function rot_x(angles)
      implicit none
      real(dp_real), intent(in) :: angles
      real(dp_real), dimension(3,3) :: rot_x
      rot_x(1,1) = 1.0
      rot_x(1,2) = 0.0
      rot_x(1,3) = 0.0

      rot_x(2,1) =  0.0
      rot_x(2,2) =  cos(angles)
      rot_x(2,3) = -sin(angles)

      rot_x(3,1) =  0.0
      rot_x(3,2) =  sin(angles)
      rot_x(3,3) =  cos(angles)
    end function rot_x 

    function rot_y(angles)
      implicit none
      real(dp_real), intent(in) :: angles
      real(dp_real), dimension(3,3) :: rot_y
      rot_y(1,1) =  cos(angles)
      rot_y(1,2) =  0.0
      rot_y(1,3) =  sin(angles)

      rot_y(2,1) =  0.0
      rot_y(2,2) =  1.0
      rot_y(2,3) =  0.0

      rot_y(3,1) = -sin(angles)
      rot_y(3,2) =  0.0
      rot_y(3,3) =  cos(angles)
    end function rot_y

    function rot_z(angles)
      implicit none
      real(dp_real), intent(in) :: angles
      real(dp_real), dimension(3,3) :: rot_z
      rot_z(1,1) = cos(angles)
      rot_z(1,2) = sin(angles)
      rot_z(1,3) = 0.0

      rot_z(2,1) = -sin(angles)
      rot_z(2,2) =  cos(angles)
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

    subroutine fft1d_faster(f,kmin,kmax,enhancement, fft1d_output)
        ! Performs an efficient series of fft1d's to suit our needs
        ! Astrophysical applications of ADT for x-ray scattering of dust
        !   typically care about small angle scattering, which corresponds to
        !   small values of K in the fourier transform. To get better resolution,
        !   one can either pad the fft1d, OR, one can do a series of offset fft1d's.
        !   The latter approach takes about a 10th of the time as the former approach.
        
        integer, intent(in) :: kmin, kmax, enhancement
        
        complex(kind=dp_complex), intent(inout) :: f(:)
        complex(kind=dp_complex), intent(out) :: fft1d_output(:)
        complex(kind=dp_complex), allocatable :: working_fft1d(:)
        complex(kind=dp_complex), dimension(size(f)) :: new_grid
        integer :: i,j,a,b,norig, fsize, fft1dsize, ix, iy
        real(dp_real) :: delta

        fsize = size(f)
        !write(stderr, *) "in fft1d_faster"
        ! shift the input function so that the k-space origin is at (kmin,kmin)
        f = kspace_shift_1d(f,real(kmin,dp_real)) 
        !write(stderr, *) "kspace shifted"
        ! do a vanilla fft1d if enhancement is 0
        if (enhancement .eq. 0) then
          allocate(working_fft1d(size(f)))
          working_fft1d = fft1d(f)

          !fft1d_output = fft1d_firstk(f, size(fft1d_output(:,1)))
          !write(stderr, *) fsize, size(fft1d_output(:,1))
          do i=1,size(fft1d_output)
            fft1d_output(i) = working_fft1d(i)
          end do
        else
          norig = kmax - kmin 
          
          fft1dsize = norig*(enhancement + 1)

          if (fft1dsize .ne. size(fft1d_output)) then
            write(stderr,*) "ERROR: (fft1d_faster) fft1d_output size is ",size(fft1d_output),&
                            ", but fft1dsize = norig*(enhancement + 1) = ",fft1dsize
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
          allocate( working_fft1d(  norig   ))

          ! Do offset fft1ds

          !-$omp parallel shared(fft1d_output, enhancement, norig, fft1dsize)
          !-$omp do schedule(dynamic) private(i, delta, new_grid, working_fft1d, a, ix)
          do i=0,enhancement
              delta = real(i)/real(enhancement + 1)
              !write(stderr,*) i, delta
              new_grid = kspace_shift_1d(f,delta)
              !write(stderr,*) "about to make working fft1d"
              working_fft1d = fft1d_firstk(new_grid,norig)
                  
              do a=1,norig
                  
                ix = i + (a-1)*(enhancement + 1) + 1
                
              
                if (ix <= fft1dsize) then
                    !-$OMP CRITICAL
                    fft1d_output(ix) = working_fft1d(a)
                    !-$OMP END CRITICAL 
                end if  
              end do 
          end do 
          !-$omp end do
          !-$omp end parallel 
        end if
        

        ! shift the input function back.
        f = kspace_shift_1d(f,real(-kmin,dp_real)) 
    end subroutine fft1d_faster
    function fft1d_firstk(f,K)
        ! How this calculation works:
        !   Say I have 15 data points, in one dimension, for which I only care about the first 5 values of its fft1d.
        !
        !   We can calculate these first five k values by doing 15/5 = 3 fft1d's of length 5.
        !
        !       Original data: dddddddddddddddd
        !
        !       fft1d1 = fft1d( d--d--d--d--d--d )
        !       fft1d2 = fft1d( -d--d--d--d--d-- )
        !       fft1d3 = fft1d( --d--d--d--d--d- )
        !
        !       fft1d(orig)(k) = sum_i(fft1di(k))*twid_factor(k)
        ! 

        integer, intent(in)                                 :: K
        integer                                             :: lentwids, keff, allocation_status
        complex(kind=dp_complex), intent(inout)             :: f(:)
        complex(kind=dp_complex), dimension(K)              :: fft1d_firstk 
        complex(kind=dp_complex), allocatable               :: small_fft1d(:), twids(:), fft1d_firstk_working(:)

        integer                                             :: N, Nfft1d, i,l

        keff    = k
        N       = size(f)
        Nfft1d    = N/Keff

        ! Make sure N is divisible by K.
        !    if not, we take the first K+n values of the fft1d 
        !    and throw away n of them (n is the smallest integer such that K+n | N)
        do while ((.not. keff*Nfft1d .eq. N) .and. keff < N )
            keff = keff + 1
            Nfft1d = N/Keff 
        end do 

        lentwids = N

        ! allocate twids, small_fft1d and fft1d_firstk_working arrays
        !   twids               -- contain twiddle factors needed to combine the different fft1d calculations
        !   small_fft1d           -- the array in which each fft1d is temporarily stored
        !   fft1d_firstk_working  -- array of length Keff >~ K to which FFT values are stored.

        allocate(twids(lentwids), stat = allocation_status)
        if (allocation_status/=0) then
            write (0,*) "ERROR: Cannot allocate twids array (fft1dwmod)"
            stop
        end if 
        allocate(small_fft1d(keff), stat = allocation_status)
        if (allocation_status/=0) then
            write (0,*) "ERROR: Cannot allocate small_fft1d array (fft1dwmod)"
            stop
        end if 
        allocate(fft1d_firstk_working(keff), stat = allocation_status)
        if (allocation_status/=0) then
            write (0,*) "ERROR: Cannot allocate fft1d_firstk_working array (fft1dwmod)"
            stop
        end if 

        ! Initialize to zero
        do i=1,keff
            fft1d_firstk_working(i) = 0.0D0 
        end do 

        ! 
        if (Keff*Nfft1d .ne. N) then
            write(0,*) "ERROR: Nfft1d = ",Nfft1d," and K = ",K,", but Nfft1d*K = ",Nfft1d*K," != ",N
            stop 
        endif

        ! Calculate twiddle factors
        do i=0,(keff-1)
            do l=0,(Nfft1d-1)    
                twids(1 + i + l*Keff) = zexp(CMPLX(0,1)*ISIGN*twopi*(real(i*l))/real(N))
            end do 
        end do

        do l=1,Nfft1d
            ! execute plan
            ! write(stderr, *) "about to do fft1d() call.", l, N, Nfft1d
            small_fft1d = fft1d(f(l:N:Nfft1d))
            ! transfer results to master array
            do i=1,Keff
                fft1d_firstk_working(i) = fft1d_firstk_working(i) + small_fft1d(i)*twids( i+(l-1)*Keff )
            end do 
        end do
        
        ! only get the K values that we care about
        fft1d_firstk = fft1d_firstk_working(:K)

        deallocate(twids)
        deallocate(small_fft1d)
        deallocate(fft1d_firstk_working)
    end function fft1d_firstk

    function fft1d_center(f)
        ! Multiplies a 1d array by a series of twiddle factors so that 
        !        \hat{f*twids}_0, \hat{f*twids}_{last} ~ \hat{f}(-K), \hat{f}(K)
        complex(kind=dp_complex), intent(in) :: f(:)
        complex(kind=dp_complex), dimension(size(f)) :: fft1d_center
        integer :: i, j, n

        n = size(f)
        do i=1,n  
          fft1d_center(i) = f(i)*(-1)**(i+1)
        end do 
    end function fft1d_center

    function kspace_shift_1d(f,shift)
        ! Multiplies a 1d array by a series of twiddle factors so that \hat{f*twids}_m = \hat{f}_{m + shift}
        real(dp_real),  intent(in) :: shift
        complex(kind=dp_complex), intent(in) :: f(:)
        complex(kind=dp_complex), dimension(size(f)) :: kspace_shift_1d
        integer :: i, j, n

        n = size(f)

        do i=1,n
          kspace_shift_1d(i) = f(i)*exp(ISIGN*CMPLX(0,1.0, kind=dp_complex)*twopi*(shift*(i-1))/real(n))
        end do
    end function kspace_shift_1d



    subroutine fft2d_faster(f,kmin,kmax,enhancement, fft2d_output)
        ! Performs an efficient series of FFT's to suit our needs
        ! Astrophysical applications of ADT for x-ray scattering of dust
        !   typically care about small angle scattering, which corresponds to
        !   small values of K in the fourier transform. To get better resolution,
        !   one can either pad the FFT, OR, one can do a series of offset FFT's.
        !   The latter approach takes about a 10th of the time as the former approach.
        
        integer, intent(in) :: kmin, kmax, enhancement
      
        complex(kind=dp_complex), intent(inout) :: f(:,:)
        complex(kind=dp_complex), intent(out) :: fft2d_output(:,:)
        complex(kind=dp_complex), allocatable :: working_fft2d(:,:)
        complex(kind=dp_complex), dimension(size(f(:,1)), size(f(1,:))) :: new_grid
        integer :: i,j,a,b,norig, fsize, fft2dsize, ix, iy
        real(dp_real) :: deltaz, deltay
        
        fsize = size(f(:,1))

        ! shift the input function so that the k-space origin is at (kmin,kmin)
        f = kspace_shift_2d(f,real(kmin,dp_real),real(kmin,dp_real)) 

        ! do a vanilla FFT if enhancement is 0
        if (enhancement .eq. 0) then
          allocate(working_fft2d(size(f(1,:)), size(f(:,1))))
          working_fft2d = fft2d(f)

          !fft_output = fft_firstk(f, size(fft_output(:,1)))
          !write(stderr, *) fsize, size(fft_output(:,1))

          do i=1,size(fft2d_output(1,:))
            do j=1, size(fft2d_output(:,1))
              fft2d_output(i, j) = working_fft2d(i, j)
            end do
          end do
        else
          norig = kmax - kmin 
          
          fft2dsize = norig*(enhancement + 1)

          if (fft2dsize .ne. size(fft2d_output(1,:))) then
            write(stderr,*) "ERROR: (fft_faster) fft_output size is ",size(fft2d_output(1,:)),&
                            ", but fftsize = norig*(enhancement + 1) = ",fft2dsize
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
          allocate( working_fft2d(  norig   ,   norig   ))
          
          ! Do offset FFTs

          ! TODO: Parallelize!! 
          !-$omp parallel shared(fft2d_output, enhancement, norig, fft2dsize)
          !-$omp do schedule(dynamic) private(i,j, deltaz, deltay, new_grid, working_fft2d, a, b, ix, iy)
          do i=0,enhancement
              deltaz = real(i)/real(enhancement + 1)
              do j=0,enhancement
                  deltay = real(j)/real(enhancement + 1)
                  
                  new_grid = kspace_shift_2d(f,deltaz,deltay)
                  !write(stderr, *) "about to do fft2d_firstk!!"
                  working_fft2d = fft2d_firstk(new_grid,norig)
                  !write(stderr, *) "done!"
                  
                  do a=1,norig
                      do b=1,norig
                          ix = i + (a-1)*(enhancement + 1) + 1
                          iy = j + (b-1)*(enhancement + 1) + 1
                        
                          if ((ix <= fft2dsize) .and. (iy <= fft2dsize)) then
                              !-$OMP CRITICAL
                              fft2d_output(ix, iy) = working_fft2d(a,b)
                              !-$OMP END CRITICAL
                          end if 
                      end do
                  end do 
              end do
          end do 
          !-$omp end do
          !-$omp end parallel 

          
        end if

        ! shift the input function back.
        f = kspace_shift_2d(f,real(-kmin,dp_real),real(-kmin,dp_real)) 
    end subroutine fft2d_faster
    function fft2d_firstk(f,K)
        ! How this calculation works:
        !   Say I have 15 data points, in one dimension, for which I only care about the first 5 values of its (1d) fft.
        !
        !   We can calculate these first five k values by doing 15/5 = 3 fft's of length 5.
        !
        !       Original data: dddddddddddddddd
        !
        !       fft2d1 = fft2d( d--d--d--d--d--d )
        !       fft2d2 = fft2d( -d--d--d--d--d-- )
        !       fft2d3 = fft2d( --d--d--d--d--d- )
        !
        !       fft2d(orig)(k) = sum_i(fft2di(k))*twid_factor(k)
        ! 

        integer, intent(in)                                 :: K
        integer                                             :: lentwids, keff, allocation_status
        complex(kind=dp_complex), intent(inout)             :: f(:,:)
        complex(kind=dp_complex), dimension(K,K)            :: fft2d_firstk 
        complex(kind=dp_complex), allocatable               :: small_fft2d(:,:), twids(:,:), fft2d_firstk_working(:,:)

        integer                                             :: N, Nfft2d, i,j,l,m 

        keff    = k
        N       = size(f(:,1))
        Nfft2d    = N/Keff

        ! Make sure N is divisible by K.
        !    if not, we take the first K+n values of the fft2d 
        !    and throw away n of them (n is the smallest integer such that K+n | N)
        do while ((.not. keff*Nfft2d .eq. N) .and. keff < N )
            keff = keff + 1
            Nfft2d = N/Keff 
        end do 

        lentwids = N

        ! allocate twids, small_fft2d and fft2d_firstk_working arrays
        !   twids               -- contain twiddle factors needed to combine the different fft2d calculations
        !   small_fft2d           -- the array in which each fft2d is temporarily stored
        !   fft2d_firstk_working  --

        allocate(twids(lentwids, lentwids), stat = allocation_status)
        if (allocation_status/=0) then
            write (0,*) "ERROR: Cannot allocate twids array (fft2dwmod)"
            stop
        end if 
         
        allocate(fft2d_firstk_working(keff, keff), stat = allocation_status)
        if (allocation_status/=0) then
            write (0,*) "ERROR: Cannot allocate fft2d_firstk_working array (fft2dwmod)"
            stop
        end if 

        do i=1,keff
            do j=1,keff
                fft2d_firstk_working(i,j) = 0.0D0
            end do 
        end do 

        if (Keff*Nfft2d .ne. N) then
            write(0,*) "ERROR: Nfft2d = ",Nfft2d," and K = ",K,", but Nfft2d*K = ",Nfft2d*K," != ",N
            stop 
        endif

        

        ! Calculate twiddle factors

        !$omp parallel shared(keff, Nfft2d, twids, N)
        !$omp do schedule(dynamic) private(i,j, l, m)
        do i=0,(keff-1)
            do j=0,(keff-1)
                do l=0,(Nfft2d-1)
                    do m=0,(Nfft2d-1)
                        !-$OMP CRITICAL
                        twids(1 + i + l*Keff, 1 + j + m*Keff ) &
                            & = zexp(CMPLX(0,1)*ISIGN*twopi*(real(i*l+j*m))/real(N))
                        !-$OMP END CRITICAL
                    end do 
                end do 
            end do 
        end do
        !$omp end do
        !$omp end parallel 
        

        ! TODO: Parallelize this loop and the similar one in fft1d_firstk
        !       (This has been giving me a whole bunch of grief and I don't know why!!)

        !-$omp parallel do schedule(static) &
        !-$omp shared(Nfft2d,N, f, Keff, fft2d_firstk_working, twids) &
        !-$omp private(l, m, i, j, small_fft2d, allocation_status)
        do l=1,Nfft2d
            allocate(small_fft2d(Keff, Keff), stat = allocation_status)
            if (allocation_status/=0) then
                write (0,*) "ERROR: Cannot allocate small_fft2d array (fft2dwmod)"
                stop
            end if
            do m=1,Nfft2d

                ! execute plan
                small_fft2d = fft2d(f(l:N:Nfft2d,m:N:Nfft2d))

                ! transfer results to master array
                do i=1,Keff
                    do j=1,Keff 
                        !-$OMP CRITICAL
                        fft2d_firstk_working(i,j) = fft2d_firstk_working(i,j) &
                                            & + small_fft2d(i,j)*twids( i+(l-1)*Keff , j+(m-1)*Keff )
                        !-OMP END CRITICAL
                    end do 
                end do 
            end do 
            deallocate(small_fft2d)
        end do
        !-$omp end parallel do
        
        ! only get the K values that we care about
        fft2d_firstk = fft2d_firstk_working(:K,:K)

        deallocate(twids)
        deallocate(fft2d_firstk_working)
    end function fft2d_firstk

    function fft2d_center(f)
        complex(kind=dp_complex), intent(in) :: f(:,:)
        complex(kind=dp_complex), dimension(size(f(:,1)),size(f(1,:))) :: fft2d_center
        integer :: i, j, n

        n = size(f(:,1))
        do i=1,n
            do j=1,n
                fft2d_center(i,j) = f(i,j)*(-1)**(i+j)
            end do
        end do 
    end function fft2d_center

    function kspace_shift_2d(f,shiftz,shifty)
        real(dp_real),  intent(in) :: shiftz,shifty
        complex(kind=dp_complex), intent(in) :: f(:,:)
        complex(kind=dp_complex), dimension(size(f(:,1)),size(f(1,:))) :: kspace_shift_2d
        integer :: i, j, n

        n = size(f(:,1))
        !-$OMP PARALLEL SHARED(kspace_shift_2d, f, shiftz, shifty, n)
        !-$OMP DO SCHEDULE(DYNAMIC) PRIVATE(i, j)
        do i=1,n
            do j=1,n
                !-$OMP CRITICAL
                kspace_shift_2d(i,j) = f(i,j)*exp(ISIGN*CMPLX(0,1.0, kind=dp_complex)*twopi*(shiftz*(i-1)+shifty*(j-1))/real(n))
                !-$OMP END CRITICAL
            end do
        end do
        !-$omp end do
        !-$omp end parallel 

    end function kspace_shift_2d
    

    

    subroutine convert_mstm_to_ddscat(angles)
     implicit none
     real(dp_real), dimension(3), intent(inout) :: angles
     real(dp_real), dimension(3) :: mstm

     mstm(:) = angles(:)

     if (cos(mstm(1))*sin(mstm(2)) == -1.) then 
      angles(1) = mstm(3)
      angles(2) = pi 
      angles(3) = pi/2.0
     else if (cos(mstm(1))*sin(mstm(2)) == 1.) then 
      angles(1) = -mstm(3)
      angles(2) = 0.0
      angles(3) = pi/2.0
     else 
      angles(2) = acos(cos(mstm(1))*sin(mstm(2)))
      angles(1) = acos(-sin(mstm(1))*sin(mstm(2))/sin(angles(2)))
      angles(3) = acos((cos(mstm(1))*cos(mstm(2))*cos(mstm(3)) - sin(mstm(1))*sin(mstm(3)))/sin(angles(2)))
      if (cos(mstm(2)) < 0) then 
        angles(1) = -angles(1)
      end if 
      if (cos(mstm(1))*cos(mstm(2))*sin(mstm(3)) + sin(mstm(1))*cos(mstm(3)) < 0.0) then
        angles(3) = -angles(3)
      end if 
     end if 
     


    end subroutine convert_mstm_to_ddscat


end module common_mod