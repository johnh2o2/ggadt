program ggadt

    use sphere
    use spheres
    use ellipsoid
    use common_mod
    use constants
    use interp
    use custom 

    use omp_lib

    implicit none

    ! Declare variables...
    real(kind=dp_real), allocatable :: z(:), y(:), kz(:), ky(:), thetaz(:), thetay(:)

    complex(kind=dp_complex), allocatable :: sh(:,:), ftsh(:,:), phi_total(:,:), gsh(:), gftsh(:)

    real(kind=dp_real), allocatable :: scatter(:,:), gscatter(:)
    real(kind=dp_real), allocatable :: ephots(:), ior_res(:,:), ior_ims(:,:)
    character(len=5) :: orient_char


    logical :: multiple_compositions
    real(kind=dp_real) :: zmin, zmax, ymin, ymax, dz, dy, k, l, dk, norm_const
    real(kind=dp_real), dimension(3) :: angles
    real(kind=dp_real), dimension(3,3) :: rm 

    integer :: kactual, kmin, kmax, enhancement

    ! Set parameter values
    call set_parameter_values()

    ! Now initialize/allocate arrays that need to be allocated
    call initialize_and_allocate_vars()
   
    
    !call set_optimization_mode(fftw_optimization_mode_name)

    ! This prints the parameters to stdout; if you're running ggadt > output.dat, this is effectively a header
    call print_parameters(stdout)
    
    ! If you're in verbose mode, prints the parameters to stderr so that you see them.
    if (verbose_mode) call print_parameters(stderr)


    if (integrated_mode) then 
        ! Integrated mode calculates total cross sections
        call calculate_int_cross_sections()
    else
        ! Else, you're generating the differential scattering cross section
        call diff_scat_cross_sect()
    end if 

    ! Deallocates things.
    call cleanup()

    if (verbose_mode) write(stderr,*) "Done."
contains

    subroutine calculate_int_cross_sections()
        ! Driver subroutine; calling this subroutine will generate total cross sections and print them out.
        implicit none
        real(kind=dp_real),dimension(size(ephots)) :: sigma_scats, sigma_abss
        integer :: i

        call get_integrated_cross_sections(sigma_scats, sigma_abss)
        do i=1,nephots
            write(stdout,*) ephots(i), sigma_scats(i), sigma_abss(i), sigma_scats(i) + sigma_abss(i) 
        end do
    end subroutine calculate_int_cross_sections

    subroutine get_ior(matfile, iorres, iorims, iorre, iorim)
        ! Reads in index file (matfile); stores index of refraction in 
        !       * iorres, iorims (arrays; you need these for total cross section calculations at different energies)
        !       * iorre, iorim (scalars; you only need one value for the IOR when computing diff scat cross section.)

        implicit none
        character(len=200), intent(in) :: matfile 
        character(len=200) :: mat, junk 
        real(kind=dp_real), intent(out) :: iorres(:), iorims(:)
        real(kind=dp_real), intent(out) :: iorre, iorim
        real(kind=dp_real) :: eps, imeps
        integer  :: i,j, allocatestatus, ICOMP, niors, counter, ios, maxrecs
        real(kind=dp_real), allocatable  :: file_ior_re(:), file_ior_im(:), file_ephots(:)

        ! Max records; just an arbitrary number. Change if needed
        maxrecs = 100000

        open(unit=1,file=matfile) ! open index file.

        ! Read header
        read(1,'(i2,a8,a64)') ICOMP, junk, mat

        ! Print header (if verbose mode is set)
        if (verbose_mode) write(stderr,*) "# Material file: '",trim(adjustl(matfile)),"'"
        if (verbose_mode) write(stderr,*) "# Material: '",trim(adjustl(mat)),"'"
        
        ! Now get the LENGTH of the file
        read(1,*) junk 
        niors = 0
        do i=1,maxrecs
            READ(1,*,IOSTAT=ios) junk 
            IF (ios /= 0) EXIT 
            IF (J == maxrecs) THEN 
                write(stderr,*) "ERROR (get_ior): Maximum number of records exceeded..."
                STOP 
            ENDIF 
            niors = niors + 1
        end do  

        ! Allocate things
        allocate(file_ior_re(niors),stat=allocatestatus)
                if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (file_ior_re) array ***"
        allocate(file_ior_im(niors),stat=allocatestatus)
                if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (file_ior_im) array ***"
        allocate(file_ephots(niors),stat=allocatestatus)
                if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (file_ephots) array ***"

        ! Rewind back to the beginning
        REWIND(1) 

        ! Read past the header
        read(1,*) junk 
        read(1,*) junk 

        ! Now read in the index file.
        do i=1,niors
            read(1,*) file_ephots(i), file_ior_re(i), file_ior_im(i), eps, imeps
            file_ephots(i) = file_ephots(i) / 1000.0 ! convert from eV to keV
        end do

        ! Get the scalar values (only necessary for differential scattering cross section mode)
        iorre = linterp(file_ephots,file_ior_re,ephot)
        iorim = linterp(file_ephots,file_ior_im,ephot)


        if (integrated_mode) then 
            ! Store index of refraction into array
            do i=1,nephots
                iorres(i) = linterp(file_ephots,file_ior_re,ephots(i))
                iorims(i) = linterp(file_ephots,file_ior_im,ephots(i))
            end do 
        else
            ! Just store one value (this is probably unnecessary...)
            iorres(1) = iorre
            iorims(1) = iorim
        end if 

        ! close the file!
        close(1)
    end subroutine get_ior

    subroutine get_integrated_cross_sections(sigma_scats,sigma_abss)
        ! Obtain total cross sections as a function of energy

        implicit none
        real(kind=dp_real) :: sigma_scat, sigma_abs, energy
        real(kind=dp_real), dimension(size(ephots)), intent(out) :: sigma_scats, sigma_abss
        complex(kind=dp_complex) :: delm
        complex(kind=dp_complex), dimension(10) :: delms

        real(kind=dp_real) :: kphot, sigma_ext
        integer :: c1,c2, nangle, i, j

        ! initialize cross sections to zero.
        do i=1,nephots
            sigma_scats(i) = 0.0
            sigma_abss(i) = 0.0
        end do 

        ! Normalize depending on whether we're outputting efficiencies (= C/pi*a^2) or simply the cross sections (C)
        if (use_efficiencies) then 
            norm_const = 1.0/(pi*a_eff*a_eff)
            write(stdout,*) "# [ephot] [Q_scat] [Q_abs] [Q_ext]"
        else 
            norm_const = 1.0
            write(stdout,*) "# [ephot] [sigma_scat] [sigma_abs] [sigma_ext]"
        end if 



        ! loop over orientations        
        do nangle = 1,norientations
            ! fetch Euler angles
            angles = get_new_angles(nangle)

            ! calculate rotation matrix corresponding to rotation angles
            ! 
            ! * angles(1) -- beta
            ! * angles(2) -- theta
            ! * angles(3) -- phi
            !
            ! ^ where beta, theta, phi refer to the DDSCAT angle convention (http://www.ddscat.org/)
            rm = rotation_matrix(angles(1),angles(2),angles(3))


            if (geometry .eq. 'spheres' .or. geometry .eq. 'agglomeration') then 
                ! rotate the monomers for the "cluster of spheres" case
                call rotate_spheres(rm)
            else if (geometry .eq. 'ellipsoid') then
                ! Set a bunch of constants in the ellipsoid module
                call set_constants(angles(1),angles(2),angles(3),grain_a)
            end if

            ! Iterate through each photon energy
            do i=1,nephots
                energy = ephots(i)
                sigma_scat = 0.0
                sigma_abs = 0.0
                ! Set m - 1 (delms) for each material
                do j=1,nmaterials
                    delms(j) = CMPLX(ior_res(j,i), ior_ims(j,i))
                end do 
                
                ! delm (scalar) is used when nmaterials = 1.
                delm = delms(1)
                kphot = TWOPI*(1000*energy/hc)

                ! CASE: sphere; using integral expression for S(theta)
                if (geometry .eq. 'sphere' .and. .not. force_numerical) then 
                   
                    call q_sphere(kphot,a_eff,delm,sigma_abs,sigma_scat,sigma_ext)

                    sigma_abs = sigma_abs*(pi*a_eff*a_eff)
                    sigma_scat = sigma_scat*(pi*a_eff*a_eff)

                ! CASE: sphere; forcing a full numerical solution    
                else if (geometry .eq. 'sphere') then

                    !$omp parallel shared(sh,z,y,kphot,a_eff,delm) 
                    !$omp do schedule(dynamic) private(c1,c2) 
                     do c1=1,size(z)
                        do c2=1,size(y)
                            phi_total(c1,c2) = phi_sphere(z(c1),y(c2),kphot,a_eff,delm)
                            sh(c1,c2) = 1-exp( (0.0,1.0)*phi_total(c1,c2) )
                        end do
                     end do
                    !$omp end do 
                    !$omp end parallel

                ! CASE: "Cluster of spheres"
                else if (geometry .eq. 'spheres' .or. geometry .eq. 'agglomeration') then 

                    ! If the monomers are of more than one composition, set their compositions
                    !   in the spheres.f90 module
                    do c1=1,nspheres
                        if (multiple_compositions) then
                            c2 = agglom_material_tags(c1)
                            delm = delms(c2)
                        end if 
                        ior_r(c1) = REAL(delm)
                        ior_i(c1) = AIMAG(delm)
                    end do
                    
                    ! Get a phi grid
                    phi_total = phi_spheres(z,y,kphot)

                    ! Now set the shadow function
                    !$omp parallel shared(sh,phi_total) 
                    !$omp do schedule(dynamic) private(c1,c2) 
                    do c1=1,size(z)
                        do c2=1,size(y)
                            sh(c1,c2) = 1.0D0-exp( (0.0D0,1.0D0)*phi_total(c1,c2) )
                        end do
                     end do
                    !$omp end do 
                    !$omp end parallel

                ! CASE: Ellipsoidal grain
                else if (geometry .eq. 'ellipsoid') then 
                    !$omp parallel shared(sh,phi_total,z,y,kphot,grain_a,delm,rm) 
                    !$omp do schedule(dynamic) private(c1,c2) 
                    do c1=1,size(z)
                        do c2=1,size(y)
                            phi_total(c1,c2) = phi_ellipsoid(z(c1),y(c2),kphot, grain_a, delm )
                            sh(c1,c2) = 1.0 -exp( (0.0,1.0)*phi_total(c1,c2) )
                        end do
                    end do
                    !$omp end do 
                    !$omp end parallel

                ! CASE: Custom geometry (see custom.f90)
                else if (geometry .eq. 'custom') then
                    sh = shadow_custom(z,y,k,rm,ngrain,ephot,ephots,ior_res,ior_ims)
                    phi_total = phi_custom(z,y,k,rm,ngrain,ephot,ephots,ior_res,ior_ims)
                else 
                    write(stderr,*) "ERROR: Do not understand geometry=",geometry
                    stop 1
                end if

                ! Perform integrals (unless we used the integral expression for S(theta) for a spherical grain)
                if (.not. geometry .eq. 'sphere' .or. force_numerical) then
                    do c1=1,size(z)
                        do c2=1,size(y)
                         sigma_abs = sigma_abs + (1.0D0 - exp(-2.0D0*AIMAG(phi_total(c1,c2))))*dz*dy 
                         sigma_scat = sigma_scat + (abs(sh(c1,c2))**2)*dz*dy
                        end do 
                    end do
                end if

                ! Add the contribution from this orientation
                sigma_scats(i) = sigma_scats(i) +  sigma_scat*norm_const/norientations
                sigma_abss(i) = sigma_abss(i) + sigma_abs*norm_const/norientations

                ! Prints out a progress meter
                if (.not. quiet_mode .and. (.not. geometry .eq. 'sphere' .or. force_numerical)) then
                    write(stderr,fmt="(a1,a,t21,f6.2,a)",advance="no") achar(13), &
                        & " percent complete: ", (real(i + (nangle-1)*nephots)/real(nephots*norientations))*100.0, "%"
                end if
            end do
            ! If sphere+analytical, get rid of the 1/norientations normalization and quit! (only one orientation necessary...)
            if (geometry .eq. 'sphere' .and. .not. force_numerical) then 
                sigma_scats(i) = sigma_scats(i) * norientations
                sigma_abss(i) = sigma_abss(i) * norientations
                EXIT
            end if
        end do

        ! Adds a line break on the terminal.
        if (.not. quiet_mode) then 
            write(stderr, *) ""
        end if 
    end subroutine get_integrated_cross_sections

    subroutine set_gftsh()
        ! This sets the global array 'gftsh' via
        !   1. integrating f(x, y) along the y direction --> g
        !   2. performaing FFT on g --> gftsh

        implicit none
        integer :: i, j

        !$omp parallel shared(sh, z, y, gsh, a_eff)
        !$omp do schedule(dynamic) private(i,j)
        do i=1,size(z)
            do j=1, size(y)
                gsh(i) = gsh(i) + sh(i, j)
            end do
            gsh(i) = gsh(i) * grid_width * a_eff / size(y)
        end do
        !$omp end do 
        !$omp end parallel
        
        call fft1d_faster(gsh, kmin, kmax, enhancement, gftsh)
    end subroutine set_gftsh

    function get_gscatter()
        ! Calculates dC/dOmega from gftsh (global)
        implicit none
        integer i
        complex(kind=dp_complex), dimension(size(gscatter)) :: get_gscatter


        !$omp parallel shared(gscatter,gftsh,k,dy) 
        !$omp do schedule(dynamic) private(i)
        do i=1, size(kz)
            get_gscatter(i) = ( k * dy *abs(gftsh(i))/twopi)**2
        end do
        !$omp end do 
        !$omp end parallel
    end function get_gscatter

    function phi_average(dSigdOm)
        ! Performs 'phi-averaging' over 2d dCsca/dOmega(theta, phi) grid.
        !   returns a one-dimensional avearage at each theta in the thetaz array
        implicit none
        integer i,j
        real(kind=dp_complex), dimension(size(gscatter)) :: phi_average
        real(kind=dp_complex), dimension(size(gscatter), size(gscatter)), intent(in) :: dSigdOm
        
        real(kind=dp_real) :: ph, th
        real(kind=dp_complex) :: dsdo_val

        !$omp parallel shared(thetaz,nphi, dSigdOm, phi_average)
        !$omp do schedule(dynamic) private(i,j,dsdo_val, th)
        do i=1, size(thetaz)
            dsdo_val = 0.0D0
            th = thetaz(i)

            do j=1, nphi
                ph = (0.5*pi)*(real(j) / real(nphi))
                dsdo_val = dsdo_val + linterp2d(thetaz, thetaz, dSigdOm, th * cos(ph), th * sin(ph))
            end do 
            phi_average(i) = dsdo_val/nphi
        end do
        !$omp end do 
        !$omp end parallel
    end function phi_average

    subroutine diff_scat_cross_sect()
        ! Calculates the differential scattering cross section and prints the results to stdout
        implicit none 
        integer :: nangle, i, j
        real(kind=dp_real) :: th, sc, dth
        real(kind=dp_real), dimension(2*size(thetaz)) :: sc_array, thets
        complex(kind=dp_complex), dimension(10) :: delms
        
        ! Set the m-1 array for each material
        do i=1,nmaterials
            delms(i) = CMPLX(ior_res_single_en(i), ior_ims_single_en(i))
        end do 

        ! Scalar value (for when there's only one material)
        delm = delms(1)

        ! Set the normalization factor for efficiencies = C/pi*a^2        
        if (use_efficiencies) then 
            norm_const = 1.0D0/(pi*a_eff*a_eff)
            write(stdout,*) "# [thetaz] [thetay] [dQ_scat/domega]"
        else 
            norm_const = 1.0D0
            write(stdout,*) "# [thetaz] [thetay] [dsigma_scat/domega]"
        end if 

        ! CASE: sphere
        if (geometry .eq. 'sphere' .and. .not. force_numerical) then 
            if (verbose_mode) write(stderr,*) " Using analytical ADT for sphere..."

            ! If we're using the full 2d grid, make one
            if (do_full_2d_fft) then
                dth = 2 * sqrt(2. + 1.0D-5)* thetaz(size(thetaz)) / (size(thets) - 1)

                !$omp parallel shared(thets, dth, thetaz, sc_array, k, a_eff, delm)
                !$omp do schedule(dynamic) private(i)
                do i=1,size(thets)
                    thets(i) = - sqrt(2.0 + 1.0D-5)* thetaz(size(thetaz)) + (i-1)*dth
                    sc_array(i) = scatter_sphere(k, a_eff, thets(i), delm)
                end do
                !$omp end do 
                !$omp end parallel
                

                !$omp parallel shared(scatter,thetaz,thetay, thets, sc_array)
                !$omp do schedule(dynamic) private(i,j, th)
                do i=1,size(thetaz)
                    do j=1, size(thetay)
                        th = sqrt(thetaz(i)*thetaz(i) + thetay(j)*thetay(j))
                        scatter(i,j) = linterp(thets, sc_array, th)
                    end do
                end do
                !$omp end do 
                !$omp end parallel 

            ! Otherwise, keep things 1d
            else
                !$omp parallel shared(scatter,thetaz)
                !$omp do schedule(dynamic) private(i)
                do i=1,size(thetaz)
                    gscatter(i) = scatter_sphere(k, a_eff, thetaz(i), delm)
                end do
                !$omp end do 
                !$omp end parallel 
            end if 

        
        else
            ! CASE: sphere + force_numerical
            if (geometry .eq. 'sphere') then
                
                !$omp parallel shared(sh,z,y,rm,k,delm,grain_a, a_eff)
                !$omp do schedule(dynamic) private(i,j)
                do i=1,size(z)
                    do j=1,size(y)
                        sh(i,j) = shadow_sphere(z(i),y(j), k, a_eff, delm)
                    end do
                end do
                !$omp end do 
                !$omp end parallel

                ! Save the shadow function if the user wants
                if (save_shadow_function) then
                    write(orient_char, '(I5.5)') 1
                    open(unit=9,file=trim(adjustl(shadow_function_file))//trim(adjustl(orient_char))//".dat")
                    write(9,*) "# Shadow function for orientation ",1,"; angles(1,2,3) = ", angles
                    
                    do i=1,size(z)
                        do j=1,size(y)
                            write(9, *) z(i), y(j), REAL(sh(i,j)), IMAG(sh(i,j))
                        end do
                    end do
                    close(9)
                end if

                ! Do full 2d FFT if necessary
                if (do_full_2d_fft) then
                    call fft2d_faster(sh,kmin,kmax,enhancement, ftsh)
                    
                    !$omp parallel shared(scatter,ftsh,k,dz,dy) 
                    !$omp do schedule(dynamic) private(i,j)
                    do i=1,size(kz)
                        do j=1,size(ky)
                            scatter(i,j) = (k*dz*dy*abs(ftsh(i,j))/twopi)**2
                        end do
                    end do
                    !$omp end do 
                    !$omp end parallel

                ! Otherwise keep things 1d
                else 
                    call set_gftsh()
                    gscatter(:) = get_gscatter()
                end if
            else 
            
                ! loop over orientations
                do nangle = 1,norientations

                    ! fetch Euler angles
                    angles = get_new_angles(nangle)

                    ! calculate rotation matrix corresponding to rotation angles

                    ! angles(1) -- beta
                    ! angles(2) -- theta
                    ! angles(3) -- phi
                    rm = rotation_matrix(angles(1),angles(2),angles(3))
                    
                    ! Compute shadow function
                    if (geometry .eq. 'ellipsoid') then 
                        call set_constants(angles(1),angles(2),angles(3),grain_a)
                        !$omp parallel shared(sh,z,y,rm,k,delm,grain_a) 
                        !$omp do schedule(dynamic) private(i,j)
                        do i=1,size(z)
                            do j=1,size(y)
                                sh(i,j) = shadow_ellipsoid(z(i),y(j),k,grain_a,delm)
                            end do
                        end do
                        !$omp end do 
                        !$omp end parallel
                    elseif (geometry .eq. 'spheres' .or. geometry .eq. 'agglomeration')    then
                        call rotate_spheres(rm)
                        sh = shadow_spheres(z,y,k)
                    elseif (geometry .eq. 'custom') then
                        sh = shadow_custom(z,y,k,rm,ngrain,ephot,ephots,ior_res,ior_ims)
                    else
                        write(stderr,*) "ERROR: Do not understand geometry: ", geometry
                    end if 
                   
                    ! Compute dsigma/dOmega
                    if (do_full_2d_fft) then
                        call fft2d_faster(sh,kmin,kmax,enhancement, ftsh)

                        if (do_phi_averaging) then

                            !$omp parallel shared(ftsh,k,dy,dz) 
                            !$omp do schedule(dynamic) private(i, j)
                            do i=1, size(kz)
                                do j=1, size(ky)
                                    !-$OMP CRITICAL
                                    scatter(i,j) = ( k*dz*dy*abs(ftsh(i,j))/twopi)**2
                                    !-$OMP END CRITICAL
                                end do
                            end do
                            !$omp end do
                            !$omp end parallel

                            gftsh(:) = phi_average(scatter)
                            !$omp parallel shared(gscatter,gftsh,k,dy, norientations) 
                            !$omp do schedule(dynamic) private(i)
                            do i=1, size(kz)
                                !-$OMP CRITICAL
                                gscatter(i) = gscatter(i) + &
                                        gftsh(i)/real(norientations,kind=dp_real)
                                !-$OMP END CRITICAL
                            end do
                            !$omp end do
                            !$omp end parallel

                        else
                            !$omp parallel shared(scatter,ftsh,k,dz,dy, norientations) 
                            !$omp do schedule(dynamic) private(i,j)
                            do i=1,size(kz)
                                do j=1,size(ky)
                                    !-$OMP CRITICAL
                                    scatter(i,j) = scatter(i,j) +  &
                                             (k*dz*dy*abs(ftsh(i,j))/twopi)**2/real(norientations,kind=dp_real)
                                    !-$OMP END CRITICAL
                                end do
                            end do
                            !$omp end do
                            !$omp end parallel
                        end if


                        

                    else 
                        call set_gftsh()

                        !$omp parallel shared(gscatter,gftsh,k,dy, norientations) 
                        !$omp do schedule(dynamic) private(i)
                        do i=1, size(kz)
                            !-$OMP CRITICAL
                            gscatter(i) = gscatter(i) + ( k * dy *abs(gftsh(i))/twopi)**2/real(norientations,kind=dp_real)
                            !-$OMP END CRITICAL
                        end do
                        !$omp end do
                        !$omp end parallel
                        
                    end if
                    if (.not. quiet_mode) then 
                        write(stderr,fmt="(a1,a,t21,f6.2,a)",advance="no") achar(13), &
                            & " percent complete: ", (real(nangle)/real(norientations))*100.0, "%"
                    end if
                    if (save_shadow_function) then
                        write(orient_char, '(I5.5)') nangle
                        open(unit=9,file=trim(adjustl(shadow_function_file))//trim(adjustl(orient_char))//".dat")
                        write(9,*) "# Shadow function for orientation ",nangle,"; angles(1,2,3) = ", angles
                        
                        do i=1,size(z)
                            do j=1,size(y)
                                write(9, *) z(i), y(j), REAL(sh(i,j)), IMAG(sh(i,j))
                            end do
                        end do
                        close(9)
                    end if
                end do
                if (.not. quiet_mode) then 
                    write(stderr, *) ""
                end if
            endif 
        endif
       

        if (timing_mode) stop 

        ! for the fast fft mode, we ASSUME isotropy, so just 1d
        if ((.not. do_full_2d_fft) .or. do_phi_averaging) then 
            do i=1,size(thetaz)
                write(stdout,*) thetaz(i),gscatter(i)*norm_const
            end do
        else
            do i=1,size(thetaz)
                do j=1,size(thetay)
                    write(stdout,*) thetay(j),thetaz(i),scatter(i,j)*norm_const
                end do
            end do
        end if
    end subroutine diff_scat_cross_sect

    subroutine initialize_and_allocate_vars()
      implicit none 

      integer :: line, status, next, ng, n, allocatestatus, i, j, norientations_new, closest_ngrid
      real(kind=dp_real) :: ds, max_dist, scale, lambda
      character(len=100) :: junk
        if (nthreads > 0) then
            call omp_set_num_threads(nthreads)
        end if
        
        ! -- Allocate variables for ephots and index of refraction
        allocate(ephots(nephots),stat=allocatestatus)
            if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (ephots) array ***"
        allocate(ior_res(nmaterials,nephots),stat=allocatestatus) 
            if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (ior_res_mult) array ***"
        allocate(ior_ims(nmaterials,nephots),stat=allocatestatus)
            if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (ior_ims_mult) array ***"

        ! set ephots array
        if (integrated_mode) then
            do i=1,nephots
                ephots(i) = ephot_min + (i-1)*dephot
            end do
        else
            ephots(1) = ephot
        end if 

        ! wavelength (in micrometers)
        lambda = 0.00123984193 / ephots(1) ! max(hc/E) = hc/min(E)

      
        ! if we have defined material files, read them
        if (material_file_specified) then 
            do i=1, nmaterials
                call get_ior(material_files(i), ior_res(i,:), ior_ims(i,:), ior_res_single_en(i), ior_ims_single_en(i))
            end do 

        else
            ! otherwise, set the index of refraction to the ior-im and ior-re parameters
            do i=1,nephots
                ior_res(1,i) = ior_re 
                ior_ims(1,i) = ior_im 
            end do 
            ior_res_single_en(1) = ior_re 
            ior_ims_single_en(1) = ior_im 
        end if 

        
 
        ! Set angles -------------------------------
        if (angle_mode == 'sequential') then
            !if (rotation_axis == 'none') then 
            if (do_phi_averaging) then
                norientations_new = int(real(norientations)**(1.0/2.0))**2
                if (norientations /= norientations_new) then
                    write(stderr,*) ""
                    write(stderr,*) "Warning: norientations: ",norientations,"-->",norientations_new
                    norientations = norientations_new
                end if
            else
                norientations_new = int(real(norientations)**(1.0/3.0))**3
                if (norientations /= norientations_new) then
                    write(stderr,*) ""
                    write(stderr,*) "Warning: norientations: ",norientations,"-->",norientations_new
                    norientations = norientations_new
                end if
            end if
            !end if 
        else if (angle_mode == 'random') then
            call init_random_seed()

        ! Check to see if the file of orientations is OK
        else if (angle_mode == 'file') then 
            ! Is the file specified?
            if (angle_file == "") then
              write(stderr,*) "ERROR: angle_mode of 'file', but specified file is empty"
              stop
            else
              line = 0
              ! open the file
              open(10,file=angle_file,iostat=status)

              ! did that work?
              if (status .ne. 0)then 
                write(stderr,*) "ERROR: angle_file of ",angle_file," does not exist."
                stop 1
              else

              ! read in until we can't anymore.
              do while(status == 0)
                read(10,'(A)',iostat=status) junk
                if (status == 0) line = line+1
              end do 

              ! Mismatch between norientations and the number of orientations in the file!
              if (line .ne. norientations) then
                ! File is empty or unreadable
                if (line == 0) then
                    write(stderr,*) "ERROR: No orientations found in ", angle_file,"!"
                    stop 1
                end if 

                ! Warn the user, but set norientations -> # lines.
                write(stderr,*) "WARNING: norientations set to ",norientations," but there are ",line," orientations in ",&
                  angle_file,". norientations -> ",line
                norientations = line 
              end if

              end if
              ! close the file.
              close(10) 
            end if 
        end if 

        

    ! ---------------------------------------------------
        ! Set the grid length (in physical units)
        if ((geometry == 'sphere') .or. (geometry == 'spheres') .or. (geometry == 'agglomeration') .or. (geometry=='custom')) then 
            if (geometry == 'spheres' .or. geometry .eq. 'agglomeration') then
                default_ior_i = ior_im
                default_ior_r = ior_re
                ! Get the minimum radius of a sphere that can contain the grain.
                call read_spheres(agglom_file_name, a_eff, max_dist)
            else
                max_dist = a_eff
                norientations = 1
            end if 
            grain_a(1) = a_eff 
            grain_a(2) = a_eff
            grain_a(3) = a_eff
            
            ! make the grid a little larger to accomodate furthest part of grain
            if (.not. grid_width_manually_set) then 
                grid_width = 2.001*max_dist/a_eff
            end if

        else if (geometry == 'ellipsoid') then
            scale = a_eff/((grain_a(1)*grain_a(2)*grain_a(3))**(1.0/3.0))
            ! normalize so that grain_a(1), (2), (3) produce an ellipse with an effective
            ! radius of a_eff
            grain_a(1) = grain_a(1)*scale
            grain_a(2) = grain_a(2)*scale
            grain_a(3) = grain_a(3)*scale

            ! make the grid a little larger than the longest principal axis
            if (.not. grid_width_manually_set) then 
                grid_width = 2.001*max(grain_a(1), grain_a(2), grain_a(3))/a_eff
            end if
        else
            write(stderr,*) "ERROR: cannot understand ", geometry
            stop 1
        end if 

        if (grid_width*a_eff < lambda) then
            write(stderr, *) "WARNING: a_eff >> lambda is violated. ADT is not valid here!"
            if (.not. grid_width_manually_set) then 
                grid_width = 2.001 * lambda / a_eff
            end if
        end if
        
        max_angle = max_angle/arcseconds_per_radian ! convert max_angle from arcseconds to radians
        k = (2*pi/hc)*1000*ephot ! wave number
        

        ngrid = ngrain ! Start with no padding 

        ! ensure that ngrid is of the form (2**i)(3**j)(5**k)
        closest_ngrid = allowed_ngrids(1)
        i=1
        do while (allowed_ngrids(i) < ngrid .and. i < size(allowed_ngrids))
            i = i + 1
            closest_ngrid = allowed_ngrids(i)
        end do
        if (closest_ngrid .ne. ngrid) then
                write(stderr,*) "Warning: your requested value of ngrain,",&
                    ngrid,", is not of the form (2**i)(3**j)(5**k). Changing ngrain to ",closest_ngrid
        end if
        ngrid = closest_ngrid

        ! Add padding if use_padded_fft is asked for.
        if (use_padded_fft) then  
            ds = grid_width*a_eff/real(ngrain)
            next = 0
            ng = ngrid
            L = ng*ds 
            n = int(ceiling(asin(max_angle)*L*k/(2*pi)))
            do while(n < nscatter .and. ng < max_ngrid)
                i = i + 1
                ng = allowed_ngrids(i)
                next = ng - ngrid
                L = ng*ds 
                n = int(ceiling(asin(max_angle)*L*k/(2*pi)))
            end do
            if (grid_width_manually_set) then 
                write(stderr,*) "ERROR: grid_width cannot be manually set in padded FFT mode."
                stop 1
            end if
            grid_width = L/a_eff
            ngrid = ng 
            if (n < nscatter) then
                write(stderr,*) "Warning: your request for an nscatter of ",&
                    nscatter," required too large a grid. Changing nscatter to ",n 
            else if (n > nscatter) then
                write(stderr,*) "Note: your request for an nscatter of ",&
                    nscatter," is already exceeded by a non-padded grid. Changing nscatter to",n
            end if 

            nscatter = n 
        end if 

       
    ! ---------------------------------------------------
   
    ! allocate arrays that depend on ngrid

        ! z , y
        allocate(z(ngrid),stat = allocatestatus)
            if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (z) array ***"
        allocate(y(ngrid),stat = allocatestatus)
            if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (y) array ***"
        ! sh
        allocate(sh(ngrid,ngrid),stat = allocatestatus)
            if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (sh) array ***"

        ! gsh = integral(f(x, y)dy)
        if (.not. do_full_2d_fft) then
            allocate(gsh(ngrid), stat=allocatestatus)
                if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (gsh) array ***"
        end if
        if (integrated_mode) then
         !   allocate(phi1(ngrid,ngrid),stat = allocatestatus)
         !       if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (phi1) array ***"
         !   allocate(phi2(ngrid,ngrid),stat = allocatestatus)
         !       if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (phi2) array ***"
            allocate(phi_total(ngrid,ngrid),stat = allocatestatus)
                if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (phi_total) array ***"
        end if 
    ! ----------------------------------------------------
     
    ! set grid boundaries and spacing in physical coords
        zmin = -grid_width*a_eff/2.0D0
        ymin = -grid_width*a_eff/2.0D0
        zmax = grid_width*a_eff/2.0D0
        ymax = grid_width*a_eff/2.0D0

        dz = (zmax-zmin)/(size(z)-1)
        dy = (ymax-ymin)/(size(y)-1)
    ! -------------------------
        
    ! set z and y arrays
        do i=1,size(z)
            z(i) = zmin + (i-1)*dz
        end do
        do i=1,size(y)
            y(i) = ymin + (i-1)*dy
        end do
    ! -------------------
        if (.not. do_full_2d_fft) then
            do i=1,size(z)
                gsh(i) = 0.0D0
            end do
        end if 
      
        L = z(size(z)) - z(1)  ! length of grid in physical coords
        write(stderr, *) "L = ", L

    ! figures out what kmin, kmax, enhancement and kactual are.
        if (do_full_2d_fft .and. .not. do_phi_averaging) then
            call get_fft_vals(-max_angle,max_angle,nscatter,kmin,kmax,enhancement,kactual)   
        else
            call get_fft_vals(0.0D0,max_angle,nscatter,kmin,kmax,enhancement,kactual)   
        end if
        !write(stderr, *) "max_angle = ", max_angle, "kmin,kmax,enh = ", kmin, kmax, enhancement
        !write(stderr, *) "L, 2pi/L = ", L, (TWOPI/L), "k=",k
      
    ! allocate arrays that depend on kactual

        ! kz
        allocate(kz(kactual),stat = allocatestatus)
            if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (kz) array ***"
        ! ky
        allocate(ky(kactual),stat = allocatestatus)
            if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (ky) array ***"
        ! thetaz
        allocate(thetaz(kactual),stat = allocatestatus)
            if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (thetaz) array ***"
        ! thetay
        allocate(thetay(kactual),stat = allocatestatus)
            if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (thetay) array ***"
        ! ftsh
        allocate(ftsh(kactual,kactual),stat = allocatestatus)
            if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (thetay) array ***"
        ! scatter
        allocate(scatter(kactual,kactual),stat = allocatestatus)
            if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (scatter) array ***"
       
        if ((.not. do_full_2d_fft) .or. do_phi_averaging) then
            allocate(gftsh(kactual),stat = allocatestatus)
                if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (gftsh) array ***"
            allocate(gscatter(kactual),stat = allocatestatus)
                if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (gscatter) array ***"
        end if 
    ! ----------------------------------

        dk = real(kmax-kmin)/real(size(kz) - 1)
        
    ! set kz, ky, thetaz and thetay arrays
    !   + make scatter array all zero
        do i=1,size(kz)
            kz(i) = ((i-1)*dk + kmin)*(TWOPI/L)
            ky(i) = kz(i)
            
            !
            ! This 1/ngrid factor is a fudge factor to ensure the alignment of the minima
            ! its a heuristic for now, unless there's a bug lingering somewhere to explain
            ! why it works. If you don't use it, the minima shift outward by a factor of ~1/ngrid 
            ! 
            if (sang_correction) then
                thetaz(i) = asin((kz(i)/k)*(1 - 1./ngrid))
                thetay(i) = asin((ky(i)/k)*(1 - 1./ngrid))
            else 
                thetaz(i) = asin(kz(i)/k)
                thetay(i) = asin(ky(i)/k)
            end if 

            ! Initialize arrays
            if (.not. do_full_2d_fft .or. do_phi_averaging) then 
                gscatter(i) = 0.0D0
            end if 

            do j=1,size(ky)
                scatter(i,j) = 0.0D0

            end do 
        end do
    ! -----------------------------------
        

        if (nmaterials .gt. 1) then 
            multiple_compositions = .TRUE.

            if (geometry .eq. "spheres"  .or. geometry .eq. 'agglomeration') then 
                allocate(agglom_material_tags(nspheres),stat=allocatestatus)
                    if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (agglom_material_tags) array ***"
                
                
                
                call read_spheres_composition(agglom_composition_file, material_tags, agglom_material_tags)
                if (.not. integrated_mode) then 
                    do i=1,nspheres
                        j = agglom_material_tags(i)
                        ior_r(i) = ior_res_single_en(j)
                        ior_i(i) = ior_ims_single_en(j)
                    end do 
                end if
            end if 

        else 
            multiple_compositions = .FALSE.
            if (material_file_specified .and. &
               & ( geometry .eq. "spheres" .or. geometry .eq. "agglomeration" ) .and. .not. integrated_mode) then 
                do i=1,nspheres
                    ior_r(i) = ior_res_single_en(1)
                    ior_i(i) = ior_ims_single_en(1)
                end do 
            end if 
        end if 



    end subroutine initialize_and_allocate_vars

! -- given thetamin and thetamax and desired resolution in scattering angle, 
!    sets the relevant FFT parameters (kmin, kmax, enhancement, Kout)
    subroutine get_fft_vals(thetamin,thetamax,Kask,kmin,kmax,enhancement,Kout)
        real(kind=dp_real), intent(in) :: thetamin, thetamax
        integer, intent(in) :: Kask
        integer, intent(out) :: kmin,kmax,enhancement, Kout
        integer :: i

        enhancement = 0
        !write(stderr, *) "thetamin, sin(thetamin), k, L, prod = ",thetamin, sin(thetamin), k, L, k*sin(thetamin)*(L/TWOPI)
        kmin = int(floor(k*sin(thetamin)*(L/TWOPI)))
        kmax = int(ceiling(k*sin(thetamax)*(L/TWOPI)))

        Kout = kmax - kmin
        i = 1
        do while ( (enhancement + 1) * Kout < Kask) 
            enhancement = allowed_ngrids(i) - 1
            i = i + 1
        end do 

        Kout = (enhancement + 1)*Kout

    end subroutine get_fft_vals


    subroutine init_random_seed()
        !use, intrinsic :: iso_fortran_env
        implicit none

        integer, allocatable :: seed(:)
        integer(4) :: i, n, un, istat, dt(8), pid, t(2), s
        !integer(kind=1) :: getpid
        integer(8) :: count, tms

        call random_seed(size = n)
        allocate(seed(n))
        ! first try if the os provides a random number generator
        !open(un, file="/dev/urandom", access="stream", &
        !     form="unformatted", action="read", status="old", iostat=istat)
        istat = 1
        if (istat == 0) then
           read(un) seed
           close(un)
        else
           ! fallback to xor:ing the current time and pid. the pid is
           ! useful in case one launches multiple instances of the same
           ! program in parallel.
           call system_clock(count)
           if (count /= 0) then
              t = transfer(count, t)
           else
              call date_and_time(values=dt)
              tms = (dt(1) - 1970) * 365_8 * 24 * 60 * 60 * 1000 &
                   + dt(2) * 31_8 * 24 * 60 * 60 * 1000 &
                   + dt(3) * 24 * 60 * 60 * 60 * 1000 &
                   + dt(5) * 60 * 60 * 1000 &
                   + dt(6) * 60 * 1000 + dt(7) * 1000 &
                   + dt(8)
              t = transfer(tms, t)
           end if
           s = ieor(t(1), t(2))
           !pid = GETPID() + 1099279 ! add a prime
           pid = 1099279
           s = ieor(s, pid)
           if (n >= 3) then
              seed(1) = t(1) + 36269
              seed(2) = t(2) + 72551
              seed(3) = pid
              if (n > 3) then
                 seed(4:) = s + 37 * (/ (i, i = 0, n - 4) /)
              end if
           else
              seed = s + 37 * (/ (i, i = 0, n - 1 ) /)
           end if
        end if
        call random_seed(put=seed)
    end subroutine init_random_seed

    function get_new_angles(i)
        real(kind=dp_real), dimension(3) :: get_new_angles
        integer, intent(in) :: i
        integer                :: j 
        character(len=50)    :: junk
        real(kind=dp_real) :: norm
        integer :: cbrt_norientations, nbeta, ntheta, nph, sqrt_norientations


        ! Note (JAH, June 26, 2014) -- this function needs to be fixed for rotating
        !                 about a given axis (check if mstm or ddscat orienation,
        !                 or better yet, provide a rotation/precession axis option.

        if (angle_mode .eq. 'sequential') then 
            !if (rotation_axis == 'none') then 
            cbrt_norientations = int(norientations**(1.0/3.0))
            sqrt_norientations = int(norientations**(1.0/2.0))
            if (do_phi_averaging) then
                norm = real(sqrt_norientations ,kind=dp_real)**(-1.0)
                nbeta = mod(i/(sqrt_norientations),   sqrt_norientations)
                ntheta = mod(i,                       sqrt_norientations)
                nph = 0
                get_new_angles(3) = 0.
            else
                norm = real(cbrt_norientations ,kind=dp_real)**(-1.0)
                nbeta = mod(i/(cbrt_norientations*cbrt_norientations),   cbrt_norientations)
                ntheta = mod(i/(cbrt_norientations),               cbrt_norientations)
                nph = mod(i,                             cbrt_norientations)
                get_new_angles(3) = (2*pi*(nph-1))*norm - pi
            endif
            get_new_angles(1) = (2*pi*(nbeta-1))*norm - pi
            get_new_angles(2) = (pi*(ntheta-1))*norm
                
            !else if ( rotation_axis .eq. 'xaxis' ) then 
            !    norm = real(norientations ,kind=dp_real)**(-1.0)
            !    get_new_angles(1) =  2*pi*(i-1)*norm - pi
            !    get_new_angles(2) = 0.0
            !    get_new_angles(3) = 0.0
            !else if ( rotation_axis .eq. 'yaxis' ) then 
            !    get_new_angles(1) = 0.0
            !    get_new_angles(2) = pi*(i-1)*norm
            !    get_new_angles(3) = 0.0
            !else if ( rotation_axis .eq. 'zaxis' ) then 
            !    get_new_angles(1) = 0.0
            !    get_new_angles(2) = 0.0
            !    get_new_angles(3) = 2*pi*(i-1)*norm - pi
            !else 
            !    write(stderr,*) "ERROR (get_new_angles): Do not understand rotation_axis: '",trim(adjustl(rotation_axis)),"'"
            !end if 
        else if ( angle_mode .eq. 'file' ) then
            open(unit=1,file=angle_file)
            if (i > 1) then 
                do j=1,i-1
                    read(1,'(A)') junk
                end do
            end if 
            read(1,*) get_new_angles
            close(1)
            if (axes_convention == 'mstm') then
              call convert_mstm_to_ddscat(get_new_angles)
            end if 
        else if ( angle_mode .eq. 'random' ) then
            call random_number(get_new_angles)
        
            !if (rotation_axis .eq. "none") then 
            get_new_angles(1) = 2*pi*get_new_angles(1) - pi
            get_new_angles(2) = pi*get_new_angles(2) 
            get_new_angles(3) = 2*pi*get_new_angles(3) - pi
            !else if ( rotation_axis .eq. 'xaxis' ) then 
            !    get_new_angles(1) = 2*pi*get_new_angles(1) - pi
            !    get_new_angles(2) = 0.0
            !    get_new_angles(3) = 0.0
            !else if ( rotation_axis .eq. 'yaxis' ) then 
            !    get_new_angles(1) = 0.0
            !    get_new_angles(2) = pi*get_new_angles(2)
            !    get_new_angles(3) = 0.0
            !else if ( rotation_axis .eq. 'zaxis' ) then 
            !    get_new_angles(1) = 0.0
            !    get_new_angles(2) = 0.0
            !    get_new_angles(3) = 2*pi*get_new_angles(3) - pi
            !else 
            !    write(stderr,*) "ERROR (get_new_angles): Do not understand rotation_axis: '",trim(adjustl(rotation_axis)),"'"
            !end if 
        else 
            write(stderr, *) "ERROR (get_new_angles): Do not understand&
            & angle_mode '",trim(adjustl(angle_mode)),"'."
            stop 

        endif

        
    end function get_new_angles

    subroutine cleanup()
        implicit none 
        integer :: deallocatestatus
        deallocate(z,stat=deallocatestatus)
        deallocate(y,stat=deallocatestatus)
        deallocate(kz,stat=deallocatestatus)
        deallocate(ky,stat=deallocatestatus)
        deallocate(thetaz,stat=deallocatestatus)
        deallocate(thetay,stat=deallocatestatus)
        deallocate(sh,stat=deallocatestatus)
        deallocate(ftsh,stat=deallocatestatus)
        deallocate(scatter,stat=deallocatestatus)
        
        if (geometry .eq. 'spheres'  .or. geometry .eq. 'agglomeration')    then
            deallocate(pos)
            deallocate(pos_rot)
            deallocate(radii)
            deallocate(ior_r)
            deallocate(ior_i)
        end if

        if (.not. do_full_2d_fft) then
            deallocate(gsh)
            deallocate(gftsh)
            deallocate(gscatter)
        end if

    end subroutine cleanup

    
    subroutine print_parameters(i)
        implicit none 
        integer, intent(in) :: i
        write(i,*) "# Parameter values"
        write(i,*) "# -----------------------------------"
        if (.not. integrated_mode) then 
            write(i,*) "# delm: ",delm
            write(i,*) "# ephot: ",ephot, "keV"
            write(i,*) "# nscatter: ",nscatter
            write(i,*) "# ior_re: ",ior_re
            write(i,*) "# ior_im: ",ior_im
        else 
            write(i,*) "# ephot_min: ",ephot_min, " keV"
            write(i,*) "# ephot_max: ",ephot_max, " keV"
            write(i,*) "# nephots: ",nephots
            write(i,*) "# Material file: '",trim(adjustl(material_file)),"'"
        end if 
        write(i,*) "# geometry: ", trim(adjustl(geometry))
        write(i,*) "# angle_mode: ",trim(adjustl(angle_mode))
        if (do_phi_averaging) then
            write(i,*) "# do-phi-averaging; nphi = ",nphi
        end if
        if (do_full_2d_fft) then
            write(i,*) "# do-full-2d-fft"
        end if
        if (angle_mode == "file") write(i,*) "# angle_file: ", trim(adjustl(angle_file))
        write(i,*) "# ngrain: ",ngrain
        write(i,*) "# norientations: ",norientations
        write(i,*) "# grain-axis-z: ",grain_a(1), "microns"
        write(i,*) "# grain-axis-y: ",grain_a(2), "microns"
        write(i,*) "# grain-axis-z: ",grain_a(3), "microns"
        write(i,*) "# grid-width: ", grid_width * a_eff, "microns"
        !write(i,*) "# rotation-axis: ",rotation_axis
        write(i,*) "# use_padded_fft: ",use_padded_fft
        write(i,*) "# parameter_file_name: ",trim(adjustl(parameter_file_name))
        write(i,*) "# fftw_optimization_mode_name: ",trim(adjustl(fftw_optimization_mode_name))
        if (geometry == "spheres" .or. geometry .eq. 'agglomeration') &
            & write(i,*) "# agglom_file_name: ",trim(adjustl(agglom_file_name))
        write(i,*) "# -----------------------------------"
    

    end subroutine print_parameters

    

end program ggadt
