program ggadt
    !use, intrinsic :: iso_c_binding
    !use, intrinsic :: iso_fortran_env

    use sphere
    use spheres
    use ellipsoid
    use common_mod
    use constants

    

    implicit none

    real(kind=dp_real), allocatable :: x(:), y(:), kx(:), ky(:), thetax(:), thetay(:)

    complex(kind=dp_complex), allocatable :: sh(:,:), ftsh(:,:), phi_total(:,:)

    real(kind=dp_real), allocatable :: scatter(:,:)
    real(kind=dp_real), allocatable :: ephots(:), ior_res(:), ior_ims(:) ! NEW variables (for doing sed of extinction)

    real(kind=dp_real) :: xmin, xmax, ymin, ymax, dx, dy, k, l, dk
    real(kind=dp_real), dimension(3) :: eul_ang
    real(kind=dp_real), dimension(3,3) :: rm 

    integer :: kactual, kmin, kmax, enhancement


    call set_parameter_values()
    call initialize_and_allocate_vars()
    !call set_optimization_mode(fftw_optimization_mode_name)
    call print_parameters(stdout)
    if (verbose_mode) call print_parameters(stderr)

    !write(stderr,*) "Allocated and initialized all variables"

    if (sed_mode) then 
        call calculate_sed_of_cross_sections()
    else
        call diff_scat_cross_sect()
    end if 

    call cleanup()

    write(stderr,*) ""
    write(stderr,*) "Done."
contains

    subroutine calculate_sed_of_cross_sections()
        implicit none
        real(kind=dp_real) :: sigma_scat, sigma_abs
        real(kind=dp_real),dimension(size(ephots)) :: sigma_scats, sigma_abss
        complex(kind=dp_complex) :: delm
        integer :: i

        call get_ior(material_file, ior_res, ior_ims)

        write(stdout,*) "# [ephot] [sigma_scat] [sigma_abs]"
        
        do i=1,nephots
            delm = CMPLX(ior_res(i), ior_ims(i))
            call get_integrated_cross_sections(ephots(i), delm, sigma_scat, sigma_abs)
            sigma_scats(i) = sigma_scat
            sigma_abss(i) = sigma_abs
            write(stderr,fmt="(a1,a,t21,f6.2,a)",advance="no") achar(13), &
                    & " percent complete: ", (real(i)/real(nephots))*100.0, "%"
        end do
        
        do i=1,nephots
            write(stdout,*) ephots(i), sigma_scats(i), sigma_abss(i)
        end do


    end subroutine calculate_sed_of_cross_sections

    subroutine get_ior(matfile, iorres, iorims)
        implicit none
        character(len=200), intent(in) :: matfile 
        character(len=200) :: mat, junk 
        real(kind=dp_real), intent(out), dimension(size(ephots)) :: iorres, iorims
        real(kind=dp_real) :: eps, imeps
        integer  :: i,j, allocatestatus, ICOMP, niors, counter, ios, maxrecs
        real(kind=dp_real), allocatable  :: file_ior_re(:), file_ior_im(:), file_ephots(:)

        maxrecs = 100000

        if (material .ne. 'custom') then 
            write(stderr,*) "ERROR: Do not recognize material", material,". &
                & GGADT does not currently have any built-in materials. &
                & Specify --material='custom' and --material-file='/path/to/file'."
            stop 
        end if 
        open(unit=1,file=matfile) ! open sphere file.

        ! Read header
        read(1,'(i3,a8,a64)') ICOMP, junk, mat
        if (verbose_mode) write(stderr,*) "# Material file: '",trim(adjustl(matfile)),"'"
        if (verbose_mode) write(stderr,*) "# Material: '",trim(adjustl(mat)),"'"
        
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
        REWIND(1) 

        read(1,*) junk 
        read(1,*) junk 

        ! -- 
        allocate(file_ior_re(niors),stat=allocatestatus)
                if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (file_ior_re) array ***"
        allocate(file_ior_im(niors),stat=allocatestatus)
                if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (file_ior_im) array ***"
        allocate(file_ephots(niors),stat=allocatestatus)
                if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (file_ephots) array ***"

        do i=1,niors
            read(1,*) file_ephots(i), file_ior_re(i), file_ior_im(i), eps, imeps
            file_ephots(i) = file_ephots(i) / 1000.0 ! convert from eV to keV
        end do


        do i=1,nephots
            iorres(i) = linterp(file_ephots,file_ior_re,ephots(i))
            iorims(i) = linterp(file_ephots,file_ior_im,ephots(i))
        end do 
       
        close(1)

    end subroutine get_ior
    subroutine get_integrated_cross_sections(energy,delm,sigma_scat,sigma_abs)
        implicit none
        real(kind=dp_real), intent(in) :: energy
        real(kind=dp_real), intent(out) :: sigma_abs, sigma_scat
        complex(kind=dp_complex), intent(in) :: delm

        real(kind=dp_real) :: kphot
        integer :: c1,c2, neul


        kphot = TWOPI*(energy/0.001239842)

        sigma_scat = 0.0
        sigma_abs = 0.0

        ! loop over orientations        
        do neul = 1,norientations

            ! fetch Euler angles
            eul_ang = get_new_euler_angles(neul)

            ! calculate rotation matrix corresponding to euler angles
            rm = matmul(matmul(rot_x(eul_ang(1)), rot_y(eul_ang(2))),rot_z(eul_ang(3)))

            if (geometry .eq. 'sphere') then

                !$omp parallel shared(sh,x,y,kphot,a_eff,delm) 
                !$omp do schedule(dynamic) private(c1,c2) 
                 do c1=1,size(x)
                    do c2=1,size(y)
                        sh(c1,c2) = shadow_sphere(x(c1),y(c2), kphot, a_eff, delm)
                        phi_total(c1,c2) = phi_sphere(x(c1),y(c2),kphot,a_eff,delm)
                    end do
                 end do
                !$omp end do nowait
                !$omp end parallel


            else if (geometry .eq. 'spheres') then 
                call rotate_spheres(rm)
                do c1=1,nspheres
                    ior_r(c1) = REAL(delm)
                    ior_i(c1) = AIMAG(delm)
                end do
                
                phi_total = phi_spheres(x,y,kphot)
                !$omp parallel shared(sh,phi_total) 
                !$omp do schedule(dynamic) private(c1,c2) 
                do c1=1,size(x)
                    do c2=1,size(y)
                        sh(c1,c2) = 1-exp( (0.0,1.0)*phi_total(c1,c2) )
                    end do
                 end do
                !$omp end do nowait
                !$omp end parallel
            else if (geometry .eq. 'ellipsoid') then 

                !$omp parallel shared(sh,phi_total,x,y,kphot,grain_a,delm,rm) 
                !$omp do schedule(dynamic) private(c1,c2) 
                do c1=1,size(x)
                    do c2=1,size(y)
                        sh(c1,c2) = shadow_ellipsoid(x(c1),y(c2), kphot, grain_a, delm, rm)
                        phi_total(c1,c2) = phi_ellipsoid(x(c1),y(c2),kphot, grain_a, delm,rm )
                    
                    end do
                end do
                !$omp end do nowait
                !$omp end parallel
            else 
                write(stderr,*) "ERROR: Do not understand geometry=",geometry
                stop
            end if

            !$omp parallel shared(sigma_scat, sh, sigma_abs, phi_total, dx, dy) 
            !$omp do schedule(dynamic) private(c1,c2) 
            do c1=1,size(x)
                do c2=1,size(y)
                 sigma_abs = sigma_abs + (1 - exp(-2*AIMAG(phi_total(c1,c2))))*dx*dy 
                 sigma_scat = sigma_scat + (abs(sh(c1,c2))**2)*dx*dy
                end do 
            end do
            !$omp end do nowait
            !$omp end parallel
        end do
        
        sigma_scat = sigma_scat/norientations
        sigma_abs = sigma_abs/norientations
        
    end subroutine get_integrated_cross_sections

    subroutine diff_scat_cross_sect()
        implicit none 
        integer :: neul, i, j
        ! loop over orientations
        do neul = 1,norientations

            ! fetch Euler angles
            eul_ang = get_new_euler_angles(neul)

            ! calculate rotation matrix corresponding to euler angles
            rm = matmul(matmul(rot_x(eul_ang(1)), rot_y(eul_ang(2))),rot_z(eul_ang(3)))
            

            if (geometry .eq. 'ellipsoid') then 
                !$omp parallel shared(sh,x,y,rm,k,delm,grain_a) 
                !$omp do schedule(dynamic) private(i,j)
                do i=1,size(x)
                    do j=1,size(y)
                        sh(i,j) = shadow_ellipsoid(x(i),y(j),k,grain_a,delm,rm)
                    end do
                end do
                !$omp end do nowait
                !$omp end parallel
            elseif  (geometry .eq. 'sphere') then 
                !$omp parallel shared(sh,x,y,rm,k,delm,grain_a)
                !$omp do schedule(dynamic) private(i,j)
                do i=1,size(x)
                    do j=1,size(y)
                        sh(i,j) = shadow_sphere(x(i),y(j), k, a_eff, delm)
                    end do
                end do
                !$omp end do nowait
                !$omp end parallel
            elseif (geometry .eq. 'spheres')    then
                call rotate_spheres(rm)
                sh = shadow_spheres(x,y,k)
            else
                write(stderr,*) "ERROR: Do not understand geometry: ", geometry
            end if 
           
            call fft_faster(sh,kmin,kmax,enhancement, ftsh)

            !$omp parallel shared(scatter,ftsh,k,dx,dy,norientations) 
            !$omp do schedule(dynamic) private(i,j)
            do i=1,size(kx)
                do j=1,size(ky)
                    scatter(i,j) = scatter(i,j) + ((k*dx*dy*abs(ftsh(i,j))/twopi)**2)/real(norientations,kind=dp_real)
                end do
            end do
            !$omp end do nowait
            !$omp end parallel


            write(stderr,fmt="(a1,a,t21,f6.2,a)",advance="no") achar(13), &
                    & " percent complete: ", (real(neul)/real(norientations))*100.0, "%"
        end do

        if (timing_mode) stop 

        write(stdout,*) "# [thetax] [thetay] [dqscat/domega]"
        
        do i=1,size(thetax)
            do j=1,size(thetay)
                write(stdout,*) thetax(i),thetay(j),scatter(i,j)/(pi*a_eff*a_eff)
            end do
        end do
        if (verbose_mode) write(stderr,*) "done."
    end subroutine diff_scat_cross_sect

    subroutine initialize_and_allocate_vars()
      implicit none 
      integer :: line, status, next, ng, n, allocatestatus, i, j, norientations_new
      real(kind=dp_real) :: ds, max_dist, scale
      character(len=100) :: junk

        if(sed_mode) then
            allocate(ephots(nephots),stat=allocatestatus)
                if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (ephots) array ***"
            allocate(ior_res(nephots),stat=allocatestatus)
                if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (ior_res) array ***"
            allocate(ior_ims(nephots),stat=allocatestatus)
                if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (ior_ims) array ***"
            

            do i=1,nephots
                ephots(i) = ephot_min + (i-1)*dephot
            end do
        end if 
      ! Set Euler angles -------------------------------
        if (euler_angle_mode == 'sequential') then
            norientations_new = int(real(norientations)**(1.0/3.0))**3
            if (norientations /= norientations_new) then
                write(stderr,*) ""
                write(stderr,*) "   + norientations: ",norientations,"-->",norientations_new
                norientations = norientations_new
            endif
        else if (euler_angle_mode == 'random') then
            call init_random_seed()

        else if (euler_angle_mode == 'file') then !check the contents of the euler angle file
            if (euler_angle_file == "") then
              write(stderr,*) "ERROR: euler_angle_mode of 'file', but specified file is empty"
              stop
            else
              line = 0
              open(10,file=euler_angle_file,iostat=status)
              if (status .ne. 0)then 
                write(stderr,*) "ERROR: euler_angle_file of ",euler_angle_file," does not exist."
                stop
              else
              do while(status == 0)
                read(10,'(A)',iostat=status) junk
                if (status == 0) line = line+1
              end do 
              if (line .ne. norientations) then
                if (line == 0) then
                    write(stderr,*) "ERROR: No orientations found in ", euler_angle_file,"!"
                    stop
                end if 
                write(stderr,*) "WARNING: norientations set to ",norientations," but there are ",line," orientations in ",&
                  euler_angle_file,". norientations -> ",line
                norientations = line 
              end if

              end if
              close(10) 
            end if 
        else 
            write(stderr,*) " ERROR: do not understand euler_angle_mode=",euler_angle_mode
            stop
        end if 

    ! ---------------------------------------------------

        if ((geometry == 'sphere') .or. (geometry == 'spheres')) then 
            if (geometry == 'spheres') then
                default_ior_i = ior_im
                default_ior_r = ior_re
                call read_spheres(cluster_file_name, a_eff, max_dist)
            else
                max_dist = a_eff
                norientations = 1
            end if 
            grain_a(1) = a_eff 
            grain_a(2) = a_eff
            grain_a(3) = a_eff

            ! make the grid a little larger to accomodate furthest part of grain
            grid_width = 2.05*max_dist/a_eff

        else if (geometry == 'ellipsoid') then
            scale = a_eff/((grain_a(1)*grain_a(2)*grain_a(3))**(1.0/3.0))
            ! normalize so that grain_a(1), (2), (3) produce an ellipse with an effective
            ! radius of a_eff
            grain_a(1) = grain_a(1)*scale
            grain_a(2) = grain_a(2)*scale
            grain_a(3) = grain_a(3)*scale

            ! make the grid a little larger than the longest principal axis
            grid_width = 2.05*max(grain_a(1), grain_a(2), grain_a(3))/a_eff
        else
            write(stderr,*) "ERROR: cannot understand ", geometry
            stop
        end if 


        
        
        max_angle = max_angle/arcseconds_per_radian ! convert max_angle from arcseconds to radians
        k = (2*pi/1.239842)*1000*ephot
        
        ngrid = ngrain ! Start with no padding 

        ! Add padding if use_padded_fft is asked for.
        if (use_padded_fft) then  
            ds = grid_width*a_eff/real(ngrain - 1)
            next = 0
            ng = ngrid
            L = (ng-1)*ds 
            n = int(ceiling(asin(max_angle)*L*k/(2*pi)))
            do while(n < nscatter .and. ng < max_ngrid)
                next = next + 1
                ng = ngrid + next
                L = (ng-1)*ds 
                n = int(ceiling(asin(max_angle)*L*k/(2*pi)))
            end do
            grid_width = ng*ds/a_eff
            ngrid = ng 
            if (n < nscatter) then
                write(stderr,*) "Warning: your request for an nscatter of ",&
                    nscatter," required too large a grid. Changing nscatter to ",n 
            else if (n > nscatter) then
                write(stderr,*) "Warning: your request for an nscatter of ",&
                    nscatter," is already exceeded by a non-padded grid. Changing nscatter to",n
            end if 

            nscatter = n 
        end if 
    ! ---------------------------------------------------
   
    ! allocate arrays that depend on ngrid

        ! x , y
        allocate(x(ngrid),stat = allocatestatus)
            if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (x) array ***"
        allocate(y(ngrid),stat = allocatestatus)
            if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (y) array ***"
        ! sh
        allocate(sh(ngrid,ngrid),stat = allocatestatus)
            if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (sh) array ***"
        if (sed_mode) then
         !   allocate(phi1(ngrid,ngrid),stat = allocatestatus)
         !       if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (phi1) array ***"
         !   allocate(phi2(ngrid,ngrid),stat = allocatestatus)
         !       if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (phi2) array ***"
            allocate(phi_total(ngrid,ngrid),stat = allocatestatus)
                if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (phi_total) array ***"
        end if 
    ! ----------------------------------------------------
      
    ! set grid boundaries and spacing in physical coords
        xmin = -grid_width*a_eff/2.0
        ymin = -grid_width*a_eff/2.0
        xmax = grid_width*a_eff/2.0
        ymax = grid_width*a_eff/2.0

        dx = (xmax-xmin)/(size(x)-1)
        dy = (ymax-ymin)/(size(y)-1)
    ! -------------------------
        
    ! set x and y arrays
        do i=1,size(x)
            x(i) = xmin + (i-1)*dx
        end do
        do i=1,size(y)
            y(i) = ymin + (i-1)*dy
        end do
    ! -------------------
      
        L = x(size(x)) - x(1)  ! length of grid in physical coords
        !write(stderr,*) "LENGTH: ",L

    ! figures out what kmin, kmax, enhancement and kactual are.
        call get_fft_vals(-max_angle,max_angle,nscatter,kmin,kmax,enhancement,kactual)   

      
      
    ! allocate arrays that depend on kactual

        ! kx
        allocate(kx(kactual),stat = allocatestatus)
            if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (kx) array ***"
        ! ky
        allocate(ky(kactual),stat = allocatestatus)
            if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (ky) array ***"
        ! thetax
        allocate(thetax(kactual),stat = allocatestatus)
            if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (thetax) array ***"
        ! thetay
        allocate(thetay(kactual),stat = allocatestatus)
            if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (thetay) array ***"
        ! ftsh
        allocate(ftsh(kactual,kactual),stat = allocatestatus)
            if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (thetay) array ***"
        ! scatter
        allocate(scatter(kactual,kactual),stat = allocatestatus)
            if (allocatestatus /= 0) stop "ERROR *** not enough memory to allocate (scatter) array ***"
    ! ----------------------------------

        dk = real(kmax-kmin)/real(size(kx) - 1)

    ! set kx, ky, thetax and thetay arrays
    !   + make scatter array all zero
        do i=1,size(kx)
            kx(i) = ((i-1)*dk + kmin)*(TWOPI/L)
            ky(i) = kx(i)

            thetax(i) = asin(kx(i)/k)
            thetay(i) = asin(ky(i)/k)
            do j=1,size(ky)
                scatter(i,j) = 0.0
            end do 
        end do
    ! -----------------------------------



    end subroutine initialize_and_allocate_vars

! -- given thetamin and thetamax and desired resolution in scattering angle, 
!    sets the relevant FFT parameters (kmin, kmax, enhancement, Kout)
    subroutine get_fft_vals(thetamin,thetamax,Kask,kmin,kmax,enhancement,Kout)
        real(kind=dp_real), intent(in) :: thetamin, thetamax
        integer, intent(in) :: Kask
        integer, intent(out) :: kmin,kmax,enhancement, Kout

        enhancement = 1

        kmin = int(floor(k*sin(thetamin)*(L/TWOPI)))
        kmax = int(ceiling(k*sin(thetamax)*(L/TWOPI)))

        Kout = kmax - kmin

        do while (enhancement*Kout + 1 < Kask) 
            enhancement = enhancement+1
        end do 

        Kout = enhancement*Kout + 1

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

    function get_new_euler_angles(i)
        real(kind=dp_real), dimension(3) :: get_new_euler_angles
        integer, intent(in) :: i
        integer                :: j 
        character(len=50)    :: junk
        integer :: cbrt_norientations, nx, ny, nz


        if (euler_angle_mode .eq. 'sequential') then 
            cbrt_norientations = int(norientations**(1.0/3.0))
            nx = mod(i/(cbrt_norientations*cbrt_norientations),   cbrt_norientations)
            ny = mod(i/(cbrt_norientations),               cbrt_norientations)
            nz = mod(i,                             cbrt_norientations) 
            get_new_euler_angles(1) = 2*pi*(nx-1)/cbrt_norientations
            get_new_euler_angles(2) = 2*pi*(ny-1)/cbrt_norientations
            get_new_euler_angles(3) = 2*pi*(nz-1)/cbrt_norientations
        else if ( euler_angle_mode .eq. 'file' ) then
            open(unit=1,file=euler_angle_file)
            if (i > 1) then 
                do j=1,i-1
                    read(1,'(A)') junk
                end do
            end if 
            read(1,*) get_new_euler_angles
            close(1)
        else
            call random_number(get_new_euler_angles)
            get_new_euler_angles(1) = 2*pi*get_new_euler_angles(1)
            get_new_euler_angles(2) = 2*pi*get_new_euler_angles(2) 
            get_new_euler_angles(3) = 2*pi*get_new_euler_angles(3)
        endif
    end function get_new_euler_angles

    subroutine cleanup()
        implicit none 
        integer :: deallocatestatus
        deallocate(x,stat=deallocatestatus)
        deallocate(y,stat=deallocatestatus)
        deallocate(kx,stat=deallocatestatus)
        deallocate(ky,stat=deallocatestatus)
        deallocate(thetax,stat=deallocatestatus)
        deallocate(thetay,stat=deallocatestatus)
        deallocate(sh,stat=deallocatestatus)
        deallocate(ftsh,stat=deallocatestatus)
        deallocate(scatter,stat=deallocatestatus)
        
        if (geometry .eq. 'spheres')    then
            deallocate(pos)
            deallocate(pos_rot)
            deallocate(radii)
            deallocate(ior_r)
            deallocate(ior_i)
        end if

    end subroutine cleanup

    
    subroutine print_parameters(i)
        implicit none 
        integer, intent(in) :: i
        write(i,*) "# Parameter values"
        write(i,*) "# -----------------------------------"
        if (.not. sed_mode) then 
            write(i,*) "# delm:",delm
            write(i,*) "# ephot:",ephot
            write(i,*) "# nscatter:",nscatter
            write(i,*) "# ior_re:",ior_re
            write(i,*) "# ior_im:",ior_im
        else 
            write(i,*) "# ephot_min:",ephot_min
            write(i,*) "# ephot_max:",ephot_max
            write(i,*) "# nephots:",nephots
            write(i,*) "# Material file: '",trim(adjustl(material_file)),"'"
        end if 
        write(i,*) "# geometry:", geometry
        write(i,*) "# euler_angle_mode:",euler_angle_mode
        if (euler_angle_mode == "file") write(i,*) "# euler_angle_file:", euler_angle_file
        write(i,*) "# ngrain:",ngrain
        write(i,*) "# norientations:",norientations
        write(i,*) "# grain_axes:",grain_a
        write(i,*) "# use_padded_fft:",use_padded_fft
        write(i,*) "# parameter_file_name:",parameter_file_name
        write(i,*) "# fftw_optimization_mode_name:",fftw_optimization_mode_name
        if (geometry == "spheres") write(i,*) "# cluster_file_name:",cluster_file_name
        write(i,*) "# -----------------------------------"
    

    end subroutine print_parameters

    

end program ggadt

