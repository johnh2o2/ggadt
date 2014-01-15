! ggadt -- general geometry anomalous diffraction theory
!
!      | this code takes a parameter file as input, and prints
!      | dqscat/domega as a function of thetax and thetay

program ggadt
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env
    use sphere
    use spheres
    use ellipsoid
    use common_mod
    use fftw
    implicit none

    
    real, allocatable :: x(:), y(:), z(:), kx(:), ky(:), thetax(:), thetay(:)
    complex(c_double_complex), allocatable :: sh(:,:), ftsh(:,:)
    real, allocatable :: chrd(:,:), scatter(:,:), scatter_temp(:,:)

    real :: xmin, xmax, ymin, ymax, zmin, zmax, dx, dy, dz, k, l
    real, dimension(3) :: eul_ang
    real, dimension(3,3) :: rm 
    integer :: i, j, neul, allocatestatus, num_args, nangle_new


    call set_parameter_values()
    call initialize_and_allocate_vars()
    call set_optimization_mode(fftw_optimization_mode_name)
    call print_parameters()
    

    if (geometry .eq. "sphere") then
        do i=1,size(x)
            do j=1,size(y)
                sh(i,j) = shadow_sphere(x(i),y(j),zmax, k )*(-1.0)**(i+j+1)
            end do
        end do

        ftsh = fft(sh,x,y)

        do i=1,size(x)
            do j=1,size(y)
                scatter(i,j) = scatter(i,j) + abs((k*ftsh(i,j)*dx*dy))**2/(4*pi*(pi*a_eff)**2)
            end do
        end do
    else
        do neul = 1,nangle
            eul_ang = get_new_euler_angles(neul)
            rm = matmul(matmul(rot_x(eul_ang(1)), rot_y(eul_ang(2))),rot_z(eul_ang(3)))
            

            if (geometry .eq. 'ellipsoid') then 
                do i=1,size(x)
                    do j=1,size(y)
                        sh(i,j) = shadow_ellipsoid(x(i),y(j), k, rm )*(-1.0)**(i+j+1) 
                    end do
                end do
            end if 

            if (geometry .eq. 'spheres')    then
                sh = shadow_spheres(x,y,k,rm)
            end if 

            ftsh = fft(sh,x,y)

            do i=1,size(x)
                do j=1,size(y)
                    scatter(i,j) = scatter(i,j) + abs((k*ftsh(i,j)*dx*dy))**2/(4*pi*(pi*a_eff)**2)
                end do
            end do

            write(0,fmt="(a1,a,t21,f6.2,a)",advance="no") achar(13), &
                    & " percent complete: ", (real(neul)/real(nangle))*100.0, "%"
        end do
    end if 
 
    if (geometry /= 'sphere') then
        do i=1,size(x)
            do j=1,size(y)
                scatter(i,j) = scatter(i,j)/real(nangle)
            end do
        end do
    end if 

    print *, "# [thetax] [thetay] [dqscat/domega]"
    do i=1,size(x)
        do j=1,size(y)
            print *,thetax(i),thetay(j),scatter(i,j)
        end do
    end do
    write(0,*) "done."

    call cleanup()
contains

    function get_k(x)
        implicit none
        real, intent(in) :: x(:)
        integer :: n, i, j 
        real :: l
        real, dimension(size(x)) :: get_k
        n = size(x)
        l = x(n) - x(1)
        do i=1,n
            get_k(i) = (2*pi/l)*(i-0.5*(n+1)-0.5)
        end do
    end function get_k

    subroutine initialize_and_allocate_vars()
        allocate(x(ngrid),stat = allocatestatus)
        if (allocatestatus /= 0) stop "*** not enough memory (x) ***"
        allocate(y(ngrid),stat = allocatestatus)
        if (allocatestatus /= 0) stop "*** not enough memory (y) ***"
        allocate(z(ngrid),stat = allocatestatus)
        if (allocatestatus /= 0) stop "*** not enough memory (z) ***"
        allocate(kx(ngrid),stat = allocatestatus)
        if (allocatestatus /= 0) stop "*** not enough memory (kx) ***"
        allocate(ky(ngrid),stat = allocatestatus)
        if (allocatestatus /= 0) stop "*** not enough memory (ky) ***"
        allocate(thetax(ngrid),stat = allocatestatus)
        if (allocatestatus /= 0) stop "*** not enough memory (thetax) ***"
        allocate(thetay(ngrid),stat = allocatestatus)
        if (allocatestatus /= 0) stop "*** not enough memory (thetay) ***"
        allocate(sh(ngrid,ngrid),stat = allocatestatus)
        if (allocatestatus /= 0) stop "*** not enough memory (sh) ***"
        allocate(ftsh(ngrid,ngrid),stat = allocatestatus)
        if (allocatestatus /= 0) stop "*** not enough memory (ftsh) ***"
        allocate(chrd(ngrid,ngrid),stat = allocatestatus)
        if (allocatestatus /= 0) stop "*** not enough memory (chrd) ***"
        allocate(scatter(ngrid,ngrid),stat = allocatestatus)
        if (allocatestatus /= 0) stop "*** not enough memory (scatter) ***"
        allocate(scatter_temp(ngrid,ngrid),stat = allocatestatus)
        if (allocatestatus /= 0) stop "*** not enough memory (scatter_temp) ***"

        if (euler_angle_mode == 'sequential') then
            nangle_new = int(real(nangle)**(1.0/3.0))**3
            if (nangle /= nangle_new) then
                write (error_unit,*) ""
                write (error_unit,*) "   + nangle: ",nangle,"-->",nangle_new
                nangle = nangle_new
            endif
        else if (euler_angle_mode == 'random') then
            call init_random_seed()
        else 
            write(error_unit,*) " ***error: do not understand euler_angle_mode=",euler_angle_mode
            call exit()
        end if 

        if ((geometry == 'sphere') .or. (geometry == 'spheres')) then 
            if (geometry == 'spheres') then
                call read_spheres()
            end if 
            grain_a(1) = a_eff 
            grain_a(2) = a_eff
            grain_a(3) = a_eff
        else if (geometry == 'ellipsoid') then
            l = a_eff/((grain_a(1)*grain_a(2)*grain_a(3))**(1.0/3.0))
            ! normalize so that grain_a(1), (2), (3) produce an ellipse with an effective
            ! radius of a_eff
            grain_a(1) = grain_a(1)*l
            grain_a(2) = grain_a(2)*l
            grain_a(3) = grain_a(3)*l
        else
            print *,"cannot understand ", geometry
            call exit()
        end if 
        

        k = (2*pi/1.239842)*1000*ephot

        xmin = -box_width*a_eff/2.0
        ymin = -box_width*a_eff/2.0
        zmin = -2*a_eff
        xmax = box_width*a_eff/2.0
        ymax = box_width*a_eff/2.0
        zmax = 2*a_eff

        dx = (xmax-xmin)/(size(x)-1)
        dy = (ymax-ymin)/(size(y)-1)
        dz = (zmax-zmin)/(size(z)-1)

        

        do i=1,size(x)
            x(i) = xmin + (i-1)*dx
        end do
        do i=1,size(y)
            y(i) = ymin + (i-1)*dy
        end do
        do i=1,size(z)
            z(i) = zmin + (i-1)*dz
        end do
        kx = get_k(x)
        ky = get_k(y)
        do i=1,size(kx)
            thetax(i) = asin(kx(i)/k)
            thetay(i) = asin(ky(i)/k)
        end do      
    end subroutine initialize_and_allocate_vars

    subroutine init_random_seed()
        use, intrinsic :: iso_fortran_env
        implicit none

        integer, allocatable :: seed(:)
        integer(4) :: i, n, un, istat, dt(8), pid, t(2), s
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
           pid = GETPID() + 1099279 ! add a prime
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
        real, dimension(3) :: get_new_euler_angles
        integer, intent(in) :: i
        integer :: cbrt_nangle, nx, ny, nz


        if (euler_angle_mode .eq. 'sequential') then 
            cbrt_nangle = int(nangle**(1.0/3.0))
            nx = mod(i/(cbrt_nangle*cbrt_nangle),   cbrt_nangle)
            ny = mod(i/(cbrt_nangle),               cbrt_nangle)
            nz = mod(i,                             cbrt_nangle) 
            get_new_euler_angles(1) = 2*pi*(nx-1)/cbrt_nangle
            get_new_euler_angles(2) = 2*pi*(ny-1)/cbrt_nangle
            get_new_euler_angles(3) = 2*pi*(nz-1)/cbrt_nangle
        else
            call random_number(get_new_euler_angles)
            get_new_euler_angles(1) = 2*pi*get_new_euler_angles(1)
            get_new_euler_angles(2) = 2*pi*get_new_euler_angles(2) 
            get_new_euler_angles(3) = 2*pi*get_new_euler_angles(3)
        endif
    end function get_new_euler_angles

    subroutine cleanup()
        deallocate(x)
        deallocate(y)
        deallocate(z)
        deallocate(kx)
        deallocate(ky)
        deallocate(thetax)
        deallocate(thetay)
        deallocate(sh)
        deallocate(ftsh)
        deallocate(scatter)

        deallocate(scatter_temp)
        deallocate(chrd)
        

        if (geometry .eq. 'spheres')    then
            deallocate(pos)
            deallocate(pos_rot)
            deallocate(radii)
            deallocate(ior_r)
            deallocate(ior_i)
        end if
    end subroutine cleanup


end program ggadt

