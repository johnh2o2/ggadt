module spheres

    ! use, intrinsic :: iso_c_binding
    use constants
    use sphere
    
    
    save
    integer :: nspheres, nrots, ok, n
    integer :: MIGRATE, ISEED, NS
    real(kind=dp_real) :: ALPHA(3), VTOT, A_1(3), A_2(3)
    real(kind=dp_real) :: default_ior_r, default_ior_i
    real(kind=dp_real), allocatable :: radii(:), ior_r(:), ior_i(:)
    real(kind=dp_real), allocatable :: pos(:,:), pos_rot(:,:)
    real(kind=dp_real) :: dxt, dyt, a_eff_temp, conv 
    
    character(len=1000) :: junk
    
    contains

    subroutine read_spheres(cluster_file_name,a_eff,max_dist)
        implicit none
        real(kind=dp_real) :: v = 0.0, dist
        integer  :: i, allocatestatus
        character(len=200), intent(in) :: cluster_file_name
        real(kind=dp_real), intent(in) :: a_eff
        real(kind=dp_real), intent(out) :: max_dist

        open(unit=1,file=cluster_file_name) ! open sphere file.

        ! Read header
        read(1,'(52X,i2,7X,i4)') MIGRATE,ISEED
        read(1,'(i9,f12.2,3f11.6)') NS, VTOT, ALPHA(1), ALPHA(2), ALPHA(3)
        read(1,'(3f10.6,13X)') A_1(1), A_1(2), A_1(3)
        read(1,'(3f10.6,13X)') A_2(1), A_2(2), A_2(3)
        read(1,'(A)') junk

        !DEBUGGING -- print header information
        !write(0,*) "DEBUG: SPHERES HEADER"
        !write(0,*) MIGRATE, ISEED
        !write(0,*) NS, VTOT, ALPHA(1), ALPHA(2), ALPHA(3)
        !write(0,*) A_1(1), A_1(2), A_1(3)
        !write(0,*) A_2(1), A_2(2), A_2(3)
        !write(0,*) "DEBUG (END)"
        !END DEBUGGING

        nspheres = NS

        ! allocate necessary memory
        allocate(pos(nspheres,3),stat = allocatestatus)
        if (allocatestatus /= 0) stop "*** not enough memory (pos) ***"
        allocate(pos_rot(nspheres,3),stat = allocatestatus)
        if (allocatestatus /= 0) stop "*** not enough memory (pos_rot) ***"
        allocate(radii(nspheres),stat = allocatestatus)
        if (allocatestatus /= 0) stop "*** not enough memory (radii) ***"
        allocate(ior_r(nspheres),stat = allocatestatus)
        if (allocatestatus /= 0) stop "*** not enough memory (ior_r) ***"
        allocate(ior_i(nspheres),stat = allocatestatus)
        if (allocatestatus /= 0) stop "*** not enough memory (ior_i) ***"

        ! read in spheres
       
        do i=1,nspheres
            read(1, *) n, pos(i,1), pos(i,2), pos(i,3), radii(i) 
            ior_r(i) = default_ior_r
            ior_i(i) = default_ior_i
            radii(i) = radii(i)/2.0
            v = v+ (4.0*pi/3.0)*(radii(i)**3)  


            !write(0,*) n, pos(i,1), pos(i,2), pos(i,3), radii(i) 
        end do 

        close(1)

        ! now normalize
        a_eff_temp = ((3*v)/(4*pi))**(1.0/3.0)
        conv = a_eff/a_eff_temp

        !write (0,*) "SPHERES (read_spheres) a_eff/a_eff_temp = ", conv

        v=0.0

        
        max_dist = 0.0
        do i=1,nspheres
            pos(i,1) = pos(i,1)*conv
            pos(i,2) = pos(i,2)*conv 
            pos(i,3) = pos(i,3)*conv
            radii(i) = radii(i)*conv   

            dist = sqrt(pos(i,1)*pos(i,1) + pos(i,2)*pos(i,2) + pos(i,3)*pos(i,3) ) + radii(i)
            if (dist > max_dist) max_dist = dist

            pos_rot(i,1) = pos(i,1)
            pos_rot(i,2) = pos(i,2)
            pos_rot(i,3) = pos(i,3)
            
            v = v+ (4.0*pi/3.0)*(radii(i)**3) 
        end do
        !write(0,*) "DEBUG SPHERES: a_eff = ",a_eff," check == ", ((3*v)/(4*pi))**(1.0/3.0) ! (Spheres are correcting a_eff OK. 2/25)
        ! working
    end subroutine read_spheres

    function phi_spheres(x,y,k)
        implicit none
        real(kind=dp_real), intent(in) :: x(:),y(:)
        real(kind=dp_real), dimension(3) :: current_pos
        real(kind=dp_real), intent(in) :: k
        complex(kind=dp_complex) :: m 
        complex(kind=dp_complex), dimension(size(x),size(y)) :: phi_spheres
        integer :: i, j, n ,xi, xf, yi,yf

        dxt = x(2) - x(1)
        dyt = y(2) - y(1)

        do i=1,size(x)
            do j=1,size(y)
                phi_spheres(i,j) = (0.0d0, 0.0d0)
            end do
        end do
        do i=1, nspheres
            ! write (0,*) " | SPHERES (phi_spheres) : doing sphere", i, " of ", nspheres
            ! only modify the relevant section of the phi grid
            ! indices: i = [xi,xj], j = [yi,yj]
            m = cmplx(ior_r(i), ior_i(i)) ! ior - 1
            !goto 99
            xi = int((pos_rot(i,1) - radii(i) - x(1))/dxt) + 1
            xf = int((pos_rot(i,1) + radii(i) - x(1))/dxt) + 1
            yi = int((pos_rot(i,2) - radii(i) - y(1))/dyt) + 1
            yf = int((pos_rot(i,2) + radii(i) - y(1))/dyt) + 1

            
            !write(0,*) "doing shortcut"
            ! write (0,*) " | ---SPHERES (phi_spheres) : xi,xf, yi,yf=",xi,xf,yi,yf
            if (yf < 0 .or. yf > size(y)) then
                write (0,*) "ERROR (spheres module, phi_spheres function) yf = ",yf," but size(y)=",size(y)
            else if (yi < 0 .or. yi > size(y)) then
                write (0,*) "ERROR (spheres module, phi_spheres function) yi = ",yi," but size(y)=",size(y)
            else if (xi < 0 .or. xi > size(x)) then
                write (0,*) "ERROR (spheres module, phi_spheres function) xi = ",xi," but size(x)=",size(x)
            else if (xf < 0 .or. xf > size(x)) then
                write (0,*) "ERROR (spheres module, phi_spheres function) xf = ",xf," but size(x)=",size(x)
            end if 
            do j=xi,xf
                do n=yi,yf
                    current_pos = (/ x(j), y(n), real(0.0,kind=dp_real) /)
                    phi_spheres(j,n) = phi_spheres(j,n)+ k*m*chord_sphere(current_pos, pos_rot(i,:), radii(i))
                end do
            end do 
            !99 do j=1,size(x)
                !if (j == 1) write (0,*) "doing whole grid!"
            !    do n=1,size(y)
            !        current_pos = (/ x(j), y(n), real(0.0,kind=dp_real) /)
            !        phi_spheres(j,n) = phi_spheres(j,n)+ k*m*chord_sphere(current_pos, pos_rot(i,:), radii(i))
            !    end do 
            !end do

        end do 
        
    end function phi_spheres

    function shadow_spheres(x,y,k,rm)
        implicit none
        real(kind=dp_real), intent(in) :: x(:),y(:)
        real(kind=dp_real), dimension(3,3), intent(in) :: rm
        real(kind=dp_real), intent(in) :: k 
        complex(kind=dp_complex), dimension(size(x),size(y)) :: shadow_spheres, phi
        integer :: i, j

        ! write (0,*) "SPHERES (shadow_spheres) : in shadow_spheres" 
        ! rotate sphere positions by the specified euler angles
        do i=1,nspheres
            pos_rot(i,:) = matmul(rm, pos(i,:))
        end do
        
        ! write (0,*) "SPHERES (shadow_spheres) : rotated positions" 

        phi = phi_spheres(x,y,k) ! paint phi grid
        !$omp parallel shared(shadow_spheres,phi) 
        !$omp do schedule(dynamic) private(i,j)

        ! write (0,*) "SPHERES (shadow_spheres) : painted phi grid" 
        do i=1,size(x)
            do j=1,size(y) 
                shadow_spheres(i,j) = 1-exp( (0.0,1.0)*phi(i,j) )
            end do
        end do 
        !$omp end do nowait
        !$omp end parallel
        
    end function shadow_spheres

end module spheres 
