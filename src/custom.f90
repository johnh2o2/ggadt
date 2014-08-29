module custom

    use constants
    use interp
    ! use omp_lib ! uncomment "use omp_lib" to activate openmp tags for faster (parallel) execution

    ! TODO: add rotation matrix speedup; for each orientation, set the inverse rotation matrix in here so
    ! you don't have to recalculate it all the time

    ! Also, hardcode the inverse rotation matrix in terms of the rotation angles -- that should be faster to compute.

    contains

    function mag(x,y,z)

        implicit none 

        real(kind=dp_real) :: mag
        real(kind=dp_real),intent(in) ::x,y,z

        mag = sqrt(x**2.0 + y**2.0 + z**2.0)

    end function mag

    function ior_custom(x,y,z,en,ephots,ior_res,ior_ims,a)
        real(kind=dp_real), intent(in) :: x,y,z,en
        real(kind=dp_real), intent(in) :: ephots(:)
        real(kind=dp_real), intent(in) :: ior_res(:,:), ior_ims(:,:)
        complex(kind=dp_complex) :: ior_custom

        ! write code for your own grain geometry here.
        ! 
        ! This function is currently set up to do a sphere
        ! 

        ! inputs : 
        !          x,y,z               cartesian coordinates of the grain in the DDSCAT axis convention
        !
        !          en                  photon energy at which the index of refraction is requested
        !
        !          ephots              an array of photon energies to use for interpolation purposes
        !
        !          ior_res, ior_ims    The real and imaginary components of the index of refraction for
        !                              the material(s) specified. 
        !
        !          a                   The effective radius of the grain
        !
        ! note: the ephots and ior_res/ior_ims arrays are organized such that 
        !        
        !                   m - 1 = ior_res(i,j) + sqrt(-1) * ior_ims(i,j) 
        !
        !       is the index of refraction (minus one) for material number "i" at a photon
        !       energy of ephots(j). Linear interpolation can be used to obtain m-1 for arbitrary 
        !       photon energies.

        real(kind=dp_real), intent(in) :: a 
        
        if (mag(x,y,z) < a) then
            if (size(ior_res(1,:)) .eq. 1) then 
                ior_custom = CMPLX(ior_res(1,1),ior_ims(1,1))
            else
                ior_custom = CMPLX(linterp(ephots,ior_res(1,:),en),linterp(ephots,ior_ims(1,:),en) )
            end if 
        else
            ior_custom = CMPLX(0.0,0.0)
        end if 

    end function ior_custom

    function det(mat)
        implicit none
        real(kind=dp_real) :: det
        real(kind=dp_real), intent(in) :: mat(3,3)
        integer :: i,j

        det = mat(1,1)*(mat(2,2)*mat(3,3) - mat(3,2)*mat(2,3)) &
            - mat(1,2)*(mat(2,1)*mat(3,3) - mat(3,1)*mat(2,3)) &
            + mat(1,3)*(mat(2,1)*mat(3,2) - mat(3,1)*mat(2,2))

    end function det


    function inverse(mat)
        implicit none
        real(kind=dp_real), dimension(3,3), intent(in) :: mat
        real(kind=dp_real), dimension(3,3) :: inverse
        real(kind=dp_real) :: detm

        detm = det(mat)

        inverse(1,1) = (mat(2,2)*mat(3,3) - mat(3,2)*mat(2,3))/detm
        inverse(1,2) = (mat(1,3)*mat(3,2) - mat(3,3)*mat(1,2))/detm
        inverse(1,3) = (mat(1,2)*mat(2,3) - mat(2,2)*mat(1,3))/detm

        inverse(2,1) = (mat(2,3)*mat(3,1) - mat(3,3)*mat(2,1))/detm
        inverse(2,2) = (mat(1,1)*mat(3,3) - mat(3,1)*mat(1,3))/detm
        inverse(2,3) = (mat(1,3)*mat(2,1) - mat(2,3)*mat(1,1))/detm

        inverse(3,1) = (mat(2,1)*mat(3,2) - mat(3,1)*mat(2,2))/detm
        inverse(3,2) = (mat(1,2)*mat(3,1) - mat(3,2)*mat(1,1))/detm
        inverse(3,3) = (mat(1,1)*mat(2,2) - mat(2,1)*mat(1,2))/detm

    end function inverse


    function phi_custom(z,y,k,rm,nx,en,ephots,ior_res,ior_ims)
        implicit none
        real(kind=dp_real), intent(in) :: z(:),y(:)
        real(kind=dp_real), dimension(3) :: current_pos
        real(kind=dp_real), intent(in) :: ephots(:)
        real(kind=dp_real), intent(in) :: ior_res(:,:),ior_ims(:,:)
        real(kind=dp_real), dimension(3,3) :: invrm
        real(kind=dp_real), intent(in) :: k,  en
        real(kind=dp_real) :: xmin, xmax
        integer, intent(in) :: nx
        complex(kind=dp_complex) :: m 
        real(kind=dp_real), intent(in), dimension(3,3) :: rm
        complex(kind=dp_complex), dimension(size(z),size(y)) :: phi_custom
        real(kind=dp_real) :: dx, x
        integer :: i, j, n

        xmin = minval(z)
        xmax = maxval(z)

       

        dx = (xmax - xmin)/(real(nx) - 1.0)

        ! initialize array
        do i=1,size(z)
            do j=1,size(y)
                phi_custom(i,j) = (0.0d0, 0.0d0)
            end do
        end do

        invrm = inverse(rm)


        !$omp parallel shared(invrm,dx,xmin,phi_custom,k) 
        !$omp do schedule(dynamic) private(i,j,n,current_pos,m)
        do i=1,size(z)
            
            do j=1,size(y)
               
                do n=1, nx
                    x = xmin + (n-1)*dx
                    current_pos(1) = x
                    current_pos(2) = y(j)
                    current_pos(3) = z(i)
                    
                    current_pos = matmul(invrm,current_pos)

                    m = ior_custom(current_pos(1),current_pos(2),current_pos(3),en,ephots,ior_res,ior_ims,0.5*(xmax-xmin))

                    phi_custom(i,j) = phi_custom(i,j) + k*m*dx


                    

                end do 
            end do 
        end do 
        !$omp end do nowait
        !$omp end parallel
    end function phi_custom

  

    function shadow_custom(z,y,k,rm,nx,en,ephots,ior_res,ior_ims)
        implicit none
        real(kind=dp_real), intent(in) :: z(:),y(:)
        real(kind=dp_real), intent(in) :: ior_res(:,:), ior_ims(:,:)
        real(kind=dp_real), intent(in) :: k,en
        real(kind=dp_real), intent(in) :: ephots(:)
        real(kind=dp_real) :: xmin,xmax
        complex(kind=dp_complex), dimension(size(z),size(y)) :: shadow_custom, phi
        real(kind=dp_real), intent(in), dimension(3,3) :: rm
        integer, intent(in) :: nx

        integer :: i, j

        phi = phi_custom(z,y,k,rm,nx,en,ephots,ior_res,ior_ims) ! paint phi grid
        
        !$omp parallel shared(shadow_custom,phi) 
        !$omp do schedule(dynamic) private(i,j)
        do i=1,size(z)
            do j=1,size(y) 
                shadow_custom(i,j) = 1-exp( (0.0,1.0)*phi(i,j) )
            end do
        end do 
        !$omp end do nowait
        !$omp end parallel
        
    end function shadow_custom

end module custom