module common_mod
  use, intrinsic :: iso_c_binding
  implicit none
  save
  real, parameter               ::  pi          = 3.14159
  complex(c_double_complex)     ::  delm 
 
  real                          ::  a_eff     
  real                          ::  ephot     
  character(len=100)            ::  geometry  
  character(len=100)            ::  euler_angle_mode


  integer                       ::  ngrid 
  integer                       ::  nangle 

  real                          ::  box_width  
  real                          ::  ior_im        
  real                          ::  ior_re     
  real, dimension(3)            ::  grain_a 
  logical                       ::  mpi_mode   
  character(len=10)             ::  fft_type
  character(len=100)            ::  fftw_optimization_mode_name
  character(len=100)            ::  sphlist_fname

  contains
    subroutine read_param_file(fname)
      implicit none
      character(len=*), intent(in)::fname
      character(len=300) :: buffer, label
      integer :: pos,pos_end,k
      integer, parameter :: fh = 15
      integer :: ios = 0
      integer :: line = 0
      print *,"#=-=-=-=-=-=-ggadt version 0.15-=-=-=-=-=-="
      print *,"#                parameters                "
      print *,"#/----------------------------------------\"
      do while (ios == 0)


      ! input related variables
     
        open(fh, file=fname)

        read(fh, '(A)', iostat=ios) buffer
        if (ios == 0) then
          line = line + 1

          ! find the first instance of whitespace.  split label and data.
          pos = scan(buffer, '    ')
          label = strlowcase(buffer(1:pos))
          !label = strlowcase(label)
          !print *,label,strlowcase(buffer)
          buffer = buffer(pos+1:)
          pos_end = scan(buffer,';')
          buffer = buffer(1:pos_end-1)
          
          select case (label)
          case ('ngrid')
             read(buffer, *, iostat=ios) ngrid
             print '(a8,i27)', '# ngrid',ngrid
          case ('nangle')
             read(buffer, *, iostat=ios) nangle
             print '(a9,i25)', '# nangle',nangle
          case ('box_width')
             read(buffer, *, iostat=ios) box_width
             print '(a12,f25.3,a6)', '# box_width',box_width,'*a_eff'
          case ('ior_re')
             read(buffer, *, iostat=ios) ior_re
             print '(a9,e31.3)', '# ior_re',ior_re
          case ('ior_im')
             read(buffer, *, iostat=ios) ior_im
             print '(a10,e30.3)', '# ior_im ',ior_im
          case ('grain_a')
             read(buffer, *, iostat=ios) grain_a
             print '(a11,a18,3f7.3)', '# grain_a ',' ',grain_a
          case ('ephot')
             read(buffer, *, iostat=ios) ephot
             print '(a9,f27.3,a4)', '# ephot ',ephot,'kev'
          case ('a_eff')
             read(buffer, *, iostat=ios) a_eff
            print '(a9,f27.3,a12)', '# a_eff ',a_eff,' micrometers'
          case ('geometry')
             read(buffer, *, iostat=ios) geometry
             geometry = strlowcase(geometry)
             print '(a12,a49)', '# geometry ',geometry
          case ('fftw_optimization_mode')
             read(buffer, *, iostat=ios) fftw_optimization_mode_name
             fftw_optimization_mode_name = strlowcase(fftw_optimization_mode_name)
             print '(a12,a49)', '# fftw_optimization_mode ',fftw_optimization_mode_name
          case ('euler_angle_mode')
             read(buffer, *, iostat=ios) euler_angle_mode
             euler_angle_mode = strlowcase(euler_angle_mode)
             print '(a20,a17)','# euler_angle_mode ',trim(adjustl(euler_angle_mode))
          case ('mpi_mode')
             read(buffer, *, iostat=ios) mpi_mode
             if (mpi_mode) then
                print '(a12,a25)', '# mpi_mode ','true'
             else 
                print '(a12,a24)', '# mpi_mode ','false'
             end if
          case ('fft_type')
             read(buffer, *, iostat=ios) fft_type
             print '(a12,a29)', '# fft_type ',fft_type
          case ('sphlist_fname')
             read(buffer, *, iostat=ios) sphlist_fname
              print '(a17,a34)','# sphlist_fname ',trim(adjustl(sphlist_fname))
          case default
             print *, '#**[error]** skipping (',trim(adjustl(label)),'), line', line
          end select
        end if
      end do
      print *,"#\---------------------------------------/"
      print *,"  "
      delm = cmplx(ior_re, ior_im)
      close(fh)
    end subroutine read_param_file

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