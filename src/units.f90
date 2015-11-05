module units
	implicit none
	real(8), parameter                 :: pi                 = 3.14159265358979323846
	private 
	public :: Unit, read_units, translate_kind_of_unit, T_ANGLE, T_LENGTH, T_ENERGY

	integer, parameter :: T_ANGLE = 0
	integer, parameter :: T_LENGTH = 1
	integer, parameter :: T_ENERGY = 2


	type Unit
		character(len=100) :: name;
		character(len=100) :: abbreviation;
		integer :: kind_of_unit;
		real(8) :: value;
	end type Unit


	contains

	function translate_kind_of_unit(koustr)
		implicit none
		character(len=100), intent(in) :: koustr
		integer :: translate_kind_of_unit

		if ( trim(adjustl(koustr)) == 'LENGTH' ) then 
			translate_kind_of_unit = T_LENGTH
		else if ( trim(adjustl(koustr)) == 'ANGLE' ) then 
			translate_kind_of_unit = T_ANGLE
		else if ( trim(adjustl(koustr)) == 'ENERGY' ) then 
			translate_kind_of_unit = T_ENERGY
		else
			write(0, *) " ** Don't understand ", koustr, " ** (units.f90)"
			stop 1
		end if

	end function translate_kind_of_unit

	subroutine read_units(units_file, all_units, nunits)
		implicit none
		character(len=300), intent(in) :: units_file
		type(Unit), dimension(:), allocatable, intent(out) :: all_units
		integer :: stat, i
		logical :: exist
		integer, intent(out) :: nunits
		real(8) :: unit_value
		character(len=100) :: unit_name, unit_abbreviation, buf, kind_of_unit_str

		
		inquire(file=trim(adjustl(units_file)), exist=exist)
		if (.not. exist) then
			stop " ** units file does not exist!! ** (units.f90)"
		end if

		open(unit=15, file=trim(adjustl(units_file)), iostat=stat)
		if (stat /= 0) then
			stop " ** cannot read units file ** (units.f90)"
		end if 

		nunits = 0
		do
			read(15, '(A)', iostat=stat) buf
			! NON-eof error
			if (stat > 0) then
				stop " ** error reading units file...don't have any more info ** (units.f90)"
			end if

			! eof!
			if (stat < 0) exit

			nunits = nunits + 1
		end do
		close(15)

		allocate(all_units(nunits), stat=stat)
		if (stat /= 0) then
			stop " ** not enough memory to allocate the Unit objects ** (units.f90)"
		end if

		open(unit=15,file=trim(adjustl(units_file)), iostat=stat)
		if (stat /= 0) then
			stop " ** cannot read units file (second time around) ** (units.f90)"
		end if 

		do i=1,nunits
        	read(15,*,iostat=stat) kind_of_unit_str, unit_name, unit_abbreviation, unit_value

        	all_units(i)%kind_of_unit = translate_kind_of_unit(kind_of_unit_str)
        	all_units(i)%name = unit_name
        	all_units(i)%abbreviation = unit_abbreviation
        	all_units(i)%value = unit_value

        end do
        close(15)

	end subroutine read_units
end module units

!use units
!implicit none
!
!character(len=300) :: filename
!type(Unit), dimension(:), allocatable :: all_units
!integer :: nunits, i
!
!write(6, * ) "Type the filename"
!read(*,'(A)') filename
!
!call read_units(filename, all_units, nunits)
!
!do i=1,nunits
!write(6, *) all_units(i)%name
!end do
!
!
!
!end program test_module_units