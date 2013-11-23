
INTEGER, PARAMETER 		:: 	NGRID 		= 2048
CHARACTER, PARAMETER 	::	GEOMETRY 	= "SPHEROID"
! If GEOMETRY == SPHERE:
REAL, PARAMETER			:: 	GRAIN_A 	= 0.2

! If GEOMETRY == SPHEROID:
! ===============================================
! REAL, PARAMETER			::	GRAIN_A 	= 0.2
! REAL, PARAMETER			::	GRAIN_B 	= 0.2
! REAL, PARAMETER			::	GRAIN_C 	= 0.2

! if GEOMETRY == GRID:
! ===============================================
! CHARACTER, PARAMETER		:: 	GRID_FILE 	= "ggadt_grid.dat"
! REAL, PARAMETER			::	

! if GEOMETRY == COLLECTION_OF_SPHERES
! CHARACTER, PARAMETER		:: SPHERES_FILE = "ggadt_spheres.dat"
! INTEGER, PARAMETER		:: 