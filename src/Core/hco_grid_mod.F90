!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: hco_grid_mod.F90
!
! !DESCRIPTION: Module HCO\_GRID\_MOD contains routines to manage gridded
!  (or potentially ungridded) data in HEMCO by marking them with a HcoGrid
!  derived type object describing their composition.
!\\
!  This module provides utility functions for grid comparison. For interpolation
!  you can use the environment-independent interpolation methods in 3.0's new
!  HCO\_INTERP\_MOD regrid routines, which will compare grids for you and
!  perform necessary interpolation.
!\\
! !INTERFACE:
!
MODULE HCO_Grid_Mod
!
! !USES:
!
  USE HCO_Types_Mod
  USE HCO_Error_Mod

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:
!
  PUBLIC  :: Compare_Grid
!
! !REVISION HISTORY:
!  13 Jan 2020 - H.P. Lin  - First version for HEMCO v3
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !PRIVATE VARIABLES:
!
CONTAINS
!EOC
!------------------------------------------------------------------------------
!                   Harmonized Emissions Component (HEMCO)                    !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: Compare_Grid
!
! !DESCRIPTION: Function Compare\_Grid compares the input and output HcoGrid
!  data structures. This used to determine regridding.
!\\
!  By default this routine compares only the horizontal grid. If comparing the
!  vertical, set CompareVert to .true.
!  Comparing the vertical is generally more resource intensive, and not needed.
!\\
!\\
! !INTERFACE:
!
  FUNCTION Compare_Grid ( am_I_Root, GridA, GridB, CompareVert ) RESULT ( IsEqual )
!
! !USES:
!

!
! !INPUT PARAMETERS:
!
    LOGICAL,                     :: am_I_Root   ! Root CPU?
    TYPE(HcoGrid),    INTENT(IN) :: GridA       ! Compare GridA to
    TYPE(HcoGrid),    INTENT(IN) :: GridB       ! ...GridB
    LOGICAL, OPTIONAL            :: CompareVert ! Compare in the vertical?
!
! !RETURN VALUE:
!
    LOGICAL                      :: IsEqual
!
! !REVISION HISTORY:
!  14 Jan 2020 - H.P. Lin    - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
    LOGICAL                      :: CompareVert_
    !======================================================================
    ! Compare_Grid begins here
    !======================================================================
    
    ! Not equal unless proven so
    IsEqual = .false.

    IF ( (GridA%NX .ne. GridB%NX) .or. (GridA%NY .ne. GridB%NY) .or. (GridA%NZ .ne. GridB%NZ) ) THEN
        RETURN ! Different dimensions!
    ENDIF

    IF ( (GridA%Handler .ne. GridB%Handler ) ) THEN
        ! This is dangerous and there will not be a possible regrid path.
        ! I will assume an error will be reported higher upstream.
        RETURN
    ENDIF

    ! ... other comparison code to be done here

    ! Compare in the vertical?
    CompareVert_ = .false.
    IF ( PRESENT(CompareVert) ) THEN
        CompareVert_ = CompareVert
    ENDIF

    IF ( CompareVert_ ) THEN
        ! TODO: Compare in the vertical
    ENDIF

    ! Safe to assume equal now
    IsEqual = .true.
  END FUNCTION Compare_Grid
END MODULE HCO_Grid_Mod
