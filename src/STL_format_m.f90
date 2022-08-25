! --------------------
! MODULE: STL_format_m
! --------------------

module STL_format_m

   ! -------------------
   use globals_m,            only: LEN_MAX
   ! -------------------

   implicit none

   private

   character(len=LEN_MAX), parameter, public :: keyword(7) = (/ &
      'solid       ', &
      'endsolid    ', &
      'facet normal', &
      'endfacet    ', &
      'outer loop  ', &
      'endloop     ', &
      'vertex      '  &
   /)

end module STL_format_m
