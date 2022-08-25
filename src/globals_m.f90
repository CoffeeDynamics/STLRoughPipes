! -----------------
! MODULE: globals_m
! -----------------

module globals_m

   ! --------------------
   use number_precision_m,   only: WP
   ! --------------------

   implicit none

   private

   integer, parameter, public :: LEN_MAX = 128
   real(kind=WP), parameter, public :: PI = 3.141592653589793_WP
   real(kind=WP), parameter,public :: EPS = 1e-16_WP
   real(kind=WP), parameter, public :: INF = huge(INF)

end module globals_m
