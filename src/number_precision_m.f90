! --------------------------
! MODULE: number_precision_m
! --------------------------

module number_precision_m

   ! -------------------
   use, intrinsic :: iso_fortran_env, only: INTEGER_KINDS,   &  ! supported kind parameters of integer type
                                            I8   => INT8,    &  !         integer number stored on   8 bits
                                            I16  => INT16,   &  !                                   16 bits
                                            I32  => INT32,   &  !                                   32 bits
                                            I64  => INT64,   &  !                                   64 bits
                                            REAL_KINDS,      &  !    supported kind parameters of real type
                                            R32  => REAL32,  &  !            real number stored on  32 bits
                                            R64  => REAL64,  &  !                                   64 bits
                                            R128 => REAL128     !                                  128 bits
   ! -------------------

   implicit none

   private

   public :: INTEGER_KINDS,I8,I16,I32,I64,REAL_KINDS,R32,R64,R128
   
   ! The following declarations ensure:
   !    - at least *_sigd significant digits and
   !    - an exponent range of at least *_expr
   !
   ! single precision
   integer, parameter :: SP_sigd = 6, SP_expr = 37
   integer, parameter, public :: SP = selected_real_kind(p=SP_sigd,r=SP_expr,radix=2)
   ! double precision
   integer, parameter :: DP_sigd = 15, DP_expr = 307
   integer, parameter, public :: DP = selected_real_kind(p=DP_sigd,r=DP_expr,radix=2)
   ! quadruple precision
   integer, parameter :: QP_sigd = 33, QP_expr = 4931
   integer, parameter, public :: QP = selected_real_kind(p=QP_sigd,r=QP_expr,radix=2)
   !
   ! working precision
   integer, parameter, public :: WP = DP
   !-----------------------

end module number_precision_m
