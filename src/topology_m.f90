! -------------------
! MODULE: topology_m
! -------------------

module topology_m

   ! -------------------
   use number_precision_m,   only: WP,I8,I16,I32,R32
   use globals_m,            only: LEN_MAX,PI,EPS,INF
   ! -------------------

   implicit none

   private

   type, public :: vertex_t
      real(kind=WP) :: x=0.0_WP,y=0.0_WP,z=0.0_WP
   end type vertex_t
   type, public :: vector3_t
      real(kind=WP) :: x=0.0_WP,y=0.0_WP,z=0.0_WP
   end type vector3_t
   type, public :: facet_t
      character(len=LEN_MAX) :: thename = ''
      integer :: nvertex = 3
      type(vertex_t) :: v1,v2,v3
      type(vertex_t), pointer :: v => null()
      type(vector3_t) :: normal
      type(facet_t), pointer :: next => null()
   end type facet_t
   type, public :: facetList_t
      type(facet_t), pointer :: ptr
   end type facetList_t
   type, public :: solid_t
      character(len=LEN_MAX) :: thename = ''
      integer :: nfacet = 0
      integer :: nvertex = 0
      type(facetList_t), pointer :: facetList(:) => null()
      type(facet_t), pointer :: first_facet => null()
      type(solid_t), pointer :: next => null()
   end type solid_t
   type, public :: solidList_t
      type(solid_t), pointer :: ptr
   end type solidList_t

end module topology_m
