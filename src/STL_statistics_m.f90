! ------------------------
! MODULE: STL_statistics_m
! ------------------------

module STL_statistics_m

   ! --------------------
   use number_precision_m,   only: WP,I8,I16,I32,R32
   use globals_m,            only: LEN_MAX,PI,EPS,INF
   use topology_m,           only: vertex_t,facet_t,solid_t
   use STL_format_m,         only: keyword
   ! --------------------

   implicit none

   private

   public :: compute_statistics

   interface compute_statistics
      module procedure :: compute_statistics
   end interface compute_statistics

   contains

   !
   ! The names Kadivar et al. refer to the paper whose DOI is 10.1016/j.ijft.2021.100077
   !
   subroutine compute_statistics(first_solid,ntotalnode,min_x,max_x,min_y,max_y,min_z,max_z, &
      mean_x,mean_y,mean_z,amd_z,std_z,skew_z,kurt_z)

      implicit none

      ! -------------------
      type(solid_t), pointer, intent(in) :: first_solid
      integer, intent(in) :: ntotalnode
      real(kind=WP), intent(inout) :: min_x,max_x,min_y,max_y,min_z,max_z,mean_x,mean_y,mean_z
      real(kind=WP), intent(inout) :: amd_z  ! arithmetic mean deviation, R_a, Kadivar et al.
      real(kind=WP), intent(inout) :: std_z  ! standard deviation, R_q, Kadivar et al.
      real(kind=WP), intent(inout) :: skew_z ! skewness, s_k, Kadivar et al.
      real(kind=WP), intent(inout) :: kurt_z ! kurtosis, k_u, Kadivar et al.
      ! -------------------
      type(solid_t), pointer :: current_solid
      type(facet_t), pointer :: current_facet
      type(vertex_t), pointer :: v1,v2,v3
      real(kind=WP) :: x1,x2,x3,y1,y2,y3,z1,z2,z3
      ! -------------------

      write(*,'(A)',advance='no') 'Computing statistics... '

      min_x = INF
      max_x = -INF
      min_y = INF
      max_y = -INF
      min_z = INF
      max_z = -INF
      mean_x = 0.0_WP
      mean_y = 0.0_WP
      mean_z = 0.0_WP
      amd_z = 0.0_WP
      std_z = 0.0_WP
      skew_z = 0.0_WP
      kurt_z = 0.0_WP

      ! first loop over solids to compute statistics
      nullify(current_solid)
      current_solid => first_solid
      do while (associated(current_solid))

         ! loop over solid facets
         nullify(current_facet)
         current_facet => current_solid%first_facet
         do while (associated(current_facet))

            ! === get current facet properties ===

            ! get the 3 vertices of the current facet and their coordinates
            v1 => current_facet%v1
            v2 => current_facet%v2
            v3 => current_facet%v3
            x1 = v1%x
            y1 = v1%y
            z1 = v1%z
            x2 = v2%x
            y2 = v2%y
            z2 = v2%z
            x3 = v3%x
            y3 = v3%y
            z3 = v3%z
           
            ! update global properties
            min_x = min(min_x,min(x1,min(x2,x3)))
            max_x = max(max_x,max(x1,max(x2,x3)))
            min_y = min(min_y,min(y1,min(y2,y3)))
            max_y = max(max_y,max(y1,max(y2,y3)))
            min_z = min(min_z,min(z1,min(z2,z3)))
            max_z = max(max_z,max(z1,max(z2,z3)))
            mean_x = mean_x + x1 + x2 + x3
            mean_y = mean_y + y1 + y2 + y3
            mean_z = mean_z + z1 + z2 + z3
            
            current_facet => current_facet%next
         end do

         current_solid => current_solid%next
      end do
      mean_x = mean_x/real(ntotalnode,WP)
      mean_y = mean_y/real(ntotalnode,WP)
      mean_z = mean_z/real(ntotalnode,WP)
      ! end of first loop

      ! second loop over solids to compute more statistics
      nullify(current_solid)
      current_solid => first_solid
      do while (associated(current_solid))

         ! loop over solid facets
         nullify(current_facet)
         current_facet => current_solid%first_facet
         do while (associated(current_facet))

            ! === get current facet properties ===

            ! get the 3 vertices of the current facet and their coordinates
            v1 => current_facet%v1
            v2 => current_facet%v2
            v3 => current_facet%v3
            x1 = v1%x
            y1 = v1%y
            z1 = v1%z
            x2 = v2%x
            y2 = v2%y
            z2 = v2%z
            x3 = v3%x
            y3 = v3%y
            z3 = v3%z

            ! update global properties
            amd_z  = amd_z  + abs(z1-mean_z)      + abs(z2-mean_z)      + abs(z3-mean_z)
            std_z  = std_z  + (z1-mean_z)**2.0_WP + (z2-mean_z)**2.0_WP + (z3-mean_z)**2.0_WP
            skew_z = skew_z + (z1-mean_z)**3.0_WP + (z2-mean_z)**3.0_WP + (z3-mean_z)**3.0_WP
            kurt_z = kurt_z + (z1-mean_z)**4.0_WP + (z2-mean_z)**4.0_WP + (z3-mean_z)**4.0_WP
            
            current_facet => current_facet%next
         end do

         current_solid => current_solid%next
      end do
      amd_z  = amd_z/real(ntotalnode,WP)
      std_z  = sqrt(std_z/real(ntotalnode,WP))
      skew_z = skew_z/(real(ntotalnode,WP)*std_z**3.0_WP)
      kurt_z = kurt_z/(real(ntotalnode,WP)*std_z**4.0_WP)
      ! end of second loop

      write(*,'(A)') 'Done.'
   
   end subroutine compute_statistics

end module STL_statistics_m
