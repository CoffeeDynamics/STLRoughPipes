! ---------------------
! MODULE: STL_rescale_m
! ---------------------

module STL_rescale_m

   ! --------------------
   use number_precision_m,   only: WP,I8,I16,I32,R32
   use globals_m,            only: LEN_MAX,PI,EPS,INF
   use topology_m,           only: vertex_t,vector3_t,facet_t,solid_t
   use STL_format_m,         only: keyword
   ! --------------------

   implicit none

   private

   public :: rescale_roughness_height

   interface rescale_roughness_height
      module procedure :: rescale_z
   end interface rescale_roughness_height

   contains

   subroutine rescale_z(first_solid,mean_z,roughness_factor)

      implicit none

      ! -------------------
      type(solid_t), pointer, intent(out) :: first_solid
      real(kind=WP), intent(in) :: mean_z,roughness_factor
      ! -------------------
      type(solid_t), pointer :: current_solid
      type(facet_t), pointer :: current_facet
      type(vertex_t), pointer :: v1,v2,v3
      real(kind=WP) :: rbuf,x1,x2,x3,y1,y2,y3,z1,z2,z3
      type(vector3_t), pointer :: normal
      ! -------------------

      write(*,'(A,ES22.15,A)',advance='no') 'Rescaling roughness height with roughness factor',roughness_factor,'... '

      ! loop over solids
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
           
            ! get the normal vector to the current facet
            normal => current_facet%normal

            ! === modify Z coordinate of vertex 1 ===

            z1 = z1 + (z1-mean_z)*(roughness_factor-1.0_WP)
            
            ! === modify Z coordinate of vertex 2 ===
            
            z2 = z2 + (z2-mean_z)*(roughness_factor-1.0_WP)
            
            ! === modify Z coordinate of vertex 3 ===

            z3 = z3 + (z3-mean_z)*(roughness_factor-1.0_WP)
             
            ! === replace values ===

            normal%x = (y2-y1)*(z3-z1) - (z2-z1)*(y3-y1)
            normal%y = (z2-z1)*(x3-x1) - (x2-x1)*(z3-z1)
            normal%z = (x2-x1)*(y3-y1) - (y2-y1)*(x3-x1)
            rbuf = sqrt(normal%x**2.0_WP+normal%y**2.0_WP+normal%z**2.0_WP)
            if (rbuf<EPS) then
               rbuf = EPS
               write(*,'(A,ES22.15)') 'WARNING: Magnitude of triangle normal vector has been set to ',rbuf
               write(*,'(A)')         '         Triangle vertex coordinates are:'
               write(*,'(3ES22.15)') x1,y1,z1
               write(*,'(3ES22.15)') x2,y2,z2
               write(*,'(3ES22.15)') x3,y3,z3
            end if
            normal%x = normal%x/rbuf
            normal%y = normal%y/rbuf
            normal%z = normal%z/rbuf

            v1%x = x1
            v1%y = y1
            v1%z = z1
            v2%x = x2
            v2%y = y2
            v2%z = z2
            v3%x = x3
            v3%y = y3
            v3%z = z3

            current_facet => current_facet%next
         end do

         current_solid => current_solid%next
      end do

      write(*,'(A)') 'Done.'

   end subroutine rescale_z

end module STL_rescale_m
