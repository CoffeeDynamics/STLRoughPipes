! ------------------------------------------------------------------------------------- !
!   This program bends an (ASCII) STL surface around the X-axis to produce a cylinder   !
!   and writes the result in (ASCII and binary) STL format                              !
! ------------------------------------------------------------------------------------- !
!                                                                                       !
! Usage:                                                                                !
!    ./STL_bend.exe surface.stl mergeTolFactor                                          !
! Produces:                                                                             !
!    surface_new_ASCII.stl and surface_new_binary.stl                                   !
!                                                                                       !
! Authors:                                                                              !
! Guillaume Sahut, Ph.D. and Himani Garg, Ph.D.                                         !
! Lund University, Department of Energy Sciences                                        !
! Sweden                                                                                !
!                                                                                       !
! Contact:                                                                              !
! guillaume.sahut@energy.lth.se                                                         !
! ------------------------------------------------------------------------------------- !

!
! Hereafter, 'Standard' refers to the STL file format available at
! https://en.wikipedia.org/wiki/STL_(file_format)
!

program STL_bend

   ! -------------------
   use number_precision_m, only: WP,I32
   use globals_m,          only: LEN_MAX,EPS,PI
   use topology_m,         only: vertex_t,vector3_t,facet_t,facetList_t,solid_t,solidList_t
   use STL_read_m,         only: read_file_ASCII
   use STL_write_m,        only: write_file_ASCII,write_file_binary
   ! -------------------

   implicit none

   !-----------------------
   character(len=LEN_MAX) :: fname,fname_wext,fname_ext,sbuf
   integer :: ntotalnode
   type(solid_t), pointer :: first_solid,current_solid
   type(facet_t), pointer :: current_facet
   type(vertex_t), pointer :: v1,v2,v3
   real(kind=WP) :: rbuf,rbuf2,R,r1,r2,r3,theta1,theta2,theta3
   real(kind=WP) :: min_x,max_x,min_y,max_y,min_z,max_z,LX,LY,LZ
   real(kind=WP) :: mean_x,mean_y,mean_z,amd_z,std_z,skew_z,kurt_z
   real(kind=WP) :: h1,h2,h3,x1,x2,x3,y1,y2,y3,z1,z2,z3,tol
   type(vector3_t), pointer :: normal
   integer :: ios,cmd_narg
   integer(kind=I32) :: ntotalfacet         ! should be of type uint32 according to Standard
   !-----------------------

   ! command-line arguments
   cmd_narg = iargc()
   if (cmd_narg==2) then

      ! 1: initial STL file
      call getarg( 1, sbuf)
      read(sbuf,'(A)',iostat=ios) fname
      if (ios.ne.0) then
         write(*,'(A)') 'Error: could not read STL file name'
         stop
      end if
      fname_wext = fname(1:len_trim(fname)-4)               ! removes '.ext'
      fname_ext = fname(len_trim(fname)-3:len_trim(fname))  ! captures '.ext'
      if (trim(fname_ext)/='.stl') then
         write(*,'(A)') "Error: file name extension must be '.stl'"
         stop
      end if

      ! 2: merge tolerance factor
      call getarg( 2, sbuf)
      tol = 0.0_WP
      read(sbuf,*,iostat=ios) tol
      if (ios.ne.0) then
         write(*,'(A)') 'Error: could not read merge tolerance factor'
         stop
      end if

   else
      write(*,'(A)') 'Error: please give initial STL file name and merge tolerance factor'
      stop
   end if

   ! write arguments
   write(*,'(A)') 'STL file:                            '//trim(fname) 
   write(*,'(A,X,ES22.15)') 'Merge tolerance factor:            ',tol
   
   nullify(first_solid)
   nullify(current_solid)
   nullify(current_facet)

   ! -------------------
   ! read ASCII STL file
   ! -------------------

   call read_file_ASCII(fname,first_solid,min_x,max_x,min_y,max_y,min_z,max_z, &
      mean_x,mean_y,mean_z,amd_z,std_z,skew_z,kurt_z,ntotalnode,ntotalfacet)

   ! -------------------
   ! modify geometry
   ! -------------------
   
   ! bounding box
   LX = max_x - min_x
   LY = max_y - min_y
   LZ = max_z - min_z
   
   ! cylinder radius
   R = LY/(2.0_WP*PI)
   
   ! tolerance to close the cylinder
   rbuf2 = tol*LY

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

         ! === modify Y and Z coordinates of vertex 1 ===
         
         r1 = y1-min_y

         ! try to properly close the cylinder
         if (abs(r1-LY)<rbuf2) then
            r1 = LY
         else if (abs(r1)<rbuf2) then
            r1 = 0.0_WP
         else if (r1<0.0_WP) then
            r1 = 0.0_WP
         else if (r1>LY) then
            r1 = LY
         end if

         h1 = z1-mean_z
         theta1 = 2.0_WP*PI*r1/LY - PI/2.0_WP
         y1 = (R-h1)*cos(theta1)
         z1 = (R-h1)*sin(theta1)+R+max_z
         
         ! === modify Y and Z coordinates of vertex 2 ===
         
         r2 = y2-min_y

         ! try to properly close the cylinder
         if (abs(r2-LY)<rbuf2) then
            r2 = LY
         else if (abs(r2)<rbuf2) then
            r2 = 0.0_WP
         else if (r2<0.0_WP) then
            r2 = 0.0_WP
         else if (r2>LY) then
            r2 = LY
         end if

         h2 = z2-mean_z
         theta2 = 2.0_WP*PI*r2/LY - PI/2.0_WP
         y2 = (R-h2)*cos(theta2)
         z2 = (R-h2)*sin(theta2)+R+max_z
         
         ! === modify Y and Z coordinates of vertex 3 ===
         
         r3 = y3-min_y

         ! try to properly close the cylinder
         if (abs(r3-LY)<rbuf2) then
            r3 = LY
         else if (abs(r3)<rbuf2) then
            r3 = 0.0_WP
         else if (r3<0.0_WP) then
            r3 = 0.0_WP
         else if (r3>LY) then
            r3 = LY
         end if

         h3 = z3-mean_z
         theta3 = 2.0_WP*PI*r3/LY - PI/2.0_WP
         y3 = (R-h3)*cos(theta3)
         z3 = (R-h3)*sin(theta3)+R+max_z

         ! === replace values ===

         normal%x = (y2-y1)*(z3-z1) - (z2-z1)*(y3-y1)
         normal%y = (z2-z1)*(x3-x1) - (x2-x1)*(z3-z1)
         normal%z = (x2-x1)*(y3-y1) - (y2-y1)*(x3-x1)
         rbuf = sqrt(normal%x**2.0_WP+normal%y**2.0_WP+normal%z**2.0_WP)
         if (rbuf<EPS) then
            write(*,'(A,ES22.15)') 'Error: norm of facet normal vector = ',rbuf
            write(*,'(3ES22.15)') x1,y1,z1
            write(*,'(3ES22.15)') x2,y2,z2
            write(*,'(3ES22.15)') x3,y3,z3
            stop
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

   ! --------------------
   ! write ASCII STL file
   ! --------------------
   call write_file_ASCII(fname_wext,fname_ext,first_solid)

   ! ---------------------
   ! write binary STL file
   ! ---------------------
   call write_file_binary(fname_wext,fname_ext,first_solid,ntotalfacet)

   ! ----------------------------------
   ! write info about surface roughness
   ! ----------------------------------
   write(*,'(A,X,ES22.15)') 'Min roughness (R_min):             ',min_z
   write(*,'(A,X,ES22.15)') 'Max roughness (R_max):             ',max_z
   write(*,'(A,X,ES22.15)') 'Mean roughness (R_mean):           ',mean_z
   write(*,'(A,X,ES22.15)') 'Arithmetic mean deviation (R_a):   ',amd_z
   write(*,'(A,X,ES22.15)') 'Standard deviation (R_q):          ',std_z
   write(*,'(A,X,ES22.15)') 'Max roughness amplitude (R_z):     ',max_z-min_z
   write(*,'(A,X,ES22.15)') 'Max - Mean (R_p):                  ',max_z-mean_z
   write(*,'(A,X,ES22.15)') 'Mean - Min (R_v):                  ',mean_z-min_z
   write(*,'(A,X,ES22.15)') 'Skewness (s_k):                    ',skew_z
   write(*,'(A,X,ES22.15)') 'Kurtosis (k_u):                    ',kurt_z

end program STL_bend
