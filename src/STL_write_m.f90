! -------------------
! MODULE: STL_write_m
! -------------------

module STL_write_m

   ! -------------------
   use number_precision_m,   only: WP,I8,I16,I32,R32
   use globals_m,            only: LEN_MAX,PI,EPS,INF
   use topology_m,           only: vertex_t,vector3_t,facet_t,facetList_t,solid_t,solidList_t
   use STL_format_m,         only: keyword
   ! -------------------

   implicit none

   private

   public :: write_file_ASCII,write_file_binary

   interface write_file_ASCII
      module procedure :: process_ASCII
   end interface write_file_ASCII

   interface write_file_binary
      module procedure :: process_binary
   end interface write_file_binary

   contains

   subroutine process_ASCII(fname_wext,fname_ext,first_solid)

      implicit none

      ! -------------------
      character(len=LEN_MAX), intent(in) :: fname_wext,fname_ext
      type(solid_t), pointer, intent(in) :: first_solid
      ! -------------------
      character(len=LEN_MAX) :: fname_new
      integer :: fid_new,ios
      logical :: fexists
      type(solid_t), pointer :: current_solid
      type(facet_t), pointer :: current_facet
      type(vertex_t), pointer :: v1,v2,v3
      type(vector3_t), pointer :: normal
      ! -------------------

      fid_new = 2021
      fname_new = trim(fname_wext)//'_new_ASCII'//trim(fname_ext)
      inquire(file=trim(fname_new),exist=fexists)
      if (fexists) then
         open(unit=fid_new,file=trim(fname_new),status='replace',action='write',iostat=ios)
      else
         open(unit=fid_new,file=trim(fname_new),status='new',action='write',iostat=ios)
      end if
      if (ios.ne.0) then
         write(*,'(A,I3)') 'Error: could not open file '//trim(fname_new)//': iostat = ',ios
         stop
      end if

      nullify(current_solid)
      current_solid => first_solid
      do while (associated(current_solid))

         write(fid_new,'(A,X,A)') trim(keyword(1)),trim(current_solid%thename)//'_NEW'

         nullify(current_facet)
         current_facet => current_solid%first_facet
         do while (associated(current_facet))

            v1 => current_facet%v1
            v2 => current_facet%v2
            v3 => current_facet%v3
            normal => current_facet%normal

            write(fid_new,'(A,X,2(F0.10,X),F0.10)') trim(keyword(3)),normal%x,normal%y,normal%z
            write(fid_new,'(2X,A)') trim(keyword(5))
            write(fid_new,'(4X,A,X,2(F0.10,X),F0.10)') trim(keyword(7)),v1%x,v1%y,v1%z
            write(fid_new,'(4X,A,X,2(F0.10,X),F0.10)') trim(keyword(7)),v2%x,v2%y,v2%z
            write(fid_new,'(4X,A,X,2(F0.10,X),F0.10)') trim(keyword(7)),v3%x,v3%y,v3%z
            write(fid_new,'(2X,A)') trim(keyword(6))
            write(fid_new,'(A)') trim(keyword(4))

            current_facet => current_facet%next
         end do

         write(fid_new,'(A,X,A)') trim(keyword(2)),trim(current_solid%thename)//'_NEW'

         current_solid => current_solid%next
      end do

      close(fid_new)

   end subroutine process_ASCII

   subroutine process_binary(fname_wext,fname_ext,first_solid,ntotalfacet)

      implicit none

      ! -------------------
      character(len=LEN_MAX), intent(in) :: fname_wext,fname_ext
      type(solid_t), pointer, intent(in) :: first_solid
      integer(kind=I32), intent(in) :: ntotalfacet
      ! -------------------
      character(len=LEN_MAX) :: fname_new
      integer :: fid_new,ios
      logical :: fexists
      type(solid_t), pointer :: current_solid
      type(facet_t), pointer :: current_facet
      type(vertex_t), pointer :: v1,v2,v3
      type(vector3_t), pointer :: normal
      integer(kind=I8) :: b_header(80)                    ! should be of type uint8 according to Standard
      real(kind=R32) :: b_n(3),b_v1(3),b_v2(3),b_v3(3)
      integer(kind=I16) :: b_attr                         ! should be of type uint16 according to Standard
      ! -------------------

      fid_new = 2022
      fname_new = trim(fname_wext)//'_new_binary'//trim(fname_ext)
      inquire(file=trim(fname_new),exist=fexists)
      if (fexists) then
         open(unit=fid_new,file=fname_new,access='stream',status='replace',action='write',iostat=ios)
      else
         open(unit=fid_new,file=fname_new,access='stream',status='new',action='write',iostat=ios)
      end if
      if (ios.ne.0) then
         write(*,'(A,I3)') 'Error: could not open file '//trim(fname_new)//': iostat = ',ios
         stop
      end if

      b_header(1:80) = 0_I8
      write(fid_new,iostat=ios) b_header(1:80),ntotalfacet

      nullify(current_solid)
      current_solid => first_solid
      do while (associated(current_solid))

         nullify(current_facet)
         current_facet => current_solid%first_facet
         do while (associated(current_facet))

            v1 => current_facet%v1
            v2 => current_facet%v2
            v3 => current_facet%v3
            normal => current_facet%normal

            b_n(1:3) = real((/normal%x,normal%y,normal%z/),R32)
            b_v1(1:3) = real((/v1%x,v1%y,v1%z/),R32)
            b_v2(1:3) = real((/v2%x,v2%y,v2%z/),R32)
            b_v3(1:3) = real((/v3%x,v3%y,v3%z/),R32)
            b_attr = 0_I16
            
            write(fid_new,iostat=ios) b_n(1:3),b_v1(1:3),b_v2(1:3),b_v3(1:3),b_attr

            current_facet => current_facet%next
         end do

         current_solid => current_solid%next
      end do

      close(fid_new)

   end subroutine process_binary

end module STL_write_m
