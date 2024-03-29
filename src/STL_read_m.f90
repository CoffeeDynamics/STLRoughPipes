! ------------------
! MODULE: STL_read_m
! ------------------

module STL_read_m

   ! --------------------
   use number_precision_m,   only: WP,I8,I16,I32,R32
   use globals_m,            only: LEN_MAX,PI,EPS,INF
   use topology_m,           only: facet_t,solid_t
   use STL_format_m,         only: keyword
   ! --------------------

   implicit none

   private

   public :: read_file_ASCII

   interface read_file_ASCII
      module procedure :: process_ASCII
   end interface read_file_ASCII

   contains

   subroutine process_ASCII(fname,first_solid,ntotalnode,ntotalfacet)

      implicit none

      ! -------------------
      character(len=LEN_MAX), intent(in) :: fname
      type(solid_t), pointer, intent(out) :: first_solid
      integer, intent(out) :: ntotalnode
      integer(kind=I32), intent(out) :: ntotalfacet
      ! -------------------
      character(len=LEN_MAX) :: word(5)
      character(len=LEN_MAX) :: sbuf,nline_s,solidName
      logical :: fexists,done,done2,facet_open,facet_loop_open
      integer :: fid,stat,nline,i,j,nspaces
      integer :: space(4)
      type(solid_t), pointer :: current_solid
      type(facet_t), pointer :: current_facet
      ! -------------------

      write(*,'(A)',advance='no') 'Reading STL file... '
      
      nullify(first_solid)
      nullify(current_solid)
      nullify(current_facet)

      fid = 2020
      inquire(file=trim(fname),exist=fexists)
      if (.not.fexists) then
         write(*,'(A)') 'Error: file '//trim(fname)//' does not exit'
         stop
      end if

      open(unit=fid,file=trim(fname),status="old",action="read")
      done = .false.
      nline = 0
      solidName = ''
      facet_open = .false.
      facet_loop_open = .false.
      ntotalnode = 0
      ntotalfacet = 0_I32

      do while (.not.done)
         read(unit=fid,fmt='(A)',iostat=stat) sbuf
         sbuf = adjustl(sbuf)
         if (is_iostat_end(stat)) then
            done = .true.
         else if (stat==0) then
            nline = nline + 1
            write(nline_s,'(I10)') nline
            word(1:5) = ''
            space(1:4) = 0
            done2 = .false.
            i = 0
            nspaces = 0
            do while (.not.done2)
               j = index(trim(sbuf(i+1:)),' ')
               if (j==0) then
                  done2 = .true.
               else
                  i = i + j
                  nspaces = nspaces + 1
                  if (nspaces.le.size(space)) then
                     space(nspaces) = i
                  end if
               end if
            end do

            if (space(1)==0) then
               word(1) = sbuf(1:)
            else
               word(1) = sbuf(1:space(1)-1)
               if (space(2)==0) then 
                  word(2) = sbuf(space(1)+1:)
               else
                  word(2) = sbuf(space(1)+1:space(2)-1)
                  if (space(3)==0) then
                     word(3) = sbuf(space(2)+1:)
                  else
                     word(3) = sbuf(space(2)+1:space(3)-1)
                     if (space(4)==0) then
                        word(4) = sbuf(space(3)+1:)
                     else
                        word(4) = sbuf(space(3)+1:space(4)-1)
                        word(5) = sbuf(space(4)+1:)
                     end if
                  end if
               end if
            end if

            ! 'solid SOLIDNAME'
            if (trim(word(1))==trim(keyword(1))) then
               if (len_trim(word(2))==0) then
                  write(*,'(A)') 'Error: empty solid name'
                  stop
               end if
               if (len_trim(solidName)>0) then
                  write(*,'(A)') 'Error: solid '//trim(solidName)//' still unclosed'
                  stop
               end if
               solidName = word(2)

               if (.not.associated(current_solid)) then
                  allocate(first_solid)
                  first_solid = solid_t()
                  current_solid => first_solid
               else
                  allocate(current_solid%next)
                  current_solid => current_solid%next
                  current_solid = solid_t()
               end if
               current_solid%thename = solidName
               current_solid%nfacet = 0
               nullify(current_facet)
            end if

            ! 'endsolid SOLIDNAME'
            if (trim(word(1))==trim(keyword(2))) then
               if (len_trim(solidName)==0) then
                  write(*,'(A)') 'Error: either solid '//trim(word(2))//' has already been closed or was never declared'
                  stop
               end if
               if (.not.associated(current_solid)) then
                  write(*,'(A)') 'Error: no solid to close'
                  stop
               end if
               solidName = ''
            end if

            ! 'facet normal X Y Z'
            if (trim(word(1))//" "//trim(word(2))==trim(keyword(3))) then
               if (.not.associated(current_solid)) then
                  write(*,'(A)') 'Error: no solid to attach facet'
                  stop
               end if
               if (facet_open) then
                  write(*,'(A)') 'Error: a facet is not closed'
                  stop
               end if
               if (facet_loop_open) then
                  write(*,'(A)') 'Error: a facet loop is not closed'
                  stop
               end if
               if (.not.associated(current_facet)) then
                  allocate(current_solid%first_facet)
                  current_solid%first_facet = facet_t()
                  current_facet => current_solid%first_facet
               else
                  allocate(current_facet%next)
                  current_facet => current_facet%next
                  current_facet = facet_t()
               end if
               current_solid%nfacet = current_solid%nfacet + 1
               ntotalfacet = ntotalfacet + 1_I32
               current_solid%nvertex = current_solid%nvertex + current_facet%nvertex
               current_facet%thename = current_solid%thename
               read(word(3),*) current_facet%normal%x
               read(word(4),*) current_facet%normal%y
               read(word(5),*) current_facet%normal%z
               current_facet%v => current_facet%v1
               facet_open = .true.
            end if

            ! 'endfacet'
            if (trim(word(1))==trim(keyword(4))) then
               if (.not.associated(current_solid)) then
                  write(*,'(A)') 'Error: no solid to close facet for'
                  stop
               end if
               if (.not.facet_open) then
                  write(*,'(A)') 'Error: no facet open'
                  stop
               end if
               if (facet_loop_open) then
                  write(*,'(A)') 'Error: a facet loop is still open'
               end if
               facet_open = .false.
            end if

            ! 'outer loop'
            if (trim(word(1))//" "//trim(word(2))==trim(keyword(5))) then
               if (.not.associated(current_solid)) then
                  write(*,'(A)') 'Error: no solid to attach facet loop'
                  stop
               end if
               if (.not.facet_open) then
                  write(*,'(A)') 'Error: no facet open'
                  stop
               end if
               if (facet_loop_open) then
                  write(*,'(A)') 'Error: a facet loop is not closed'
                  stop
               end if
               facet_loop_open = .true.
            end if

            ! 'endloop'
            if (trim(word(1))==trim(keyword(6))) then
               if (.not.associated(current_solid)) then
                  write(*,'(A)') 'Error: no solid to close facet loop for'
                  stop
               end if
               if (.not.facet_open) then
                  write(*,'(A)') 'Error: no facet open'
                  stop
               end if
               if (.not.facet_loop_open) then
                  write(*,'(A)') 'Error: no facet loop open'
                  stop
               end if
               facet_loop_open = .false.
            end if

            ! 'vertex X Y Z'
            if (trim(word(1))==trim(keyword(7))) then
               if (.not.associated(current_solid)) then
                  write(*,'(A)') 'Error: no solid to attach vertex'
                  stop
               end if
               if (.not.facet_open) then
                  write(*,'(A)') 'Error: no facet open'
                  stop
               end if
               if (.not.facet_loop_open) then
                  write(*,'(A)') 'Error: no facet loop open'
                  stop
               end if

               ntotalnode = ntotalnode + 1

               read(word(2),*) current_facet%v%x
               read(word(3),*) current_facet%v%y
               read(word(4),*) current_facet%v%z

               if (associated(current_facet%v,current_facet%v1)) then
                  current_facet%v => current_facet%v2 
               else if (associated(current_facet%v,current_facet%v2)) then
                  current_facet%v => current_facet%v3
               end if

            end if

         end if
      end do
      close(unit=fid)

      if (len_trim(solidName)>0) then
         write(*,'(A)') 'Error: EOF reached but solid '//trim(solidName)//' is unclosed'
         stop
      end if
      if (facet_open) then
         write(*,'(A)') 'Error: EOF reached but a facet is open'
         stop
      end if
      if (facet_loop_open) then
         write(*,'(A)') 'Error: EOF reached but a facet loop is open'
         stop
      end if

      write(*,'(A)') 'Done.'

   end subroutine process_ASCII

end module STL_read_m
