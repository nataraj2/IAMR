
! ::: -----------------------------------------------------------
! ::: This routine will tag high error cells based on the state
! ::: 
! ::: INPUTS/OUTPUTS:
! ::: 
! ::: tag        <=  integer tag array
! ::: tag_lo,hi   => index extent of tag array
! ::: state       => state array
! ::: state_lo,hi => index extent of state array
! ::: set         => integer value to tag cell for refinement
! ::: clear       => integer value to untag cell
! ::: lo,hi       => work region we are allowed to change
! ::: dx          => cell size
! ::: problo      => phys loc of lower left corner of prob domain
! ::: time        => problem evolution time
! ::: level       => refinement level of this array
! ::: -----------------------------------------------------------

subroutine state_error(tag,tag_lo,tag_hi, &
		       set,clear,state,state_lo,state_hi, &
                       ncomp, lo,hi,&
                       dx,problo,time,level) bind(C, name="state_error")

  use amrex_fort_module, only : amrex_real
  implicit none
  
  integer          :: lo(3),hi(3),ncomp,level
  integer          :: state_lo(3),state_hi(3)
  integer          :: tag_lo(3),tag_hi(3)
  real(amrex_real) :: state(state_lo(1):state_hi(1), &
                            state_lo(2):state_hi(2), &
                            state_lo(3):state_hi(3))
  integer          :: tag(tag_lo(1):tag_hi(1),tag_lo(2):tag_hi(2),tag_lo(3):tag_hi(3))
  real(amrex_real) :: problo(3),dx(3),time,phierr
  integer          :: set,clear
  integer          :: i, j, k
  double precision :: y

   print*, "REACHING HERE.....................",lo(3),hi(3)
   print*, "REACHING HERE.....................",lo(2),hi(2)

  ! Tag on regions of high phi
  do       k = lo(3), hi(3)
     do    j = lo(2), hi(2)
	   y = problo(2) + (j+0.5d0)*dx(2)
	   print*, "y value is ", y
        do i = lo(1), hi(1)
           !if (state(i,j,k) .ge. phierr) then
	    if(y.ge.0.0d0.and.y.le.0.005d0)then
		print*, "i am inside here"
              tag(i,j,k) = set
           endif
        enddo
     enddo
  enddo

end subroutine state_error

