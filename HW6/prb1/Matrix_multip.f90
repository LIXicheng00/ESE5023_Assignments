subroutine  Matrix_multip(a,b,c)
implicit none

integer                                          :: k, i, j, ra, cb, ca
real(4), dimension(:,:),allocatable, intent(in)  :: a, b
real(4), dimension(:,:),allocatable, intent(out) :: c



k=size(a,dim=1)
j=size(b,dim=2)
i=size(a,dim=2)


do ra=1,k
  do cb=1,j
    do ca=1,i
     c(ra,cb)=c(ra,cb)+a(ra,ca)*b(ca,cb)
    end do
  end do
end do




end subroutine Matrix_multip
