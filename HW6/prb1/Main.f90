program Main
 
implicit none
real(4), dimension(:,:), allocatable :: a, b, c
integer                              :: u, n, i, v, m 

u = 50
v = 40
open(unit=u, file='/work/ese-lixc/fortran_2/M.dat', status='old')
open(unit=v, file='/work/ese-lixc/fortran_2/N.dat', status='old')


m=4
n=3

allocate( a(m,3), b(n,3),c(3,3)) 
! Read data line by line
do i = 1,m
  read(u, *) a(:,i)
enddo

do i = 1,n
  read(v, *) b(:,i)
enddo

close(u)
close(v)

call Matrix_multip(a,b,c)
write(*,*) 'c is: ', c


!write to a file
open(unit=u, file='MN.dat', status='replace')

do i = 1,n
  write(u,'(f8.1,f8.1,f8.1)') c(:,i)
enddo
close(u)


deallocate( a, b, c )

end program Main
