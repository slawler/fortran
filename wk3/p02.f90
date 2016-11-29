
program gauss_naive
   use mod_my_matrix_io

   implicit none

   integer                                    ::nm,nv
   double precision,allocatable,dimension(:,:)::amat
   double precision,allocatable,dimension(:)  ::vvect, x
   character(len=80)                          ::fnmatrix,fnvector
   
       
   interface
      subroutine forward(nm,nv,amat,vvect,x)
      integer, intent(in)							::nm,nv
      double precision, dimension(nm,nm)			::amat
      double precision, dimension(nv), intent(in)	::vvect
      double precision, dimension(nv), intent(out)	::x
      end subroutine forward
	end interface

   write(6,100)
   read *,fnmatrix

   ! read matrix, place it into dense matrix format
   call io_get_matrix(fnmatrix,nm,amat)
   ! print matrix in readable format
   call io_print_matrix(nm,amat)
      

	   write(6,110)
   read *,fnvector
   ! read vector
   call io_get_vector(fnvector,nv,vvect)
   ! print vector in readable format
   call io_print_vector(nv,vvect)
 
 
   allocate (x(nv)) 

   print*,"-------"
   call forward(nm,nv,amat,vvect,x)
   print*,x
   
   
100 format('[INFO]: Please enter name of matrix')
110 format('[INFO]: Please enter name of vector')

end program

subroutine forward(n,n2,a,b,x)

	implicit none
	integer,intent(in)				::n,n2
	integer			 				::i,j,k
	double precision,dimension(n,n) ::a
	double precision,dimension(n2)  ::b
	double precision,dimension(n)	::x,s
	double precision 				::xmult
   
!---------------------------FORWARD_ELIMINATION
		do k=1,n-1
			 do i=k+1,n
				xmult=a(i,k)/a(k,k)
				a(i,k) =xmult
					do j=k+1,n
						a(i,j)=a(i,j)-xmult*a(k,j)
					end do
				b(i)=b(i)-xmult*b(k)
			end do
		end do

!--------------------------------BACK_SUBSTITUTION
      x(n) = 0.0d00     
      x(n)=b(n)/a(n,n)
     
      do i=n-1,1,-1
         s(i)=b(i)
          do j=i+1, n
           s(i)=s(i)-a(i,j)*x(j)
         end do
        x(i)=s(i)/a(i,i)
      end do

end subroutine
