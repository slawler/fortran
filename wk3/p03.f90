
program gauss_w_pivot

   use mod_my_matrix_io

   implicit none

   integer                                    ::nm,nv
   double precision,allocatable,dimension(:,:)::amat
   double precision,allocatable,dimension(:)  ::vvect, x
   character(len=80)                          ::fnmatrix,fnvector
   
       
   interface
      subroutine gaus_piv(nm,amat,vvect,x)
      integer, intent(in)							::nm
      double precision, dimension(nm,nm)			::amat
      double precision, dimension(nm), intent(in)	::vvect
      double precision, dimension(nm), intent(out)	::x
      end subroutine gaus_piv
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
   call gaus_piv(nm,amat,vvect,x)
   print*,x
   
   deallocate(x)
   
100 format('[INFO]: Please enter name of matrix')
110 format('[INFO]: Please enter name of vector')

end program

!-----------------------------------------
!subroutine: Fancy Gauss
!-----------------------------------------

subroutine gaus_piv(n,a,b,x)

      integer										::i,j,k,p
      integer, intent(in)							::n
      double precision, dimension(n,n)				::a
      double precision, dimension(n)				::b
      double precision, dimension(n), intent(out)	::x
      double precision,dimension(n)					::s
      double precision								::xmult,pivot,interchange  	
    
	 do k=1,n-1   
		do i=k,n                      
		s(i) = 0.0d+00
		   do j=k,n                 
		   s(i) = max(s(i),abs(a(i,j)))  !determine a 'scale' factor for each row
		end do
	 end do

	  pivot = abs(a(k,k)/s(k)) !criteria to identify pivot
	  p = k
	  
	  do j=k+1,n
		if(abs(a(j,k)/s(j)) > pivot) then
		  pivot = abs(a(j,k)/s(j))  !search for the pivot in columns
		  p = j
		end if
	  end do

	if (p /= k) then 
	  do j=k,n
		 interchange = a(k,j) !Interchange rows if necessary
		 a(k,j) = a(p,j)
		 a(p,j) = interchange
	  end do
	  interchange = b(k) 
	  b(k) = b(p)
	  b(p) = interchange 
	end if

	   do i=k+1,n
		  xmult=a(i,k)/a(k,k) !calculate pivot ratio
		  a(i,k) = 0.0
		  b(i)=b(i)- xmult*b(k)
		  do j=k+1,n
			 a(i,j) = a(i,j)-xmult*a(k,j) !multiply through each element with pivot ratio
		  end do
	    end do
	  end do

	x(n) = b(n)/a(n,n) 
	do i=n-1,1,-1  !go backward to find solution vector
	   xmult=0.0
	   do j=i+1,n
		 xmult= xmult + a(i,j)*x(j)
	   end do 
	   x(i) = (b(i)- xmult)/a(i,i)
	end do

end subroutine 

!Note: So this should work...but I really struggle to understand why. I found many different approaches online, using
!partial pivots. this code is principally derived using your pseudo code and some power point examples I found 
!online of how to search through the rows and columns to identify and swap the pivot. 
