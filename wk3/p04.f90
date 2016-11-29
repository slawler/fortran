
program jacobi

   use mod_my_matrix_io

   implicit none

   integer                                    ::nm,nv
   double precision,allocatable,dimension(:,:)::amat
   double precision,allocatable,dimension(:)  ::vvect, x
   character(len=80)                          ::fnmatrix,fnvector
          
   interface
      subroutine jacob_it(nm,amat,vvect,x)
      integer, intent(in)							::nm
      double precision, dimension(nm,nm),intent(in)	::amat
      double precision, dimension(nm), intent(in)	::vvect
      double precision, dimension(nm), intent(out)	::x
      end subroutine jacob_it
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
 
 
   allocate (x(nm)) 

   print*,"-------"
   call jacob_it(nm,amat,vvect,x)
 
   
   
100 format('[INFO]: Please enter name of matrix')
110 format('[INFO]: Please enter name of vector')

end program

!-----------------------------------------
!subroutine: Jacobi_iterator
!-----------------------------------------

subroutine jacob_it(n,a,b,x)

      integer										::i,j,k
      integer, intent(in)							::n
      double precision, dimension(n,n),intent(in)	::a  
      double precision, dimension(n)				::b,s
      double precision, dimension(n), intent(out)	::x	
  	 
	!initial guess
	x(n) = 1.0d+00
	k = 0
	
	do 
		do i=1,n
		s(i) = 0.0d+00
			do j=1,n
				if (j /= i) then
				s(i) = s(i) + a(i,j)*x(j)
				end if
			end do
			x(i) = (b(i)-s(i))/a(i,i)
		end do	
		k = k+1
			
		if (k>= 100) then
		exit
		end if
		
	end do
	
    print*, x
end subroutine 


!debug checklist

!1)compile with -g
!2)type gdb ./your_exe
!whatis (variable) 
!type p for print i.e.  p i
!at break in line (whatever) when the sub starts, type s and you can go through the sub line by line
!type break line(whatever)
!type n to goto next line (shows what the command is)
!type p and the variable (shows the output, i.e. NaN)
!go line by line from here


