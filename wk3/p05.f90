
program conjugate_gradient

   use mod_my_matrix_io

   implicit none

   integer                                    ::nm,nv
   double precision,allocatable,dimension(:,:)::amat
   double precision,allocatable,dimension(:)  ::vvect, x
   character(len=80)                          ::fnmatrix,fnvector
          
   interface
      subroutine cg(nm,amat,vvect,x)
      integer, intent(in)							::nm
      double precision, dimension(nm,nm)			::amat
      double precision, dimension(nm), intent(in)	::vvect
      double precision, dimension(nm), intent(out)	::x
      end subroutine cg
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
   call cg(nm,amat,vvect,x)
   deallocate(x)
      
100 format('[INFO]: Please enter name of matrix')
110 format('[INFO]: Please enter name of vector')

end program

!-----------------------------------------
!subroutine: Conjugate Gradient
!-----------------------------------------

subroutine cg(n,a,b,x)

      integer										::k
      integer, intent(in)							::n
      double precision, dimension(n,n),intent (in)	::a
      double precision, dimension(n),intent (in)	::b
      double precision, dimension(n), intent(out)	::x
      double precision,dimension(n)					::xkm1,rkm1,dk, dkm1,qk,xk,rk
      double precision								::rho_km1,rho_km2,beta_km1, alpha_k,limit
   	
	xkm1(1:n) =  0.01d+00
	rkm1(1:n) = b(1:n) - matmul(a(1:n,1:n), xkm1(1:n))
	k =1 
	 
	do 
		 rho_km1 = dot_product(rkm1(1:n),rkm1(1:n)) 
			
			 if (k==1) then 		 
				 dk(1:n) = rkm1(1:n) 
			 
				 else 
				 beta_km1 = rho_km1 / rho_km2 
				 
				 dk(1:n) = rkm1(1:n) + beta_km1*dkm1(1:n) 
			end if 
			 
			 qk(1:n) = matmul(a(1:n,1:n), dk(1:n)) 
			 alpha_k = rho_km1 /dot_product(dk(1:n),qk(1:n)) 
			 xk(1:n) = xkm1(1:n) + alpha_k* dk(1:n) 
			 rk(1:n) = rkm1(1:n) - alpha_k*qk(1:n) 
			 		
			if (k==100 .or. rho_km1 <= (epsilon(1.0)**2)) then 
			  exit
			end if
		 		 
		 !update vectors  
		 xkm1(1:n) = xk(1:n)
		 dkm1(1:n) = dk(1:n)
		 rkm1(1:n) = rk(1:n)
		 !update scalar
		 rho_km2 = rho_km1
		 
		 x(1:n) = xk(1:n)
     end do

print*, x(1:n)
	 

end subroutine 
