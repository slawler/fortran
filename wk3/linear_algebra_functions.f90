module linear_algebra_functions

private

public	::matrix_sum, matrix_mult, mixed_ops
public  ::vector_sum, vector_dot,vector_cross, vector_length, vector_norm

contains

!-----------------------------------------------------------------------
!subroutine: sum of matrices
!-----------------------------------------------------------------------

subroutine matrix_sum(n,a,b,c)
      implicit none
      integer			                      ::n,i,j
      double precision,dimension(n,n)         ::a,b,c
   
     print*,""
     print*,""
		
		do i=1,n
			 do j=1,n
				c(i,j) = a(i,j) + b(i,j)
			 end do
		end do
	  print*, "Sum of matrices:",c
	  print*, "-----------------------------------------------"
end subroutine

!-----------------------------------------------------------------------
!subroutine: matrix multiplication
!-----------------------------------------------------------------------

  subroutine matrix_mult(n,a,b,c)
      integer                       			    ::n,i,j,k
      double precision,dimension(n,n),intent(in)    ::a,b
      double precision,dimension(n,n),intent(out)   ::c
      	
      print*,""
          	
         do i=1,n
			do j=1,n
				do k=1,n
				 c(i,j) = c(i,j) + a(i,k)*b(k,j)
				end do
			end do
		end do 
       
       print*,"Product of matrices" ,c
       print*, "-----------------------------------------------"
   end subroutine  
   
!-----------------------------------------------------------------------
!subroutine: vector sum
!-----------------------------------------------------------------------
 subroutine vector_sum(n,v1,v2,v3)
      integer,intent(in)                          ::n
      integer									  ::i 	
      double precision,dimension(n),intent(in)    ::v1,v2
      double precision,dimension(n),intent(out)   ::v3
            
      v3=0.0d00
      
      do i=1,n
		v3(i) = v1(i)+v2(i)
      end do
      print*, "Sum of vectors:", v3
      print*, "-----------------------------------------------"  
  end subroutine 
  
!-----------------------------------------------------------------------
!subroutine: vector dot
!-----------------------------------------------------------------------
 subroutine vector_dot(n,v1,v2,s)
      integer,intent(in)                          ::n
      double precision,dimension(n),intent(in)    ::v1,v2
      double precision,intent(out)				  ::s
      
      s=0.0d00
        
      do i=1,n
		s = s + v1(i)*v2(i)
      end do
      print*, "Dot product of vectors:",s
      print*, "-----------------------------------------------"
  end subroutine 
  
!-----------------------------------------------------------------------
!subroutine: vector cross
!-----------------------------------------------------------------------
 subroutine vector_cross(n,v1,v2,v3)
      integer,intent(in)                          ::n
      double precision,dimension(n),intent(in)    ::v1,v2
      double precision,dimension(n),intent(out)   ::v3
             
	 if (n /=3) then 
	 print*,"This Cross Product function only valid for 3x3 matrices"
	 else
	  v3= 0.0d00 !initialize solution vector    
      
      v3(1)= v1(2)*v2(3)-v1(3)*v2(2)
      v3(2)= v1(3)*v2(1)-v1(1)*v2(3) 
      v3(3)= v1(1)*v2(2)-v1(2)*v2(1)
      
      print*, "Cross Product of vectors:", v3
      print*, "-----------------------------------------------"
    end if  
  end subroutine 
  
!-----------------------------------------------------------------------
!subroutine: vector length
!-----------------------------------------------------------------------
 subroutine vector_length(n,v1,v2,s1,s2)
      integer,intent(in)                          ::n
      integer									  ::i	
      double precision,dimension(n),intent(in)    ::v1,v2
      double precision,intent(out)			      ::s1,s2
      double precision							  ::x_dot_x
      
      x_dot_x = 0.0d00
      
      do i=1,n
        x_dot_x =x_dot_x +v1(i)*v1(i)
      end do
      
      s1 = sqrt(x_dot_x)
      print*,"Length of Vector 1=",s1
       
      x_dot_x = 0.0d00
      
      do i=1,n
        x_dot_x =x_dot_x +v2(i)*v2(i)
      end do
      
      s2 = sqrt(x_dot_x)
      
      print*, ""
      print*,"Length of Vector 2=",s2
      print*, "-----------------------------------------------"
          
  end subroutine 
  
!-----------------------------------------------------------------------
!subroutine: vector normal
!-----------------------------------------------------------------------
 subroutine vector_norm(n,v1,v2,v3,v4)
	  integer,intent(in)                          ::n
      integer									  ::i	
      double precision,dimension(n),intent(in)    ::v1,v2
      double precision,dimension(n),intent(out)   ::v3,v4
      double precision							  ::x_dot_x,mod_x
      
      x_dot_x = 0.0d00 !initialize vector solution
        
      do i=1,n
        x_dot_x =x_dot_x + v1(i)*v1(i)
      end do
      
      mod_x=sqrt(x_dot_x)
      
      do i=1,n
		v3(i)=v1(i)/mod_x
      end do
      
      print*,""
      print*,"The Normal Vector for vector 1:",v3
      print*, "-----------------------------------------------"
      
            x_dot_x = 0.0d00
      
      !Check the normal vector was computed correctly
        
      do i=1,n
        x_dot_x =x_dot_x + v2(i)*v2(i)
      end do
      
      mod_x=sqrt(x_dot_x)
      
      do i=1,n
		v4(i)=v2(i)/mod_x
      end do
      
      print*,""
      print*,"The Normal Vector for vector 2:",v4
      print*, "-----------------------------------------------"
       
  end subroutine 
  
!-----------------------------------------------------------------------
!subroutine: mixed_ops
!-----------------------------------------------------------------------
subroutine mixed_ops(n,m,a,b,x)  !Multiplies a vector and a matrix

      integer										::i,j,k
      integer, intent(in)							::n,m
      double precision, dimension(n,n),intent(in)	::a  
      double precision, dimension(n)				::b
      double precision, dimension(n), intent(out)	::x	
  	 
	if (n /= m) then
		print*, "Error: Invalid Entry, matrix must be (n x m), vector must be (m x 1)"
		else
		
		x(m) = 0.0d+00 !initialize solution vector
		k=m	

			do i=1,k
				do j=1,k
				x(i) = x(i) + a(i,j)*b(i)
				end do	
			end do
		
		print*, "Solution vector=", x
     end if
    
end subroutine 


end module  
  
