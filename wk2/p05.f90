
program	trapezoidal	

   implicit none
   integer 			 		   :: i,n
   double precision  		   :: a,b
   double precision  		   :: x,y,mysum
   double precision			   :: funct
       
  interface
      subroutine read_numbers(a,b,n)
         implicit none
         double precision, intent(inout) :: a,b
		 integer, intent (inout)         :: n
      end subroutine read_numbers
	 !------------------------------------------------------
	  subroutine algo(a,b,n,mysum)
		 implicit none
		 double precision, intent(in) :: a,b
		 integer, intent (in)         :: n
		 double precision,intent(out) :: mysum
	  end subroutine	
     !----------------------------------------------------- 
  end interface

	call read_numbers(a,b,n) 
    call algo(a,b,n,mysum)
   	
end program trapezoidal

!================================================
! SUBROUTINE #1
!================================================
subroutine read_numbers(a,b,n)
    implicit none
    double precision, intent(inout) :: a,b
    integer, intent (inout)         :: n
    character(len=80)   		    :: text="Enter lower bound 'a', &
				upper bound 'b' and desired number of intervals 'n '"
  
  
  
   10 continue
   print*, "Numerical Integration Function"
   print*, ""
   print*, "NOTE: If you wish to use values of 'pi' please enter the &
				numbers manually, i.e. 3.14...."
   print*, ""						
   print*, text
   read *,a,b,n
   
   if (a >= b .or. n<=0) then
	print*, "Invalid entry"
	print*, "--------------------------------------------------------"
	go to 10
   end if
	  
end subroutine read_numbers
!================================================
! SUBROUTINE #2
!================================================
subroutine algo(a,b,n,mysum)

		implicit none
		double precision, intent(in) :: a,b
		integer, intent (in)         :: n
		integer						 :: i	
		double precision,intent(out) :: mysum
		double precision			 :: dx,x,y, funct, diff 
		double precision 			 :: integral_1, integral_2, integral_3
		character (len = 20)		 :: text
							
	print*, "Please type Equation 'a', 'b', or 'c'"	
	print*, ""
    read*, text
		if (text == 'a')then
			goto 10
			else if (text =='b')then
			goto 20
			else if (text == 'c') then
		    goto 30
		    else 
		    goto 40
		end if
    10 continue	
      
	  dx = (b-a)/n !initialize values for function 
	  x = a
	  mysum = 0.0d+00 
		
	   do i=1,n-1  ! Calculate Function given a,b,n
		 x = x +dx
		 y = integral_1(x)
		 mysum = mysum + y
	   end do
	   
	   mysum = dx*(0.5d+00*(integral_1(a)+ integral_1(b)) + mysum)  
	   
	   print*, mysum
	   goto 40
	   
	20 continue
	  dx = (b-a)/n !initialize values for function 
	  x = a
	  mysum = 0.0d+00 
		
	   do i=1,n-1  ! Calculate Function given a,b,n
		 x = x +dx
		 y = integral_2(x)
		 mysum = mysum + y
	   end do
	   
	   mysum = dx*(0.5d+00*(integral_2(a)+ integral_2(b)) + mysum)  
	   
	   print*, mysum
	   goto 40
	   
	30 continue
	  dx = (b-a)/n !initialize values for function 
	  x = a
	  mysum = 0.0d+00 
		
	   do i=1,n-1  ! Calculate Function given a,b,n
		 x = x +dx
		 y = integral_3(x)
		 mysum = mysum + y
	   end do
	   
	   mysum = dx*(0.5d+00*(integral_3(a)+ integral_3(b)) + mysum)  
	   diff = 9076.67-abs(mysum)
	   	   
	   print*, mysum
	   print*, ""
	   print*, "Precision = ", diff
	40 continue   
end subroutine	   	
	  
!--------------------------------------------------------------------
! function #1
!--------------------------------------------------------------------
double precision function integral_1(x) 
   implicit none
   double precision::x
   double precision, parameter :: pi = 3.14159265358979

   integral_1 =10.0d+00*sin(x/pi)*sin(x/pi)

end function
!--------------------------------------------------------------------
! function #2
!--------------------------------------------------------------------
double precision function integral_2(x)
   implicit none
   double precision::x
   double precision::a,b
   
   a = 20.0d+00
   b = 10.0d+00

   integral_2 =sqrt(1.0d+00-((a*a-b*b)/(a*a))*sin(x)*sin(x))

end function
!--------------------------------------------------------------------
! function #3
!--------------------------------------------------------------------
double precision function integral_3(x) 
   implicit none
   double precision::x

   integral_3=(-x*x*x + 20.0d+00*x*x - x + 1)

end function
!--------------------------------------------------------------------

