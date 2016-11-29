

module my_func2
 
 	implicit none
	
contains

!================================================
! FUNCTION : Recursive Fibonacci
!================================================

recursive function fr(n) result(fib)
   implicit none
   integer,intent(in)  :: n
   integer			   :: fib	
   
   if(n <= 2) then
		fib = 1
   else
   		fib = fr(n-1) + fr(n-2)
   end if
  
end function fr

!================================================
! FUNCTION : Closed Fibonacci
!================================================
 function fnr(n) result(fib2)
   
   implicit none
   integer,intent(in)          :: n
   double precision            :: fib2
   double precision, parameter :: x = 1.618
   
   fib2 = (x**n-(1-x)**n)/(x -(1-x))   	
   
  
  
end function fnr



   
 end module
 
