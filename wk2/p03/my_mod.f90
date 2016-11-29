module my_mod

use my_func

implicit none
public
integer			  ::a1,a2
integer		      ::a,b, divisor

contains

subroutine calc()			
		
print*, "Enter a,b"
read*, a1,a2
		
		if (a1 < a2) then
			b = a1
			a = a2
			
		else if (a2 < a1)then
			b = a2
			a = a1
		end if
		
		divisor = gcd(a,b)
	
		print*, "The greatest common divisor is", divisor
 end subroutine
 
	
end module

