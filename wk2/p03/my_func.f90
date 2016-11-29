

module my_func
 
 	implicit none

	
contains

	recursive function gcd(a,b) result(r)
	  integer	::a,b
      integer	::r
      
      if (mod(a,b) /=0) then
      r = gcd(b,mod(a,b))
      
      else
      r = b
      end if

   end function gcd
   
 end module
 
