module my_mod2

use my_func2

  implicit none
   integer                       :: i, n
   integer			             :: fib, fib2
   double precision				 :: t1, t2

contains

subroutine fibonacci

  print*, "Fibonacci Sequence, please enter the 'n' value"
  read*, n
  !----------------------------------------------recursive
   call cpu_time(t1)
		do i=1,n
       	end do           
         
   print*, "Result of the recursive series is", fr(n) 
   
   call cpu_time(t2)
   write(*,*) 'Elapsed time =',t2-t1
  !----------------------------------------------non-recursive 
   call cpu_time(t1)  
       
   print*, "Result of the non-recursive series is", fnr(n) 
   
   call cpu_time(t2)
   write(*,*) 'Elapsed time =',t2-t1
   
       
end subroutine fibonacci   
 
	
end module

