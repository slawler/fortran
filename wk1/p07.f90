
program sum_numbers
implicit none

real		:: x,dx,xmin,xmax,sum_2, sum_3, sum_5

! initialize values
xmin    = 0
xmax    = 100
dx      =  2
x    	= xmin
sum_2   = 0
sum_3   = 0
sum_5   = 0


10 continue

if( x >= 0 .and. x <= (xmax-2) ) then   
   x = x + dx
   sum_2 = x + sum_2

end if


if( x <= (xmax-2) ) go to 10  


x    = xmin 
xmax = 100
dx   = 3

20 continue

if( x >= 0 .and. x <= (xmax-3) ) then  
   x = x + dx
   sum_3 = x + sum_3

if( x <= (xmax-3) ) go to 20  

end if


if( x <= (xmax-3) ) go to 20  

x     = xmin 
xmax  = 100
dx    = 5

30 continue

if( x >= 0 .and. x <= (xmax-5) ) then   
   x = x + dx
   sum_5 = x + sum_5

end if

if( x <= (xmax-5) ) go to 30 

print *, "sum_2 =", sum_2
print *, "sum_3 =", sum_3
print *, "sum_5 =", sum_5



open(unit=80,file="sum.txt")

80 write(80,*) "sum_2 =", sum_2, "sum_3 =", sum_3, "sum_5 =", sum_5



end program sum_numbers








 
