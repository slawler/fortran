
program geometric_series

implicit none
integer			  :: nmax, iter
integer           :: n,j,n2
double precision  :: geo,x, diff, geometric


print*, "Geometric Series Evaluater: Geo"
print*, "Enter a value between -1 and 1"
read*, x

if (x > -1 .and. x < 1) then

print*, "Please enter number of iterations"
read*, iter
 
nmax = iter
geo = 1

		do n=0,nmax
			n2 = n + 1
			geo = geo + x**n2
      
		end do
        
	geometric = 1/(1-x)
	diff = geo- geometric
	
	print*, "geometric_fortran =", geometric
	print*, "geo =", geo
	print*, "Difference =", diff
    
else if (x < -1 .or. x > 1)then 

print*, "Invalid input"

end if

end program geometric_series
