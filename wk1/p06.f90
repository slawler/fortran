
program quadratic_solver

implicit none

real :: x1, x2,a, b, c, discrim

print*, "This is a Quadractic Equation Solver"
10 print*, "Enter a"
read*, a

	if (a==0) then 
		print*, "Invalid value"
		goto 10
		
		else 
		print *, "Enter b"
	end if

read*, b
print*, "Enter c"
read*, c

discrim = b*b-(4*a*c)  

x1 = (-b + sqrt(discrim))/(2*a)
x2 = (-b - sqrt(discrim))/(2*a)

	if (discrim<0) then
	print*, "No real roots exist"
	
	else 
	
	print*, "X1 =", x1
	print*, "X2 =", x2
	
	end if



	
end program quadratic_solver
