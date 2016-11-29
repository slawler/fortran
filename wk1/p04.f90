
program triangle

implicit none

real :: x1, x2, y1, y2, x3, y3
real :: X21, Y21, X31, Y31, X23, Y23
real :: a1, a2, p1, p2

!enter coordinates of vertices
print*, "Triangle Properties Calculator"
print*, " Enter coordinates of each vertex, beginning with x1,y1"
read*, x1, y1

print*, "Enter x2,y2"
read*, x2, y2

print*, "Enter x3,y3"
read*, x3,y3


X21 = x2-x1 
Y21 = y2-y1
X31 = x3-x2
Y31 = y3-y2
X23 = x1-x3
Y23 = y1-y3

!compute area
a1 = 0.5*abs(X21*Y31-Y21*X31)
!convert units (meters to inches)	
a2 = a1*1550

!perimter computation
p1 = sqrt(X21**2 + Y21**2)+sqrt(X23**2 + Y23**2)+ sqrt(X31**2+Y31**2) 
!convert units
p2 = p1*39.3701

	if (a1 == 0) then
		print*, "The coordinates entered do not form a triangle"
		
	else if (a1/= 0.0) then
 
	print*, "Area (m^2)", a1
	print*, "Area (in^2)", a2

	print*, "Perimeter (m)", p1
	print*, "Perimeter (in)", p2

	end if

end program triangle

