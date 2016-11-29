program rectangle	!Area and perimeter of a rectangle

implicit none

real :: l, w, a1, a2, p1, p2

print*, "Enter rectangle length (m)"
read*, l
print*, "Enter rectangle width (m)"
read*, w

a1 = l*w			! Calculates area in meters
a2 = a1*1550		! Converts area to inches
p1 = 2*(l + w)  	! Calculates perimeter in meters
p2 = p1*39.3701		! Converts perimeter to inches 

print*, "Area (m)"
print*, a1
print*, "Area (in)"
print*, a2
print*, "Perimter (m)"
print*, p1
print*, "Perimeter (in)"
print*, p2

end program rectangle
