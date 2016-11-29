program circle	!Calculates area and perimeter of a circle

implicit none

real :: r, a, a1, p, p1
real, parameter :: pi = 3.1415927

print*, "Enter circle radius (m)" 
read*, r

a = (pi/2)*r**2
a1 = a*1550
p = pi*2*r
p1 = p*39.701

print*, "Area (m)"
print*, a
print*, "Area (in)"
print*, a1
print*, "Perimter (m)"
print*, p
print*, "Perimter (in)"
print*, p1

end program circle
