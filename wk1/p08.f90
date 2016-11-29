program line_reader

implicit none

real	:: a, b, c, d, e, f, x1,y1, x2,y2
real	:: slope_1, slope_2

print*, "This program evaluates lines for intersection or perpendicularity"
print*, "Enter values for each line in the following format : 'ax + by = c' and 'dx + ey = f'" 
print*, "Enter 'a', 'b', 'c' for the first line"
read*, a, b, c
print*, "Enter 'd', 'e', 'f' for the first line"
read*, d, e, f 


slope_1 = (-a/b)
slope_2 = (-d/e)


	if (slope_1 == slope_2) then
	print*, "The lines are parallel, no intersection occurs"
		else if(slope_1*slope_2 == -1) then
		print*, "The lines are perpendicular"
			else if (slope_1<slope_2 .or. slope_1>slope_2) then
			print*, "The lines will intersect, but are not perpendicular"
	end if
	
end program line_reader 

