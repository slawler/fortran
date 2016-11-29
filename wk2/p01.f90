program read_functions

   implicit none
   double precision 	:: l, w, r
   double precision 	:: x1, x2, y1, y2, x3, y3
   double precision 	:: rect, circ, tri
   character(len=10)	:: s



10 continue
print*, ""
print*, "Area Calculator"
print*, ""
print*, "Area of a rectangle   ", "[r]"
print*, "Area of a circle      ", "[c]"
print*, "Area of a triangle    ", "[t]"
print*, "Quit                  ", "[q]"
print*, ""
print*, "Enter your selection:"

read*, s

	if (s == 'r') then 
	 
		print*, "Enter rectangle length & width"
		read*, l, w
		print*, "Area of rectangle =", rect(l,w), "units"
		goto 10
		
	else if(s == 'c') then 
	 
		print*, "Enter radius of circle"
		read*, r
		print*, "Area of circle =", circ(r), "units"
		goto 10
		
	else if(s == 't') then 
	 
		print*, "Triangle Properties Calculator"
		print*, " Enter coordinates of each vertex, beginning with x1,y1"
		read*, x1, y1
		print*, "Enter x2,y2"
		read*, x2, y2
		print*, "Enter x3,y3"
		read*, x3,y3
		
		print*, "Area of triangle =", tri(x1, x2, y1, y2, x3, y3), "units"
		goto 10	
		
	else if(s == 'q') then 
	goto 20
	
	else if (s /= 'r' .or. s /='c' .or. s /='t' .or. s /='q') then
	print*, "--------------------------------------------------------"
	print*, "Invalid entry"
	print*, "---------------------------------------------------------"
	goto 10
	
end if

20 continue

end program read_functions

!================================================
! FUNCTION : Area of Rectangle
!================================================
function rect(l,w) result(a1)
   
double precision, intent (in) :: l, w
double precision			  :: a1	

a1 = l*w			
  
end function rect

!--------------------------------------------------------------------
! function: Area of Circle
!--------------------------------------------------------------------
function circ(r) result(a1)
   
double precision, intent (in) :: r
double precision			  :: a1	
real, parameter :: pi = 3.1415927

a1 = (pi)*r*r			
  
end function circ

!--------------------------------------------------------------------
! function #3
!--------------------------------------------------------------------

function tri(x1, x2, y1, y2, x3, y3) result(a1)
   
double precision, intent (in) :: x1, x2, y1, y2, x3, y3
double precision			  :: X21, Y21, X31, Y31, X23, Y23
double precision			  :: a1

X21 = x2-x1 
Y21 = y2-y1
X31 = x3-x2
Y31 = y3-y2
X23 = x1-x3
Y23 = y1-y3			

a1 = 0.5*abs(X21*Y31-Y21*X31)

  
end function tri

