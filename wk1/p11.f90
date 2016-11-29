program ladder_length

implicit none
real			:: x, dx, l, l_new, diff, ladder


x = 8.1
dx = 0.1

10 continue
  l = x + (10*x)/(sqrt(x*x-64))
  
  x = x + dx
  
  l_new = x + (10*x)/(sqrt(x*x-64))
	
  diff = l-l_new
  
  if (diff > 0) then 

	go to 10 

end if

ladder = x + (l-x)

print*, "The Longest ladder would be: ", ladder, "ft"


end program ladder_length
