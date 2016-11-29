
program taylor_sin

implicit none
integer			  :: nmax, iter
integer           :: n,j,n2fact,n2
double precision  :: sinx,x, diff



print*, "Taylors expansion for the sine of an angle"  
print*, "Enter number of iterations:"
read*, iter

nmax = iter

print*, "Enter angle in radians:"
read*, x

sinx = x


do n=2,nmax
   n2 = 2*n-1
   n2fact = 1


   do j=1,n2
      n2fact = n2fact * j
   end do
   
   sinx = sinx + (x**n2)*(-1)**(n-1)/dble(n2fact) 
   
end do
		

write(6,11) x,sinx
write(6,12) x,sin(x)
11 format('sine      (',f17.14,') =',f17.14)
12 format('sinfortran(',f17.14,') =',f17.14)


diff = abs((sinx-sin(x))/sin(x))

print*, "Difference =", diff

end program taylor_sin
