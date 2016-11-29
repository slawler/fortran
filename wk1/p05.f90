
program mean

   implicit none
   integer                       :: i
   integer, parameter            :: n=100
   real, dimension(n)			 :: a, an, avgn, bn1, xinv 
   real, dimension (1)			 :: a1, avg1, b1, h_mean, sd, sum_xinv	
   real (kind(1.d0))			 :: x, g_mean	

   open(unit=80,status='old',file="temperature.txt")
  
   do i=1,n
      read(80,*) a(i)
   end do
    
   close(80)
!ARITHMETIC MEAN
 a1 = sum(a) 	 		!sums a to one scalar value
 an = sum(a) 	 		!sums a to an array of size n
 
 avg1 = a1/n			! average (scalar)
 avgn = an/n			! average (array)
  
 bn1 = (a-avgn)*(a-avgn)	
 b1 = sum(bn1)
 
  sd = sqrt(b1/n)

!GEOMETRIC MEAN
x = a(1)

do i=1,(n-1)


x = x*a(i+1)


end do

  
 g_mean = x**(1.0/n)


!HARMONIC MEAN 
 xinv = 1/a
 sum_xinv= sum(xinv)
 
 h_mean = n/sum_xinv
 
 print *, "Arithmetic Mean =", avg1
 print *, "Standard Deviation =", sd  
 print *, "Geometric Mean =", g_mean   
 print *, "Harmonic Mean =", h_mean

 
 
end program mean
