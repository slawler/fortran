program bc

implicit none
integer :: n, k, coeff, bn_rec
integer	::coeff2, bc_no_rec
double precision ::t1,t2


print*, "Binomial Coefficient: Enter n,k"
read*, n,k
	if ( k > n ) then

		print*, "Invalid entry" 

		else


	print*, ""
	print*, ""
	call cpu_time(t1)

	 coeff= bn_rec(n,k) !----------Call recursive function
	 
	print*, "Result of the recursive series is", coeff

	call cpu_time(t2)
	write(*,*) 'Elapsed time in seconds =',t2-t1
	print*, ""

	call cpu_time(t1)

	 coeff2= bc_no_rec(n,k) !----------Call non-recursive function
	 
	print*, "Result of the non-recursive series is", coeff2

	call cpu_time(t1)
	 
	write(*,*) 'Elapsed time in seconds =',t2-t1 


	end if


end program
 
!--------------------------------------------------------------------
! recursive function
!--------------------------------------------------------------------
integer recursive function bn_rec(n,k) result(r)
   
   if (n==k) then 
	   r =1
	   	   else if (k==0) then 
	   r =1   
	   
	   else
	   r = bn_rec(n-1,k-1) + bn_rec(n-1,k) 
   end if

end function
!--------------------------------------------------------------------
! non-recursive function
!--------------------------------------------------------------------
integer function bc_no_rec(n,k) result (r2)
integer:: i,j,ij, diff
integer:: nfact,kfact, diff2 

!------initialize factorials
nfact = product((/(i, i=1,n)/))
kfact = product((/(i, i=1,k)/))
diff_fact = product((/(i, i=1,n-k)/))
	 	 
	
   	 r2 = nfact/(kfact*diff_fact)   
	   
end function	
	
