! CSI 501
!------------------------------------------------
! Matrix & Vector Operations
!================================================
program matrix_ops

   use mod_my_matrix_io 
   use linear_algebra_functions	
  

   implicit none

   integer                                    ::nm,nv, nm2, nv2
   double precision,allocatable,dimension(:,:)::amat, bmat, cmat,cmat2
   double precision,allocatable,dimension(:)  ::vvect, vvect2, vvect3, vvect4, vvect5
   double precision							  ::scalar,scal_1,scal_2 
   character(len=80)                          ::fnmatrix,fnmatrix2,fnvector,fnvector2
   character(len=80)                          ::s
	   	     
    
   print*, "This program performs matrix and vector operations"	
    
    
    !----------------USER CONSOLE:---------------------  
	10 continue
	print*, ""
	print*, "Choose an Option"
	print*, ""
	print*, "Matrix Operations          ", "[m]"
	print*, "Vector Operations          ", "[v]"
	print*, "Mixed Matrix & Vector Ops  ", "[b]"
	print*, "Quit                       ", "[q]"
    print*, ""
	print*, ""
	print*, "Enter your selection:"
	
	read*, s

	!Directs user to input array preference 
	if (s == 'm') then 
		goto 20
		else if (s == 'v') then
		goto 30
		else if (s == 'b') then 
		goto 40
		else if(s == 'q') then 
		goto 50
		
		else if (s /= 'm' .or. s /='v' .or. s /='b' .or. s /='q') then
		print*, "--------------------------------------------------------"
		print*, "Invalid entry"
		print*, "---------------------------------------------------------"
		goto 10
	end if	
	
!======================================================================
!=======================MATRIX OPERATIONS==============================
!======================================================================
  20 continue
  
   write(6,100)
   read *,fnmatrix

   ! read matrix, place it into dense matrix format
   call io_get_matrix(fnmatrix,nm,amat)
   ! print matrix in readable format
   call io_print_matrix(nm,amat)
	 
 
   write(6,100)
   read *,fnmatrix2

   ! read matrix, place it into dense matrix format
   call io_get_matrix(fnmatrix2,nm2,bmat)
   ! print matrix in readable format
   call io_print_matrix(nm2,bmat)

	!calls subroutines to perform operations
   allocate (cmat(nm,nm),cmat2(nm,nm))
   
   call matrix_sum(nm,amat,bmat,cmat) !Sums matrices
   call matrix_mult(nm,amat,bmat,cmat2)	!Multiplies matrices

   cmat = matmul(amat,bmat) !Performs check using intrinsic function
   print*, "[Check against intrinsic function]:",cmat2
   print*, "-----------------------------------------------"
   
  
  !-----give the user option to print text file of output

  print*, ""
  print*, "----------SAVE FILE OPTION--------------" 
  print*, "Type 'y' if you would like to save solution to text file" 	
  read*, s

	if (s == 'y') then 
	  
	   open(unit=90,file="Matrix_ops.txt")
	   write(90,*) 'Sum',cmat, 'Product',cmat2
	   print*, "FIle saved in directory"
	   close(unit=90)
	   goto 10   
   end if
	
  !I tried to deallocate the vectors/matrices so that the program could 
  !loop back to the 'console' but I couldnt quite get it to work, so instead,
  !I just tell the program to exit after each execution.... 	
  deallocate (cmat,cmat2)
						 	

   goto 50
!======================================================================
!=========================VECTOR OPERATIONS============================
!======================================================================
    
   30 continue 
        
   print*, "This program performs vector operations"	
  
   write(6,110)
   read *,fnvector
   ! read vector
   call io_get_vector(fnvector,nv,vvect)
   ! print vector in readable format
   call io_print_vector(nv,vvect)
	 
 
   write(6,110)
   read *,fnvector2
   ! read vector
   call io_get_vector(fnvector2,nv2,vvect2)
   ! print vector in readable format
   call io_print_vector(nv2,vvect2)

   allocate (vvect3(nv),vvect4(nv),vvect5(nv)) 
   
   call vector_sum(nv,vvect,vvect2,vvect3)
   call vector_dot(nv,vvect,vvect2,scalar)
   call vector_cross(nv,vvect,vvect2,vvect4)
   call vector_length(nv,vvect,vvect2,scal_1,scal_2)   
   call vector_norm(nv,vvect,vvect2,vvect3,vvect5)
  
    !-----give the user option to print text file of output

  print*, ""
  print*, "----------SAVE FILE OPTION--------------" 
  print*, "Type 'y' if you would like to save solution to text file" 	
  read*, s

	if (s == 'y') then 
	  
	   open(unit=90,file="Vector_ops.txt")
	   write(90,*) 'Sum=',vvect3, 'Dot=', scalar, 'Cross=',vvect4, 'length=',scal_2, 'norm1 =', vvect3, 'norm2 =', vvect5 
	   print*, "FIle saved in directory"
	   close(unit=90)
	   goto 10   
   end if
   
   deallocate (vvect3, vvect4, vvect5) 
   
   goto 50
   
!======================================================================
!==================MATRIX-VECTOR OPERATIONS============================
!======================================================================

40 continue   
   
   write(6,100) 
   read *,fnmatrix

   ! read matrix, place it into dense matrix format
   call io_get_matrix(fnmatrix,nm,amat)
   ! print matrix in readable format
   call io_print_matrix(nm,amat)
   
   write(6,110)
   read *,fnvector
   
   ! read vector
   call io_get_vector(fnvector,nv,vvect)
   ! print vector in readable format
   call io_print_vector(nv,vvect)
	 
   allocate (vvect2(nv)) 
   call mixed_ops(nm,nv,amat,vvect,vvect2)
   
   deallocate (vvect2) 
   
   goto 50

   
100 format('[INFO]: Please enter name of matrix')
110 format('[INFO]: Please enter name of vector')

50 continue
print*, "goodbye"


end program


