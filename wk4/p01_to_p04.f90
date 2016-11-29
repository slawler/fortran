! CSI 501
!------------------------------------------------
! This program tests number sorting algorithms
!------------------------------------------------ 
program num_sorter
   
   implicit none
   
   integer                        		::istat
   integer,allocatable,dimension(:)		::a
   double precision                		::t0,t1
   character(len=99)               		::fname0='numbers_list.txt'
   character(len=99)               		::fname1
   character(len=99)               		::s
   
       
   interface
	  subroutine read_numbers(fname0,a,istat)
	  character(len=99)                 ::fname0
      integer,intent(inout)             ::istat
       integer,allocatable,dimension(:)	::a
      end subroutine 
   end interface 
   interface 
	  subroutine print_numbers(fname1,a)
      character(len=99),intent(in)    	::fname1
       integer,allocatable,dimension(:)	::a
      end subroutine 
   end interface
   interface 
	  subroutine selection_sort(fname1,a)
      character(len=99),intent(in)    	::fname1
      integer,allocatable,dimension(:)	::a,b
      end subroutine 
   end interface
   interface 
      subroutine insertion_sort(fname1,a)
      character(len=99),intent(in)    	::fname1
      integer,allocatable,dimension(:)	::a
      end subroutine 
   end interface
   interface 
	  subroutine bubble_sort(fname1,a)
      character(len=99),intent(in)    	::fname1
      integer,allocatable,dimension(:)	::a
      end subroutine 
   end interface

         
      
!-----------------------------------------------------------------------
!-------------USER CONSOLE----------------------------------------------
!---Provides user with choice of sorting method
!-----------------------------------------------------------------------
print*, ""
print*, "Number Sorting Programs"
print*, ""
print*, "Insertion             ", "[i]"
print*, "Selection             ", "[s]"
print*, "Bubble                ", "[b]"
print*, "Quit                  ", "[q]"
print*, ""
print*, "Enter your selection:"


read*, s

	if (s == 's') then 
		goto 25
		else if(s == 'i') then 
			goto 35
			
		else if(s == 'b') then 
			goto 45
			
		else if(s == 'q') then 
			goto 105
		
		else if (s /= 'i' .or. s /='s' .or. s /='b' .or. s /='q') then
		print*, "--------------------------------------------------------"
		print*, "Invalid entry"
		print*, "---------------------------------------------------------"
	end if
	
   
   25 continue   
    call read_numbers(fname0,a,istat) ! Reads file
   
   if(istat>0) then
	 write(6,10)
     stop
   endif
     
   fname1='sort_numbers_selection.txt' !Calls selection sorting algorithm
   call cpu_time(t0) ! Begin Timer
   call selection_sort(fname1,a)
   call cpu_time(t1)! End Timer
   print *,"Selection sort process time (seconds):",t1-t0
   goto 105

   35 continue
     call read_numbers(fname0,a,istat)
   
   if(istat>0) then
	 write(6,10)
     stop
   endif
   
   fname1='sort_numbers_insertion.txt' !Calls insertion sorting algorithm
   call cpu_time(t0) ! Begin Timer
   call insertion_sort(fname1,a)
   call cpu_time(t1)! End Timer
   print *,"Insertion sort process time (seconds):",t1-t0
   goto 105
     
   45 continue
     call read_numbers(fname0,a,istat)
   
   if(istat>0) then
	 write(6,10)
     stop
   endif
   
   fname1='sort_numbers_bubble.txt'  !Calls bubble sorting algorithm
   call cpu_time(t0) ! Begin Timer
   call bubble_sort(fname1,a)
   call cpu_time(t1)! End Timer
   print *,"Bubble sort process time (seconds):",t1-t0    
   goto 105   
      
10 format('[ERROR]: failed opening file ',a99,' stopping program')
105 continue
 
end program num_sorter

!-----------------------------------------------------------------
! subroutine: read numbers from file
! input:  fname             = file name
!         allocatable array = array to store numbers
! output: allocated array
!-----------------------------------------------------------------
    subroutine read_numbers(fname,a,istat)
      character(len=*)                              ::fname
      integer,allocatable,dimension(:)				::a
      integer,intent(out)                           ::istat
      integer									    ::ios,n,tmp,i
            
      write(6,10) fname   ! message to users
      open(unit=50,iostat=ios,err=200,file=fname,status="old")
     
      n = 0   ! count number of integers in the file
      do
         read(50,*,end=110) tmp
         n = n + 1      
      end do
      
  110 continue
      rewind(50)
      
      allocate(a(n))				! allocate array
            
      do i=1,n 						! read numbers
         read(50,*) a(i)
      end do
            
      close(50)
      write(6,20) n
       
      istat=ios   ! unable to open file? write a message and return
  200 if(istat>0) then
         write(6,30) trim(fname)
         return
      end if
      
   10 format('[INFO] : reading file ',a99)
   20 format('[INFO] : # of integers in file is ',i5)
   30 format('[ERROR]: unable to open file ',a99)
   
   end subroutine read_numbers

!-----------------------------------------------------------------
! subroutine:print numbers to file
! input:  fname             = file name
!         allocatable array = array to store numbers
!-----------------------------------------------------------------
   subroutine print_numbers(fname,a)
      implicit none
      
      character(len=*),intent(in)   	 ::fname
      integer,allocatable,dimension(:)	 ::a
      integer 							 ::i,n
      
      n=size(a)
      open(unit=50,file=fname)
      
      do i=1,n
         write(50,*)a(i)
      end do
      close(50)
      
   end subroutine print_numbers
   
!-----------------------------------------------------------------
!subroutine selection sort
!----------------------------------------------------------------- 
subroutine selection_sort(fname,a)
      
      implicit none
      character(len=99),intent(in)       ::fname
      integer,allocatable,dimension(:)	 ::a	
	  integer 							 ::i,n,iptr,j,temp

      n=size(a)
      open(unit=50,file=fname)
       		      
      do i=1,n-1			
		 iptr=i 
	  	 do j=i+1,n
			if (a(j)<a(iptr)) then !I changed this from  '>' to  '<' from the Pseudocode example
				iptr=j              !so that the sorting for all three algorithms would output 	 
			end if				     !the numbers in ascending order				
	  	 end do
	  	 temp = a(i)
	  	 a(i) = a(iptr)
	  	 a(iptr) = temp
	  end do
	  	 
	  print*,a
      write(50,*)a
	  
      close(50)
	  print*, 
      
end subroutine      
      
!-----------------------------------------------------------------
!subroutine insertion sort
!-----------------------------------------------------------------
subroutine insertion_sort(fname,a)
      
      implicit none
      character(len=99),intent(in)    	 ::fname
      integer,allocatable,dimension(:)	 ::a
      integer 							 ::i,n,key,j
				
	  n=size(a)
      open(unit=50,file=fname)
      print*,a 
       		      
      do j=2,n
         key=a(j)   			
		 i=j-1
		 do while (i>0 .and. a(i)>key)
			a(i+1)=a(i)	
			i=i-1	
	  	 end do
	  	a(i+1)=key
	  end do
	  print*,a
      write(50,*)a
	  
      close(50)
	  print*, 	
        
end subroutine 

!-----------------------------------------------------------------
!subroutine bubble sort
!-----------------------------------------------------------------
! 
subroutine bubble_sort(fname,a)
      
      implicit none
      character(len=99),intent(in)       ::fname
      integer,allocatable,dimension(:)	 ::a
	  integer 							 ::i,n,j,temp

      n=size(a)
      open(unit=50,file=fname)
       print*,a 		      
      do i=1,n-1			
	  	 do j=1,n-1
			if (a(j)>a(j+1)) then
				temp = a(j)
				a(j) = a(j+1)
				a(j+1) = temp
			end if
	  	 end do
	  end do
	  	 
	  print*,a
      write(50,*)a
	  
      close(50)
	  print*, 
      
end subroutine 


      
      
      
      
      
      
