module mod_my_matrix_io

private

public ::io_print_matrix,io_print_vector,io_get_matrix,io_get_vector

contains
   !------------------------------------------------------------------------
   ! SUB: io_get_matrix
   !      reads a matrix from file in MatrixMarket format
   !------------------------------------------------------------------------
   !   Arguments:
   !   name     type      in/out description
   !   --------------------------------------------------------------------
   !   fnmatrix character in     file name
   !   n        integer   in     dimension of matrix
   !   a        double    in     array  a(n,n)
   !------------------------------------------------------------------------
   subroutine io_get_matrix(fnmatrix,n,amat)
    
      use mod_mmio

      implicit none
      ! arguments
      integer,intent(out)                                    ::n
      double precision,allocatable,dimension(:,:),intent(out)::amat
      character(len=*),intent(in)                            ::fnmatrix
      ! local
      integer                                    			 ::iunit,rows,cols,nnz,nnzmax
      integer,allocatable,dimension(:)          			 ::indx,jndx,ival
      integer                                   			 ::i,j,ipos
      double precision,allocatable,dimension(:)  			 ::rmatval
      complex,allocatable,dimension(:)          			 ::cmatval
      character(len=10) :: rep
      character(len=7)  :: field
      character(len=19) :: symm

      ! open matrix file
      iunit=50
      open(unit=iunit,file=fnmatrix,status='old')

      ! get information of matrix file
      call mminfo(iunit,rep,field,symm,rows,cols,nnz)

      write(6,100)rep 
      write(6,110)field 
      write(6,120)symm 
      write(6,130)rows
      write(6,140)cols 
      write(6,150)nnz

      ! allocate arrays to read matrix
      allocate(indx(nnz),jndx(nnz),ival(nnz))
      allocate(rmatval(nnz))
      allocate(cmatval(nnz))

      ! read matrix
      nnzmax=nnz
      rmatval=0.0d+00
      call mmread(iunit,rep,field,symm,rows,cols,nnz,nnzmax,  &
                  indx,jndx,ival,rmatval,cmatval)
 
      ! close matrix file
      close(iunit)

      ! allocate dense matrix
      allocate(amat(rows,cols))
      ! set matrix to zero
      do j=1,cols
         do i=1,rows
            amat(i,j)=0.0d+00
         enddo
      enddo

      ! assign values from array rval to matrix amat
      do i=1,rows
         do j=1,cols
            ipos=(j-1)*cols+i
            amat(i,j)=rmatval(ipos)
         enddo
      enddo
  
      ! assign dimension to n 
      n=cols

      ! free memory
      deallocate(indx,jndx,ival)
      deallocate(rmatval)
      deallocate(cmatval)

      ! format for prints
100   format('[INFO(io_get_matrix)]: rep  :',a10)
110   format('[INFO(io_get_matrix)]: field:',a7)
120   format('[INFO(io_get_matrix)]: symm :',a17)
130   format('[INFO(io_get_matrix)]: rows =',i10)
140   format('[INFO(io_get_matrix)]: cols =',i10)
150   format('[INFO(io_get_matrix)]: nnz  =',i10)

   end subroutine io_get_matrix


   !------------------------------------------------------------------------
   ! SUB: io_get_vector
   !      reads a vector from file in MatrixMarket format
   !------------------------------------------------------------------------
   !   Arguments:
   !   name     type      in/out description
   !   --------------------------------------------------------------------
   !   fnvector character in     file name
   !   n        integer   in     dimension of vector
   !   v        double    in     array v(n)
   !------------------------------------------------------------------------
   subroutine io_get_vector(fnvector,n,v)
    
      use mod_mmio

      implicit none
      ! arguments
      integer,intent(out)                                  ::n
      double precision,allocatable,dimension(:),intent(out)::v
      character(len=*),intent(in)                          ::fnvector
      ! local
      integer                                    ::iunit,rows,cols,nnz,nnzmax
      integer,allocatable,dimension(:)           ::indx,jndx,ival
      integer                                    ::i
      double precision,allocatable,dimension(:)  ::rmatval
      complex,allocatable,dimension(:)           ::cmatval
      character(len=10) :: rep
      character(len=7)  :: field
      character(len=19) :: symm

      ! open matrix file
      iunit=50
      open(unit=iunit,file=fnvector,status='old')

      ! get information of matrix file
      call mminfo(iunit,rep,field,symm,rows,cols,nnz)

      write(6,100)rep 
      write(6,110)field 
      write(6,120)symm 
      write(6,130)rows
      write(6,140)cols 
      write(6,150)nnz

      ! allocate arrays to read matrix
      allocate(indx(nnz),jndx(nnz),ival(nnz))
      allocate(rmatval(nnz))
      allocate(cmatval(nnz))

      ! read matrix
      nnzmax=nnz
      rmatval=0.0d+00
      call mmread(iunit,rep,field,symm,rows,cols,nnz,nnzmax,  &
                  indx,jndx,ival,rmatval,cmatval)
 
      ! close matrix file
      close(iunit)

      ! allocate vector
      allocate(v(rows))
      ! set matrix to zero
      do i=1,rows
         v(i)=0.0d+00
      enddo

      ! assign values from array rval to matrix amat
      do i=1,rows
         v(i)=rmatval(i)
      enddo
  
      ! assign dimension to n 
      n=rows

      ! free memory
      deallocate(indx,jndx,ival)
      deallocate(rmatval)
      deallocate(cmatval)

      ! format for prints
100   format('[INFO(io_get_vector)]: rep  :',a10)
110   format('[INFO(io_get_vector)]: field:',a7)
120   format('[INFO(io_get_vector)]: symm :',a17)
130   format('[INFO(io_get_vector)]: rows =',i10)
140   format('[INFO(io_get_vector)]: cols =',i10)
150   format('[INFO(io_get_vector)]: nnz  =',i10)

   end subroutine io_get_vector


!------------------------------------------------------------------------
! SUB: io_print_matrix
!      prints a matrix a (console output)
!------------------------------------------------------------------------
!   Arguments:
!   name     type      in/out description
!   ---------------------------------------------------------------------
!   n        integer   in     dimension of matrix
!   a        double    in     array of nxn
!------------------------------------------------------------------------
   subroutine io_print_matrix(n,a)

      implicit none
      ! arguments
      integer,intent(in)                        ::n
      double precision,dimension(n,n),intent(in)::a
      ! local
      integer::i,j

      write(6,20)n
      do i=1,n
         do j=1,n
            write(6,10,advance='no') a(i,j)
         end do
         write(6,*)
      end do
 10   format(f10.5)
 20   format('[INFO(io_print_matrix)]: matrix dimension:',i10)

   end subroutine io_print_matrix

!------------------------------------------------------------------------
! SUB: io_print_vector
!      prints a vector x (console output)
!------------------------------------------------------------------------
!   Arguments:
!   name     type      in/out description
!   ---------------------------------------------------------------
!   n        integer   in     dimension of vector
!   x        double    in     vector of 1xn
!------------------------------------------------------------------------
   subroutine io_print_vector(n,x)

      implicit none
      ! arguments
      integer,intent(in)                      ::n
      double precision,dimension(n),intent(in)::x
      ! local
      integer::i

      write(6,20)n
      do i=1,n
         write(6,10) x(i)
      end do
 10   format(f10.5)
 20   format('[INFO(io_print_vector)]: vector dimension:',i10)
   end subroutine io_print_vector
   
  
end module
