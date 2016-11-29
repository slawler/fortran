module mod_mmio

private

public :: mmread, mminfo, mmwrite
contains
   !------------------------------------------------------------------------      
   ! This routine will read data from a matrix market formatted file.
   ! The data may be either sparse coordinate format, or dense array format.
   !
   ! The unit iunit must be open, and the file will be rewound on return.
   !
   ! 20-Sept-96  Karin A. Remington, NIST ACMD (karin@cam.nist.gov)
   ! 18-Oct-96   Change in routine name to match C and Matlab routines.
   ! 30-Oct-96   Bug fixes in mmio.f:
   !                  -looping for comment lines
   !                  -fixed non-ansi zero stringlength
   !                  -incorrect size calculation for skew-symmetric arrays
   !          Other changes in mmio.f:
   !                  -added integer value parameter to calling sequences  
   !                  -enforced proper count in size info line
   !                  -added routine to count words in string (countwd)
   !            (Thanks to G.P.Leendetse and H.Oudshoom for their review
   !             of the initial version and suggested fixes.)
   ! 15-Oct-08  fixed illegal attempt of mimicking "do while" construct
   !            by redifing limits inside loop. (lines 443-450)
   !            (Thanks to Geraldo Veiga for his comments.)
   !------------------------------------------------------------------------      
   !   Arguments:
   !   name     type      in/out description
   !   ---------------------------------------------------------------
   !   iunit    integer     in   Unit identifier for the file
   !                             containing the data to be read.
   !                             Must be open prior to call.
   !                             Will be rewound on return.
   !   rep     character*10 out  Matrix Market 'representation' 
   !                             indicator. On return:
   !                                coordinate   (for sparse data)
   !                                array        (for dense data)
   !                                elemental    (to be added)    
   !   field   character*7  out  Matrix Market 'field'. On return:
   !                                real 
   !                                complex
   !                                integer
   !                                pattern
   !   symm    character*19 out  Matrix Market 'field'. On return:
   !                                symmetric
   !                                hermitian
   !                                skew-symmetric
   !                                general          
   !   rows     integer     out  Number of rows in matrix.
   !   cols     integer     out  Number of columns in matrix.
   !   nnz      integer     out  Number of nonzero entries required to
   !                             store matrix.
   !   nnzmax   integer     in   Maximum dimension of data arrays.
   !   indx     integer(nnz)out  Row indices for coordinate format.
   !                             Undefined for array format.
   !   jndx     integer(nnz)out  Column indices for coordinate format.
   !                             Undefined for array format.
   !   ival     integer(nnz) out Integer data (if applicable, see 'field')
   !   rval     double(nnz) out  Real data (if applicable, see 'field')
   !   cval     complex(nnz)out  Complex data (if applicable, see 'field')
   !------------------------------------------------------------------------
   subroutine mmread(iunit,rep,field,symm,rows,cols,nnz,nnzmax,  &
                     indx,jndx,ival,rval,cval)

      implicit none
      !
      ! Declarations:
      !
      integer,dimension(:)         :: ival
      double precision,dimension(:):: rval
      complex,dimension(:)         :: cval
      integer,dimension(:)         :: indx
      integer,dimension(:)         :: jndx
      double precision             :: rpart,ipart
      integer                      :: i, rows, cols, nnz, nnzreq, nnzmax, iunit
      integer                      :: icount,next
      character(len=14)            :: mmhead
      character(len=6)             :: mmtype
      character(len=10)            :: rep
      character(len=7)             :: field
      character(len=19)            :: symm
      character(len=1024)          :: tmp1
      character(len=2)             :: tmp2
      integer                      :: start,slen
      !
      ! Read header line and check validity:
      !
      read (iunit,end=1000,fmt=5) tmp1
 5    format(1024A)
      slen=1024
      start=1
      call getwd(mmhead,tmp1,slen,start,next,icount)
      if ( icount .eq. 0 ) go to 5000
      call getwd(mmtype,tmp1,slen,next,next,icount)
      if ( icount .eq. 0 ) go to 5000
      call getwd(rep,tmp1,slen,next,next,icount)
      if ( icount .eq. 0 ) go to 5000
      call getwd(field,tmp1,slen,next,next,icount)
      if ( icount .eq. 0 ) go to 5000
      call getwd(symm,tmp1,slen,next,next,icount)
      if ( icount .eq. 0 ) go to 5000
      if ( mmhead .ne. '%%MatrixMarket' ) go to 5000
      !
      ! Convert type code to lower case for easier comparisons:
      !
      call lowerc(mmtype,1,6)
      if ( mmtype .ne. 'matrix' ) then
         print *,'Invalid matrix type: ',mmtype
         print *,'This reader only understands type ''matrix''.'
         stop
      else
         start=1
         slen=10
         call lowerc(rep,start,slen)
         slen=7
         call lowerc(field,start,slen)
         slen=19
         call lowerc(symm,start,slen)
      endif
      !
      ! Test input qualifiers:
      !
      if (rep .ne. 'coordinate' .and. rep .ne. 'array' )             go to 6000
      if (rep .eq. 'coordinate' .and. field .ne. 'integer' .and.  &
          field .ne. 'real' .and. field .ne. 'complex' .and.      &
          field .ne. 'pattern')                                      go to 7000
      if (rep .eq. 'array' .and. field .ne. 'integer' .and.       &
          field .ne. 'real' .and. field .ne. 'complex' )             go to 8000
      if (symm .ne. 'general' .and. symm .ne. 'symmetric' .and.   &
          symm .ne. 'hermitian' .and. symm .ne. 'skew-symmetric')    go to 9000
      !
      ! Read through comment lines, ignoring content:
      !
      read (iunit,end=2000,fmt=200) tmp2
 200  format(1a)
 10   continue
         if ( tmp2(1:1) .ne. '%' ) then
            go to 20
         endif
         read (iunit,end=2000,fmt=200) tmp2
      goto 10
 20   continue
      !
      ! Just read a non-comment.
      !   Now, back up a line, and read for first int, and back up
      !   again. This will set pointer to just before apparent size
      !   info line.
      !   Before continuing with free form input, count the number of
      !   words on the size info line to ensure there is the right amount
      !   of info (2 words for array matrices, 3 for coordinate matrices).
      !
      backspace (iunit)
      read (iunit,end=1000,fmt=5) tmp1
      start=1
      call countwd(tmp1,slen,start,icount)
      if ( rep .eq. 'array' .and. icount .ne. 2 )      go to 3000
      if ( rep .eq. 'coordinate' .and. icount .ne. 3 ) go to 3500
      !
      !   Correct number of words are present, now back up and read them.
      !
      backspace (iunit)
 
      if ( rep .eq. 'coordinate' ) then 
      !
      ! Read matrix in sparse coordinate format
      !
         read (iunit,fmt=*) rows,cols,nnz
         !
         ! Check to ensure adequate storage is available
         !
         if ( nnz .gt. nnzmax ) then
             print *,'insufficent array lengths for matrix of ',nnz,' nonzeros.' 
             print *,'resize nnzmax to at least ',nnz,'. (currently ',nnzmax,')'
             stop
         endif
         !
         ! Read data according to data type (real,integer,complex, or pattern)
         !
         if ( field .eq. 'integer' ) then
            do i=1,nnz
               read (iunit,fmt=*,end=4000) indx(i),jndx(i),ival(i)
            enddo
         elseif ( field .eq. 'real' ) then
            do i=1,nnz
               read (iunit,fmt=*,end=4000) indx(i),jndx(i),rval(i)
            enddo
         elseif ( field .eq. 'complex' ) then
            do i=1,nnz
               read (iunit,fmt=*,end=4000) indx(i),jndx(i),rpart,ipart
               cval(i) = cmplx(real(rpart),real(ipart))
            enddo
         elseif ( field .eq. 'pattern' ) then
            do i=1,nnz
               read (iunit,fmt=*,end=4000) indx(i),jndx(i)
            enddo
         else 
              print *,'''',field,''' data type not recognized.'
              stop
         endif
         rewind(iunit)
         return
      elseif ( rep .eq. 'array' ) then
         !
         ! Read matrix in dense column-oriented array format
         !
         read (iunit,fmt=*) rows,cols
         !
         ! Check to ensure adequate storage is available
         !
         if ( symm .eq. 'symmetric' .or. symm .eq. 'hermitian' ) then
            nnzreq = (rows*cols - rows)/2 + rows
            nnz = nnzreq
         elseif ( symm .eq. 'skew-symmetric' ) then
            nnzreq = (rows*cols - rows)/2 
            nnz = nnzreq
         else
            nnzreq = rows*cols
            nnz = nnzreq
         endif
         if ( nnzreq .gt. nnzmax ) then
            print *,'insufficent array length for ',rows, ' by ', &
                    cols,' dense ',symm,' matrix.'
            print *,'resize nnzmax to at least ',nnzreq,'. (currently ',nnzmax,')'
            stop
         endif
         !
         ! Read data according to data type (real,integer,complex, or pattern)
         !
         if ( field .eq. 'integer' ) then
            do i=1,nnzreq
               read (iunit,fmt=*,end=4000) ival(i)
            enddo
         elseif ( field .eq. 'real' ) then
            do i=1,nnzreq
               read (iunit,fmt=*,end=4000) rval(i)
            enddo
         elseif ( field .eq. 'complex' ) then
            do i=1,nnzreq
               read (iunit,fmt=*,end=4000) rpart,ipart
               cval(i) = cmplx(real(rpart),real(ipart))
            enddo
         else
            print *,'''pattern'' data not consistant with type ''array'''
            stop
         endif
         rewind(iunit)
         return
      else
         print *,'''',rep,''' representation not recognized.'
         print *, 'Recognized representations:'
         print *, '   array'
         print *, '   coordinate'
         stop
      endif
      !
      ! Various error conditions:
      !
 1000 print *,'Premature end-of-file.'
      print *,'No lines found.'
      stop
 2000 print *,'Premature end-of-file.'
      print *,'No data lines found.'
      stop
 3000 print *,'Size info inconsistant with representation.'
      print *,'Array matrices need exactly 2 size descriptors.'
      print *, icount,' were found.'
      stop
 3500 print *,'Size info inconsistant with representation.'
      print *,'Coordinate matrices need exactly 3 size descriptors.'
      print *, icount,' were found.'
      stop
 4000 print *,'Premature end-of-file.'
      print *,'Check that the data file contains ',nnz,' lines of  i,j,[val] data.'
      print *,'(it appears there are only ',i,' such lines.)'
      stop
 5000 print *,'Invalid matrix header: ',tmp1
      print *,'Correct header format:'
      print *,'%%MatrixMarket type representation field symmetry'
      print *
      print *,'Check specification and try again.'
 6000 print *,'''',rep,''' representation not recognized.'
      print *, 'Recognized representations:'
      print *, '   array'
      print *, '   coordinate'
      stop
 7000 print *,'''',field,''' field is not recognized.'
      print *, 'Recognized fields:'
      print *, '   real'
      print *, '   complex'
      print *, '   integer'
      print *, '   pattern'
      stop
 8000 print *,'''',field,''' arrays are not recognized.'
      print *, 'Recognized fields:'
      print *, '   real'
      print *, '   complex'
      print *, '   integer'
      stop
 9000 print *,'''',symm,''' symmetry is not recognized.'
      print *, 'Recognized symmetries:'
      print *, '   general'
      print *, '   symmetric'
      print *, '   hermitian'
      print *, '   skew-symmetric'
      stop
   end subroutine mmread
   !------------------------------------------------------------------------ 
   !------------------------------------------------------------------------ 
    
   !------------------------------------------------------------------------      
   ! This routine will read header information from a Matrix Market 
   ! formatted file.  
   !
   ! The unit iunit must be open, and the file will be rewound on return.
   !
   ! 20-Sept-96  Karin A. Remington, NIST ACMD (karin@cam.nist.gov)
   ! 18-Oct-96   Change in routine name to match C and Matlab routines.
   ! 30-Oct-96   Bug fixes in mmio.f:
   !                  -looping for comment lines
   !                  -fixed non-ansi zero stringlength
   !                  -incorrect size calculation for skew-symmetric arrays
   !             Other changes in mmio.f:
   !                  -added integer value parameter to calling sequences  
   !                  -enforced proper count in size info line
   !                  -added routine to count words in string (countwd)
   !            (Thanks to G.P.Leendetse and H.Oudshoom for their review
   !             of the initial version and suggested fixes.)
   !------------------------------------------------------------------------      
   !   Arguments:
   !   name     type      in/out description
   !   ---------------------------------------------------------------
   !   iunit  integer     in   Unit identifier for the open file
   !                             containing the data to be read.
   !   rep     character*10 out  Matrix Market 'representation' 
   !                             indicator. On return:
   !                                coordinate   (for sparse data)
   !                                array        (for dense data)
   !                                elemental    (to be added)    
   !   field   character*7  out  Matrix Market 'field'. On return:
   !                                real 
   !                                complex
   !                                integer
   !                                pattern
   !   symm    character*19 out  Matrix Market 'field'. On return:
   !                                symmetric
   !                                hermitian
   !                                skew-symmetric
   !                                general          
   !   rows     integer     out  Number of rows in matrix.
   !   cols     integer     out  Number of columns in matrix.
   !   nnz      integer     out  Number of nonzero entries required to store 
   !                             the matrix.
   !------------------------------------------------------------------------ 
   subroutine mminfo(iunit,rep,field,symm,rows,cols,nnz)

      implicit none
      !
      ! Declarations:
      !
      integer            :: rows, cols, nnz, iunit,slen,start
      integer            :: icount,next
      character(len=14)  :: mmhead
      character(len=6)   :: mmtype
      character(len=10)  :: rep
      character(len=7)   :: field
      character(len=19)  :: symm
      character(len=1024):: tmp1
      character(len=2)   :: tmp2
      !
      ! Read header line and check validity:
      !
      read (iunit,end=1000,fmt=5) tmp1
 5    format(1024A)
      !
      ! Parse words from header line:
      !
      slen=1024
      start=1
      call getwd(mmhead,tmp1,slen,start,next,icount)
      if ( icount .eq. 0 ) go to 5000
      call getwd(mmtype,tmp1,slen,next,next,icount)
      if ( icount .eq. 0 ) go to 5000
      call getwd(rep,tmp1,slen,next,next,icount)
      if ( icount .eq. 0 ) go to 5000
      call getwd(field,tmp1,slen,next,next,icount)
      if ( icount .eq. 0 ) go to 5000
      call getwd(symm,tmp1,slen,next,next,icount)
      if ( icount .eq. 0 ) go to 5000
      if ( mmhead .ne. '%%MatrixMarket' ) go to 5000
      !
      ! Convert type code to upper case for easier comparisons:
      !
      call lowerc(mmtype,1,6)
      if ( mmtype .ne. 'matrix' ) then
         print *,'Invalid matrix type: ',mmtype
         print *,'This reader only understands type ''matrix''.'
         stop
      else
         start=1
         slen=10
         call lowerc(rep,start,slen)
         slen=7
         call lowerc(field,start,slen)
         slen=19
         call lowerc(symm,start,slen)
      endif
      !
      ! Test input qualifiers:
      !
      if (rep .ne. 'coordinate' .and. rep .ne. 'array' )            go to 6000
      if (rep .eq. 'coordinate' .and. field .ne. 'integer' .and. &
          field .ne. 'real' .and. field .ne. 'complex' .and.     &
          field .ne. 'pattern')                                     go to 7000
      if (rep .eq. 'array' .and. field .ne. 'integer' .and.      &
          field .ne. 'real' .and. field .ne. 'complex' )            go to 8000
      if (symm .ne. 'general' .and. symm .ne. 'symmetric' .and.  &
          symm .ne. 'hermitian' .and. symm .ne. 'skew-symmetric')   go to 9000
      !
      ! Read through comment lines, ignoring content:
      !
      read (iunit,end=2000,fmt=200) tmp2
 200  format(1a)
 10   continue
         if ( tmp2(1:1) .ne. '%' ) then
            go to 20
         endif
         read (iunit,end=2000,fmt=200) tmp2
      go to 10
 20   continue
      !
      ! Just read a non-comment.
      !   Now, back up a line, and read for first int, and back up
      !   again. This will set pointer to just before apparent size
      !   info line.
      !   Before continuing with free form input, count the number of
      !   words on the size info line to ensure there is the right amount
      !   of info (2 words for array matrices, 3 for coordinate matrices).
      !
      backspace (iunit)
      read (iunit,end=1000,fmt=5) tmp1
      start=1
      call countwd(tmp1,slen,start,icount)
      if ( rep .eq. 'array' .and. icount .ne. 2 )      go to 3000
      if ( rep .eq. 'coordinate' .and. icount .ne. 3 ) go to 3500
      !
      !   Correct number of words are present, now back up and read them.
      !
      backspace (iunit)
 
      if ( rep .eq. 'coordinate' ) then 
         !
         ! Read matrix in sparse coordinate format
         !
         read (iunit,fmt=*) rows,cols,nnz
         !
         ! Rewind before returning 
         !
         rewind(iunit)
         return
 
      elseif ( rep .eq. 'array' ) then
         !
         ! Read matrix in dense column-oriented array format
         !
         read (iunit,fmt=*) rows,cols
         if ( symm .eq. 'symmetric' .or. symm .eq. 'hermitian' ) then
            nnz = (rows*cols - rows)/2 + rows
         elseif ( symm .eq. 'skew-symmetric' ) then
            nnz = (rows*cols - rows)/2 
         else
            nnz = rows*cols
         endif
         !
         ! Rewind before returning 
         !
         rewind(iunit)
         return
      else
         print *,'''',rep,''' representation not recognized.'
         print *, 'Recognized representations:'
         print *, '   array'
         print *, '   coordinate'
         stop
      endif
      !
      ! Various error conditions:
      !
 1000 print *,'Premature end-of-file.'
      print *,'No lines found.'
      stop
 2000 print *,'Premature end-of-file.'
      print *,'No data found.'
      stop
 3000 print *,'Size info inconsistant with representation.'
      print *,'Array matrices need exactly 2 size descriptors.'
      print *, icount,' were found.'
      stop
 3500 print *,'Size info inconsistant with representation.'
      print *,'Coordinate matrices need exactly 3 size descriptors.'
      print *, icount,' were found.'
      stop
 5000 print *,'Invalid matrix header: ',tmp1
      print *,'Correct header format:'
      print *,'%%MatrixMarket type representation field symmetry'
      print *
      print *,'Check specification and try again.'
      stop
 6000 print *,'''',rep,''' representation not recognized.'
      print *, 'Recognized representations:'
      print *, '   array'
      print *, '   coordinate'
      stop
 7000 print *,'''',field,''' field is not recognized.'
      print *, 'Recognized fields:'
      print *, '   real'
      print *, '   complex'
      print *, '   integer'
      print *, '   pattern'
      stop
 8000 print *,'''',field,''' arrays are not recognized.'
      print *, 'Recognized fields:'
      print *, '   real'
      print *, '   complex'
      print *, '   integer'
      stop
 9000 print *,'''',symm,''' symmetry is not recognized.'
      print *, 'Recognized symmetries:'
      print *, '   general'
      print *, '   symmetric'
      print *, '   hermitian'
      print *, '   skew-symmetric'
      stop

   end subroutine mminfo
   !------------------------------------------------------------------------ 
   !------------------------------------------------------------------------ 

   !------------------------------------------------------------------------      
   ! This routine will write data to a matrix market formatted file.
   ! The data may be either sparse coordinate format, or dense array format.
   !
   ! The unit ounit must be open.
   !
   ! 20-Sept-96  Karin A. Remington, NIST ACMD (karin@cam.nist.gov)
   ! 18-Oct-96   Change in routine name to match C and Matlab routines.
   ! 30-Oct-96   Bug fixes in mmio.f:
   !                  -looping for comment lines
   !                  -fixed non-ansi zero stringlength
   !                  -incorrect size calculation for skew-symmetric arrays
   !             Other changes in mmio.f:
   !                  -added integer value parameter to calling sequences  
   !                  -enforced proper count in size info line
   !                  -added routine to count words in string (countwd)
   !            (Thanks to G.P.Leendetse and H.Oudshoom for their review
   !             of the initial version and suggested fixes.)
   !------------------------------------------------------------------------      
   !   Arguments:
   !   name     type      in/out description
   !   ---------------------------------------------------------------
   !   ounit  integer     in   Unit identifier for the file
   !                             to which the data will be written.
   !                             Must be open prior to call.
   !   rep     character*   in   Matrix Market 'representation' 
   !                             indicator. Valid inputs:
   !                                coordinate   (for sparse data)
   !                                array        (for dense data)
   !                               *elemental*    (to be added)    
   !   field   character*   in   Matrix Market 'field'. Valid inputs:
   !                                real 
   !                                complex
   !                                integer
   !                                pattern (not valid for dense arrays)
   !   symm    character*   in   Matrix Market 'field'. Valid inputs:
   !                                symmetric
   !                                hermitian
   !                                skew-symmetric
   !                                general          
   !   rows     integer     in   Number of rows in matrix.
   !   cols     integer     in   Number of columns in matrix.
   !   nnz      integer     in   Number of nonzero entries in matrix.
   !                             (rows*cols for array matrices)
   !   indx     integer(nnz)in   Row indices for coordinate format.
   !                             Undefined for array format.
   !   jndx     integer(nnz)in   Column indices for coordinate format.
   !                             Undefined for array format.
   !   ival     integer(nnz) in  Integer data (if applicable, see 'field')
   !   rval     double(nnz) in   Real data (if applicable, see 'field')
   !   cval     complex(nnz)in   Complex data (if applicable, see 'field')
   !------------------------------------------------------------------------
   subroutine mmwrite(ounit,rep,field,symm,rows,cols,nnz, &
                      indx,jndx,ival,rval,cval)

      implicit none 
      !
      ! Declarations:
      !
      integer          :: ival(:)
      double precision :: rval(:)
      complex          :: cval(:)
      integer          :: indx(:)
      integer          :: jndx(:)
      integer          :: i, rows, cols, nnz, nnzreq, ounit
      character(*)     :: rep,field,symm
      !
      ! Test input qualifiers:
      !
      if (rep .ne. 'coordinate' .and. rep .ne. 'array' )            go to 1000
      if (rep .eq. 'coordinate' .and. field .ne. 'integer' .and. &
          field .ne. 'real' .and. field .ne. 'complex' .and.     &
          field .ne. 'pattern')                                     go to 2000
      if (rep .eq. 'array' .and. field .ne. 'integer' .and.      &
          field .ne. 'real' .and. field .ne. 'complex' )            go to 3000
      if (symm .ne. 'general' .and. symm .ne. 'symmetric' .and.  &
          symm .ne. 'hermitian' .and. symm .ne. 'skew-symmetric')   go to 4000
      !
      ! Write header line:
      !
      write(unit=ounit,fmt=5)rep,' ',field,' ',symm
 5    format('%%MatrixMarket matrix ',11A,1A,8A,1A,20A)
      !
      ! Write size information:
      !
      if ( rep .eq. 'coordinate' ) then
         nnzreq=nnz
         write(unit=ounit,fmt=*) rows,cols,nnz
         if ( field .eq. 'integer' ) then
            do i=1,nnzreq
               write(unit=ounit,fmt=*)indx(i),jndx(i),ival(i)
            enddo
         elseif ( field .eq. 'real' ) then
            do i=1,nnzreq
               write(unit=ounit,fmt=*)indx(i),jndx(i),rval(i)
            enddo
         elseif ( field .eq. 'complex' ) then
            do i=1,nnzreq
               write(unit=ounit,fmt=*)indx(i),jndx(i),real(cval(i)),aimag(cval(i))
            enddo
         else !field .eq. 'pattern' 
            do i=1,nnzreq
               write(unit=ounit,fmt=*)indx(i),jndx(i)
            enddo
         endif
      else ! rep .eq. 'array'
         if ( symm .eq. 'general' ) then
            nnzreq = rows*cols
         elseif ( symm .eq. 'symmetric' .or. symm .eq. 'hermitian' ) then
            nnzreq = (rows*cols - rows)/2 + rows
         else ! symm .eq. 'skew-symmetric' 
            nnzreq = (rows*cols - rows)/2 
         endif
         write(unit=ounit,fmt=*)rows,cols
         if ( field .eq. 'integer' ) then
            do i=1,nnzreq
               write(unit=ounit,fmt=*)ival(i)
            enddo
         elseif ( field .eq. 'real' ) then
            do i=1,nnzreq
               write(unit=ounit,fmt=*)rval(i)
            enddo
         else ! field .eq. 'complex' 
            do i=1,nnzreq
               write(unit=ounit,fmt=*)real(cval(i)),aimag(cval(i))
            enddo
         endif
      endif
      return
      !
      ! Various errors
      !
 1000 print *,'''',rep,''' representation not recognized.'
      print *, 'Recognized representations:'
      print *, '   array'
      print *, '   coordinate'
      stop
 2000 print *,'''',field,''' field is not recognized.'
      print *, 'Recognized fields:'
      print *, '   real'
      print *, '   complex'
      print *, '   integer'
      print *, '   pattern'
      stop
 3000 print *,'''',field,''' arrays are not recognized.'
      print *, 'Recognized fields:'
      print *, '   real'
      print *, '   complex'
      print *, '   integer'
      stop
 4000 print *,'''',symm,''' symmetry is not recognized.'
      print *, 'Recognized symmetries:'
      print *, '   general'
      print *, '   symmetric'
      print *, '   hermitian'
      print *, '   skew-symmetric'
      stop

   end subroutine mmwrite
   !-------------------------------------------------------------------
   !-------------------------------------------------------------------


   !-------------------------------------------------------------------
   !-------------------------------------------------------------------
   ! PRIVATE METHODS
   !-------------------------------------------------------------------
   !-------------------------------------------------------------------

   !------------------------------------------------------------------------ 
   ! Convert uppercase letters to lowercase letters in string with
   ! starting postion pos and length len.
   !------------------------------------------------------------------------
   subroutine lowerc(string,pos,ilen)
     
      implicit none 
      integer      :: pos, ilen, i, k
      character(*) :: string

      character(len=26) :: lcase, ucase
      save lcase,ucase
      data lcase/'abcdefghijklmnopqrstuvwxyz'/
      data ucase/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/

      do i=pos,ilen
         k = index(ucase,string(i:i))
         if (k.ne.0) string(i:i) = lcase(k:k)
      enddo
      return
   end subroutine lowerc

   !------------------------------------------------------------------------ 
   !     Getwd extracts the first  word from string starting
   !     at position start.  On return, next is the position
   !     of the blank which terminates the word in string.   
   !     If the found word is longer than the allocated space
   !     for the word in the calling program, the word will be 
   !     truncated to fit.
   !     Count is set to the length of the word found.
   ! 30-Oct-96   Bug fix: fixed non-ansi zero stringlength
   !------------------------------------------------------------------------
   subroutine getwd(word,string,slen,start,next,wlen)
   
      implicit none   
      integer      :: slen, start, next, begin, space, wlen 
      integer      :: i
      character(*) :: word
      character(*) :: string

      begin = start
      !next  = 1
      do i=start,slen
         space = index(string(i:slen),' ')
         if ( space .gt. 1) then
            next = i+space-1
            exit
         endif
         begin=begin+1
      enddo

      wlen=next-begin
      if ( wlen .le. 0 ) then
         wlen = 0
         word = ' '
         return
      endif
      word=string(begin:begin+wlen)
      return

   end subroutine getwd

   !------------------------------------------------------------------------ 
   !     Getwd extracts the first  word from string starting
   !     at position start.  On return, next is the position
   !     of the blank which terminates the word in string.   
   !     If the found word is longer than the allocated space
   !     for the word in the calling program, the word will be 
   !     truncated to fit.
   !     Count is set to the length of the word found.
   !     
   ! 30-Oct-96   Bug fix: fixed non-ansi zero stringlength
   !------------------------------------------------------------------------
   subroutine countwd(string,slen,start,icount)
  
      implicit none   
      character(*)     :: string
      integer          :: slen, start, next, wordlength, icount
      character(len=2) :: tmp2

      icount = 0
      next = 1
 10   call getwd(tmp2,string,slen,start,next,wordlength)
      if ( wordlength .gt. 0 ) then
         icount = icount + 1
         start=next
         go to 10
      endif
      return
   end subroutine countwd

end module
