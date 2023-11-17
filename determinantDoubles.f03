include "mqc_binary.F03"
!
!     This is a simple program to demonstrate the use of the in-prep MQC_Binary
!     module to build a set of singles substituted determinants from a reference
!     defined by the user on the command line.
!
!     The program take two command line arguments: (1) number of
!     electrons/occupied orbitals; and (2) total number of orbitals.
!
!     H. P. Hratchian, 2023.
!
!     This program further builds a double substituted determinant list from a
!     reference single substituded determinant array.
!
!     A. J. Bovill, 2023.
!
!
      program determinantDoubles
      USE MQC_General
      USE MQC_Binary
      implicit none
      integer,parameter::iOut=6
      integer::i,ii,ia,nOcc,nMOs,iDetRef,nVirt,nOV
      integer::jj,ja,j
      integer,dimension(:),allocatable::iDetSingles,iDetDoubles
!
!     Format statements.
!
 1000 format(1x,'nOcc=',i3,3x,'nMOs=',i4)
 2000 format(1x,a,': ',b31)
 2010 format(1x,a,': ',I3)
 2100 format(1x,a,1x,i5,': ',b31)
!     Andrew -- added extra format statement to see the integer to the reference
!     binary state
 2200 format(1x,a,1x,i5,': ',I4)
!
!
!     Read the number of electrons and number of orbitals from the command line.
!
      call mqc_get_command_argument_integer(1,nOcc)
      call mqc_get_command_argument_integer(2,nMOs)
      write(iOut,1000) nOcc,nMOs
!
!     This version of the program uses intrinsic integers to store the
!     determinant bit strings. So, for now, we limit the number of MOs to 31.
!
      if(nMOs.gt.31) call mqc_error('More than 31 MOs requested. NYI.')
!
!     Set the reference determinant string in iDetRef.
!
      iDetRef = 0
      do i = 0,nOcc-1
        iDetRef = IBSet(iDetRef,i)
      endDo
      write(iOut,2000) 'Reference',iDetRef
!
!     Compute the number of singles substituted determinants and then build them
!     all in the array iDetSingles.
!
      nVirt = nMOs-nOcc
      nOV = nOcc*nVirt
      Allocate(iDetSingles(nOV))
      i = 0
      do ii = 0,nOcc-1
        write(*,*) "Andrew check ii", ii
        do ia = nOcc,nMOs-1
          write(*,*) "Andrew check ia", ia
          i = i + 1
          iDetSingles(i) = IBClr(iDetRef,ii)
          iDetSingles(i) = IBSet(iDetSingles(i),ia)
          write(iOut,2100) 'Singles',i,iDetSingles(i)
          write(iOut,2200) 'Singles',i,iDetSingles(i)
        endDo
      endDo
      write(*,*) SIZE((iDetDoubles))    
!
!     Compute the number of doubles substituted determinants and then build them
!     all in the array iDetSingles.
!     To get the accurate size of the array for iDetDoubles, you need the
!     following logic: factorial(i)/i i.e. if you have only say 3 virtuals, then
!     you only have two unique possible substituted determinants on the virtual
!     size, likewise say if you have only two mo's you can only have 1 possible
!     substituded determinant on the right hand side
!     Therefore the number of 

      if(nVirt.lt.2.or.nOcc.lt.2) then
        call mqc_error('not enough occupied or virtual orbitals to produce double &
          substituted determinants')
      end if

      nOV = (factorial(nOcc)/nOcc)*(factorial(nVirt)/nVirt)
      Allocate(iDetDoubles(nOV))

      i = 0
      do ii = 0,nOcc
        do ia = ii+1,nOcc
            do jj = 0,nVirt
              do ja = jj+1,nVirt
                i = i + 1
                if(i.gt.nOV) then
                  stop
                end if 
                iDetDoubles(i) = IBClr(iDetRef,ii)
                iDetDoubles(i) = IBClr(iDetDoubles(i),ia)
                iDetDoubles(i) = IBSet(iDetDoubles(i),jj+nOcc)
                iDetDoubles(i) = IBSet(iDetDoubles(i),ja+nOcc)
                write(iOut,2100) 'Doubles',i,iDetDoubles(i)
                write(iOut,2200) 'Doubles',i,iDetDoubles(i)
              end do
            end do
        endDo
      endDo

      write(*,*) "Andrew factorial of 3 is : ", factorial(3)
      write(*,*) "Andrew factorial of 10 is : ", factorial(10)
      write(*,*) "Andrew Size of doubles determinants : ", size(iDetDoubles)

!
!     That's the end of the program...
!
      write(*,*)' All Done.'
      end program determinantDoubles

