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
!
      program determinantSingles
      USE MQC_General
      USE MQC_Binary
      implicit none
      integer,parameter::iOut=6
      integer::i,ii,ia,nOcc,nMOs,iDetRef,nVirt,nOV
      integer,dimension(:),allocatable::iDetSingles
!
!     Format statements.
!
 1000 format(1x,'nOcc=',i3,3x,'nMOs=',i4)
 2000 format(1x,a,': ',b31)
 2100 format(1x,a,1x,i5,': ',b31)
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
        do ia = nOcc,nMOs-1
          i = i + 1
          iDetSingles(i) = IBClr(iDetRef,ii)
          iDetSingles(i) = IBSet(iDetSingles(i),ia)
          write(iOut,2100) 'Singles',i,iDetSingles(i)
        endDo
      endDo
!
!     That's the end of the program...
!
      write(*,*)' All Done.'
      end program determinantSingles
