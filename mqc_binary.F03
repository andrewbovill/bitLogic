      module MQC_Binary



      USE MQC_General
      USE iso_fortran_env, only: int64, real64
!                                                                    
!----------------------------------------------------------------    
!                                                               |    
!     TYPE AND CLASS DEFINITIONS                                |    
!                                                               |    
!----------------------------------------------------------------    
      implicit none
!
!     mqc_bits
      type :: mqc_bits
        integer(kind=int64)::nBitsPerInteger=2
        integer(kind=int64)::nBits,nIntegers
        integer(kind=int64),dimension(:),allocatable::integerList
      end type mqc_bits
!
!
!----------------------------------------------------------------
!                                                               |
!     PROCEDURE INTERFACES                                      |
!                                                               |
!----------------------------------------------------------------
!
!
!     Interface MQC_Bits to the custom constructor.
      interface mqc_bits
        module procedure MQC_Bits_Initialize
      end interface mqc_bits
!
!
!----------------------------------------------------------------
!                                                               |
!     SUBROUTINES AND FUNCTIONS                                 |
!                                                               |
!----------------------------------------------------------------
!
!
      CONTAINS
!
!PROCEDURE MQC_Bits_Initialize
      function MQC_Bits_Initialize(nBits) result(mqc_bits_out)
!
!     This function is used to initialize an MQC_Bit object. Input dummy
!     argument <nBits> is the number of bits requested by the user. Keep in mind
!     that bit 0 counts as a bit in this count.
!
!     H. P. Hratchian, 2021.
!
!
      implicit none
      type(mqc_bits),intent(out)::mqc_bits_out
      integer(kind=int64),intent(in),optional::nBits
!
      integer(kind=int64)::my_nBits,nIntegers
!
!
!     Start by setting up my_nBits.
!
      if(PRESENT(nBits)) then
        my_nBits = nBits
      else
        my_nBits = 5
      endIf
!
!     Set nIntegers.
!
      nIntegers = my_nBits/mqc_bits_out%nBitsPerInteger
      if(MOD(my_nBits,mqc_bits_out%nBitsPerInteger).ne.0)  &
        nIntegers = nIntegers+1
!
!     Set mqc_bits_out%nBits. Then allocate the integer array in the bit object
!     and set all the values to 0.
!
      mqc_bits_out%nBits = my_nBits
      mqc_bits_out%nIntegers = nIntegers
      Allocate(mqc_bits_out%integerList(nIntegers))
      mqc_bits_out%integerList = 0

      write(*,*)
      write(*,*)' Init: nBits     = ',nBits
      write(*,*)'       nIntegers = ',nIntegers
      write(*,*)'       bits/int  = ',mqc_bits_out%nBitsPerInteger
      write(*,*)

!
      return
      end function MQC_Bits_Initialize
!
!
!PROCEDURE MQC_Bits_Write
      subroutine MQC_Bits_Write(mqc_bits_in,iUnit)
!
!     This subroutine is used to print out the bits in mqc_bits_in.
!
!     H. P. Hratchian, 2021.
!
!
      implicit none
      type(mqc_bits),intent(in)::mqc_bits_in
      integer(kind=int64),intent(in),optional::iUnit
!
      integer(kind=int64)::my_iUnit,i
      character(len=256)::formatText,charTemp
!
!
!     Format Statements
!
 1000 format(1x,i2,': ',B5.5)
!
!
!     Start by setting up my_iUnit.
!
      if(PRESENT(iUnit)) then
        my_iUnit = iUnit
      else
        my_iUnit = 6
      endIf
!
!     Set up the format string.
!
      charTemp = num2char(mqc_bits_in%nBitsPerInteger)
      formatText = '(1x,I2,'':'',B'//TRIM(charTemp)//'.'//TRIM(charTemp)//')'
!
!     Loop over integers in the bit list and write them out in binary form.
!
      do i = 1,mqc_bits_in%nIntegers
        write(UNIT=my_iUnit,FMT=formatText) i,mqc_bits_in%integerList(i)
      endDo
!
      return
      end subroutine MQC_Bits_Write
!
!
!PROCEDURE MQC_BitPosition
      subroutine MQC_BitPosition(mqc_bits_in,bitnum,iInteger,iBitnum)
!
!     This subroutine is used to determine which integer <iInteger> and which
!     bit <iBitnum> within that integer in an MQC_Bits object correspond the
!     overall bit <bitnum>.
!
!     H. P. Hratchian, 2021.
!
!
      implicit none
      type(mqc_bits),intent(in)::mqc_bits_in
      integer(kind=int64),intent(in)::bitnum
      integer(kind=int64),intent(out)::iInteger,iBitnum
!
!
!     Do the work.
!
      iInteger = (bitnum+1)/mqc_bits_in%nBitsPerInteger
      if(MOD((bitnum+1),mqc_bits_in%nBitsPerInteger).ne.0)  &
        iInteger = iInteger+1
      iBitnum = MOD(bitnum,mqc_bits_in%nBitsPerInteger)
!
      return
      end subroutine MQC_BitPosition
!
!
!PROCEDURE MQC_BTest
      function MQC_BTest(mqc_bits_in,bitnum) result(bitValue)
!
!     This function tests the switch in bit number <bitNum> in <mqc_bits_in> and
!     returns TRUE or FALSE accordingly.
!
!     H. P. Hratchian, 2022.
!
!
      implicit none
      type(mqc_bits),intent(inOut)::mqc_bits_in
      integer(kind=int64),intent(in)::bitNum
      logical,intent(out)::bitValue
!
      integer(kind=int64)::iInteger,iBitNum
!
!
!     Do the work.
!
      call MQC_BitPosition(mqc_bits_in,bitNum,iInteger,iBitNum)
      bitValue = BTest(mqc_bits_in%integerList(iInteger),iBitnum)
!
      return
      end function MQC_BTest
!
!
!PROCEDURE MQC_IBitSet
      subroutine MQC_IBitSet(mqc_bits_in,bitnum)
!
!     This subroutine is used to set a bit in mqc_bits_in.
!
!     H. P. Hratchian, 2021.
!
!
      implicit none
      type(mqc_bits),intent(inOut)::mqc_bits_in
      integer(kind=int64),intent(in)::bitnum
!
      integer(kind=int64)::iInteger,iBitnum
!
!
!     Do the work.
!
      call MQC_BitPosition(mqc_bits_in,bitnum,iInteger,iBitnum)
      mqc_bits_in%integerList(iInteger) = IBSet(mqc_bits_in%integerList(iInteger),iBitnum)
!
      return
      end subroutine MQC_IBitSet
!
!
!PROCEDURE MQC_IBitClr
      subroutine MQC_IBitClr(mqc_bits_in,bitnum)
!
!     This subroutine is used to clear a bit in mqc_bits_in.
!
!     H. P. Hratchian, 2022.
!
!
      implicit none
      type(mqc_bits),intent(inOut)::mqc_bits_in
      integer(kind=int64),intent(in)::bitnum
!
      integer(kind=int64)::iInteger,iBitnum
!
!
!     Do the work.
!
      call MQC_BitPosition(mqc_bits_in,bitnum,iInteger,iBitnum)
      mqc_bits_in%integerList(iInteger) = IBClr(mqc_bits_in%integerList(iInteger),iBitnum)
!
      return
      end subroutine MQC_IBitClr
!hph+
!!
!!
!!PROCEDURE MQC_IBitAnd
!      subroutine MQC_IBitAnd(mqc_bits_in1,mqc_bits_in2,mqc_bits_out)
!!
!!     This subroutine is used to do AND operation on two MQC_Bits objects,
!!     mqc_bits_in1 and mqc_bits_in2.
!!
!!     H. P. Hratchian, 2022.
!!
!!
!      implicit none
!      type(mqc_bits),intent(in)::mqc_bits_in1,mqc_bits_in2
!      type(mqc_bits),intent(out)::mqc_bits_out
!!
!      integer(kind=int64)::i
!!
!!
!!     Do the work.
!!
!      if((mqc_bits_in1%nIntegers.eq.mqc_bits_in2%nIntegers).and.
!        (mqc_bits_in1%nBits.eq.mqc_bits_in2%nBits)) then
!        mqc_bits_out = MQC_Bits_Initialize(mqc_bits_in1%nBits)
!        do i = 1,mqc_bits_in1%nIntegers
!
!
!
!
!
!        endDo
!      else
!        call mqc_error('MQC_BitAnd: Non-conformable bit objects.')
!      endIf
!!
!      return
!      end subroutine MQC_IBitAnd
!hph-

      end module MQC_Binary
