include "mqc_binary.F03"
!
!     This is a simple program used during the preparation of the MQC_Binary
!     module. It can serve as a basic test program and as an example code using
!     the MQC_Binary module.
!
!     H. P. Hratchian, 2021.
!
!
      program bit
      USE MQC_General
      USE MQC_Binary
      implicit none
      type(mqc_bits)::bitTest1,bitTest2,bitTest3
      integer(kind=int64),parameter::iOut=6
      integer(kind=int64)::iInteger,iBitnum
      character(len=256)::formatText,charTemp
!
!     Format Statements
!
 2000 format(1x,'Object ',A,' bit number ',A,' equals ',L1,'.')
!
!     Begin by initializing a couple MQC binary objects.
!
      bitTest1 = mqc_bits_initialize(8_int64)
      bitTest2 = mqc_bits_initialize(8_int64)
      bitTest3 = mqc_bits_initialize(8_int64)
      call MQC_Bits_Print(bitTest1)
!
!     Turn on a few bits in bitTest1, write it out, make some changes, write it
!     out again, and try out BTest a few times.
!
      call MQC_IBitSet(bitTest1,0_int64)
      call MQC_IBitSet(bitTest1,2_int64)
      call MQC_IBitSet(bitTest1,5_int64)
      call MQC_Bits_Print(bitTest1,header='bitTest1 with 0,2,5 turned ON:_')
      call MQC_Bits_Print(bitTest1,verbose=.true.)
!
      write(iOut,*)
      write(iOut,2000) 'bitTest1','0',mqc_BTest(bitTest1,0)
      write(iOut,2000) 'bitTest1','1',mqc_BTest(bitTest1,1)
      write(iOut,2000) 'bitTest1','2',mqc_BTest(bitTest1,2)
      write(iOut,2000) 'bitTest1','3',mqc_BTest(bitTest1,3)
      write(iOut,2000) 'bitTest1','4',mqc_BTest(bitTest1,4)
      write(iOut,2000) 'bitTest1','5',mqc_BTest(bitTest1,5)
      write(iOut,2000) 'bitTest1','6',mqc_BTest(bitTest1,6)
      write(iOut,2000) 'bitTest1','7',mqc_BTest(bitTest1,7)
!
!     Turn on a few bits in bitTest2, write them out, make some changes, write it
!     out again, and try out BTest a few times.
!
      call MQC_IBitSet(bitTest2,0_int64)
      call MQC_IBitSet(bitTest2,5_int64)
      call MQC_IBitSet(bitTest2,7_int64)
      call MQC_Bits_Print(bitTest2,header='bitTest2 with 0,5,7 turned ON:_')
!
!     Now, try IAnd, IEOR, and IOR.
!
      write(iOut,*)
      write(iOut,*)' Trying out IAnd...'
      call MQC_Bits_Print(bitTest1,header='bitTest1              = ')
      call MQC_Bits_Print(bitTest2,header='bitTest2              = ')
      call MQC_Bits_Print(MQC_IAnd(bitTest1,bitTest2),header='bitTest1.AND.bitTest2 = ')
      write(iOut,*)
      write(iOut,*)' Trying out IEOR...'
      call MQC_Bits_Print(bitTest1,header='bitTest1              = ')
      call MQC_Bits_Print(bitTest2,header='bitTest2              = ')
      call MQC_Bits_Print(MQC_IEOR(bitTest1,bitTest2),header='bitTest1.XOR.bitTest2 = ')
      write(iOut,*)
      write(iOut,*)' Trying out IOR...'
      call MQC_Bits_Print(bitTest1,header='bitTest1              = ')
      call MQC_Bits_Print(bitTest2,header='bitTest2              = ')
      call MQC_Bits_Print(MQC_IOR(bitTest1,bitTest2),header='bitTest1.OR.bitTest2  = ')
  999 write(*,*)
      write(*,*)' All Done.'
      end program bit
