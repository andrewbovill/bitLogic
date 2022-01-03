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
      type(mqc_bits)::bitTest1
      integer(kind=int64)::iInteger,iBitnum
      character(len=256)::formatText,charTemp
!
      bitTest1 = mqc_bits_initialize(31_int64)
      write(*,*)' bitTest1 = ',bitTest1
      call MQC_Bits_Write(bitTest1)

      call MQC_BitPosition(bitTest1,0_int64,iInteger,iBitnum)
      call MQC_BitPosition(bitTest1,1_int64,iInteger,iBitnum)
      call MQC_BitPosition(bitTest1,2_int64,iInteger,iBitnum)
      call MQC_BitPosition(bitTest1,3_int64,iInteger,iBitnum)
      call MQC_BitPosition(bitTest1,4_int64,iInteger,iBitnum)
      call MQC_BitPosition(bitTest1,5_int64,iInteger,iBitnum)
      call MQC_BitPosition(bitTest1,6_int64,iInteger,iBitnum)
      call MQC_BitPosition(bitTest1,7_int64,iInteger,iBitnum)
      call MQC_BitPosition(bitTest1,8_int64,iInteger,iBitnum)
      call MQC_BitPosition(bitTest1,9_int64,iInteger,iBitnum)

      call MQC_IBitSet(bitTest1,0_int64)
      call MQC_IBitSet(bitTest1,5_int64)
      call MQC_Bits_Write(bitTest1)
!
      formatText = '(1x,''hello Hrant, this is '',A)'
      
      charTemp = num2char(bitTest1%nBitsPerInteger)
      formatText = '(1x,I2,'':'',B'//TRIM(charTemp)//'.'//TRIM(charTemp)//')'

      write(*,*)
      write(*,*)' Hrant - formatText ==>',TRIM(formatText),'<=='
      write(*,*)



!
  999 write(*,*)
      write(*,*)' All Done.'
      end program bit
