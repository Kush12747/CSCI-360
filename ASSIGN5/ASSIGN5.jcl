//XXXXXXXXX JOB ,'K. GANDHI',MSGCLASS=H
//JSTEP01  EXEC PGM=ASSIST
//STEPLIB  DD DSN=KC00NIU.ASSIST.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
*****************************************************************
*                                                               *
*  CSCI 360-PE1            ASSIGNMENT 5              SPRING 23  *
*                     DISASSEMBLY & DECODING                    *
*                                                               *
*  DEVELOPER NAME: KUSH GANDHI                                  *
*        DUE DATE: 3/3/23                                       *
*                                                               *
*  PROGRAM NAME: DISASSEMBLY & DECODING                         *
*                                                               *
*  FUNCTION: USE THE GIVEN XDUMP CONTENTS AND DECODE THEM TO    *
*            MAKE A PROGRAM.                                    *
*****************************************************************
*
ASSIGN5  CSECT
         USING ASSIGN5,15   ESTABLISH ADDRESSABILITY ON REG 15
*
         LA    4,23         LOADS THE ADDRESS IN REG 4
         L     5,60(,15)    COPIES THE ABSOLUTE ADDRESS
         LR    2,4          OVERWRITES THE CONTENTS FROM REG 2
         ST    2,64(,15)    STORES THE CONTENTS
*
         LR    7,5          OVERWRITES CONTENTS FROM REG 7
         MR    6,5          MULTIPLY REGS
         M     4,80(,15)    MULTIPLY 2 32 BIT NUMBERS
*
         DR    4,9          DIVIDES THE REGESTIER
         XDUMP ASSIGN5,84   DUMP CONTENTS
         BCR   B'1111',14   UNCONDITIONAL RETURN TO CALLER (OS)
*
         DC    F'124'       STORAGE DECLARATIONS
         DC    F'2323'
         DC    F'9'
         DC    F'4'
         DC    F'1'
         LTORG
         END   ASSIGN5
/*
//
