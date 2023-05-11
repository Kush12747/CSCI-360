//XXXXXXXXX JOB ,'K. GANDHI',MSGCLASS=H
//JSTEP01  EXEC PGM=ASSIST
//STEPLIB  DD DSN=KC00NIU.ASSIST.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
*****************************************************************
*                                                               *
*  CSCI 360-PE1           ASSIGNMENT 4           SPRING 2023    *
*                                                               *
*  DEVELOPER NAME: KUSH GANDHI                                  *
*                                                               *
*        DUE DATE: 02/24/2023                                   *
*                                                               *
*****************************************************************
*
DUMPEX   CSECT
         USING DUMPEX,15    ESTABLISH REG 15 AS BASE REG
*
         SR    7,7          CLEAR REG 7
*
         LA    8,NUM1       LOAD ADDRESS OF NUM1
         LA    9,NUM2       LOAD ADDRESS OF NUM2
*
         A     7,0(,8)      ADD NUM1 TO REG 7
         A     7,0(,9)      ADD NUM2 TO REG 7
*
         LA    10,SUMMED    LOAD ADDRESS OF SUMMED
         ST    7,0(,10)     STORE THE SUM OF VARIABLES
*
         XDUMP ,            DUMP REGS
*
         LTORG
*
SUMMED   DS    F            FULLWORD OF ZERO TO HOLD SUMMED INTEGERS
*
NUM1     DC    F'972460'      RANDOM INTEGER 1
NUM2     DC    F'1206000000'  RANDOM INTEGER 2
NUM3     DC    F'1344335922'  RANDOM INTEGER 3
*
         END   DUMPEX
/*
//
