//KC03E3BA JOB ,'K. GANDHI',MSGCLASS=H
//JSTEP01  EXEC PGM=ASSIST
//STEPLIB  DD DSN=KC00NIU.ASSIST.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
*****************************************************************
*                                                               *
*  CSCI 360-PE1           ASSIGNMENT 3           SPRING 2023    *
*                                                               *
*  DEVELOPER NAME: KUSH GANDHI                                  *
*                                                               *
*        DUE DATE: 02/17/2023                                   *
*                                                               *
*    PROGRAM NAME: BASIC ASSEMBLY PROGRAM                       *
*                                                               *
*        FUNCTION: ADDING REGISTERS 2 AND 3 USING IMPLICIT      *
*                  ADDRESSING. THEN USE XDUMP TO DUMP REGISTERS.*
*****************************************************************
*
MAIN     CSECT
         USING MAIN,15     ESTABLISH ADDRESSABILITY ON REG 15
         L     2,NUM1      LOADING NUM1 INTO REG 2
         L     3,NUM2      LOADING NUM2 INTO REG 3
*
         SR    2,3         SUBTACT 3 FROM 2
         ST    4,ANSWER    STORE THE DIFFERENCE IN ANSWER
*
         XDUMP ANSWER,4    DUMPS THE REGISTRER
         BCR   B'1111',14  UNCONDITIONAL RETURN TO CALLER (OS)
*
NUM2     DC    F'15'       DEFINE FULLWORD OF 15 IN STORAGE
NUM1     DC    F'7'        DEFINE FULLWORD OF 7 IN STORAGE
ANSWER   DS    F           FULLWORD STORAGE
*
         END   MAIN
/*
//
