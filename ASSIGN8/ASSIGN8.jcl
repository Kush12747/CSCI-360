//XXXXXXXXX JOB ,'K.GANDHI',MSGCLASS=H
//JSTEP01  EXEC PGM=ASSIST
//STEPLIB  DD DSN=KC00NIU.ASSIST.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
         PRINT NOGEN
*******************************************************************
*                                                                 *
*  CSCI 360-PE1           ASSIGNMENT 8                 SPRING 23  *
*                    INTRO TO PACKED DECIMAL                      *
*                                                                 *
*  DEVELOPER NAME: KUSH GANDHI                                    *
*        DUE DATE: 4/7/23                                         *
*                                                                 *
*    PROGRAM NAME: PACKED DECIMAL                                 *
*                                                                 *
*    FUNCTION: PRINTS A PROPER REPORT USING PACKED DECIMAL        *
*******************************************************************
*
PAYROLL2 CSECT
*
*  STANDARD ENTRY LINKAGE WITH R12 AS BASE REGISTER
*
         STM   14,12,12(13)  SAVE REGS IN CALLER'S SAVE AREA
         LR    12,15         COPY CSECT ADDR INTO R12
         USING PAYROLL2,12   ESTABLISH R12 AS THE BASE REG
         LA    14,SAVEREGS   R14 POINTS TO THIS CSECT'S SAVE AREA
         ST    14,8(,13)     STORE ADDR OF THIS CSECT'S SAVE AREA
         ST    13,4(,14)     STORE ADDR OF CALLER'S SAVE AREA
         LR    13,14         POINT R13 AT THIS CSECT'S SAVE AREA
*
*
         XREAD DETAIL,80
LOOP     BC   B'0100',ENDLOOP BRANCH TO ENDLOOP1 IF EOF
*
* READS THE EMPLOYEE NAME AND ID INTO PRINTLINE
         MVC   OEMPNAME(25),DETAIL+0
         MVC   OEMPID(6),DETAIL+25
*
         PACK  PAYRATE(3),PAY(5)            PACKS PAYRATE
         MVC   OHOURPAY(5),=X'20204B2020'
         ED    OHOURPAY(5),PAYRATE          GETS IT READY FOR OUTPUT
*
         PACK  HOURS(3),HOURSWOR(5)         PACKS HOURS WORKED
         MVC   OHOURS(5),=X'20204B2020'     EDIT PATTERN FOR HOURS
         ED    OHOURS(5),HOURS              GETS IT READY FOR OUTPUT
*
* AFTER THIS STEP, I AM GETTING AN ABEND FOR ZAP WHICH I DIDN'T INCLUDE
* IT. BUT IT SHOULD BE SOMETHING LIKE ZAP GPAY(6),PAY(3) MP GPAY(6),
* HOURSWORK(3) SRP GPAY(6),64-2,5
*
         XPRNT EMPLOYEE,133                 PRINTS EMPLOYEE DETAILS
         XREAD DETAIL,133                   READ THE NEXT EMPLOYEE
         BC    B'1111',LOOP
*
ENDLOOP  DS    0H
*
*  STANDARD EXIT LINKAGE WITH RC OF 0
*
         SR    15,15        R15 = RETURN CODE OF 0
         L     13,4(,13)    POINT R13 TO CALLER'S SAVE AREA
         L     14,12(,13)   RESTORE REGISTER 14
         LM    0,12,20(13)  RESTORE R0 THRU R12
*
         BR    14           RETURN TO CALLER
*
         LTORG
*
SAVEREGS DS    18F          PROGRAM'S REGISTER SAVE AREA
*
*  Here is where your storage will be defined.
*
DETAIL   DS    CL80
*
PAY      DS    ZL5          PACK INTO 3 BYTES FOR PAYRATE
HOURSWOR DS    ZL5          PACKS INTO 3 BYTES FOR HOURS
*
PAYRATE  DC    PL3'0'       PACKED EMPLOYEE PAYRATE
HOURS    DC    PL3'0'       PACKED HOUR RATE
GPAY     DC    PL5'0'       PACKED TOTAL GROSS PAY
*
* OUTPUT FOR EMPLOYEE DETAIL RECORD
*
EMPLOYEE DC    C'0'
OEMPNAME DS    CL25
         DC    4C' '
OEMPID   DS    CL5
         DC    4C' '
OHOURPAY DS    CL4
         DC    4C' '
OHOURS   DS    CL5
         DC    4C' '
OGPAY    DS    CL8
         DC    70C' '
*
         END   PAYROLL2
/*
//*
//FT05F001 DD DSN=KC02322.CSCI360.ASNDATA(DATA8),DISP=SHR
//*
//FT06F001 DD SYSOUT=*
//
