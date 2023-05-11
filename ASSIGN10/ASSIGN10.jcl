//XXXXXXXXX JOB ,'K.GANDHI',MSGCLASS=H
//JSTEP01  EXEC PGM=ASSIST
//STEPLIB  DD DSN=KC00NIU.ASSIST.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
*******************************************************************
*                                                                 *
*  CSCI 360-PE1           ASSIGNMENT 10                 SPRING 23 *
*                    SUBPROGRMAS AND LINKAGE                      *
*                                                                 *
*  DEVELOPER NAME: KUSH GANDHI                                    *
*        DUE DATE: 5/5/23                                         *
*                                                                 *
*    PROGRAM NAME: SUBPROGRMAS AND LINKAGE                        *
*                                                                 *
*    FUNCTION: USES EXTERNAL SUBROUTINGS TO MAKE A REPORT         *
*******************************************************************
*
PAYROLL3 CSECT
*
*  STANDARD ENTRY LINKAGE WITH R12 AS BASE REGISTER
*
         STM   14,12,12(13)  SAVE REGS IN CALLER'S SAVE AREA
         LR    12,15         COPY CSECT ADDR INTO R12
         USING PAYROLL3,12   ESTABLISH R12 AS THE BASE REG
         LA    14,SAVEREGS   R14 POINTS TO THIS CSECT'S SAVE AREA
         ST    14,8(,13)     STORE ADDR OF THIS CSECT'S SAVE AREA
         ST    13,4(,14)     STORE ADDR OF CALLER'S SAVE AREA
         LR    13,14         POINT R13 AT THIS CSECT'S SAVE AREA
*
         LA    11,4095(,12)  R11 -> R12 + 4095
         LA    11,1(,11)     R11 -> R12 + 4096
         USING PAYROLL3+4096,11
*
         LA    1,BTPARMS           R1 -> PARAMETER LIST FOR BUILDTBL
         L     15,=V(BUILDTBL)     R15 -> SUBPROGRAM BUILDTBL
         BALR  14,15               BRANCH AND LINK TO BUILDTBL
*
         LA    1,PTPARMS           R1 -> PARAMETER LIST FOR PROCTBL
         L     15,=V(PROCTBL)      R15 -> SUBPROGRAM PROCTBL
         BALR  14,15               BRANCH AND LINK TO PROCTBL
*
         LA    1,CNPPARMS          R1 -> PARAMETER LIST FOR CALCNPAY
         L     15,=V(CALCNPAY)     R15 -> SUBPROGRAM CALCNPAY
         BALR  14,15               BRANCH AND LINK TO CALCNPAY
*
         LA    1,CAPARMS           R1 -> PARAMETER LIST FOR CALCAVG
         L     15,=V(CALCAVG)      R15 -> SUBPROGRAM CALCAVG
         BALR  14,15               BRANCH AND LINK TO CALCAVG
         LA    2,EMPTBL
         LA    3,117
*
LOOP2    XPRNT 5(2),42
*
         LA    2,42(,2)
*
         BCT   3,LOOP2
*
*  STANDARD EXIT LINKAGE WITH RC OF 0
*
ENDPGM   SR    15,15        R15 = RETURN CODE OF 0
         L     13,4(,13)    POINT R13 TO CALLER'S SAVE AREA
         L     14,12(,13)   RESTORE REGISTER 14
         LM    0,12,20(13)  RESTORE R0 THRU R12
*
         BR    14           RETURN TO CALLER
*
         LTORG              LITERAL ORGANIZATION
*
SAVEREGS DS    18F          REGISTER SAVE AREA
*
*
*
BTPARMS  DC    A(EMPTBL)    ADDRESS OF EMPLOYEE TABLE
         DC    A(PEMPCTR)   ADDRESS OF EMPLOYEE COUNTER
         DC    A(PFWHPCT)   ADDRESS OF FEDERAL PERCENT
         DC    A(PSWHPCT)   ADDRESS OF STATE PERCENT
*
PTPARMS  DC    A(EMPTBL)    ADDRESS OF EMPLOYEE TABLE
         DC    A(PEMPCTR)   ADDRESS OF EMPLOYEE COUNTER
         DC    A(PFWHPCT)   ADDRESS OF FEDERAL PERCENT
         DC    A(PSWHPCT)   ADDRESS OF STATE PERCENT
*
CNPPARMS DC    A(PEMPGPAY)  ADDRESS OF GROSS PAY
         DC    A(PEMPNPAY)  ADDRESS OF NET PAY
         DC    A(PFWHPCT)   ADDRESS OF FEDERAL PERCENT
         DC    A(PFEDWITH)  ADDRESS OF FEDERL WITHOLDING
         DC    A(PSWHPCT)   ADDRESS OF STATE PERCENT
         DC    A(PSTWITH)   ADDRESS OF STATE WITHOLDING
*
CAPARMS  DC    A(PTOTAL)    ADDRESS OF TOTALS CALCULATION
         DC    A(PEMPCTR)   ADDRESS OF EMPLOYEE COUNTER
         DC    A(PAVG)      ADDRESS OF AVERGAES
*
PTOTAL   DC    PL6'0'         PACKED CALCULATED TOTAL
PAVG     DC    PL7'0'         PACKED CALCULATED AVG
PFEDWITH DC    PL6'0'         PACKED CALCULATED FEDERAL WITHHOLDING
PSTWITH  DC    PL6'0'         PACKED CALCULATED STATE WITHOLDING
PEMPNPAY DC    PL6'0'         PACKED CALCULATED EMPLOYEE NET PAY
*
PTGRPAY  DC    PL7'0'         PACKED TOTAL GROSS EMPLOYEE PAY
PTFWITH  DC    PL7'0'         PACKED TOTAL WITHHOLDING
PTSWITH  DC    PL7'0'
PTNETPAY DC    PL7'0'         PACKED TOTAL NET EMPLOYEE PAY
*
PEMPCTR  DC    PL3'0'
*
EMPTBL   DS    120CL42        EMPLOYEE TABLE
*
*
*
*
*
*
*****************************************************************
*                                                               *
* THIS IS THE BUILDTBL EXTERNAL IMPLEMENTATION WHICH PACKS THE  *
* EMPLOYEE CONTENTS INTO A TABLE.                               *
*                                                               *
*****************************************************************
*
$INFO    DSECT
$EMPID   DS   ZL8            EMPLOYE ID
$NAME    DS   CL25           EMPLOYEE NAME
$HRPAY   DS   ZL5            HOURLY PAY
$HRWORK  DS   ZL5            HOURS WORKED
$DEDUCT  DS   ZL5            DEDUCTIONS
$BONUS   DS   ZL5            BONUS
*
$TBLENT1 DSECT               TABLE DECLARATION
$TEMPID  DS   PL5            EMPLOYEE ID
$TEMPNM  DS   CL25           EMPLOYEE NAME
$THRPAY  DS   PL3            HOURLY PAY
$THRWORK DS   PL3            HOURS WORKED
$TDEDUCT DS   PL3            DEDUCTIONS
$TBONUS  DS   PL3            BONUS
*
BUILDTBL CSECT
*
*  STANDARD ENTRY LINKAGE WITH R12 AS BASE REGISTER
*
         STM   14,12,12(13)  SAVE REGS IN CALLER'S SAVE AREA
         LR    12,15         COPY CSECT ADDR INTO R12
         USING BUILDTBL,12   ESTABLISH R12 AS THE BASE REG
         LA    14,BTSAVE     R14 POINTS TO THIS CSECT'S SAVE AREA
         ST    14,8(,13)     STORE ADDR OF THIS CSECT'S SAVE AREA
         ST    13,4(,14)     STORE ADDR OF CALLER'S SAVE AREA
         LR    13,14         POINT R13 AT THIS CSECT'S SAVE AREA
*
         LM    2,5,0(1)      R2 -> EMPTBL
*                            R3 -> PEMPCTR
*                            R4 -> PFWHPCT
*                            R5 -> PSWHPCT
*
         USING $INFO,4       ESTABLISH DSECT ADDRESSABILITY
         USING $TBLENT1,2    ESTABLISH TABLE DSECT ADDRESSABILITY
*
         XREAD RECORD,80     READ PERCENTAGES
*
         PACK  PFWHPCT(3),RECORD(5)     PACK FEDERAL PERCENT
         PACK  PSWHPCT(3),NUM1(5)       PACK STATE PERCENT
*
         LA    4,RECORD      R4 -> RECORD BUFFER
*
         XREAD RECORD,80     READ FIRST RECORD
*
LOOP1    BNZ   ENDLOOP1      BRANCH IF NO RECORD (EOF)
*
         AP    0(3,3),=PL1'1'          INCREMENT EMP COUNTER
         PACK  $TEMPID(4),$EMPID(8)    PACK EMPLOYEE ID
         PACK  $THRPAY(3),$HRPAY(5)    PACK HOURLY PAY
         PACK  $THRWORK(3),$HRWORK(5)  PACK HOURS WORKED
         PACK  $TDEDUCT(3),$DEDUCT(5)  PACK DEDUCTIONS
         PACK  $TBONUS(3),$BONUS(5)    PACK BONUS
         MVC   $TEMPNM(25),$NAME       READ EMPLOYEE NAME
*
         LA    2,42(,2)      R2 -> NEXT ENTRY IN EMPTBL
*
         XREAD RECORD,80     READ NEXT RECORD
*
         B     LOOP1         BRACH BACK TO TOP OF LOOP1
*
         DROP  4,2           DROP R4 AS BASE FOR DSECT
*
ENDLOOP1 SR    15,15         R15 = RETURN CODE OF 0
         L     13,4(,13)     POINT R13 TO CALLER'S SAVE AREA
         L     14,12(,13)    RESTORE REGISTER 14
         LM    0,12,20(13)   RESTORE R0 THRU R12
*
         BR    14            RETURN TO CALLER
*
         LTORG               LITERAL ORGANIZATION
*
BTSAVE   DS    18F           REGISTER SAVE AREA
*
RECORD   DS    CL80          INPUT BUFFER
NUM1     DC    ZL5'3250'
PFWHPCT  DC    PL4'0'        PACKED FEDERAL WITHHOLDING PERCENTAGE
PSWHPCT  DC    PL4'0'        PACKED STATE WITHHOLDING PERCENTAGE
*
*
*
*****************************************************************
*                                                               *
* THIS IS THE PROCTBL EXTERNAL IMPLEMENTATION AND THE           *
* TABLEIZED DSECT TO ACCESS THE DATA FROM THE TABLE             *
*                                                               *
*****************************************************************
*
$TABLE2  DSECT
$TBLNM2  DS    CL25          TABLEIZED NAME
$TBLID2  DS    PL5           TABLEIZED ID
$TBLHR2  DS    PL3           TABLEIZED HOURLY PAY
$TBWORK2 DS    PL3           TABLEIZED HOURS WORKED
$TBLDUC2 DS    PL3           TABLEIZED DEDUCTIONS
$TBLBON2 DS    PL3           TABLEIZED BONUS
*
PROCTBL  CSECT
*
*  STANDARD ENTRY LINKAGE WITH R12 AS BASE REGISTER
*
         STM   14,12,12(13)  SAVE REGS IN CALLER'S SAVE AREA
         LR    12,15         COPY CSECT ADDR INTO R12
         USING PROCTBL,12    ESTABLISH R12 AS THE BASE REG
         LA    14,PTSAVE     R14 POINTS TO THIS CSECT'S SAVE AREA
         ST    14,8(,13)     STORE ADDR OF THIS CSECT'S SAVE AREA
         ST    13,4(,14)     STORE ADDR OF CALLER'S SAVE AREA
         LR    13,14         POINT R13 AT THIS CSECT'S SAVE AREA
*
         LA    6,99          LINE COUNTER
         LM    2,5,0(1)      R2 -> EMPLOYEE TABLE
*                            R3 -> EMPLOYEE COUNTER
*                            R4 -> FEDERAL WITHOLDING PERCENT
*                            R5 -> STATE WITHOLDING PERCENT
*
         USING $TABLE2,2     EST. ADDRESSABILITY FOR TABLE ENTRY DSECT'
*
         ZAP   DBLWORD(8),0(3,3)  PUT EMPCTR IN DOUBLE WORD
         CVB   4,DBLWORD          CONVERT EMPCTR TO BINARY IN R4
*
LOOP     MVC   OEMPNAME(25),$TBLNM2  COPY THE EMPLOYEE NAME TO PRINT
*
*
* GET HOURLY PAY
         LA    1,OHRPAY+1                     SETS BYTE
         MVC   OHRPAY(7),=X'402021204B20' EDIT PATTERN
         ED    OHRPAY(7),$TBLHR2             CONVERT TO PRINTABLE EBCDI
         BCTR  1,0            DECREMENT $ SIGN POINTER BY 1
         MVI   0(1),C'$'                      INSERT A $
*
*
* GET HOURS WORKED
         MVC   OHOURS(7),=X'402021204B2020'   EDIT PATTERN
         ED    OHOURS(7),$TBWORK2             CONVERT TO PRINTABLE
*
*
* PRINTS OUT PAGE COUNTER AND FORMATING
         C     6,=F'17'                COMPARE EVERY 17 DETAIL LINES
         BL    SKIPHEAD                BRANCH IF NOT READY
         AP    PPAGECTR(2),=PL1'1'     ADD 1 PAGE COUNTER
         MVC   OPAGE(4),=X'40202120'   EDIT PATTERN
         ED    OPAGE(4),PPAGECTR       CONVERT TO PRINTABLE EBCDIC
*
         XPRNT HEADER,133              PRINTS THE TITLE AND PAGE NUM
         XPRNT SUBHEAD,133             PRINTS SUB HEAD TITLE
         XPRNT HEADER1,133             PRINTS TABLE NAMES
         XPRNT HEADER2,133             MORE TABLE NAMES
         XPRNT HYPHENS,133             PRINT LINES
*
         SR    6,6                     ZERO OUT THE LINE COUNTER
*
SKIPHEAD XPRNT EMPLOYEE,133  PRINT EMPLOYEE DETAILS
         LA    6,1(,6)       ADD 1 TO LINE COUNTER
*
         LA    2,42(,2)      ADVANCE TO NEXT ENTRY
*
         BCT   4,LOOP        BRACNCH BACK TO LOOP
*
         XPRNT HEADER,133           PRINTS THE TITLE AND PAGE NUM
         XPRNT SUBHEAD,133          PRINTS SUB HEAD TITLE
         XPRNT LINE,133             PRINTS 'TOTAL'
         XPRNT TOTALS,133           PRINTS NUMBER OF EMPLOYEES
         XPRNT TOTALS1,133          PRINTS TOTAL GPAY AND AVERAGE
         XPRNT TOTALS2,133          PRINTS TOTAL FED AND AVERAGE
         XPRNT TOTALS3,133          PRINTS TOTAL ST AND AVERAGE
         XPRNT TOTALS4,133          PRINTS NET PAY AND AVERAGE
*
         DROP  2
*
         SR    15,15         R15 = RETURN CODE OF 0
         L     13,4(,13)     POINT R13 TO CALLER'S SAVE AREA
         L     14,12(,13)    RESTORE REGISTER 14
         LM    0,12,20(13)   RESTORE R0 THRU R12
         BR    14            RETURN TO CALLER
*
         LTORG
*
PTSAVE   DS    18F           REGISTER SAVE AREA
*
CNPARMS  DS    A(PEMPGPAY)  <- DECLARED IN PROCTBL
         DS    A(PEMPNPAY)  <- DECLARED IN PROCTBL
         DS    A(0)         <- DECLARED IN PROCTBL
         DS    A(PFEDWITH)  <- DECLARED IN PROCTBL
         DS    A(0)         <- DECLARED IN PROCTBL
         DS    A(PSTWITH)   <- DECLARED IN PROCTBL
*
DBLWORD  DC    D'0'
PPAGECTR DC    PL2'0'       PACKED PAGE COUNTER (MAX 999)
PEMPGPAY DC    PL6'0'       PACKED CALCULATED EMPLOYEE GROSS PAY
*
EMPLOYEE DC    C'0'
OEMPID   DS    CL8
         DC    4C' '
OEMPNAME DS    CL25
         DC    4C' '
OHRPAY   DS    CL5
         DS    4C' '
OHOURS   DS    CL5
         DC    4C' '
OGPAY    DS    CL15
         DC    4C' '
OFEDWH   DS    CL15
         DC    4C' '
OSTWH    DS    CL15
         DC    4C' '
ONETPAY  DS    CL15
         DC    2C' '
*
* PRINTS OUT THE TITLE AND PAGE COUNTER
*
HEADER   DC    C'0'
         DC    40C' '
         DC    C'STATE OF ILLINOIS NATIONAL BANK'
         DC    50C' '
         DC    C'PAGE: '
OPAGE    DS    CL4
         DC    39C' '
*
* PRINTS OUT SUB HEADER TITLE
*
SUBHEAD  DC    C'0'
         DC    38C' '
         DC    C'SEMI-MONTHLY EMPLOYEE PAYROLL REPORT'
         DC    95C' '
*
* PRINTS OUT COLUMN NAMES
*
HEADER1  DC    C'0'
         DC    C'EMPLOYEE'
         DC    4C' '
         DC    C'EMPLOYEE'
         DC    21C' '
         DC    C'HOURLY'
         DC    2C' '
         DC    C'HOURS'
         DC    8C' '
         DC    C'EMPLOYEE'
         DC    4C' '
         DC    C'EMPLOYEE FEDERAL'
         DC    4C' '
         DC    C'EMPLOYEE STATE'
         DC    8C' '
         DC    C'EMPLOYEE'
         DC    82C' '
*
* PRINTS MORE COLUMN NAMES
*
HEADER2  DC    C'0'
         DC    C'ID'
         DC    10C' '
         DC    C'NAME'
         DC    25C' '
         DC    C'PAY'
         DC    5C' '
         DC    C'WORKED'
         DC    7C' '
         DC    C'GROSS PAY'
         DC    9C' '
         DC    C'WITHOLDING'
         DC    8C' '
         DC    C'WITHOLDING'
         DC    8C' '
         DC    C'NET PAY'
         DC    61C' '
*
* OUTPUT FOR HYPHENS
*
HYPHENS  DC    C'0'
         DC    C'---------'
         DC    3C' '
         DC    C'-------------------------'
         DC    4C' '
         DC    C'-------'
         DC    1C' '
         DC    C'-------'
         DC    2C' '
         DC    C'---------------'
         DC    3C' '
         DC    C'----------------'
         DC    3C' '
         DC    C'----------------'
         DC    3C' '
         DC    C'----------------'
         DC    114C' '
*
* OUTPUT FOR THE TOTALS
*
TOTALS   DC    C'0'
         DC    5C' '
         DC    C'NUMBER OF EMPLOYEES: '
         DC    10C' '
OTEMPCT  DS    CL3
         DC    115C' '
*
* OUTPUT FOR TOTAL GROSS PAY AND AVERAGE
*
TOTALS1  DC    C'0'
         DC    9C' '
         DC    C'TOTAL GROSS PAY:      '
OTGPAY   DS    CL10
         DC    19C' '
         DC    C'AVERAGE GROSS PAY:    '
OGPAVG   DS    CL13
         DC    82C' '
*
* OUTPUT FOR TOTAL FEDERAL AND AVERAGE
*
TOTALS2  DC    C'0'
         DC    C'TOTAL FEDERAL WITHOLDING:       '
OTFWH    DS    CL10
         DC    10C' '
         DC    C'AVERAGE FEDERAL WITHOLDING:          '
OFEDAVG  DS    CL11
         DC    102C' '
*
* OUTPUT FOR TOTAL STATE AND AVERAGE
*
TOTALS3  DC    C'0'
         DC    2C' '
         DC    C'TOTAL STATE WITHOLDING:       '
OTSTWH   DS    CL9
         DC    12C' '
         DC    C'AVERAGE STATE WITHOLDING:     '
OSTAVG   DS    CL12
         DC    98C' '
*
* OUTPUT FOR TOTAL NET PAY AND AVERAGE
*
TOTALS4  DC    C'0'
         DC    11C' '
         DC    C'TOTAL NET PAY:     '
OTNP     DS    CL10
         DC    23C' '
         DC    C'AVERAGE NET PAY:        '
ONETAVG  DS    CL15
         DC    74C' '
*
* OUTPUT FOR TITLE
*
LINE     DC    C'0'
         DC    52C' '
         DC    C'TOTALS'
         DC    81C' '
*
*
*
*****************************************************************
*                                                               *
* THIS IS THE CALCNPAY EXTERNAL IMPLEMENTATION                  *
*                                                               *
*****************************************************************
*
CALCNPAY CSECT
*
*  STANDARD ENTRY LINKAGE WITH R12 AS BASE REGISTER
*
         STM   14,12,12(13)  SAVE REGS IN CALLER'S SAVE AREA
         LR    12,15         COPY CSECT ADDR INTO R12
         USING CALCNPAY,12   ESTABLISH R12 AS THE BASE REG
         LA    14,CNSAVE     R14 POINTS TO THIS CSECT'S SAVE AREA
         ST    14,8(,13)     STORE ADDR OF THIS CSECT'S SAVE AREA
         ST    13,4(,14)     STORE ADDR OF CALLER'S SAVE AREA
         LR    13,14         POINT R13 AT THIS CSECT'S SAVE AREA
*
         LM    2,6,0(1)
*                            R2 -> PEMPNPAY
*                            R3 -> PFWHPCT
*                            R4 -> PFEDWITH
*                            R5 -> PSWHPCT
*                            R6 -> PSTWITH
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
CNSAVE   DS    18F          REGISTER SAVE AREA
*
*
*
*****************************************************************
*                                                               *
* THIS IS THE PROCTBL EXTERNAL IMPLEMENTATION AND THE           *
* TABLEIZED DSECT TO ACCESS THE DATA FROM THE TABLE             *
*                                                               *
*****************************************************************
*
CALCAVG  CSECT
*
*  STANDARD ENTRY LINKAGE WITH R12 AS BASE REGISTER
*
         STM   14,12,12(13)  SAVE REGS IN CALLER'S SAVE AREA
         LR    12,15         COPY CSECT ADDR INTO R12
         USING CALCAVG,12    ESTABLISH R12 AS THE BASE REG
         LA    14,CAVG       R14 POINTS TO THIS CSECT'S SAVE AREA
         ST    14,8(,13)     STORE ADDR OF THIS CSECT'S SAVE AREA
         ST    13,4(,14)     STORE ADDR OF CALLER'S SAVE AREA
         LR    13,14         POINT R13 AT THIS CSECT'S SAVE AREA

         LM    2,4,0(1)      R2 -> PTOTAL
*                            R3 -> PEMPCTR
*                            R4 -> PAVG
*
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
CAVG     DS    18F
*
PCALC    DC    PL9'0'      USED TO CALCULATE WITHHOLDING AND AVGS
*
         END   PAYROLL3
/*
//*
//FT05F001 DD DSN=KC02322.CSCI360.ASNDATA(DATA9),DISP=SHR
//*
//FT06F001 DD SYSOUT=*
//
