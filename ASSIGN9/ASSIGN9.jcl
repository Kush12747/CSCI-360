//XXXXXXXX JOB ,'K.GANDHI',MSGCLASS=H
//JSTEP01  EXEC PGM=ASSIST
//STEPLIB  DD DSN=KC00NIU.ASSIST.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
*******************************************************************
*                                                                 *
*  CSCI 360-PE1           ASSIGNMENT 9                 SPRING 23  *
*                        PACKED DECIMAL                           *
*                                                                 *
*  DEVELOPER NAME: KUSH GANDHI                                    *
*        DUE DATE: 4/21/23                                        *
*                                                                 *
*    PROGRAM NAME: PACKED DECIMAL                                 *
*                                                                 *
*    FUNCTION: CREATING A PROPER REPORT USING PACKED DECIMAL      *
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
         LA    2,99           LINE COUNTER
*
         XREAD RECORD,80      READS STATE PERCENT
*
         PACK  PFWHPCT(3),RECORD(5) PACK AND STORE FED PERCENT
         PACK  PSWHPCT(3),NUM1(5)   PACK AND STORE ST PERCENT
*
         XREAD RECORD,80      READS EMPLOYEE DATA
*
LOOP1    BC   B'0100',ENDLOOP1 BRANCH TO ENDLOOP1 IF EOF
*
         AP    PEMPCTR(3),=PL1'1'    ADD 1 TO EMPLOYEE COUNTER
         MVI   EMPLOYEE+1,C' '
         MVC   EMPLOYEE+2(131),EMPLOYEE+1
*
*
* PACKS AND STORES EMPLOYEE ID
         PACK  PEMPID(4),IEMPID(8)   PACK AND STORE EMPLOYEE ID
         MVC   OEMPID(8),=X'402020602020202020202020'  EDIT PATTERN
         ED    OEMPID(8),PEMPID      CONVERT TO PRINTABLE EBCDIC
*
*
* READS EMPLOYEE NAME BY MVC
         MVC   OEMPNAME(25),IEMPNME  READS NAME AND MOVE TO PRINTLINE
*
*
* GET HOURLY PAY AND PACK IT
         LA    1,OHRPAY+1                     SETS BYTE
         PACK  PHRPAY(3),IHRPAY(5)            PACK HOURLY PAY
         MVC   OHRPAY(7),=X'402021204B202020' EDIT PATTERN
         ED    OHRPAY(7),PHRPAY             CONVERT TO PRINTABLE EBCDIC
         MVI   0(1),C'$'                      INSERT A $
*
*
* GET HOURS WORKED AND PACK IT
         PACK  PHOURS(3),IHOURS(5)          PACK HOURS
         MVC   OHOURS(7),=X'402021204B2020'   EDIT PATTERN
         ED    OHOURS(7),PHOURS             CONVERT TO PRINTABLE EBCDIC
*
*
* GET DEDUCTION AMT AND PACK IT
         PACK  PDEDUCT(3),IDEDUCT(5)
*
*
* GET BONUS AMT AND PACK IT
         PACK  PBONUS(3),IBONUS(5)
*
*
* GROSS PAY CALCULATIONS
         LA    1,OGPAY+6               SETS BYTE FOR $
         ZAP   PEMPGPAY(6),PHRPAY(3)   INITIALIZE GROSS PAY WITH HR PAY
         MP    PEMPGPAY(6),PHOURS(3)   HOURLY PAY * HOURS WORKED
         SRP   PEMPGPAY(6),64-2,5      ROUND 2 DECIMAL PLACES
         SP    PEMPGPAY(6),PDEDUCT(3)  GROSS PAY - DEDUCTIONS
         AP    PEMPGPAY(6),PBONUS(3)   GROSS PAY + BONUS
*
         MVC   OGPAY(15),=X'402020206B2020206B2020204B2020'
*
         ED    OGPAY(15),PEMPGPAY      CONVERT TO PRINTABLE EBCDIC
         AP    PTGRPAY(7),PEMPGPAY(6)  ADD TO TOTAL
         MVI   0(1),C'$'               INSERT A $
*
*
* CALCULATE EMPLOYEE FEDERAL WITHOLDING
         LA    1,OFEDWH+4              SETS BYTE FOR $
         ZAP   PCALC(10),PEMPGPAY(6)   INITIALIZE THE PCALC FOR G-PAY
         MP    PCALC(10),PFWHPCT(3)    GROSS PAY * FEDERAL PERCENT
         SRP   PCALC(10),64-4,5        ROUND 4 DECIMAL PLACES
         ZAP   PFEDWITH(6),PCALC(10)   COPY AMT OVER TO PFEDWITH
*
         MVC   OFEDWH(15),=X'402020206B2020206B2020204B2020'
*
         ED    OFEDWH(15),PFEDWITH     CONVERT TO PRINTABLE EBCDIC
         AP    PTFWITH(7),PFEDWITH(6)  ADD TO TOTAL
         MVI   0(1),C'$'               INSERT A $
*
*
* CALCULATE EMPLOYEE STATE WITHOLDING
         LA    1,OSTWH+6               SETS BYTE FOR $
         ZAP   PCALC(10),PEMPGPAY(6)   INITIALIZE THE PCALC FOR G-PAY
         MP    PCALC(10),PSWHPCT(3)    GROSS PAY * STATE PERCENT
         SRP   PCALC(10),64-4,5        ROUND 4 DECIMAL PLACES
         ZAP   PSTWITH(6),PCALC(10)    COPY AMT OVER TO PSEDWITH
*
         MVC   OSTWH(14),=X'4020206B2020206B2020204B2020'
*
         ED    OSTWH(14),PSTWITH       CONVERT TO PRINTABLE EBCDIC
         AP    PTSWITH(7),PSTWITH(6)   ADD TO TOTAL
         MVI   0(1),C'$'               INSERT A $
*
*
* CALCULATE EMPLOYE NET PAY
         LA    1,ONETPAY+6               SETS BYTE FOR $ OUTPUT
         SP    PEMPGPAY(6),PFEDWITH(6)   GROSS PAY - FEDERAL
         SP    PEMPGPAY(6),PSTWITH(6)    GROSS PAY - STATE
         ZAP   PEMPNPAY(6),PEMPGPAY(6)   INITIALIZE INTO NETPAY

*
         MVC   ONETPAY(15),=X'402020206B2020206B2020204B2020'
*
         ED    ONETPAY(15),PEMPNPAY      CONVERT TO PRINTABLE EBCDIC
         AP    PTNETPAY(7),PEMPNPAY(6)   ADD TO TOTAL
         MVI   0(1),C'$'                 INSERT A $
*
*
* PRINTS OUT PAGE COUNTER AND FORMATING
         C     2,=F'17'                COMPARE EVERY 17 DETAIL LINES
         BL    SKIPHEAD                BRANCH IF NOT READY
         AP    PPAGECTR(2),=PL1'1'     ADD 1 PAGE COUNTER
         MVC   OPAGE(4),=X'40202120'   EDIT PATTERN
         ED    OPAGE(4),PPAGECTR       CONVERT TO PRINTABLE EBCDIC
*
         XPRNT HEADER,133           PRINTS THE TITLE AND PAGE NUM
         XPRNT SUBHEAD,133          PRINTS SUB HEAD TITLE
         XPRNT HEADER1,133          PRINTS TABLE NAMES
         XPRNT HEADER2,133          MORE TABLE NAMES
         XPRNT HYPHENS,133          PRINT LINES
*
         SR    2,2                  ZERO OUT THE LINE COUNTER
*
SKIPHEAD XPRNT EMPLOYEE,133         PRINTS EMPLOYEE DETAILS
         LA    2,1(,2)              ADD 1 TO LINE COUNTER
*
         XREAD RECORD,80            READ THE NEXT EMPLOYEE
*
         BC    B'1111',LOOP1  BRANCH TO TOP OF LOOP1 TO CHECK EOF
*
ENDLOOP1 DS    0H
*
* EMPLOYE COUNT
         SRP   PEMPCTR(3),2,0          SHIFT 2 LEFT
         MVC   OTEMPCT(3),=X'202020'   EDIT PATTERN
         ED    OTEMPCT(3),PEMPCTR      CONVERT TO PRINTABLE EBCDIC
*
* TOTAL GROSS PAY AND AVERAGE
         LA    1,OTGPAY+0              SETS BYTE FOR $
         SRP   PTGRPAY(7),5,0          SHIFT LEFT 5
*
         MVC   OTGPAY(11),=X'402020206B2021204B2020'
*
         ED    OTGPAY(11),PTGRPAY      CONVERT TO PRINTABLE EBCDIC
         MVI   0(1),C'$'               INSERT A $
*
         LA    1,OGPAVG+4              SETS BYTE FOR $
         ZAP   PCALC(10),PTGRPAY(7)    INITIALIZE TOTAL GPAY INTO PCALC
         DP    PCALC(10),PEMPCTR(3)    TOTAL GPAY / NUM EMPLOYESS
*
         MVC   OGPAVG(13),=X'4020202020206B2021204B20202020'
*
         ED    OGPAVG(13),PCALC        CONVERT TO PRINTABLE EBCDIC
         MVI   0(1),C'$'               INSERT A $
*
* TOTAL FEDERAL WITHOLDING AND AVERAGE
         LA    1,OTFWH+1               SET BYTE FOR $
         SRP   PTFWITH(7),5,0          SHIFT 5 LEFT
*
         MVC   OTFWH(11),=X'402020206B2021204B2020'     EDIT PATTERN
*
         ED    OTFWH(11),PTFWITH       CONVERT TO PRINTABLE EBCDIC
         MVI   0(1),C'$'               INSERT A $
*
         LA    1,OFEDAVG+0             SETS BYTE FOR $
         ZAP   PCALC(10),PTFWITH(7)    INITIALIZE TOT FEDERAL IN PCALC
         DP    PCALC(10),PEMPCTR(3)    TOTAL FED / NUM EMPLOYEES
*
         MVC   OFEDAVG(11),=X'20202020202020204B2020'
*
         ED    OFEDAVG(11),PCALC       CONVERT TO PRINTABLE EBCDIC
         MVI   0(1),C'$'               INSERT A $
*
* TOTAL STATE WITHOLDING AND AVERAGE
         LA    1,OTSTWH+0              SETS BYTE FOR $
         SRP   PTSWITH(7),5,0          SHIFT LEFT 5
*
         MVC   OTSTWH(11),=X'4020206B2020214B202020'   EDIT PATTERN
*
         ED    OTSTWH(11),PTSWITH      CONVERT TO PRINTABLE EBCDIC
         MVI   0(1),C'$'               INSERT A $
*
         LA    1,OSTAVG+6              SETS BYTE FOR $
         ZAP   PCALC(10),PTSWITH(7)    INITIALIZE TOTAL STATE IN PCALC
         DP    PCALC(10),PEMPCTR(3)    TOTAL ST. / NUM EMPLOYEES
*
         MVC   OSTAVG(11),=X'40202020206B2020214B202020'
*
         ED    OSTAVG(11),PCALC        CONVERT TO PRINTABLE EBCDIC
         MVI   0(1),C'$'               INSERT A $
*
* TOTAL NET PAY AND AVERAGE
         LA    1,OTNP+0                 SETS BYTE FOR $
         SRP   PTNETPAY(7),5,0          SHIFT LEFT 5
*
         MVC   OTNP(11),=X'402020206B2021204B2020'    EDIT PATTERN
*
         ED    OTNP(11),PTNETPAY        CONVERT TO PRINTABLE EBCDIC
         MVI   0(1),C'$'                INSERT A $
*
         LA    1,ONETAVG+0
         ZAP   PCALC(10),PTNETPAY(7)    INITIALIZE TOTAL NP INTO PCALC
         DP    PCALC(10),PEMPCTR(3)     TOTAL NET PAY / NUM EMPLOYEES
*
         MVC   ONETAVG(11),=X'2020206B2021204B2020'
*
         ED    ONETAVG(11),PCALC        CONVERT TO PRINTABLE EBCDIC
         MVI   0(1),C'$'                INSERT A $
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
* STANDARD EXIT LINKAGE WITH RC OF 0
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
* PACKED DECIMAL VARIABLES
*
PFWHPCT  DC    PL4'0'         PACKED FEDERAL WITHHOLDING PERCENTAGE
PSWHPCT  DC    PL4'0'         PACKED STATE WITHHOLDING PERCENTAGE
NUM1     DC    ZL5'3250'
*
PEMPCTR  DC    PL3'0'         PACKED EMPLOYEE COUNTER (MAX 999)
PPAGECTR DC    PL2'0'         PACKED PAGE COUNTER (MAX 999)
*
PEMPID   DC    PL5'0'         PACKED EMPLOYEE ID
PHRPAY   DC    PL3'0'         PACKED EMPLOYEE HOURLY PAY RATE
PHOURS   DC    PL3'0'         PACKED EMPLOYEE HOURS WORKED
PDEDUCT  DC    PL3'0'         PACKED EMPLOYEE DEDUCTION
PBONUS   DC    PL3'0'         PACKED EMPLOYEE BONUS
PEMPGPAY DC    PL6'0'         PACKED CALCULATED EMPLOYEE GROSS PAY
PFEDWITH DC    PL6'0'         PACKED CALCULATED FEDERAL WITHHOLDING
PSTWITH  DC    PL6'0'         PACKED CALCULATED STATE WITHOLDING
PEMPNPAY DC    PL6'0'         PACKED CALCULATED EMPLOYEE NET PAY
*
PCALC    DC    PL10'0'        USED TO CALCULATE WITHHOLDING AND AVGS
*
PTGRPAY  DC    PL7'0'         PACKED TOTAL GROSS EMPLOYEE PAY
PTFWITH  DC    PL7'0'         PACKED TOTAL WITHHOLDING
PTSWITH  DC    PL7'0'
PTNETPAY DC    PL7'0'         PACKED TOTAL NET EMPLOYEE PAY
*
RECORD   DS    0H
IEMPID   DS    ZL8            PACKS INTO 5 BYTES (8/2 = 4 + 1 = 5)
IHRPAY   DS    ZL5            PACKS INTO 3 BYTES (5/2 = 2 + 1 = 3)
IHOURS   DS    ZL5            PACKS INTO 3 BYTES
IDEDUCT  DS    ZL5            PACKS INTO 3 BYTES
IBONUS   DS    ZL5            PACKS INTO 3 BYTES
IEMPNME  DS    CL25
         DS    CL27
*
* OUTPUT FOR EACH EMPLOYEE DETAILS
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
         END   PAYROLL2       END PROGRAM
*
/*
//*
//FT05F001 DD DSN=KC02322.CSCI360.ASNDATA(DATA9),DISP=SHR
//*
//FT06F001 DD SYSOUT=*
//
