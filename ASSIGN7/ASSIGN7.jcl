//XXXXXXXXX JOB ,'K.GANDHI',MSGCLASS=H
//JSTEP01  EXEC PGM=ASSIST
//STEPLIB  DD DSN=KC00NIU.ASSIST.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
*****************************************************************
*                                                               *
*  CSCI 360-PE1            ASSIGNMENT 7              SPRING 23  *
*                       REPORT FINISHING                        *
*                                                               *
*  DEVELOPER NAME: KUSH GANDHI                                  *
*        DUE DATE: 3/31/23                                      *
*                                                               *
*  PROGRAM NAME: REPORT FINISHING                               *
*                                                               *
*  FUNCTION: PRODUCING A PROPER REPORT FINISHING                *
*****************************************************************
*
ASSIGN6  CSECT
         USING ASSIGN6,15     ESTABLISH ADDRESSABILITY ON REG 15
*
         LA    10,16          GET FED WITHHOLDING PERCENT INTO R10
         LA    11,4           GET STATE WITHHOLDING PERCENT INTO R11
         SR    7,7            ZEROED OUT FOR FEDERAL TOTAL
         SR    8,8            ZEROED OUT FOR STATE TOTAL
         SR    9,9            ZEROED OUT FOR NET PAY TOTAL
         SR    6,6            ZEROED OUT FOR GROSS TOTAL
*
         LA    7,0            PAGE COUNTER
         LA    8,99           LINE COUNTER
*
         XREAD RECORD,80      GET FIRST EMPLOYEE RECORD
*
LOOP1    BC   B'0100',ENDLOOP1 BRANCH TO ENDLOOP1 IF EOF
*
*
*  HERE IS WHERE THE BODY OF YOUR LOOP WILL BE CODED
*
*
* GETS EMPLOYEE ID AND NAME THEN PUTS IT INTO RECORD BY READING COL NUM
         MVC   OEMPNME(25),RECORD+0
         MVC   OEMPID(5),RECORD+25
*
         XDECI 2,RECORD+31    GET PAY RATE
         XDECO 2,OPAYRATE     DISPLAY PAY RATE
         XDECI 3,0(1)         GET HOURS
         XDECO 3,OHOURS       DISPLAY HOURS
*
         MR    2,2            PAY RATE * HOURS (PRODUCT IN R3)
*
         XDECI 2,0(1)         GET DEDUCTIONS
         SR    3,2            SUBTRACT FROM PAY
         XDECI 2,0(1)         GET BONUS
         AR    3,2            ADD TO PAY
*
         XDECO 3,OGROSS       DISPLAY GROSS PAY AMT
         ST    3,GROSSPAY     STORE GROSS PAY IN STORAGE
         AR    13,3           ADD GROSS PAY TO REG 13 FOR TOTALS
*
         LR    5,3            SAVE GROSS PAY INTO REG 5
         LR    6,5            SAVE GROSS PAY INTO REG 6 FOR NET PAY
*
         MR    2,10           GROSS PAY * FED WITHOLDING PERCENT
         D     2,=F'100'      DIVIDE BY 100
         XDECO 3,OFEDWH       DISPLAY FEDERAL WITHOLDING
         AR    7,3            ADD TO FEDERAL TOTAL
         ST    7,FEDWH        STORE FEDERAL WH. TOTAL IN SOTRAGE
*
         MR    4,11           GROSS PAY * STATE WITHOLDING
         D     4,=F'100'      DIVIDE BY 100
         XDECO 5,OSTWH        DISPLAY STATE WITHOLDING
         AR    8,5            ADD TO STATE TOTAL
         ST    8,STATEWH
*
         SR    6,5            GROSS PAY - STATE WITHOLDING
         SR    6,3            GROSS PAY - FEDERAL WITHOLDING
         XDECO 6,ONETPAY      DISPLAY NET PAY
         AR    9,6            ADD TO NET PAY TOTAL
*
         C     8,=F'17'       COMPARE TO SEE IF IT'S TIME TO PRINT
         LA    7,1(,7)        ADD 1 TO PAGE COUNTER
         XDECO 7,PAGE         DISPLAY PAGE NUMBER
         XPRNT HEADER1,133    PRINT FIRST HEADING
         SR    8,8            RESET THE COUNTER TO 0
         XPRNT DETAIL,133     PRINT THE EMPLOYEE DETAIL RECORD
         LA    8,1(,8)        ADD 1
*
         XREAD RECORD,80      READ NEXT EMPLOYEE RECORD
*
         BC    B'1111',LOOP1  BRANCH TO TOP OF LOOP1 TO CHECK EOF
*
ENDLOOP1 DS    0H
*
         XDECO 7,FEDLBL       DISPLAYS FEDERAL TOTAL
         XDECO 8,STLBL        DISPLAYS STATE TOTAL
         XDECO 9,NTOT         DISPLAYS NET PAY TOTAL
         XDECO 13,GPTOT       DISPLAYS GROSS PAY TOTAL
         XPRNT TOTALS1,133
         BCR   B'1111',14     UNCONDITIONAL RETURN TO CALLER
*
         LTORG                LITERAL ORGANIZATION
*
* STORAGE FOR TOTALS TO FREE UP REGISTERS
*
GROSSPAY DS    F
NETPAY   DS    F
STATEWH  DS    F
FEDWH    DS    F
*
* THE FOLLOWING 16 LINES ARE THE DEFINITION FOR THE OUTPUT DETAIL
*   LINE FOR EACH EMPLOYEE FOR THE REPORT YOU ARE CREATING.  IT
*   IS EXACTLY 133 BYTES LONG.  EACH OUTPUT FIELD IS SEPARATED
*   BY 4 OR 5 BYTES OF EBCDIC SPACES, i.e., X'40'.
*
DETAIL   DC    C'0'   CARRIAGE CONTROL CHARACTER
OEMPNME  DS    CL25   OUTPUT FIELD FOR EMPLOYEE NAME
         DC    4C' '  SPACES
OEMPID   DS    CL5    OUTPUT FIELD FOR EMPLOYEE ID
         DC    4C' '  SPACES
OPAYRATE DS    CL12   OUTPUT FIELD FOR XDECO OF EMPLOYEE PAY RATE
         DC    4C' '  SPACES
OHOURS   DS    CL12   OUTPUT FIELD FOR XDECO OF EMPLOYEE HOURS WORKED
         DC    4C' '  SPACES
OGROSS   DS    CL12   OUTPUT FIELD FOR XDECO OF EMPLOYEE GROSS PAY AMT
         DC    4C' '  SPACES
OFEDWH   DS    CL12   OUTPUT FIELD FOR XDECO OF FEDERAL WITHHOLDING AMT
         DC    5C' '  SPACES
OSTWH    DS    CL12   OUTPUT FIELD FOR XDECO OF STATE WITHHOLDING AMT
         DC    5C' '  SPACES
ONETPAY  DS    CL12   OUTPUT FIELD FOR XDECO OF EMPLOYEE NET PAY AMT
*
* OUTPUT FOR THE TOTALS LINE
*
TOTALS1  DC    C'0'
         DC    C'NUMBER OF EMPLOYEES: 24'
         DC    30C' '
         DC    C'TOTALS: '
         DC    5C' '
GPTOT    DS    CL4
         DC    5C' '
FEDLBL   DS    CL4
         DS    5C' '
STLBL    DS    CL4
         DC    5C' '
NTOT     DS    CL4
*
* OUTPUT FOR FIRST HEADING WITH  TITLE AND PAGE NUMBER
*
HEADER1  DC    C'0'
         DC    50C' '
         DC    C'SEMI-MONTHLY PAYROLL REPORT'
         DC    40C' '
         DC    C'PAGE: '
PAGE     DS    CL2
         DC    91C' '
*
* OUTPUT FOR COLUMN NAMES
*
COLHDR1  DC    C'0'
         DC    80C' '
         DC    C'FEDERAL'
         DC    8C' '
         DC    C'STATE'
         DC    8C' '
         DC    C'DIRECT'
*
* SAME THING FROM ABOVE, COLUMN NAMES
*
COLHDR2  DC    C'0'
         DC    C'EMPLOYEE NAME'
         DC    20C' '
         DC    C'ID'
         DC    4C' '
         DC    C'HOURLY PAY'
         DC    4C' '
         DC    C'HOURS WORKED'
         DC    4C' '
         DC    C'GROSS PAY'
         DC    4C' '
         DC    C'WITHOLDING'
         DC    4C' '
         DC    C'WITHOLDING'
         DC    4C' '
         DC    C'DEPOSIT AMT'
*
* OUTPUT FOR LINES
*
HYPHENS1 DC    C'0'
         DC    C'-------------------------'
         DC    4C' '
         DC    C'-----'
         DC    4C' '
         DC    C'------------'
         DC    4C' '
         DC    C'------------'
         DC    4C' '
         DC    C'------------'
         DC    4C' '
         DC    C'------------'
         DC    4C' '
         DC    C'------------'
         DC    4C' '
         DC    C'------------'
*
*  THE FOLLOWING IS THE BUFFER FOR THE INPUT RECORD
*
RECORD   DS    CL80   BUFFER FOR EMPLOYEE RECORD READ FROM INPUT FILE
*
         END   ASSIGN6
/*
//*
//* THE FOLLOWING IS THE INPUT DATA SET
//*
//FT05F001 DD DSN=KC02322.CSCI360.ASNDATA(DATA6),DISP=SHR
//*
//* THE FOLLOWING IS THE OUTPUT DATA SET
//*
//FT06F001 DD SYSOUT=*
//*
//
