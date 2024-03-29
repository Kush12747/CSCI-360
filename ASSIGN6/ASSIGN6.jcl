//XXXXXXXXX JOB ,'K.GANDHI',MSGCLASS=H
//JSTEP01  EXEC PGM=ASSIST
//STEPLIB  DD DSN=KC00NIU.ASSIST.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
*****************************************************************
*                                                               *
*  CSCI 360-PE1            ASSIGNMENT 6              SPRING 23  *
*                  MULTIPLICATION & DIVISION                    *
*                                                               *
*  DEVELOPER NAME: KUSH GANDHI                                  *
*        DUE DATE: 3/24/23                                      *
*                                                               *
*  PROGRAM NAME: PAYROLL CALCULATION                            *
*                                                               *
*  FUNCTION: CALCULATE EMPLOYEE STATE, FEDERAL, AND NET PAY     *
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
         AR    13,3           ADD GROSS PAY TO REG 13 FOR TOTALS
*
         LR    5,3            SAVE GROSS PAY INTO REG 5
         LR    6,5            SAVE GROSS PAY INTO REG 6 FOR NET PAY
*
         MR    2,10           GROSS PAY * FED WITHOLDING PERCENT
         D     2,=F'100'      DIVIDE BY 100
         XDECO 3,OFEDWH       DISPLAY FEDERAL WITHOLDING
         AR    7,3            ADD TO FEDERAL TOTAL
*
         MR    4,11           GROSS PAY * STATE WITHOLDING
         D     4,=F'100'      DIVIDE BY 100
         XDECO 5,OSTWH        DISPLAY STATE WITHOLDING
         AR    8,5            ADD TO STATE TOTAL
*
         SR    6,5            GROSS PAY - STATE WITHOLDING
         SR    6,3            GROSS PAY - FEDERAL WITHOLDING
         XDECO 6,ONETPAY      DISPLAY NET PAY
         AR    9,6            ADD TO NET PAY TOTAL
*
         XPRNT DETAIL,133     PRINT THE EMPLOYEE DETAIL RECORD
*
         XREAD RECORD,80      READ NEXT EMPLOYEE RECORD
*
         BC    B'1111',LOOP1  BRANCH TO TOP OF LOOP1 TO CHECK EOF
*
ENDLOOP1 DS    0H
*
         XDECO 7,FEDLBL       DISPLAYS FEDERAL TOTAL
         XDECO 8,STLBL        DISPLAYS STATE TOTAL
         XPRNT DETAIL2,133    PRINTS THE EMPLOYEE DETAIL RECORD
*
         XDECO 9,NTOT         DISPLAYS NET PAY TOTAL
         XDECO 13,GPTOT       DISPLAYS GROSS PAY TOTAL
         XPRNT DETAIL3,133    PRINTS THE EMPLOYEE DETAIL RECORD
*
         BCR   B'1111',14     UNCONDITIONAL RETURN TO CALLER
*
         LTORG                LITERAL ORGANIZATION
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
GPAY     DS    F      STORAGE FOR GROSS PAY
*
* OUTPUT FOR FEDERAL AND STATE TOTALS
*
DETAIL2  DC    C'0'   CARRIAGE CONTROL CHARACTER
         DC    C'TOTAL FEDERL WITHOLDING: '
FEDLBL   DS    CL12   OUTPUT FIELD FOR XDECO OF FEDERAL TOTAL
         DC    4C' '  SPACES
         DC    C'TOTAL STATE WITHOLDING: '
STLBL    DS    CL12   OUTPUT FIELD FOR XDECO OF STATE TOTAL
         DC    117C' ' SPACES
*
* OUTPUT FOR NET PAY AND GROSS PAY TOTAL
*
DETAIL3  DC    C'0'
         DC    C'TOTAL NET PAY: '
NTOT     DS    CL12   OUTPUT FIELD FOR XDECO OF NET PAY TOTAL
         DC    4C' '  SPACES
         DC    C'TOTAL GROSS PAY: '
GPTOT    DS    CL12   OUTPUT FIELD FOR XDECO OF GROSS PAY TOTAL
         DC    117C' ' SPACES
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
