//******** JOB ,'K. GANDHI',MSGCLASS=H
//JSTEP01  EXEC PGM=ASSIST
//STEPLIB  DD DSN=KC00NIU.ASSIST.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
*****************************************************************
*                                                               *
*  CSCI 360-PE1           ASSIGNMENT 1           SPRING 2023    *
*                                                               *
*  DEVELOPER NAME: KUSH GANDHI                                  *
*        DUE DATE: 01/27/2023                                   *
*                                                               *
*  Replace nnn in KC03nnn in the first line above with your     *
*  KC-ID assigned to you by your instructor. DO NOT LEAVE OUT   *
*  THE CAPITAL LETTER A AT THE END OF YOUR KC-ID! Then, put     *
*  your first initial, a period and your last name in all       *
*  capital letters in the first line where it says 'your name   *
*  here'.                                                       *
*                                                               *
*  Change CSCI 360-n to reflect your section number. Change     *
*  current semester to reflect the current semester like        *
*  SUMMER 2022. Fill in your name as the developer and enter    *
*  the assignment's due date.                                   *
*                                                               *
*  Note that this documentation box does not go past column 65! *
*                                                               *
*****************************************************************
*
MAIN     CSECT
         USING MAIN,15     ESTABLISH ADDRESSABILITY ON REG 15
         LA    4,13        LOAD 13 INTO REG 4
         LA    8,6         LOAD 6 INTO REG 8
         AR    4,8         ADD REG 8'S CONTENTS TO REG 4'S
         XDUMP ,           DUMP CONTENTS OF ALL 16 REGS
         BCR   B'1111',14  UNCONDITIONAL RETURN TO CALLER (OS)
*
         LTORG             LITERAL ORGANIZATION
*
         END   MAIN
/*
//
