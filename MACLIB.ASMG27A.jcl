//MACLIB JOB CLASS=A,MSGCLASS=X,REGION=4096K
//*
//* DELETE ANY OLD MACLIB
//*
//* STEP01 EXEC PGM=IEFBR14
//* SYSPRINT DD  SYSOUT=*
//* DD1      DD DISP=(OLD,DELETE,DELETE),DSN=CBT593.ASMG27A.MACLIB
//*
//* ALLOCATE AND CATALOG THE NEW MACLIB
//*
//STEP02 EXEC PGM=IEFBR14
//SYSPRINT DD  SYSOUT=*
//DD1      DD DISP=(NEW,CATLG,DELETE),
//             DSN=CBT593.ASMG27A.MACLIB,
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=12800),
//             SPACE=(CYL,(3,1,7),RLSE),
//             UNIT=3330,VOL=SER=PSTCAD
//*
//* LOAD THE NEW MACLIB
//*
//STEP03 EXEC PGM=IEBUPDTE
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD DISP=OLD,DSN=CBT593.ASMG27A.MACLIB
//SYSUT2   DD DISP=OLD,DSN=CBT593.ASMG27A.MACLIB
//SYSIN DD *
./ ADD NAME=ASMGSET
*                  COPIED BY NAME 'ASMGSET'                             00020026
*                  THIS BLOCK OF CODING SETS THE GLOBAL SWITCHES        00040026
         GBLA      &MAXUT      MAXIMUM NUMBER OF UTILITIES              00060026
         GBLA      &MOVENUM    NUMBER OF BYTES MOVED AT ONE TIME        00080026
         GBLA      &MAXMAC      MAXIMUM NUMBER OF MACROS IN SYSLIB DIR  00100026
         GBLA      &UT1BUFF,&UT3BUFF,&UTBUFF                            00120026
         GBLA      &WORK                                                00140027
*.* DAN SKOMSKY 09/22/2007 USE EQUATE TO DEFINE SEQUENCE LEN  DS092207  00142025
         GBLA      &UPSEQLN    USED IN ASMGUP, LENGTH OF SEQ  DS092207  00144027
         GBLB      &DEBUG,&NOTBUG,&STAT,&NOTSTAT                        00160026
         GBLB      &HRDCOPY    TO PRODUCE A HARD COPY WITH UPDATE       00180026
         GBLB      &LINKCHK    CHECK CORE MANAGEMENT LINKED LISTS       00200026
         GBLB      &SERUSE     GENERATE SERIALLY REUSABLE CODE          00220026
         GBLB      &SYSGO      SET 1 TO TRY SYSGO IF SYSLIN WON'T OPEN  00240026
         GBLB      &SYM370     SET TO 1 FOR SYSTEM 370 INSTS            00260026
         GBLB      &RPQ67      SET TO 1 FOR MODEL 67 RPQ INSTS          00280026
         SPACE     2                                                    00300026
*        &MAXUT USED IN ASMGBUFF ONLY                                   00320026
&MAXUT   SETA      3                   MAXIMUM NUMBER OF UTILITIES      00340026
         SPACE                                                          00360026
*        &MOVENUM USED IN ASMGBUFF ONLY                                 00380026
&MOVENUM SETA      4*14*10             NUMBER OF BYTES MOVED AT A SHOT  00400026
*                    IN MOVE ROUTINE.  MUST BE A MULTIPLE OF 14*4.      00420026
         SPACE                                                          00440026
*        &LINKCHK USED ONLY IN ASMGBUFF                                 00460026
&LINKCHK SETB      0                   DEBUG SETTING = 1.               00480026
         SPACE                                                          00500026
*        &HRDCOPY USED ONLY IN ASMGUP                                   00520026
&HRDCOPY SETB      1                   SET TO 1 FOR HARD SYSPUNCH COPY  00540027
         SPACE                                                          00560026
*        &SERUSE USED IN ASMGBUFF ONLY                                  00580026
&SERUSE  SETB      0                   =0 SAVES A BIT OF CORE           00600026
         SPACE                                                          00620026
*        &MAXMAC USED IN ASMGF1 ONLY                                    00640026
&MAXMAC  SETA      1000                DEFAULT NUMBER OF IN CORE MACROS 00660027
         SPACE                                                          00680026
*        &DEBUG AND &STAT USED IN MANY ROUTINES INDEPENDENTLY           00700026
&DEBUG   SETB      0                   SET 1 FOR DEBUG VERSION          00720026
&STAT    SETB      0                   SET 1 FOR STATISTICS VERSION     00740026
&NOTBUG  SETB      ( NOT &DEBUG)                                        00760026
&NOTSTAT SETB      ( NOT &STAT)                                         00780026
         SPACE                                                          00800026
*        &SYSGO USED IN ASMGF1 ONLY                                     00820026
&SYSGO   SETB      1                   TRY SYSGO IF SYSLIN FAILS        00840026
         SPACE                                                          00860026
*        &UT1BUFF, &UT3BUFF, &UTBUFF USED IN ASMGF3 ONLY                00880026
&UT1BUFF SETA      2                   NUMBER OF UT1 BUFFERS, MIN=2     00900026
&UT3BUFF SETA      1                   NUMBER OF UT3 BUFFERS, MIN=1     00920026
&UTBUFF  SETA      &UT1BUFF+&UT3BUFF   TOTAL INPUT BUFFERS              00940026
         SPACE                                                          00960026
*        &WORK USED IN ASMGBUFF AND ASMGF3 ONLY                         00980026
         SPACE                                                          01000026
*                                                                       01020027
*.* DAN SKOMSKY 09/22/2007 USE EQUATE TO DEFINE SEQUENCE LEN  DS092207  01022025
&UPSEQLN SETA      6           USED IN ASMGUP, LENGTH OF SEQ  DS092207  01024027
*                                                                       01040027
*                                                                       01060027
         SPACE     1                                                    01080026
*        &SYM370 USED IN ASMGF7X AND ASMGF8M                            01100026
&SYM370  SETB      1                   INCLUDE 370 INSTRUCTIONS         01120026
         SPACE     1                                                    01140026
*        &RPQ67 USED IN ASMGF7X AND ASMGF8M                             01160026
&RPQ67   SETB      0                   DON'T INCLUDE MOD 67 INSTS       01180026
         SPACE     1                                                    01200026
*                  END OF COPIED CODE                                   01220026
./ ADD NAME=APARM
         MACRO     --  PARM  --  USED BY ASMGF1                         00020027
&LABEL   APARM     &KEYW,&MIN=,&ALEN=,&FORM=L,&ADDR=,&ON=,&OFF=,       X00040027
               &SUFFIX=NO,&TYPE=                                        00060027
         LCLA      &LENS,&I,&J,&T                                       00080027
         LCLB      &S,&SUF,&B(48)                                       00100027
         LCLC      &Q,&X(12)                                            00120027
         AIF       (K'&KEYW LT 1 OR K'&KEYW GT 12).MNOTE1               00140027
&LENS    SETA      K'&KEYW                                              00160027
&Q       SETC      ''''                                                 00180027
         AIF       ('&KEYW'(1,1) NE '''').SKIP00                        00200027
&LENS    SETA      K'&KEYW-2                                            00220027
&Q       SETC      ''                                                   00240027
.SKIP00  ANOP                                                           00260027
         AIF       (K'&MIN LT 1).MNOTE2                                 00280027
         AIF       ('&SUFFIX' NE 'NO' AND '&SUFFIX' NE 'YES').MNOTE3    00300027
         AIF       ('&FORM' NE 'L' AND '&FORM' NE 'S').MNOTE4           00320027
         AIF       ('&ADDR' NE '' AND ('&ON' NE '' OR '&OFF' NE '')).MNX00340027
               OTE5                                                     00360027
         AIF       (K'&ALEN LT 1).SKPLEN1                               00380027
         AIF       (&ALEN LT &MIN).MNOTE8                               00400027
&LENS    SETA      &ALEN                                                00420027
.SKPLEN1 ANOP                                                           00440027
&LENS    SETA      (&MIN-1)*16+&LENS-1                                  00460027
&S       SETB      ('&FORM' EQ 'S')                                     00480027
&SUF     SETB      ('&SUFFIX' EQ 'YES')                                 00500027
&T       SETA      &S*128+&SUF*64                                       00520027
         AIF       ('&ADDR' NE '').KWTYPE                               00540027
         AIF       ('&ON' EQ '' AND '&OFF' EQ '').MNOTE6                00560027
         AIF       ('&OFF' NE '').TRYOFF                                00580027
&I       SETA      N'&ON                                                00600027
.ONLOOP  ANOP                                                           00620027
&J       SETA      L'&ON(&I)                                            00640027
         AIF       (&J LT 1 OR &J GT 48).MNOTE7                         00660027
&B(&J)   SETB      (1)                                                  00680027
&I       SETA      &I-1                                                 00700027
         AIF       (&I GT 0).ONLOOP                                     00720027
         AGO       .SETBIT                                              00740027
.TRYOFF  AIF       ('&ON' NE '').TRYONF                                 00760027
&I       SETA      N'&OFF                                               00780027
.OFFLOOP ANOP                                                           00800027
&J       SETA      L'&OFF(&I)                                           00820027
         AIF       (&J LT 1 OR &J GT 48).MNOTE7                         00840027
&B(&J)   SETB      (1)                                                  00860027
&I       SETA      &I-1                                                 00880027
         AIF       (&I GT 0).OFFLOOP                                    00900027
&I       SETA      48                                                   00920027
.OFFREV  ANOP                                                           00940027
&B(&I)   SETB      (NOT &B(&I))                                         00960027
&I       SETA      &I-1                                                 00980027
         AIF       (&I GT 0).OFFREV                                     01000027
&T       SETA      &T+1                                                 01020027
         AGO       .SETBIT                                              01040027
.TRYONF  ANOP                                                           01060027
&I       SETA      N'&ON                                                01080027
.ONFLOP1 ANOP                                                           01100027
&J       SETA      L'&ON(&I)                                            01120027
         AIF       (&J LT 17 OR &J GT 48).MNOTE7                        01140027
&B(&J)   SETB      (1)                                                  01160027
&I       SETA      &I-1                                                 01180027
         AIF       (&I GT 0).ONFLOP1                                    01200027
&I       SETA      N'&OFF                                               01220027
.ONFLOP2 ANOP                                                           01240027
&J       SETA      L'&OFF(&I)                                           01260027
         AIF       (&J LT 1 OR &J GT 16).MNOTE7                         01280027
&B(&J)   SETB      (1)                                                  01300027
&I       SETA      &I-1                                                 01320027
         AIF       (&I GT 0).ONFLOP2                                    01340027
&I       SETA      16                                                   01360027
.ONFREV  ANOP                                                           01380027
&B(&I)   SETB      (NOT &B(&I))                                         01400027
&I       SETA      &I-1                                                 01420027
         AIF       (&I GT 0).ONFREV                                     01440027
&T       SETA      &T+2                                                 01460027
         AGO       .SETBIT                                              01480027
.KWTYPE  AIF       (K'&TYPE LT 1).RKEWT                                 01500027
&T       SETA      &T+&TYPE                                             01520027
         AGO       .SETBIT                                              01540027
.RKEWT   ANOP                                                           01560027
&T       SETA      &T+3                                                 01580027
&LABEL   DC        CL12&Q&KEYW&Q                                        01600027
         DC        AL1(&LENS,&T),AL2(&ADDR.-TAB3PARM),4X'00'            01620027
         MEXIT                                                          01640027
.SETBIT  ANOP                                                           01660027
&I       SETA      4                                                    01680027
.LOOPX   ANOP                                                           01700027
&J       SETA      &B(&I-3)*8+&B(&I-2)*4+&B(&I-1)*2+&B(&I)+1            01720027
&X(&I/4) SETC      '0123456789ABCDEF'(&J,1)                             01740027
&I       SETA      &I+4                                                 01760027
         AIF       (&I LE 48).LOOPX                                     01780027
&LABEL   DC        CL12&Q&KEYW&Q                                        01800027
         DC        AL1(&LENS,&T),XL6'&X(1)&X(2)&X(3)&X(4)&X(5)&X(6)&X(7X01820027
               )&X(8)&X(9)&X(10)&X(11)&X(12).'                          01840027
         MEXIT                                                          01860027
.MNOTE1  MNOTE     12,'PARM -- PARAMETER &KEYW INVALID'                 01880027
         MEXIT                                                          01900027
.MNOTE2  MNOTE     12,'PARM -- MIN= PARM MISSING'                       01920027
.MNOTE3  MNOTE     12,'PARM -- SUFFIX= &SUFFIX INVALID'                 01940027
         MEXIT                                                          01960027
.MNOTE4  MNOTE     12',PARM -- FORM= &FORM INVALID'                     01980027
         MEXIT                                                          02000027
.MNOTE5  MNOTE     12,'PARM -- ADDR= &ADDR INVALID'                     02020027
         MEXIT                                                          02040027
.MNOTE6  MNOTE     12,'PARM -- MISSING ON= AND OFF='                    02060027
         MEXIT                                                          02080027
.MNOTE7  MNOTE     12,'PARM -- ON/OFF PARM &J OUT OF RANGE'             02100027
         MEXIT                                                          02120027
.MNOTE8  MNOTE     12,'PARM -- ALEN= &ALEN LT MIN= &MIN'                02140027
         MEND                                                           02160027
./ ADD NAME=COMMENCE
         MACRO                                                          00010026
&LABEL   COMMENCE  &EOF=                                                00020026
&LABEL   STM       14,12,12(13)        SAVE CALLER'S REGISTERS          00030026
         BALR      12,0                ESTABLISH THE BASE               00040026
         USING     *,12                AND TELL THE ASSEMBLER           00050026
         LR        15,13               REMEMBER CALLER'S SAVE AREA      00060026
         LA        13,ZZZZSAVE         WHAT IS MY SAVE AREA             00070026
         ST        13,8(15)            TELL THE CALLER                  00080026
         ST        15,ZZZZSAVE+4       SAVE CALLER'S SAVE AREA          00090026
         ST        1,ZZZZSAVE          SAVE POINTER TO ASMG DCB LIST    00100026
         AIF       (K'&EOF EQ 0).NOEOF WAS EOF SPECIFIED .Q             00110026
         L         1,16(0,1)           POINT TO SYSIN DCB               00120026
         MVC       33(3,1),=AL3(&EOF)  MODIFY THE EODAD                 00130026
.NOEOF   ANOP                                                           00140026
         LA        1,50                50 LINES PER PAGE FOR USER       00150026
         ST        1,ZZZLINES          AND REMEMBER IT                  00160026
         B         ZZAROUND            BRANCH AROUND DATA               00170026
ZZZDUMMY DS        D                   DUMMY CONVERT AREA               00180026
ZZZZTEMP DS        4F                  TEMPORARY SAVE AREA              00190026
ZZZZSAVE DS        18F                 MY SAVE AREA                     00200026
ZZZLINES DS        F                   LINES PER USER OUTPUT PAGE       00210026
ZZZINPUT DS        CL80                CARD INPUT AREA                  00220026
ZZOUTPUT DC        CL133' '            PRINTER OUTPUT AREA              00230026
ZZAROUND DS        0H                                                   00240026
         MEND                                                           00250026
./ ADD NAME=COMMON    UPDATE DONE
         MACRO                                                          00020027
         COMMON    &PHASE=                                              00040027
*********************************************************************** 00060027
*        THE FOLLOWING DESCRIBES THE COMMON AREA                        00080027
*        SET UP BY F1 AND USED BY F2 AND F2A.                           00100027
*                                                                       00120027
         AIF       ('&PHASE' NE 'ASMGF1').TRYF2                         00140027
COMMON   CSECT                                                          00160027
*        THE PERMANENT HASH TABLE 'PHASHT' AND CONSTANTS 'PNDX' AND     00180027
*        'ENDOPC' ARE SET FROM DATA IN THE DYNAMICALLY LOADED           00200027
*        ASMGISXX MODULE.                                               00220027
*                                                                       00240027
         AGO       .COMSTRT                                             00260027
.TRYF2   AIF       ('&PHASE' NE 'ASMGF2').TRYF2A                        00280027
COMMON   CSECT                                                          00300027
         AGO       .COMSTRT                                             00320027
.TRYF2A  ANOP                                                           00340027
         AIF       ('&PHASE' NE 'ASMGF2A').PHERROR                      00360027
COMMON   DSECT                                                          00380027
         AGO       .COMSTRT                                             00400027
.PHERROR MNOTE     16,'COMMON  --  PHASE &PHASE NOT RECOGNIZED.'        00420027
         MEXIT                                                          00440027
.COMSTRT ANOP                                                           00460027
*                                                                       00480027
*        CONSTANTS FROM HERE TO 'COMEND' ARE INITIALIZED BY PHASE F1    00500027
*        AND USED BY PHASES F1-F2.  REGISTER CM WILL POINT TO COMMON.   00520027
*                                                                       00540027
         DC    18F'0' .                O/S SAVE AREA                    00560027
PATCH    DC    10S(*)                  PATCH AREA                       00580027
*                                                                       00600027
*        START OF F1/F2 SHARED COMMON AREA                              00620027
*                                                                       00640027
F1F2STRT DS        0H                                                   00660027
GDP      DC    A(0)                     BEGINNING OF DICTIONARY         00680027
DEND     DC    A(0)                     END OF DICTIONARY               00700027
SSEG     DC    A(0)                     UT1 BLOCKSIZE                   00720027
*                                                                       00740027
*                                                                       00760027
RETURN   DC    F'0'                     RETURN ADDR TO PHASE 'ASM'      00780027
*                                                                       00800027
*                                                                       00820027
*                                                                       00840027
ADCBLB   DC        A(0)                ADDRESS OF SYSLIB DCB            00860027
ADCBIN   DC        A(0)                ADDRESS OF SYSIN DCB             00880027
*                                                                       00900027
INPLEN   DC        F'0'                 UT3 BLOCKSIZE                   00920027
OBSIZ    DC        F'0'                 SIZE OF OUTPUT 1 AREA           00940027
BUFFENT  DC        A(0)                ADDRESS OF BUFFERING ROUTINE     00960027
ADLIST1  DC        A(0)                ADDRESS OF LIST1 IN ASM          00980027
*                                                                       01000027
ASMSAVE  DC        F'0'                ADDRESS OF SAVE AREA IN ASM      01020027
*                                                                       01040027
*        STORAGE AREAS USED BY THE PARM SCAN ROUTINE                    01060027
*                                                                       01080027
TAB3PARM DC        0F'0'               DIGIT PARM TABLE                 01100027
TAB3DEF  EQU       0                        VALUE OF PARM, DEFAULT      01120027
TAB3MIN  EQU       TAB3DEF+4                MINIMUM VALUE ACCEPTABLE    01140027
TAB3MAX  EQU       TAB3MIN+4                MAXIMUM VALUE ACCEPTABLE    01160027
*                                                                       01180027
EXTMWD   DC        A(5,1,9999)         EXECUTION TIME IN SECONDS  V7A32 01200028
LCTBYT   DC        A(55,0,254)         DEFAULT LINE COUNT               01220027
UTBUFF   DC        A(3,0,3)            UTBUFF= VALUE                    01240027
LSETCBYT DC        A(8,1,255)          LSETC= VALUE                     01260027
ISBIN    DC        A(1,0,99)           INSTSET IN BINARY                01280027
SPACEPOS DC        A(0-1,MINIMUM,0-1)  SPACE=VALUE                      01300027
SPACENEG DC        A(*-*,8,0-1)        SPACE=MAX- VALUE                 01320027
COLCOUNT DC        A(1,0,3)            DEFAULT COL= VALUE               01340027
UPCOND   DC        A(12,1,20)          DEFAULT MAX UPDATE CONDITION     01360027
CALIGN   DC        A(0,0,255)          CALIGN= VALUE                    01380027
*                                                                       01400027
PARBYT0  DC        AL1(0)         *PARBYT0                              01420027
*                  BIT                 ON             OFF               01440027
*        EQU       X'80'               RESERVED       --                01460027
*        EQU       X'40'               RESERVED       --                01480027
*        EQU       X'20'               RESERVED       --                01500027
*        EQU       X'10'               RESERVED       --                01520027
*        EQU       X'08'               RESERVED       --                01540027
*        EQU       X'04'               RESERVED       --                01560027
*        EQU       X'02'               RESERVED       --                01580027
*        EQU       X'01'               RESERVED       --                01600027
*                                                                       01620027
PARBYT1  DC        AL1(EXTEN+ALGN)          *PARBYT1                    01640027
*                  BIT                 ON             OFF               01660027
FUPLIST  EQU       X'80'               FULLUPLIST     UPLIST/NOUPLIST   01680027
DOS      EQU       X'40'               DOS            OS                01700027
ESD      EQU       X'20'               ESD            NOESD             01720027
BATCH    EQU       X'10'               BATCH          NOBATCH           01740027
FLIST    EQU       X'08'               FULLLIST       LIST/NOLIST       01760027
EXTEN    EQU       X'04'               EXTEN          NOEXTEN           01780027
FXREF    EQU       X'02'               FULLXREF       XREF/NOXREF       01800027
ALGN     EQU       X'01'               ALGN           NOALGN            01820027
*                                                                       01840027
PARBYT   DC        AL1(LOAD+LIST+UPLIST+XREF)  *PARBYT                  01860027
*                  BIT                 ON             OFF               01880027
DECK     EQU       X'80'               DECK           NODECK            01900027
LOAD     EQU       X'40'               LOAD           NOLOAD            01920027
RENT     EQU       X'20'               RENT           NORENT            01940027
LIST     EQU       X'10'               LIST/FULLLIST  NOLIST            01960027
RLD      EQU       X'08'               RLD            NORLD             01980027
UPLIST   EQU       X'04'               UPLIST/FUPLIST NOUPLIST          02000027
XREF     EQU       X'02'               XREF/FULLXREF  NOXREF            02020027
TEST     EQU       X'01'               TEST           NOTEST            02040027
*                                                                       02060027
PARBYT2  DC        AL1(LREF+NUM+STMT+SEQPARM)  *PARBYT2           V7A43 02080028
*                  BIT                 ON             OFF               02100027
LREF     EQU       X'80'               LREF           NOLREF            02120027
UPDATE   EQU       X'40'               UPDATE         NOUPDATE          02140027
EXECUTE  EQU       X'20'               EXECUTE        NOEXECUTE         02160027
SPACEM   EQU       X'10'               SPACE=MAX      SPACE=N/=MAX-N    02180027
TERM     EQU       X'08'               TERM           NOTERM            02200027
NUM      EQU       X'04'               NUM            NONUM             02220027
STMT     EQU       X'02'               STMT           NOSTMT            02240027
SEQPARM  EQU       X'01'               SEQ            NOSEQ       V7A43 02260028
*                                                                       02280027
PARBYT3  DC        AL1(UMAP+PRT+YFLAG)      *PARBYT3              V7A44 02300028
*                  BIT                 ON             OFF               02320027
UMAP     EQU       X'80'               UMAP           NOUMAP            02340027
CMS      EQU       X'40'               CMS            NOCMS             02360027
XREFFS   EQU       X'20'               XREF(SHORT)    XREF(FULL)        02380027
PRT      EQU       X'10'               PRINT          NOPRINT           02400027
YFLAG    EQU       X'08'               YFLAG          NOYFLAG     V7A44 02420028
*        EQU       X'04'               RESERVED       --                02440027
*        EQU       X'02'               RESERVED       --                02460027
UNUSED   EQU       X'01'               UNUSED         --                02480027
*                                                                       02500027
PARBYT4  DC        AL1(0)                   *PARBYT4                    02520027
*                  BIT                 ON             OFF               02540027
*        EQU       X'80'               RESERVED       --                02560027
*        EQU       X'40'               RESERVED       --                02580027
*        EQU       X'20'               RESERVED       --                02600027
*        EQU       X'10'               RESERVED       --                02620027
*        EQU       X'08'               RESERVED       --                02640027
*        EQU       X'04'               RESERVED       --                02660027
*        EQU       X'02'               RESERVED       --                02680027
*        EQU       X'01'               RESERVED       --                02700027
         SPACE     2                                                    02720027
         DC        0D'0'                                                02740027
NPTL     EQU       3                      NPTL = NOTE/POINT ENTRY LNTH  02760027
PASHL    EQU       64                  NUMBER OF PERM HASH TBL ENTRIES  02780027
PASHL1   EQU       2*PASHL-2           DEFINES EXPR FOR 8K TAPE ASSEM   02800027
TASHL    EQU       128                 NUMBER OF TRANS HASH TBL ENTRIES 02820027
PHASHT   DC        (PASHL)H'0'          PERM (GLOBAL) HASH TABLE        02840027
*                                         (2 BYTES PER ENTRY)           02860027
THASHT   DC        (TASHL)AL3(0)        TRANSIENT (LOCAL) HASH TABLE    02880027
*                                                                       02900027
         DC        0D'0'                                                02920027
BWBUF1   DC    A(0)                                                     02940027
BWBUF2   DC    A(0)                                                     02960027
BWBLKS   DC        0F'0'                FILE BLOCK ORIGIN               02980027
*        UT1 FILE BLOCK                                                 03000027
         DC    A(0)           BWBUF****POINTER TO BUFFER  (FROM PH 1)   03020027
         DC    AL2(0)         BWBFSZ****BUFFER LENGTH      (FROM PH 1)  03040027
         DC        H'0'       BWRLTH    LENGTH OF LOGICAL RECORD        03060027
         DC        F'0'       BWFNOT    NOTE POINT                      03080027
         DC        H'5'       BWNDEX    CURRENT BUFFER POINTER          03100027
         DC        2X'00'              TO MAKE 1ST BLOCK 16 LONG        03120027
*        UT3 FILE BLOCK                                                 03140027
         DC    A(0)           BWBUFF****                                03160027
         DC    AL2(0)         BWBFSZ****                                03180027
         DC        H'0'       BWRLTH                                    03200027
         DC        F'0'       BWFNOT                                    03220027
         DC        H'5'       BWNDEX                                    03240027
*        FOLLOWING ARE REL LOCATIONS WITHIN THE FILE BLOCKS 'BWBLKS'    03260027
BWBUFF   EQU       0                                                    03280027
BWBFSZ   EQU       4                                                    03300027
BWRLTH   EQU       6                                                    03320027
BWFNOT   EQU       8                                                    03340027
BWNDEX   EQU       12                                                   03360027
*                                                                       03380027
HAFSZ    EQU       512                 HALF BLOCKSIZE                   03400027
BLKSZ    EQU       HAFSZ*2                                              03420027
DCFILE   EQU       4                   DICT FILE NO. (SYSUT2)           03440027
*                                                                       03460027
LBDECBAD DC    2A(0)                    ADDRS OF TWO SYSLIB DECB'S      03480027
LBRDECB  EQU   0                        EVENT CONTROL BLOCK             03500027
LBRDTYPE EQU   LBRDECB+4                TYPE FIELD                      03520027
LBRDLEN  EQU   LBRDTYPE+2               LENGTH                          03540027
LBDCBAD  EQU   LBRDLEN+2                DCB ADDRESS                     03560027
LBAREAAD EQU   LBDCBAD+4                AREA ADDRESS                    03580027
LBRECPW  EQU   LBAREAAD+4               RECORD POINTER WORD             03600027
*        DECB SUFFIX                                                    03620027
LBLIBNOT EQU   LBRECPW+4                NOTED LIBRARY FDAD              03640027
LBLIBNTA EQU   LBLIBNOT+8               LIBRARY BUFFER RECORD OFFSET    03660027
LBLIBEOB EQU   LBLIBNTA+4               LIBRARY BUFFER EOB OFFSET       03680027
LBRDSW   EQU   LBLIBEOB+4               READ SWITCH                     03700027
LBRDRS   EQU   X'02'                         PROCESSING STARTED         03720027
LBRDOT   EQU   X'80'                         OUTSTANDING READS          03740027
LBEND    EQU   LBRDSW+8                 END OF DECB/SUFFIX              03760027
*                                                                       03780027
LIBRDSW  DC    X'00'                    SYSLIB READ SWITCH              03800027
ERRSW1   DC        X'00'               ERROR SWITCH                     03820027
ERRBDPM  EQU       X'01'               BAD ENTRY IN PARM FIELD          03840027
ERRLPRN  EQU       X'02'               MATCHED LEFT PARENS SWITCH       03860027
*                                                                       03880027
ENDOPC   DC        A(*-*)               END OF OPCODES (REL)            03900027
PNDX     DC        A(*-*)               PERM AREA ALLOCATION INDEX.     03920027
HISTRY   DC        3F'0'               HOLDS CRA, PPA, THLDA            03940027
F1F2END  DS        0H                                                   03960027
*                                                                       03980027
*        END OF F1/F2 SHARED COMMON AREA                                04000027
*                                                                       04020027
F1       DC    F'1'                                                     04040027
H0       EQU   F1                                                       04060027
H1       EQU   F1+2                                                     04080027
ONEH     EQU       H1                                                   04100027
F2       DC    F'2'                                                     04120027
H2       EQU   F2+2                                                     04140027
F3       DC    F'3'                                                     04160027
H3       EQU   F3+2                                                     04180027
F4       DC    F'4'                                                     04200027
H4       EQU   F4+2                                                     04220027
F5       DC    F'5'                                                     04240027
H5       EQU   F5+2                                                     04260027
F6       DC    F'6'                                                     04280027
H6       EQU   F6+2                                                     04300027
F7       DC    F'7'                                                     04320027
H7       EQU   F7+2                                                     04340027
F8       DC    F'8'                                                     04360027
H8       EQU   F8+2                                                     04380027
H9       DC    H'9'                                                     04400027
H10      DC    H'10'                                                    04420027
H11      DC    H'11'                                                    04440027
H12      DC    H'12'                                                    04460027
F15      DC    F'15'                                                    04480027
F31      DC    F'31'                                                    04500027
H40      DC    H'40'                                                    04520027
H80      DC    H'80'                                                    04540027
MH187    DC    H'-187'                                                  04560027
H200     DC    H'200'                                                   04580027
F255     DC    F'255'                                                   04600027
H255     EQU   F255+2                                                   04620027
H256     DC    H'256'                                                   04640027
H32767   DC    H'32767'                                                 04660027
F65535   DC    F'65535'                                                 04680027
MH1      EQU   F65535+2                                                 04700027
*                                       ******************************* 04720027
*                                       * TRANSLATE AND TEST TABLE    * 04740027
*                                       ******************************* 04760027
PNCTAB   DC    51X'00'                  (MUST BE ALIGNED TO FULL WORD)  04780027
*        FIRST 39 ENTRIES ARE NORMALLY ZERO, CORRESPONDING TO VALID     04800027
*        ASSEMBLER ALPHA AND NUMERIC CHARACTERS. NEXT 12 ENTRIES        04820027
*        CORRESPOND TO THE SPECIAL CHARACTERS SIGNIFICANT TO THE        04840027
*        ASSEMBLER, AND THIS PART OF THE TABLE MUST BE SET UP BY THE    04860027
*        USER, DEPENDING ON WHAT HE IS SCANNING FOR.                    04880027
*        A NON-ASSEMBLER CHARACTER WILL SPILL OVER INTO THE TRANSLATE   04900027
*        TABLE WHICH FOLLOWS, RETURNING A NON-ZERO VALUE FROM THERE.    04920027
*                                       ******************************* 04940027
*                                       * TRANSLATE TABLE             * 04960027
*                                       ******************************* 04980027
*        THE TRANSLATE TABLE PERMITS TRANSLATION FROM EXTERNAL CODE TO  05000027
*        INTERNAL CODE AND VICE VERSA. THE POSITION CORRESPONDING TO    05020027
*        THE EXTERNAL CODE FOR A SYMBOL CONTAINS ITS INTERNAL CODE,     05040027
*        AND THE POSITION CORRESPONDING TO ITS INTERNAL CODE CONTAINS   05060027
*        ITS EXTERNAL CODE.                                             05080027
*        EXAMPLE- THE EXTERNAL CODE FOR 'A' IS HEX C1 (DECIMAL 193),    05100027
*        AND THE INTERNAL CODE IS HEX 0A (DECIMAL 10). THE 193-D TABLE  05120027
*        ENTRY IS HEX 0A, AND THE 10-TH TABLE ENTRY IS HEX C1           05140027
*        TABLE STARTS WITH THE 0-TH ENTRY                               05160027
*                                                                       05180027
TRSLTB   DC    X'F0F1F2F3F4F5F6F7F8F9C1C2C3C4C5C6'                      05200027
         DC    X'C7C8C9D1D2D3D4D5D6D7D8D9E2E3E4E5'                      05220027
         DC    X'E6E7E8E95B7B7C4E605C616B7E504B4D'                      05240027
         DC    X'5D7D40333435363738393A3B3C3D3E3F'                      05260027
         DC    X'324142434445464748494A2E4C2F274F'                      05280027
         DC    X'2D5152535455565758595A2429305E5F'                      05300027
         DC    X'282A62636465666768696A2B6C6D6E6F'                      05320027
         DC    X'707172737475767778797A2526312C7F'                      05340027
         DC    X'808182838485868788898A8B8C8D8E8F'                      05360027
         DC    X'909192939495969798999A9B9C9D9E9F'                      05380027
         DC    X'A0A1A2A3A4A5A6A7A8A9AAABACADAEAF'                      05400027
         DC    X'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF'                      05420027
         DC    X'C00A0B0C0D0E0F101112CACBCCCDCECF'                      05440027
         DC    X'D0131415161718191A1BDADBDCDDDEDF'                      05460027
         DC    X'E0E11C1D1E1F20212223EAEBECEDEEEF'                      05480027
FINDEC   DC    X'00010203040506070809FAFBFCFDFEFF'                      05500027
FINBLANK EQU       FINDEC-X'32'        TRANSLATE TABLE FOR BLANKS       05520027
         DC        (256-X'32'-L'FINDEC)X'FF'  MORE NON ZEROS NEEDED     05540027
         AIF       ('&PHASE' EQ 'ASMGF1').COMONF1                       05560027
         EJECT                                                          05580027
*                                                                       05600027
*        BEGINNING OF F2/F2A COMMON WORK AREA                           05620027
*                                                                       05640027
*        NAMING CONVENTION FOR TRANSLATED CHARACTER REPRESENTATION.     05660027
*             ALPHABETICS STAND FOR THEMSELVES                          05680027
*             NUMERICS HAVE THE LETTER 'N' PREFIXED TO THE DIGIT        05700027
*             SPECIAL CHARACTERS ARE NAMED MNEMONICALLY                 05720027
*                                                                       05740027
N0       EQU       X'00'                                                05760027
N1       EQU       X'01'                                                05780027
N2       EQU       X'02'                                                05800027
N3       EQU       X'03'                                                05820027
N4       EQU       X'04'                                                05840027
N5       EQU       X'05'                                                05860027
N6       EQU       X'06'                                                05880027
N7       EQU       X'07'                                                05900027
N8       EQU       X'08'                                                05920027
N9       EQU       X'09'                                                05940027
A        EQU       X'0A'                                                05960027
B        EQU       X'0B'                                                05980027
C        EQU       X'0C'                                                06000027
D        EQU       X'0D'                                                06020027
E        EQU       X'0E'                                                06040027
F        EQU       X'0F'                                                06060027
G        EQU       X'10'                                                06080027
H        EQU       X'11'                                                06100027
I        EQU       X'12'                                                06120027
J        EQU       X'13'                                                06140027
K        EQU       X'14'                                                06160027
L        EQU       X'15'                                                06180027
M        EQU       X'16'                                                06200027
N        EQU       X'17'                                                06220027
O        EQU       X'18'                                                06240027
P        EQU       X'19'                                                06260027
Q        EQU       X'1A'                                                06280027
R        EQU       X'1B'                                                06300027
S        EQU       X'1C'                                                06320027
T        EQU       X'1D'                                                06340027
U        EQU       X'1E'                                                06360027
V        EQU       X'1F'                                                06380027
W        EQU       X'20'                                                06400027
X        EQU       X'21'                                                06420027
Y        EQU       X'22'                                                06440027
Z        EQU       X'23'                                                06460027
$        EQU       X'24'                                                06480027
#        EQU       X'25'                                                06500027
@        EQU       X'26'                                                06520027
PLUS     EQU       X'27'                                                06540027
MINUS    EQU       X'28'                                                06560027
STAR     EQU       X'29'                                                06580027
SLASH    EQU       X'2A'                                                06600027
COMMA    EQU       X'2B'                                                06620027
EQUALS   EQU       X'2C'                                                06640027
AMPSND   EQU       X'2D'                                                06660027
DOT      EQU       X'2E'                                                06680027
LPAREN   EQU       X'2F'                                                06700027
RPAREN   EQU       X'30'                                                06720027
QUOTE    EQU       X'31'                                                06740027
BLANK    EQU       X'32'                                                06760027
HCHAR    EQU       X'32'               LAST INTERNAL CHARACTER          06780027
*                                                                       06800027
MINIMUM  EQU       0                   DUMMY VALUE FOR F2/F2A SPACEPOS  06820027
         EJECT                                                          06840027
TEMP     DC    D'0'                                                     06860027
ECOL     DC    A(71)                    END COLUMN                 (71) 06880027
SAVOUT   DC       2F'0'                 OUTPUT POINTER STORAGE          06900027
SAVEIN   EQU   SAVOUT+4                 INPUT POINTER STORAGE           06920027
WRTEXT   DC       F'0'                  FILE IDENTIFICATION             06940027
*        0 - UTILITY 1                                                  06960027
*        4 - UTILITY 2                                                  06980027
*        8 - UTILITY 3                                                  07000027
*        12 - SYSIN                                                     07020027
*        16 - COPY                                                      07040027
PARA2    DC        A(OUTPUT)           ADDRESS OF OUTPUT AREA           07060027
WRTLNG   DC        F'0'                LENGTH OF OUTPUT RECORD          07080027
LSTFLG   DC        F'0'                 OPERAND LIST WORD STORAGE       07100027
DCAREA   DC        5F'0'                BUILD AREA DICTIONARY REF/DEF   07120027
LTTLA    EQU   DCAREA+18                LITTLE A POINTER STORAGE        07140027
SAVPTR   DC        F'0'                 INPUT POINTER STORAGE           07160027
*                                                                       07180027
* NEXT 10 CONSTANTS USED IN MULTIPLE LOADS IN GETSRC ROUTINE.           07200027
*                                                                       07220027
* NUMBERS IN () ARE STANDARD VALUES , UNLESS ALTERED BY ICTL CARD.      07240027
*                                                                       07260027
SEQLEN   DC        F'7'                 SEQ FIELD LENGTH MINUS 1    (7) 07280027
SEQCL1   DC        F'73'                SEQUENCE FIELD START       (73) 07300027
ADJCON   DC        A(SRCBUF)           SOURCE BUFFER ADDR               07320027
CNTCL1   DC        F'72'                CONTINUATION CHAR COL      (72) 07340027
BEGCOL   DC        F'1'                 BEGIN COLUMN                (1) 07360027
SRCLEN   DC        F'70'                END COL MINUS BEGIN COL    (70) 07380027
BEGOUT   DC        A(INPUTBUF-1)       INPUT BUFFER LOC MINUS ONE       07400027
BEGCNT   DC        F'16'                CONTINUE COLUMN            (16) 07420027
CONLEN   DC        F'55'                END COL MINUS CONTINUE COL (55) 07440027
ENDCOL   DC        A(INPUTBUF+71)      NEXT AVAIL LOC IN INPUT BUFFER   07460027
*                                         MINUS ONE                     07480027
DENTRY   DC        A(DCAREA)           DCAREA LOCATION AND FLAG BYTE    07500027
PARA1    DC        A(*-*)               LOCATION OF SOURCE BUFFER       07520027
LNGTHS   DC        H'84'                LENGTH OF SOURCE BUFFER    (84) 07540027
NAMBYT   DC        AL1(*-*)             NAME SUMMARY BYTE               07560027
OPBYTE   DC        AL1(*-*)             OPERATION SUMMARY BYTE          07580027
NMBFLG   EQU       X'04'               NO OF FLAG BYTES IN REC FORMAT   07600027
NBFLM1   EQU       X'03'               SAME THING LESS ONE              07620027
PARA     DC        A(SRCBUF+NMBFLG)    LOCATION OF READ AREA            07640027
ABT8TY   DC        H'80'                LENGTH OF READ AREA        (80) 07660027
ADDCNT   DC        AL1(*-*)             NUMBER OF LINES IN STATEMENT    07680027
DERR     DC        AL1(*-*)             DICTIONARY ERROR BYTE           07700027
         DC        0F'0'                                                07720027
TYPRED   DC        H'12'                UNIT TO BE READ (12 = SYSIN)    07740027
WRTUNT   DC        H'8'                 UNIT TO BE WRITTEN              07760027
SWTCH1   DC        0F'0'                DRIVER BITS                     07780027
         DC        X'08',7X'00'                                         07800027
*  BIT 0       SEQUENCE CHECK                                           07820027
*      1       WITHIN MACRO DEFN                                        07840027
*      2       RETURN TO MISCAN                                         07860027
*      3       LAST STATEMENT WAS 'MACRO'- EXPECT PROTOTYPE             07880027
*      4       =1 UNLESS ICTL SAYS NO CONTINUATIONS ALLOWED             07900027
*      5                                                                07920027
*      6       END CARD                                                 07940027
*      7       'ENDOPR' EXIT CONTROL-                                   07960027
*              1 = RETURN TO CALLER (END OF OPERAND)                    07980027
*              0 = EXIT TO DRIVER (END OF STATEMENT)                    08000027
TYPATR   EQU   SWTCH1+1                 TYPE ATTRIBUTE                  08020027
SQNOTE   EQU   TYPATR                   NOTE/PT INFORMATION             08040027
LNGTAT   EQU   TYPATR+1                 LENGTH ATTRIBUTE                08060027
SCLATR   EQU   LNGTAT+2                 SCALE ATTRIBUTE                 08080027
SWTCH3   EQU   SCLATR+2                 ENTRY BITS                      08100027
*  BIT 0       GTSRC1,GTSRC2,GTSRC4,GTSRC5  GETSRC ENTRY POINTS         08120027
*      1       GTSRC1,GTSRC2,GTSRC4     GETSRC ENTRY POINTS             08140027
*      2       GTSRC2,GTSRC4            GETSRC ENTRY POINTS             08160027
*      3       GTSRC4                   GETSRC ENTRY POINTS             08180027
*      4       GTSRC4                   GETSRC ENTRY POINTS             08200027
*              BITS 0-4 = 0 FOR GTSRC ENTRY                             08220027
*      5       SET IF - SCANNING MNOTE, PUNCH OR TITLE STATEMENT        08240027
*      6       UNUSED                                                   08260027
*      7       ERROR FOUND                                              08280027
SWTCH4   EQU   SWTCH3+1                                                 08300027
*  BIT 0       COMMENTS CONTINUED                                       08320027
*      1       NEXT CARD IS A CONTINUATION OF THIS CARD                 08340027
*      2       THIS CARD IS A CONTINUATION OF THE PREVIOUS CARD         08360027
*      3       LAST CARD                                                08380027
*      4       I VALUE                                                  08400027
*      5       J VALUE                                                  08420027
*      6       MACRO INSTRUCTION OR PROTOTYPE BIT                       08440027
*      7       BUFFERING INDICATOR                                      08460027
         DC        0F'0'                                                08480027
SWTCH5   DC        X'81'                                                08500027
*  BIT 0       SET IF - PROCESSING PROGRAMMER MACRO.                    08520027
*      1              - IN OPEN CODE.                                   08540027
*      2              - IN SYSTEM MACRO DEFINITION.                     08560027
*      3              - GBL NOT ALLOWABLE.                              08580027
*      4              - LCL NOT ALLOWABLE.                              08600027
*      5              - SKIPPING TO MEND.                               08620027
*      6              - SKIPPING TO END.                                08640027
*      7              - EXTEN PARAMETER.                                08660027
SWTCH6   DC        X'00'                                                08680027
*  BIT 0       SET IF - WITHIN COPY CODE.                               08700027
*      1              - 'MACRO' OP EXPECTED.                            08720027
*      2              -                                                 08740027
*      3              - BWRITE ENTRY.                                   08760027
*      4              - FIRST CARD.                                     08780027
*      5              - SUPPRESS DICTIONARY ENTRIES FROM OPNDL LIST     08800027
*      6              -   DICTIONARY MUST BE SUBSET                     08820027
*      7              - NORMAL COMMENT WITHIN A MACRO.                  08840027
SWTCH7   DC        X'00'                                                08860027
*  BIT 0 (80)  SET IF PROCESSING SUBLIST.                               08880027
*      1 (40)  SET IF CONCATENATION IN OPERAND.                         08900027
*      2 (20)  SET IF PROCESSING KEYWORD PARAMETER.                     08920027
*      3 (10)                                                           08940027
*      4 (08)                                                           08960027
*      5 (04)  SET IF SEQ ERROR.                                        08980027
*      6 (02)  SET IF END CARD IS GENERATED.                            09000027
*      7 (01)  SET IF IN COPY CODE.                                     09020027
SWTCH8   DC        X'00'                                                09040027
*  BIT 0 (80)  SET IF OPSYN INVALID.                                    09060027
*      1 (40)  SET IF ERROR 78 NEEDED BEFORE OTHER ERRORS.              09080027
*      2 (20)  SET IF SKIPPING COPY BACK TO SYSIN LEVEL           V7A28 09100028
*      3 (10)                                                           09120027
*      4 (08)                                                           09140027
*      5 (04)                                                           09160027
*      6 (02)                                                           09180027
*      7 (01)                                                           09200027
*                                                                       09220027
*                                                                       09240027
IOPNDX   DC        F'0'                 SAVE SLOT FOR OPNDX             09260027
DEFENT   EQU   IOPNDX+1                 TYPE OF ENTRY FLAG BYTE         09280027
GCALL    EQU   DEFENT+1                 GSCAN CALLING ARGUMENT          09300027
OPNDX    EQU   GCALL+1                  POINTER TO OPNDL LIST           09320027
SRCFLG   DC        0F'0'                HEADER BYTES FOR SOURCE RECORD  09340027
SAMBYT   DC        X'08'                  TYPE ID (SET TO 08 IN F1)     09360027
SRCRLI   DC        FL2'84'                REC LNTH (=84 IN F1)          09380027
SRCFG1   DC        X'00'                  FLAGA BYTE                    09400027
*  BIT(S) 0        UNUSED (IN EDITING PHASES)                           09420027
*         1,2,3    000 PRINT AS IS                                      09440027
*                  001 ERROR                                            09460027
*                  010 CONSTRUCT FOR PRINT                              09480027
*                  011 CONSTRUCT FOR PRINT IF GENERATED                 09500027
*                  100 PROCESS ONLY                                     09520027
*                  101 ILLEGAL ****                                     09540027
*                  110 PROCESS AND CONSTRUCT FOR PRINT                  09560027
*                  111 PROCESS AND CONSTRUCT FOR PRINT IF GENERATED     09580027
*         4        UNUSED (IN EDITING PHASES)                           09600027
*         5        0   NO ERROR RECORD FOLLOWS                          09620027
*                  1   ERROR RECORD FOLLOWS                             09640027
*         6        0   NOT A CONTINUATION CARD                          09660027
*                  1   A CONTINUATION CARD                              09680027
*         7        UNUSED (IN EDITING PHASES)                           09700027
*                                       ******************************* 09720027
*                                       * AREA DEFINITIONS            * 09740027
*                                       ******************************* 09760027
DUMMY    DC        H'0'                 ALIGNMENT AREA                  09780027
EQUTYPE  DC        X'00'                SAVE AREA FOR EXT EQU EVAL      09800027
ENDBUF   DC        A(INPUTBUF+319)                                      09820027
INPUTE   EQU   ENDBUF                                                   09840027
*                                                                       09860027
*        'GETSRC' ROUTINE READS A CARD INTO SOURCE BUFFER 'SRCBUF' AND  09880027
*        BUILDS A STATEMENT IN INPUT BUFFER 'INPUTBUF'.                 09900027
OBUFS    DC    A(OUTPUT) .             POINTER TO OUTPUT BUFFER         09920027
INPUT    DC    A(INPUTBUF) .           POINTER TO BEGINNING OF INPUT    09940027
         SPACE     2                                                    09960027
INPUTBUF DC        320C' '             TO HOLD UP TO 3 CONTIN CARDS     09980027
SRCBUF   DC        22F'0'              INPUT CARD BUFFER                10000027
OUTPUT   DC        60D'0'              EDITED TEXT OUTPUT BUFFER        10020027
DBLWD    EQU   OUTPUT+16 .             TEMP BUFFER FOR GBL, LCL OPNDS   10040027
SAVREG   DC        25F'0'                                               10060027
SAVEM    EQU   SAVREG+64                                                10080027
COPYSV   DC        10F'0'                                               10100027
INFILS   DC        X'00'                                                10120027
SWTCH1X  DC        X'00'                                                10140027
SVNXT    DC        F'0'                                                 10160027
SVPTR    DC        F'0'                                                 10180027
MCALL    DC        H'0'                                                 10200027
* PARLVL = PARENTHESIS NEST LEVEL = LEFTS MINUS RIGHTS                  10220027
PARLVL   DC        F'0'                                                 10240027
         DC        H'0'                                                 10260027
GSUMRY   EQU   PARLVL+2                 GSCAN RESULT BYTE               10280027
GSTAT    EQU   GSUMRY+1                                                 10300027
* BIT 0 (80) = 1 IF ODD NUMBER OF QUOTES READ                           10320027
*                                                                       10340027
MVBYTE   EQU   GSTAT+1                                                  10360027
* BIT 0 (80)                                                            10380027
*     1 (40) = 1 IF OPERAND TREATED AS DUMMY                            10400027
*     2 (20) = 1 IF SDT (SELF-DEFINING TERM) IS DISALLOWED              10420027
*     3 (10) = 1 IF LEFT PAREN WAS READ                                 10440027
*     4 (08)                                                            10460027
*     5 (04)                                                            10480027
*     6 (02) = 1 IF QUOTE MODE (ODD NUMBER OF QUOTES READ)              10500027
*     7 (01) = 1 IF NEW CARD READ                                       10520027
TXTFLG   EQU   MVBYTE+1                                                 10540027
SDTEMP   DC        F'0'                 NOTE ALIGNMENT                  10560027
*AVSP- POINTS TO NEXT AVAILABLE SLOT IN RSTACK. RESET TO A(RSTACK)      10580027
*ON EACH ENTRY TO GSCAN.                                                10600027
AVSP     DC        F'0'                                                 10620027
SEQVAL   DC        CL40' '              LAST SEQUENCE VALUE             10640027
*RSTACK- WORK AREA STACK FOR RECURSIVELY ENTERED SCAN ROUTINES.         10660027
*8 BYTES/LEVEL. FORMAT IS FLLLXASS-                                     10680027
*  F = FLAG BYTE (2 FORMATS)-                                           10700027
*        BIT 0(128)- 1 = SETA      (SAVED FROM CURRENT MCALL INDICATOR  10720027
*            1(64)-  1 = SETC        FOR METSCN)                        10740027
*            2(32)-  1 = COMPLEX SUPERSTATE, 0 = SIMPLE                 10760027
*        OR                                                             10780027
*        BIT 0(128)- 1 = SYSLIST                                        10800027
*            1(64)-  1 = SUBSCRIPTED                                    10820027
*            2(32)-  1 = DOUBLE SUBSCRIPT                               10840027
*  LLL = LINK REG SAVE AREA                                             10860027
*  X = OPNDX SAVE BYTE                                                  10880027
*  A = ATTRIBUTE OPERATOR SAVE BYTE                                     10900027
*  SS = STATE POINTER SAVE AREA                                         10920027
RSTACK   DC        CL96' '                                              10940027
*OPNDL- OPERAND POINTER LIST. 4 BYTES/ENTRY. FORMAT IS FIDO-            10960027
*  F = FLAG BYTE.                                                       10980027
*     FOR VARIABLE SYMBOLS-                                             11000027
*        BIT 0(128)- 0 = &SYSLIST, 1 = OTHER V.S.                       11020027
*            1(64)-  SAME                                               11040027
*            2(32)-  1 = BOOLEAN (SETB) REQUIRED                        11060027
*            3(16)-  1 = SYMBOLIC PARAM REQUIRED                        11080027
*            4(8)-   1 = SUBSCRIPTED (DIMENSIONED)                      11100027
*            5-7     OPERAND LENGTH - 1                                 11120027
*     FOR ORDINARY OR SEQUENCE SYMBOLS-                                 11140027
*        BIT 0(128)- 1 = DICTIONARY ENTRY REQUIRED                      11160027
*            1(64)-                                                     11180027
*            2(32)-  1 = SEQ SYM, 0 = O.S.                              11200027
*            3(16)-                                                     11220027
*            4(8)-   1 = POINT OF DEFINITION, 0 = PT OF REFERENCE       11240027
*            5-7     OPERAND LENGTH - 1                                 11260027
*  I = REL LOC OF OPERAND IN INPUT BUFFER.                              11280027
*  D = DIMENSION OR PARAMETER NO.                                       11300027
*  O = REL LOC OF OPERAND IN OUTPUT BUFFER.                             11320027
OPNDL    DC        XL200'00'            (MUST BE FULL-WORD ALIGNED)     11340027
ASTAT    DC        F'0'                                                 11360027
RSLTA    EQU   ASTAT+3                                                  11380027
AFLGS    EQU   RSLTA                                                    11400027
LMAX     DC        H'0'                                                 11420027
SMAX     DC        H'0'                                                 11440027
MAPTYP   DC        CL1' '                                               11460027
ATYP     DC        CL1' '               RESULT  TYPE   *                11480027
ALENG    DC        H'0'                 RESULT  LENGTH *                11500027
ASCAL    DC        H'0'                 RESULT  SCALE  *                11520027
RFACT    DC        CL1' '                                               11540027
SFACT    DC        CL1' '                                               11560027
ACMAX    DC        CL1' '                                               11580027
ACALL    DC        CL1' '               CALLING SEQUENCE FOR ASCAN      11600027
BWSAVE   DC        10F'0'               SAVE AREA FOR 'BW' ROUTINES     11620027
SOPNDX   DC        C' '                                                 11640027
ERRCOD   DC        C' '                 ERROR FLAG FOR 'LEGOP' ROUTINE  11660027
NPRIME   DC        H'0'                 SUBLIST PARAMETER COUNT         11680027
NRKEY    DC        H'0'                 KEYWORD PARAMETER COUNT         11700027
POSNO    DC        H'0'                 POSITIONAL PARAMETER COUNT      11720027
POSNOMAX DC        H'240'               MAX POS PARM, 200 NOEXTEN V7A58 11740028
POSNOINT DC        H'6'                 INITIAL DUMMY POSITIONAL PARMS  11760027
INTPTR   DC        F'0'                                                 11780027
FSTINP   DC        F'0'                                                 11800027
SVLNR1   DC        F'0'                                                 11820027
INTOPT   DC        F'0'                                                 11840027
SVLINK   DC        F'0'                                                 11860027
MCD1     DC        F'0'                                                 11880027
SAVE2    DC        4F'0'                                                11900027
LTTLAF   DC        F'0'                FULLWORD LITTLA                  11920027
FINDSAVE DC        7F'0'               SAVE AREA FOR FIND IN ASMGF2     11940027
FINDAD   DC        A(FIND)             FIND ROUTINE ADDRESS IN ASMGF2   11960027
ERRSAV   DC        7F'0'               SAVE FOR POINTERS AND REGS       11980027
TMPOUT   DC        4F'0'               SAVE FOR OVERLAID OUTPUT AREA    12000027
SVLNKR   DC        F'0'                                                 12020027
SVOPTR   DC        F'0'                                                 12040027
OPS99    DC        F'0'                SAVE LINK FOR OPSYN PROCESSOR    12060027
TREGS    DC        5F'0'               TEMPORARY FOR SCRATCH REGISTERS  12080027
MACNAME  DC        CL8' '              MACRO NAME BEING EDITED (INTERN) 12100027
COPYNAME DC        CL8' '              MACRO NAME WITH COPIED CODE(INT) 12120027
STNOTE   DC        CL8' '               STORAGE FOR STATEMENT NOTE      12140027
         EJECT                                                          12160027
DUMCMNT  DC    X'08003A00292929320E1B1B181B32292929323232201112150E320E*12180027
               0D121D1217103215120B1B0A1B2232160A0C1B183228322929292929*12200027
               292929'                                                  12220027
SYSL     DC    X'1C221C15121C1D'       SYSLIST IN INTERNAL              12240027
*                                                                 V7A49 12242028
*        SYSTEM PARAMETER CONSTANTS                               V7A49 12244028
*        FORMAT - FLAG BYTE, NAME IN INTERNAL, PARAMETER NUMBER   V7A49 12246028
SYSNDX   DC    X'4E2D1C221C170D210000'                            V7A49 12248028
SYSECT   DC              X'0E0C1D0001'                            V7A49 12250028
SYSDATE  DC    X'4F2D1C221C0D0A1D0E0002'                          V7A49 12252028
SYSTIME  DC              X'1D12160E0003'                          V7A49 12254028
SYSSTYP  DC              X'1C1D22190004'                          V7A49 12256028
SYSPARM  DC              X'190A1B160005'                          V7A49 12258028
*                                                                       12260027
ERRST    EQU   X'0D'                   ERROR STATEMENT TYPE FLAG        12280027
WRNST    EQU   X'0D'                   WARNING STATEMENT TYPE FLAG      12300027
*                                                                       12320027
SNATTR   EQU       55                                                   12340027
DTYP     EQU       25                  SPECIAL DUMMY TYPE BYTE          12360027
SPUT     EQU       X'FD'               PUT RAW TEST FLAG                12380027
ENDFLD   EQU       X'F8'               END OF FIELD FLAG                12400027
SCEXPR   EQU       X'27'               CHARACTER EXPRESSION FLAG        12420027
HSNATR   DC        0H'0'                                                12440027
         DC    AL2(SNATTR)                                              12460027
TYPATI   DC    AL1(DTYP)                ASSUMED TYPE, LENGTH, SCALE FOR 12480027
         DC    X'00000000'                NAME FIELD SYMBOLS -UNDEFINED 12500027
PERASK   DC    AL2(DOT*256+STAR)                                        12520027
OMTFLD   DC    AL1(SPUT)                                                12540027
         DC    AL2(ENDFLD)                                              12560027
TRMOPN   DC    AL1(SCEXPR)                                              12580027
         DC    AL2(BLANK)                                               12600027
         DC    AL1(ENDFLD)                                              12620027
         SPACE     2                                                    12640027
FINIS    EQU   *                                                        12660027
*                                                                       12680027
PSOPSW   DC        X'00'                SWITCH BYTE FOR SMTSEQ          12700027
*                                       ******************************* 12720027
         EJECT                                                          12740027
*              ONE-BYTE TEMPORARIES AND COMMON CELLS.                   12760027
*                                                                       12780027
DSSTRT   DC        5C' '                INITIAL FILE POSITION.        A 12800027
*                                                                       12820027
*              HALFWORD TEMPORARIES AND COMMON CELLS.                   12840027
*                                                                       12860027
         DC        0D'0'                                                12880027
ENTLN    DC    H'0'                     ENTRY LENGTH - CHAIN POINTER -1 12900027
HSHVAL   DC    H'0'                     HASH VALUE - 16 BITS.           12920027
*                                                                       12940027
*              FULL-WORD TEMPORARIES AND COMMON CELLS.                  12960027
*                                                                       12980027
DUSING   DC        V(ENTDCT)            BASE REGISTERS                  13000027
DCLOS1AD DC        V(DCLOS1)             FOR PHASE F2A                  13020027
LASCAN   DC        V(ASCAN)            .                          V7A54 13021028
DCLSE    DC        V(DCLOSE)           .                          V7A54 13022028
GETAD    DC        V(GETSRC)           .                          V7A54 13023028
DLOOKUP  DC        V(LOOKUP)           .                          V7A54 13024028
ADWS     DC        V(BWFORC)           .                          V7A54 13025028
ADWS2    DC        V(BWRITE)           .                          V7A54 13026028
VCHECK   DC        V(LIBCHK)           .                          V7A54 13027028
VLREAD   DC        V(LIBRFND1)         .                          V7A54 13028028
VWAIT    DC        V(LIBWAIT)          .                          V7A54 13029028
AKLOSIT  DC        V(KLOSIT)           .                          V7A54 13030028
VENTKWB  DC        V(ENTKWB)           .                          V7A54 13031028
VCR      DC        F'4'                 RELATIVE VIRTUAL STORAGE PTR.   13040027
EVENX    DC    2F'0'                    LEFT TWO BYTES ALWAYS 00.       13060027
EVENY    DC        F'0'                 LEFT ONE BYTE ALWAYS 0.         13080027
HSHA     DC    2F'0'                    HASHING AREA.                   13100027
TNDX     DC    F'4'                     TRANSIENT BLOCK ALLOC. INDEX.   13120027
THRESH   DC    F'0'                     THRESHOLD POINTER.              13140027
PHASHM   DC        A((PASHL-1)*2)       PERMANENT AREA HT HASH MASK.    13160027
THASHM   DC        A(TASHL-1)           TRANSIENT AREA HT HASH MASK.    13180027
TBEG     EQU       20                  TRANS LITTLE 'A' POINTER ORIGIN  13200027
PBEG     EQU       4                   PERM  LITTLE 'A' POINTER ORIGIN  13220027
LTLAP    DC    A(PBEG+1)                                                13240027
         DC    A(PBEG)                                                  13260027
LTLAT    DC    A(TBEG+1)                                                13280027
         DC    A(TBEG)                                                  13300027
BTNRP    DC    F'0'                     BIT NUMBER, PERM AREA SETB'S.   13320027
BTNRT    DC    F'0'                     BIT NUMBER, TRANS AREA SETB'S.  13340027
FZRO     DC    2F'0'                                                    13360027
MACHN    DC    F'0'                     MACRO CHAIN ORIGIN.             13380027
IOCTL    DC    F'0'                     CORE LOCATION.                  13400027
         DC    AL2(BLKSZ)               BYTE COUNT.                     13420027
         DC    AL1(DCFILE)                                              13440027
         DC    AL4(0)                   N/P ADDRESS.                    13460027
         DC        3C' '               TO MAKE LOOK LIKE FILE BLOCK     13480027
         DC        AL2(BLKSZ)          BLOCK LENGTH WRITTEN             13500027
         EJECT                                                          13520027
*                                                                       13540027
*        COMMON AREA USED BY DICTIONARY SUBSETTING ROUTINE              13560027
*                                                                       13580027
ACTRV    EQU   4096            STANDARD ACTR VALUE                      13600027
ACTR     DC    A(ACTRV)        ACTR VALUE                               13620027
*        INPUT PARAMETER LIST - TRANSIENT DICTIONARIES                  13640027
TRD      DC    A(0)            TRANSIENT DICTIONARY INPUT AREA ADDRESS  13660027
         DC    AL2(BLKSZ)      INPUT AREA SIZE                          13680027
*        OUTPUT PARAMETER LIST - SUBSETTED DICTIONARIES                 13700027
MCD      DC    A(0)            DICTIONARY SUBSETTING AREA ADDRESS       13720027
MACNAM   DC    F'0'            STORAGE FOR ADDR OF MACRO NAME ENTRY     13740027
MCDSZ    DC    F'0'                                                     13760027
ENDTRD   DC    F'0'                                                     13780027
NPMAIN   DC        F'1'        STORAGE FOR N/P ADDR OF OPENCODE DICT    13800027
RECSZ    EQU   SSEG                                                     13820027
         SPACE     2                                                    13840027
*                                                                       13860027
*        TABLE USED BY STATEMENT SEQUENCE CHECKING ROUTINE              13880027
*                                                                       13900027
*        SWITCH BYTES BY PSEUDO-OP FOR STATEMENT TYPE SEQUENCING        13920027
*        BIT   0(X'80')=1  IF END PSEUDO-OP                             13940027
*              1(X'40')=1     MEND PSEUDO-OP                            13960027
*              2(X'20')=1     MACRO PSEUDO-OP                           13980027
*              3(X'10')=1     MEXIT,MNOTE,MEND PSEUDO-OPS               14000027
*              4(X'08')=1     GBLX,LCLX PSEUDO-OPS                      14020027
*              5(X'04')=1     ILLEGAL WITHIN COPY CODE                  14040027
*              6(X'02')=1     ILLEGAL WITHIN MACRO DEFINITIONS          14060027
*              7(X'01')=1     ALLOWED BETWEEN PROGRAMMER MACROS         14080027
*                                                                       14100027
*                                       INTERNAL OP CODES               14120027
POBYTE   DC    X'08'          GBLA      0                               14140027
         DC    X'08'          GBLB      1                               14160027
         DC    X'08'          GBLC      2                               14180027
         DC    X'08'          LCLA      3                               14200027
         DC    X'08'          LCLB      4                               14220027
         DC    X'08'          LCLC      5                               14240027
         DC    X'00'          SETA      6                               14260027
         DC    X'00'          SETB      7                               14280027
         DC    X'00'          SETC      8                               14300027
         DC    X'00'          AIF       9                               14320027
         DC    X'00'          AGO       10                              14340027
         DC    X'00'          ANOP      11                              14360027
POCOPY   DC    X'01'          COPY      12     DC  X'04' IF NOEXTEN     14380027
POMACRO  DC    X'23'          MACRO     13     DC  X'27' IF NOEXTEN     14400027
         DC    X'10'          MNOTE     14                              14420027
         DC    X'10'          MEXIT     15                              14440027
POMEND   DC    X'50'          MEND      16     DC  X'54' IF NOEXTEN     14460027
         DC    X'06'          ICTL      17                              14480027
         DC    X'07'          ISEQ      18                              14500027
POPRINT  DC    X'01'          PRINT     19     DC  X'03' IF NOEXTEN     14520027
         DC    X'01'          SPACE     20                              14540027
         DC    X'01'          EJECT     21                              14560027
         DC    X'00'          PUNCH     22                              14580027
         DC    X'00'          REPRO     23                              14600027
         DC    X'01'          TITLE     24                              14620027
         DC    X'00'          ENTRY     25                              14640027
         DC    X'00'          EXTRN     26                              14660027
         DC    X'00'          START     27                              14680027
         DC    X'00'          CSECT     28                              14700027
         DC    X'00'          DSECT     29                              14720027
         DC    X'00'          COM       30                              14740027
         DC    X'00'          EQU       31                              14760027
         DC    X'00'          ORG       32                              14780027
POEND    DC    X'82'          END       33  DC  X'86' IF NOEXTEN  V7A28 14800028
         DC    X'00'          LTORG     34                              14820027
         DC    X'00'          USING     35                              14840027
         DC    X'00'          DROP      36                              14860027
         DC    X'00'          ACTR      37                              14880027
         DC    X'00'          DC        38                              14900027
         DC    X'00'          DS        39                              14920027
         DC    X'00'          CCW       40                              14940027
         DC    X'00'          CNOP      41                              14960027
         DC    X'00'          *EXTRA*   42                              14980027
         DC    X'00'          DXD       43                              15000027
         DC    X'00'          CXD       44                              15020027
         DC    X'03'          OPSYN     45                              15040027
         DC    X'00'          WXTRN     46                              15060027
         DC    X'00'          POP       47                              15080027
         DC    X'00'          PUSH      48                              15100027
         SPACE 2                                                        15120027
*                                                                       15140027
*        MACRO/COPY SAVE AREAS                                          15160027
*                                                                       15180027
CPLENGTH EQU   32                       BYTES PER COPY LEVEL            15200027
CPDEPTH  EQU   5                        MAX COPY DEPTH, 1 IF NOEXTEN    15220027
CPLEN    DC    A(CPLENGTH)              LENGTH OF ONE LEVEL             15240027
CPMAX    DC    A(CPLENGTH*CPDEPTH)      MAX NESTING LEVEL               15260027
CPCUR    DC    A(0-CPLENGTH)            CURRENT NESTING LEVEL           15280027
CPWORK   DC    0F'0',(CPLENGTH*(CPDEPTH+1))X'00'  NESTED COPY WORK      15300027
*                                                                       15320027
CPLIBNOT EQU   0                        NOTED LIBRARY FDAD              15340027
CPLIBNTA EQU   CPLIBNOT+8               LIBRARY BUFFER RECORD POINTER   15360027
CPLIBEOB EQU   CPLIBNTA+4               LIBRARY BUFFER EOB ADDRESS      15380027
CPCPNAME EQU   CPLIBEOB+4               LIBRARY MEMBER NAME             15400027
CPLIBNO2 EQU   CPCPNAME+8               2ND LIBRARY BUFFER FDAD         15420027
CPLIBEND EQU   CPLIBNO2+8               END OF ONE LEVEL WORK           15440027
         EJECT                                                          15460027
         AGO       .COMEND                                              15480027
.COMONF1 ANOP                                                           15500027
*                                                                       15520027
*        WORK AREA FOR F1 ONLY                                          15540027
*                                                                       15560027
DWORD    DC        D'0'                DOUBLE WORD WORK AREA            15580027
UTJFCB   DC        XL176'00'           READ UTILITY JFCB'S HERE         15600027
IOSAVE   DC        5F'0'               SAVE AREA FOR SYSPRINT I/O       15620027
PARMOFF  DC        A(0)                OFFSET OF A PARM ERROR           15640027
LINE     DC        CL133' '            PRINT LINE BUFFER                15660027
.COMEND  ANOP                                                           15680027
         LIST1EQU                                                       15700027
COMEND   DC        0D'0'               MAKE COMMON DBL WORDS LONG       15720027
*                                                                       15740027
         AIF       ('&PHASE' NE 'ASMGF1').ENDF2                         15760027
*        ASMGF1 PHASE MUST BE AS LARGE AS ANY SUBSEQUENT                15780027
*        PHASE, SO ROUND UP ASMGF1 TO A LARGER FIGURE.                  15800027
*                                                                       15820027
DUMLENF1 EQU       1024*26-(F1END-ASMGF1)-(COMEND-COMMON)         V7A16 15840028
         DS        (DUMLENF1)X         UP TO SIZE OF ALL OTHERS         15860027
         AGO       .MEND                                                15880027
.ENDF2   ANOP                                                           15900027
*        ASMGF2 PHASE MUST BE AS LARGE AS ASMGF1 PHASE,                 15920027
*        SO ROUND UP ASMGF2A TO A LARGER FIGURE.                  V7A16 15940028
*                                                                       15960027
ELENF2   EQU       10500               ROUNDED EST F2 LENGTH      V7A16 15980028
         AIF       ('&PHASE' EQ 'ASMGF2').ENDF2T                  V7A16 16000028
ASMGF2A  CSECT     ,                   RESUME FORMER CSECT        V7A16 16010028
DUMLENF2 EQU   1024*26-(F2AEND-ASMGF2A)-(COMEND-COMMON)-ELENF2    V7A16 16020028
         DS        (DUMLENF2)X         UP SIZE OF ASMGF2A         V7A16 16040028
FIND     EQU       *                   DUMMY FIND ADDR FOR F2A    V7A16 16050028
         AGO       .MEND                                                16060027
.ENDF2T  ANOP      ,                                              V7A16 16080028
*                                                                 V7A16 16100028
ALENF2   EQU       (F2END-ASMGF2+99)/100*100  ROUNDED ACTUAL F2   V7A16 16120028
F2TEST   DC        0S(ALENF2-ELENF2) 0S(ELENF2-ALENF2)            V7A16 16140028
.MEND    ANOP                                                           16160027
         DC        0D'0'               THIS IS WHERE WE'RE AT     V7A16 16170028
*                                                                       16180027
         MEND                                                           16200027
./ ADD NAME=DCBOUT
         MACRO                                                          00020020
         DCBOUT                                                         00040020
         GBLB      &NOTBUG,&NOTSTAT                                     00060020
         AIF       (&NOTBUG AND &NOTSTAT).EXIT                          00080020
SAVOUT1  DS        6F                                                   00100020
SAVOUT2  DS        18F                                                  00120020
DCBOUT   DCB       DSORG=PS,MACRF=(PMC),DDNAME=STATDBUG,DEVD=PR,       X00140020
               PRTSP=1,RECFM=F,LRECL=50,BLKSIZE=50,BFTEK=S,            X00160020
               BUFNO=1,BUFL=50,EROPT=ACC                                00180020
.EXIT    MEND                                                           00200020
./ ADD NAME=DEFCHAR
         MACRO                                                          00020020
         DEFCHAR                                                        00040020
         GBLA      &NUMCHAR,&EBDIC(64)                                  00060020
         GBLC      &CHARSET(64)                                         00080020
&NUMCHAR SETA      51                                                   00100020
&CHARSET(1)  SETC  '.'                                                  00120020
&CHARSET(2)  SETC  '('                                                  00140020
&CHARSET(3)  SETC  '+'                                                  00160020
&CHARSET(4)  SETC  ''''                                                 00180020
&CHARSET(5)  SETC  '&&'                                                 00200020
&CHARSET(6)  SETC  '*'                                                  00220020
&CHARSET(7)  SETC  ')'                                                  00240020
&CHARSET(8)  SETC  '-'                                                  00260020
&CHARSET(9)  SETC  '/'                                                  00280020
&CHARSET(10) SETC  ','                                                  00300020
&CHARSET(11) SETC  '='                                                  00320020
&CHARSET(12) SETC  'A'                                                  00340020
&CHARSET(13) SETC  'B'                                                  00360020
&CHARSET(14) SETC  'C'                                                  00380020
&CHARSET(15) SETC  'D'                                                  00400020
&CHARSET(16) SETC  'E'                                                  00420020
&CHARSET(17) SETC  'F'                                                  00440020
&CHARSET(18) SETC  'G'                                                  00460020
&CHARSET(19) SETC  'H'                                                  00480020
&CHARSET(20) SETC  'I'                                                  00500020
&CHARSET(21) SETC  'J'                                                  00520020
&CHARSET(22) SETC  'K'                                                  00540020
&CHARSET(23) SETC  'L'                                                  00560020
&CHARSET(24) SETC  'M'                                                  00580020
&CHARSET(25) SETC  'N'                                                  00600020
&CHARSET(26) SETC  'O'                                                  00620020
&CHARSET(27) SETC  'P'                                                  00640020
&CHARSET(28) SETC  'Q'                                                  00660020
&CHARSET(29) SETC  'R'                                                  00680020
&CHARSET(30) SETC  'S'                                                  00700020
&CHARSET(31) SETC  'T'                                                  00720020
&CHARSET(32) SETC  'U'                                                  00740020
&CHARSET(33) SETC  'V'                                                  00760020
&CHARSET(34) SETC  'W'                                                  00780020
&CHARSET(35) SETC  'X'                                                  00800020
&CHARSET(36) SETC  'Y'                                                  00820020
&CHARSET(37) SETC  'Z'                                                  00840020
&CHARSET(38) SETC  '0'                                                  00860020
&CHARSET(39) SETC  '1'                                                  00880020
&CHARSET(40) SETC  '2'                                                  00900020
&CHARSET(41) SETC  '3'                                                  00920020
&CHARSET(42) SETC  '4'                                                  00940020
&CHARSET(43) SETC  '5'                                                  00960020
&CHARSET(44) SETC  '6'                                                  00980020
&CHARSET(45) SETC  '7'                                                  01000020
&CHARSET(46) SETC  '8'                                                  01020020
&CHARSET(47) SETC  '9'                                                  01040020
&CHARSET(48) SETC  ' '                                                  01060020
&CHARSET(49) SETC  '$'                                                  01080020
&CHARSET(50) SETC  '@'                                                  01100020
&CHARSET(51) SETC  '#'                                                  01120020
&EBDIC(1)  SETA    X'2E'                                                01140020
&EBDIC(2)  SETA    X'2F'                                                01160020
&EBDIC(3)  SETA    X'27'                                                01180020
&EBDIC(4)  SETA    X'31'                                                01200020
&EBDIC(5)  SETA    X'2D'                                                01220020
&EBDIC(6)  SETA    X'29'                                                01240020
&EBDIC(7)  SETA    X'30'                                                01260020
&EBDIC(8)  SETA    X'28'                                                01280020
&EBDIC(9)  SETA    X'2A'                                                01300020
&EBDIC(10) SETA    X'2B'                                                01320020
&EBDIC(11) SETA    X'2C'                                                01340020
&EBDIC(12) SETA    X'0A'                                                01360020
&EBDIC(13) SETA    X'0B'                                                01380020
&EBDIC(14) SETA    X'0C'                                                01400020
&EBDIC(15) SETA    X'0D'                                                01420020
&EBDIC(16) SETA    X'0E'                                                01440020
&EBDIC(17) SETA    X'0F'                                                01460020
&EBDIC(18) SETA    X'10'                                                01480020
&EBDIC(19) SETA    X'11'                                                01500020
&EBDIC(20) SETA    X'12'                                                01520020
&EBDIC(21) SETA    X'13'                                                01540020
&EBDIC(22) SETA    X'14'                                                01560020
&EBDIC(23) SETA    X'15'                                                01580020
&EBDIC(24) SETA    X'16'                                                01600020
&EBDIC(25) SETA    X'17'                                                01620020
&EBDIC(26) SETA    X'18'                                                01640020
&EBDIC(27) SETA    X'19'                                                01660020
&EBDIC(28) SETA    X'1A'                                                01680020
&EBDIC(29) SETA    X'1B'                                                01700020
&EBDIC(30) SETA    X'1C'                                                01720020
&EBDIC(31) SETA    X'1D'                                                01740020
&EBDIC(32) SETA    X'1E'                                                01760020
&EBDIC(33) SETA    X'1F'                                                01780020
&EBDIC(34) SETA    X'20'                                                01800020
&EBDIC(35) SETA    X'21'                                                01820020
&EBDIC(36) SETA    X'22'                                                01840020
&EBDIC(37) SETA    X'23'                                                01860020
&EBDIC(38) SETA    X'00'                                                01880020
&EBDIC(39) SETA    X'01'                                                01900020
&EBDIC(40) SETA    X'02'                                                01920020
&EBDIC(41) SETA    X'03'                                                01940020
&EBDIC(42) SETA    X'04'                                                01960020
&EBDIC(43) SETA    X'05'                                                01980020
&EBDIC(44) SETA    X'06'                                                02000020
&EBDIC(45) SETA    X'07'                                                02020020
&EBDIC(46) SETA    X'08'                                                02040020
&EBDIC(47) SETA    X'09'                                                02060020
&EBDIC(48) SETA    X'32'                                                02080020
&EBDIC(49) SETA    X'24'                                                02100020
&EBDIC(50) SETA    X'26'                                                02120020
&EBDIC(51) SETA    X'25'                                                02140020
         MEND                                                           02160020
./ ADD NAME=ELZERO
         MACRO                                                          00020027
         ELZERO                                                         00040027
         GBLA  &NBR                     INPUT ERROR IN BINARY           00060027
         GBLC  &CNBR                    OUTPUT ERROR IN CHARACTER       00080027
         LCLA  &DIGCTR,&POWTEN          DIGITS REQUIRED, POWER OF TEN   00100027
&DIGCTR  SETA  4                                                        00120027
&POWTEN  SETA  10                                                       00140027
&CNBR    SETC  '&NBR'                   SET ERROR NUMBER CHARACTER      00160027
.SKIP01  AIF   (&NBR GE &POWTEN).SKIP02                                 00180027
&CNBR    SETC  '0'.'&CNBR'              ADD A LEADING ZERO              00200027
.SKIP02  ANOP                                                           00220027
&POWTEN  SETA  &POWTEN*10                                               00240027
&DIGCTR  SETA  &DIGCTR-1                                                00260027
         AIF   (&DIGCTR GT 1).SKIP01                                    00280027
         MEND                                                           00300027
./ ADD NAME=EMSG
         MACRO                                                          00020027
         EMSG  &ERNO,&SVC,&MSG          GENERATE ERROR MESSAGE          00040027
         GBLA  &GMAXNO                  MAX NUMBER OF ERRORS            00060027
         GBLA  &NBR                     ERROR NUMBER BINARY             00080027
         GBLB  &GERROR(200)             DUMMY ERROR FLAG                00100027
         GBLC  &CNBR                    ERROR NUMBER CHARACTER          00120027
         LCLA  &CNT                     WORK FOR LENGTH OF MESSAGE      00140027
&NBR     SETA  &ERNO                                                    00160027
         AIF   (&NBR LE &GMAXNO).MSGOK  IF NO POINTER, FLAG             00180027
         MNOTE 4,'EMSG  ***ERROR***  THIS MESSAGE HAS NO POINTER'       00200027
.MSGOK   ANOP                                                           00220027
         AIF   (&GERROR(&NBR)).SKIP01                                   00240027
.*                                                                      00260027
.*       CONVERT NUMBER TO CHARACTER                                    00280027
         ELZERO                                                         00300027
&CNT     SETA  K'&MSG-3                GET LENGTH-1 OF MESSAGE LESS 'S  00320027
E&CNBR   DC    AL1(&CNT,&SVC),C&MSG                                     00340027
         AGO   .SKIP02                                                  00360027
.SKIP01  MNOTE *,'ERROR &NBR IS AN ASSEMBLER ERROR.'                    00380027
.SKIP02  ANOP                                                           00400027
         SPACE 1                                                        00420027
         MEND                                                           00440027
./ ADD NAME=ENTRYOUT
         MACRO                                                          00020020
&S       ENTRYOUT  &PHASE,&DCB=YES,&CSECT=GSECT                         00040020
         GBLB      &STAT,&NOTBUG                                        00060020
&S       DS        0H                                                   00080020
         AIF       (&STAT).ENTRY1                                       00100020
         AIF       (&NOTBUG).EXIT                                       00120020
         MESSAGE   'PHASE &PHASE ENTERED',DCB=&DCB,CSECT=&CSECT         00140020
         AGO       .EXIT                                                00160020
.ENTRY1  TIMEOUT   'PHASE &PHASE ENTERED',DCB=&DCB,CSECT=&CSECT         00180020
.EXIT    MEND                                                           00200020
./ ADD NAME=EPTR
         MACRO                                                          00020027
         EPTR  &MAXNO,&ERROR=           GENERATE ERROR POINTERS         00040027
         GBLA  &GMAXNO                  MAXIMUM NUMBER OF ERRORS        00060027
         GBLA  &NBR                     ERROR NUMBER BINARY             00080027
         GBLB  &GERROR(200)             DUMMY ERROR FLAG                00100027
         GBLC  &CNBR                    ERROR NUMBER CHARACTER          00120027
         LCLA  &I                                                       00140027
&NBR     SETA  N'&ERROR                                                 00160027
.SKIP01  AIF   (&NBR LE 0).SKIP02                                       00180027
&I       SETA  &ERROR(&NBR)                                             00200027
&GERROR(&I) SETB (1)                                                    00220027
&NBR     SETA  &NBR-1                                                   00240027
         AGO   .SKIP01                                                  00260027
.SKIP02  ANOP                                                           00280027
&NBR     SETA  1                                                        00300027
* GENERATED RELATIVE POINTER TABLE                                      00320027
.SKIP03  ANOP                                                           00340027
         AIF   (&GERROR(&NBR)).SKIP04                                   00360027
.*                                                                      00380027
.*       CONVERT NUMBER TO CHARACTER                                    00400027
         ELZERO                                                         00420027
         DC    AL2(E&CNBR-ESTART)                                       00440027
         AGO   .SKIP05                                                  00460027
.SKIP04  DC    AL2(EFM-ESTART) ASSEMBLER ERROR DIAGNOSTIC               00480027
.SKIP05  ANOP                                                           00500027
&NBR     SETA  &NBR+1                   UP TO NEXT ERROR NUMBER         00520027
         AIF   (&NBR LE &MAXNO).SKIP03  LOOP IF MORE ERROR POINTERS     00540027
&GMAXNO  SETA  &MAXNO                   SET SO SAME POINTERS AS MSG     00560027
         MEND                                                           00580027
./ ADD NAME=FDIMEN   UPDATE DONE
         MACRO                                                          00020020
         FDIMEN &GRA=4,&GRB=5,&GRC=6,&GRD=7,&SP1=10,&SP2=11,&SRR=9      00040020
         MNOTE *,'ASMG CENTRAL DIMENSIONING DECK--SEE RTA FOR LISTING'  00060020
*                                                                       00080020
******** GENERAL REGISTER SYMBOLIC ASSIGNMENTS ************************ 00100020
* VARIABLE REGISTRS--MAY BE SPECIFIED TO NON-STANDARD VALUES            00120020
SRR      EQU   &SRR      COMMON SUBROUTINE RETURN ADDRESS ODD   CONTIG. 00140020
SP1      EQU   &SP1      COMMON SUBROUTINE PARAMETER 1    EVEN          00160020
SP2      EQU   &SP2      COMMON SUBROUTINE PARAMETER 2    ODD           00180020
GRA      EQU   &GRA      LEVEL 0-1 GP REG A  EVEN    A-D CONTIGUOUS     00200020
GRB      EQU   &GRB      LEVEL 0-1 GP REG B  ODD                        00220020
GRC      EQU   &GRC      LEVEL 0-1 GP REG C                             00240020
GRD      EQU   &GRD      LEVEL 0-1 GP REG D                             00260020
ACT      EQU    3        ASSEMBLER CONTROL TABLE POINTER                00280020
SRB      EQU    8        COMMON SUBROUTINE BASE ADDRESS   EVEN  SRB-SP1 00300020
GRX      EQU   14        GENERAL PURPOSE REG X  EVEN   X-Y CONTIGUOUS   00320020
GRY      EQU   15        GENERAL PURPOSE REG Y  ODD                     00340020
GRZ      EQU   13        GENERAL PURPOSE REG Z                          00360020
FRB      EQU   12        FUNCTIONAL ROUTINE BASE ADDRESS                00380020
CRB      EQU   GRC       MAIN LINE CONTROL BASE ADDRESS                 00400020
CRR      EQU   GRD       MAIN LINE CONTROL RETURN ADDRESS               00420020
GR0      EQU   0                                                        00440020
GR1      EQU   1                                                        00460020
GR2      EQU   2                                                        00480020
         LIST1EQU                                                       00490025
         EJECT                                                          00500020
******** INTERNAL CHARACTER CODES ************************************* 00520020
IZERO    EQU   X'00'     NUMBERS                                        00540020
I1       EQU   X'01'                                                    00560020
I2       EQU   X'02'                                                    00580020
I3       EQU   X'03'                                                    00600020
I4       EQU   X'04'                                                    00620020
I5       EQU   X'05'                                                    00640020
I6       EQU   X'06'                                                    00660020
I7       EQU   X'07'                                                    00680020
I8       EQU   X'08'                                                    00700020
I9       EQU   X'09'                                                    00720020
IA       EQU   X'0A'     LETTERS                                        00740020
IB       EQU   X'0B'                                                    00760020
IC       EQU   X'0C'                                                    00780020
ID       EQU   X'0D'                                                    00800020
IE       EQU   X'0E'                                                    00820020
IF       EQU   X'0F'                                                    00840020
IG       EQU   X'10'                                                    00860020
IH       EQU   X'11'                                                    00880020
II       EQU   X'12'                                                    00900020
IJ       EQU   X'13'                                                    00920020
IK       EQU   X'14'                                                    00940020
IL       EQU   X'15'                                                    00960020
IM       EQU   X'16'                                                    00980020
IN       EQU   X'17'                                                    01000020
IO       EQU   X'18'                                                    01020020
IP       EQU   X'19'                                                    01040020
IQ       EQU   X'1A'                                                    01060020
IR       EQU   X'1B'                                                    01080020
IS       EQU   X'1C'                                                    01100020
IT       EQU   X'1D'                                                    01120020
IU       EQU   X'1E'                                                    01140020
IV       EQU   X'1F'                                                    01160020
IW       EQU   X'20'                                                    01180020
IX       EQU   X'21'                                                    01200020
IY       EQU   X'22'                                                    01220020
IZ       EQU   X'23'                                                    01240020
IDOLLR   EQU   X'24'     EXTRA LETTERS                                  01260020
IPOUND   EQU   X'25'                                                    01280020
IAT      EQU   X'26'                                                    01300020
IPLUS    EQU   X'27'     SPECIAL CHARACTERS                             01320020
IMINUS   EQU   X'28'                                                    01340020
IMULT    EQU   X'29'                                                    01360020
IASTER   EQU   X'29'                                                    01380020
IDIVID   EQU   X'2A'                                                    01400020
ICOMMA   EQU   X'2B'                                                    01420020
IEQUAL   EQU   X'2C'                                                    01440020
IAMPSD   EQU   X'2D'                                                    01460020
IPRIOD   EQU   X'2E'                                                    01480020
ILPARN   EQU   X'2F'                                                    01500020
IRPARN   EQU   X'30'                                                    01520020
IQUOTE   EQU   X'31'                                                    01540020
IBLANK   EQU   X'32'                                                    01560020
IALPHA   EQU   IAT       LAST ALPHA CHARACTER                           01580020
******** END INTERNAL CHARACTER CODES ********************************* 01600027
         EJECT                                                          01620020
CT1C     EQU   40                       TOTAL LENGTH OF CT1             01640020
CT2C     EQU       14                   NUMBER OF FUNCTIONAL ROUTINES   01660022
CT3C     EQU   10                       NUMBER OF COMMON SUBROUTINES    01680020
CT4C     EQU   32                      NUMBER OF COMMON DATA AREAS      01700022
CT5C     EQU   644                      TOTAL LENGTH OF CENTRAL TABLE   01720020
CT6C     EQU       740                  LENGTH OF INTERNAL TEXT BUFFER  01740022
CT1      EQU   0                                                        01760020
CT2      EQU   CT1+CT1C                                                 01780020
CT3      EQU   CT2+4*CT2C                                               01800020
CT4      EQU   CT3+4*CT3C                                               01820020
CT5      EQU   CT4+4*CT4C                                               01840020
CT6      EQU   CT5+CT5C                                                 01860020
CTRTRN   EQU   CT1+16                   PLC RETURN FOR FUNCTIONAL ROUT  01880020
CTLNK2   EQU   CT1+6                    MLC LINKAGE ALGORITM FOR ESD    01900020
*./      DELETE    SEQ1=01920020,SEQ2=01920020                          01920022
CTSAVE   EQU   CTLNK2+18                CONTROL AEGISTER SAVE AREA      01940020
******** CT2 DISPLACEMENT VALUES FOR FUNCTIONAL ROUTINE BASE ADDRESSES  01960020
CBRNDA   EQU   CT2+00    BRENDA                                         01980020
CDCVAL   EQU   CT2+44    DCEVAL                                         02000022
*./      DELETE    SEQ1=02020020,SEQ2=02020020                          02020022
CTXGET   EQU   CT2+48    TXGET                                          02040022
ASOPRO   EQU   CT2+40                   ENTRY TO ASSEMBLER OP PROCESSOR 02060022
MACHOP   EQU   CT2+36                   ENTRY TO MACHINE OP PROCESSOR   02080022
PRNT     EQU   CT2+32                   ENTRY TO PRINT ROUTINE          02100022
CTESTR   EQU   CT2+52                                                   02120022
******** ESD FUNCTIONAL ROUTINE ENTRY POINTS ************************** 02140020
CCOM     EQU   CT2+04    COM   (BRENDA)                                 02160020
CCSECT   EQU   CT2+08    CSECT (BRENDA)                                 02180020
CDSECT   EQU   CT2+12    DSECT (BRENDA)                                 02200020
CENTRY   EQU   CT2+16    ENTRY (BRENDA)                                 02220020
*./      DELETE    SEQ1=02240020,SEQ2=02280020                          02280022
CEXTRN   EQU   CT2+20    EXTRN (BRENDA)                                 02300022
CORG     EQU   CT2+24    ORG   (BRENDA)                                 02320022
CQUIT    EQU   CT2+28    QUIT  (BRENDA)                                 02340022
CSTART   EQU   CT2+32    START (BRENDA)                                 02360022
CUPC     EQU   CT2+36    UPC   (BRENDA)                                 02380022
CDXD     EQU   CT2+40    CXD   (BRENDA)                                 02400022
******** CT3 DISPLACEMENT VALUES FOR COMMON SUBROUTINE BASE ADDRESSES * 02420020
CTXTIO   EQU   CT3+00    TEXTIO                                         02440020
CSTGET   EQU   CT3+04    STGET                                          02460020
CSTPUT   EQU   CT3+08    STPUT                                          02480020
CEEVAL   EQU   CT3+12    EEVAL                                          02500020
CLOGER   EQU   CT3+16    LOGERR                                         02520020
CDCGET   EQU   CT3+20    DCGETR                                         02540020
CVCON    EQU   CT3+24    VCON                                           02560020
CTCOMT   EQU   CT3+36                   F/8 PRINT COMMENT               02580020
CTBLDG   EQU   CT3+40                   F/8 PRINT BUILDING              02600020
CSTROM   EQU   CT3+28    STROOM                                         02620020
F8STSC   EQU   CT3+20                                                   02640020
CCONV    EQU   CT3+32                                                   02660020
F8LIGN   EQU   CSTPUT                                                   02680020
F8DCMP   EQU   CVCON                                                    02700020
F8EXPX   EQU   CSTROM                                                   02720020
CSTREF   EQU       CCONV               F7 XREF OUTPUT                   02740020
******** IO SUBROUTINE ENTRY POINT DISPLACEMENT VALUES **************** 02760020
GETPT    EQU    2   (TEXTIO)                                            02780020
GETXTM   EQU    6   (TEXTIO)                                            02800020
PUTXT    EQU   10   (TEXTIO)                                            02820020
CLSTXT   EQU   14   (TEXTIO)                                            02840020
PHCLS    EQU   22   (TEXTIO)                                            02860020
CF7I     EQU   18   (TEXTIO)                                            02880020
PUTXRF   EQU   34   (TEXTIO)                                            02900020
PUTLBT   EQU   38   (TEXTIO)                                            02920020
PUTRLD   EQU   42   (TEXTIO)                                            02940020
GETLAT   EQU   46   (TEXTIO)                                            02960020
WTERR    EQU   50   (TEXTIO)                                            02980020
SYSLST   EQU   54   (TEXTIO)                                            03000020
SYSOUT   EQU   58   (TEXTIO)                                            03020020
CF8I     EQU   CF7I                                                     03040020
         EJECT                                                          03060020
******** CT4 DISPLACEMENT VALUES FOR POINTERS TO COMMON DATA AREAS **** 03080020
CTEXTP   EQU   CT4+00    FIRST BYTE OF CURRENT TEXT RECORD              03100020
CTXOFP   EQU   CT4+04    TEXT OPERAND FIELD LENGTH INDICATOR /TXOPNL/   03120020
CTXABP   EQU   CT4+08    FIRST BYTE OF TEXT APPENDED FIXED FIELD        03140020
CTXWBP   EQU   CT4+12    FIRST SYMBOL WORK BUCKET IN TEXT RECORD        03160020
CTERRP   EQU   CT4+16    ERROR RECORD WORK AREA                         03180020
CTLDCP   EQU   CT4+20    LITERAL DC RECORD WORK AREA                    03200020
CTXWAP   EQU   CT4+24    TEXT RECORD WORK AREA                          03220020
CTSYMP   EQU   CT4+28    SYMBOL TABLE                                   03240020
CTESDP   EQU   CT4+32    ESD TABLE                                      03260020
CTRTBP   EQU   CT4+36    TRANSLATE TABLE                                03280020
CESIOA   EQU   CT4+40    BOTTOM OF MEMORY GOTTEN FROM BUFF BY F7I       03300020
CTESRB   EQU   CT4+44    ESD SEGMENT RESIDENCE TABLE BASE ADDRESS       03320020
CESIOB   EQU   CT4+48    TOP OF MEMORY GOTTEN FROM BUFF                 03340020
CTXIO1   EQU   CT4+52    IO BUFFER 1                                    03360020
CTFVEVAL EQU   CT4+56    POINTER TO F7V/F8V TERMS WORK AREA             03380027
CTXIO3   EQU   CT4+60    IO BUFFER 3                                    03400020
CTXIO4   EQU   CT4+64    IO BUFFER 4                                    03420020
CTESRP   EQU   CT4+68    ESD SEGMENT RESIDENCE ENTRY POINTER            03440020
CFREEP   EQU   CT4+72    SYMBOL TABLE FREE STORAGE                      03460020
CADJBS   EQU   CT4+76    ADJUSTMENT TABLE                               03480020
H1STC    EQU   CT4+80                   PAGE TITLE LINE                 03500020
CTXRFP   EQU   CT4+84              XRF                                  03520020
CTRLDP   EQU   CT4+88              RLD                                  03540020
CTLBTP   EQU   CT4+92              LBT & LAT                            03560020
CTERRB   EQU   CT4+96              PH8 ERROR BUFFER                     03580020
CTESDS   EQU   CT4+100             LAST BYTE OF ESD (600 BYTE BUFFER)   03600020
*        EQU   CT4+104    UNUSED                                        03620022
*        EQU   CT4+108    UNUSED                                        03640022
ADLIST1  EQU   CT4+112    ADDRESS OF LIST1 COMMON AREA IN PHASE ASM     03660022
LSTDCB   EQU   CT4+116                 PTR TO SYSPRINT DCB  *(DCBPRNT)  03680020
PCHDCB   EQU   CT4+120                 PTR TO SYSPUNCH DCB *(DCBPCH)    03700020
GODCB    EQU   CT4+124                 PTR TO SYSLIN DCB  *(DCBLIN)     03720020
*./      DELETE    SEQ1=03740020,SEQ2=03760020                          03760022
         EJECT                                                          03780020
******** CT5 DISPLACEMENT VALUES FOR CENTRAL TABLES AND ITEMS ********* 03800020
CTLOC    EQU   CT5+00     4        CURRENT LOCATION COUNTER             03820020
CTSEQN   EQU   CT5+04     4        CURRENT STATEMENT SEQUENCE NUMBER    03840020
CTLEN    EQU   CT5+08     4        CURRENT STATEMENT LENGTH             03860020
CTITLE   EQU   CT5+12     8        FIRST TITLE NAME FIELD               03880027
STVALU   EQU   CT5+20     4        VALUE FOR STPUT ENTRIES              03900020
CPRIME   EQU   CT5+24     4        PRIME DIVISOR FOR SYMBOL TABLE       03920020
CSTVAL   EQU   CT5+28     4        VALUE FROM START CARD                03940027
CTXLEN   EQU   CT5+32     2        TEXT BLOCK LENGTH                    03960020
CNOESD   EQU   CT5+34     2        NUMBER OF ESDS                       03980020
CENTCT   EQU   CT5+36     2        NUMBER OF ENTRIES                    04000020
CLASID   EQU   CT5+38     2        LAST ID                              04020020
CTNDID   EQU   CT5+40     2        NEXT DSECT ID                        04040020
CESDNO   EQU   CT5+42     2        CURRENT ESD NUMBER                   04060020
CSGCTR   EQU   CT5+44     2        ESD RESIDENT SEGMENT COUNTER         04080020
CPCNO    EQU   CT5+46     2        PRIVATE CODE ESD NUMBER              04100020
CCMNO    EQU   CT5+48     2        COMMON ESD NUMBER                    04120020
STLONG   EQU   CT5+50     2        LENGTH ATTRIBUTE FOR STPUT ENTRIES   04140020
ESSGSZ   EQU   CT5+52     2        ESD SEGMENT SIZE                     04160020
CESDID   EQU   CT5+54     1        CURRENT ESD ID                       04180020
CTPCSW   EQU   CT5+55     1        PRIVATE CODE SWITCH                  04200020
CTCMSW   EQU   CT5+56     1        COMMON SWITCH                        04220020
CFSTID   EQU   CT5+57     1        FIRST CSECT ID                       04240020
CTYPE    EQU   CT5+58     1        CURRENT CSECT TYPE                   04260020
CTLIT2   EQU   CT5+59     1        LTORG OR END CARD SWITCH             04280020
ESDID    EQU   CT5+60     1        ASSIGNED ESD ID                      04300020
ADJCOD   EQU   CT5+61     1        ADJECTIVE CODE                       04320020
CTALIN   EQU   CT5+62     1        ALIGNMENT CODE  0-B,1-H,3-F,7-D      04340020
*        EQU   CT5+64 .0   .1      UNUSED                               04360020
*        EQU   CT5+64 .1   .2      UNUSED                               04380020
*        EQU   CT5+64 .3   .1      UNUSED                               04400020
*        EQU   CT5+64 .4   .1      UNUSED                               04420020
CTERRI   EQU   CT5+64 .5   .1      ERROR RECORD INDICATOR               04440020
CTPH7C   EQU   CT5+64 .6   .1      PHASE 7 COMPLETE INDICATOR           04460020
*        EQU   CT5+64 .7   .1      UNUSED                               04480020
CTSDVI   EQU   CT5+65 .0   .1      SELF DEFINING VALUE INDICATOR        04500026
CTLCRI   EQU   CT5+65 .1   .1      LOCATION COUNTER REFERENCE INDICATOR 04520026
CTPDSI   EQU   CT5+65 .2   .1      DEFINED SYMBOLS REQ. FOR F/7 EEVAL   04540026
CTDORCI  EQU   CT5+65 .3   .1      CURRENTLY IN DSECT OR COM IND.       04560026
CTSCON   EQU   CT5+65 .4   .2      ADDRESS ERROR IN S-CON               04570026
CTEXENSW EQU   CT5+65 .5   .1      ENTRY-EXTRN EQUATED SWITCH           04580026
CTSWXT   EQU   CT5+65 .6   .1      EXTRN-WXTRN MODE SWITCH              04600026
CTLIT3   EQU   CT5+65 .7   .1      TELL IF LITS ARE GEN'D               04620026
         SPACE 2                                                        04630026
CTFUPLI  EQU   CT5+66 .0   .1      FULLUPLIST OPTION INDICATOR          04640026
CTDOSI   EQU   CT5+66 .1   .1      DOS OPTION INDICATOR                 04660026
CTESDI   EQU   CT5+66 .2   .1      ESD OPTION INDICATOR                 04680026
CTBTCHI  EQU   CT5+66 .3   .1      BATCH OPTION INDICATOR               04700026
CTFLLSTI EQU   CT5+66 .4   .1      FULLLIST OPTION INDICATOR            04720026
CTEXTENI EQU   CT5+66 .5   .1      EXTEN OPTION INDICATOR               04725026
CTFXRFI  EQU   CT5+66 .6   .1      FULLXREF OPTION INDICATOR            04730026
CTALGNI  EQU   CT5+66 .7   .1      ALGN OPTION INDICATOR                04735026
         SPACE 2                                                        04740026
CTPCHI   EQU   CT5+67 .0   .1      DECK OPTION INDICATOR                04742026
CTCGOI   EQU   CT5+67 .1   .1      LOAD OPTION INDICATOR                04743026
CTRENTI  EQU   CT5+67 .2   .1      RE-ENTRANT OPTION INDICATOR          04744026
CTLSTI   EQU   CT5+67 .3   .1      LIST OPTION INDICATOR                04745026
CTRLDI   EQU   CT5+67 .4   .1      RLD OPTION INDICATOR                 04746026
CTUPLI   EQU   CT5+67 .5   .1      UPLIST OPTION INDICATOR              04747026
CTXRFI   EQU   CT5+67 .6   .1      X-REF OPTION INDICATOR               04748026
CTTSTI   EQU   CT5+67 .7   .1      TESTRAN OPTION INDICATOR             04749026
         SPACE 2                                                        04750026
CTLREF   EQU   CT5+68 .0   .1      LITERAL CROSS REFERENCE INDICATOR    04751026
CTUPDATE EQU   CT5+68 .1   .1      UPDATE OPTION INDICATOR              04752026
CTEXEC   EQU   CT5+68 .2   .1      EXECUTE OPTION INDICATOR             04753026
CTMXSPCE EQU   CT5+68 .3   .1      SPACE=MAX INDICATOR                  04754026
CTTRMI   EQU   CT5+68 .4   .1      TERM OPTION INDICATOR                04755026
CTNUMI   EQU   CT5+68 .5   .1      LINE NUMBER OPTION INDICATOR         04756026
CTSTMI   EQU   CT5+68 .6   .1      STATEMENT NUMBER OPTION INDICATOR    04757026
CTSEQI   EQU   CT5+68 .7   .1      SEQ NUMBERS FOR SQUISHED FILES V7A43 04758028
         SPACE 2                                                        04759026
CTUMAPI  EQU   CT5+69 .0   .1      USING MAP OPTION INDICATOR           04760027
CTCMSI   EQU   CT5+69 .1   .1      CMS OPTION INDICATOR                 04761027
CTXRFFSI EQU   CT5+69 .2   .1      XREF(FULL/SHORT) INDICATOR           04762027
CTPRTI   EQU   CT5+69 .3   .1      PRINTER OPTION SPECIFIED       V7A44 04763028
CTYFLGI  EQU   CT5+69 .4   .1      Y-CON ERROR FLAG               V7A44 04764028
*        EQU   CT5+69 .5   .1      RESERVED                             04765026
*        EQU   CT5+69 .6   .1      RESERVED                             04766026
CTUNUSED EQU   CT5+69 .7   .1      UNUSED                               04767026
         SPACE 2                                                        04768026
CBDNO    EQU   CT5+70     2        BLANK DSECT ESD NUMBER               04770026
CBDSW    EQU   CT5+72     1        BLANK DSECT ID NO                    04780020
CTSEVCOD EQU   CT5+73     1        HIGHEST SEVERITY CODE (MNOTE/ERROR)  04790026
CTPGLNCT EQU   CT5+74     1        PAGE LINE COUNT                      04800026
CTMRSRTN EQU   CT5+76     4        RETURN TO RTA                        04820027
*        EQU   CT5+80     16       UNUSED                               04830027
CTZERO   EQU   CT5+96     8        TWO FULL WORDS OF ZEROES             04840020
CTWORK   EQU   CT5+104  256        256 BYTE WORK AREA/F8 PUSH,POP USING 04860027
STCHAIN  EQU   CT5+360    4            START OF SYMBOL TABLE CHAIN      04880020
CHAINPT  EQU   CT5+364    4            END   OF SYMBOL TABLE CHAIN      04900020
CTADJEND EQU   CT5+368    4            END OF ADJUSTMENT TABLE          04920020
CTRLBT   EQU   CT5+372    4        FIRST LBT BLOCK PTR ON OVF1          04940020
CTRRLD   EQU   CT5+376    4        FIRST RLD BLOCK PTR ON OVF1          04960020
CTRERR   EQU   CT5+380    4        FIRST ERROR BLOCK (PH8)              04980020
CTCXRF   EQU   CT5+384    2        XRF BLOCK COUNT                      05000020
*        EQU   CT5+386    2        UNUSED                               05020027
CTCRLD   EQU   CT5+388    2        RLD BLOCK COUNT                      05040020
CTCERR   EQU   CT5+390    2        ERROR BLOCK COUNT (PH8)              05060020
CTUSPP   EQU   CT5+392    2        USING PUSH/POP COUNT                 05080027
CTPRPP   EQU   CT5+394    2        PRINT PUSH/POP COUNT                 05100027
STLREF   EQU   CT5+396    4        START OF LITERAL ENTRIES IN S. T.    05110025
CTLITA   EQU   CT5+400   16        CURRENT LITERAL POOL STRING LENGTHS  05120020
CTLITB   EQU   CT5+416   16        CURRENT LITERAL POOL STRING COUNTS   05140020
CTXSAV   EQU   CT5+432        8                                         05160020
CTFSTN   EQU   CT5+440   8         FIRST CSECT NAME                     05180020
CTDATE   EQU   CT5+448    9        DATE FOR LISTING                     05200020
CTENDSW  EQU   CT5+457 1        END CARD PROCESSED IN F8                05210023
CTERRSW  EQU   CT5+458 1        ERROR SWITCH FOR F8P                    05215023
*        EQU   CT5+459 1         UNUSED                                 05220023
CTLINECT EQU   CT5+460    4        PRINT LINE COUNT        1 OF 2       05240020
CTPBUFPT EQU   CT5+464    4        PRINT BUFFER POINTER    2 OF 2       05260020
CADJTB   EQU   CT5+468                  ADJUSTMENT TABLE BASE           05280020
RR2SWH   EQU   CT5+472                  RR2 INSTRUCTION TYPE SWITCH     05300020
ERSWH    EQU   CT5+476                  ERROR SWITCH                    05320020
CESDIDPS EQU   CT5+477                  PSEUDO CURRENT ESD ID           05340020
CTEMERR  EQU   CT5+478                  MNOTE ERRROR INDICATOR          05360020
SPACSW   EQU   CT5+480                  SPACE SWITCH                    05380020
EJCTSW   EQU   CT5+484                  EJECT SWITCH                    05400020
REPSW    EQU   CT5+488                  REPO SWITCH                     05420020
CCRDCT   EQU   CT5+492                  CARD COUNT                      05440020
STUMAP   EQU   CT5+496                  START OF USING ENTRIES IN S.T.  05460027
ENDSWH   EQU   CT5+500                  END SWITCH                      05480020
F8OPRN   EQU   CT5+504                  OPERAND POINTER                 05500020
BUFFPT   EQU   CT5+508                 POINTER TO BUFF ROUTINE          05520020
F8CADJ   EQU   CT5+512                  CURRENT ADJUSTMENT              05540020
ALIGN4   EQU   CT5+516                  FOR ALIGNING                    05560020
F8ALLB   EQU   CT5+520                  FULL WORD OF BITS               05580020
F83BYT   EQU   CT5+524                  3 BYTS OF BITS, LOW ORDER       05600020
F82BYT   EQU   CT5+528                  2 BYTES OF BITS, LOW ORDER      05620020
F81BYT   EQU   CT5+532                  1 BYTE OF BITS, LOW ORDER       05640020
F8PON    EQU   CT5+536                                                  05660020
F8PGEN   EQU   CT5+540                                                  05680020
F8PDAT   EQU   CT5+544                                                  05700020
F8ZERO   EQU   CT5+548                  ONE FULL WORD OF ZERO           05720020
F8INST00 EQU   CT5+552                                                  05740020
F8INST   EQU   CT5+553                  16 DEEP INSTRUCTION BLDG AREA   05760020
F8ZRO    EQU   CT5+572                  ONE FULL WORD OF ZWRO           05780020
PYRSW    EQU   CT5+576                                                  05800020
F8YDC    EQU   CT5+580                                                  05820020
CTESRN   EQU   CT5+584                  ESD SEG COUNT                   05840020
CTWRAP7  EQU   CT5+588                  SEQ FOR WRAPAROUND              05860020
CTSEQ8   EQU   CT5+592                  SEQ FOR WRAP AROUND CHECK       05880020
CTTERMC  EQU   CT5+596    2             MAX NUM TERMS IN F7V/F8V        05900027
CTPARENC EQU   CT5+598    2             MAX PAREN LEVEL IN F7V/F8V      05910027
CTF8LNGT EQU   CT5+600                  STATEMENT LENGTH FOR WRAP AROU  05920020
CTWRAPX  EQU   CT5+604                                                  05940020
CTIDR    EQU   CT5+608  1  IDR SWITCH  40=NO,F1=ONE,F2=TWO IDR ITEMS    05945026
CT2IDR   EQU   CT5+609 19  SECOND IDR IF PRESENT                        05950026
F8WORK   EQU   CT6                                                      05960020
         EJECT                                                          05980020
******** DIMENSIONING FOR INTERNAL TEXT RECORDS *********************** 06000020
*        FIXED FIELD                                                    06020020
TXRL     EQU   0 .0   2.0     RECORD LENGTH                             06040020
TXLRI    EQU   2 .0    .1     LAST RECORD IN BUFFER INDICATOR           06060020
TXRT     EQU   2 .1    .3     RECORD TYPE                               06080020
TXBF     EQU   2 .4    .1     BREAK FLAG                                06100020
TXERI    EQU   2 .5    .1     ERROR RECORD FOLLOWS INDICATOR            06120020
TXESI    EQU   2 .6    .1     EQUAL SIGN INDICATOR                      06140020
TXMARK   EQU   2 .7    .1     PHASE 7 ITERATION POINT FLAG              06160020
TXTO     EQU   3 .0    .2     TYPE OF OPERATION                         06180020
TXEMF    EQU   3 .2    .1     EXTENDED MNEMONIC FLAG                    06200020
TXMDN    EQU   3 .3    .1     MULTIPLY DEFINED NAME INDICATOR           06220020
TXR1M    EQU   3 .4    .4     R1 MASK FOR EXTENDED MNEMONICS            06240020
TXHEX    EQU   4 .0   1.0     MACHINE OP CODE OR ASSEMBLER CODE         06260020
TXASC    EQU   5 .0   1.0     ASSEMBLER SWITCH CODE                     06280020
TXABP    EQU   6 .0   2.0     APPENDED FIXED FIELD POINTER              06300020
*        VARIABLE FIELD                                                 06320020
TXNAML   EQU   8 .0   1.0     NAME FIELD LENGTH                         06340020
TXNAME   EQU   9 .0   V.0     NAME FIELD                                06360020
TXOPL    EQU   0 .0   1.0     OPERATION FIELD LENGTH                    06380020
TXOP     EQU   1 .0   V.0     OPERATION FIELD                           06400020
TXOPNL   EQU   0 .0   1.0     OPERAND FIELD LENGTH                      06420020
TXOPN    EQU   1 .0   V.0     OPERAND FIELD                             06440020
TXCOML   EQU   0 .0   1.0     COMMENTS FIELD LENGTH                     06460020
TXCOM    EQU   1 .0   V.0     COMMENTS FIELD                            06480020
*        APPENDED FIXED FIELD                                           06500020
TXLOC    EQU   0 .0   3.0     LOCATION COUNTER                          06520020
TXURS    EQU   3 .0   1.0     UNRESOLVED SYMBOL COUNTER                 06540020
TXLES    EQU   4 .4    .1     END OF STRING INDICATOR                   06560020
TXSTG    EQU   4 .5    .3     STRING NUMBER                             06580020
TXALIN   EQU   4 .5    .3     ALIGNMENT FOR MACHINE OPS                 06600020
TXFAFL   EQU   5              LENGTH OF FIXED APPENDED FIELD            06620020
*        WORK BUCKET TYPE 1  - LITERAL IN OPERAND FIELD                 06640020
*ZERO    EQU   5 .0    .2     FIRST TWO BITS MUXT BE ZERO               06660020
TXLEVI   EQU   5 .6    .1     LITERAL EVALUATED INDICATOR               06680020
TXLASI   EQU   5 .7    .1     LITERAL ASSIGNED INDICATOR                06700020
TXLSTG   EQU   6 .0   1       LITERAL STRING NUMBER                     06720020
TXLDSP   EQU   7 .0   3       LITERAL STRING DISPLACEMENT               06740020
TXLLEN   EQU  10    1         LITERAL LENGTH ATTRIBUTE                  06760020
*        WORK BUCKET TYPE 2  - 6 BYTE SYMBOL W.B.                       06780026
TXWTYP   EQU   0 .0    .1     WORK BUCKET TYPE  1-TYPE 2, 0-TYPE 1 OR 3 06800020
TXWLEN   EQU   0 .1    .1     WORK BUCKET LENGTH  1- 15BYTES, 0- 6 BYTE 06820020
TXSDOC   EQU   0 .2    .1     SYMBOL DEFINED IN DSECT OR COM INDICATOR  06840020
TXSUBS   EQU   0 .3    .1     VALUE SUBSTITUDED INDICATOR               06860020
TXSLAS   EQU   0 .4    .1     LAST SYMBOL IN OPERAND INDICATOR          06880020
TXSEXI   EQU   0 .5    .1     IMPLIED LENGTH EXCEEDS 256 INDICATOR      06900020
TXSTPC   EQU   0 .6       .2       TO BE DEFINED BY B. BITTNER          06920020
TXSLEN   EQU   1      1       IMPLIED LENGTH                            06940020
TXSESD   EQU   2      1       ESD ID                                    06960020
TXSVAL   EQU   3      3       VALUE                                     06980020
TXSBLN   EQU   4      1       SYMBOL BYTE LENGTH                        07000020
TXSESL   EQU   4 .0  .1       LAST OPERAND IN EXTRN/ENTRY INDICATOR     07020020
TXSPTR   EQU   5      1       POINTER TO SYMBOL IN OPERAND FIELD        07040020
*        WORK BUCKET TYPE 3  - 15 BYTE DC, DS, LDC W.B.                 07060020
*TXWTYP  EQU   0 .0   .1      WORK BUCKET TYPE  1-TYPE 2, 0-TYPE 1 OR 3 07080020
*TXWLEN  EQU   0 .1   .1      WORK BUCKET LENGTH  1-15 BYTES, 0-6 BYTES 07100020
TXDPPI   EQU   0 .2   .1      DC PREVIOUSLY PROCESSED INDICATOR         07120020
TXDLMP   EQU   0 .3   .1      LENGTH MODIFIER PRESENT INDICATOR         07140020
TXDTYP   EQU   1      1       TYPE - TRANSLATED                         07160020
TXDLEN   EQU   2      3       TOTAL LENGTH                              07180020
TXDLAS   EQU   0 .4    .1     LAST OPERAND INDICATOR                    07200020
TXDUPL   EQU   5      3       DUPLICATION FACTOR                        07220020
TXDCON   EQU   8      1       NUMBER OF CONSTANTS                       07240020
TXDPTR   EQU   9      1       POINTER TO FIRST BYTE OF OPERAND          07260020
TXDEXP   EQU  10      1       EXPONENT                                  07280020
TXDSCM   EQU  11      1.3     SCALE MODIFIER                            07300020
TXDSYM   EQU  12 .3    .1     SYMBOL WORK BUCKETS FLAG                  07320020
TXDALN   EQU  12 .4    .3     ALIGNMENT                                 07340020
TXDLMD   EQU  12 .7    .1     LENGTH MODIFIER TYPE  1-BIT, 0-BYTE       07360020
TXDLNM   EQU  13      2       LENGTH MODIFIER VALUE                     07380020
*./      DELETE    SEQ1=07400020,SEQ2=07580020                          07580026
         MEND                                                           07600020
./ ADD NAME=FINISH
         MACRO                                                          00070026
&LABEL   FINISH    &RC=0                                                00080026
&LABEL   L         13,ZZZZSAVE+4       POINT TO CALLER'S SAVE AREA      00090026
         LM        14,12,12(13)        RESTORE CALLER'S REGISTERS       00100026
         LA        15,&RC              SET RETURN CODE                  00110026
         BR        14                  RETURN TO CALLER                 00120026
         MEND                                                           00130026
./ ADD NAME=GETT
         MACRO                                                          00010026
&LABEL   GETT      &INPUT,&LENGTH=80,&EOF=                              00020026
         LCLA      &TLEN                                                00030026
&TLEN    SETA      &LENGTH                                              00040026
         AIF       (&TLEN GE 1).NEXT1                                   00050026
         MNOTE     0,'LENGTH PARAMETER WAS TOO SMALL'                   00060026
&TLEN    SETA      80                                                   00070026
.NEXT1   AIF       (&TLEN LE 80).NEXT2                                  00080026
         MNOTE     0,'LENGTH PARAMETER WAS TOO BIG'                     00090026
&TLEN    SETA      80                                                   00100026
.NEXT2   ANOP                                                           00110026
&LABEL   STM       0,3,ZZZZTEMP        SAVE WORK REGISTERS              00120026
         L         1,ZZZZSAVE          POINTER TO ASMG DCB LIST         00130026
         L         1,16(,1)            POINT TO SYSIN DCB               00140026
         AIF       (K'&EOF EQ 0).NOEOF WAS EOF SPECIFIED                00150026
         MVC       ZZZDUMMY(3),33(1)   SAVE GLOBAL EODAD EXIT           00160026
         LR        2,1                 REMEMBER DCB ADDR OVER GET       00170026
         MVC       33(3,1),=AL3(&EOF)  MODIFY THE SYSIN EODAD           00180026
.NOEOF   ANOP                                                           00190026
         LA        0,ZZZINPUT          POINT TO MY INPUT AREA           00200026
         L         15,48(,1)           LOAD GET ADDRESS FROM DCB        00210026
         BALR      14,15               DO THE GET                       00220026
         AIF       (K'&EOF EQ 0).NOEOD WAS EOF SPECIFIED                00230026
         MVC       33(3,2),ZZZDUMMY    RESTORE EODAD ADDRESS            00240026
.NOEOD   ANOP                                                           00250026
         MVC       &INPUT.(&TLEN),ZZZINPUT TRANSFER TO USER'S AREA      00260026
         LM        0,3,ZZZZTEMP        RESTORE WORK REGISTERS           00270026
         MEND                                                           00280026
./ ADD NAME=INST
         MACRO                                                          00020020
&L       INST  &OP=00,                 OPCODE IN HEX                   ?00040020
               &TYPE=MACH,             MACH, ASSEM, OR EXTEN           ?00060020
               &RPQ67=NO,              YES OR NO                       ?00070023
               &FLOAT=NO,              YES OR NO      MACH OR EXTEN    ?00080020
               &EVEN=NO,               YES/NO/DOUBLE  MACH OR EXTEN    ?00100023
               &ALIGN=C,               C, H, F, OR D  MACH OR EXTEN    ?00120020
               &CLASS=0,               0, 1, 2, OR 3  MACH OR EXTEN    ?00140020
               &LIT1=NO,               YES OR NO      MACH OR EXTEN    ?00160020
               &LIT23=NO,              YES OR NO      MACH OR EXTEN    ?00180020
               &UPC=NO,                YES OR NO      ASSEM            ?00200020
               &STE=NO,                YES OR NO      ASSEM            ?00220020
               &LCR=NO,                YES OR NO      ASSEM            ?00240020
               &F7XREF=NO,             YES OR NO      ASSEM            ?00260020
               &SUB=NO,                YES OR NO      ASSEM            ?00280020
               &F8UPC=NO,              YES OR NO      ASSEM            ?00300020
               &MASK=00                MASK IN HEX    EXTEN OR ASSEM    00320020
.*                                                                      00340020
         GBLA      &GDIND,&CHAINAD(128),&VALUE                          00360020
         GBLB      &CHAINSW(128),&RPQ67SW                               00380023
         GBLC      &CHAR,&LASTL                                         00400027
         LCLA      &V(8),&T,&W1,&B0,&B1,&B2,&B3,&B4,&B5,&B6,&B7         00420020
         LCLC      &C(8),&TM                                            00440020
.*       TEST IF MOD 67 RPQ INST                                        00450023
         AIF       (('&RPQ67' EQ 'YES') AND (NOT &RPQ67SW)).MEND        00455023
.*                 TRANSLATE OP-CODE TO INTERNAL CODE                   00460020
.TRANS   ANOP                                                           00480020
&W1      SETA      &W1+1                                                00500020
&CHAR    SETC      '&L'(&W1,1)                                          00520020
         TRANSLAT                                                       00540020
&V(&W1)  SETA      &VALUE                                               00560020
&C(&W1)  SETC      '&VALUE,'                                            00580020
         AIF       (&W1 NE K'&L).TRANS                                  00600020
&C(&W1)  SETC      '&VALUE'                                             00620020
.*                 HASH THE OPCODE                                      00640020
&W1      SETA      (((&V(1)+&V(5))*256+&V(2)+&V(6))*256+&V(3)+&V(7))*25?00660020
               6+&V(4)+&V(8)                                            00680020
&W1      SETA      &W1-&W1/8209*8209                                    00700020
&W1      SETA      (&W1-&W1/128*128)/2*2                                00720020
.*                 CHAIN TO HASH TABLE                                  00740020
         AIF       (&CHAINSW(&W1+1)).CHAIN1                             00760020
&CHAINSW(&W1+1) SETB 1                                                  00780020
         ORG       PHASHT+&W1                                           00800020
         AGO       .CHAIN2                                              00820020
.*                 CHAIN TO A PREVIOUS ENTRY                            00840020
.CHAIN1  ORG       GLDICT+&CHAINAD(&W1+1)                               00860020
.CHAIN2  DC        AL2(&GDIND)                                          00880020
         ORG                                                            00900020
&CHAINAD(&W1+1) SETA &GDIND                                             00920020
&W1      SETA      (K'&L-1)                                             00940020
         AIF       ('&TYPE' EQ 'MACH').MACH                             00960020
&TM      SETC      'X''&MASK'','                                        00980020
&GDIND   SETA      &GDIND+1                                             01000020
&T       SETA      2                                                    01020020
         AIF       ('&TYPE' EQ 'EXTEN').MACH                            01040020
.*                 ASSEMBLER OPCODE                                     01060020
&T       SETA      1                                                    01080020
         AIF       ('&UPC' EQ 'NO').NO1                                 01100020
&B0      SETA      1                                                    01120020
.NO1     AIF       ('&STE' EQ 'NO').NO2                                 01140020
&B1      SETA      1                                                    01160020
.NO2     AIF       ('&LCR' EQ 'NO').NO3                                 01180020
&B2      SETA      1                                                    01200020
.NO3     AIF       ('&F7XREF' EQ 'NO').NO4                              01220020
&B3      SETA      1                                                    01240020
.NO4     AIF       ('&SUB' EQ 'NO').NO5                                 01260020
&B4      SETA      1                                                    01280020
.NO5     AIF       ('&F8UPC' EQ 'NO').NO6                               01300020
&B7      SETA      1                                                    01320020
         AGO       .NO6                                                 01340020
.*                 MACHINE OR EXTENDED OP-CODE                          01360020
.MACH    AIF       ('&FLOAT' EQ 'NO').NO7                               01380020
&B0      SETA      1                                                    01400020
.NO7     AIF       ('&EVEN' EQ 'NO').NO8                                01420020
         AIF       ('&EVEN' EQ 'DOUBLE').NO8                            01430023
&B1      SETA      1                                                    01440020
.NO8     AIF       ('&ALIGN' EQ 'C').NO9                                01460020
         AIF       ('&ALIGN' EQ 'H').NOA                                01480020
&B2      SETA      1                                                    01500020
         AIF       ('&ALIGN' EQ 'F').NO9                                01520020
.NOA     ANOP                                                           01540020
&B3      SETA      1                                                    01560020
.NO9     ANOP                                                           01580020
&B4      SETA      (&CLASS/2)                                           01600020
&B5      SETA      (&CLASS-&B4*2)                                       01620020
         AIF       ('&LIT23' EQ 'NO').NOB                               01640020
&B6      SETA      1                                                    01660020
.NOB     AIF       ('&LIT1' EQ 'NO').NO6                                01680020
&B7      SETA      1                                                    01700020
.NO6     DC    FL2'0',FL.5'&T',AL.3(&W1),AL1(&C(1)&C(2)&C(3)&C(4)&C(5)&?01720020
               C(6)&C(7)&C(8)),X'&OP',&TM.B'&B0&B1&B2&B3&B4&B5&B6&B7'   01740020
&GDIND   SETA      (&GDIND+K'&L+5)                                      01760020
         AIF       ('&L' EQ 'AIFB' OR '&L' EQ 'AGOB').MEND              01762027
         AIF       ('&L' GT '&LASTL').INCROK                            01764027
         MNOTE     4,'INST -- OPCODE ''&L'' OUT OF ORDER'               01768027
.INCROK  ANOP                                                           01772027
&LASTL   SETC      '&L'                                                 01776027
.MEND    MEND                                                           01780023
./ ADD NAME=INSTSET
         MACRO                                                          00020020
&L       INSTSET                                                        00040020
         GBLA      &GDIND                                               00060020
&GDIND   SETA      1                                                    00080020
&L       START                                                          00100021
*./      DELETE    SEQ1=00110021,SEQ2=00140021                          00140022
*                                                                       00160020
*./      DELETE    SEQ1=00180020,SEQ2=00180020                          00180021
*        THE PERMANENT HASH TABLE (PHASHT) AND THE CONSTANT             00200021
*         GLDLEN ARE MOVED FROM THIS ASMGISXX MODULE INTO THE           00220021
*         COMMON MODULE ASSEMBLED AS PART OF ASMGF1.                    00240021
*./      DELETE    SEQ1=00260020,SEQ2=00260020                          00260021
*                                                                       00280020
GLDLEN   DC        A(GLDEND-GLDICT)     END OF OPCODES (REL)            00300021
*                                                                       00320020
*./      DELETE    SEQ1=00340020,SEQ2=08820020                          08820021
PASHL    EQU       64                  NUMBER OF PERM HASH TBL ENTRIES  08840020
*./      DELETE    SEQ1=08860020,SEQ2=08860020                          08860021
PHASHT   DC        (PASHL)H'0'          PERM (GLOBAL) HASH TABLE        08880020
*                                         (2 BYTES PER ENTRY)           08900020
*./      DELETE    SEQ1=08920020,SEQ2=10060020                          10060021
*********************************************************************** 10080020
         EJECT                                                          10100020
*************** GLOBAL (PERMANENT) DICTIONARY--OP CODES *************** 10120020
*                                                                       10140020
*                                                                       10160020
*                                                                       10180020
.*       SET UP CHARACTER TO INTERNAL CODE TRANSLATION TABLE            10200020
         DEFCHAR                                                        10220020
*                                                                       10240020
*        THE FOLLOWING IS AN EXPLANATION OF THE PARMS ON THE INST MACRO 10260020
*                                                                       10280020
*        0P=XX     OPCODE (MACHINE OR ASSEMBLER INTERNAL) IN HEX        10300020
*        TYPE=ASSEM     ASSEMBLER OP-CODE  EXAMPLE START,DC,ACTR        10320020
*        TYPE=MACH      MACHINE OP-CODE    EXAMPLE BC,LTR,BXLE          10340020
*        TYPE=EXTEN     EXTENDED MNEMONIC  EXAMPLE BE,NOPR,BM           10360020
*                       ALSO SI EX. INSTS  EXAMPLE HDV,SCK,STIDP        10370024
*        MASK=XX   MASK BYTE NEEDED ONLY FOR ASSEM OR EXTEN OPS         10380020
*        RPQ67=YES ONLY IF THIS IS A MOD 67 RPQ INST                    10390023
******  FOLLOWING PARAMETERS NEEDED ONLY FOR MACH OR EXTEN OPS          10400026
*        FLOAT=YES/NO   IF REGISTER SPECS MUST BE .LE. 6                10420020
*        EVEN=YES/NO    IF REGISTER SPECS MUST BE EVEN  EXAMPLE M,D     10440020
*            =DOUBLE    IF REGISTER MUST BE 0 OR 4 (ONLY FOR FLOATING)  10450023
*        CLASS=(0,1,2,3)     TOGETHER WITH OP= DETERMINES OPERAND TYPES 10460020
*                            OP   CLASS          OPERAND FORMAT         10480020
*                            --   -----          --------------         10500020
*                            RR     0       AR    R1,R2                 10520020
*                                   1       SPM   R1                    10540020
*                                   2       SVC   I                     10560020
*                                   3       BR    R2                    10580020
*                            RX     0       D     R1,ADDRX              10600020
*                                   1       B     ADDRX                 10620020
*                            RS/SI  0       LM    R1,R3,ADDR            10640020
*                                   1       SRDA  R1,ADDR               10660020
*                                   2       CLI   ADDR,I                10680020
*                                   3       HIO   ADDR                  10700020
*                            SS     0       AP    ADDR1(L1),ADDR2(L2)   10720020
*                                   1       MVC   ADDR1(L),ADDR2        10740020
*                                   2       XIO   ADDR1(I),ADDR2        10760024
*                                   3       SRP   ADDR1(L1),ADDR2,I3    10765024
*        LIT1=YES/NO    IF FIRST OPERAND MAY BE A LITERAL               10780020
*        LIT23=YES/NO   IF SECOND AND THIRD OPERANDS MAY BE LITERALS    10800020
******  FOLLOWING PARAMATERS NEEDED ONLY FOR ASSEM OPS                  10820020
*        UPC=YES/NO     UNINITIATED PRIVATE CODE                        10840020
*        STE=YES/NO     POSSIBLE SYMBOL TABLE ENTRY                     10860020
*        LCR=YES/NO     LOCATION COUNTER REFERENCE                      10880020
*        F7XREF=YES/NO  SPECIAL PHASE F7 CROSS-REFERENCE                10900020
*        SUB=YES/NO     SUBSTITUTION REQUIRED                           10920020
*        F8UPC=YES/NO   PHASE F8 UNINITIATED PRIVATE CODE               10940020
*                                                                       10960020
*                                                                       10980020
*********************************************************************** 11000020
*                                                                       11020020
*                  IMPORTANT NOTE                                       11040020
*                                                                       11060020
*        TO ADD A NEW INSTRUCTION TO THE ASSEMBLER YOU                  11080020
*         MUST NOT ONLY ADD AN NEW INST TO AN ASMGISXX MODULE,          11100020
*            BUT YOU SHOULD ALSO ADD THE NEW INSTRUCTION TO THE TABLE   11120020
*         AT THE END OF DECK ASMGF7X.                                   11140020
*********************************************************************** 11160020
GLDICT   DC    X'00'     EXTRA BYTE REQUIRED FOR REL REF TO TABLE.      11180020
         MEND                                                           11200020
./ ADD NAME=LIST1EQU   UPDATE DONE
         MACRO                                                          00020026
         LIST1EQU                                                       00040026
*                                                                       00060026
*        SYMBOLIC DISPLACEMENTS INTO LIST1 IN ASMGASM                   00080026
*                                                                       00100026
L1DCBADS EQU       0                   ADDRESSES OF DCBS                00120026
L1SLIN   EQU       0                   .    OFFSET OF SYSLIN            00140026
L1STERM  EQU       4                   .    OFFSET OF SYSTERM           00160026
L1SUP    EQU       8                   .    OFFSET OF SYSUP             00180026
L1SLIB   EQU       12                  .    OFFSET OF SYSLIB            00200026
L1SIN    EQU       16                  .    OFFSET OF SYSIN             00220026
L1SPRINT EQU       20                  .    OFFSET OF SYSPRINT          00240026
L1SPUNCH EQU       24                  .    OFFSET OF SYSPUNCH          00260026
L1SUT1   EQU       28                  .    OFFSET OF SYSUT1            00280026
L1SUT2   EQU       32                  .    OFFSET OF SYSUT2            00300026
L1SUT3   EQU       36                  .    OFFSET OF SYSUT3            00320026
L1VACFL  EQU       L1DCBADS+10*4       VACANT FULL WORD AREA            00340027
L1CTLPRM EQU       L1VACFL+4           CONTROL PROGRAM PARM ADDRESS     00360027
L1TIMELM EQU       L1CTLPRM+4          EXECUTE TIME LIMIT               00380026
L1LINECT EQU       L1TIMELM+4          LINES PER PAGE ON SYSPRINT       00400026
L1PBYT1  EQU       L1LINECT+2          PARBYT1                          00420026
L1PBYT   EQU       L1PBYT1+1           PARBYT                           00440026
L1PBYT2  EQU       L1PBYT+1            PARBYT2                          00460026
L1PBYT3  EQU       L1PBYT2+1           PARBYT3                          00480026
L1SYNERR EQU       L1PBYT3+1           SYNAD EXIT ERROR SWITCHES        00500026
L1IOERR  EQU       L1SYNERR+2          SYNAD EXIT ERROR COUNTS          00520026
L1FINDCT EQU       L1IOERR+4           FINDS ON SYSLIB COUNT            00540026
L1CALIGN EQU       L1FINDCT+4          OFFSET FOR MACRO COMMENTS        00560027
L1BUFADR EQU       L1CALIGN+4          ADDR BUFFER ROUTINE IN BUFF      00580027
L1DDNAM  EQU       L1BUFADR+4          TABLE OF DDNAMES                 00600026
L1PRDATE EQU       L1DDNAM+8*10        DATE IN EBCDIC                   00620026
L1UPCOND EQU       L1PRDATE+9          UPDATE CONDITION CODE            00640027
L1ABEND  EQU       L1UPCOND+1          ABNORMAL ABEND ADDRESS           00660027
L1LSETC  EQU       L1ABEND+8           DEFAULT SETC VARIABLE LENGTH     00680026
L1COLCT  EQU       L1LSETC+1           COLUMN= COUNT FOR XREFS          00700026
L1BUFERR EQU       L1COLCT+1           ADDR BUFF ERROR ROUTINE          00720026
L1SINAD  EQU       L1BUFERR+4          SYSIN MONITOR ADDRESS            00740026
L1FREEMN EQU       L1SINAD+12          FREEMAIN OPERANDS                00760026
L1ISADDR EQU       L1FREEMN+8          ADDR IN INSTRUCTION SET MODULE   00780026
L1ISNAME EQU       L1ISADDR+4          NAME OF INSTRUCTION SET MODULE   00800026
L1PRTIME EQU       L1ISNAME+8          TIME IN EBCDIC                   00820026
L1BTCSUM EQU       L1PRTIME+8          BATCH SUMMARY NUMBER AND ADDRESS 00840026
L1ASPARM EQU       L1BTCSUM+4          LENGTH AND ADDRESS OF SYSPARM    00860026
L1SAVECC EQU       L1ASPARM+4          HIGHEST CONDITION CODE           00880026
L1BLDL   EQU       L1SAVECC+8          BLDL TABLE OF MODULES            00900026
L1LENBL  EQU       36                  LENGTH OF EACH BLDL ENTRY        00920026
L1SINSW  EQU       L1BLDL+7*L1LENBL    SYSIN SWITCH                     00940027
L1RLSDTE EQU       L1SINSW+1           RELEASE DATE                     00960026
L1ASDATE EQU       L1RLSDTE+7          DATE IN INTERNAL CODE            00980026
L1ASTIME EQU       L1ASDATE+8          TIME IN INTERNAL CODE            01000026
L1VERMOD EQU       L1ASTIME+8          ASSEMBLER VERSION AND LEVEL      01020026
L1JULDTE EQU       L1VERMOD+4          JULIAN DATE IN CHARACTER         01040026
L1PID    EQU       L1JULDTE+6          ASSEMBLER PROGRAM I.D.           01060026
L1TRSAVE EQU       L1PID+10            SYSTERM LINE DATA                01080026
L1UTLINO EQU       L1TRSAVE+8          UPDATE FEATURE LINE NUMBER       01100026
L1PAGENO EQU       L1UTLINO+4          PAGE NUMBER ON SYSPRINT          01120026
L1LENMC  EQU       L1PAGENO+4          LENGTH OF MACRO DRCTRY D ALIGNED 01140026
L1BOTMC  EQU       L1LENMC+4           BOTTOM OF MACRO DIRECTORY        01160026
L1TOPMC  EQU       L1BOTMC+4           TOP ENTRY IN MACRO DIRECTORY     01180026
L1OUTRTN EQU       L1TOPMC+4           COMMON OUTPUT ROUTINE            01200026
L1DCBBLK EQU       94                  OFFSET UTILITY JFCB BLKSIZE      01210027
L1DCBRCT EQU       96                  OFFSET OF RECORD COUNT IN DCB    01220026
L1DCBLPT EQU       100                 OFFSET OF LAST RECORD POINTER    01240026
L1DCBCC  EQU       104                 OFFSET OF OUTPUT CARRIAGE IND    01260026
L1DCBPAD EQU       104                 OFFSET OF REAL GET/PUT ADDR      01280027
L1DPTRTN EQU       L1OUTRTN+8          DUMMY SYSPRINT ROUTINE ADDR      01300026
L1BLANKS EQU       L1DPTRTN+2          HEX ZERO AND BLANKS              01320026
L1DBLK   EQU       L1BLANKS+134        ADDR OF DEBLOCK ROUTINE    V7A42 01330028
L1$JOB   EQU       L1DBLK+8            EXECUTE JOB SEPARATOR      V7A42 01340028
L1PATCH  EQU       L1$JOB+6            LIST1 PATCH AREA                 01350027
*                                                                       01360026
         SPACE     2                                                    01380026
         MEND                                                           01400026
./ ADD NAME=MESSAGE
         MACRO                                                          00020020
&S       MESSAGE   &M,&DCB=YES,&SAVE=YES,&TYPE=DEBUG,&CSECT=GSECT       00040020
         GBLB      &NOTSTAT,&NOTBUG                                     00060020
&S       DS        0H                                                   00080020
         AIF       ('&TYPE' EQ 'DEBUG').MES1                            00100020
         AIF       (&NOTSTAT).EXIT                                      00120020
         AGO       .MES2                                                00140020
.MES1    AIF       (&NOTBUG).EXIT                                       00160020
.MES2    AIF      ('&SAVE' NE 'YES').MES3                               00180020
*        THIS MACRO PRINTS ONE LINE                                     00200020
*        IT MUST BE COVERED BY A BASE REGISTER OTHER THAN 1,2,13,14,15  00220020
         STM       14,15,SV&SYSNDX                                      00240020
         L         15,AD&SYSNDX                                         00260020
         USING     &CSECT,15                                            00280020
         BAL       14,T0&SYSNDX                                         00300020
&CSECT   CSECT                                                          00320020
T0&SYSNDX STM      13,2,SAVOUT1                                         00340020
         LR        2,15                                                 00360020
         DROP      15                                                   00380020
         USING     &CSECT,2                                             00400020
         LA        13,SAVOUT2                                           00420020
.MES3    OPEN      (DCBOUT,(OUTPUT))                                    00440020
         PUT       DCBOUT,MS&SYSNDX                                     00460020
         CLOSE     (DCBOUT,)                                            00480020
         FREEPOOL  DCBOUT                                               00500020
         AIF       ('&SAVE' EQ 'NO').MES5                               00520020
         LM        13,2,SAVOUT1                                         00540020
         DROP      2                                                    00560020
         BR        14                                                   00580020
.MES6    ANOP                                                           00600020
MS&SYSNDX DC       CL50&M                                               00620020
         AIF       ('&DCB' EQ 'NO').MES7                                00640020
         DCBOUT                                                         00660020
.MES7    AIF       ('&SAVE' EQ 'NO').MES8                               00680020
&SYSECT  CSECT                                                          00700020
         LM        14,15,SV&SYSNDX                                      00720020
.MES5    B         EX&SYSNDX                                            00740020
         AIF       ('&SAVE' EQ 'NO').MES6                               00760020
SV&SYSNDX DS       2F                                                   00780020
AD&SYSNDX DC       A(&CSECT)                                            00800020
.MES8    ANOP                                                           00820020
EX&SYSNDX DS       0H                                                   00840020
.EXIT    MEND                                                           00860020
./ ADD NAME=PRINTOUT
         MACRO                                                          00020020
&S       PRINTOUT  &M,&DCB=YES,&SAVE=YES,&TYPE=DEBUG,&CSECT=GSECT       00040020
         GBLB      &NOTSTAT,&NOTBUG                                     00060020
&S       DS        0H                                                   00080020
         AIF       ('&TYPE' EQ 'DEBUG').MES1                            00100020
         AIF       (&NOTSTAT).EXIT                                      00120020
         AGO       .MES2                                                00140020
.MES1    AIF       (&NOTBUG).EXIT                                       00160020
.MES2    AIF      ('&SAVE' NE 'YES').MES3                               00180020
*        THIS MACRO PRINTS ONE LINE                                     00200020
*        IT MUST BE COVERED BY A BASE REGISTER OTHER THAN 1,2,13,14,15  00220020
         STM       14,15,SV&SYSNDX                                      00240020
         L         15,AD&SYSNDX                                         00260020
         USING     &CSECT,15                                            00280020
         BAL       14,T0&SYSNDX                                         00300020
&CSECT   CSECT                                                          00320020
T0&SYSNDX STM      13,2,SAVOUT1                                         00340020
         LR        2,15                                                 00360020
         DROP      15                                                   00380020
         USING     &CSECT,2                                             00400020
         LA        13,SAVOUT2                                           00420020
.MES3    OPEN      (DCBOUT,(OUTPUT))                                    00440020
         PUT       DCBOUT,&M                                            00460020
         CLOSE     (DCBOUT,)                                            00480020
         FREEPOOL  DCBOUT                                               00500020
         AIF       (('&SAVE' EQ 'NO') AND ('&DCB' EQ 'NO')).EXIT        00520020
         AIF       ('&SAVE' EQ 'NO').MES5                               00540020
         LM        13,2,SAVOUT1                                         00560020
         DROP      2                                                    00580020
         BR        14                                                   00600020
         AIF       ('&DCB' EQ 'NO').MES6                                00620020
         DCBOUT                                                         00640020
.MES6    ANOP                                                           00660020
&SYSECT  CSECT                                                          00680020
         LM        14,15,SV&SYSNDX                                      00700020
         B         EX&SYSNDX                                            00720020
SV&SYSNDX DS       2F                                                   00740020
AD&SYSNDX DC       A(&CSECT)                                            00760020
         AGO       .MES7                                                00780020
.MES5    B         EX&SYSNDX                                            00800020
         DCBOUT                                                         00820020
.MES7    ANOP                                                           00840020
EX&SYSNDX DS       0H                                                   00860020
.EXIT    MEND                                                           00880020
./ ADD NAME=PRNT    0100-1988153-1988153-1259-00044-00044-00000-CHSY227
         MACRO                                                          00010000
&LABEL   PRNT      &OUTPUT,&NUMBER=8                                    00020000
         LCLA      &TNUM                                                00030000
&TNUM    SETA      &NUMBER                                              00040000
         AIF       (&TNUM GE 1).NEXT1                                   00050000
         MNOTE     0,'NUMBER PARAMETER WAS TOO SMALL'                   00060000
&TNUM    SETA      1                                                    00070000
.NEXT1   AIF       (&TNUM LE 10).NEXT2                                  00080000
         MNOTE     0,'NUMBER PARAMETER WAS TOO BIG'                     00090000
&TNUM    SETA      10                                                   00100000
.NEXT2   ANOP                                                           00110000
&LABEL   STM       0,3,ZZZZTEMP        SAVE FOUR WORK REGISTERS         00120000
         LA        14,&OUTPUT          GET ADDRESS OF SOURCE            00130000
         MVI       ZZOUTPUT,C' '       BLANK OUTPUT FIELD               00140000
         MVC       ZZOUTPUT+1(132),ZZOUTPUT THE WHOLE FIELD             00150000
         LA        15,ZZOUTPUT         POINTER TO OUTPUT FIELD          00160000
         SR        3,3                 ZERO A NUMBER COUNT              00170000
.LOOP1   L         2,0(,14)            GET A NUMBER FOR OUTPUT          00180000
         LTR       2,2                 TEST ITS SIGN                    00190000
         BNL       *+12 .LOOP2         IS IT POSITIVE                   00200000
         MVI       2(15),C'-'          NO IT IS NEGATIVE                00210000
         B         *+8 .LOOP3          BRANCH AROUND                    00220000
.LOOP2   MVI       2(15),C'+'          IT IS POSITIVE                   00230000
.LOOP3   CVD       2,ZZZDUMMY          CONVERT TO DECIMAL               00240000
         UNPK      3(10,15),ZZZDUMMY   CONVERT IT TO ZONED              00250000
         OI        12(15),X'F0'        SET ZONE SIGN POSITIVE           00260000
         LA        14,4(,14)           INCREMENT SOURCE FIELD           00270000
         LA        15,12(,15)          INCREMENT TARGET FIELD           00280000
         LA        3,1(,3)             INCREMENT NUMBER OF NUMBERS      00290000
         LA        2,&TNUM             GET TOTAL NUMBER OF NUMBERS      00300000
         CR        3,2                 ARE WE DONE .Q                   00310000
         BNE       *-54 .LOOP1         NO, DO NEXT NUMBER               00320000
         L         3,ZZZLINES          GET LINES LEFT ON PAGE           00330000
         BCT       3,*+12 .LOOP4       DECREMENT AND TEST               00340000
         LA        3,50                RESTORE PAGE SIZE                00350000
         MVI       ZZOUTPUT,C'1'       SKIP TO NEW PAGE                 00360000
.LOOP4   ST        3,ZZZLINES          UPDATE LINES LEFT COUNT          00370000
         L         1,ZZZZSAVE          POINT TO ASMG DCB LIST           00380000
         L         1,20(,1)            GET SYSPRINT DCB ADDRESS         00390000
         LA        0,ZZOUTPUT          POINT TO OUTPUT LINE             00400000
         L         15,48(,1)           GET PUT ADDRESS FROM DCB         00410000
         BALR      14,15               DO THE PUT                       00420000
         LM        0,3,ZZZZTEMP        RESTORE WORK REGISTERS           00430000
         MEND                                                           00440000
./ ADD NAME=PUTT
         MACRO                                                          00010026
&LABEL   PUTT      &A,&LENGTH=121                                       00020026
         LCLA      &TLEN                                                00030026
&TLEN    SETA      &LENGTH                                              00040026
         AIF       (&TLEN GE 1).NEXT1                                   00050026
         MNOTE     0,'LENGTH PARAMETER TOO SMALL'                       00060026
&TLEN    SETA      121                                                  00070026
.NEXT1   AIF       (&TLEN LE 133).NEXT2                                 00080026
         MNOTE     0,'LENGTH PARAMETER WAS TOO BIG'                     00090026
&TLEN    SETA      133                                                  00100026
.NEXT2   ANOP                                                           00110026
&LABEL   STM       0,3,ZZZZTEMP        SAVE SOME WORK REGISTERS         00120026
         MVI       ZZOUTPUT,C' '       INITIALIZE THE OUTPUT AREA       00130026
         MVC       ZZOUTPUT+1(132),ZZOUTPUT ALL OF IT                   00140026
         MVC       ZZOUTPUT(&LENGTH),&A MOVE OUTPUT LINE                00150026
         L         3,ZZZLINES          LOAD NUMBER OF LINES LEFT        00160026
         CLI       ZZOUTPUT,C'1'       NEW PAGE .Q                      00170026
         BNE       *+12 .LOOP1         BRANCH IF NO                     00180026
         LA        3,1                 ZERO THE LINES LEFT              00190026
         B         *+44 .LOOP5         GO PUT IT OUT                    00200026
.LOOP1   CLI       ZZOUTPUT,C'-'       TRIPLE SPACE .Q                  00210026
         BNE       *+10 .LOOP2         BRANCH IF NOT                    00220026
         BCTR      3,0                 DECREASE LINES BY ONE            00230026
         B         *+12 .LOOP3         BRANCH                           00240026
.LOOP2   CLI       ZZOUTPUT,C'0'       DOUBLE SPACE .Q                  00250026
         BNE       *+10 .LOOP4         BRANCH IF NOT                    00260026
.LOOP3   BCTR      3,0                 DECREASE LINES BY ONE            00270026
         B         *+16 .LOOP5         BRANCH                           00280026
.LOOP4   CLI       ZZOUTPUT,C'+'       NO SPACE AT ALL .Q               00290026
         BNE       *+8 .LOOP5          BRANCH IF NOT                    00300026
         B         *+28 .LOOP6         BRANCH TO SINGLE SPACE           00310026
.LOOP5   BCTR      3,0                 DECREASE LINES BY ONE            00320026
         ST        3,ZZZLINES          UPDATE CURRENT LINE COUNT        00330026
         LTR       3,3                 TEST LINES LEFT                  00340026
         BP        *+16 .LOOP6         BRANCH IF SOME LEFT              00350026
         LA        3,50                RESTORE PAGE COUNT               00360026
         ST        3,ZZZLINES          AND STORE IT                     00370026
         MVI       ZZOUTPUT,C'1'       NEW PAGE                         00380026
.LOOP6   L         1,ZZZZSAVE          POINTER TO ASMG DCB LIST         00390026
         L         1,20(,1)            SYSPRINT DCB ADDRESS             00400026
         LA        0,ZZOUTPUT          OUTPUT POINTER                   00410026
         L         15,48(,1)           LOAD PUT ADDRESS FROM DCB        00420026
         BALR      14,15               DO THE PUT                       00430026
         LM        0,3,ZZZZTEMP        RESTORE WORK REGISTERS           00440026
         MEND                                                           00450026
./ ADD NAME=RD
         MACRO                                                          00010026
&LABEL   RD        &INPUT,&NUMBER=8,&EOF=                               00020026
         LCLA      &TNUM                                                00030026
&TNUM    SETA      &NUMBER                                              00040026
         AIF       (&TNUM GE 1).NEXT1                                   00050026
         MNOTE     0,'NUMBER PARAMETER WAS TOO SMALL'                   00060026
&TNUM    SETA      1                                                    00070026
.NEXT1   AIF       (&TNUM LE 8).NEXT2                                   00080026
         MNOTE     0,'NUMBER PARAMETER WAS TOO BIG'                     00090026
&TNUM    SETA      8                                                    00100026
.NEXT2   ANOP                                                           00110026
&LABEL   STM       0,3,ZZZZTEMP        SAVE WORK REGISTERS              00120026
         L         1,ZZZZSAVE          POINTER TO ASMG DCB LIST         00130026
         L         1,16(,1)            POINT TO SYSIN DCB               00140026
         AIF       (K'&EOF EQ 0).NOEOF                                  00150026
         MVC       ZZZDUMMY(3),33(1)   SAVE GLOBAL EODAD EXIT           00160026
         LR        2,1                 REMEMBER DCB ADDR OVER GET       00170026
         MVC       33(3,1),=AL3(&EOF)  MODIFY THE SYSIN EODAD           00180026
.NOEOF   ANOP                                                           00190026
         LA        0,ZZZINPUT          POINT TO MY INPUT AREA           00200026
         L         15,48(,1)           LOAD GET ADDRESS FROM DCB        00210026
         BALR      14,15               DO THE GET                       00220026
         AIF       (K'&EOF EQ 0).NOEOD WAS EOF SPECIFIED                00230026
         MVC       33(3,2),ZZZDUMMY    RESTORE EODAD ADDRESS            00240026
.NOEOD   ANOP                                                           00250026
         SR        3,3                 ZERO A NUMBER COUNTER            00260026
         LA        14,ZZZINPUT         POINT TO CARD IMAGE              00270026
         LA        15,&INPUT           POINT TO TARGET                  00280026
.LOOP1   NI        9(14),X'0F'         REMOVE THE SIGN                  00290026
         CLI       0(14),C'-'          IS NUMBER NEGATIVE               00300026
         BNE       *+8 .LOOP2          NO                               00310026
         OI        9(14),X'D0'         MAKE IT NEGATIVE                 00320026
.LOOP2   OI        9(14),X'C0'         MAKE IT POSITIVE                 00330026
         PACK      ZZZDUMMY,1(9,14)    CONVERT TO DECIMAL               00340026
         CVB       2,ZZZDUMMY          CONVERT TO BINARY                00350026
         ST        2,0(,15)            ENTER NUMBER IN TARGET           00360026
         LA        15,4(,15)           INCREMENT TARGET                 00370026
         LA        14,10(,14)          INCREMENT CARD FIELD             00380026
         LA        3,1(,3)             INCREMENT NUMBER COUNT           00390026
         LA        2,&TNUM             GET TOTAL NUMBER TO BE CONVERTED 00400026
         CR        3,2                 ARE WE DONE .Q                   00410026
         BNE       *-52 .LOOP1         NO, DO NEXT NUMBER               00420026
         LM        0,3,ZZZZTEMP        RESTORE WORK REGISTERS           00430026
         MEND                                                           00440026
./ ADD NAME=SETR
         MACRO                                                          00020022
         SETR                                                           00040022
         LCLA      &A                                                   00060022
&A       SETA      0                                                    00080022
.S       ANOP                                                           00100022
R&A      EQU       &A                                                   00120022
&A       SETA      &A+1                                                 00140022
         AIF       (&A NE 16).S                                         00160022
         MEND                                                           00180022
./ ADD NAME=SETX
         MACRO                                                          03320020
         SETX                                                           03340020
         LCLA      &A                                                   03360020
&A       SETA      1                                                    03380020
.S       ANOP                                                           03400020
XR&A     DSECT                                                          03420020
         USING     XR&A,R&A                                             03440020
&A       SETA      &A+1                                                 03460020
         AIF       (&A NE 16).S                                         03480020
&SYSECT  CSECT                                                          03500020
         MEND                                                           03520020
./ ADD NAME=DROPR
         MACRO                                                          03560020
         DROPR                                                          03580020
         LCLA      &A                                                   03600020
         AIF       (N'&SYSLIST EQ 0).ALL                                03620020
&A       SETA      1                                                    03640020
.S       AIF       (N'&SYSLIST(&A) EQ 0).EXIT                           03660020
         USING     X&SYSLIST(&A),&SYSLIST(&A)                           03680020
&A       SETA      &A+1                                                 03700020
         AGO       .S                                                   03720020
.ALL     SETX                                                           03740020
.EXIT    MEND                                                           03760020
./ ADD NAME=DROPRUSE
         MACRO                                                          03800020
         DROPRUSE  &OR,&S,&NR                                           03820020
         DROPR     &OR                                                  03840020
         USING     &S,&NR                                               03860020
         MEND                                                           03880020
./ ADD NAME=SETM
         MACRO                                                          03920020
         SETM                                                           03940020
         LCLC      &B1(8),&B2(8),&B3                                    03960020
         LCLA      &WORK,&BITPT                                         03980020
.*       INITIALIZE &B2 TO '1'S AND &B1 TO NUMBERS                      04000020
.LOOP1   ANOP                                                           04020020
&B1(&WORK+1) SETC  '&WORK'                                              04040020
&B2(&WORK+1) SETC  '1'                                                  04060020
&WORK    SETA      &WORK+1                                              04080020
         AIF       (&WORK LE 7).LOOP1                                   04100020
.*       ADD AND RIPPLE                                                 04120020
.LOOP2   ANOP                                                           04140020
&BITPT   SETA      1                                                    04160020
.LOOP3   AIF       ('&B2(&BITPT)' EQ '1').ADD                           04180020
&B2(&BITPT) SETC   '1'                                                  04200020
&WORK    SETA      &BITPT-1                                             04220020
&B1(&BITPT) SETC   '&WORK'                                              04240020
&BITPT   SETA      &BITPT+1                                             04260020
         AIF       (&BITPT EQ 9).EXIT                                   04280020
         AGO       .LOOP3                                               04300020
.ADD     ANOP                                                           04320020
&B2(&BITPT) SETC   '0'                                                  04340020
&B1(&BITPT) SETC   ''                                                   04360020
.*       GENERATE THE EQU                                               04380020
&B3      SETC      '&B2(1)&B2(2)&B2(3)&B2(4)&B2(5)&B2(6)&B2(7)&B2(8)'   04400020
M&B1(1)&B1(2)&B1(3)&B1(4)&B1(5)&B1(6)&B1(7)&B1(8)    EQU     B'&B3'     04420020
         AGO       .LOOP2                                               04440020
.EXIT    MEND                                                           04460020
./ ADD NAME=SNAPDATA
         MACRO                                                          00010000
&NAME    SNAPDATA  &ADDR=,&LENGTH=                                      00020000
         GBLB      &NOTBUG                                              00030000
&NAME    DS        0H                                                   00040000
         AIF       (&NOTBUG).MEND                                       00050000
         ST        &ADDR,SARG&SYSNDX                                    00060000
         ST        &LENGTH,SARG&SYSNDX+4                                00070000
         CNOP      0,4                                                  00080000
         BAL       R8,SNAPDATA                                          00090000
SARG&SYSNDX DS     2F                                                   00100000
.MEND    MEND                                                           00110000
./ ADD NAME=SNAPOUT
         MACRO                                                          00020020
&L       SNAPOUT   &A,&B,&C,&D,&E,&F,&G,&H,&I,&J,&K,&M,&N,&TYPE=DEBUG   00040020
         GBLB      &OPEN1              SET TO 1 FOR SINGLE OPEN         00050023
         GBLB      &NOTSTAT,&NOTBUG                                     00060020
         GBLA      &SNAPCNT                                             00080020
         LCLA      &OPNUM                                               00100020
         LCLC      &WORK                                                00120020
&L       DS        0H                                                   00140020
         AIF       ('&TYPE' EQ 'DEBUG').MES1                            00160020
         AIF       (&NOTSTAT).EXIT                                      00180020
         AGO       .MES2                                                00200020
.MES1    AIF       (&NOTBUG).EXIT                                       00220020
.MES2    ANOP                                                           00240020
&OPNUM   SETA      1                                                    00260020
.*       GENERATE CODING                                                00280020
.LOOP    AIF       (N'&SYSLIST(&OPNUM) EQ 0).CON                        00300020
&WORK    SETC     '&SYSLIST(&OPNUM)'                                    00320020
         AIF       ('&WORK'(1,1) EQ '''').LOOP1                         00340020
         AIF       ('&WORK'(1,1) NE '(').SYMBOL                         00360020
         ST        &WORK,SNAP$1                                         00380020
         AGO       .UNPACK                                              00400020
.SYMBOL  MVC       SNAP$1(4),&SYSLIST(&OPNUM)                           00420020
.UNPACK  UNPK        DC&OPNUM&SYSNDX.(9),SNAP$1(5)                      00440020
         NC        DC&OPNUM&SYSNDX.(9),SNAP$7                           00460020
         TR        DC&OPNUM&SYSNDX.(8),SNAP$8                           00480020
.LOOP1   ANOP                                                           00500020
&OPNUM   SETA      &OPNUM+1                                             00520020
         AGO       .LOOP                                                00540020
.CON     STM       13,1,SNAP$4                                          00560020
         LM        13,14,AD&SYSNDX                                      00580020
         BAL       1,SNAP$0                                             00600020
         LM        13,1,SNAP$4                                          00620020
         B         EX&SYSNDX                                            00640020
&OPNUM   SETA      1                                                    00660020
.*       GENERATE DC                                                    00680020
DC&SYSNDX EQU      *+1                                                  00700020
.LOOP2   AIF       (N'&SYSLIST(&OPNUM) EQ 0).END                        00720020
&WORK    SETC     '&SYSLIST(&OPNUM)'                                    00740020
         DC        C' '                                                 00760020
         AIF       ('&WORK'(1,1) NE '''').PAREN                         00780020
         DC        C&SYSLIST(&OPNUM)                                    00800020
         AGO       .LOOP3                                               00820020
.PAREN   ANOP                                                           00840020
DC&OPNUM&SYSNDX DS 8C                                                   00860020
.LOOP3   ANOP                                                           00880020
&OPNUM   SETA      &OPNUM+1                                             00900020
         AGO       .LOOP2                                               00920020
.END     ANOP                                                           00940020
ND&SYSNDX DS       C                                                    00960020
AD&SYSNDX DC       A(DC&SYSNDX,ND&SYSNDX-DC&SYSNDX-1)                   00980020
         AIF       (&SNAPCNT NE 0).GO                                   01000020
.*       GENERATE OUTPUT ROUTINE ONCE                                   01020020
*        THIS MUST BE COVERED BY A BASE REGISTER OTHER THAN 1,13,14,15  01040020
SNAP$0   MVI       SNAP$2,C' '                                          01060020
         MVC       SNAP$2+1(119),SNAP$2                                 01080020
         EX        14,SNAP$3                                            01100020
         ST        1,SNAP$5                                             01120020
         LA        13,SNAP$6                                            01140020
         AIF       (NOT &OPEN1).OPEN                                    01145023
         TM        SNAP$9+48,X'10'     TEST IF OPEN                     01150023
         BO        SNAP$PUT                                             01155023
.OPEN    OPEN      (SNAP$9,(OUTPUT))                                    01160023
SNAP$PUT PUT       SNAP$9,SNAP$2                                        01180023
         AIF       (&OPEN1).NOCLOSE                                     01190023
         CLOSE     (SNAP$9,)                                            01200020
         FREEPOOL  SNAP$9                                               01220020
.NOCLOSE L         1,SNAP$5                                             01240023
         BR        1                                                    01260020
SNAP$9   DCB       DSORG=PS,MACRF=(PMC),DDNAME=STATDBUG,DEVD=PR,       X01280020
               PRTSP=1,RECFM=F,LRECL=120,BLKSIZE=120,BFTEK=S,          X01300020
               BUFNO=1,BUFL=120,EROPT=ACC                               01320020
SNAP$8   DC        C'0123456789ABCDEF'                                  01340020
SNAP$7   DC        X'0F0F0F0F0F0F0F0F',C' '                             01360020
SNAP$6   DS        18F                                                  01380020
SNAP$5   DS        F                                                    01400020
SNAP$4   DS        5F                                                   01420020
SNAP$3   MVC       SNAP$2(*-*),0(13)                                    01440020
SNAP$2   DS        120C                                                 01460020
SNAP$1   DS        F                                                    01480020
         DC        X'FF'                                                01500020
.GO      ANOP                                                           01520020
EX&SYSNDX DS       0H                                                   01540020
&SNAPCNT SETA      &SNAPCNT+1                                           01560020
.EXIT    MEND                                                           01580020
./ ADD NAME=TIMEOUT
         MACRO                                                          00020020
&S       TIMEOUT   &M,&DCB=YES,&CSECT=GSECT                             00040020
         GBLB      &NOTSTAT                                             00060020
&S       DS        0H                                                   00080020
         AIF        (&NOTSTAT).EXIT                                     00100020
*                  THIS MACRO PRINTS THE TIME                           00120020
*        IT MUST BE COVERED BY A BASE REGISTER OTHER THAN 1,2,13,14,15  00140020
         STM       14,15,SV&SYSNDX                                      00160020
         L         15,AD&SYSNDX                                         00180020
         USING     &CSECT,15                                            00200020
         BAL       14,T0&SYSNDX                                         00220020
&CSECT   CSECT                                                          00240020
T0&SYSNDX STM      13,2,SAVOUT1                                         00260020
         LR        2,15                                                 00280020
         DROP      15                                                   00300020
         USING     &CSECT,2                                             00320020
         LA        13,SAVOUT2                                           00340020
          TIME     BIN                                                  00360020
         CVD       0,T1&SYSNDX                                          00380020
         MVC       T3&SYSNDX,T4&SYSNDX                                  00400020
         ED        T3&SYSNDX,T1&SYSNDX+3                                00420020
         PRINTOUT T2&SYSNDX,DCB=&DCB,SAVE=NO,TYPE=STAT                  00440020
         LM        13,2,SAVOUT1                                         00460020
         DROP      2                                                    00480020
         BR        14                                                   00500020
T1&SYSNDX DS       D                                                    00520020
T2&SYSNDX DC       50C' '                                               00540020
         ORG       T2&SYSNDX                                            00560020
         DC        C&M                                                  00580020
         DC        C' AT TIME ='                                        00600020
T3&SYSNDX DS       CL11                                                 00620020
         DC        C' SECONDS'                                          00640020
         ORG       T2&SYSNDX+50                                         00660020
T4&SYSNDX DC       C' '                                                 00680020
         DC        X'20202020202120'                                    00700020
         DC        C'.'                                                 00720020
         DC        X'2020'                                              00740020
&SYSECT  CSECT                                                          00760020
         LM        14,15,SV&SYSNDX                                      00780020
         B         EX&SYSNDX                                            00800020
SV&SYSNDX DS       2F                                                   00820020
AD&SYSNDX DC       A(&CSECT)                                            00840020
EX&SYSNDX DS       0H                                                   00860020
.EXIT    MEND                                                           00880020
./ ADD NAME=TRANSLAT
         MACRO                                                          00020020
         TRANSLAT                                                       00040020
         GBLA      &EBDIC(64),&NUMCHAR,&VALUE                           00060020
         GBLC      &CHARSET(64),&CHAR                                   00080020
         LCLA      &I                                                   00100020
&I       SETA      0                                                    00120020
.ADDR10  ANOP                                                           00140020
&I       SETA      &I+1                                                 00160020
         AIF   ('&CHAR'(1,1) EQ '&CHARSET(&I)'(1,1)).ADDR20             00180020
         AIF       (&I LT &NUMCHAR).ADDR10                              00200020
&VALUE   SETA      0                                                    00220020
         AGO       .END                                                 00240020
.ADDR20  ANOP                                                           00260020
&VALUE   SETA      &EBDIC(&I)                                           00280020
.END     MEND                                                           00300020
./ ADD NAME=UPEMSG  UPDATE DONE
         MACRO                                                          00020027
         UPEMSG    &ERNO,&SVC,&MSG     GENERATE ASMGUP ERRORS           00040027
         GBLA      &NBR                ERROR NUMBER IN BINARY           00060027
         GBLC      &CNBR               ERROR NUMBER IN CHARACTER        00080027
         LCLA      &CNT                WORK FOR LENGTH OF MESSAGE       00100027
&NBR     SETA      &ERNO               TRANSFER ERROR NUMBER            00120027
.*                                                                      00140027
.*       CONVERT NUMBER TO CHARACTER                                    00160027
         ELZERO                                                         00180027
&NBR     SETA      &NBR-200            OFFSET NUMBERS FROM 200          00200027
         AIF       ('&SVC' EQ '').SKIP02                          V7A48 00202028
         AIF       ('&MSG' EQ '').SKIP01                                00220027
&CNT     SETA      K'&MSG-3            GET LENGTH-1 OF MESSAGE          00240027
$LIT&CNBR DC       AL1(&NBR,&CNT,&SVC),C&MSG                            00260027
         MEXIT                                                          00280027
.SKIP01  ANOP                                                           00300027
&CNT     SETA      0                                                    00320027
$LIT&CNBR DC       AL1(&NBR,&CNT,&SVC),C' '                             00340027
         MEXIT     ,                                              V7A48 00342028
.SKIP02  ANOP      ,                                              V7A48 00344028
         L         R3,=A(UPERROR)      ERROR BASE                 V7A48 00346028
         LA        R3,$LIT&CNBR-UPERROR(,R3)  ERROR TEXT          V7A48 00348028
         MEND                                                           00360027
./ ADD NAME=SETN
         MACRO                                                          04500020
         SETN                                                           04520020
         LCLC      &B1(8),&B2(8),&B3                                    04540020
         LCLA      &WORK,&BITPT                                         04560020
.*       INITIALIZE &B2 TO '0'S AND &B1 TO NUMBERS                      04580020
.LOOP1   ANOP                                                           04600020
&B1(&WORK+1) SETC  '&WORK'                                              04620020
&B2(&WORK+1) SETC  '0'                                                  04640020
&WORK    SETA      &WORK+1                                              04660020
         AIF       (&WORK LE 7).LOOP1                                   04680020
.*       ADD AND RIPPLE                                                 04700020
.LOOP2   ANOP                                                           04720020
&BITPT   SETA      1                                                    04740020
.LOOP3   AIF       ('&B2(&BITPT)' EQ '0').ADD                           04760020
&B2(&BITPT) SETC   '0'                                                  04780020
&WORK    SETA      &BITPT-1                                             04800020
&B1(&BITPT) SETC   '&WORK'                                              04820020
&BITPT   SETA      &BITPT+1                                             04840020
         AIF       (&BITPT EQ 9).EXIT                                   04860020
         AGO       .LOOP3                                               04880020
.ADD     ANOP                                                           04900020
&B2(&BITPT) SETC   '1'                                                  04920020
&B1(&BITPT) SETC   ''                                                   04940020
.*       GENERATE THE EQU                                               04960020
&B3      SETC      '&B2(1)&B2(2)&B2(3)&B2(4)&B2(5)&B2(6)&B2(7)&B2(8)'   04980020
N&B1(1)&B1(2)&B1(3)&B1(4)&B1(5)&B1(6)&B1(7)&B1(8)    EQU     B'&B3'     05000020
         AGO       .LOOP2                                               05020020
.EXIT    MEND                                                           05040020
./ ADD NAME=ALIGN
         MACRO                                                          05080020
&L       ALIGN     &R,&B,&INC=0                                         05100020
.*       THIS MACRO WILL NOT WORK ON R0                                 05120020
         AIF       ('&B' NE 'H').F                                      05140020
&L       LA        &R,&INC+1(0,&R)                                      05160020
         N         &R,=X'FFFFFFFE'                                      05180020
         AGO       .EXIT                                                05200020
.F       AIF       ('&B' NE 'F').D                                      05220020
&L       LA        &R,&INC+3(0,&R)                                      05240020
         N         &R,=X'FFFFFFFC'                                      05260020
         AGO       .EXIT                                                05280020
.D       AIF       ('&B' EQ 'D').D1                                     05300020
         MNOTE     'ALIGN ERROR. &B NOT VALID SECOND ARGUMENT'          05320020
.D1      ANOP                                                           05340020
&L       LA        &R,&INC+7(0,&R)                                      05360020
         N         &R,=X'FFFFFFF8'                                      05380020
.EXIT    MEND                                                           05400020
./ ADD NAME=ACCUM
         MACRO                                                          05440020
&L       ACCUM     &S                                                   05460020
         GBLB      &NOTSTAT                                             05480020
&L       DS        0H                                                   05500020
         AIF       (&NOTSTAT).EXIT                                      05520020
         AP        &S.(5),=PL2'10'                                      05540020
.EXIT    MEND                                                           05560020
./ ADD NAME=LINKCHK
         MACRO                                                          05560821
         LINKCHK                                                        05561221
         GBLB      &LINKCHK                                             05561621
.*       THIS CODE VERIFIES THE VALIDITY OF THE CML                     05562021
         AIF       (NOT &LINKCHK).END                                   05562421
.*       SAVE REGISTERS                                                 05562821
         STM       R8,R10,LSAV&SYSNDX                                   05563221
.*       CHECK THE UP POINTERS                                          05563621
         LA        R8,CORELIST                                          05564021
         LR        R9,R8                                                05564421
         LR        R10,R8                                               05564821
         L         R9,FCPPTUP-FCP(0,R9)                                 05565221
         L         R10,CMLPTUP-CML(0,R10)                               05565621
         CR        R10,R9                                               05566021
         BNE       *-6                                                  05566421
         CR        R10,R8                                               05566821
         BNE       *-16                                                 05567221
.*       CHECK THE DOWN POINTERS                                        05567621
         L         R9,FCPPTDWN-FCP(0,R9)                                05568021
         L         R10,CMLPTDWN-CML(0,R10)                              05568421
         LA        R10,0(0,R10)                                         05568821
         CR        R10,R9                                               05569221
         BNE       *-10                                                 05569621
         CR        R10,R8                                               05570021
         BNE       *-20                                                 05570421
.*       RESTORE REGISTERS                                              05570821
         LM        R8,R10,LSAV&SYSNDX                                   05571221
         CNOP      0,4                                                  05571621
         B         *+16                                                 05572021
LSAV&SYSNDX DC     3F'0'                                                05572421
.END     MEND                                                           05572821
./ ADD NAME=REF
         MACRO                                                          05600020
         REF                                                            05620020
         LCLA      &WORK                                                05640020
&WORK    SETA      1                                                    05660020
.LOOP    AIF       (T'&SYSLIST(&WORK) EQ 'O').EXIT                      05680020
         DS        0AL4(&SYSLIST(&WORK))                                05700020
&WORK    SETA      &WORK+1                                              05720020
         AGO       .LOOP                                                05740020
.EXIT    MEND                                                           05760020
./ ADD NAME=BALR0
         MACRO                                                          05960020
&L       BALR0     &R                                                   05980020
&L       BALR      &R,0                                                 06000020
         MEND                                                           06020020
./ ADD NAME=BCTR0
         MACRO                                                          06040020
&L       BCTR0     &R                                                   06060020
&L       BCTR      &R,0                                                 06080020
         MEND                                                           06100020
./ ADD NAME=BN
         MACRO                                                          06140020
&L       BN        &A                                                   06160020
&L       BC        4,&A            BRANCH NEGATIVE                      06180020
         MEND                                                           06200020
./ ADD NAME=BNLC
         MACRO                                                          06220020
&L       BNLC      &S                                                   06240020
&L       BC        12,&S           BRANCH NOT LOGICAL CARRY             06260020
         MEND                                                           06280020
./ ADD NAME=BZR
         MACRO                                                          06300020
&L       BZR       &R                                                   06320020
&L       BCR       8,&R            BRANCH ZERO REGISTER                 06340020
         MEND                                                           06360020
./ ADD NAME=BNER
         MACRO                                                          06380020
&L       BNER      &R                                                   06400020
&L       BCR       7,&R            BRANCH NOT EQUAL REGISTER            06420020
         MEND                                                           06440020
./ ADD NAME=BNZR
         MACRO                                                          06460020
&L       BNZR      &R                                                   06480020
&L       BCR       7,&R            BRANCH NOT ZERO REGISTER             06500020
         MEND                                                           06520020
./ ADD NAME=BLR
         MACRO                                                          06540020
&L       BLR       &R                                                   06560020
&L       BCR       4,&R            BRANCH LOW REGISTER                  06580020
         MEND                                                           06600020
./ ADD NAME=BER
         MACRO                                                          06620020
&L       BER       &R                                                   06640020
&L       BCR       8,&R            BRANCH EQUAL REGISTER                06660020
         MEND                                                           06680020
./ ADD NAME=BNLR
         MACRO                                                          06700020
&L       BNLR      &R                                                   06720020
&L       BCR       11,&R           BRANCH NOT LOW REGISTER              06740020
         MEND                                                           06760020
./ ADD NAME=TMBZ
         MACRO                                                          06800020
&L       TMBZ      &S,&M,&B                                             06820020
&L       TM        &S,&M           TEST UNDER MASK AND                  06840020
         BZ        &B              BRANCH ZERO                          06860020
         MEND                                                           06880020
./ ADD NAME=TMBNZ
         MACRO                                                          06900020
&L       TMBNZ     &S,&M,&B                                             06920020
&L       TM        &S,&M           TEST UNDER MASK AND                  06940020
         BNZ       &B              BRANCH NOT ZERO                      06960020
         MEND                                                           06980020
./ ADD NAME=TMBZR
         MACRO                                                          07000020
&L       TMBZR     &S,&M,&R                                             07020020
&L       TM        &S,&M           TEST UNDER MASK AND                  07040020
         BZR       &R              BRANCH ZERO REGISTER                 07060020
         MEND                                                           07080020
./ ADD NAME=TMRBZ
         MACRO                                                          07120020
&L       TMRBZ     &R,&M,&B                                             07140020
&L       STC       &R,*+5          TEST UNDER                           07160020
         TM        =AL1(&M),*-*    MASK REGISTER AND                    07180020
         BZ        &B              BRANCH ZERO                          07200020
         MEND                                                           07220020
./ ADD NAME=TMRBNZ
         MACRO                                                          07240020
&L       TMRBNZ    &R,&M,&B                                             07260020
&L       STC       &R,*+5          TEST UNDER                           07280020
         TM        =AL1(&M),*-*    MASK REGISTER AND                    07300020
         BNZ       &B              BRANCH NOT ZERO                      07320020
         MEND                                                           07340020
./ ADD NAME=LTAND
         MACRO                                                          07380020
&L       LTAND     &R,&S                                                07400020
&L       L         &R,&S           LOAD AND                             07420020
         LTR       &R,&R           TEST AND                             07440020
         MEND                                                           07460020
./ ADD NAME=LTBZ
         MACRO                                                          07500020
&L       LTBZ      &R,&S,&B                                             07520020
&L       LTAND     &R,&S           LOAD AND TEST AND                    07540020
         BZ        &B              BRANCH ZERO                          07560020
         MEND                                                           07580020
./ ADD NAME=LTBNZ
         MACRO                                                          07600020
&L       LTBNZ     &R,&S,&B                                             07620020
&L       LTAND     &R,&S           LOAD AND TEST AND                    07640020
         BNZ       &B              BRANCH NOT ZERO                      07660020
         MEND                                                           07680020
./ ADD NAME=LTBP
         MACRO                                                          07700020
&L       LTBP      &R,&S,&B                                             07720020
&L       LTAND     &R,&S           LOAD AND TEST AND                    07740020
         BP        &B              BRANCH POSITIVE                      07760020
         MEND                                                           07780020
./ ADD NAME=LTBZR
         MACRO                                                          07820020
&L       LTBZR     &R,&S,&B                                             07840020
&L       LTAND     &R,&S           LOAD AND TEST AND                    07860020
         BZR       &B              BRANCH ZERO REGISTER                 07880020
         MEND                                                           07900020
./ ADD NAME=LTRBZ
         MACRO                                                          07940020
&L       LTRBZ     &R,&B                                                07960020
&L       LTR       &R,&R           LOAD AND TEST REGISTER AND           07980020
         BZ        &B              BRANCH ZERO                          08000020
         MEND                                                           08020020
./ ADD NAME=LTRBNL
         MACRO                                                          08040020
&L       LTRBNL    &R,&B                                                08060020
&L       LTR       &R,&R           LOAD AND TEST REGISTER AND           08080020
         BNL       &B              BRANCH NOT LOW                       08100020
         MEND                                                           08120020
./ ADD NAME=CBNH
         MACRO                                                          08160020
&L       CBNH      &R,&S,&B                                             08180020
&L       C         &R,&S           COMPARE AND                          08200020
         BNH       &B              BRANCH NOT HIGH                      08220020
         MEND                                                           08240020
./ ADD NAME=CBNL
         MACRO                                                          08260020
&L       CBNL      &R,&S,&B                                             08280020
&L       C         &R,&S           COMPARE AND                          08300020
         BNL       &B              BRANCH NOT LOW                       08320020
         MEND                                                           08340020
./ ADD NAME=CBL
         MACRO                                                          08360020
&L       CBL       &R,&S,&B                                             08380020
&L       C         &R,&S           COMPARE AND                          08400020
         BL        &B              BRANCH LOW                           08420020
         MEND                                                           08440020
./ ADD NAME=CBE
         MACRO                                                          08460020
&L       CBE       &R,&S,&B                                             08480020
&L       C         &R,&S           COMPARE AND                          08500020
         BE        &B              BRANCH EQUAL                         08520020
         MEND                                                           08540020
./ ADD NAME=CBH
         MACRO                                                          08560020
&L       CBH       &R,&S,&B                                             08580020
&L       C         &R,&S           COMPARE AND                          08600020
         BH        &B              BRANCH HIGH                          08620020
         MEND                                                           08640020
./ ADD NAME=CBNE
         MACRO                                                          08660020
&L       CBNE      &R,&S,&B                                             08680020
&L       C         &R,&S           COMPARE AND                          08700020
         BNE       &B              BRANCH NOT EQUAL                     08720020
         MEND                                                           08740020
./ ADD NAME=CBLR
         MACRO                                                          08780020
&L       CBLR      &R,&S,&B                                             08800020
&L       C         &R,&S           COMPARE AND                          08820020
         BLR       &B              BRANCH LOW REGISTER                  08840020
         MEND                                                           08860020
./ ADD NAME=CBNLR
         MACRO                                                          08880020
&L       CBNLR     &R,&S,&B                                             08900020
&L       C         &R,&S           COMPARE AND                          08920020
         BNLR      &B              BRANCH NOT LOW REGISTER              08940020
         MEND                                                           08960020
./ ADD NAME=CLBNL
         MACRO                                                          09000020
&L       CLBNL     &R,&S,&B                                             09020020
&L       CL        &R,&S           COMPARE LOGICAL AND                  09040020
         BNL       &B              BRANCH NOT LOW                       09060020
         MEND                                                           09080020
./ ADD NAME=CLBL
         MACRO                                                          09100020
&L       CLBL      &R,&S,&B                                             09120020
&L       CL        &R,&S           COMPARE LOGICAL AND                  09140020
         BL        &B              BRANCH LOW                           09160020
         MEND                                                           09180020
./ ADD NAME=CRBNH
         MACRO                                                          09220020
&L       CRBNH     &R1,&R2,&B                                           09240020
&L       CR        &R1,&R2         COMPARE REGISTER AND                 09260020
         BNH       &B              BRANCH NOT HIGH                      09280020
         MEND                                                           09300020
./ ADD NAME=CRBH
         MACRO                                                          09320020
&L       CRBH      &R1,&R2,&B                                           09340020
&L       CR        &R1,&R2         COMPARE REGISTER AND                 09360020
         BH        &B              BRANCH HIGH                          09380020
         MEND                                                           09400020
./ ADD NAME=CRBE
         MACRO                                                          09420020
&L       CRBE      &R1,&R2,&B                                           09440020
&L       CR        &R1,&R2         COMPARE REGISTER AND                 09460020
         BE        &B              BRANCH EQUAL                         09480020
         MEND                                                           09500020
./ ADD NAME=CRBL
         MACRO                                                          09520020
&L       CRBL      &R1,&R2,&B                                           09540020
&L       CR        &R1,&R2         COMPARE REGISTER AND                 09560020
         BL        &B              BRANCH LOW                           09580020
         MEND                                                           09600020
./ ADD NAME=OPCD
         MACRO                                                          00060027
&LABEL   OPCD  &OP=XX,                                                 X00080027
               &TYPE=MACH,             ASSEM,EXTEN,                    X00100027
               &ILLGEN=NO,             YES,                            X00120027
               &NAME=,                 YES,NO,                         X00140027
               &OPND=,                 YES,NO,                         X00160027
               &IS=,                                                   X00180027
               &MASK=,                 ONLY IF EXTEN,                  X00200027
               &ALIGN=C,               H,F,D,               *          X00220027
               &CLASS=,                0,1,2,3,             *          X00240027
               &EVEN=NO,               YES,DOUBLE,          *ONLY      X00260027
               &FLOAT=NO,              YES,                 *FOR       X00280027
               &LIT1=NO,               YES,                 *MACHINE   X00300027
               &LIT23=NO,              YES                  *TYPES     X00320027
               &F7XREF=NO,             YES,       *ONLY                X00340027
               &F8UPC=NO,              YES,       *FOR                 X00360027
               &LCR=NO,                YES,       *ASSEM               X00380027
               &STE=NO,                YES,       *TYPES               X00400027
               &SUB=NO,                YES,       *                    X00420027
               &UPC=NO                 YES        *                     00440027
         GBLA      &VALUE,&GLENLAB(400),&GTXTO(400),&GTXASC(400)        00460027
         GBLA      &GDIM,&ERROR,&FIRST                                  00480027
         GBLB      &RPQ67,&SYM370                                       00500027
         GBLC      &CHAR,&LASTLAB                                       00520027
         GBLC      &GLABEL(400),&GOP(400)                               00540027
         LCLA      &S1,&TDIM,&W1                                        00560027
         LCLC      &WC                                                  00580027
.*                 CHECK IF THIS IS FIRST TIME IN                       00600027
         AIF       (&FIRST NE 0).SKIP01                                 00620027
         DEFCHAR                                                        00640027
&FIRST   SETA      1                                                    00660027
&LASTLAB SETC      '........'                                           00670027
.SKIP01  ANOP                                                           00680027
.* DAN SKOMSKY 10/23/2007 CLEAN UP LOGIC TO TEST ALPHA ORDER  DS102307  00682027
.* &WC   SETC      '&LABEL'.'       '(1,8)                    DS102307  00700027
         AIF       (T'&LABEL EQ 'O').MNOTE1                   DS102307  00702027
&WC      SETC      '&LABEL'.'.......'(1,7)                    DS102307  00704027
&WC      SETC      '&WC'(1,8)                                 DS102307  00706027
         AIF       ('&WC' GT '&LASTLAB').SKIP02                         00720027
         MNOTE     1,'OPCD -- OPCODE LABEL &LABEL OUT OF ALPHA ORDER'   00740027
         MNOTE     *,'WC = &WC'                                         00750027
         MNOTE     *,'LASTLAB = &LASTLAB'                               00755027
.SKIP02  ANOP                                                           00760027
.* DAN SKOMSKY 10/23/2007 CLEAN UP LOGIC TO TEST ALPHA ORDER  DS102307  00762027
.*       MNOTE     *,'WC = &WC'            <== DEBUG     ONLY DS102307  00764027
.*       MNOTE     *,'LASTLAB = &LASTLAB'  <==       USE      DS102307  00766027
&LASTLAB SETC      '&WC'                                                00780027
.*                 DETERMINE IF OPCODE IN THIS SET                      00800027
         AIF       ('&IS' EQ '').OPOK                                   00820027
&W1      SETA      N'&IS                                                00840027
         AIF       (&W1 EQ 1).SINGTST                                   00860027
.SKIP03  AIF       ('&IS(&W1)' NE '67').TST70                           00880027
         AIF       (&RPQ67).OPOK                                        00900027
         AGO       .TSTEND                                              00920027
.TST70   AIF       ('&IS(&W1)' NE '70').OPOK                            00940027
         AIF       (&SYM370).OPOK                                       00960027
.TSTEND  ANOP                                                           00980027
&W1      SETA      &W1-1                                                01000027
         AIF       (&W1 GT 0).SKIP03                                    01020027
         AGO       .NOTGEND                                             01040027
.SINGTST ANOP                                                           01060027
.*                 TEST IF MOD 67 RPQ INSTRUCTION                       01080027
         AIF       (('&IS' EQ '67') AND (NOT &RPQ67)).NOTGEND           01100027
.*                 TEST IF 370 INSTRUCTION                              01120027
         AIF       (('&IS' EQ '70') AND (NOT &SYM370)).NOTGEND          01140027
.OPOK    ANOP                                                           01160027
.*                 SET LABEL AND OPCODE                                 01180027
&TDIM    SETA      &GDIM+1                                              01200027
&W1      SETA      K'&LABEL                                             01220027
         AIF       (&W1 EQ 0).MNOTE1                                    01240027
&GLENLAB(&TDIM) SETA &W1                                                01260027
&GLABEL(&TDIM) SETC '&LABEL'                                            01280027
         AIF       ('&OP' EQ 'XX').MNOTE5                               01300027
&GOP(&TDIM) SETC '&OP'                                                  01320027
.*                 CHECK THE INSTRUCTION TYPE                           01340027
         AIF       ('&TYPE' NE 'ASSEM').TXTO1                           01360027
&S1      SETA      X'80'                                                01380027
         AGO       .TXTOF                                               01400027
.TXTO1   AIF       ('&TYPE' NE 'MACH').TXTO2                            01420027
&S1      SETA      X'40'                                                01440027
         AGO       .TXTOF                                               01460027
.TXTO2   AIF       ('&TYPE' NE 'EXTEN').MNOTE2                          01480027
&S1      SETA      X'60'                                                01500027
.TXTOF   ANOP                                                           01520027
.*                 IS OPCODE ILLEGAL IF GENNED .Q                       01540027
         AIF       ('&ILLGEN' EQ 'NO').TXRM0                            01560027
         AIF       ('&TYPE' NE 'ASSEM').MNOTE9                          01580027
&S1      SETA      &S1+X'20'                                            01600027
.*                 NAME REQUIRED OR NOT ALLOWED                         01620027
.TXRM0   AIF       ('&TYPE' EQ 'EXTEN').TXRM7                           01640027
         AIF       ('&NAME' EQ '').TXRM3                                01660027
         AIF       ('&NAME' NE 'YES').TXRM2                             01680027
&S1      SETA      &S1+X'08'                                            01700027
         AGO       .TXRM3                                               01720027
.TXRM2   AIF       ('&NAME' NE 'NO').MNOTE3                             01740027
&S1      SETA      &S1+X'04'                                            01760027
.*                 OPERAND REQUIRED OR NOT ALLOWED                      01780027
.TXRM3   AIF       ('&OPND' EQ '').TXRM8                                01800027
         AIF       ('&OPND' NE 'YES').TXRM5                             01820027
&S1      SETA      &S1+X'02'                                            01840027
         AGO       .TXRM8                                               01860027
.TXRM5   AIF       ('&OPND' NE 'NO').MNOTE4                             01880027
&S1      SETA      &S1+X'01'                                            01900027
         AGO       .TXRM8                                               01920027
.*                 SET EXTEN MASK                                       01940027
.TXRM7   ANOP                                                           01960027
         AIF       ('&MASK' EQ '').MNOTE6                               01980027
&W1      SETA      K'&MASK                                              02000027
&CHAR    SETC      '&MASK'(&W1,1)                                       02020027
         TRANSLAT                                                       02040027
&S1      SETA      &S1+&VALUE                                           02060027
.TXRM8   ANOP                                                           02080027
&GTXTO(&TDIM) SETA &S1                                                  02100027
.*                 TEST IF ASSEM OR NOT                                 02120027
&S1      SETA      0                                                    02140027
         AIF       ('&TYPE' NE 'ASSEM').MACH1                           02160027
         AIF       ('&UPC' EQ 'NO').ASSEM1                              02180027
&S1      SETA      &S1+X'80'                                            02200027
.ASSEM1  AIF       ('&STE' EQ 'NO').ASSEM2                              02220027
&S1      SETA      &S1+X'40'                                            02240027
.ASSEM2  AIF       ('&LCR' EQ 'NO').ASSEM3                              02260027
&S1      SETA      &S1+X'20'                                            02280027
.ASSEM3  AIF       ('&F7XREF' EQ 'NO').ASSEM4                           02300027
&S1      SETA      &S1+X'10'                                            02320027
.ASSEM4  AIF       ('&SUB' EQ 'NO').ASSEM5                              02340027
&S1      SETA      &S1+X'08'                                            02360027
.ASSEM5  AIF       ('&F8UPC' EQ 'NO').SET1                              02380027
&S1      SETA      &S1+X'01'                                            02400027
         AGO       .SET1                                                02420027
.*                 MACHINE OR EXTEN INSTRUCTION                         02440027
.MACH1   AIF       ('&FLOAT' EQ 'NO').MACH2                             02460027
&S1      SETA      &S1+X'80'                                            02480027
.MACH2   AIF       ('&EVEN' EQ 'NO').MACH3                              02500027
         AIF       ('&EVEN' EQ 'DOUBLE').MACH3                          02520027
&S1      SETA      &S1+X'40'                                            02540027
.MACH3   AIF       ('&ALIGN' EQ 'C').MACH6                              02560027
         AIF       ('&ALIGN' NE 'H').MACH4                              02580027
&S1      SETA      &S1+X'10'                                            02600027
         AGO       .MACH6                                               02620027
.MACH4   AIF       ('&ALIGN' NE 'F').MACH5                              02640027
&S1      SETA      &S1+X'20'                                            02660027
         AGO       .MACH6                                               02680027
.MACH5   AIF       ('&ALIGN' NE 'D').MNOTE7                             02700027
&S1      SETA      &S1+X'30'                                            02720027
.MACH6   ANOP                                                           02740027
         AIF       ('&CLASS' EQ '').MNOTE8                              02760027
&W1      SETA      &CLASS                                               02780027
         AIF       (&W1 GT 3 OR &W1 LT 0).MNOTE8                        02800027
&S1      SETA      &S1+&W1*4                                            02820027
         AIF       ('&LIT23' EQ 'NO').MACH7                             02840027
&S1      SETA      &S1+X'02'                                            02860027
.MACH7   AIF       ('&LIT1' EQ 'NO').SET1                               02880027
&S1      SETA      &S1+X'01'                                            02900027
.*                 SET TXASC BYTE                                       02920027
.SET1    ANOP                                                           02940027
&GTXASC(&TDIM) SETA &S1                                                 02960027
&GDIM    SETA      &GDIM+1                                              02980027
         MEXIT                                                          03000027
.NOTGEND MNOTE     *,'OPCD  --  OPCODE NOT GENERATED IS= &IS'           03020027
         AGO       .MEND                                                03040027
.*                 THESE ARE THE ERRORS                                 03060027
.MNOTE1  MNOTE     4,'OPCD -- MISSING LABEL'                            03080027
         AGO       .MENDERR                                             03100027
.MNOTE2  MNOTE     4,'OPCD -- INVALID TYPE=&TYPE'                       03120027
         AGO       .MENDERR                                             03140027
.MNOTE3  MNOTE     4,'OPCD -- INVALID NAME=&NAME'                       03160027
         AGO       .MENDERR                                             03180027
.MNOTE4  MNOTE     4,'OPCD -- INVALID OPND=&OPND'                       03200027
         AGO       .MENDERR                                             03220027
.MNOTE5  MNOTE     4,'OPCD -- MISSING &&OP KEYWORD'                     03240027
         AGO       .MENDERR                                             03260027
.MNOTE6  MNOTE     4,'OPCD -- MISSING &&MASK FOR EXTEN'                 03280027
         AGO       .MENDERR                                             03300027
.MNOTE7  MNOTE     4,'OPCD -- INVALID ALIGN=&ALIGN'                     03320027
         AGO       .MENDERR                                             03340027
.MNOTE8  MNOTE     4,'OPCD -- INVALID CLASS=&CLASS'                     03360027
         AGO       .MENDERR                                             03380027
.MNOTE9  MNOTE     4,'OPCD  --  ILLGEN=&ILLGEN INVALID WITH TYPE=&TYPE' 03400027
         AGO       .MENDERR                                             03420027
.MENDERR ANOP                                                           03440027
&ERROR   SETA      &ERROR+1                                             03460027
.MEND    MEND                                                           03480027
./ ADD NAME=OPCDGEN
         MACRO                                                          03520027
         OPCDGEN                                                        03540027
         GBLA      &GLENLAB(400),&GTXTO(400),&GTXASC(400)               03560027
         GBLA      &GDIM,&ERROR,&FIRST,&VALUE                           03580027
         GBLC      &GLABEL(400),&GOP(400),&CHAR                         03600027
         LCLA      &I,&J,&W,&OPLX,&OPPT(400)                            03620027
         LCLC      &C(8),&WC                                            03640027
         ACTR      100000                                               03660027
         AIF       (&FIRST EQ 0 OR &GDIM LE 1 OR &ERROR GT 0).MNOTE1    03680027
.LOOP0   ANOP                                                           03700027
&I       SETA      &I+1                                                 03720027
&OPPT(&I) SETA     &I                                                   03740027
         AIF       (&I LT &GDIM).LOOP0                                  03760027
&I       SETA      0                                                    03780027
.*                 BUBBLE SORT OPCODES BY NAME AND LENGTH               03800027
.LOOP1   ANOP                                                           03820027
&I       SETA      &I+1                                                 03840027
&J       SETA      &I                                                   03860027
.LOOP2   ANOP                                                           03880027
&J       SETA      &J+1                                                 03900027
         AIF       ('&GLABEL(&I)' LT '&GLABEL(&J)').LOOP3               03920027
&W       SETA      &OPPT(&I)                                            03940027
&WC      SETC      '&GLABEL(&I)'                                        03960027
&OPPT(&I) SETA     &OPPT(&J)                                            03980027
&GLABEL(&I) SETC   '&GLABEL(&J)'                                        04000027
&OPPT(&J) SETA     &W                                                   04020027
&GLABEL(&J) SETC   '&WC'                                                04040027
.LOOP3   AIF       (&J LT &GDIM).LOOP2                                  04060027
         AIF       (&I LT (&GDIM-1)).LOOP1                              04080027
.*                 OUTPUT RESULTS                                       04100027
&I       SETA      0                                                    04120027
.OUTLP1  ANOP                                                           04140027
&I       SETA      &I+1                                                 04160027
&J       SETA      &OPPT(&I)                                            04180027
&W       SETA      0                                                    04200027
         AIF       (&GLENLAB(&J) EQ &OPLX).OUTLP2                       04220027
&OPLX    SETA      &GLENLAB(&J)                                         04240027
OPL&OPLX EQU       *                                                    04260027
.*                 TRANSLATE OPCODE TO INTERNAL CODE                    04280027
.OUTLP2  ANOP                                                           04300027
&W       SETA      &W+1                                                 04320027
&CHAR    SETC      '&GLABEL(&I)'(&W,1)                                  04340027
&C(&W)   SETC      'I&CHAR,'                                            04360027
         AIF       (&W LT &OPLX).OUTLP2                                 04380027
&WC      SETC      '##'.'&GLABEL(&I)'                                   04400027
&WC      DC        AL1(&C(1)&C(2)&C(3)&C(4)&C(5)&C(6)&C(7)&C(8)&GTXTO(&*04420027
               J)),X'&GOP(&J)',AL1(&GTXASC(&J))                         04440027
         AIF       (&I LT &GDIM).OUTLP1                                 04460027
OPFN     EQU       *                                                    04480027
         AGO       .MEND                                                04500027
.MNOTE1  MNOTE     8,'OPCDGEN -- OPCD MACROS FAILED'                    04520027
         MNOTE     8,'OPCDGEN -- YOU LOSE ...... AGAIN'                 04540027
.MEND    MEND                                                           04560027
./ ENDUP                                                                00340027
/*                                                                      00340027
//                                                                      00360027
