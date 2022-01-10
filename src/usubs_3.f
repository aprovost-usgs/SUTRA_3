C     SUBROUTINE        B  C  T  I  M  E           SUTRA VERSION 3.0     BCTIME.........100
C                                                                        BCTIME.........200
C *** PURPOSE :                                                          BCTIME.........300
C ***  USER-PROGRAMMED SUBROUTINE WHICH ALLOWS THE USER TO SPECIFY:      BCTIME.........400
C ***   (1) TIME-DEPENDENT SPECIFIED PRESSURES AND TIME-DEPENDENT        BCTIME.........500
C ***       CONCENTRATIONS OR TEMPERATURES OF INFLOWS AT THESE POINTS    BCTIME.........600
C ***   (2) TIME-DEPENDENT SPECIFIED CONCENTRATIONS OR TEMPERATURES      BCTIME.........700
C ***   (3) TIME-DEPENDENT FLUID SOURCES AND CONCENTRATIONS              BCTIME.........800
C ***       OR TEMPERATURES OF INFLOWS AT THESE POINTS                   BCTIME.........900
C ***   (4) TIME-DEPENDENT ENERGY OR SOLUTE MASS SOURCES                 BCTIME........1000
C                                                                        BCTIME........1100
      SUBROUTINE BCTIME(IPBC,PBC,IUBC,UBC,QIN,UIN,QUIN,IQSOP,IQSOU,      BCTIME........1200
     1   IPBG,PBG1,QPBG1,PBG2,QPBG2,CPQL1,CPQL2,UPBGI,UPBGO,CUPBGO,      BCTIME........1300
     2   IUBG,UBG1,QUBG1,UBG2,QUBG2,IPBCT,IUBCT,IQSOPT,IQSOUT,           BCTIME........1400
     3   IPBGT,IUBGT,X,Y,Z,IBCPBC,IBCUBC,IBCSOP,IBCSOU,IBCPBG,IBCUBG,    BCTIME........1500
     4   IBCSF,IBCSS,IBCSP,IBCSU,IBCSPG,IBCSUG)                          BCTIME........1600
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                BCTIME........1700
      LOGICAL OPND                                                       BCTIME........1800
      CHARACTER*1 CPQL1(NPBG),CPQL2(NPBG)                                BCTIME........1900
      CHARACTER*3 CUPBGO(NPBG)                                           BCTIME........2000
      DIMENSION IPBC(NBCN),PBC(NBCN),IUBC(NBCN),UBC(NBCN),               BCTIME........2100
     1   QIN(NN),UIN(NN),QUIN(NN),IQSOP(NSOP),IQSOU(NSOU),               BCTIME........2200
     2   X(NN),Y(NN),Z(NN)                                               BCTIME........2300
      DIMENSION PBG1(NPBG),QPBG1(NPBG),PBG2(NPBG),QPBG2(NPBG),           BCTIME........2400
     1   UPBGI(NPBG),UPBGO(NPBG),UBG1(NUBG),QUBG1(NUBG),                 BCTIME........2500
     2   UBG2(NUBG),QUBG2(NUBG)                                          BCTIME........2600
      DIMENSION IPBG(NPBG),IUBG(NUBG)                                    BCTIME........2700
      INTEGER(1) IBCPBC(NBCN),IBCUBC(NBCN),IBCSOP(NSOP),IBCSOU(NSOU),    BCTIME........2800
     1   IBCPBG(NPBG),IBCUBG(NUBG)                                       BCTIME........2900
      INTEGER IBCSP(NBCN),IBCSU(NBCN),IBCSF(NSOP),IBCSS(NSOU),           BCTIME........3000
     1   IBCSPG(NPBG),IBCSUG(NUBG)                                       BCTIME........3100
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              BCTIME........3200
     1   NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                                  BCTIME........3300
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8,K9,                 BCTIME........3400
     1   K10,K11,K12,K13,K14,K15,K16,K17,K18,K19,K20,K21,K22,K23         BCTIME........3500
      COMMON /GRAVEC/ GRAVX,GRAVY,GRAVZ                                  BCTIME........3600
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       BCTIME........3700
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITBCS,ITRST,ITMAX,TSTART      BCTIME........3800
C                                                                        BCTIME........3900
C.....DEFINITION OF REQUIRED VARIABLES                                   BCTIME........4000
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  BCTIME........4100
C     NN = EXACT NUMBER OF NODES IN MESH                                 BCTIME........4200
C     NPBC = EXACT NUMBER OF SPECIFIED PRESSURE NODES                    BCTIME........4300
C     NUBC = EXACT NUMBER OF SPECIFIED CONCENTRATION                     BCTIME........4400
C            OR TEMPERATURE NODES                                        BCTIME........4500
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  BCTIME........4600
C     IT = NUMBER OF CURRENT TIME STEP                                   BCTIME........4700
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  BCTIME........4800
C     TSEC = TIME AT END OF CURRENT TIME STEP IN SECONDS                 BCTIME........4900
C     TMIN = TIME AT END OF CURRENT TIME STEP IN MINUTES                 BCTIME........5000
C     THOUR = TIME AT END OF CURRENT TIME STEP IN HOURS                  BCTIME........5100
C     TDAY = TIME AT END OF CURRENT TIME STEP IN DAYS                    BCTIME........5200
C     TWEEK = TIME AT END OF CURRENT TIME STEP IN WEEKS                  BCTIME........5300
C     TMONTH = TIME AT END OF CURRENT TIME STEP IN MONTHS                BCTIME........5400
C     TYEAR = TIME AT END OF CURRENT TIME STEP IN YEARS                  BCTIME........5500
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  BCTIME........5600
C     PBC(IP) = SPECIFIED PRESSURE VALUE AT IP(TH) SPECIFIED             BCTIME........5700
C               PRESSURE NODE                                            BCTIME........5800
C     UBC(IP) = SPECIFIED CONCENTRATION OR TEMPERATURE VALUE OF ANY      BCTIME........5900
C               INFLOW OCCURRING AT IP(TH) SPECIFIED PRESSURE NODE       BCTIME........6000
C     IPBC(IP) = ACTUAL NODE NUMBER OF IP(TH) SPECIFIED PRESSURE NODE    BCTIME........6100
C                {WHEN NODE NUMBER I=IPBC(IP) IS NEGATIVE (I<0),         BCTIME........6200
C                VALUES MUST BE SPECIFIED FOR PBC AND UBC.}              BCTIME........6300
C     IBCPBC(IP) = INDICATOR OF WHERE THIS PRESSURE SPECIFICATION        BCTIME........6400
C                  WAS MADE. MUST BE SET TO -1 TO INDICATE THAT THIS     BCTIME........6500
C                  SPECIFICATION WAS MADE IN SUBROUTINE BCTIME.          BCTIME........6600
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  BCTIME........6700
C     UBC(IUP) = SPECIFIED CONCENTRATION OR TEMPERATURE VALUE AT         BCTIME........6800
C                IU(TH) SPECIFIED CONCENTRATION OR TEMPERATURE NODE      BCTIME........6900
C                (WHERE IUP=IU+NPBC)                                     BCTIME........7000
C     IUBC(IUP) = ACTUAL NODE NUMBER OF IU(TH) SPECIFIED CONCENTRATION   BCTIME........7100
C                 OR TEMPERATURE NODE (WHERE IUP=IU+NPBC)                BCTIME........7200
C                 {WHEN NODE NUMBER I=IUBC(IU) IS NEGATIVE (I<0),        BCTIME........7300
C                 A VALUE MUST BE SPECIFIED FOR UBC.}                    BCTIME........7400
C     IBCUBC(IUP) = INDICATOR OF WHERE THIS CONCENTRATION OR TEMPERATURE BCTIME........7500
C                  SPECIFICATION WAS MADE. MUST BE SET TO -1 TO INDICATE BCTIME........7600
C                  THAT THIS SPECIFICATION WAS MADE IN SUBROUTINE        BCTIME........7700
C                  BCTIME.                                               BCTIME........7800
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  BCTIME........7900
C     IQSOP(IQP) = NODE NUMBER OF IQP(TH) FLUID SOURCE NODE.             BCTIME........8000
C                  {WHEN NODE NUMBER I=IQSOP(IQP) IS NEGATIVE (I<0),     BCTIME........8100
C                  VALUES MUST BE SPECIFIED FOR QIN AND UIN.}            BCTIME........8200
C     QIN(-I) = SPECIFIED FLUID SOURCE VALUE AT NODE (-I)                BCTIME........8300
C     UIN(-I) = SPECIFIED CONCENTRATION OR TEMPERATURE VALUE OF ANY      BCTIME........8400
C               INFLOW OCCURRING AT FLUID SOURCE NODE (-I)               BCTIME........8500
C     IBCSOP(IQP) = INDICATOR OF WHERE THIS FLUID SOURCE SPECIFICATION   BCTIME........8600
C                   WAS MADE. MUST BE SET TO -1 TO INDICATE THAT THIS    BCTIME........8700
C                   SPECIFICATION WAS MADE IN SUBROUTINE BCTIME.         BCTIME........8800
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  BCTIME........8900
C     IQSOU(IQU) = NODE NUMBER OF IQU(TH) ENERGY OR                      BCTIME........9000
C                  SOLUTE MASS SOURCE NODE                               BCTIME........9100
C                  {WHEN NODE NUMBER I=IQSOU(IQU) IS NEGATIVE (I<0),     BCTIME........9200
C                  A VALUE MUST BE SPECIFIED FOR QUIN.}                  BCTIME........9300
C     QUIN(-I) = SPECIFIED ENERGY OR SOLUTE MASS SOURCE VALUE            BCTIME........9400
C                AT NODE (-I)                                            BCTIME........9500
C     IBCSOU(IQU) = INDICATOR OF WHERE THIS ENERGY OR SOLUTE MASS        BCTIME........9600
C                   SOURCE SPECIFICATION WAS MADE. MUST BE SET TO -1     BCTIME........9700
C                   TO INDICATE THAT THIS SPECIFICATION WAS MADE IN      BCTIME........9800
C                   SUBROUTINE BCTIME.                                   BCTIME........9900
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  BCTIME.......10000
C                                                                        BCTIME.......10100
C.....ADDITIONAL USEFUL VARIABLES                                        BCTIME.......10200
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  BCTIME.......10300
C     "FUNITS" ARE UNIT NUMBERS FOR INPUT AND OUTPUT FILES               BCTIME.......10400
C         AS ASSIGNED IN THE INPUT FILE "SUTRA.FIL"                      BCTIME.......10500
C                                                                        BCTIME.......10600
C     X(I), Y(I), AND Z(I) ARE THE X-, Y-, AND Z-COORDINATES OF NODE I   BCTIME.......10700
C     (FOR 2-D PROBLEMS, Z(I) IS THE MESH THICKNESS AT NODE I)           BCTIME.......10800
C                                                                        BCTIME.......10900
C     GRAVX, GRAVY AND GRAVZ ARE THE X-, Y-, AND Z-COMPONENTS OF THE     BCTIME.......11000
C     GRAVITY VECTOR (FOR 2-D PROBLEMS, GRAVZ = 0)                       BCTIME.......11100
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  BCTIME.......11200
C                                                                        BCTIME.......11300
C                                                                        BCTIME.......11400
C.....NSOPI IS ACTUAL NUMBER OF FLUID SOURCE NODES                       BCTIME.......11500
      NSOPI=NSOP-1                                                       BCTIME.......11600
C.....NSOUI IS ACTUAL NUMBER OF ENERGY OR SOLUTE MASS SOURCE NODES       BCTIME.......11700
      NSOUI=NSOU-1                                                       BCTIME.......11800
C                                                                        BCTIME.......11900
C                                                                        BCTIME.......12000
C                                                                        BCTIME.......12100
C                                                                        BCTIME.......12200
C                                                                        BCTIME.......12300
C                                                                        BCTIME.......12400
      IF(IPBCT) 50,240,240                                               BCTIME.......12500
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......12600
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......12700
C.....SECTION (1):  SET TIME-DEPENDENT SPECIFIED PRESSURES OR            BCTIME.......12800
C     CONCENTRATIONS (TEMPERATURES) OF INFLOWS AT SPECIFIED              BCTIME.......12900
C     PRESSURE NODES                                                     BCTIME.......13000
C                                                                        BCTIME.......13100
   50 CONTINUE                                                           BCTIME.......13200
      DO 200 IP=1,NPBC                                                   BCTIME.......13300
      I=IPBC(IP)                                                         BCTIME.......13400
      IF(I) 100,200,200                                                  BCTIME.......13500
  100 CONTINUE                                                           BCTIME.......13600
C     NOTE: A FLOW AND TRANSPORT SOLUTION MUST OCCUR FOR ANY             BCTIME.......13700
C           TIME STEP IN WHICH PBC( ) CHANGES.                           BCTIME.......13800
C     PBC(IP) =  ((          ))                                          BCTIME.......13900
C     UBC(IP) =  ((          ))                                          BCTIME.......14000
C.....IBCPBC(IP) MUST BE SET TO -1 TO INDICATE THAT PBC(IP)              BCTIME.......14100
C        AND/OR UBC(IP) HAVE BEEN SET BY SUBROUTINE BCTIME.              BCTIME.......14200
      IBCPBC(IP) = -1                                                    BCTIME.......14300
      IBCSP(IP) = 0                                                      BCTIME.......14400
  200 CONTINUE                                                           BCTIME.......14500
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......14600
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......14700
C                                                                        BCTIME.......14800
C                                                                        BCTIME.......14900
C                                                                        BCTIME.......15000
C                                                                        BCTIME.......15100
C                                                                        BCTIME.......15200
C                                                                        BCTIME.......15300
  240 IF(IUBCT) 250,440,440                                              BCTIME.......15400
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......15500
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......15600
C.....SECTION (2):  SET TIME-DEPENDENT SPECIFIED                         BCTIME.......15700
C     CONCENTRATIONS (TEMPERATURES)                                      BCTIME.......15800
C                                                                        BCTIME.......15900
  250 CONTINUE                                                           BCTIME.......16000
      DO 400 IU=1,NUBC                                                   BCTIME.......16100
      IUP=IU+NPBC                                                        BCTIME.......16200
      I=IUBC(IUP)                                                        BCTIME.......16300
      IF(I) 300,400,400                                                  BCTIME.......16400
  300 CONTINUE                                                           BCTIME.......16500
C     NOTE: A TRANSPORT SOLUTION MUST OCCUR FOR ANY TIME STEP IN WHICH   BCTIME.......16600
C           UBC( ) CHANGES.  IN ADDITION, IF FLUID PROPERTIES ARE        BCTIME.......16700
C           SENSITIVE TO 'U', THEN A FLOW SOLUTION MUST OCCUR AS WELL.   BCTIME.......16800
C     UBC(IUP) =   ((          ))                                        BCTIME.......16900
C.....IBCUBC(IUP) MUST BE SET TO -1 TO INDICATE THAT UBC(IUP)            BCTIME.......17000
C        HAS BEEN SET BY SUBROUTINE BCTIME.                              BCTIME.......17100
      IBCUBC(IUP) = -1                                                   BCTIME.......17200
      IBCSU(IUP) = 0                                                     BCTIME.......17300
  400 CONTINUE                                                           BCTIME.......17400
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......17500
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......17600
C                                                                        BCTIME.......17700
C                                                                        BCTIME.......17800
C                                                                        BCTIME.......17900
C                                                                        BCTIME.......18000
C                                                                        BCTIME.......18100
C                                                                        BCTIME.......18200
  440 IF(IQSOPT) 450,640,640                                             BCTIME.......18300
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......18400
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......18500
C.....SECTION (3):  SET TIME-DEPENDENT FLUID SOURCES/SINKS,              BCTIME.......18600
C      OR CONCENTRATIONS (TEMPERATURES) OF SOURCE FLUID                  BCTIME.......18700
C                                                                        BCTIME.......18800
  450 CONTINUE                                                           BCTIME.......18900
      DO 600 IQP=1,NSOPI                                                 BCTIME.......19000
      I=IQSOP(IQP)                                                       BCTIME.......19100
      IF(I) 500,600,600                                                  BCTIME.......19200
  500 CONTINUE                                                           BCTIME.......19300
C     NOTE: A FLOW AND TRANSPORT SOLUTION MUST OCCUR FOR ANY             BCTIME.......19400
C           TIME STEP IN WHICH QIN( ) CHANGES.                           BCTIME.......19500
C     QIN(-I) =   ((           ))                                        BCTIME.......19600
C     NOTE: A TRANSPORT SOLUTION MUST OCCUR FOR ANY                      BCTIME.......19700
C           TIME STEP IN WHICH UIN( ) CHANGES.                           BCTIME.......19800
C     UIN(-I) =   ((           ))                                        BCTIME.......19900
C.....IBCSOP(IQP) MUST BE SET TO -1 TO INDICATE THAT QIN(-I)             BCTIME.......20000
C        AND/OR UIN(-I) HAVE BEEN SET BY SUBROUTINE BCTIME.              BCTIME.......20100
      IBCSOP(IQP) = -1                                                   BCTIME.......20200
      IBCSF(IQP) = 0                                                     BCTIME.......20300
  600 CONTINUE                                                           BCTIME.......20400
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......20500
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......20600
C                                                                        BCTIME.......20700
C                                                                        BCTIME.......20800
C                                                                        BCTIME.......20900
C                                                                        BCTIME.......21000
C                                                                        BCTIME.......21100
C                                                                        BCTIME.......21200
  640 IF(IQSOUT) 650,840,840                                             BCTIME.......21300
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......21400
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......21500
C.....SECTION (4):  SET TIME-DEPENDENT SOURCES/SINKS                     BCTIME.......21600
C     OF SOLUTE MASS OR ENERGY                                           BCTIME.......21700
C                                                                        BCTIME.......21800
  650 CONTINUE                                                           BCTIME.......21900
      DO 800 IQU=1,NSOUI                                                 BCTIME.......22000
      I=IQSOU(IQU)                                                       BCTIME.......22100
      IF(I) 700,800,800                                                  BCTIME.......22200
  700 CONTINUE                                                           BCTIME.......22300
C     NOTE: A TRANSPORT SOLUTION MUST OCCUR FOR ANY                      BCTIME.......22400
C           TIME STEP IN WHICH QUIN( ) CHANGES.                          BCTIME.......22500
C     QUIN(-I) =   ((           ))                                       BCTIME.......22600
C.....IBCSOU(IQU) MUST BE SET TO -1 TO INDICATE THAT QUIN(-I)            BCTIME.......22700
C        HAS BEEN SET BY SUBROUTINE BCTIME.                              BCTIME.......22800
      IBCSOU(IQU) = -1                                                   BCTIME.......22900
      IBCSS(IQU) = 0                                                     BCTIME.......23000
  800 CONTINUE                                                           BCTIME.......23100
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......23200
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......23300
C                                                                        BCTIME.......23400
C                                                                        BCTIME.......23500
C                                                                        BCTIME.......23600
C                                                                        BCTIME.......23700
C                                                                        BCTIME.......23800
C                                                                        BCTIME.......23900
  840 IF(IPBGT) 850,1040,1040                                            BCTIME.......24000
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......24100
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......24200
C.....SECTION (5):  SET TIME-DEPENDENT GENERALIZED-FLOW CONDITIONS       BCTIME.......24300
C                                                                        BCTIME.......24400
  850 CONTINUE                                                           BCTIME.......24500
      DO 1000 IPG=1,NPBG                                                 BCTIME.......24600
      I=IPBG(IPG)                                                        BCTIME.......24700
      IF(I) 900,1000,1000                                                BCTIME.......24800
  900 CONTINUE                                                           BCTIME.......24900
C     NOTE: A TRANSPORT SOLUTION MUST OCCUR FOR ANY                      BCTIME.......25000
C           TIME STEP IN WHICH CHANGES IN GENERALIZED-FLOW               BCTIME.......25100
C           SPECIFICATIONS CHANGE THE RESULTANT FLOW OF FLUID            BCTIME.......25200
C           IN OR OUT OF THE MODEL.                                      BCTIME.......25300
C     PBG1(-I) =    ((           ))                                      BCTIME.......25400
C     QPBG1(-I) =   ((           ))                                      BCTIME.......25500
C     PBG2(-I) =    ((           ))                                      BCTIME.......25600
C     QPBG2(-I) =   ((           ))                                      BCTIME.......25700
C     CPQL1(-I) =   ((           ))                                      BCTIME.......25800
C     CPQL2(-I) =   ((           ))                                      BCTIME.......25900
C     UPBGI(-I) =   ((           ))                                      BCTIME.......26000
C     CUPBGO(-I) =  ((           ))                                      BCTIME.......26100
C     UPBGO(-I) =   ((           ))                                      BCTIME.......26200
C.....IBCPBG(IPG) MUST BE SET TO -1 TO INDICATE THAT THE GENERALIZED-    BCTIME.......26300
C        FLOW BOUNDARY CONDITION HAS BEEN SET BY SUBROUTINE BCTIME.      BCTIME.......26400
      IBCPBG(IPG) = -1                                                   BCTIME.......26500
      IBCSPG(IPG) = 0                                                    BCTIME.......26600
 1000 CONTINUE                                                           BCTIME.......26700
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......26800
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......26900
C                                                                        BCTIME.......27000
C                                                                        BCTIME.......27100
C                                                                        BCTIME.......27200
C                                                                        BCTIME.......27300
C                                                                        BCTIME.......27400
C                                                                        BCTIME.......27500
 1040 IF(IUBGT) 1050,1240,1240                                           BCTIME.......27600
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......27700
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......27800
C.....SECTION (6):  SET TIME-DEPENDENT GENERALIZED-TRANSPORT CONDITIONS  BCTIME.......27900
C                                                                        BCTIME.......28000
 1050 CONTINUE                                                           BCTIME.......28100
      DO 1200 IUG=1,NUBG                                                 BCTIME.......28200
      I=IUBG(IUG)                                                        BCTIME.......28300
      IF(I) 1100,1200,1200                                               BCTIME.......28400
 1100 CONTINUE                                                           BCTIME.......28500
C     NOTE: A TRANSPORT SOLUTION MUST OCCUR FOR ANY                      BCTIME.......28600
C           TIME STEP IN WHICH CHANGES IN GENERALIZED-TRANSPORT          BCTIME.......28700
C           SPECIFICATIONS CHANGE THE RESULTANT FLOW OF SOLUTE OR        BCTIME.......28800
C           ENERGY IN OR OUT OF THE MODEL.                               BCTIME.......28900
C     UBG1(-I) =    ((           ))                                      BCTIME.......29000
C     QUBG1(-I) =   ((           ))                                      BCTIME.......29100
C     UBG2(-I) =    ((           ))                                      BCTIME.......29200
C     QUBG2(-I) =   ((           ))                                      BCTIME.......29300
C.....IBCUBG(IUG) MUST BE SET TO -1 TO INDICATE THAT THE GENERALIZED-    BCTIME.......29400
C        FLOW BOUNDARY CONDITION HAS BEEN SET BY SUBROUTINE BCTIME.      BCTIME.......29500
      IBCUBG(IUG) = -1                                                   BCTIME.......29600
      IBCSUG(IUG) = 0                                                    BCTIME.......29700
 1200 CONTINUE                                                           BCTIME.......29800
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......29900
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......30000
C                                                                        BCTIME.......30100
C                                                                        BCTIME.......30200
C                                                                        BCTIME.......30300
C                                                                        BCTIME.......30400
C                                                                        BCTIME.......30500
C                                                                        BCTIME.......30600
 1240 CONTINUE                                                           BCTIME.......30700
C                                                                        BCTIME.......30800
      RETURN                                                             BCTIME.......30900
      END                                                                BCTIME.......31000
C                                                                        BCTIME.......31100
C     SUBROUTINE        U  N  S  A  T              SUTRA VERSION 3.0     UNSAT..........100
C                                                                        UNSAT..........200
C *** PURPOSE :                                                          UNSAT..........300
C ***  USER-PROGRAMMED SUBROUTINE GIVING:                                UNSAT..........400
C ***  (1)  SATURATION AS A FUNCTION OF PRESSURE ( SW(PRES) )            UNSAT..........500
C ***  (2)  DERIVATIVE OF SATURATION WITH RESPECT TO PRESSURE            UNSAT..........600
C ***       AS A FUNCTION OF EITHER PRESSURE OR SATURATION               UNSAT..........700
C ***       ( DSWDP(PRES), OR DSWDP(SW) )                                UNSAT..........800
C ***                                                                    UNSAT..........900
C ***  CODE BETWEEN DASHED LINES MUST BE REPLACED TO GIVE THE            UNSAT.........1000
C ***  PARTICULAR UNSATURATED RELATIONSHIPS DESIRED.                     UNSAT.........1100
C ***                                                                    UNSAT.........1200
C ***  DIFFERENT FUNCTIONS MAY BE GIVEN FOR EACH REGION OF THE MESH.     UNSAT.........1300
C ***  REGIONS ARE SPECIFIED BY BOTH NODE NUMBER AND ELEMENT NUMBER      UNSAT.........1400
C ***  IN INPUT DATA FILE FOR UNIT K1 (INP).                             UNSAT.........1500
C                                                                        UNSAT.........1600
      SUBROUTINE UNSAT(SW,DSWDP,PRES,KREG)                               UNSAT.........1700
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                UNSAT.........1800
      DIMENSION KTYPE(2)                                                 UNSAT.........1900
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  UNSAT.........2000
     1   NPCYC,NUCYC,NPRINT,NBCFPR,NBCSPR,NBCPPR,NBCUPR,NLAKPR,IREAD,    UNSAT.........2100
     1   NBGPPR,NBGUPR,ISTORE,NOUMAT,IUNSAT,IALSAT,KTYPE                 UNSAT.........2200
C                                                                        UNSAT.........2300
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- UNSAT.........2400
C     E X A M P L E   C O D I N G   FOR                                  UNSAT.........2500
C     MESH WITH TWO REGIONS OF UNSATURATED PROPERTIES USING              UNSAT.........2600
C     THREE PARAMETER-UNSATURATED FLOW RELATIONSHIPS OF                  UNSAT.........2700
C     VAN GENUCHTEN(1980)                                                UNSAT.........2800
C        RESIDUAL SATURATION, SWRES, GIVEN IN UNITS {L**0}               UNSAT.........2900
C        PARAMETER, AA, GIVEN IN INVERSE PRESSURE UNITS {m*(s**2)/kg}    UNSAT.........3000
C        PARAMETER, VN, GIVEN IN UNITS {L**0}                            UNSAT.........3100
C                                                                        UNSAT.........3200
      REAL SWRES,AA,VN,SWRM1,AAPVN,VNF,AAPVNN,DNUM,DNOM                  UNSAT.........3300
      REAL SWRES1,SWRES2,AA1,AA2,VN1,VN2                                 UNSAT.........3400
C                                                                        UNSAT.........3500
C     DATA FOR REGION 1:                                                 UNSAT.........3600
      DATA   SWRES1/0.30E0/,   AA1/5.0E-5/,   VN1/2.0E0/                 UNSAT.........3700
      SAVE SWRES1, AA1, VN1                                              UNSAT.........3800
C     DATA FOR REGION 2:                                                 UNSAT.........3900
      DATA   SWRES2/0.30E0/,   AA2/5.0E-5/,   VN2/2.0E0/                 UNSAT.........4000
      SAVE SWRES2, AA2, VN2                                              UNSAT.........4100
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- UNSAT.........4200
C                                                                        UNSAT.........4300
C *** BECAUSE THIS ROUTINE IS CALLED OFTEN FOR UNSATURATED FLOW RUNS,    UNSAT.........4400
C *** EXECUTION TIME MAY BE SAVED BY CAREFUL CODING OF DESIRED           UNSAT.........4500
C *** RELATIONSHIPS USING ONLY INTEGER AND SINGLE PRECISION VARIABLES!   UNSAT.........4600
C *** RESULTS OF THE CALCULATIONS MUST THEN BE PLACED INTO DOUBLE        UNSAT.........4700
C *** PRECISION VARIABLES SW AND DSWDP BEFORE LEAVING THIS SUBROUTINE.   UNSAT.........4800
C                                                                        UNSAT.........4900
C                                                                        UNSAT.........5000
C*********************************************************************** UNSAT.........5100
C*********************************************************************** UNSAT.........5200
C                                                                        UNSAT.........5300
C     SET PARAMETERS FOR CURRENT REGION, KREG                            UNSAT.........5400
      GOTO(10,20),KREG                                                   UNSAT.........5500
   10 SWRES=SWRES1                                                       UNSAT.........5600
      AA=AA1                                                             UNSAT.........5700
      VN=VN1                                                             UNSAT.........5800
      GOTO 100                                                           UNSAT.........5900
   20 SWRES=SWRES2                                                       UNSAT.........6000
      AA=AA2                                                             UNSAT.........6100
      VN=VN2                                                             UNSAT.........6200
  100 CONTINUE                                                           UNSAT.........6300
C                                                                        UNSAT.........6400
C                                                                        UNSAT.........6500
C*********************************************************************** UNSAT.........6600
C*********************************************************************** UNSAT.........6700
C.....SECTION (1):                                                       UNSAT.........6800
C     SW VS. PRES   (VALUE CALCULATED ON EACH CALL TO UNSAT)             UNSAT.........6900
C     CODING MUST GIVE A VALUE TO SATURATION, SW.                        UNSAT.........7000
C                                                                        UNSAT.........7100
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  UNSAT.........7200
C     THREE PARAMETER MODEL OF VAN GENUCHTEN(1980)                       UNSAT.........7300
      SWRM1=1.E0-SWRES                                                   UNSAT.........7400
      AAPVN=1.E0+(AA*(-PRES))**VN                                        UNSAT.........7500
      VNF=(VN-1.E0)/VN                                                   UNSAT.........7600
      AAPVNN=AAPVN**VNF                                                  UNSAT.........7700
      S W   =   DBLE (SWRES+SWRM1/AAPVNN)                                UNSAT.........7800
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  UNSAT.........7900
C*********************************************************************** UNSAT.........8000
C*********************************************************************** UNSAT.........8100
C                                                                        UNSAT.........8200
C                                                                        UNSAT.........8300
C                                                                        UNSAT.........8400
C                                                                        UNSAT.........8500
C                                                                        UNSAT.........8600
C                                                                        UNSAT.........8700
      IF(IUNSAT-2) 600,1800,1800                                         UNSAT.........8800
C*********************************************************************** UNSAT.........8900
C*********************************************************************** UNSAT.........9000
C.....SECTION (2):                                                       UNSAT.........9100
C     DSWDP VS. PRES, OR DSWDP VS. SW   (CALCULATED ONLY WHEN IUNSAT=1)  UNSAT.........9200
C     CODING MUST GIVE A VALUE TO DERIVATIVE OF SATURATION WITH          UNSAT.........9300
C     RESPECT TO PRESSURE, DSWDP.                                        UNSAT.........9400
C                                                                        UNSAT.........9500
  600 CONTINUE                                                           UNSAT.........9600
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  UNSAT.........9700
      DNUM=AA*(VN-1.E0)*SWRM1*(AA*(-PRES))**(VN-1.E0)                    UNSAT.........9800
      DNOM=AAPVN*AAPVNN                                                  UNSAT.........9900
      D S W D P   =   DBLE (DNUM/DNOM)                                   UNSAT........10000
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  UNSAT........10100
C*********************************************************************** UNSAT........10200
C*********************************************************************** UNSAT........10300
C                                                                        UNSAT........10400
C                                                                        UNSAT........10500
C                                                                        UNSAT........10600
C                                                                        UNSAT........10700
C                                                                        UNSAT........10800
C                                                                        UNSAT........10900
 1800 RETURN                                                             UNSAT........11000
C                                                                        UNSAT........11100
      END                                                                UNSAT........11200
C                                                                        UNSAT........11300
C     SUBROUTINE        R  K  S  A  T              SUTRA VERSION 3.0     RKSAT..........100
C                                                                        RKSAT..........200
C *** PURPOSE :                                                          RKSAT..........300
C ***  USER-PROGRAMMED SUBROUTINE GIVING RELATIVE PERMEABILITY AS A      RKSAT..........400
C ***  FUNCTION OF WATER SATURATION ( RELK(SW) ).                        RKSAT..........500
C ***                                                                    RKSAT..........600
C ***  CODE BETWEEN DASHED LINES MUST BE REPLACED TO GIVE THE            RKSAT..........700
C ***  PARTICULAR UNSATURATED RELATIONSHIPS DESIRED.                     RKSAT..........800
C ***                                                                    RKSAT..........900
C ***  DIFFERENT FUNCTIONS MAY BE GIVEN FOR EACH REGION OF THE MESH.     RKSAT.........1000
C ***  REGIONS ARE SPECIFIED BY BOTH NODE NUMBER AND ELEMENT NUMBER      RKSAT.........1100
C ***  IN INPUT DATA FILE FOR UNIT K1 (INP).                             RKSAT.........1200
C                                                                        RKSAT.........1300
      SUBROUTINE RKSAT(RELK,SW,PRES,KREG)                                RKSAT.........1400
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                RKSAT.........1500
      DIMENSION KTYPE(2)                                                 RKSAT.........1600
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  RKSAT.........1700
     1   NPCYC,NUCYC,NPRINT,NBCFPR,NBCSPR,NBCPPR,NBCUPR,NLAKPR,IREAD,    RKSAT.........1800
     1   NBGPPR,NBGUPR,ISTORE,NOUMAT,IUNSAT,IALSAT,KTYPE                 RKSAT.........1900
C                                                                        RKSAT.........2000
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- RKSAT.........2100
C     E X A M P L E   C O D I N G   FOR                                  RKSAT.........2200
C     MESH WITH TWO REGIONS OF UNSATURATED PROPERTIES USING              RKSAT.........2300
C     THREE PARAMETER-UNSATURATED FLOW RELATIONSHIPS OF                  RKSAT.........2400
C     VAN GENUCHTEN(1980)                                                RKSAT.........2500
C        RESIDUAL SATURATION, SWRES, GIVEN IN UNITS {L**0}               RKSAT.........2600
C        PARAMETER, VN, GIVEN IN UNITS {L**0}                            RKSAT.........2700
C                                                                        RKSAT.........2800
      REAL SWRES,VN,SWRM1,VNF,SWSTAR                                     RKSAT.........2900
      REAL SWRES1,SWRES2,VN1,VN2                                         RKSAT.........3000
C                                                                        RKSAT.........3100
C     DATA FOR REGION 1:                                                 RKSAT.........3200
      DATA   SWRES1/0.30E0/,   VN1/2.0E0/                                RKSAT.........3300
      SAVE SWRES1, VN1                                                   RKSAT.........3400
C     DATA FOR REGION 2:                                                 RKSAT.........3500
      DATA   SWRES2/0.30E0/,   VN2/2.0E0/                                RKSAT.........3600
      SAVE SWRES2, VN2                                                   RKSAT.........3700
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- RKSAT.........3800
C                                                                        RKSAT.........3900
C *** BECAUSE THIS ROUTINE IS CALLED OFTEN FOR UNSATURATED FLOW RUNS,    RKSAT.........4000
C *** EXECUTION TIME MAY BE SAVED BY CAREFUL CODING OF DESIRED           RKSAT.........4100
C *** RELATIONSHIPS USING ONLY INTEGER AND SINGLE PRECISION VARIABLES!   RKSAT.........4200
C *** RESULTS OF THE CALCULATIONS MUST THEN BE PLACED INTO DOUBLE        RKSAT.........4300
C *** PRECISION VARIABLE RELK BEFORE LEAVING THIS SUBROUTINE.            RKSAT.........4400
C                                                                        RKSAT.........4500
C                                                                        RKSAT.........4600
C*********************************************************************** RKSAT.........4700
C*********************************************************************** RKSAT.........4800
C                                                                        RKSAT.........4900
C     SET PARAMETERS FOR CURRENT REGION, KREG                            RKSAT.........5000
      GOTO(10,20),KREG                                                   RKSAT.........5100
   10 SWRES=SWRES1                                                       RKSAT.........5200
      VN=VN1                                                             RKSAT.........5300
      GOTO 100                                                           RKSAT.........5400
   20 SWRES=SWRES2                                                       RKSAT.........5500
      VN=VN2                                                             RKSAT.........5600
  100 CONTINUE                                                           RKSAT.........5700
C                                                                        RKSAT.........5800
C                                                                        RKSAT.........5900
C*********************************************************************** RKSAT.........6000
C*********************************************************************** RKSAT.........6100
C     RELK VS. P, OR RELK VS. SW.                                        RKSAT.........6200
C     CODING MUST GIVE A VALUE TO RELATIVE PERMEABILITY, RELK.           RKSAT.........6300
C                                                                        RKSAT.........6400
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  RKSAT.........6500
C     GENERAL RELATIVE PERMEABILITY MODEL FROM VAN GENUCHTEN(1980)       RKSAT.........6600
      SWRM1=1.E0-SWRES                                                   RKSAT.........6700
      SWSTAR=(SW-SWRES)/SWRM1                                            RKSAT.........6800
      VNF=(VN-1.E0)/VN                                                   RKSAT.........6900
      R E L K   =   DBLE (SQRT(SWSTAR)*                                  RKSAT.........7000
     1                   (1.E0-(1.E0-SWSTAR**(1.E0/VNF))**(VNF))**2)     RKSAT.........7100
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  RKSAT.........7200
C*********************************************************************** RKSAT.........7300
C*********************************************************************** RKSAT.........7400
C                                                                        RKSAT.........7500
C                                                                        RKSAT.........7600
C                                                                        RKSAT.........7700
C                                                                        RKSAT.........7800
C                                                                        RKSAT.........7900
C                                                                        RKSAT.........8000
 1800 RETURN                                                             RKSAT.........8100
C                                                                        RKSAT.........8200
      END                                                                RKSAT.........8300
