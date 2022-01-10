C     SUBROUTINE        A  R  E  A  S              SUTRA VERSION 3.0     AREAS..........100
C                                                                        AREAS..........200
C *** PURPOSE :                                                          AREAS..........300
C ***  TO CALCULATE AREAS ASSOCIATED WITH SURFACE NODES.                 AREAS..........400
C                                                                        AREAS..........500
      SUBROUTINE AREAS()                                                 AREAS..........600
      USE LARR                                                           AREAS..........700
      USE ALLARR, ONLY : IN, X, Y                                        AREAS..........800
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                AREAS..........900
      DIMENSION F(4,4),DET(4),AREAE(4)                                   AREAS.........1000
      DIMENSION FX(4),FY(4),DFDXL(4),DFDYL(4),XIIX(4),YIIY(4)            AREAS.........1100
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              AREAS.........1200
     1   NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                                  AREAS.........1300
      DATA GLOC/0.577350269189626D0/                                     AREAS.........1400
      DATA XIIX/-1.D0,+1.D0,+1.D0,-1.D0/,                                AREAS.........1500
     1     YIIY/-1.D0,-1.D0,+1.D0,+1.D0/                                 AREAS.........1600
C                                                                        AREAS.........1700
C.....INITIALIZE AREA ARRAY                                              AREAS.........1800
      LAKNOD(:)%AREA = 0D0                                               AREAS.........1900
C                                                                        AREAS.........2000
C.....LOOP THROUGH ALL ELEMENTS TO CARRY OUT SPATIAL INTEGRATION         AREAS.........2100
C        OF AREAS                                                        AREAS.........2200
      DO 9500 L=1,NE                                                     AREAS.........2300
       XIX=-1.D0                                                         AREAS.........2400
       YIY=-1.D0                                                         AREAS.........2500
       KG=0                                                              AREAS.........2600
C                                                                        AREAS.........2700
C......OBTAIN BASIS FUNCTION AND RELATED INFORMATION AT EACH OF          AREAS.........2800
C        FOUR GAUSS POINTS IN THE ELEMENT                                AREAS.........2900
       DO 2200 IYL=1,2                                                   AREAS.........3000
        DO 2100 IXL=1,2                                                  AREAS.........3100
         KG=KG+1                                                         AREAS.........3200
         XLOC=XIX*GLOC                                                   AREAS.........3300
         YLOC=YIY*GLOC                                                   AREAS.........3400
         XF1=1.D0-XLOC                                                   AREAS.........3500
         XF2=1.D0+XLOC                                                   AREAS.........3600
         YF1=1.D0-YLOC                                                   AREAS.........3700
         YF2=1.D0+YLOC                                                   AREAS.........3800
         FX(1)=XF1                                                       AREAS.........3900
         FX(2)=XF2                                                       AREAS.........4000
         FX(3)=XF2                                                       AREAS.........4100
         FX(4)=XF1                                                       AREAS.........4200
         FY(1)=YF1                                                       AREAS.........4300
         FY(2)=YF1                                                       AREAS.........4400
         FY(3)=YF2                                                       AREAS.........4500
         FY(4)=YF2                                                       AREAS.........4600
         DO 10 I=1,4                                                     AREAS.........4700
   10    F(I,KG)=0.250D0*FX(I)*FY(I)                                     AREAS.........4800
         DO 20 I=1,4                                                     AREAS.........4900
         DFDXL(I)=XIIX(I)*0.250D0*FY(I)                                  AREAS.........5000
   20    DFDYL(I)=YIIY(I)*0.250D0*FX(I)                                  AREAS.........5100
         CJ11=0.D0                                                       AREAS.........5200
         CJ12=0.D0                                                       AREAS.........5300
         CJ21=0.D0                                                       AREAS.........5400
         CJ22=0.D0                                                       AREAS.........5500
         DO 100 IL=1,4                                                   AREAS.........5600
         II=(L-1)*8+IL                                                   AREAS.........5700
         I=IN(II)                                                        AREAS.........5800
         CJ11=CJ11+DFDXL(IL)*X(I)                                        AREAS.........5900
         CJ12=CJ12+DFDXL(IL)*Y(I)                                        AREAS.........6000
         CJ21=CJ21+DFDYL(IL)*X(I)                                        AREAS.........6100
  100    CJ22=CJ22+DFDYL(IL)*Y(I)                                        AREAS.........6200
         DET(KG)=CJ11*CJ22-CJ21*CJ12                                     AREAS.........6300
 2100   XIX=-XIX                                                         AREAS.........6400
 2200  YIY=-YIY                                                          AREAS.........6500
C                                                                        AREAS.........6600
C.....INTEGRATE AREAS                                                    AREAS.........6700
 4200  DO 4300 I=1,4                                                     AREAS.........6800
        AREAE(I) = 0.D0                                                  AREAS.........6900
 4300  CONTINUE                                                          AREAS.........7000
       DO 5000 KG=1,4                                                    AREAS.........7100
        DO 4400 I=1,4                                                    AREAS.........7200
         AREAE(I) = AREAE(I) + F(I,KG)*DET(KG)                           AREAS.........7300
 4400   CONTINUE                                                         AREAS.........7400
 5000  CONTINUE                                                          AREAS.........7500
C                                                                        AREAS.........7600
C.....ASSEMBLE AREAS                                                     AREAS.........7700
      N1=(L-1)*8+1                                                       AREAS.........7800
      N4=N1+3                                                            AREAS.........7900
 9050 IE=0                                                               AREAS.........8000
      DO 9100 II=N1,N4                                                   AREAS.........8100
      IE=IE+1                                                            AREAS.........8200
      IB=IN(II)                                                          AREAS.........8300
      ISURF = ISURFACE(IB)                                               AREAS.........8400
      IF (ISURF.GT.0) LAKNOD(ISURF)%AREA=LAKNOD(ISURF)%AREA+AREAE(IE)    AREAS.........8500
      IB=IN(II+4)                                                        AREAS.........8600
      ISURF = ISURFACE(IB)                                               AREAS.........8700
      IF (ISURF.GT.0) LAKNOD(ISURF)%AREA=LAKNOD(ISURF)%AREA+AREAE(IE)    AREAS.........8800
 9100 CONTINUE                                                           AREAS.........8900
C                                                                        AREAS.........9000
 9500 CONTINUE                                                           AREAS.........9100
C                                                                        AREAS.........9200
      RETURN                                                             AREAS.........9300
      END                                                                AREAS.........9400
C                                                                        AREAS.........9500
C     SUBROUTINE        B  T  R  L  V  L           SUTRA VERSION 3.0     BTRLVL.........100
C                                                                        BTRLVL.........200
C *** PURPOSE :                                                          BTRLVL.........300
C ***  TO GENERATE THE ROW OF LAKE NUMBERS FOR A GIVEN LEVEL OF THE      BTRLVL.........400
C ***  LAKE HIERARCHY.                                                   BTRLVL.........500
C                                                                        BTRLVL.........600
      SUBROUTINE BTRLVL(LEVEL,LKOL,CLKOL,CVERT,CHORZ)                    BTRLVL.........700
      USE LARR, ONLY : LCHD, LPAR                                        BTRLVL.........800
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               BTRLVL.........900
      DIMENSION LKOL(*)                                                  BTRLVL........1000
      CHARACTER CLKOL(*)*3, CVERT(*)*3, CHORZ(*)*1                       BTRLVL........1100
      CHARACTER CFMT*100, CFMTV*100, DASHES*3, SPACES*3                  BTRLVL........1200
      COMMON /DIMEN/ NLAKES, NSIL, NCEL, NPLK                            BTRLVL........1300
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8,K9,                 BTRLVL........1400
     1   K10,K11,K12,K13,K14,K15,K16,K17,K18,K19,K20,K21,K22,K23         BTRLVL........1500
      COMMON /LEV/ LEVLST                                                BTRLVL........1600
      DATA DASHES /"---"/                                                BTRLVL........1700
      DATA SPACES /"   "/                                                BTRLVL........1800
C                                                                        BTRLVL........1900
C.....CONVERT LAKE NUMBERS TO CHARACTER STRINGS.                         BTRLVL........2000
      NOL = 2**(LEVEL-1)                                                 BTRLVL........2100
      IF (LEVEL.EQ.1) THEN                                               BTRLVL........2200
         WRITE(CLKOL(1),'(2X,"1")')                                      BTRLVL........2300
      ELSE                                                               BTRLVL........2400
         DO 200 N=1,NOL                                                  BTRLVL........2500
            LK = LKOL(N)                                                 BTRLVL........2600
            IF (LK.EQ.0) THEN                                            BTRLVL........2700
               CLKOL(N) = "   "                                          BTRLVL........2800
            ELSE                                                         BTRLVL........2900
               NDIGS = INT(LOG10(REAL(MAX(LKOL(N),1) + 0.1))) + 1        BTRLVL........3000
               NDSHS = 3 - NDIGS                                         BTRLVL........3100
               WRITE(CFMT,'(A,I1,A)') "(A,I",NDIGS,")"                   BTRLVL........3200
               IF (N.EQ.2*(N/2)) THEN                                    BTRLVL........3300
                  WRITE(CLKOL(N),CFMT) DASHES(1:NDSHS), LK               BTRLVL........3400
               ELSE                                                      BTRLVL........3500
                  WRITE(CLKOL(N),CFMT) SPACES(1:NDSHS), LK               BTRLVL........3600
               END IF                                                    BTRLVL........3700
            END IF                                                       BTRLVL........3800
  200    CONTINUE                                                        BTRLVL........3900
      END IF                                                             BTRLVL........4000
C                                                                        BTRLVL........4100
C.....PRINT THE ROW OF LAKE NUMBERS FOR THE CURRENT LEVEL.               BTRLVL........4200
      DO 400 N=1,NOL                                                     BTRLVL........4300
         LK = LKOL(N)                                                    BTRLVL........4400
         IF (LK.EQ.0) THEN                                               BTRLVL........4500
            CVERT(N) = "   "                                             BTRLVL........4600
            CHORZ(N) = " "                                               BTRLVL........4700
         ELSE                                                            BTRLVL........4800
            IF (LCHD(LK,1).NE.0) THEN                                    BTRLVL........4900
               CVERT(N) = "  |"                                          BTRLVL........5000
            ELSE                                                         BTRLVL........5100
               CVERT(N) = "   "                                          BTRLVL........5200
            END IF                                                       BTRLVL........5300
            IF (N.EQ.2*(N/2)) THEN                                       BTRLVL........5400
               CHORZ(N) = " "                                            BTRLVL........5500
            ELSE                                                         BTRLVL........5600
               CHORZ(N) = "-"                                            BTRLVL........5700
            END IF                                                       BTRLVL........5800
         END IF                                                          BTRLVL........5900
  400 CONTINUE                                                           BTRLVL........6000
      NSPCS0 = 3*(2**(LEVLST-LEVEL) - 1)                                 BTRLVL........6100
      IF (LEVEL.EQ.1) THEN                                               BTRLVL........6200
         WRITE(CFMT,'(A,I9,A)') "(",NSPCS0+1,"X,A3)"                     BTRLVL........6300
         WRITE(CFMTV,'(A,I9,A)') "(",NSPCS0+1,"X,A3)"                    BTRLVL........6400
         WRITE(K23,CFMT) CLKOL(1)                                        BTRLVL........6500
         DO 500 M=1,2                                                    BTRLVL........6600
            WRITE(K23,CFMTV) CVERT(1)                                    BTRLVL........6700
  500    CONTINUE                                                        BTRLVL........6800
      ELSE                                                               BTRLVL........6900
         NOLOT = NOL/2                                                   BTRLVL........7000
         NSPCS = 3*(2**(LEVLST-LEVEL+1) - 1)                             BTRLVL........7100
         WRITE(CFMT,'(A,I9,A,I9,A,I9,A)')                                BTRLVL........7200
     1      "(",NSPCS0+1,"X,",NOL,"(A3,",NSPCS,"(A1)))"                  BTRLVL........7300
         WRITE(CFMTV,'(A,I9,A,I9,A,I9,A,I9,A)')                          BTRLVL........7400
     1      "(",NSPCS0+1,"X,",NOLOT,"(A3,",NSPCS,"X,A3,",NSPCS,"X))"     BTRLVL........7500
         WRITE(K23,CFMT) (CLKOL(N),(CHORZ(N),NH=1,NSPCS),N=1,NOL)        BTRLVL........7600
         DO 550 M=1,2                                                    BTRLVL........7700
            WRITE(K23,CFMTV) (CVERT(N),N=1,NOL)                          BTRLVL........7800
  550    CONTINUE                                                        BTRLVL........7900
      END IF                                                             BTRLVL........8000
C                                                                        BTRLVL........8100
      RETURN                                                             BTRLVL........8200
      END                                                                BTRLVL........8300
C                                                                        BTRLVL........8400
C     SUBROUTINE        F  I  N  D  K  L           SUTRA VERSION 3.0     FINDKL.........100
C                                                                        FINDKL.........200
C *** PURPOSE :                                                          FINDKL.........300
C ***  TO IDENTIFY CLUSTERS THAT DEFINE LAKES.                           FINDKL.........400
C                                                                        FINDKL.........500
      SUBROUTINE FINDKL(STAGE, KLLGST, ACTIVE, LCHD0)                    FINDKL.........600
      USE LARR                                                           FINDKL.........700
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               FINDKL.........800
      LOGICAL ACTIVE(999)                                                FINDKL.........900
      INTEGER LCHD0(999,2)                                               FINDKL........1000
      CHARACTER*80 ERRCOD,CHERR(10)                                      FINDKL........1100
      DIMENSION INERR(10),RLERR(10)                                      FINDKL........1200
      COMMON /DIMSRF/ MAXCON, NNSURF, NNVERT                             FINDKL........1300
C                                                                        FINDKL........1400
C                                                                        FINDKL........1500
C.....FOR THIS STAGE, IDENTIFY DISTINCT CLUSTERS.  USE THEM TO           FINDKL........1600
C        DETERMINE CLUSTER NUMBERS THAT DEFINE LAKES.                    FINDKL........1700
      DO 250 LAKE=1,KLLGST                                               FINDKL........1800
C                                                                        FINDKL........1900
C........IF THE LAKE IS INACTIVE, SKIP IT.                               FINDKL........2000
         IF (.NOT.ACTIVE(LAKE)) CYCLE                                    FINDKL........2100
C                                                                        FINDKL........2200
C........COMPUTE CLUSTER NUMBERS FOR THIS LAKE.  THESE ARE "LOCAL"       FINDKL........2300
C           NUMBERS IN THE SENSE THAT THEY TAKE INTO ACCOUNT ONLY        FINDKL........2400
C           NODES IN THIS LAKE AND THE NUMBERING DIFFERS FROM            FINDKL........2500
C           THE "GLOBAL" NUMBERING WE ARE ULTIMATELY COMPUTING.          FINDKL........2600
         CALL HOSKOP(LAKE, STAGE, KLMAX)                                 FINDKL........2700
C                                                                        FINDKL........2800
         IF (KLMAX.EQ.0) THEN                                            FINDKL........2900
C...........NO CLUSTERS FOUND, WHICH MEANS THAT STAGE IS BELOW           FINDKL........3000
C              LOWEST NODE IN THIS LAKE.  THIS WILL ALWAYS BE THE        FINDKL........3100
C              CASE FROM HERE ON, SO MAKE THE LAKE INACTIVE TO           FINDKL........3200
C              SAVE COMPUTATIONAL EFFORT WHEN PROCESSING THE             FINDKL........3300
C              REMAINING (LOWER) STAGES.  NO CHILDREN FOR THIS LAKE.     FINDKL........3400
            ACTIVE(LAKE) = .FALSE.                                       FINDKL........3500
            LCHD0(LAKE,1) = 0                                            FINDKL........3600
            LCHD0(LAKE,2) = 0                                            FINDKL........3700
         ELSE IF (KLMAX.EQ.2) THEN                                       FINDKL........3800
C...........THIS LAKE HAS SPLIT INTO TWO BECAUSE THE STAGE HAS           FINDKL........3900
C             DROPPED TO A SILL.  UPDATE THE CLUSTER NUMBERS WITHIN      FINDKL........4000
C             THESE TWO NEW LAKES AND DESIGNATE THEM AS ACTIVE AND       FINDKL........4100
C             AS CHILDREN OF THE LAKE THAT SPLIT.  MAKE THE PARENT       FINDKL........4200
C             INACTIVE.                                                  FINDKL........4300
            DO 200 ISURF=1,NNSURF                                        FINDKL........4400
               IF (KL(ISURF).NE.0)                                       FINDKL........4500
     1            LAKNOD(ISURF)%KLUSTR = KLLGST + KL(ISURF)              FINDKL........4600
  200       CONTINUE                                                     FINDKL........4700
            KLLGST = KLLGST + 2                                          FINDKL........4800
            ACTIVE(KLLGST-1) = .TRUE.                                    FINDKL........4900
            ACTIVE(KLLGST) = .TRUE.                                      FINDKL........5000
            LCHD0(LAKE,1) = KLLGST - 1                                   FINDKL........5100
            LCHD0(LAKE,2) = KLLGST                                       FINDKL........5200
            ACTIVE(LAKE) = .FALSE.                                       FINDKL........5300
         ELSE IF (KLMAX.GT.2) THEN                                       FINDKL........5400
C...........THIS LAKE HAS SPLIT INTO MORE THAN TWO BECAUSE THE STAGE     FINDKL........5500
C              HAS DROPPED TO MULTIPLE SILLS OF IDENTICAL ELEVATION.     FINDKL........5600
C              THIS CASE IS DISALLOWED.                                  FINDKL........5700
            ERRCOD = 'LKAR-1B-1'                                         FINDKL........5800
            INERR(1) = LAKE                                              FINDKL........5900
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     FINDKL........6000
         END IF                                                          FINDKL........6100
C                                                                        FINDKL........6200
  250 CONTINUE                                                           FINDKL........6300
C                                                                        FINDKL........6400
      RETURN                                                             FINDKL........6500
      END                                                                FINDKL........6600
C                                                                        FINDKL........6700
C     SUBROUTINE        G  E  T  L  O  N           SUTRA VERSION 3.0     GETLON.........100
C                                                                        GETLON.........200
C *** PURPOSE :                                                          GETLON.........300
C ***  TO FIND LOWEST LAKE TO WHICH EACH LAKE NODE DRAINS.               GETLON.........400
C                                                                        GETLON.........500
      SUBROUTINE GETLON()                                                GETLON.........600
      USE LARR                                                           GETLON.........700
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               GETLON.........800
      LOGICAL HIT1,HIT2                                                  GETLON.........900
      CHARACTER*80 ERRCOD,CHERR(10)                                      GETLON........1000
      DIMENSION INERR(10),RLERR(10)                                      GETLON........1100
      INTEGER KTYPE(2)                                                   GETLON........1200
      INTEGER RMVDIM,IMVDIM,I1VDIM,CMVDIM,LMVDIM,PMVDIM                  GETLON........1300
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  GETLON........1400
     1   NPCYC,NUCYC,NPRINT,NBCFPR,NBCSPR,NBCPPR,NBCUPR,NLAKPR,IREAD,    GETLON........1500
     1   NBGPPR,NBGUPR,ISTORE,NOUMAT,IUNSAT,IALSAT,KTYPE                 GETLON........1600
      COMMON /DIMEN/ NLAKES, NSIL, NCEL, NPLK                            GETLON........1700
      COMMON /DIMSRF/ MAXCON, NNSURF, NNVERT                             GETLON........1800
      COMMON /MEMCNT/ RMVDIM,IMVDIM,I1VDIM,CMVDIM,LMVDIM,PMVDIM          GETLON........1900
C                                                                        GETLON........2000
C.....ALLOCATE AND INITIALIZE ARRAY.                                     GETLON........2100
      ALLOCATE (LLOWN(NNSURF))                                           GETLON........2200
      IMVDIM = IMVDIM + NNSURF                                           GETLON........2300
      LLOWN = 0                                                          GETLON........2400
C                                                                        GETLON........2500
C.....FIND LOWEST LAKE TO WHICH EACH LAKE NODE DRAINS.  SET ANY NODE     GETLON........2600
C        IN A CHILDLESS LAKE TO DRAIN TO THAT LAKE.  OTHERWISE, USE      GETLON........2700
C        AN APPROXIMATE, "DISCRETE" STEEPEST-DESCENT SEARCH.  PROCESS    GETLON........2800
C        FROM HIGHEST TO LOWEST LAKE NUMBER, ASSUMING ALL LAKES ON A     GETLON........2900
C        GIVEN LEVEL OF THE TREE ARE NUMBERED BEFORE NUMBERING ON THE    GETLON........3000
C        NEXT LEVEL BEGINS.                                              GETLON........3100
      DO 600 LK=NLAKES,1,-1                                              GETLON........3200
         KPL = KLOW(LK)                                                  GETLON........3300
         KPH = KHIGH(LK)                                                 GETLON........3400
         IF (LCHD(LK,1).EQ.0) THEN                                       GETLON........3500
            DO 400 K=KPL,KPH                                             GETLON........3600
               ISURF = ELEVND(K)%ISURF                                   GETLON........3700
               IF (LAKNOD(ISURF)%KLUSTR.NE.LK) CYCLE                     GETLON........3800
               LLOWN(ISURF) = LK                                         GETLON........3900
  400       CONTINUE                                                     GETLON........4000
         ELSE                                                            GETLON........4100
            DO 500 K=KPL,KPH                                             GETLON........4200
               ISURF = ELEVND(K)%ISURF                                   GETLON........4300
               IF (LAKNOD(ISURF)%KLUSTR.NE.LK) CYCLE                     GETLON........4400
               ISULO = ISURF                                             GETLON........4500
               DO WHILE (LLOWN(ISULO).EQ.0)                              GETLON........4600
                  SLPNMX = -HUGE(1D0)                                    GETLON........4700
                  DO 450 M=1,MAXCON                                      GETLON........4800
                     IISU = NBR(ISULO,M)                                 GETLON........4900
                     IF (IISU.EQ.0) EXIT                                 GETLON........5000
                     X1 = LAKNOD(ISULO)%X                                GETLON........5100
                     X2 = LAKNOD(IISU)%X                                 GETLON........5200
                     Y1 = LAKNOD(ISULO)%Y                                GETLON........5300
                     Y2 = LAKNOD(IISU)%Y                                 GETLON........5400
                     Z1 = LAKNOD(ISULO)%ELEV                             GETLON........5500
                     Z2 = LAKNOD(IISU)%ELEV                              GETLON........5600
                     DROP = Z1 - Z2                                      GETLON........5700
                     RUN = DSQRT((X2 - X1)**2 + (Y2 - Y1)**2)            GETLON........5800
                     SLPNEG = DROP/RUN                                   GETLON........5900
                     IF (SLPNEG.GT.SLPNMX) THEN                          GETLON........6000
                        SLPNMX = SLPNEG                                  GETLON........6100
                        ISULNX = IISU                                    GETLON........6200
                     END IF                                              GETLON........6300
  450             CONTINUE                                               GETLON........6400
                  IF (SLPNMX.EQ.0D0) THEN                                GETLON........6500
                     DO 470 KK=KPL,K                                     GETLON........6600
                     IKK = ELEVND(KK)%ISURF                              GETLON........6700
                     DO 470 M=1,MAXCON                                   GETLON........6800
                        IISU = NBR(ISULO,M)                              GETLON........6900
                        IF (IISU.EQ.IKK) THEN                            GETLON........7000
                           ISULO = IISU                                  GETLON........7100
                           GOTO 480                                      GETLON........7200
                        END IF                                           GETLON........7300
  470                CONTINUE                                            GETLON........7400
                     ERRCOD = 'COD-GETLON-1'                             GETLON........7500
                     CALL SUTERR(ERRCOD,CHERR,INERR,RLERR)               GETLON........7600
  480                CONTINUE                                            GETLON........7700
                  ELSE IF (SLPNMX.LT.0D0) THEN                           GETLON........7800
                     LLOWN(ISULO) = LAKNOD(ISULO)%KLUSTR                 GETLON........7900
                  ELSE                                                   GETLON........8000
                     ISULO = ISULNX                                      GETLON........8100
                  END IF                                                 GETLON........8200
               END DO                                                    GETLON........8300
               LLOWN(ISURF) = LLOWN(ISULO)                               GETLON........8400
  500       CONTINUE                                                     GETLON........8500
         END IF                                                          GETLON........8600
  600 CONTINUE                                                           GETLON........8700
C                                                                        GETLON........8800
      RETURN                                                             GETLON........8900
      END                                                                GETLON........9000
C                                                                        GETLON........9100
C     SUBROUTINE        G  E  T  L  O  W           SUTRA VERSION 3.0     GETLOW.........100
C                                                                        GETLOW.........200
C *** PURPOSE :                                                          GETLOW.........300
C ***  FIND LOWEST LAKE TO WHICH EACH LAKE (EXCEPT 1) CAN DRAIN WHEN     GETLOW.........400
C ***  SPILLING OVER THE SILL WITH ITS SIBLING.                          GETLOW.........500
C                                                                        GETLOW.........600
      SUBROUTINE GETLOW()                                                GETLOW.........700
      USE LARR                                                           GETLOW.........800
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               GETLOW.........900
      LOGICAL HIT1,HIT2                                                  GETLOW........1000
      CHARACTER*80 ERRCOD,CHERR(10)                                      GETLOW........1100
      DIMENSION INERR(10),RLERR(10)                                      GETLOW........1200
      INTEGER RMVDIM,IMVDIM,I1VDIM,CMVDIM,LMVDIM,PMVDIM                  GETLOW........1300
      COMMON /DIMEN/ NLAKES, NSIL, NCEL, NPLK                            GETLOW........1400
      COMMON /DIMSRF/ MAXCON, NNSURF, NNVERT                             GETLOW........1500
      COMMON /MEMCNT/ RMVDIM,IMVDIM,I1VDIM,CMVDIM,LMVDIM,PMVDIM          GETLOW........1600
C                                                                        GETLOW........1700
C.....ALLOCATE AND INITIALIZE ARRAY.                                     GETLOW........1800
      ALLOCATE (LLOW(NLAKES))                                            GETLOW........1900
      IMVDIM = IMVDIM + NLAKES                                           GETLOW........2000
      LLOW = 0                                                           GETLOW........2100
C                                                                        GETLOW........2200
C.....FIND LOWEST LAKE TO WHICH EACH LAKE (EXCEPT 1) CAN DRAIN WHEN      GETLOW........2300
C       SPILLING OVER THE SILL WITH ITS SIBLING.  USE AN APPROXIMATE,    GETLOW........2400
C       "DISCRETE" STEEPEST-DESCENT SEARCH FROM EACH SILL INTO EACH      GETLOW........2500
C       OF THE TWO SIBLINGS.                                             GETLOW........2600
      DO 800 LK=1,NLAKES                                                 GETLOW........2700
         KPL = KLOW(LK)                                                  GETLOW........2800
         ELSILL = ELEVND(KPL)%ELEV                                       GETLOW........2900
         LKC1 = LCHD(LK,1)                                               GETLOW........3000
         LKC2 = LCHD(LK,2)                                               GETLOW........3100
         IF (LKC1.EQ.0) CYCLE                                            GETLOW........3200
C........IF THERE ARE MULTIPLE NODES WITH THE SILL ELEVATION IN THE LAKE GETLOW........3300
C           ASSUME KLOW IS THE FIRST LISTED.  GO THROUGH THEM UNTIL ONE  GETLOW........3400
C           THAT QUALIFIES AS A SILL IS FOUND.                           GETLOW........3500
         DO 400 K=KPL,NNSURF                                             GETLOW........3600
            IF (ELEVND(K)%ELEV.GT.ELSILL) EXIT                           GETLOW........3700
            ISURF = ELEVND(K)%ISURF                                      GETLOW........3800
            HIT1 = .FALSE.                                               GETLOW........3900
            HIT2 = .FALSE.                                               GETLOW........4000
            DO 200 M=1,MAXCON                                            GETLOW........4100
               IISURF = NBR(ISURF,M)                                     GETLOW........4200
               IF (IISURF.EQ.0) CYCLE                                    GETLOW........4300
               IF (LAKNOD(IISURF)%KLUSTR.EQ.LKC1) THEN                   GETLOW........4400
                  HIT1 = .TRUE.                                          GETLOW........4500
                  ISULO1 = IISURF                                        GETLOW........4600
               ELSE IF (LAKNOD(IISURF)%KLUSTR.EQ.LKC2) THEN              GETLOW........4700
                  HIT2 = .TRUE.                                          GETLOW........4800
                  ISULO2 = IISURF                                        GETLOW........4900
               END IF                                                    GETLOW........5000
  200       CONTINUE                                                     GETLOW........5100
            IF (HIT1.AND.HIT2) GOTO 450                                  GETLOW........5200
  400    CONTINUE                                                        GETLOW........5300
         ERRCOD = 'COD-GETLOW-1'                                         GETLOW........5400
         CALL SUTERR(ERRCOD,CHERR,INERR,RLERR)                           GETLOW........5500
  450    CONTINUE                                                        GETLOW........5600
         DO 600 LC=1,2                                                   GETLOW........5700
            IF (LC.EQ.1) THEN                                            GETLOW........5800
               ISULO = ISULO1                                            GETLOW........5900
               LKC = LKC1                                                GETLOW........6000
            ELSE                                                         GETLOW........6100
               ISULO = ISULO2                                            GETLOW........6200
               LKC = LKC2                                                GETLOW........6300
            END IF                                                       GETLOW........6400
            LLOW(LKC) = LLOWN(ISULO)                                     GETLOW........6500
  600    CONTINUE                                                        GETLOW........6600
  800 CONTINUE                                                           GETLOW........6700
C                                                                        GETLOW........6800
      RETURN                                                             GETLOW........6900
      END                                                                GETLOW........7000
C                                                                        GETLOW........7100
C     SUBROUTINE        G  E  T  P  L  K           SUTRA VERSION 3.0     GETPLK.........100
C                                                                        GETPLK.........200
C *** PURPOSE :                                                          GETPLK.........300
C ***  IDENTIFY ACTIVE LAKE NODES AND UPDATE THEIR LAKE PRESSURES.       GETPLK.........400
C                                                                        GETPLK.........500
      SUBROUTINE GETPLK()                                                GETPLK.........600
      USE LARR                                                           GETPLK.........700
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               GETPLK.........800
      COMMON /DIMEN/ NLAKES, NSIL, NCEL, NPLK                            GETPLK.........900
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              GETPLK........1000
     1   NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                                  GETPLK........1100
      COMMON /DIMSRF/ MAXCON, NNSURF, NNVERT                             GETPLK........1200
      COMMON /GRAVEC/ GRAVX,GRAVY,GRAVZ                                  GETPLK........1300
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      GETPLK........1400
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        GETPLK........1500
      COMMON /RHOLAK/ RHOLK                                              GETPLK........1600
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           GETPLK........1700
      EXTERNAL INCA                                                      GETPLK........1800
C                                                                        GETPLK........1900
C.....IDENTIFY ACTIVE LAKE NODES AND UPDATE LAKE PRESSURES.              GETPLK........2000
      ISLAKE = .FALSE.                                                   GETPLK........2100
      PLK = 0D0                                                          GETPLK........2200
      DO 200 ISURF=1,NNSURF                                              GETPLK........2300
         IF (.NOT.LAKNOD(ISURF)%LAKEOK) CYCLE                            GETPLK........2400
         ILN = LAKNOD(ISURF)%INODE                                       GETPLK........2500
  160    LKK = LAKNOD(ISURF)%KLUSTR                                      GETPLK........2600
C........FIND ACTIVE ANCESTOR, IF ANY.                                   GETPLK........2700
         LK = INCA(ISTAT,LKK)                                            GETPLK........2800
         IF ((ISTAT(LK).NE.0).AND.(ISTAT(LK).NE.-1)) THEN                GETPLK........2900
            STG = STGLAK(LK)                                             GETPLK........3000
            ELV = LAKNOD(ISURF)%ELEV                                     GETPLK........3100
            IF (STG.GE.ELV) THEN                                         GETPLK........3200
               ISLAKE(ILN) = .TRUE.                                      GETPLK........3300
               PLAK = -RHOLK*GRAVZ*(STG - ELV)                           GETPLK........3400
               PLK(ISURF) = PLAK                                         GETPLK........3500
            END IF                                                       GETPLK........3600
         END IF                                                          GETPLK........3700
  200 CONTINUE                                                           GETPLK........3800
C                                                                        GETPLK........3900
C.....SET LOWEST NODE(S) IN EACH DRY LAKE TO BE AN ACTIVE LAKE NODE      GETPLK........4000
C        TO ALLOW REWETTING.                                             GETPLK........4100
      DO 400 LK=1,NLAKES                                                 GETPLK........4200
         IF (ISTAT(LK).EQ.0) THEN                                        GETPLK........4300
            KLO = KLOW(LK)                                               GETPLK........4400
            KHI = KHIGH(LK)                                              GETPLK........4500
            ELO = ELEVND(KLO)%ELEV                                       GETPLK........4600
            DO 300 K=KLO,KHI                                             GETPLK........4700
               IF (ELEVND(K)%ELEV.EQ.ELO) THEN                           GETPLK........4800
                  ISURF = ELEVND(K)%ISURF                                GETPLK........4900
                  ILN = LAKNOD(ISURF)%INODE                              GETPLK........5000
                  ISLAKE(ILN) = .TRUE.                                   GETPLK........5100
                  PLK(ISURF) = 0D0                                       GETPLK........5200
               ELSE                                                      GETPLK........5300
                  EXIT                                                   GETPLK........5400
               END IF                                                    GETPLK........5500
  300       CONTINUE                                                     GETPLK........5600
         END IF                                                          GETPLK........5700
  400 CONTINUE                                                           GETPLK........5800
C                                                                        GETPLK........5900
      RETURN                                                             GETPLK........6000
      END                                                                GETPLK........6100
C                                                                        GETPLK........6200
C     SUBROUTINE        H  I  L  O                 SUTRA VERSION 3.0     HILO...........100
C                                                                        HILO...........200
C *** PURPOSE :                                                          HILO...........300
C ***  TO FIND, IN A LIST OF NODES SORTED BY ELEVATION, THE FIRST        HILO...........400
C ***  (LOWEST), LAST (HIGHEST), AND MAX (HIGHEST LIMITED) FOR EACH      HILO...........500
C ***  LAKE, AND TO IDENTIFY LIMITED AND ALWAYS-SPLIT LAKES.             HILO...........600
C                                                                        HILO...........700
      SUBROUTINE HILO()                                                  HILO...........800
      USE LARR                                                           HILO...........900
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               HILO..........1000
      INTEGER RMVDIM,IMVDIM,I1VDIM,CMVDIM,LMVDIM,PMVDIM                  HILO..........1100
      COMMON /DIMEN/ NLAKES, NSIL, NCEL, NPLK                            HILO..........1200
      COMMON /DIMSRF/ MAXCON, NNSURF, NNVERT                             HILO..........1300
      COMMON /MEMCNT/ RMVDIM,IMVDIM,I1VDIM,CMVDIM,LMVDIM,PMVDIM          HILO..........1400
C                                                                        HILO..........1500
      ALLOCATE(KLOW(NLAKES),KHIGH(NLAKES),KVMAX(NLAKES))                 HILO..........1600
      IMVDIM = IMVDIM + 3*NLAKES                                         HILO..........1700
      ALLOCATE(ISLIMITED(NLAKES),ISSPLIT(NLAKES))                        HILO..........1800
      LMVDIM = LMVDIM + 2*NLAKES                                         HILO..........1900
C                                                                        HILO..........2000
C.....FIND FIRST (LOWEST) NODE FOR EACH LAKE NUMBER. (FOR LAKE WITH      HILO..........2100
C        CHILDREN, LOWEST NODE IS AT SILL ELEVATION.)                    HILO..........2200
      KLOW = 0                                                           HILO..........2300
      DO 200 K=1,NNSURF                                                  HILO..........2400
         ISURF = ELEVND(K)%ISURF                                         HILO..........2500
         LK = LAKNOD(ISURF)%KLUSTR                                       HILO..........2600
         IF (KLOW(LK).EQ.0) KLOW(LK) = K                                 HILO..........2700
  200 CONTINUE                                                           HILO..........2800
C                                                                        HILO..........2900
C.....DISQUALIFY LAKES WHOSE LOWEST NODE CANNOT HAVE LAKE WATER.         HILO..........3000
      DO 300 LK=1,NLAKES                                                 HILO..........3100
         ISURLOW = ELEVND(KLOW(LK))%ISURF                                HILO..........3200
         IF ((.NOT.LAKNOD(ISURLOW)%LAKEOK).OR.                           HILO..........3300
     1       (LAKNOD(ISURLOW)%ISEDGE)) THEN                              HILO..........3400
            ISTAT(LK) = -4                                               HILO..........3500
            DO 280 ISURF=1,NNSURF                                        HILO..........3600
               IF (LAKNOD(ISURF)%KLUSTR.EQ.LK)                           HILO..........3700
     1            LAKNOD(ISURF)%LAKEOK = .FALSE.                         HILO..........3800
  280       CONTINUE                                                     HILO..........3900
         END IF                                                          HILO..........4000
  300 CONTINUE                                                           HILO..........4100
C                                                                        HILO..........4200
C.....FIND LAST AND "MAX" NODES FOR EACH LAKE NUMBER. SET LIMITED        HILO..........4300
C        AND ALWAYS-SPLIT LAKES.                                         HILO..........4400
      KHIGH = 0                                                          HILO..........4500
      DO 490 LK=1,NLAKES                                                 HILO..........4600
         KVMAX(LK) = KLOW(LPAR(LK))                                      HILO..........4700
  490 CONTINUE                                                           HILO..........4800
      ISLIMITED = .FALSE.                                                HILO..........4900
      DO 500 K=1,NNSURF                                                  HILO..........5000
         ISURF = ELEVND(K)%ISURF                                         HILO..........5100
         LK = LAKNOD(ISURF)%KLUSTR                                       HILO..........5200
         KHIGH(LK) = K                                                   HILO..........5300
         IF (.NOT.ISLIMITED(LK)) THEN                                    HILO..........5400
            IF ((.NOT.LAKNOD(ISURF)%LAKEOK).OR.                          HILO..........5500
     1          (LAKNOD(ISURF)%ISEDGE)) THEN                             HILO..........5600
               KVMAX(LK) = K                                             HILO..........5700
               ISLIMITED(LK) = .TRUE.                                    HILO..........5800
            END IF                                                       HILO..........5900
         END IF                                                          HILO..........6000
  500 CONTINUE                                                           HILO..........6100
      ISSPLIT = .FALSE.                                                  HILO..........6200
      DO 550 LK=NLAKES,2,-1                                              HILO..........6300
         IF (ISLIMITED(LK)) THEN                                         HILO..........6400
            LKK = LK                                                     HILO..........6500
            DO WHILE (LKK.NE.1)                                          HILO..........6600
               LKK = LPAR(LKK)                                           HILO..........6700
               ISSPLIT(LKK) = .TRUE.                                     HILO..........6800
            END DO                                                       HILO..........6900
         END IF                                                          HILO..........7000
  550 CONTINUE                                                           HILO..........7100
C                                                                        HILO..........7200
      RETURN                                                             HILO..........7300
      END                                                                HILO..........7400
C                                                                        HILO..........7500
C     SUBROUTINE        H  O  S  K  O  P           SUTRA VERSION 3.0     HOSKOP.........100
C                                                                        HOSKOP.........200
C *** PURPOSE :                                                          HOSKOP.........300
C ***  TO IDENTIFY CLUSTERS WITHIN A LAKE ON AN IRREGULAR MESH USING     HOSKOP.........400
C ***  A SIMPLE IMPLEMENTATION OF THE GENERALIZED HOSHEN-KOPELMAN        HOSKOP.........500
C ***  ALGORITHM (HOSHEN & KOPELMAN, 1976; AL-FUTAISI & PATZEK, 2002).   HOSKOP.........600
C ***  IF LAKE=-1 ON INPUT, CLUSTERS ARE IDENTIFIED FOR THE ENTIRE       HOSKOP.........700
C ***  DOMAIN.                                                           HOSKOP.........800
C                                                                        HOSKOP.........900
      SUBROUTINE HOSKOP(LAKE, STAGE, KLMAX)                              HOSKOP........1000
      USE LARR                                                           HOSKOP........1100
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               HOSKOP........1200
      LOGICAL ISSUBM                                                     HOSKOP........1300
      DIMENSION LAC(0:NNSURF)                                            HOSKOP........1400
      INTEGER, DIMENSION (:), ALLOCATABLE :: LACTAL, LACC                HOSKOP........1500
      COMMON /DIMSRF/ MAXCON, NNSURF, NNVERT                             HOSKOP........1600
      EXTERNAL ISSUBM                                                    HOSKOP........1700
C                                                                        HOSKOP........1800
C.....INITIALIZE NUMBER OF CLUSTERS, EQUIVALENCE CLASS LABELS, AND       HOSKOP........1900
C        CLUSTER NUMBERS.                                                HOSKOP........2000
      KLMAX = 0                                                          HOSKOP........2100
      DO 50 L=0,NNSURF                                                   HOSKOP........2200
         LAC(L) = L                                                      HOSKOP........2300
   50 CONTINUE                                                           HOSKOP........2400
      KL = 0                                                             HOSKOP........2500
C                                                                        HOSKOP........2600
C.....MAKE THE NODE-BY-NODE HOSHEN-KOPELMAN PASS.                        HOSKOP........2700
      DO 500 ISURF=1,NNSURF                                              HOSKOP........2800
C........IF NODE IS IN THE LAKE, COMPUTE ITS CLUSTER NUMBER;             HOSKOP........2900
C           ELSE GIVE IT A CLUSTER NUMBER OF ZERO.                       HOSKOP........3000
         IF (((LAKNOD(ISURF)%KLUSTR.EQ.LAKE).OR.(LAKE.EQ.-1)).AND.       HOSKOP........3100
     1         ISSUBM(ISURF,STAGE)) THEN                                 HOSKOP........3200
C...........CHECK NEIGHBORS AND DETERMINE THE SMALLEST NON-ZERO          HOSKOP........3300
C              EQUIVALENCE CLASS LABEL.                                  HOSKOP........3400
            KLSML = HUGE(1)                                              HOSKOP........3500
            DO 400 M=1,MAXCON                                            HOSKOP........3600
               IISURF = NBR(ISURF,M)                                     HOSKOP........3700
               IF (IISURF.EQ.0) EXIT                                     HOSKOP........3800
               IF (KL(IISURF).NE.0) KLSML = MIN(KLSML,LAC(KL(IISURF)))   HOSKOP........3900
  400       CONTINUE                                                     HOSKOP........4000
C...........IF ALL NEIGHBORS HAVE ZERO CLUSTER NUMBER,                   HOSKOP........4100
C              GIVE NODE A NEW CLUSTER NUMBER.  ELSE MERGE IT INTO ONE   HOSKOP........4200
C              CLUSTER WITH ITS LABELLED NEIGHBORS USING THE             HOSKOP........4300
C              SMALLEST EQUIVALENCE CLASS LABEL.                         HOSKOP........4400
            IF (KLSML.NE.HUGE(1)) THEN                                   HOSKOP........4500
               KL(ISURF) = KLSML                                         HOSKOP........4600
               DO 420 M=1,MAXCON                                         HOSKOP........4700
                  IISURF = NBR(ISURF,M)                                  HOSKOP........4800
                  IF (IISURF.EQ.0) EXIT                                  HOSKOP........4900
                  IF (KL(IISURF).NE.0) LAC(KL(IISURF)) = KLSML           HOSKOP........5000
  420          CONTINUE                                                  HOSKOP........5100
            ELSE                                                         HOSKOP........5200
               KLMAX = KLMAX + 1                                         HOSKOP........5300
               KL(ISURF) = KLMAX                                         HOSKOP........5400
            END IF                                                       HOSKOP........5500
         ELSE                                                            HOSKOP........5600
            KL(ISURF) = 0                                                HOSKOP........5700
         END IF                                                          HOSKOP........5800
  500 CONTINUE                                                           HOSKOP........5900
C                                                                        HOSKOP........6000
C.....COLLAPSE EQUIVALENCE CLASS LABELS.                                 HOSKOP........6100
      DO 700 L=1,KLMAX                                                   HOSKOP........6200
         LL = L                                                          HOSKOP........6300
         LLAC = LAC(LL)                                                  HOSKOP........6400
  600    IF (LLAC.NE.LL) THEN                                            HOSKOP........6500
            LL = LLAC                                                    HOSKOP........6600
            LLAC = LAC(LL)                                               HOSKOP........6700
            GOTO 600                                                     HOSKOP........6800
         ELSE                                                            HOSKOP........6900
            GOTO 650                                                     HOSKOP........7000
         END IF                                                          HOSKOP........7100
  650    LAC(L) = LL                                                     HOSKOP........7200
  700 CONTINUE                                                           HOSKOP........7300
C.....GET RID OF GAPS IN THE NUMBERING AND UPDATE KLMAX.                 HOSKOP........7400
      ALLOCATE (LACTAL(KLMAX),LACC(0:KLMAX))                             HOSKOP........7500
      DO 740 L=1,KLMAX                                                   HOSKOP........7600
         LACTAL(L) = 0                                                   HOSKOP........7700
  740 CONTINUE                                                           HOSKOP........7800
      DO 750 L=1,KLMAX                                                   HOSKOP........7900
         LACTAL(LAC(L)) = LACTAL(LAC(L)) + 1                             HOSKOP........8000
  750 CONTINUE                                                           HOSKOP........8100
      LACC(0) = 0                                                        HOSKOP........8200
      LL = 0                                                             HOSKOP........8300
      DO 760 L=1,KLMAX                                                   HOSKOP........8400
         IF (LACTAL(L).GT.0) THEN                                        HOSKOP........8500
            LL = LL + 1                                                  HOSKOP........8600
            LACC(L) = LL                                                 HOSKOP........8700
         END IF                                                          HOSKOP........8800
  760 CONTINUE                                                           HOSKOP........8900
      DEALLOCATE(LACTAL)                                                 HOSKOP........9000
      KLMAX = LL                                                         HOSKOP........9100
C                                                                        HOSKOP........9200
C.....MAKE A PASS TO CONVERT TO COLLAPSED LABELS                         HOSKOP........9300
      DO 800 ISURF=1,NNSURF                                              HOSKOP........9400
         KL(ISURF) = LACC(LAC(KL(ISURF)))                                HOSKOP........9500
  800 CONTINUE                                                           HOSKOP........9600
      DEALLOCATE(LACC)                                                   HOSKOP........9700
C                                                                        HOSKOP........9800
      RETURN                                                             HOSKOP........9900
      END                                                                HOSKOP.......10000
C                                                                        HOSKOP.......10100
C     FUNCTION          I  N  C  A                 SUTRA VERSION 3.0     INCA...........100
C                                                                        INCA...........200
C *** PURPOSE :                                                          INCA...........300
C ***  TO RETURN THE NON-COALESCED ANCESTOR OF A GIVEN LAKE.             INCA...........400
C                                                                        INCA...........500
      INTEGER FUNCTION INCA(ISTAL,LK)                                    INCA...........600
      USE LARR, ONLY : LPAR                                              INCA...........700
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                INCA...........800
      DIMENSION ISTAL(NLAKES)                                            INCA...........900
      COMMON /DIMEN/ NLAKES, NSIL, NCEL, NPLK                            INCA..........1000
C                                                                        INCA..........1100
C.....RETURN NON-COALESCED ANCESTOR.                                     INCA..........1200
      LKK = LK                                                           INCA..........1300
      DO WHILE (ISTAL(LKK).EQ.-3)                                        INCA..........1400
         LKK = LPAR(LKK)                                                 INCA..........1500
      END DO                                                             INCA..........1600
      INCA = LKK                                                         INCA..........1700
C                                                                        INCA..........1800
      RETURN                                                             INCA..........1900
      END                                                                INCA..........2000
C                                                                        INCA..........2100
C     FUNCTION          I  N  C  D                 SUTRA VERSION 3.0     INCD...........100
C                                                                        INCD...........200
C *** PURPOSE :                                                          INCD...........300
C ***  TO RETURN THE NON-COALESCED DESCENDANT OF A GIVEN LAKE.           INCD...........400
C                                                                        INCD...........500
      INTEGER FUNCTION INCD(LK)                                          INCD...........600
      USE LARR                                                           INCD...........700
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                INCD...........800
C                                                                        INCD...........900
C.....RETURN NON-COALESCED DESCENDANT.                                   INCD..........1000
      LKK = LLOW(LK)                                                     INCD..........1100
      DO WHILE (ISTAT(LKK).LT.0)                                         INCD..........1200
         LKK = LPAR(LKK)                                                 INCD..........1300
      END DO                                                             INCD..........1400
      INCD = LKK                                                         INCD..........1500
C                                                                        INCD..........1600
      RETURN                                                             INCD..........1700
      END                                                                INCD..........1800
C                                                                        INCD..........1900
C     SUBROUTINE        I  N  I  T  L  N           SUTRA VERSION 3.0     INITLN.........100
C                                                                        INITLN.........200
C *** PURPOSE :                                                          INITLN.........300
C ***  TO INITIALIZE THE POTENTIAL LAKE NODES.                           INITLN.........400
C                                                                        INITLN.........500
      SUBROUTINE INITLN()                                                INITLN.........600
      USE ALLARR, ONLY : X, Y, Z                                         INITLN.........700
      USE LARR                                                           INITLN.........800
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               INITLN.........900
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              INITLN........1000
     1   NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                                  INITLN........1100
      COMMON /DIMSRF/ MAXCON, NNSURF, NNVERT                             INITLN........1200
      COMMON /SOLVI/ KSOLVP, KSOLVU, NN1, NN2, NN3                       INITLN........1300
C                                                                        INITLN........1400
C.....COMPUTE AREAS.                                                     INITLN........1500
      CALL AREAS()                                                       INITLN........1600
C                                                                        INITLN........1700
C.....ASSIGN COORDINATES AND ELEVATIONS.  INITIALIZE ALL                 INITLN........1800
C        CLUSTER NUMBERS TO ONE.                                         INITLN........1900
      DO 100 ISURF=1,NNSURF                                              INITLN........2000
         N = LAKNOD(ISURF)%INODE                                         INITLN........2100
         LAKNOD(ISURF)%X = X(N)                                          INITLN........2200
         LAKNOD(ISURF)%Y = Y(N)                                          INITLN........2300
         LAKNOD(ISURF)%ELEV = Z(N)                                       INITLN........2400
         LAKNOD(ISURF)%LAKEOK = .TRUE.                                   INITLN........2500
         LAKNOD(ISURF)%KLUSTR = 1                                        INITLN........2600
  100 CONTINUE                                                           INITLN........2700
C                                                                        INITLN........2800
      RETURN                                                             INITLN........2900
      END                                                                INITLN........3000
C                                                                        INITLN........3100
C     SUBROUTINE        I  N  P  L  K  A  R        SUTRA VERSION 3.0     INPLKAR........100
C                                                                        INPLKAR........200
C *** PURPOSE :                                                          INPLKAR........300
C ***  TO READ AND PROCESS LAKE-AREA SPECIFICATIONS, AND, OPTIONALLY,    INPLKAR........400
C ***  LAKE-BOTTOM ELEVATIONS.                                           INPLKAR........500
C                                                                        INPLKAR........600
      SUBROUTINE INPLKAR()                                               INPLKAR........700
      USE LARR                                                           INPLKAR........800
      USE EXPINT                                                         INPLKAR........900
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               INPLKAR.......1000
      CHARACTER*80 ERRCOD,CHERR(10),UNAME,FNAME(0:23)                    INPLKAR.......1100
      CHARACTER INTFIL*1000                                              INPLKAR.......1200
      CHARACTER*9 CBOT                                                   INPLKAR.......1300
      CHARACTER*10 CDUM10                                                INPLKAR.......1400
      DIMENSION INERR(10),RLERR(10)                                      INPLKAR.......1500
      COMMON /DIMEN/ NLAKES, NSIL, NCEL, NPLK                            INPLKAR.......1600
      COMMON /DIMSRF/ MAXCON, NNSURF, NNVERT                             INPLKAR.......1700
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8,K9,                 INPLKAR.......1800
     1   K10,K11,K12,K13,K14,K15,K16,K17,K18,K19,K20,K21,K22,K23         INPLKAR.......1900
C                                                                        INPLKAR.......2000
C.....INITIALIZE ALL SURFACE NODES TO NOT ALLOW LAKES.                   INPLKAR.......2100
      LAKNOD(:)%LAKEOK = .FALSE.                                         INPLKAR.......2200
C                                                                        INPLKAR.......2300
C.....READ AND PROCESS LAKE-AREA SPECS.                                  INPLKAR.......2400
      ERRCOD = "REA-LKAR-1A"                                             INPLKAR.......2500
      CALL READIF(K18, 0, INTFIL, ERRCOD)                                INPLKAR.......2600
      READ(INTFIL,*,IOSTAT=INERR(1)) CDUM10, NLAN, CBOT                  INPLKAR.......2700
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INPLKAR.......2800
      IF (CDUM10.NE.'LAKE      ') THEN                                   INPLKAR.......2900
         ERRCOD = 'LKAR-1A-1'                                            INPLKAR.......3000
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INPLKAR.......3100
      END IF                                                             INPLKAR.......3200
      DO 200 ILAN=1,NLAN                                                 INPLKAR.......3300
         ERRCOD = "REA-LKAR-1B"                                          INPLKAR.......3400
         CALL READIF(K18, 0, INTFIL, ERRCOD)                             INPLKAR.......3500
         IF (CBOT.EQ.'DEFAULT') THEN                                     INPLKAR.......3600
            READ(INTFIL,*,IOSTAT=INERR(1)) IL                            INPLKAR.......3700
         ELSE IF (CBOT.EQ.'SPECIFIED') THEN                              INPLKAR.......3800
            READ(INTFIL,*) IL, ELVLB                                     INPLKAR.......3900
         ELSE                                                            INPLKAR.......4000
            ERRCOD = "LKAR-1B-2"                                         INPLKAR.......4100
            CHERR(1) = CBOT                                              INPLKAR.......4200
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INPLKAR.......4300
         END IF                                                          INPLKAR.......4400
         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)     INPLKAR.......4500
         ISURF = ISURFACE(IL)                                            INPLKAR.......4600
         IF (ISURF.EQ.0) THEN                                            INPLKAR.......4700
            ERRCOD = "LKAR-1B-3"                                         INPLKAR.......4800
            INERR(1) = IL                                                INPLKAR.......4900
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INPLKAR.......5000
         END IF                                                          INPLKAR.......5100
         LAKNOD(ISURF)%LAKEOK = .TRUE.                                   INPLKAR.......5200
         IF (CBOT.EQ.'SPECIFIED') LAKNOD(ISURF)%ELEV = ELVLB             INPLKAR.......5300
  200 CONTINUE                                                           INPLKAR.......5400
      ERRCOD = "REA-LKAR-1B"                                             INPLKAR.......5500
      CALL READIF(K18, 0, INTFIL, ERRCOD)                                INPLKAR.......5600
      READ(INTFIL,*,IOSTAT=INERR(1)) IL                                  INPLKAR.......5700
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INPLKAR.......5800
      IF (IL.NE.0) THEN                                                  INPLKAR.......5900
         ERRCOD = "LKAR-1B-4"                                            INPLKAR.......6000
         INERR(1) = NLAN                                                 INPLKAR.......6100
         CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                        INPLKAR.......6200
      END IF                                                             INPLKAR.......6300
C                                                                        INPLKAR.......6400
      RETURN                                                             INPLKAR.......6500
      END                                                                INPLKAR.......6600
C                                                                        INPLKAR.......6700
C     SUBROUTINE        I  N  P  L  K  B  C        SUTRA VERSION 3.0     INPLKBC........100
C                                                                        INPLKBC........200
C *** PURPOSE :                                                          INPLKBC........300
C ***  TO READ AND PROCESS SPECIFICATIONS FOR INTERACTIONS OF LAKES      INPLKBC........400
C ***  WITH BOUNDARY CONDITIONS.                                         INPLKBC........500
C                                                                        INPLKBC........600
      SUBROUTINE INPLKBC()                                               INPLKBC........700
      USE LARR                                                           INPLKBC........800
      USE BCSDEF, ONLY : FNAMB                                           INPLKBC........900
      USE EXPINT                                                         INPLKBC.......1000
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               INPLKBC.......1100
      CHARACTER*80 ERRCOD,CHERR(10),UNAME,FNAME(0:23)                    INPLKBC.......1200
      CHARACTER INTFIL*1000                                              INPLKBC.......1300
      CHARACTER BCSFNM*80                                                INPLKBC.......1400
      CHARACTER*1 CTIPG1, CTIUG1                                         INPLKBC.......1500
      DIMENSION INERR(10),RLERR(10)                                      INPLKBC.......1600
      COMMON /DIMEN/ NLAKES, NSIL, NCEL, NPLK                            INPLKBC.......1700
      COMMON /DIMSRF/ MAXCON, NNSURF, NNVERT                             INPLKBC.......1800
      COMMON /FUNIB/ NFBCS                                               INPLKBC.......1900
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8,K9,                 INPLKBC.......2000
     1   K10,K11,K12,K13,K14,K15,K16,K17,K18,K19,K20,K21,K22,K23         INPLKBC.......2100
C                                                                        INPLKBC.......2200
C.....READ INTERACTION COUNTS.                                           INPLKBC.......2300
      ERRCOD = "REA-LKBC-1"                                              INPLKBC.......2400
      CALL READIF(K17, 0, INTFIL, ERRCOD)                                INPLKBC.......2500
      READ(INTFIL,*,IOSTAT=INERR(1)) NBCIF,NBCIS,NBCIP,NBCIU,            INPLKBC.......2600
     1   NBCIPG,NBCIUG                                                   INPLKBC.......2700
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INPLKBC.......2800
C                                                                        INPLKBC.......2900
C.....READ AND PROCESS FLUID SOURCE BOUNDARY CONDITION INTERACTIONS.     INPLKBC.......3000
      DO 200 IBCIF=1,NBCIF                                               INPLKBC.......3100
         ERRCOD = "REA-LKBC-2"                                           INPLKBC.......3200
         CALL READIF(K17, 0, INTFIL, ERRCOD)                             INPLKBC.......3300
         READ(INTFIL,*,IOSTAT=INERR(1)) BCSFNM, ILKF1                    INPLKBC.......3400
         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)     INPLKBC.......3500
         IF ((IBCIF.EQ.1).AND.(BCSFNM.NE."DEFAULT")) THEN                INPLKBC.......3600
            ERRCOD = "LKBC-2-1"                                          INPLKBC.......3700
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INPLKBC.......3800
         ELSE IF (BCSFNM.EQ."DEFAULT") THEN                              INPLKBC.......3900
            IF (IBCIF.NE.1) THEN                                         INPLKBC.......4000
               ERRCOD = "LKBC-2-2"                                       INPLKBC.......4100
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                  INPLKBC.......4200
            ELSE                                                         INPLKBC.......4300
               ILKF = ILKF1                                              INPLKBC.......4400
            END IF                                                       INPLKBC.......4500
         ELSE                                                            INPLKBC.......4600
            DO 150 NFB=1,NFBCS                                           INPLKBC.......4700
               IF (FNAMB(NFB).EQ.BCSFNM) THEN                            INPLKBC.......4800
                  ILKF(NFB) = ILKF1                                      INPLKBC.......4900
                  GOTO 160                                               INPLKBC.......5000
               END IF                                                    INPLKBC.......5100
  150       CONTINUE                                                     INPLKBC.......5200
            ERRCOD = "LKBC-2-3"                                          INPLKBC.......5300
            CHERR(2) = BCSFNM                                            INPLKBC.......5400
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INPLKBC.......5500
  160       CONTINUE                                                     INPLKBC.......5600
         END IF                                                          INPLKBC.......5700
  200 CONTINUE                                                           INPLKBC.......5800
C                                                                        INPLKBC.......5900
C.....READ AND PROCESS SOLUTE/ENERGY SOURCE BOUNDARY CONDITION           INPLKBC.......6000
C        INTERACTIONS.                                                   INPLKBC.......6100
      DO 230 IBCIS=1,NBCIS                                               INPLKBC.......6200
         ERRCOD = "REA-LKBC-3"                                           INPLKBC.......6300
         CALL READIF(K17, 0, INTFIL, ERRCOD)                             INPLKBC.......6400
         READ(INTFIL,*,IOSTAT=INERR(1)) BCSFNM, ILKS1                    INPLKBC.......6500
         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)     INPLKBC.......6600
         IF ((IBCIS.EQ.1).AND.(BCSFNM.NE."DEFAULT")) THEN                INPLKBC.......6700
            ERRCOD = "LKBC-3-1"                                          INPLKBC.......6800
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INPLKBC.......6900
         ELSE IF (BCSFNM.EQ."DEFAULT") THEN                              INPLKBC.......7000
            IF (IBCIS.NE.1) THEN                                         INPLKBC.......7100
               ERRCOD = "LKBC-3-2"                                       INPLKBC.......7200
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                  INPLKBC.......7300
            ELSE                                                         INPLKBC.......7400
               ILKS = ILKS1                                              INPLKBC.......7500
            END IF                                                       INPLKBC.......7600
         ELSE                                                            INPLKBC.......7700
            DO 210 NFB=1,NFBCS                                           INPLKBC.......7800
               IF (FNAMB(NFB).EQ.BCSFNM) THEN                            INPLKBC.......7900
                  ILKS(NFB) = ILKS1                                      INPLKBC.......8000
                  GOTO 220                                               INPLKBC.......8100
               END IF                                                    INPLKBC.......8200
  210       CONTINUE                                                     INPLKBC.......8300
            ERRCOD = "LKBC-3-3"                                          INPLKBC.......8400
            CHERR(2) = BCSFNM                                            INPLKBC.......8500
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INPLKBC.......8600
  220       CONTINUE                                                     INPLKBC.......8700
         END IF                                                          INPLKBC.......8800
  230 CONTINUE                                                           INPLKBC.......8900
C                                                                        INPLKBC.......9000
C.....READ AND PROCESS SPEC-P BOUNDARY CONDITION INTERACTIONS.           INPLKBC.......9100
      DO 300 IBCIP=1,NBCIP                                               INPLKBC.......9200
         ERRCOD = "REA-LKBC-4"                                           INPLKBC.......9300
         CALL READIF(K17, 0, INTFIL, ERRCOD)                             INPLKBC.......9400
         READ(INTFIL,*,IOSTAT=INERR(1)) BCSFNM, ILKP1                    INPLKBC.......9500
         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)     INPLKBC.......9600
         IF ((IBCIP.EQ.1).AND.(BCSFNM.NE."DEFAULT")) THEN                INPLKBC.......9700
            ERRCOD = "LKBC-4-1"                                          INPLKBC.......9800
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INPLKBC.......9900
         ELSE IF (BCSFNM.EQ."DEFAULT") THEN                              INPLKBC......10000
            IF (IBCIP.NE.1) THEN                                         INPLKBC......10100
               ERRCOD = "LKBC-4-2"                                       INPLKBC......10200
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                  INPLKBC......10300
            ELSE                                                         INPLKBC......10400
               ILKP = ILKP1                                              INPLKBC......10500
            END IF                                                       INPLKBC......10600
         ELSE                                                            INPLKBC......10700
            DO 250 NFB=1,NFBCS                                           INPLKBC......10800
               IF (FNAMB(NFB).EQ.BCSFNM) THEN                            INPLKBC......10900
                  ILKP(NFB) = ILKP1                                      INPLKBC......11000
                  GOTO 260                                               INPLKBC......11100
               END IF                                                    INPLKBC......11200
  250       CONTINUE                                                     INPLKBC......11300
            ERRCOD = "LKBC-4-3"                                          INPLKBC......11400
            CHERR(2) = BCSFNM                                            INPLKBC......11500
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INPLKBC......11600
  260       CONTINUE                                                     INPLKBC......11700
         END IF                                                          INPLKBC......11800
  300 CONTINUE                                                           INPLKBC......11900
C                                                                        INPLKBC......12000
C.....READ AND PROCESS SPEC-U BOUNDARY CONDITION INTERACTIONS.           INPLKBC......12100
      DO 330 IBCIU=1,NBCIU                                               INPLKBC......12200
         ERRCOD = "REA-LKBC-5"                                           INPLKBC......12300
         CALL READIF(K17, 0, INTFIL, ERRCOD)                             INPLKBC......12400
         READ(INTFIL,*,IOSTAT=INERR(1)) BCSFNM, ILKU1                    INPLKBC......12500
         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)     INPLKBC......12600
         IF ((IBCIU.EQ.1).AND.(BCSFNM.NE."DEFAULT")) THEN                INPLKBC......12700
            ERRCOD = "LKBC-5-1"                                          INPLKBC......12800
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INPLKBC......12900
         ELSE IF (BCSFNM.EQ."DEFAULT") THEN                              INPLKBC......13000
            IF (IBCIU.NE.1) THEN                                         INPLKBC......13100
               ERRCOD = "LKBC-5-2"                                       INPLKBC......13200
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                  INPLKBC......13300
            ELSE                                                         INPLKBC......13400
               ILKU = ILKU1                                              INPLKBC......13500
            END IF                                                       INPLKBC......13600
         ELSE                                                            INPLKBC......13700
            DO 310 NFB=1,NFBCS                                           INPLKBC......13800
               IF (FNAMB(NFB).EQ.BCSFNM) THEN                            INPLKBC......13900
                  ILKU(NFB) = ILKU1                                      INPLKBC......14000
                  GOTO 320                                               INPLKBC......14100
               END IF                                                    INPLKBC......14200
  310       CONTINUE                                                     INPLKBC......14300
            ERRCOD = "LKBC-5-3"                                          INPLKBC......14400
            CHERR(2) = BCSFNM                                            INPLKBC......14500
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INPLKBC......14600
  320       CONTINUE                                                     INPLKBC......14700
         END IF                                                          INPLKBC......14800
  330 CONTINUE                                                           INPLKBC......14900
C                                                                        INPLKBC......15000
C.....READ AND PROCESS GEN-FLOW BOUNDARY CONDITION INTERACTIONS.         INPLKBC......15100
      DO 400 IBCIPG=1,NBCIPG                                             INPLKBC......15200
         ERRCOD = "REA-LKBC-6A"                                          INPLKBC......15300
         CALL READIF(K17, 0, INTFIL, ERRCOD)                             INPLKBC......15400
         READ(INTFIL,*,IOSTAT=INERR(1)) BCSFNM, ILKPG1, CTIPG1           INPLKBC......15500
         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)     INPLKBC......15600
         IF (CTIPG1.EQ."F") THEN                                         INPLKBC......15700
            ITIPG1 = 1                                                   INPLKBC......15800
         ELSE IF (CTIPG1.EQ."P") THEN                                    INPLKBC......15900
            ITIPG1 = 2                                                   INPLKBC......16000
         ELSE                                                            INPLKBC......16100
            ERRCOD = "LKBC-6A-4"                                         INPLKBC......16200
            CHERR(2) = CTIPG1                                            INPLKBC......16300
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INPLKBC......16400
         END IF                                                          INPLKBC......16500
         IF ((IBCIPG.EQ.1).AND.(BCSFNM.NE."DEFAULT")) THEN               INPLKBC......16600
            ERRCOD = "LKBC-6A-1"                                         INPLKBC......16700
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INPLKBC......16800
         ELSE IF (BCSFNM.EQ."DEFAULT") THEN                              INPLKBC......16900
            IF (IBCIPG.NE.1) THEN                                        INPLKBC......17000
               ERRCOD = "LKBC-6A-2"                                      INPLKBC......17100
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                  INPLKBC......17200
            ELSE                                                         INPLKBC......17300
               ILKPG = ILKPG1                                            INPLKBC......17400
               ITIPG = ITIPG1                                            INPLKBC......17500
            END IF                                                       INPLKBC......17600
         ELSE                                                            INPLKBC......17700
            DO 350 NFB=1,NFBCS                                           INPLKBC......17800
               IF (FNAMB(NFB).EQ.BCSFNM) THEN                            INPLKBC......17900
                  ILKPG(NFB) = ILKPG1                                    INPLKBC......18000
                  ITIPG(NFB) = ITIPG1                                    INPLKBC......18100
                  GOTO 360                                               INPLKBC......18200
               END IF                                                    INPLKBC......18300
  350       CONTINUE                                                     INPLKBC......18400
            ERRCOD = "LKBC-6A-3"                                         INPLKBC......18500
            CHERR(2) = BCSFNM                                            INPLKBC......18600
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INPLKBC......18700
  360       CONTINUE                                                     INPLKBC......18800
         END IF                                                          INPLKBC......18900
  400 CONTINUE                                                           INPLKBC......19000
C                                                                        INPLKBC......19100
C.....READ AND PROCESS GEN-TRANSPORT BOUNDARY CONDITION INTERACTIONS.    INPLKBC......19200
      DO 500 IBCIUG=1,NBCIUG                                             INPLKBC......19300
         ERRCOD = "REA-LKBC-6B"                                          INPLKBC......19400
         CALL READIF(K17, 0, INTFIL, ERRCOD)                             INPLKBC......19500
         READ(INTFIL,*,IOSTAT=INERR(1)) BCSFNM, ILKUG1, CTIUG1           INPLKBC......19600
         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)     INPLKBC......19700
         IF (CTIUG1.EQ."S") THEN                                         INPLKBC......19800
            ITIUG1 = 1                                                   INPLKBC......19900
         ELSE IF (CTIUG1.EQ."U") THEN                                    INPLKBC......20000
            ITIUG1 = 2                                                   INPLKBC......20100
         ELSE                                                            INPLKBC......20200
            ERRCOD = "LKBC-6B-4"                                         INPLKBC......20300
            CHERR(2) = CTIUG1                                            INPLKBC......20400
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INPLKBC......20500
         END IF                                                          INPLKBC......20600
         IF ((IBCIUG.EQ.1).AND.(BCSFNM.NE."DEFAULT")) THEN               INPLKBC......20700
            ERRCOD = "LKBC-6B-1"                                         INPLKBC......20800
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INPLKBC......20900
         ELSE IF (BCSFNM.EQ."DEFAULT") THEN                              INPLKBC......21000
            IF (IBCIUG.NE.1) THEN                                        INPLKBC......21100
               ERRCOD = "LKBC-6B-2"                                      INPLKBC......21200
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                  INPLKBC......21300
            ELSE                                                         INPLKBC......21400
               ILKUG = ILKUG1                                            INPLKBC......21500
               ITIUG = ITIUG1                                            INPLKBC......21600
            END IF                                                       INPLKBC......21700
         ELSE                                                            INPLKBC......21800
            DO 450 NFB=1,NFBCS                                           INPLKBC......21900
               IF (FNAMB(NFB).EQ.BCSFNM) THEN                            INPLKBC......22000
                  ILKUG(NFB) = ILKUG1                                    INPLKBC......22100
                  ITIUG(NFB) = ITIUG1                                    INPLKBC......22200
                  GOTO 460                                               INPLKBC......22300
               END IF                                                    INPLKBC......22400
  450       CONTINUE                                                     INPLKBC......22500
            ERRCOD = "LKBC-6B-3"                                         INPLKBC......22600
            CHERR(2) = BCSFNM                                            INPLKBC......22700
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INPLKBC......22800
  460       CONTINUE                                                     INPLKBC......22900
         END IF                                                          INPLKBC......23000
  500 CONTINUE                                                           INPLKBC......23100
C                                                                        INPLKBC......23200
      RETURN                                                             INPLKBC......23300
      END                                                                INPLKBC......23400
C                                                                        INPLKBC......23500
C     SUBROUTINE        I  N  P  L  K  I  N        SUTRA VERSION 3.0     INPLKIN........100
C                                                                        INPLKIN........200
C *** PURPOSE :                                                          INPLKIN........300
C ***  TO READ AND PROCESS MAIN INPUT FOR LAKES AND INITIALIZE LAKES.    INPLKIN........400
C                                                                        INPLKIN........500
      SUBROUTINE INPLKIN()                                               INPLKIN........600
      USE LARR                                                           INPLKIN........700
      USE ALLARR, ONLY : IPBC,IUBC,IQSOP,IQSOU,IPBG,IUBG,                INPLKIN........800
     1   IBCPBC,IBCUBC,IBCSOP,IBCSOU,IBCPBG,IBCUBG                       INPLKIN........900
      USE EXPINT                                                         INPLKIN.......1000
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               INPLKIN.......1100
      LOGICAL LAKUP,ISDESC                                               INPLKIN.......1200
      CHARACTER*4 CTYPE                                                  INPLKIN.......1300
      CHARACTER*80 ERRCOD,CHERR(10),UNAME,FNAME(0:23)                    INPLKIN.......1400
      CHARACTER INTFIL*1000                                              INPLKIN.......1500
      DIMENSION INERR(10),RLERR(10)                                      INPLKIN.......1600
      DIMENSION ISPEC(NLAKES)                                            INPLKIN.......1700
      DIMENSION KTYPE(2)                                                 INPLKIN.......1800
      INTEGER RMVDIM,IMVDIM,I1VDIM,CMVDIM,LMVDIM,PMVDIM                  INPLKIN.......1900
      COMMON /BUDO/ TSECO,LUTSO                                          INPLKIN.......2000
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  INPLKIN.......2100
     1   NPCYC,NUCYC,NPRINT,NBCFPR,NBCSPR,NBCPPR,NBCUPR,NLAKPR,IREAD,    INPLKIN.......2200
     2   NBGPPR,NBGUPR,ISTORE,NOUMAT,IUNSAT,IALSAT,KTYPE                 INPLKIN.......2300
      COMMON /DIMEN/ NLAKES, NSIL, NCEL, NPLK                            INPLKIN.......2400
      COMMON /DIMSRF/ MAXCON, NNSURF, NNVERT                             INPLKIN.......2500
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8,K9,                 INPLKIN.......2600
     1   K10,K11,K12,K13,K14,K15,K16,K17,K18,K19,K20,K21,K22,K23         INPLKIN.......2700
      COMMON /GRAVEC/ GRAVX,GRAVY,GRAVZ                                  INPLKIN.......2800
      COMMON /LUP/ LAKUP                                                 INPLKIN.......2900
      COMMON /MEMCNT/ RMVDIM,IMVDIM,I1VDIM,CMVDIM,LMVDIM,PMVDIM          INPLKIN.......3000
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      INPLKIN.......3100
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        INPLKIN.......3200
      COMMON /RHOLAK/ RHOLK                                              INPLKIN.......3300
      COMMON /RISSAV/ FRROD,FDROD                                        INPLKIN.......3400
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           INPLKIN.......3500
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       INPLKIN.......3600
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITBCS,ITRST,ITMAX,TSTART      INPLKIN.......3700
      COMMON /VALLAK/ RNOLK                                              INPLKIN.......3800
      EXTERNAL ISDESC                                                    INPLKIN.......3900
C                                                                        INPLKIN.......4000
C.....READ AND PROCESS MAIN INPUT FOR LAKES AND INITIALIZE LAKES.        INPLKIN.......4100
C                                                                        INPLKIN.......4200
      ERRCOD = "REA-LKIN-1"                                              INPLKIN.......4300
      CALL READIF(K16, 0, INTFIL, ERRCOD)                                INPLKIN.......4400
      READ(INTFIL,*,IOSTAT=INERR(1)) NLAKPR                              INPLKIN.......4500
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INPLKIN.......4600
      ERRCOD = "REA-LKIN-2"                                              INPLKIN.......4700
      CALL READIF(K16, 0, INTFIL, ERRCOD)                                INPLKIN.......4800
      READ(INTFIL,*,IOSTAT=INERR(1)) NLSPEC, FRROD, FDROD, RNOLK         INPLKIN.......4900
      IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)        INPLKIN.......5000
      ALLOCATE(FRRO(NLAKES),FDRO(NLAKES))                                INPLKIN.......5100
      RMVDIM = RMVDIM + 2*NLAKES                                         INPLKIN.......5200
C                                                                        INPLKIN.......5300
      IF (IREAD.EQ.+1) THEN                                              INPLKIN.......5400
C.....COLD START.                                                        INPLKIN.......5500
C                                                                        INPLKIN.......5600
      FRRO = FRROD                                                       INPLKIN.......5700
      FDRO = FDROD                                                       INPLKIN.......5800
      VOLW = 0D0                                                         INPLKIN.......5900
      UW = 0D0                                                           INPLKIN.......6000
      DO 50 LK=1,NLAKES                                                  INPLKIN.......6100
         IF (ISTAT(LK).NE.-4) THEN                                       INPLKIN.......6200
            ISTAT(LK) = -999                                             INPLKIN.......6300
            VOLW(LK) = -HUGE(1D0)                                        INPLKIN.......6400
          END IF                                                         INPLKIN.......6500
   50 CONTINUE                                                           INPLKIN.......6600
C                                                                        INPLKIN.......6700
      IF (NLSPEC.EQ.0) GOTO 900                                          INPLKIN.......6800
C                                                                        INPLKIN.......6900
C.....LOOP THROUGH AND APPLY INITIAL SPECIFICATIONS                      INPLKIN.......7000
      ISPEC = 0                                                          INPLKIN.......7100
      DO 800 IST=1,NLSPEC                                                INPLKIN.......7200
C........READ SPECIFICATION, FIND ISURF AND LAKE NUMBER, AND DO ERROR    INPLKIN.......7300
C           CHECKING                                                     INPLKIN.......7400
         ERRCOD = "REA-LKIN-3"                                           INPLKIN.......7500
         CALL READIF(K16, 0, INTFIL, ERRCOD)                             INPLKIN.......7600
         READ(INTFIL,*,IOSTAT=INERR(1)) CTYPE, ILON, STGI, UWI,          INPLKIN.......7700
     1     FRROIN, FDROIN                                                INPLKIN.......7800
         IF (INERR(1).NE.0) CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)     INPLKIN.......7900
         IF (CTYPE.EQ.'LAKE') THEN                                       INPLKIN.......8000
            LK = ILON                                                    INPLKIN.......8100
            ILON = -ILON                                                 INPLKIN.......8200
         ELSE IF (CTYPE.EQ.'NODE') THEN                                  INPLKIN.......8300
            ISURF = ISURFACE(ILON)                                       INPLKIN.......8400
            IF (ISURF.EQ.0) THEN                                         INPLKIN.......8500
               ERRCOD = "LKIN-3-1"                                       INPLKIN.......8600
               INERR(1) = ILON                                           INPLKIN.......8700
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                  INPLKIN.......8800
            ELSE IF (.NOT.LAKNOD(ISURF)%LAKEOK) THEN                     INPLKIN.......8900
               ERRCOD = "LKIN-3-2"                                       INPLKIN.......9000
               INERR(1) = ILON                                           INPLKIN.......9100
               CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                  INPLKIN.......9200
            END IF                                                       INPLKIN.......9300
            LK = LAKNOD(ISURF)%KLUSTR                                    INPLKIN.......9400
         ELSE                                                            INPLKIN.......9500
            ERRCOD = "LKIN-3-3"                                          INPLKIN.......9600
            CHERR(1) = CTYPE                                             INPLKIN.......9700
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INPLKIN.......9800
         END IF                                                          INPLKIN.......9900
         FRRO(LK) = FRROIN                                               INPLKIN......10000
         FDRO(LK) = FDROIN                                               INPLKIN......10100
C........NOTE: MULTIPLYING BY CW CONVERTS TEMPERATURE TO SPECIFIC ENERGY INPLKIN......10200
C           CONTENT IN THE CASE OF ENERGY TRANSPORT.  NO EFFECT          INPLKIN......10300
C           IN THE CASE OF SOLUTE TRANSPORT, SINCE CW=1.                 INPLKIN......10400
         UWI = UWI*CW                                                    INPLKIN......10500
C........IF SPECIFICATION IN DISQUALIFIED LAKE, ERROR.                   INPLKIN......10600
         IF (ISTAT(LK).EQ.-4) THEN                                       INPLKIN......10700
            ERRCOD = "LKIN-3-4"                                          INPLKIN......10800
            INERR(1) = ILON                                              INPLKIN......10900
            CALL SUTERR(ERRCOD, CHERR, INERR, RLERR)                     INPLKIN......11000
         END IF                                                          INPLKIN......11100
C........APPLY INITIAL STAGE SPECIFICATION TO RELEVANT LAKE(S).          INPLKIN......11200
         DO 750 LKK=LK,NLAKES                                            INPLKIN......11300
            IF (ISPEC(LKK).EQ.IST) THEN                                  INPLKIN......11400
C..............LAKE LKK HAS ALREADY BEEN PROCESSED UNDER THIS SPEC.      INPLKIN......11500
               CYCLE                                                     INPLKIN......11600
            ELSE IF ((LKK.EQ.LK).OR.(ISDESC(LKK,LK))) THEN               INPLKIN......11700
C..............LAKE LKK IS LAKE LK OR A DESCENDANT.                      INPLKIN......11800
               VOLII = VOLLAK(LKK,STGI)                                  INPLKIN......11900
C..............(NOTE: VOLII ABOVE COULD BE AN EXTRAPOLATION ABOVE        INPLKIN......12000
C                 OR BELOW THE MAX AND MIN VOLUMES FOR LAKE LKK.)        INPLKIN......12100
               IF (VOLII.LE.VOLW(LKK)) THEN                              INPLKIN......12200
C.................LAKE LKK IS ALREADY AT THAT STAGE OR HIGHER,           INPLKIN......12300
C                    SO SKIP IT. THIS INCLUDES THE CASE OF A DRY         INPLKIN......12400
C                    LAKE (VOLW=0.) THAT REMAINS DRY.                    INPLKIN......12500
                  CYCLE                                                  INPLKIN......12600
               ELSE IF (VOLII.GT.VMAX(LKK)) THEN                         INPLKIN......12700
C.................LAKE LKK IS OVERFILLED BY INITIAL STAGE.               INPLKIN......12800
                  CALL LAKOVIN(LKK,STGI,UWI,IST,ISPEC)                   INPLKIN......12900
               ELSE                                                      INPLKIN......13000
C.................LAKE LKK IS NOT OVERFILLED BY INITIAL STAGE.           INPLKIN......13100
                  LLKC = LCHD(LKK,1)                                     INPLKIN......13200
                  IF (VOLII.EQ.VMAX(LKK)) THEN                           INPLKIN......13300
C....................LAKE IS FILLED TO CAPACITY.                         INPLKIN......13400
                     ISTAT(LKK) = 2                                      INPLKIN......13500
                     VOLW(LKK) = VOLII                                   INPLKIN......13600
                     UW(LKK) = UWI                                       INPLKIN......13700
                     ISPEC(LKK) = IST                                    INPLKIN......13800
                  ELSE IF (LLKC.EQ.0) THEN                               INPLKIN......13900
C....................LAKE IS CHILDLESS AND SO PARTIALLY FILLED.          INPLKIN......14000
                     ISTAT(LKK) = 1                                      INPLKIN......14100
                     VOLW(LKK) = VOLII                                   INPLKIN......14200
                     UW(LKK) = UWI                                       INPLKIN......14300
                     ISPEC(LKK) = IST                                    INPLKIN......14400
                  ELSE                                                   INPLKIN......14500
C....................LAKE IS NOT CHILDLESS AND SO COULD BE EITHER        INPLKIN......14600
C                       PARTIALLY FILLED OR SPLIT.                       INPLKIN......14700
                     LLKC = LCHD(LKK,1)                                  INPLKIN......14800
                     VOLIIC = VOLLAK(LLKC,STGI)                          INPLKIN......14900
                     IF (VOLIIC.LE.VMAX(LLKC)) THEN                      INPLKIN......15000
C.......................LAKE IS SPLIT.                                   INPLKIN......15100
                        ISTAT(LKK) = -1                                  INPLKIN......15200
                        ISPEC(LKK) = IST                                 INPLKIN......15300
                     ELSE                                                INPLKIN......15400
C.......................LAKE IS PARTIALLY FILLED.                        INPLKIN......15500
                        ISTAT(LKK) = 1                                   INPLKIN......15600
                        VOLW(LKK) = VOLII                                INPLKIN......15700
                        UW(LKK) = UWI                                    INPLKIN......15800
                        ISPEC(LKK) = IST                                 INPLKIN......15900
                     END IF                                              INPLKIN......16000
                  END IF                                                 INPLKIN......16100
               END IF                                                    INPLKIN......16200
            END IF                                                       INPLKIN......16300
  750    CONTINUE                                                        INPLKIN......16400
  800 CONTINUE                                                           INPLKIN......16500
C                                                                        INPLKIN......16600
C.....SET DEFAULTS FOR LAKES NOT ALREADY SET BY SPECIFICATIONS.          INPLKIN......16700
  900 DO 1000 LK=1,NLAKES                                                INPLKIN......16800
         IF (ISTAT(LK).EQ.-999) THEN                                     INPLKIN......16900
            IF (LCHD(LK,1).EQ.0) THEN                                    INPLKIN......17000
               VOLW(LK) = 0D0                                            INPLKIN......17100
               UW(LK) = 0D0                                              INPLKIN......17200
               ISTAT(LK) = 0                                             INPLKIN......17300
            ELSE                                                         INPLKIN......17400
               VOLW(LK) = 0D0                                            INPLKIN......17500
               UW(LK) = 0D0                                              INPLKIN......17600
               ISTAT(LK) = -1                                            INPLKIN......17700
            END IF                                                       INPLKIN......17800
         END IF                                                          INPLKIN......17900
 1000 CONTINUE                                                           INPLKIN......18000
C                                                                        INPLKIN......18100
C.....IF ANY LAKE AND ITS SIBLING ARE BOTH FULL, COALESCE THEM.          INPLKIN......18200
      DO 1050 LK=2,NLAKES                                                INPLKIN......18300
         LKSIB = LSIB(LK)                                                INPLKIN......18400
         IF ((ISTAT(LK).EQ.2).AND.(ISTAT(LKSIB).EQ.2)) THEN              INPLKIN......18500
            LKPAR = LPAR(LK)                                             INPLKIN......18600
            VOLW(LK) = VFUL(LK)                                          INPLKIN......18700
            VOLW(LKSIB) = VFUL(LKSIB)                                    INPLKIN......18800
            VOLW(LKPAR) = VOLW(LK) + VOLW(LKSIB)                         INPLKIN......18900
C...........IN MIXING WATERS, ASSUME CONSTANT, UNIFORM DENSITY IN LAKES. INPLKIN......19000
            UW(LKPAR) = (UW(LK)*VOLW(LK) +                               INPLKIN......19100
     1         UW(LKSIB)*VOLW(LKSIB))/VOLW(LKPAR)                        INPLKIN......19200
            UW(LK) = UW(LKPAR)                                           INPLKIN......19300
            UW(LKSIB) = UW(LKPAR)                                        INPLKIN......19400
            ISTAT(LK) = -3                                               INPLKIN......19500
            ISTAT(LKSIB) = -3                                            INPLKIN......19600
            ISTAT(LKPAR) = 1                                             INPLKIN......19700
         END IF                                                          INPLKIN......19800
 1050 CONTINUE                                                           INPLKIN......19900
C                                                                        INPLKIN......20000
C.....INITIALIZE UWMS.                                                   INPLKIN......20100
      UWMS = RHOLK*VOLW*UW                                               INPLKIN......20200
C                                                                        INPLKIN......20300
C.....PROPAGATE WATER VOLUME AND SOLUTE MASS OR ENERGY CUMULATIVELY      INPLKIN......20400
C        UP THE TREE.                                                    INPLKIN......20500
      DO 1100 LK=NLAKES,1,-1                                             INPLKIN......20600
         IF (ISTAT(LK).EQ.-1) THEN                                       INPLKIN......20700
            LKC1 = LCHD(LK,1)                                            INPLKIN......20800
            LKC2 = LCHD(LK,2)                                            INPLKIN......20900
            VOLW(LK) = VOLW(LKC1) + VOLW(LKC2)                           INPLKIN......21000
            UWMS(LK) = UWMS(LKC1) + UWMS(LKC2)                           INPLKIN......21100
         END IF                                                          INPLKIN......21200
 1100 CONTINUE                                                           INPLKIN......21300
C                                                                        INPLKIN......21400
C.....INITIALIZE ULK TO INITIAL LAKE CONC/TEMP.                          INPLKIN......21500
      ULK = 0D0                                                          INPLKIN......21600
      DO 1200 ISURF=1,NNSURF                                             INPLKIN......21700
         IF (.NOT.LAKNOD(ISURF)%LAKEOK) CYCLE                            INPLKIN......21800
         ILN = LAKNOD(ISURF)%INODE                                       INPLKIN......21900
         LKK = LAKNOD(ISURF)%KLUSTR                                      INPLKIN......22000
C........FIND ACTIVE ANCESTOR, IF ANY.                                   INPLKIN......22100
         LK = INCA(ISTAT,LKK)                                            INPLKIN......22200
         IF ((ISTAT(LK).NE.0).AND.(ISTAT(LK).NE.-1)) THEN                INPLKIN......22300
            STG = STGLAK(LK)                                             INPLKIN......22400
            ELV = LAKNOD(ISURF)%ELEV                                     INPLKIN......22500
            IF (STG.GE.ELV) THEN                                         INPLKIN......22600
C..............NOTE: DIVIDING BY CW CONVERTS SPECIFIC ENERGY CONTENT     INPLKIN......22700
C                 TO TEMPERATURE IN THE CASE OF ENERGY TRANSPORT.        INPLKIN......22800
C                 NO EFFECT IN THE CASE OF SOLUTE TRANSPORT,             INPLKIN......22900
C                 SINCE CW=1.                                            INPLKIN......23000
               ULK(ISURF) = UW(LK)/CW                                    INPLKIN......23100
            END IF                                                       INPLKIN......23200
         END IF                                                          INPLKIN......23300
 1200 CONTINUE                                                           INPLKIN......23400
C                                                                        INPLKIN......23500
C.....INITIALIZE LAKE PRESSURES AND INTERACTION EFFECTS.                 INPLKIN......23600
      CALL GETPLK()                                                      INPLKIN......23700
      CALL LAKAPP(IPBC,IUBC,IQSOP,IQSOU,IPBG,IUBG,                       INPLKIN......23800
     1   IBCSF,IBCSS,IBCSP,IBCSU,IBCSPG,IBCSUG)                          INPLKIN......23900
C                                                                        INPLKIN......24000
C.....INITIALIZE BUDGET INFORMATION.                                     INPLKIN......24100
      DO 1250 LK=1,NLAKES                                                INPLKIN......24200
         STGB(LK) = STGLAK(LK)                                           INPLKIN......24300
1250  CONTINUE                                                           INPLKIN......24400
      VOLWO = VOLW                                                       INPLKIN......24500
      STGBO = STGB                                                       INPLKIN......24600
      UWMSO = UWMS                                                       INPLKIN......24700
      UWO = UW                                                           INPLKIN......24800
      ISTATO = ISTAT                                                     INPLKIN......24900
      TSECO = TSEC                                                       INPLKIN......25000
      LUTSO = IT                                                         INPLKIN......25100
C                                                                        INPLKIN......25200
      ELSE                                                               INPLKIN......25300
C.....WARM START.                                                        INPLKIN......25400
C                                                                        INPLKIN......25500
      DO 1300 IST=1,NLAKES                                               INPLKIN......25600
         READ(K16,*) CTYPE, LK, STGI, UWI, FRROIN, FDROIN                INPLKIN......25700
         FRRO(LK) = FRROIN                                               INPLKIN......25800
         FDRO(LK) = FDROIN                                               INPLKIN......25900
 1300 CONTINUE                                                           INPLKIN......26000
C                                                                        INPLKIN......26100
      READ(K16,*) LUTSO,TSECO,LAKUP                                      INPLKIN......26200
      DO 850 LK=1,NLAKES                                                 INPLKIN......26300
         READ(K16,*) ISTATO(LK),VOLWO(LK),STGBO(LK),UWO(LK),UWMSO(LK)    INPLKIN......26400
         READ(K16,*) ISTAT(LK),VOLW(LK),STGB(LK),UW(LK),UWMS(LK)         INPLKIN......26500
         READ(K16,*) FGWG(LK),FEXG(LK),                                  INPLKIN......26600
     1      FLKG(LK),FROG(LK)                                            INPLKIN......26700
         READ(K16,*) FGWL(LK),FEXL(LK),                                  INPLKIN......26800
     1      FLKL(LK),FLLL(LK)                                            INPLKIN......26900
         READ(K16,*) GGWG(LK),GEXG(LK),                                  INPLKIN......27000
     1      GLKG(LK),GROG(LK)                                            INPLKIN......27100
         READ(K16,*) GGWL(LK),GEXL(LK),                                  INPLKIN......27200
     1      GLKL(LK),GLLL(LK)                                            INPLKIN......27300
  850 CONTINUE                                                           INPLKIN......27400
      ISLAKE = .FALSE.                                                   INPLKIN......27500
      DO 890 ISURF=1,NNSURF                                              INPLKIN......27600
         ILN = LAKNOD(ISURF)%INODE                                       INPLKIN......27700
         READ(K16,*) ISLAKE(ILN),PLK(ISURF),ULK(ISURF)                   INPLKIN......27800
  890 CONTINUE                                                           INPLKIN......27900
C                                                                        INPLKIN......28000
      END IF                                                             INPLKIN......28100
C                                                                        INPLKIN......28200
C.....INITIALIZE OVERDRAFTS AND LAKUP FLAG.                              INPLKIN......28300
      VOVER = 0D0                                                        INPLKIN......28400
      WMOVER = 0D0                                                       INPLKIN......28500
      SMOVER = 0D0                                                       INPLKIN......28600
      LAKUP = .TRUE.                                                     INPLKIN......28700
C                                                                        INPLKIN......28800
      RETURN                                                             INPLKIN......28900
      END                                                                INPLKIN......29000
C                                                                        INPLKIN......29100
C     FUNCTION          I  S  D  E  S  C           SUTRA VERSION 3.0     ISDESC.........100
C                                                                        ISDESC.........200
C *** PURPOSE :                                                          ISDESC.........300
C ***  TO RETURN WHETHER A GIVEN LAKE IS A DESCENDANT OF ANOTHER         ISDESC.........400
C ***  GIVEN LAKE.                                                       ISDESC.........500
C                                                                        ISDESC.........600
      LOGICAL FUNCTION ISDESC(LK1, LK2)                                  ISDESC.........700
      USE LARR                                                           ISDESC.........800
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               ISDESC.........900
C                                                                        ISDESC........1000
      IF (LK1.LE.LK2) THEN                                               ISDESC........1100
C........ACCORDING TO THE NUMBERING CONVENTION, A LAKE'S DESCENDANT      ISDESC........1200
C           MUST HAVE A HIGHER LAKE NUMBER.                              ISDESC........1300
         ISDESC = .FALSE.                                                ISDESC........1400
         GOTO 999                                                        ISDESC........1500
      ELSE IF (LK2.EQ.1) THEN                                            ISDESC........1600
C........ALL LAKES (EXCEPT LAKE 1 ITSELF) ARE DESCENDANTS OF LAKE 1.     ISDESC........1700
         ISDESC = .TRUE.                                                 ISDESC........1800
         GOTO 999                                                        ISDESC........1900
      ELSE                                                               ISDESC........2000
C........SEARCH UP THE TREE.                                             ISDESC........2100
         ISDESC = .FALSE.                                                ISDESC........2200
         LKPAR = LPAR(LK1)                                               ISDESC........2300
         DO WHILE (LKPAR.GE.LK2)                                         ISDESC........2400
            IF (LKPAR.EQ.LK2) THEN                                       ISDESC........2500
               ISDESC = .TRUE.                                           ISDESC........2600
               GOTO 999                                                  ISDESC........2700
            END IF                                                       ISDESC........2800
            LKPAR = LPAR(LKPAR)                                          ISDESC........2900
         END DO                                                          ISDESC........3000
      END IF                                                             ISDESC........3100
C                                                                        ISDESC........3200
  999 RETURN                                                             ISDESC........3300
      END                                                                ISDESC........3400
C                                                                        ISDESC........3500
C     FUNCTION          I  S  S  U  B  M           SUTRA VERSION 3.0     ISSUBM.........100
C                                                                        ISSUBM.........200
C *** PURPOSE :                                                          ISSUBM.........300
C ***  TO RETURN WHETHER A GIVEN SURFACE NODE IS SUBMERGED BY A GIVEN    ISSUBM.........400
C ***  LAKE STAGE.                                                       ISSUBM.........500
C                                                                        ISSUBM.........600
      LOGICAL FUNCTION ISSUBM(ISURF,STAGE)                               ISSUBM.........700
      USE LARR                                                           ISSUBM.........800
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               ISSUBM.........900
C                                                                        ISSUBM........1000
      IF (LAKNOD(ISURF)%ELEV.LE.STAGE) THEN                              ISSUBM........1100
         ISSUBM = .TRUE.                                                 ISSUBM........1200
      ELSE                                                               ISSUBM........1300
         ISSUBM = .FALSE.                                                ISSUBM........1400
      END IF                                                             ISSUBM........1500
C                                                                        ISSUBM........1600
      RETURN                                                             ISSUBM........1700
      END                                                                ISSUBM........1800
C                                                                        ISSUBM........1900
C     SUBROUTINE        L  A  K  A  P  P           SUTRA VERSION 3.0     LAKAPP.........100
C                                                                        LAKAPP.........200
C *** PURPOSE :                                                          LAKAPP.........300
C ***  TO DETERMINE APPLICATION OF BOUNDARY CONDITIONS, AS               LAKAPP.........400
C ***  DETERMINED BY USER-SPECIFIED LAKE INTERACTIONS.                   LAKAPP.........500
C                                                                        LAKAPP.........600
      SUBROUTINE LAKAPP(IPBC,IUBC,IQSOP,IQSOU,IPBG,IUBG,                 LAKAPP.........700
     1   IBCSF,IBCSS,IBCSP,IBCSU,IBCSPG,IBCSUG)                          LAKAPP.........800
      USE ALLARR, ONLY : CIDBCS                                          LAKAPP.........900
      USE BCSDEF                                                         LAKAPP........1000
      USE EXPINT                                                         LAKAPP........1100
      USE LLDEF                                                          LAKAPP........1200
      USE SCHDEF                                                         LAKAPP........1300
      USE LARR, ONLY : ISLAKE,ILKF,ILKS,ILKP,ILKU,ILKPG,ITIPG,ILKUG,     LAKAPP........1400
     1   ITIUG,LAKNOD,LKBCPBC,LKBCUBC,LKBCSOP,LKBCSOU,LKBCPBG,LKBCUBG,   LAKAPP........1500
     2   ISURFACE                                                        LAKAPP........1600
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                LAKAPP........1700
      INTEGER IBCSP(NBCN),IBCSU(NBCN),IBCSF(NSOP),IBCSS(NSOU),           LAKAPP........1800
     1   IBCSPG(NPBG),IBCSUG(NUBG)                                       LAKAPP........1900
      DIMENSION IPBC(NBCN),IUBC(NBCN),IQSOP(NSOP),IQSOU(NSOU),           LAKAPP........2000
     1   IPBG(NPBG),IUBG(NUBG)                                           LAKAPP........2100
      DIMENSION KTYPE(2)                                                 LAKAPP........2200
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  LAKAPP........2300
     1   NPCYC,NUCYC,NPRINT,NBCFPR,NBCSPR,NBCPPR,NBCUPR,NLAKPR,IREAD,    LAKAPP........2400
     1   NBGPPR,NBGUPR,ISTORE,NOUMAT,IUNSAT,IALSAT,KTYPE                 LAKAPP........2500
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              LAKAPP........2600
     1   NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                                  LAKAPP........2700
      COMMON /DIMSRF/ MAXCON, NNSURF, NNVERT                             LAKAPP........2800
      COMMON /FUNIB/ NFBCS                                               LAKAPP........2900
      COMMON /LAKU/ LAKUSD                                               LAKAPP........3000
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       LAKAPP........3100
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITBCS,ITRST,ITMAX,TSTART      LAKAPP........3200
C                                                                        LAKAPP........3300
C                                                                        LAKAPP........3400
C.....NSOPI IS ACTUAL NUMBER OF FLUID SOURCE NODES                       LAKAPP........3500
      NSOPI=NSOP-1                                                       LAKAPP........3600
C.....NSOUI IS ACTUAL NUMBER OF ENERGY OR SOLUTE MASS SOURCE NODES       LAKAPP........3700
      NSOUI=NSOU-1                                                       LAKAPP........3800
C                                                                        LAKAPP........3900
C.....FLUID SOURCES/SINKS, OR CONCENTRATIONS (TEMPERATURES) OF           LAKAPP........4000
C        SOURCE FLUID                                                    LAKAPP........4100
      IF (NSOPI.GT.0) THEN                                               LAKAPP........4200
C.....APPLY CONDITION UNLESS LAKE INTERACTION CALLS FOR NOT              LAKAPP........4300
C       NOT APPLYING, AS DETERMINED BELOW.                               LAKAPP........4400
      LKBCSOP = .TRUE.                                                   LAKAPP........4500
      DO 200 IQP=1,NSOPI                                                 LAKAPP........4600
         I=IQSOP(IQP)                                                    LAKAPP........4700
C........IF NOT ELIGIBLE TO BE A LAKE NODE, SKIP.                        LAKAPP........4800
         ISURF = ISURFACE(I)                                             LAKAPP........4900
         IF (ISURF.EQ.0) CYCLE                                           LAKAPP........5000
         IF (.NOT.LAKNOD(ISURF)%LAKEOK) CYCLE                            LAKAPP........5100
C........INTERACTION.                                                    LAKAPP........5200
         NFB = IBCSF(IQP)                                                LAKAPP........5300
         IF (ISLAKE(IABS(I))) THEN                                       LAKAPP........5400
C...........LAKE IS PRESENT, SO DO NOT APPLY CONDITION IF                LAKAPP........5500
C              INTERACTION FLAG = -1                                     LAKAPP........5600
            IF (ILKF(NFB).EQ.-1) LKBCSOP(IQP) = .FALSE.                  LAKAPP........5700
         ELSE                                                            LAKAPP........5800
C...........LAKE IS ABSENT, SO DO NOT APPLY CONDITION IF                 LAKAPP........5900
C              INTERACTION FLAG = 1                                      LAKAPP........6000
            IF (ILKF(NFB).EQ.1) LKBCSOP(IQP) = .FALSE.                   LAKAPP........6100
         END IF                                                          LAKAPP........6200
  200 CONTINUE                                                           LAKAPP........6300
      END IF                                                             LAKAPP........6400
C                                                                        LAKAPP........6500
C.....SOURCES/SINKS OF SOLUTE MASS OR ENERGY                             LAKAPP........6600
      IF (NSOUI.GT.0) THEN                                               LAKAPP........6700
C.....APPLY CONDITION UNLESS LAKE INTERACTION CALLS FOR NOT              LAKAPP........6800
C       NOT APPLYING, AS DETERMINED BELOW.                               LAKAPP........6900
      LKBCSOU = .TRUE.                                                   LAKAPP........7000
      DO 400 IQU=1,NSOUI                                                 LAKAPP........7100
         I=IQSOU(IQU)                                                    LAKAPP........7200
C........IF NOT ELIGIBLE TO BE A LAKE NODE, SKIP.                        LAKAPP........7300
         ISURF = ISURFACE(I)                                             LAKAPP........7400
         IF (ISURF.EQ.0) CYCLE                                           LAKAPP........7500
         IF (.NOT.LAKNOD(ISURF)%LAKEOK) CYCLE                            LAKAPP........7600
C........INTERACTION.                                                    LAKAPP........7700
         NFB = IBCSS(IQU)                                                LAKAPP........7800
         IF (ISLAKE(IABS(I))) THEN                                       LAKAPP........7900
C...........LAKE IS PRESENT, SO DO NOT APPLY CONDITION IF                LAKAPP........8000
C              INTERACTION FLAG = -1                                     LAKAPP........8100
            IF (ILKS(NFB).EQ.-1) LKBCSOU(IQU) = .FALSE.                  LAKAPP........8200
         ELSE                                                            LAKAPP........8300
C...........LAKE IS ABSENT, SO DO NOT APPLY CONDITION IF                 LAKAPP........8400
C              INTERACTION FLAG = 1                                      LAKAPP........8500
            IF (ILKS(NFB).EQ.1) LKBCSOU(IQU) = .FALSE.                   LAKAPP........8600
         END IF                                                          LAKAPP........8700
  400 CONTINUE                                                           LAKAPP........8800
         END IF                                                          LAKAPP........8900
C                                                                        LAKAPP........9000
C.....SPECIFIED PRESSURES OR CONCENTRATIONS (TEMPERATURES) OF INFLOWS    LAKAPP........9100
C        AT SPECIFIED-PRESSURE NODES                                     LAKAPP........9200
      IF (NPBC.GT.0) THEN                                                LAKAPP........9300
C.....APPLY CONDITION UNLESS LAKE INTERACTION CALLS FOR NOT              LAKAPP........9400
C       NOT APPLYING, AS DETERMINED BELOW.                               LAKAPP........9500
      LKBCPBC = .TRUE.                                                   LAKAPP........9600
      DO 600 IP=1,NPBC                                                   LAKAPP........9700
         I = IPBC(IP)                                                    LAKAPP........9800
C........IF NOT ELIGIBLE TO BE A LAKE NODE, SKIP.                        LAKAPP........9900
         ISURF = ISURFACE(I)                                             LAKAPP.......10000
         IF (ISURF.EQ.0) CYCLE                                           LAKAPP.......10100
         IF (.NOT.LAKNOD(ISURF)%LAKEOK) CYCLE                            LAKAPP.......10200
C........INTERACTION.                                                    LAKAPP.......10300
         NFB = IBCSP(IP)                                                 LAKAPP.......10400
         IF (ISLAKE(IABS(I))) THEN                                       LAKAPP.......10500
            IF (ILKP(NFB).EQ.-1) LKBCPBC(IP) = .FALSE.                   LAKAPP.......10600
C...........LAKE IS PRESENT, SO DO NOT APPLY CONDITION IF                LAKAPP.......10700
C              INTERACTION FLAG = -1                                     LAKAPP.......10800
         ELSE                                                            LAKAPP.......10900
C...........LAKE IS ABSENT, SO DO NOT APPLY CONDITION IF                 LAKAPP.......11000
C              INTERACTION FLAG = 1                                      LAKAPP.......11100
            IF (ILKP(NFB).EQ.1) LKBCPBC(IP) = .FALSE.                    LAKAPP.......11200
         END IF                                                          LAKAPP.......11300
  600 CONTINUE                                                           LAKAPP.......11400
      END IF                                                             LAKAPP.......11500
C                                                                        LAKAPP.......11600
C.....SPECIFIED CONCENTRATIONS (TEMPERATURES)                            LAKAPP.......11700
      IF (NUBC.GT.0) THEN                                                LAKAPP.......11800
C.....APPLY CONDITION UNLESS LAKE INTERACTION CALLS FOR NOT              LAKAPP.......11900
C       NOT APPLYING, AS DETERMINED BELOW.                               LAKAPP.......12000
      LKBCUBC = .TRUE.                                                   LAKAPP.......12100
      DO 800 IU=1,NUBC                                                   LAKAPP.......12200
         IUP = IU + NPBC                                                 LAKAPP.......12300
         I=IUBC(IUP)                                                     LAKAPP.......12400
C........IF NOT ELIGIBLE TO BE A LAKE NODE, SKIP.                        LAKAPP.......12500
         ISURF = ISURFACE(I)                                             LAKAPP.......12600
         IF (ISURF.EQ.0) CYCLE                                           LAKAPP.......12700
         IF (.NOT.LAKNOD(ISURF)%LAKEOK) CYCLE                            LAKAPP.......12800
C........INTERACTION.                                                    LAKAPP.......12900
         NFB = IBCSU(IUP)                                                LAKAPP.......13000
         IF (ISLAKE(IABS(I))) THEN                                       LAKAPP.......13100
            IF (ILKU(NFB).EQ.-1) LKBCUBC(IUP) = .FALSE.                  LAKAPP.......13200
C...........LAKE IS PRESENT, SO DO NOT APPLY CONDITION IF                LAKAPP.......13300
C              INTERACTION FLAG = -1                                     LAKAPP.......13400
         ELSE                                                            LAKAPP.......13500
C...........LAKE IS ABSENT, SO DO NOT APPLY CONDITION IF                 LAKAPP.......13600
C              INTERACTION FLAG = 1                                      LAKAPP.......13700
            IF (ILKU(NFB).EQ.1) LKBCUBC(IUP) = .FALSE.                   LAKAPP.......13800
         END IF                                                          LAKAPP.......13900
  800 CONTINUE                                                           LAKAPP.......14000
         END IF                                                          LAKAPP.......14100
C                                                                        LAKAPP.......14200
C.....GENERALIZED FLOWS OR CONCENTRATIONS (TEMPERATURES) OF INFLOWS      LAKAPP.......14300
C        AT GENERALIZED-FLOW NODES                                       LAKAPP.......14400
      IF (NPBG.GT.0) THEN                                                LAKAPP.......14500
C.....APPLY CONDITION UNLESS LAKE INTERACTION CALLS FOR NOT              LAKAPP.......14600
C       NOT APPLYING, AS DETERMINED BELOW.                               LAKAPP.......14700
      LKBCPBG = .TRUE.                                                   LAKAPP.......14800
      DO 850 IPG=1,NPBG                                                  LAKAPP.......14900
         I = IPBG(IPG)                                                   LAKAPP.......15000
C........IF NOT ELIGIBLE TO BE A LAKE NODE, SKIP.                        LAKAPP.......15100
         ISURF = ISURFACE(I)                                             LAKAPP.......15200
         IF (ISURF.EQ.0) CYCLE                                           LAKAPP.......15300
         IF (.NOT.LAKNOD(ISURF)%LAKEOK) CYCLE                            LAKAPP.......15400
C........INTERACTION.                                                    LAKAPP.......15500
         NFB = IBCSPG(IPG)                                               LAKAPP.......15600
         IF (ISLAKE(IABS(I))) THEN                                       LAKAPP.......15700
            IF (ILKPG(NFB).EQ.-1) LKBCPBG(IPG) = .FALSE.                 LAKAPP.......15800
C...........LAKE IS PRESENT, SO DO NOT APPLY CONDITION IF                LAKAPP.......15900
C              INTERACTION FLAG = -1                                     LAKAPP.......16000
         ELSE                                                            LAKAPP.......16100
C...........LAKE IS ABSENT, SO DO NOT APPLY CONDITION IF                 LAKAPP.......16200
C              INTERACTION FLAG = 1                                      LAKAPP.......16300
            IF (ILKPG(NFB).EQ.1) LKBCPBG(IPG) = .FALSE.                  LAKAPP.......16400
         END IF                                                          LAKAPP.......16500
  850 CONTINUE                                                           LAKAPP.......16600
      END IF                                                             LAKAPP.......16700
C                                                                        LAKAPP.......16800
C.....GENERALIZED-TRANSPORT CONDITIONS                                   LAKAPP.......16900
      IF (NUBG.GT.0) THEN                                                LAKAPP.......17000
C.....APPLY CONDITION UNLESS LAKE INTERACTION CALLS FOR NOT              LAKAPP.......17100
C       NOT APPLYING, AS DETERMINED BELOW.                               LAKAPP.......17200
      LKBCUBG = .TRUE.                                                   LAKAPP.......17300
      DO 870 IUG=1,NUBG                                                  LAKAPP.......17400
         I=IUBG(IUG)                                                     LAKAPP.......17500
C........IF NOT ELIGIBLE TO BE A LAKE NODE, SKIP.                        LAKAPP.......17600
         ISURF = ISURFACE(I)                                             LAKAPP.......17700
         IF (ISURF.EQ.0) CYCLE                                           LAKAPP.......17800
         IF (.NOT.LAKNOD(ISURF)%LAKEOK) CYCLE                            LAKAPP.......17900
C........INTERACTION.                                                    LAKAPP.......18000
         NFB = IBCSUG(IUG)                                               LAKAPP.......18100
         IF (ISLAKE(IABS(I))) THEN                                       LAKAPP.......18200
            IF (ILKUG(NFB).EQ.-1) LKBCUBG(IUG) = .FALSE.                 LAKAPP.......18300
C...........LAKE IS PRESENT, SO DO NOT APPLY CONDITION IF                LAKAPP.......18400
C              INTERACTION FLAG = -1                                     LAKAPP.......18500
         ELSE                                                            LAKAPP.......18600
C...........LAKE IS ABSENT, SO DO NOT APPLY CONDITION IF                 LAKAPP.......18700
C              INTERACTION FLAG = 1                                      LAKAPP.......18800
            IF (ILKUG(NFB).EQ.1) LKBCUBG(IUG) = .FALSE.                  LAKAPP.......18900
         END IF                                                          LAKAPP.......19000
  870 CONTINUE                                                           LAKAPP.......19100
      END IF                                                             LAKAPP.......19200
C                                                                        LAKAPP.......19300
      RETURN                                                             LAKAPP.......19400
      END                                                                LAKAPP.......19500
C                                                                        LAKAPP.......19600
C     SUBROUTINE        L  A  K  D  O  N           SUTRA VERSION 3.0     LAKDON.........100
C                                                                        LAKDON.........200
C *** PURPOSE :                                                          LAKDON.........300
C ***  TO CLEAN UP ONCE ALL LAKE CALCULATIONS ARE DONE.                  LAKDON.........400
C                                                                        LAKDON.........500
      SUBROUTINE LAKDON()                                                LAKDON.........600
      USE LARR                                                           LAKDON.........700
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               LAKDON.........800
      INTEGER KTYPE(2)                                                   LAKDON.........900
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  LAKDON........1000
     1   NPCYC,NUCYC,NPRINT,NBCFPR,NBCSPR,NBCPPR,NBCUPR,NLAKPR,IREAD,    LAKDON........1100
     1   NBGPPR,NBGUPR,ISTORE,NOUMAT,IUNSAT,IALSAT,KTYPE                 LAKDON........1200
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8,K9,                 LAKDON........1300
     1   K10,K11,K12,K13,K14,K15,K16,K17,K18,K19,K20,K21,K22,K23         LAKDON........1400
C                                                                        LAKDON........1500
C.....CLOSE LAKE-RELATED FILES.                                          LAKDON........1600
      CLOSE(K16)                                                         LAKDON........1700
      CLOSE(K17)                                                         LAKDON........1800
      CLOSE(K18)                                                         LAKDON........1900
      CLOSE(K19)                                                         LAKDON........2000
      CLOSE(K20)                                                         LAKDON........2100
      CLOSE(K21)                                                         LAKDON........2200
      CLOSE(K22)                                                         LAKDON........2300
      CLOSE(K23)                                                         LAKDON........2400
C                                                                        LAKDON........2500
C.....DEALLOCATE ARRAYS.                                                 LAKDON........2600
      DEALLOCATE (PLK, ULK)                                              LAKDON........2700
      DEALLOCATE (ULKITR)                                                LAKDON........2800
      DEALLOCATE (KL)                                                    LAKDON........2900
      DEALLOCATE (KLOW, KHIGH, KVMAX)                                    LAKDON........3000
      DEALLOCATE (ISLIMITED,ISSPLIT)                                     LAKDON........3100
      DEALLOCATE (FRRO, FDRO)                                            LAKDON........3200
      DEALLOCATE (LAKNOD)                                                LAKDON........3300
      DEALLOCATE (LLOW, LLOWN, ISTAT, VOLW, UW)                          LAKDON........3400
      DEALLOCATE (VOLWM1, UWM1, UWMSM1, ISTATM1)                         LAKDON........3500
      DEALLOCATE (VOVER, WMOVER, SMOVER, STGB)                           LAKDON........3600
      DEALLOCATE (LPAR, LCHD, LSIB)                                      LAKDON........3700
      DEALLOCATE (ELEVND)                                                LAKDON........3800
      DEALLOCATE (VFUL, VMAX)                                            LAKDON........3900
      DEALLOCATE (NSVMAX, SVARRY)                                        LAKDON........4000
      DEALLOCATE(FGWGLO,FGWLLO)                                          LAKDON........4100
      DEALLOCATE(FEXGLO,FEXLLO)                                          LAKDON........4200
      DEALLOCATE(FROGLO)                                                 LAKDON........4300
      DEALLOCATE(QLO,QULO)                                               LAKDON........4400
      DEALLOCATE(GGWGLO,GGWLLO)                                          LAKDON........4500
      DEALLOCATE(GEXGLO,GEXLLO)                                          LAKDON........4600
      DEALLOCATE(GROGLO)                                                 LAKDON........4700
      DEALLOCATE(FGWG,FGWL)                                              LAKDON........4800
      DEALLOCATE(FEXG,FEXL)                                              LAKDON........4900
      DEALLOCATE(FROG)                                                   LAKDON........5000
      DEALLOCATE(FLKG,FLKL,FLLL)                                         LAKDON........5100
      DEALLOCATE(Q,QU)                                                   LAKDON........5200
      DEALLOCATE(FSPILL,GSPILL)                                          LAKDON........5300
      DEALLOCATE(GGWG,GGWL)                                              LAKDON........5400
      DEALLOCATE(GEXG,GEXL)                                              LAKDON........5500
      DEALLOCATE(GROG)                                                   LAKDON........5600
      DEALLOCATE(GLKG,GLKL,GLLL)                                         LAKDON........5700
      DEALLOCATE (LSPL)                                                  LAKDON........5800
      DEALLOCATE (UWMS,UWMSO)                                            LAKDON........5900
      DEALLOCATE (STGBO,UWO)                                             LAKDON........6000
      DEALLOCATE (ISTATO,VOLWO)                                          LAKDON........6100
      DEALLOCATE (NBR)                                                   LAKDON........6200
      DEALLOCATE (ISURFACE)                                              LAKDON........6300
C                                                                        LAKDON........6400
      RETURN                                                             LAKDON........6500
      END                                                                LAKDON........6600
C                                                                        LAKDON........6700
C     SUBROUTINE        L  A  K  D  R  Y           SUTRA VERSION 3.0     LAKDRY.........100
C                                                                        LAKDRY.........200
C *** PURPOSE :                                                          LAKDRY.........300
C ***  TO SET A LAKE TO DRY.                                             LAKDRY.........400
C                                                                        LAKDRY.........500
      SUBROUTINE LAKDRY(LK,ISTATNEW,VOLWNEW,WMCNEW,UWMSNEW,UWNEW,        LAKDRY.........600
     1   QNEW,QUNEW)                                                     LAKDRY.........700
      USE LARR                                                           LAKDRY.........800
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               LAKDRY.........900
      DIMENSION ISTATNEW(NLAKES)                                         LAKDRY........1000
      DIMENSION VOLWNEW(NLAKES)                                          LAKDRY........1100
      DIMENSION WMCNEW(NLAKES),UWMSNEW(NLAKES),UWNEW(NLAKES)             LAKDRY........1200
      DIMENSION QNEW(NLAKES),QUNEW(NLAKES)                               LAKDRY........1300
      COMMON /DIMEN/ NLAKES, NSIL, NCEL, NPLK                            LAKDRY........1400
      COMMON /RHOLAK/ RHOLK                                              LAKDRY........1500
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       LAKDRY........1600
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITBCS,ITRST,ITMAX,TSTART      LAKDRY........1700
C                                                                        LAKDRY........1800
C.....SET LAKE TO DRY.                                                   LAKDRY........1900
      ISTATNEW(LK) = 0                                                   LAKDRY........2000
      VOLWNEW(LK) = 0D0                                                  LAKDRY........2100
      WMCNEW(LK) = 0D0                                                   LAKDRY........2200
      UWMSNEW(LK) = 0D0                                                  LAKDRY........2300
      UWNEW(LK) = 0D0                                                    LAKDRY........2400
      ISURF = KLOW(LK)                                                   LAKDRY........2500
      ILN = LAKNOD(ISURF)%INODE                                          LAKDRY........2600
      ISLAKE(ILN) = .FALSE.                                              LAKDRY........2700
C                                                                        LAKDRY........2800
      RETURN                                                             LAKDRY........2900
      END                                                                LAKDRY........3000
C                                                                        LAKDRY........3100
C     SUBROUTINE        L  A  K  N  E  W           SUTRA VERSION 3.0     LAKNEW.........100
C                                                                        LAKNEW.........200
C *** PURPOSE :                                                          LAKNEW.........300
C ***  TO PERFORM FLOW AND TRANSPORT CALCULATIONS FOR LAKES AND UPDATE   LAKNEW.........400
C ***  LAKE STATUS.                                                      LAKNEW.........500
C                                                                        LAKNEW.........600
      SUBROUTINE LAKNEW()                                                LAKNEW.........700
      USE LARR                                                           LAKNEW.........800
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               LAKNEW.........900
      DIMENSION ISTATNEW(NLAKES),LSPLNEW(NLAKES)                         LAKNEW........1000
      DIMENSION VOLWNEW(NLAKES),QNEW(NLAKES),QUNEW(NLAKES)               LAKNEW........1100
      DIMENSION FSPLNEW(NLAKES),GSPLNEW(NLAKES)                          LAKNEW........1200
      DIMENSION WMCNEW(NLAKES),UWMSNEW(NLAKES),UWNEW(NLAKES)             LAKNEW........1300
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  LAKNEW........1400
     1   NPCYC,NUCYC,NPRINT,NBCFPR,NBCSPR,NBCPPR,NBCUPR,NLAKPR,IREAD,    LAKNEW........1500
     1   NBGPPR,NBGUPR,ISTORE,NOUMAT,IUNSAT,IALSAT,KTYPE                 LAKNEW........1600
      COMMON /DIMEN/ NLAKES, NSIL, NCEL, NPLK                            LAKNEW........1700
      COMMON /RHOLAK/ RHOLK                                              LAKNEW........1800
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       LAKNEW........1900
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITBCS,ITRST,ITMAX,TSTART      LAKNEW........2000
      EXTERNAL INCD                                                      LAKNEW........2100
C                                                                        LAKNEW........2200
C.....INITIALIZE NEW TOTAL RATES.                                        LAKNEW........2300
      QNEW = Q                                                           LAKNEW........2400
      QUNEW = QU                                                         LAKNEW........2500
C                                                                        LAKNEW........2600
C.....CALCULATE NEW CUMULATIVE LAKE-WATER VOLUMES AND MASSES             LAKNEW........2700
C        AND SOLUTE MASSES OR ENERGY CONTENTS.                           LAKNEW........2800
      DO 200 LK=1,NLAKES                                                 LAKNEW........2900
         VOLWNEW(LK) = VOLW(LK) + DELT*QNEW(LK)/RHOLK                    LAKNEW........3000
         WMCNEW(LK) = VOLWNEW(LK)*RHOLK                                  LAKNEW........3100
         UWMSNEW(LK) = UWMS(LK) + DELT*QUNEW(LK)                         LAKNEW........3200
  200 CONTINUE                                                           LAKNEW........3300
C                                                                        LAKNEW........3400
C.....RECOMPUTE LAKE STATUS, SPILL RATES, AND CONCENTRATIONS OR          LAKNEW........3500
C        SPECIFIC ENERGY CONTENTS.                                       LAKNEW........3600
      ISTATNEW = -999                                                    LAKNEW........3700
      LSPLNEW = 0                                                        LAKNEW........3800
      FSPLNEW = 0D0                                                      LAKNEW........3900
      GSPLNEW = 0D0                                                      LAKNEW........4000
      UWNEW = 0D0                                                        LAKNEW........4100
      DO 300 LK=1,NLAKES                                                 LAKNEW........4200
         IF (ISTATNEW(LK).NE.-999) THEN                                  LAKNEW........4300
C...........IF LAKE STATUS HAS ALREADY BEEN RECOMPUTED, SKIP.            LAKNEW........4400
C              THIS OCCURS IF A LAKE'S STATUS WAS DETERMINED             LAKNEW........4500
C              WHILE ITS SIBLING WAS BEING PROCESSED.                    LAKNEW........4600
            CYCLE                                                        LAKNEW........4700
         ELSE IF (ISTAT(LK).EQ.-4) THEN                                  LAKNEW........4800
C...........LAKE IS DISQUALIFIED AND REMAINS SO.                         LAKNEW........4900
            ISTATNEW(LK) = -4                                            LAKNEW........5000
         ELSE IF (ISSPLIT(LK)) THEN                                      LAKNEW........5100
C...........LAKE IS ALWAYS SPLIT.                                        LAKNEW........5200
            ISTATNEW(LK) = -1                                            LAKNEW........5300
         ELSE IF (LK.EQ.1) THEN                                          LAKNEW........5400
C...........THIS IS LAKE 1.                                              LAKNEW........5500
            VEXCESS = VOLWNEW(1) - VMAX(1)                               LAKNEW........5600
            IF (VEXCESS.LE.0D0) THEN                                     LAKNEW........5700
C..............LAKE 1 IS NOT OVERFILLED.                                 LAKNEW........5800
               CALL LAKNOV(1,ISTATNEW,VOLWNEW,WMCNEW,UWMSNEW,UWNEW,      LAKNEW........5900
     1            QNEW,QUNEW)                                            LAKNEW........6000
            ELSE                                                         LAKNEW........6100
C..............LAKE 1 IS OVERFILLED.                                     LAKNEW........6200
               VSPILL = VEXCESS                                          LAKNEW........6300
               CALL LAKOV(1,VSPILL,ISTATNEW,VOLWNEW,LSPLNEW,             LAKNEW........6400
     1            FSPLNEW,GSPLNEW,QNEW,QUNEW,WMCNEW,UWMSNEW,UWNEW)       LAKNEW........6500
            END IF                                                       LAKNEW........6600
         ELSE IF (ISTATNEW(LPAR(LK)).EQ.-1) THEN                         LAKNEW........6700
C...........PARENT ENDS SPLIT (LAKE DOES NOT END COALESCED).             LAKNEW........6800
            LKSIB = LSIB(LK)                                             LAKNEW........6900
            IF (ISLIMITED(LK).AND.ISLIMITED(LKSIB)) THEN                 LAKNEW........7000
C..............BOTH LAKE AND SIBLING ARE LIMITED.  PROCESS THEM          LAKNEW........7100
C                 INDEPENDENTLY.                                         LAKNEW........7200
               VEXCESS = VOLWNEW(LK) - VMAX(LK)                          LAKNEW........7300
               IF (VEXCESS.LE.0D0) THEN                                  LAKNEW........7400
C.................LAKE IS NOT OVERFILLED.                                LAKNEW........7500
                  CALL LAKNOV(LK,ISTATNEW,VOLWNEW,WMCNEW,UWMSNEW,UWNEW,  LAKNEW........7600
     1               QNEW,QUNEW)                                         LAKNEW........7700
               ELSE                                                      LAKNEW........7800
C.................LAKE IS OVERFILLED.                                    LAKNEW........7900
                  VSPILL = VEXCESS                                       LAKNEW........8000
                  CALL LAKOV(LK,VSPILL,ISTATNEW,VOLWNEW,LSPLNEW,         LAKNEW........8100
     1               FSPLNEW,GSPLNEW,QNEW,QUNEW,WMCNEW,UWMSNEW,UWNEW)    LAKNEW........8200
               END IF                                                    LAKNEW........8300
               VEXSIB = VOLWNEW(LKSIB) - VMAX(LKSIB)                     LAKNEW........8400
               IF (VEXSIB.LE.0D0) THEN                                   LAKNEW........8500
C.................SIBLING IS NOT OVERFILLED.                             LAKNEW........8600
                  CALL LAKNOV(LKSIB,ISTATNEW,VOLWNEW,WMCNEW,UWMSNEW,     LAKNEW........8700
     1               UWNEW,QNEW,QUNEW)                                   LAKNEW........8800
               ELSE                                                      LAKNEW........8900
C.................SIBLING IS OVERFILLED.                                 LAKNEW........9000
                  VSPILL = VEXSIB                                        LAKNEW........9100
                  CALL LAKOV(LKSIB,VSPILL,ISTATNEW,VOLWNEW,LSPLNEW,      LAKNEW........9200
     1               FSPLNEW,GSPLNEW,QNEW,QUNEW,WMCNEW,UWMSNEW,UWNEW)    LAKNEW........9300
               END IF                                                    LAKNEW........9400
            ELSE                                                         LAKNEW........9500
C..............ONE OR NEITHER LAKE IS LIMITED.                           LAKNEW........9600
               IF (.NOT.ISLIMITED(LK)) THEN                              LAKNEW........9700
                  LKU = LK                                               LAKNEW........9800
                  LKO = LKSIB                                            LAKNEW........9900
               ELSE IF (.NOT.ISLIMITED(LK)) THEN                         LAKNEW.......10000
                  LKU = LKSIB                                            LAKNEW.......10100
                  LKO = LK                                               LAKNEW.......10200
               END IF                                                    LAKNEW.......10300
               VEXCESS = VOLWNEW(LKU) - VMAX(LKU)                        LAKNEW.......10400
               IF (VEXCESS.LE.0D0) THEN                                  LAKNEW.......10500
C.................UNLIMITED LAKE IS NOT OVERFILLED.  CHECK OTHER.        LAKNEW.......10600
                  VEXO = VOLWNEW(LKO) - VMAX(LKO)                        LAKNEW.......10700
                  IF (VEXO.LE.0D0) THEN                                  LAKNEW.......10800
C....................OTHER IS NOT OVERFILLED.                            LAKNEW.......10900
                     CALL LAKNOV(LKU,ISTATNEW,VOLWNEW,WMCNEW,UWMSNEW,    LAKNEW.......11000
     1                  UWNEW,QNEW,QUNEW)                                LAKNEW.......11100
                     CALL LAKNOV(LKO,ISTATNEW,VOLWNEW,WMCNEW,UWMSNEW,    LAKNEW.......11200
     1                  UWNEW,QNEW,QUNEW)                                LAKNEW.......11300
                  ELSE                                                   LAKNEW.......11400
C....................OTHER IS OVERFILLED.                                LAKNEW.......11500
                     VSPILL = VEXO                                       LAKNEW.......11600
                     CALL LAKOV(LKO,VSPILL,ISTATNEW,VOLWNEW,LSPLNEW,     LAKNEW.......11700
     1                  FSPLNEW,GSPLNEW,QNEW,QUNEW,WMCNEW,UWMSNEW,UWNEW) LAKNEW.......11800
                  END IF                                                 LAKNEW.......11900
               ELSE                                                      LAKNEW.......12000
C.................UNLIMITED LAKE IS OVERFILLED.                          LAKNEW.......12100
                  VSPILL = VEXCESS                                       LAKNEW.......12200
                  CALL LAKOV(LKU,VSPILL,ISTATNEW,VOLWNEW,LSPLNEW,        LAKNEW.......12300
     1               FSPLNEW,GSPLNEW,QNEW,QUNEW,WMCNEW,UWMSNEW,UWNEW)    LAKNEW.......12400
               END IF                                                    LAKNEW.......12500
            END IF                                                       LAKNEW.......12600
         ELSE                                                            LAKNEW.......12700
C...........PARENT DOES NOT END SPLIT (LAKE ENDS COALESCED).             LAKNEW.......12800
            IF (ISTAT(LPAR(LK)).NE.-1) THEN                              LAKNEW.......12900
C..............PARENT DOES NOT BEGIN SPLIT (LAKE BEGINS COALESCED).      LAKNEW.......13000
C                 NO SPILLS.                                             LAKNEW.......13100
               LKSIB = LSIB(LK)                                          LAKNEW.......13200
               LKPAR = LPAR(LK)                                          LAKNEW.......13300
               ISTATNEW(LK) = -3                                         LAKNEW.......13400
               ISTATNEW(LKSIB) = -3                                      LAKNEW.......13500
               VOLWNEW(LK) = VMAX(LK)                                    LAKNEW.......13600
               VOLWNEW(LKSIB) = VMAX(LKSIB)                              LAKNEW.......13700
               WMCNEW(LK) = VOLWNEW(LK)*RHOLK                            LAKNEW.......13800
               WMCNEW(LKSIB) = VOLWNEW(LKSIB)*RHOLK                      LAKNEW.......13900
               UWNEW(LK) = UWNEW(LKPAR)                                  LAKNEW.......14000
               UWNEW(LKSIB) = UWNEW(LKPAR)                               LAKNEW.......14100
               UWMSNEW(LK) = UWNEW(LK)*WMCNEW(LK)                        LAKNEW.......14200
               UWMSNEW(LKSIB) = UWNEW(LKSIB)*WMCNEW(LKSIB)               LAKNEW.......14300
            ELSE                                                         LAKNEW.......14400
C..............PARENT BEGINS SPLIT (LAKE DOES NOT BEGIN COALESCED).      LAKNEW.......14500
               LKSIB = LSIB(LK)                                          LAKNEW.......14600
               QFILL = QNEW(LK)/RHOLK                                    LAKNEW.......14700
               QFILLSIB = QNEW(LKSIB)/RHOLK                              LAKNEW.......14800
               VFILL = VMAX(LK) - VOLW(LK)                               LAKNEW.......14900
               VFILLSIB = VMAX(LKSIB) - VOLW(LKSIB)                      LAKNEW.......15000
               IF (QNEW(LKSIB).LE.0D0) THEN                              LAKNEW.......15100
C.................SIBLING CAN'T BE SPILLING, SO IT MUST BE THE LAKE.     LAKNEW.......15200
C.................(FILL TIMES WITHOUT SPILL.)                            LAKNEW.......15300
                  TFILL = VFILL/QFILL                                    LAKNEW.......15400
                  TFILLSIB = HUGE(1D0)                                   LAKNEW.......15500
               ELSE IF (QNEW(LK).LE.0D0) THEN                            LAKNEW.......15600
C.................LAKE CAN'T BE SPILLING, SO IT MUST BE THE SIBLING.     LAKNEW.......15700
C.................(FILL TIMES WITHOUT SPILL.)                            LAKNEW.......15800
                  TFILL = HUGE(1D0)                                      LAKNEW.......15900
                  TFILLSIB = VFILLSIB/QFILLSIB                           LAKNEW.......16000
               ELSE                                                      LAKNEW.......16100
C.................COULD BE EITHER LAKE OR SIBING THAT IS SPILLING,       LAKNEW.......16200
C                    OR NEITHER.                                         LAKNEW.......16300
C.................(FILL TIMES WITHOUT SPILL.)                            LAKNEW.......16400
                  TFILL = VFILL/QFILL                                    LAKNEW.......16500
                  TFILLSIB = VFILLSIB/QFILLSIB                           LAKNEW.......16600
               END IF                                                    LAKNEW.......16700
               IF (TFILL.LT.TFILLSIB) THEN                               LAKNEW.......16800
C.................LAKE FILLS UP FIRST, SO IT IS SPILLING.                LAKNEW.......16900
                  QFILLSIB = QFILLSIB + QFILL                            LAKNEW.......17000
C.................(FILL TIME INCLUDING SPILL.)                           LAKNEW.......17100
                  TFILLSIB = (VFILLSIB + TFILL*QFILL)/QFILLSIB           LAKNEW.......17200
                  TSPILL = TFILLSIB - TFILL                              LAKNEW.......17300
                  VSPILL = TSPILL*QFILL                                  LAKNEW.......17400
                  CALL LAKOV(LK,VSPILL,ISTATNEW,VOLWNEW,LSPLNEW,         LAKNEW.......17500
     1               FSPLNEW,GSPLNEW,QNEW,QUNEW,WMCNEW,UWMSNEW,UWNEW)    LAKNEW.......17600
               ELSE IF (TFILL.GT.TFILLSIB) THEN                          LAKNEW.......17700
C.................SIBLING FILLS UP FIRST, SO IT IS SPILLING.             LAKNEW.......17800
                  QFILL = QFILL + QFILLSIB                               LAKNEW.......17900
C.................(FILL TIME INCLUDING SPILL.)                           LAKNEW.......18000
                  TFILL = (VFILL + TFILLSIB*QFILLSIB)/QFILL              LAKNEW.......18100
                  TSPILL = TFILL - TFILLSIB                              LAKNEW.......18200
                  VSPILL = TSPILL*QFILLSIB                               LAKNEW.......18300
                  CALL LAKOV(LKSIB,VSPILL,ISTATNEW,VOLWNEW,LSPLNEW,      LAKNEW.......18400
     1               FSPLNEW,GSPLNEW,QNEW,QUNEW,WMCNEW,UWMSNEW,UWNEW)    LAKNEW.......18500
               ELSE                                                      LAKNEW.......18600
C.................LAKE AND SIBLING FILL UP AT THE SAME TIME, SO          LAKNEW.......18700
C                    NEITHER SPILLS.                                     LAKNEW.......18800
                  LKSIB = LSIB(LK)                                       LAKNEW.......18900
                  LKPAR = LPAR(LK)                                       LAKNEW.......19000
                  ISTATNEW(LK) = -3                                      LAKNEW.......19100
                  ISTATNEW(LKSIB) = -3                                   LAKNEW.......19200
                  VOLWNEW(LK) = VMAX(LK)                                 LAKNEW.......19300
                  VOLWNEW(LKSIB) = VMAX(LKSIB)                           LAKNEW.......19400
                  WMCNEW(LK) = VOLWNEW(LK)*RHOLK                         LAKNEW.......19500
                  WMCNEW(LKSIB) = VOLWNEW(LKSIB)*RHOLK                   LAKNEW.......19600
                  UWNEW(LK) = UWNEW(LKPAR)                               LAKNEW.......19700
                  UWNEW(LKSIB) = UWNEW(LKPAR)                            LAKNEW.......19800
                  UWMSNEW(LK) = UWNEW(LK)*WMCNEW(LK)                     LAKNEW.......19900
                  UWMSNEW(LKSIB) = UWNEW(LKSIB)*WMCNEW(LKSIB)            LAKNEW.......20000
               END IF                                                    LAKNEW.......20100
            END IF                                                       LAKNEW.......20200
         END IF                                                          LAKNEW.......20300
  300 CONTINUE                                                           LAKNEW.......20400
C                                                                        LAKNEW.......20500
C.....HANDLE OVERDRAFTS AND SET STATUS OF DRY LAKES.                     LAKNEW.......20600
      VOVER = 0D0                                                        LAKNEW.......20700
      WMOVER = 0D0                                                       LAKNEW.......20800
      SMOVER = 0D0                                                       LAKNEW.......20900
      DO 500 LK=1,NLAKES                                                 LAKNEW.......21000
         IF ((ME.EQ.-1).AND.(UWMSNEW(LK).LT.0D0)) THEN                   LAKNEW.......21100
C...........SOLUTE MASS OVERDRAWN.                                       LAKNEW.......21200
            SMOVER(LK) = -UWMSNEW(LK)                                    LAKNEW.......21300
            UWMSNEW(LK) = 0D0                                            LAKNEW.......21400
            UWNEW(LK) = 0D0                                              LAKNEW.......21500
         END IF                                                          LAKNEW.......21600
         IF (VOLWNEW(LK).LE.0D0) THEN                                    LAKNEW.......21700
C...........LAKE WATER OVERDRAWN.                                        LAKNEW.......21800
            VOVER(LK) = -VOLWNEW(LK)                                     LAKNEW.......21900
            WMOVER(LK) = -WMCNEW(LK)                                     LAKNEW.......22000
            CALL LAKDRY(LK,ISTATNEW,VOLWNEW,WMCNEW,UWMSNEW,UWNEW,        LAKNEW.......22100
     1         QNEW,QUNEW)                                               LAKNEW.......22200
         END IF                                                          LAKNEW.......22300
  500 CONTINUE                                                           LAKNEW.......22400
C                                                                        LAKNEW.......22500
C.....LOAD PERMANENT LAKE VARIABLES WITH NEW VALUES.                     LAKNEW.......22600
      ISTAT = ISTATNEW                                                   LAKNEW.......22700
      VOLW = VOLWNEW                                                     LAKNEW.......22800
      UWMS = UWMSNEW                                                     LAKNEW.......22900
      UW = UWNEW                                                         LAKNEW.......23000
      LSPL = LSPLNEW                                                     LAKNEW.......23100
      FSPILL = FSPLNEW                                                   LAKNEW.......23200
      GSPILL = GSPLNEW                                                   LAKNEW.......23300
      Q = QNEW                                                           LAKNEW.......23400
      QU = QUNEW                                                         LAKNEW.......23500
C                                                                        LAKNEW.......23600
      RETURN                                                             LAKNEW.......23700
      END                                                                LAKNEW.......23800
C                                                                        LAKNEW.......23900
C     SUBROUTINE        L  A  K  N  O  V           SUTRA VERSION 3.0     LAKNOV.........100
C                                                                        LAKNOV.........200
C *** PURPOSE :                                                          LAKNOV.........300
C ***  TO UPDATE A LAKE THAT IS NOT OVERFILLED.                          LAKNOV.........400
C                                                                        LAKNOV.........500
      SUBROUTINE LAKNOV(LK,ISTATNEW,VOLWNEW,WMCNEW,UWMSNEW,UWNEW,        LAKNOV.........600
     1   QNEW,QUNEW)                                                     LAKNOV.........700
      USE LARR                                                           LAKNOV.........800
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               LAKNOV.........900
      DIMENSION ISTATNEW(NLAKES)                                         LAKNOV........1000
      DIMENSION VOLWNEW(NLAKES)                                          LAKNOV........1100
      DIMENSION WMCNEW(NLAKES),UWMSNEW(NLAKES),UWNEW(NLAKES)             LAKNOV........1200
      DIMENSION QNEW(NLAKES),QUNEW(NLAKES)                               LAKNOV........1300
      COMMON /DIMEN/ NLAKES, NSIL, NCEL, NPLK                            LAKNOV........1400
      COMMON /RHOLAK/ RHOLK                                              LAKNOV........1500
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       LAKNOV........1600
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITBCS,ITRST,ITMAX,TSTART      LAKNOV........1700
C                                                                        LAKNOV........1800
      IF (VOLWNEW(LK).EQ.VMAX(LK)) THEN                                  LAKNOV........1900
C........LAKE IS FULL.                                                   LAKNOV........2000
         ISTATNEW(LK) = 2                                                LAKNOV........2100
         UWNEW(LK) = UWMSNEW(LK)/WMCNEW(LK)                              LAKNOV........2200
      ELSE IF (LCHD(LK,1).EQ.0) THEN                                     LAKNOV........2300
C........LAKE HAS NO CHILDREN, SO IT IS PARTIALLY FILLED OR DRY.         LAKNOV........2400
         IF (VOLWNEW(LK).GT.0D0) THEN                                    LAKNOV........2500
C...........PARTIALLY FILLED.                                            LAKNOV........2600
            ISTATNEW(LK) = 1                                             LAKNOV........2700
            UWNEW(LK) = UWMSNEW(LK)/WMCNEW(LK)                           LAKNOV........2800
         ELSE                                                            LAKNOV........2900
C...........DRY. (STATUS UPDATED ELSEWHERE.)                             LAKNOV........3000
         END IF                                                          LAKNOV........3100
      ELSE                                                               LAKNOV........3200
C........CHECK NEW VOLUME OF LAKE AGAINST SUM OF ITS CHILDREN.           LAKNOV........3300
         LKC1 = LCHD(LK,1)                                               LAKNOV........3400
         LKC2 = LCHD(LK,2)                                               LAKNOV........3500
         VSUM = VMAX(LKC1) + VMAX(LKC2)                                  LAKNOV........3600
         IF (VOLWNEW(LK).GT.VSUM) THEN                                   LAKNOV........3700
C...........NEW VOLUME OF LAKE EXCEEDS SUM OF ITS CHILDREN, SO           LAKNOV........3800
C           LAKE IS PARTIALLY FILLED.                                    LAKNOV........3900
            ISTATNEW(LK) = 1                                             LAKNOV........4000
            UWNEW(LK) = UWMSNEW(LK)/WMCNEW(LK)                           LAKNOV........4100
         ELSE                                                            LAKNOV........4200
C...........NEW VOLUME OF LAKE DOES NOT EXCEED SUM OF CHILDREN,          LAKNOV........4300
C           LAKE IS SPLIT.                                               LAKNOV........4400
            ISTATNEW(LK) = -1                                            LAKNOV........4500
         END IF                                                          LAKNOV........4600
      END IF                                                             LAKNOV........4700
C                                                                        LAKNOV........4800
      RETURN                                                             LAKNOV........4900
      END                                                                LAKNOV........5000
C                                                                        LAKNOV........5100
C     SUBROUTINE        L  A  K  O  V              SUTRA VERSION 3.0     LAKOV..........100
C                                                                        LAKOV..........200
C *** PURPOSE :                                                          LAKOV..........300
C ***  TO UPDATE A LAKE THAT IS OVERFILLED, AND ITS SIBLING.             LAKOV..........400
C                                                                        LAKOV..........500
      SUBROUTINE LAKOV(LK,VSPILL,ISTATNEW,VOLWNEW,LSPLNEW,               LAKOV..........600
     1   FSPLNEW,GSPLNEW,QNEW,QUNEW,WMCNEW,UWMSNEW,UWNEW)                LAKOV..........700
      USE LARR                                                           LAKOV..........800
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               LAKOV..........900
      LOGICAL ISOVER                                                     LAKOV.........1000
      DIMENSION ISTATNEW(NLAKES),LSPLNEW(NLAKES)                         LAKOV.........1100
      DIMENSION VOLWNEW(NLAKES),QNEW(NLAKES)                             LAKOV.........1200
      DIMENSION FSPLNEW(NLAKES),GSPLNEW(NLAKES)                          LAKOV.........1300
      DIMENSION WMCNEW(NLAKES),UWMSNEW(NLAKES),UWNEW(NLAKES)             LAKOV.........1400
      DIMENSION QUNEW(NLAKES)                                            LAKOV.........1500
      COMMON /DIMEN/ NLAKES, NSIL, NCEL, NPLK                            LAKOV.........1600
      COMMON /RHOLAK/ RHOLK                                              LAKOV.........1700
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       LAKOV.........1800
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITBCS,ITRST,ITMAX,TSTART      LAKOV.........1900
      EXTERNAL INCD                                                      LAKOV.........2000
C                                                                        LAKOV.........2100
      WMSPILL = RHOLK*VSPILL                                             LAKOV.........2200
C                                                                        LAKOV.........2300
      IF (ISLIMITED(LK)) THEN                                            LAKOV.........2400
C........LAKE IS LIMITED, SO IT SPILLS OUT OF THE MODEL.  SIBLING        LAKOV.........2500
C           IS UNAFFECTED.                                               LAKOV.........2600
         LSPLNEW(LK) = -1                                                LAKOV.........2700
         FSPLNEW(LK) = WMSPILL/DELT                                      LAKOV.........2800
         GSPLNEW(LK) = FSPLNEW(LK)*UW(LK)                                LAKOV.........2900
         ISTATNEW(LK) = 2                                                LAKOV.........3000
         VOLWNEW(LK) = VMAX(LK)                                          LAKOV.........3100
         WMCNEW(LK) = VOLWNEW(LK)*RHOLK                                  LAKOV.........3200
         UWMSNEW(LK) = UWMSNEW(LK) - WMSPILL*UW(LK)                      LAKOV.........3300
         UWNEW(LK) = UWMSNEW(LK)/WMCNEW(LK)                              LAKOV.........3400
         GOTO 999                                                        LAKOV.........3500
      END IF                                                             LAKOV.........3600
C                                                                        LAKOV.........3700
C.....LAKE IS NOT LIMITED, SO IT EITHER COALESCES WITH ITS               LAKOV.........3800
C        SIBLING OR SPILLS INTO IT.  ULTIMATE FATE OF ANY                LAKOV.........3900
C        SPILLOVER DEPENDS ON SIBLING.                                   LAKOV.........4000
      LKSIB = LSIB(LK)                                                   LAKOV.........4100
      LKK = INCD(LKSIB)                                                  LAKOV.........4200
      LSPLNEW(LK) = LKK                                                  LAKOV.........4300
      FSPLNEW(LK) = WMSPILL/DELT                                         LAKOV.........4400
      GSPLNEW(LK) = FSPLNEW(LK)*UW(LK)                                   LAKOV.........4500
      DO WHILE (LKK.GE.LKSIB)                                            LAKOV.........4600
         VOLWNEW(LKK) = VOLWNEW(LKK) + VSPILL                            LAKOV.........4700
         WMCNEW(LKK) = VOLWNEW(LKK)*RHOLK                                LAKOV.........4800
         UWMSNEW(LKK) = UWMSNEW(LKK) + WMSPILL*UW(LK)                    LAKOV.........4900
         QNEW(LKK) = QNEW(LKK) + FSPLNEW(LK)                             LAKOV.........5000
         QUNEW(LKK) = QUNEW(LKK) + GSPLNEW(LK)                           LAKOV.........5100
         LKK = LPAR(LKK)                                                 LAKOV.........5200
      END DO                                                             LAKOV.........5300
      IF (ISTATNEW(LPAR(LK)).NE.-1) THEN                                 LAKOV.........5400
C........PARENT DOES NOT END SPLIT, SO LAKE AND SIBLING COALESCED.       LAKOV.........5500
C          OVERRIDE SIBLING'S VOLUME WITH FULL VOLUME.                   LAKOV.........5600
C          OVERRIDE "FULL" STATUS OF LAKE WITH "COALESCED".              LAKOV.........5700
         LKSIB = LSIB(LK)                                                LAKOV.........5800
         LKPAR = LPAR(LK)                                                LAKOV.........5900
         ISTATNEW(LK) = -3                                               LAKOV.........6000
         ISTATNEW(LKSIB) = -3                                            LAKOV.........6100
         VOLWNEW(LK) = VMAX(LK)                                          LAKOV.........6200
         VOLWNEW(LKSIB) = VMAX(LKSIB)                                    LAKOV.........6300
         WMCNEW(LK) = VOLWNEW(LK)*RHOLK                                  LAKOV.........6400
         WMCNEW(LKSIB) = VOLWNEW(LKSIB)*RHOLK                            LAKOV.........6500
         UWNEW(LK) = UWNEW(LKPAR)                                        LAKOV.........6600
         UWNEW(LKSIB) = UWNEW(LKPAR)                                     LAKOV.........6700
         UWMSNEW(LK) = UWNEW(LK)*WMCNEW(LK)                              LAKOV.........6800
         UWMSNEW(LKSIB) = UWNEW(LKSIB)*WMCNEW(LKSIB)                     LAKOV.........6900
      ELSE                                                               LAKOV.........7000
C........LAKE SPILLS INTO SIBLING.                                       LAKOV.........7100
         ISTATNEW(LK) = 2                                                LAKOV.........7200
         VOLWNEW(LK) = VMAX(LK)                                          LAKOV.........7300
         WMCNEW(LK) = VOLWNEW(LK)*RHOLK                                  LAKOV.........7400
         UWMSNEW(LK) = UWMSNEW(LK) - WMSPILL*UW(LK)                      LAKOV.........7500
         UWNEW(LK) = UWMSNEW(LK)/WMCNEW(LK)                              LAKOV.........7600
         IF (ISSPLIT(LKSIB)) THEN                                        LAKOV.........7700
C...........SIBLING IS AN ALWAYS-SPLIT LAKE, SO ULTIMATE FATE OF         LAKOV.........7800
C              RECEIVED SPILLOVER WILL BE DETERMINED LATER BY ONE        LAKOV.........7900
C              OF ITS DESCENDANTS.                                       LAKOV.........8000
            ISTATNEW(LKSIB) = -1                                         LAKOV.........8100
            GOTO 999                                                     LAKOV.........8200
         ELSE IF (.NOT.ISLIMITED(LKSIB)) THEN                            LAKOV.........8300
C...........SIBLING IS NOT LIMITED, SO IT IS NOT OVERFILLED.             LAKOV.........8400
            ISOVER = .FALSE.                                             LAKOV.........8500
         ELSE                                                            LAKOV.........8600
C...........SIBLING IS LIMITED, SO CHECK WHETHER IT'S OVERFILLED.        LAKOV.........8700
            VEXSIB = VOLWNEW(LKSIB) - VMAX(LKSIB)                        LAKOV.........8800
            IF (VEXSIB.LE.0D0) THEN                                      LAKOV.........8900
               ISOVER = .FALSE.                                          LAKOV.........9000
            ELSE                                                         LAKOV.........9100
               ISOVER = .TRUE.                                           LAKOV.........9200
            END IF                                                       LAKOV.........9300
         END IF                                                          LAKOV.........9400
         IF (.NOT.ISOVER) THEN                                           LAKOV.........9500
C...........SIBLING IS NOT OVERFILLED.                                   LAKOV.........9600
            CALL LAKNOV(LKSIB,ISTATNEW,VOLWNEW,WMCNEW,UWMSNEW,UWNEW,     LAKOV.........9700
     1         QNEW,QUNEW)                                               LAKOV.........9800
          ELSE                                                           LAKOV.........9900
C...........SIBING IS OVERFILLED AND SPILLS OUT OF THE MODEL.            LAKOV........10000
            WMSSIB = RHOLK*VEXSIB                                        LAKOV........10100
            LSPLNEW(LKSIB) = -1                                          LAKOV........10200
            FSPLNEW(LKSIB) = WMSSIB/DELT                                 LAKOV........10300
            GSPLNEW(LKSIB) = FSPLNEW(LKSIB)*UW(LKSIB)                    LAKOV........10400
            ISTATNEW(LKSIB) = 2                                          LAKOV........10500
            VOLWNEW(LKSIB) = VMAX(LKSIB)                                 LAKOV........10600
            WMCNEW(LKSIB) = VOLWNEW(LKSIB)*RHOLK                         LAKOV........10700
            UWMSNEW(LKSIB) = UWMSNEW(LKSIB) - WMSSIB*UW(LKSIB)           LAKOV........10800
            UWNEW(LKSIB) = UWMSNEW(LKSIB)/WMCNEW(LKSIB)                  LAKOV........10900
          END IF                                                         LAKOV........11000
      END IF                                                             LAKOV........11100
C                                                                        LAKOV........11200
  999 RETURN                                                             LAKOV........11300
      END                                                                LAKOV........11400
C                                                                        LAKOV........11500
C     SUBROUTINE        L  A  K  O  V  I  N        SUTRA VERSION 3.0     LAKOVIN........100
C                                                                        LAKOVIN........200
C *** PURPOSE :                                                          LAKOVIN........300
C ***  TO APPLY AN INITIAL SPECIFICATION WHEN THE SPECIFIED LAKE IS      LAKOVIN........400
C ***  OVERFILLED BY THAT SPECIFICATION.                                 LAKOVIN........500
C                                                                        LAKOVIN........600
      SUBROUTINE LAKOVIN(LK,STGI,UWI,IST,ISPEC)                          LAKOVIN........700
      USE LARR                                                           LAKOVIN........800
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               LAKOVIN........900
      LOGICAL ISDESC                                                     LAKOVIN.......1000
      DIMENSION ISPEC(NLAKES)                                            LAKOVIN.......1100
      CHARACTER*80 ERRCOD,CHERR(10)                                      LAKOVIN.......1200
      DIMENSION INERR(10),RLERR(10)                                      LAKOVIN.......1300
      COMMON /DIMEN/ NLAKES, NSIL, NCEL, NPLK                            LAKOVIN.......1400
      EXTERNAL ISDESC                                                    LAKOVIN.......1500
C                                                                        LAKOVIN.......1600
C.....SPECIFIED LAKE IS OVERFILLED BY INITIAL STAGE, SO APPLY INITIAL    LAKOVIN.......1700
C        STAGE TO HIGHEST ANCESTOR THAT IS NOT OVERFILLED BY IT.         LAKOVIN.......1800
C        SET ALL DESCENDANTS OF THAT ANCESTOR TO BE COALESCED.           LAKOVIN.......1900
      VOLI = VOLLAK(LK,STGI)                                             LAKOVIN.......2000
      DO WHILE (VOLI.GT.VMAX(LK))                                        LAKOVIN.......2100
         IF (ISLIMITED(LK).OR.ISSPLIT(LK)) THEN                          LAKOVIN.......2200
            ERRCOD = "LKIN-3-5"                                          LAKOVIN.......2300
            IF (ILON.GT.0) THEN                                          LAKOVIN.......2400
               CHERR(1) = "node"                                         LAKOVIN.......2500
               INERR = ILON                                              LAKOVIN.......2600
            ELSE                                                         LAKOVIN.......2700
               CHERR(1) = "lake"                                         LAKOVIN.......2800
               INERR = -ILON                                             LAKOVIN.......2900
            END IF                                                       LAKOVIN.......3000
            CALL SUTERR(ERRCOD,CHERR,INERR,RLERR)                        LAKOVIN.......3100
         END IF                                                          LAKOVIN.......3200
         LK = LPAR(LK)                                                   LAKOVIN.......3300
         VOLI = VOLLAK(LK,STGI)                                          LAKOVIN.......3400
      END DO                                                             LAKOVIN.......3500
C.....IF THE LAKE IS ALREADY AT A HIGHER STAGE, SKIP IT. OTHERWISE,      LAKOVIN.......3600
C        APPLY THE SPECIFICATION AND SET ITS DESCENDANTS TO COALESCED.   LAKOVIN.......3700
      IF (VOLI.GE.VOLW(LK)) THEN                                         LAKOVIN.......3800
         VOLW(LK) = VOLI                                                 LAKOVIN.......3900
         UW(LK) = UWI                                                    LAKOVIN.......4000
         IF (VOLI.EQ.VMAX(LK)) THEN                                      LAKOVIN.......4100
            ISTAT(LK) = 2                                                LAKOVIN.......4200
         ELSE                                                            LAKOVIN.......4300
            ISTAT(LK) = 1                                                LAKOVIN.......4400
         END IF                                                          LAKOVIN.......4500
         ISPEC(LK) = IST                                                 LAKOVIN.......4600
         IF (LCHD(LK,1).NE.0) THEN                                       LAKOVIN.......4700
            DO 700 LKK=LK+1,NLAKES                                       LAKOVIN.......4800
               IF (.NOT.ISDESC(LKK,LK)) CYCLE                            LAKOVIN.......4900
               VOLW(LKK) = VMAX(LKK)                                     LAKOVIN.......5000
               UW(LKK) = UWI                                             LAKOVIN.......5100
               ISTAT(LKK) = -3                                           LAKOVIN.......5200
               ISPEC(LKK) = IST                                          LAKOVIN.......5300
700         CONTINUE                                                     LAKOVIN.......5400
         END IF                                                          LAKOVIN.......5500
      END IF                                                             LAKOVIN.......5600
C                                                                        LAKOVIN.......5700
      RETURN                                                             LAKOVIN.......5800
      END                                                                LAKOVIN.......5900
C                                                                        LAKOVIN.......6000
C     SUBROUTINE        L  A  K  S  E  T           SUTRA VERSION 3.0     LAKSET.........100
C                                                                        LAKSET.........200
C *** PURPOSE :                                                          LAKSET.........300
C ***  TO SET UP LAKE-RELATED INFORMATION AT THE START OF THE            LAKSET.........400
C ***  SIMULATION.                                                       LAKSET.........500
C                                                                        LAKSET.........600
      SUBROUTINE LAKSET()                                                LAKSET.........700
      USE LARR                                                           LAKSET.........800
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               LAKSET.........900
      CHARACTER*40 LKNFIL,STGFIL,BLKFIL,BTRFIL                           LAKSET........1000
      LOGICAL ACTIVE(999)                                                LAKSET........1100
      LOGICAL ONCEBL                                                     LAKSET........1200
      LOGICAL LAKUP                                                      LAKSET........1300
      INTEGER LCHD0(999,2)                                               LAKSET........1400
      INTEGER KTYPE(2)                                                   LAKSET........1500
      INTEGER RMVDIM,IMVDIM,I1VDIM,CMVDIM,LMVDIM,PMVDIM                  LAKSET........1600
      COMMON /BUDO/ TSECO,LUTSO                                          LAKSET........1700
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  LAKSET........1800
     1   NPCYC,NUCYC,NPRINT,NBCFPR,NBCSPR,NBCPPR,NBCUPR,NLAKPR,IREAD,    LAKSET........1900
     1   NBGPPR,NBGUPR,ISTORE,NOUMAT,IUNSAT,IALSAT,KTYPE                 LAKSET........2000
      COMMON /DIMEN/ NLAKES, NSIL, NCEL, NPLK                            LAKSET........2100
      COMMON /DIMSRF/ MAXCON, NNSURF, NNVERT                             LAKSET........2200
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8,K9,                 LAKSET........2300
     1   K10,K11,K12,K13,K14,K15,K16,K17,K18,K19,K20,K21,K22,K23         LAKSET........2400
      COMMON /LUP/ LAKUP                                                 LAKSET........2500
      COMMON /MEMCNT/ RMVDIM,IMVDIM,I1VDIM,CMVDIM,LMVDIM,PMVDIM          LAKSET........2600
      COMMON /OBL/ ONCEBL                                                LAKSET........2700
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      LAKSET........2800
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        LAKSET........2900
      COMMON /RHOLAK/ RHOLK                                              LAKSET........3000
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           LAKSET........3100
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       LAKSET........3200
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITBCS,ITRST,ITMAX,TSTART      LAKSET........3300
C                                                                        LAKSET........3400
      ONCEBL = .FALSE.                                                   LAKSET........3500
C                                                                        LAKSET........3600
C.....DETERMINE CONNECTIVITY OF SURFACE NODES FOR IDENTIFYING CLUSTERS.  LAKSET........3700
      CALL SURCON()                                                      LAKSET........3800
C.....ALLOCATE LAKE NODE INFO ARRAY AND COMPUTE COORDINATES              LAKSET........3900
C        AND AREAS.  INITIALIZE ALL CLUSTER NUMBERS TO ONE.              LAKSET........4000
      CALL INITLN()                                                      LAKSET........4100
C                                                                        LAKSET........4200
C.....SET LAKE-AREA NODES AND OVERWRITE LAKE-BOTTOM ELEVATIONS IF        LAKSET........4300
C        SPECIFIED.                                                      LAKSET........4400
      IF (K18.NE.-1) CALL INPLKAR()                                      LAKSET........4500
C.....READ IN BOUNDARY CONDITION INTERACTIONS                            LAKSET........4600
      CALL INPLKBC()                                                     LAKSET........4700
C                                                                        LAKSET........4800
C.....SORT LAKE NODES BY ELEVATION.                                      LAKSET........4900
      CALL SORTEL()                                                      LAKSET........5000
C                                                                        LAKSET........5100
C.....LOOP STAGE THROUGH LAKE NODE ELEVATIONS FROM HIGH TO LOW,          LAKSET........5200
C        IDENTIFYING LAKES.                                              LAKSET........5300
      ALLOCATE (KL(NNSURF))                                              LAKSET........5400
      IMVDIM = IMVDIM + NNSURF                                           LAKSET........5500
      STAGE = 1D98                                                       LAKSET........5600
      ACTIVE(1) = .TRUE.                                                 LAKSET........5700
      LCHD0 = 0                                                          LAKSET........5800
      KLLGST = 1                                                         LAKSET........5900
      DO 400 K=NNSURF,1,-1                                               LAKSET........6000
C........SET STAGE TO LAKE NODE ELEVATION.  SKIP IF SAME AS PREVIOUS.    LAKSET........6100
         STPRV = STAGE                                                   LAKSET........6200
         STAGE = ELEVND(K)%ELEV                                          LAKSET........6300
         IF (STAGE.EQ.STPRV) CYCLE                                       LAKSET........6400
C........FOR THIS STAGE, IDENTIFY DISTINCT CLUSTERS.  USE THEM TO        LAKSET........6500
C           DETERMINE CLUSTER NUMBERS THAT DEFINE LAKES.                 LAKSET........6600
         CALL FINDKL(STAGE, KLLGST, ACTIVE, LCHD0)                       LAKSET........6700
C........COMPUTE LOCALLY NUMBERED CLUSTERS IN THE ENTIRE DOMAIN          LAKSET........6800
C           (AS OPPOSED TO WITHIN A SINGLE LAKE), JUST TO SEE THEM.      LAKSET........6900
         LAKE = -1                                                       LAKSET........7000
         CALL HOSKOP(LAKE, STAGE, KLMAX)                                 LAKSET........7100
  400 CONTINUE                                                           LAKSET........7200
C.....SET NUMBER OF LAKES.                                               LAKSET........7300
      NLAKES = KLLGST                                                    LAKSET........7400
C                                                                        LAKSET........7500
C.....RENUMBER THE LAKES TO CONFORM TO THE REQUIREMENTS OF THE           LAKSET........7600
C       ALGORITHM THAT CASCADES RATES OF WATER INPUT/OUTPUT DOWN THE     LAKSET........7700
C       TREE: NO "LEVEL" OF THE PARENT/CHILD TREE MAY CONTAIN A LOWER    LAKSET........7800
C       NUMBER THAN DOES THE LEVEL ABOVE.                                LAKSET........7900
      CALL RENUMB(LCHD0)                                                 LAKSET........8000
C                                                                        LAKSET........8100
C.....ALLOCATE STATUS ARRAYS.                                            LAKSET........8200
      ALLOCATE(ISTAT(NLAKES),ISTATO(NLAKES),ISTATM1(NLAKES))             LAKSET........8300
      IMVDIM = IMVDIM + 3*NLAKES                                         LAKSET........8400
C                                                                        LAKSET........8500
C.....COMPUTE STAGE-VOLUME ARRAYS AND SET FULL VOLUMES.                  LAKSET........8600
      CALL HILO()                                                        LAKSET........8700
      CALL VSTAGE()                                                      LAKSET........8800
C.....FIND LOWEST LAKE TO WHICH EACH NODE DRAINS.                        LAKSET........8900
      CALL GETLON()                                                      LAKSET........9000
C.....FIND LOWEST LAKE TO WHICH EACH LAKE DRAINS.                        LAKSET........9100
      CALL GETLOW()                                                      LAKSET........9200
C                                                                        LAKSET........9300
C.....ALLOCATE ARRAYS FOR FLOW AND TRANSPORT.                            LAKSET........9400
      ALLOCATE(VOLW(NLAKES),VOLWO(NLAKES),VOLWM1(NLAKES),                LAKSET........9500
     1   VOVER(NLAKES),WMOVER(NLAKES),                                   LAKSET........9600
     1   UWMS(NLAKES),UWMSO(NLAKES),UWMSM1(NLAKES),SMOVER(NLAKES),       LAKSET........9700
     1   UW(NLAKES),UWO(NLAKES),UWM1(NLAKES),STGB(NLAKES),STGBO(NLAKES), LAKSET........9800
     1   QLO(NLAKES),QULO(NLAKES),Q(NLAKES),QU(NLAKES),                  LAKSET........9900
     1   FGWGLO(NLAKES),FGWLLO(NLAKES),FEXGLO(NLAKES),FEXLLO(NLAKES),    LAKSET.......10000
     1   FROGLO(NLAKES),FGWG(NLAKES),FGWL(NLAKES),FEXG(NLAKES),          LAKSET.......10100
     1   FEXL(NLAKES),FROG(NLAKES),FLKG(NLAKES),FLKL(NLAKES),            LAKSET.......10200
     1   FLLL(NLAKES),FSPILL(NLAKES),                                    LAKSET.......10300
     1   GGWGLO(NLAKES),GGWLLO(NLAKES),GEXGLO(NLAKES),GEXLLO(NLAKES),    LAKSET.......10400
     1   GROGLO(NLAKES),GGWG(NLAKES),GGWL(NLAKES),GEXG(NLAKES),          LAKSET.......10500
     1   GEXL(NLAKES),GROG(NLAKES),GLKG(NLAKES),GLKL(NLAKES),            LAKSET.......10600
     1   GLLL(NLAKES),GSPILL(NLAKES))                                    LAKSET.......10700
      RMVDIM = RMVDIM + 44*NLAKES                                        LAKSET.......10800
      ALLOCATE(LSPL(NLAKES))                                             LAKSET.......10900
      IMVDIM = IMVDIM + NLAKES                                           LAKSET.......11000
C                                                                        LAKSET.......11100
C.....SET CONSTANT LAKE-WATER DENSITY.                                   LAKSET.......11200
      RHOLK = RHOW0                                                      LAKSET.......11300
C                                                                        LAKSET.......11400
C.....ALLOCATE SOME LAKE-RELATED ARRAYS.                                 LAKSET.......11500
      NPLK = NNSURF                                                      LAKSET.......11600
      ALLOCATE(PLK(NPLK),ULK(NPLK),ULKITR(NPLK))                         LAKSET.......11700
      RMVDIM = RMVDIM + 3*NPLK                                           LAKSET.......11800
C                                                                        LAKSET.......11900
C.....READ MAIN INPUT FOR LAKES AND INITIALIZE LAKES.                    LAKSET.......12000
      CALL INPLKIN()                                                     LAKSET.......12100
C                                                                        LAKSET.......12200
      RETURN                                                             LAKSET.......12300
      END                                                                LAKSET.......12400
C                                                                        LAKSET.......12500
C     SUBROUTINE        L  B  C  C  M  P           SUTRA VERSION 3.0     LBCCMP.........100
C                                                                        LBCCMP.........200
C *** PURPOSE :                                                          LBCCMP.........300
C ***  TO CALCULATE BOUNDARY-CONDITION SOURCES/SINKS TO/FROM THE LAKES.  LBCCMP.........400
C                                                                        LBCCMP.........500
      SUBROUTINE LBCCMP()                                                LBCCMP.........600
      USE LARR                                                           LBCCMP.........700
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               LBCCMP.........800
      COMMON /DIMEN/ NLAKES, NSIL, NCEL, NPLK                            LBCCMP.........900
      COMMON /DIMSRF/ MAXCON, NNSURF, NNVERT                             LBCCMP........1000
      EXTERNAL INCA                                                      LBCCMP........1100
C                                                                        LBCCMP........1200
C.....COMPILE BOUNDARY-CONDITION SOURCES/SINKS, ASSIGNING THEM TO THE    LBCCMP........1300
C        LOWEST (CHILDLESS) LAKES IN THE TREE SO THEY CAN LATER BE       LBCCMP........1400
C        PROPAGATED UP TO ACTIVE ANCESTORS AS NEEDED.                    LBCCMP........1500
C                                                                        LBCCMP........1600
C.....LOOP OVER SURFACE NODES, CHECKING EACH TYPE OF BOUNDARY            LBCCMP........1700
C        CONDITION AND ACCUMULATING FLOWS.                               LBCCMP........1800
      FGWGLO = 0D0                                                       LBCCMP........1900
      GGWGLO = 0D0                                                       LBCCMP........2000
      FGWLLO = 0D0                                                       LBCCMP........2100
      GGWLLO = 0D0                                                       LBCCMP........2200
      FEXGLO = 0D0                                                       LBCCMP........2300
      GEXGLO = 0D0                                                       LBCCMP........2400
      FEXLLO = 0D0                                                       LBCCMP........2500
      GEXLLO = 0D0                                                       LBCCMP........2600
      FROGLO = 0D0                                                       LBCCMP........2700
      GROGLO = 0D0                                                       LBCCMP........2800
      DO 200 ISURF=1,NNSURF                                              LBCCMP........2900
         IF (.NOT.LAKNOD(ISURF)%LAKEOK) CYCLE                            LBCCMP........3000
         LK = LAKNOD(ISURF)%KLUSTR                                       LBCCMP........3100
         LKLO = LLOWN(ISURF)                                             LBCCMP........3200
         ILN = LAKNOD(ISURF)%INODE                                       LBCCMP........3300
         CALL LBCSOP(LK,LKLO,ILN,ISURF)                                  LBCCMP........3400
         CALL LBCPBC(LK,LKLO,ILN,ISURF)                                  LBCCMP........3500
         CALL LBCPBG(LK,LKLO,ILN,ISURF)                                  LBCCMP........3600
         CALL LBCSOU(LK,LKLO,ILN,ISURF)                                  LBCCMP........3700
         CALL LBCUBC(LK,LKLO,ILN,ISURF)                                  LBCCMP........3800
         CALL LBCUBG(LK,LKLO,ILN,ISURF)                                  LBCCMP........3900
  200 CONTINUE                                                           LBCCMP........4000
C                                                                        LBCCMP........4100
C.....SET TOTAL NET FLOWS TO LOWEST LAKES.                               LBCCMP........4200
      QLO = (FGWGLO - FGWLLO) + (FEXGLO - FEXLLO) + FROGLO               LBCCMP........4300
      QULO = (GGWGLO - GGWLLO) + (GEXGLO - GEXLLO) + GROGLO              LBCCMP........4400
C                                                                        LBCCMP........4500
C.....INITIALIZE TOTAL RATES TO "LOWEST LAKE" VALUES.                    LBCCMP........4600
      Q = QLO                                                            LBCCMP........4700
      QU = QULO                                                          LBCCMP........4800
C                                                                        LBCCMP........4900
C.....PROPAGATE TOTAL RATES UP THE TREE.                                 LBCCMP........5000
      DO 400 LK=NLAKES,2,-1                                              LBCCMP........5100
         LKPAR = LPAR(LK)                                                LBCCMP........5200
         Q(LKPAR) = Q(LKPAR) + Q(LK)                                     LBCCMP........5300
         QU(LKPAR) = QU(LKPAR) + QU(LK)                                  LBCCMP........5400
  400 CONTINUE                                                           LBCCMP........5500
C                                                                        LBCCMP........5600
      RETURN                                                             LBCCMP........5700
      END                                                                LBCCMP........5800
C                                                                        LBCCMP........5900
C     SUBROUTINE        L  B  C  P  B  C           SUTRA VERSION 3.0     LBCPBC.........100
C                                                                        LBCPBC.........200
C *** PURPOSE :                                                          LBCPBC.........300
C ***  TO COMPUTE THE EXCHANGES OF WATER AND SOLUTE/ENERGY BETWEEN       LBCPBC.........400
C        A LAKE AND A SPECIFIED-PRESSURE NODE.                           LBCPBC.........500
C                                                                        LBCPBC.........600
      SUBROUTINE LBCPBC(LK,LKLO,ILN,ISURF)                               LBCPBC.........700
      USE LARR                                                           LBCPBC.........800
      USE ALLARR, ONLY : UVEC,IPBC,UBC,PBC,PVEC,GNUPP,QPLITR,IBCPBC      LBCPBC.........900
      USE EXPINT                                                         LBCPBC........1000
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               LBCPBC........1100
      LOGICAL ISBC                                                       LBCPBC........1200
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              LBCPBC........1300
     1   NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                                  LBCPBC........1400
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      LBCPBC........1500
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        LBCPBC........1600
C                                                                        LBCPBC........1700
C.....COMPUTE THE EXCHANGES OF WATER AND SOLUTE/ENERGY BETWEEN A LAKE    LBCPBC........1800
C        AND A SPECIFIED-PRESSURE NODE.  ASSIGN THE EXCHANGES TO THE     LBCPBC........1900
C        LOWEST (CHILDLESS) DESCENDANT LAKE.                             LBCPBC........2000
C                                                                        LBCPBC........2100
C.....NOTE: MULTIPLYING TEMPERATURES BY CW CONVERTS THEM TO              LBCPBC........2200
C        SPECIFIC ENERGY CONTENTS IN THE CASE OF ENERGY TRANSPORT.       LBCPBC........2300
C        (ULAKE IS ALREADY AN ENERGY CONTENT, SO IT IS NOT CONVERTED.)   LBCPBC........2400
C        NO EFFECT IN THE CASE OF SOLUTE TRANSPORT, SINCE CW=1.          LBCPBC........2500
C                                                                        LBCPBC........2600
C.....FIND INDEX OF THE SPECIFIED-PRESSURE NODE.  IF NOT A               LBCPBC........2700
C         SPECIFIED-PRESSURE NODE, RETURN.                               LBCPBC........2800
      ISBC = .FALSE.                                                     LBCPBC........2900
      DO 140 IP=1,NPBC                                                   LBCPBC........3000
         INP = IP                                                        LBCPBC........3100
         IF (IABS(IPBC(INP)).EQ.ILN) THEN                                LBCPBC........3200
            ISBC = .TRUE.                                                LBCPBC........3300
            EXIT                                                         LBCPBC........3400
         END IF                                                          LBCPBC........3500
  140 CONTINUE                                                           LBCPBC........3600
      IF (.NOT.ISBC) GOTO 999                                            LBCPBC........3700
C.....IF NOT APPLIED BECAUSE OF LAKE-BOUNDARY-CONDITION                  LBCPBC........3800
C        INTERACTION OR BCS SPECIFICATION, RETURN.                       LBCPBC........3900
      IF ((IBCPBC(INP).EQ.2).OR.(.NOT.LKBCPBC(INP))) GOTO 999            LBCPBC........4000
C                                                                        LBCPBC........4100
      IF (ISLAKE(ILN)) THEN                                              LBCPBC........4200
C........THIS IS A LAKE NODE (INUNDATED).                                LBCPBC........4300
C                                                                        LBCPBC........4400
C........GET FLOW AND CONC/TEMP ASSOCIATED WITH THE LAKE NODE,           LBCPBC........4500
C           DEPENDING ON WHETHER GROUNDWATER IS RECHARGING OR            LBCPBC........4600
C           DISCHARGING.                                                 LBCPBC........4700
         CALL BUDPBC(INP,QPBC,QUPBC,QEXGW,QEXLK,QEXRO,                   LBCPBC........4800
     1      QUEXGW,QUEXLK,QUEXRO,UEX,PEX,ILN,ISURF)                      LBCPBC........4900
         QLN = QEXLK                                                     LBCPBC........5000
         QULN = QUEXLK                                                   LBCPBC........5100
C........ADD INTO TOTALS.                                                LBCPBC........5200
         IF (QLN.GE.0D0) THEN                                            LBCPBC........5300
            FGWGLO(LKLO) = FGWGLO(LKLO) + QLN                            LBCPBC........5400
         ELSE                                                            LBCPBC........5500
            FGWLLO(LKLO) = FGWLLO(LKLO) - QLN                            LBCPBC........5600
         END IF                                                          LBCPBC........5700
         IF (QULN.GE.0D0) THEN                                           LBCPBC........5800
            GGWGLO(LKLO) = GGWGLO(LKLO) + QULN                           LBCPBC........5900
         ELSE                                                            LBCPBC........6000
            GGWLLO(LKLO) = GGWLLO(LKLO) - QULN                           LBCPBC........6100
         END IF                                                          LBCPBC........6200
C                                                                        LBCPBC........6300
      ELSE                                                               LBCPBC........6400
C........THIS IS NOT A LAKE NODE (NOT INUNDATED).                        LBCPBC........6500
C                                                                        LBCPBC........6600
C........GET RUNOFF AND CONC/TEMP ASSOCIATED WITH THE NON-LAKE NODE,     LBCPBC........6700
C           DEPENDING ON WHETHER GROUNDWATER IS RECHARGING OR            LBCPBC........6800
C           DISCHARGING.                                                 LBCPBC........6900
         CALL BUDPBC(INP,QPBC,QUPBC,QEXGW,QEXLK,QEXRO,                   LBCPBC........7000
     1      QUEXGW,QUEXLK,QUEXRO,UEX,PEX,ILN,ISURF)                      LBCPBC........7100
         QLN = QEXRO                                                     LBCPBC........7200
         QULN = QUEXRO                                                   LBCPBC........7300
C........ADD INTO TOTALS.                                                LBCPBC........7400
         FROGLO(LKLO) = FROGLO(LKLO) + QLN                               LBCPBC........7500
         GROGLO(LKLO) = GROGLO(LKLO) + QULN                              LBCPBC........7600
C                                                                        LBCPBC........7700
      END IF                                                             LBCPBC........7800
C                                                                        LBCPBC........7900
999   RETURN                                                             LBCPBC........8000
      END                                                                LBCPBC........8100
C                                                                        LBCPBC........8200
C     SUBROUTINE        L  B  C  P  B  G           SUTRA VERSION 3.0     LBCPBG.........100
C                                                                        LBCPBG.........200
C *** PURPOSE :                                                          LBCPBG.........300
C ***  TO COMPUTE THE EXCHANGES OF WATER AND SOLUTE/ENERGY BETWEEN       LBCPBG.........400
C        A LAKE AND A GENERALIZED-FLOW NODE.                             LBCPBG.........500
C                                                                        LBCPBG.........600
      SUBROUTINE LBCPBG(LK,LKLO,ILN,ISURF)                               LBCPBG.........700
      USE LARR                                                           LBCPBG.........800
      USE ALLARR, ONLY : UVEC,IPBG,QPGITR,UPBGI,UPBGO,CUPBGO,PVEC,       LBCPBG.........900
     1   PBC,PBG1,PBG2,QPBG1,QPBG2,CPQL1,CPQL2,QPBGIC,GNUPG,IBCPBG       LBCPBG........1000
      USE EXPINT                                                         LBCPBG........1100
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               LBCPBG........1200
      LOGICAL ISBC                                                       LBCPBG........1300
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              LBCPBG........1400
     1   NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                                  LBCPBG........1500
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      LBCPBG........1600
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        LBCPBG........1700
C                                                                        LBCPBG........1800
C.....COMPUTE THE EXCHANGES OF WATER AND SOLUTE/ENERGY BETWEEN A         LBCPBG........1900
C        LAKE AND A GENERALIZED-FLOW NODE.  ASSIGN THE EXCHANGES         LBCPBG........2000
C        TO THE LOWEST (CHILDLESS) DESCENDANT LAKE.                      LBCPBG........2100
C                                                                        LBCPBG........2200
C.....NOTE: MULTIPLYING TEMPERATURES BY CW CONVERTS THEM TO              LBCPBG........2300
C        SPECIFIC ENERGY CONTENTS IN THE CASE OF ENERGY TRANSPORT.       LBCPBG........2400
C        (ULAKE IS ALREADY AN ENERGY CONTENT, SO IT IS NOT CONVERTED.)   LBCPBG........2500
C        NO EFFECT IN THE CASE OF SOLUTE TRANSPORT, SINCE CW=1.          LBCPBG........2600
C                                                                        LBCPBG........2700
C.....FIND INDEX OF THE GENERALIZED-FLOW NODE.  IF NOT A                 LBCPBG........2800
C         GENERALIZED-FLOW NODE, RETURN.                                 LBCPBG........2900
      ISBC = .FALSE.                                                     LBCPBG........3000
      DO 140 IPG=1,NPBG                                                  LBCPBG........3100
         INP = IPG                                                       LBCPBG........3200
         IF (IABS(IPBG(INP)).EQ.ILN) THEN                                LBCPBG........3300
            ISBC = .TRUE.                                                LBCPBG........3400
            EXIT                                                         LBCPBG........3500
         END IF                                                          LBCPBG........3600
  140 CONTINUE                                                           LBCPBG........3700
      IF (.NOT.ISBC) GOTO 999                                            LBCPBG........3800
C.....IF NOT APPLIED BECAUSE OF LAKE-BOUNDARY-CONDITION                  LBCPBG........3900
C        INTERACTION OR BCS SPECIFICATION, RETURN.                       LBCPBG........4000
      IF ((IBCPBG(INP).EQ.2).OR.(.NOT.LKBCPBG(INP))) GOTO 999            LBCPBG........4100
C                                                                        LBCPBG........4200
      IF (ISLAKE(ILN)) THEN                                              LBCPBG........4300
C........THIS IS A LAKE NODE (INUNDATED).                                LBCPBG........4400
C                                                                        LBCPBG........4500
C........GET FLOW AND CONC/TEMP ASSOCIATED WITH THE LAKE NODE,           LBCPBG........4600
C           DEPENDING ON WHETHER WATER IS RECHARGING OR                  LBCPBG........4700
C           DISCHARGING.                                                 LBCPBG........4800
         CALL BUDPBG(INP,QPBG,QUPBG,QEXGW,QEXLK,QEXRO,                   LBCPBG........4900
     1      QUEXGW,QUEXLK,QUEXRO,UEX,PEX1,PEX2,ILN,ISURF)                LBCPBG........5000
         QLN = QEXLK                                                     LBCPBG........5100
         QULN = QUEXLK                                                   LBCPBG........5200
C........ADD INTO TOTALS.                                                LBCPBG........5300
         IF (QLN.GE.0D0) THEN                                            LBCPBG........5400
            IF (LIT.EQ.1) THEN                                           LBCPBG........5500
               FEXGLO(LKLO) = FEXGLO(LKLO) + QLN                         LBCPBG........5600
            ELSE                                                         LBCPBG........5700
               FGWGLO(LKLO) = FGWGLO(LKLO) + QLN                         LBCPBG........5800
            END IF                                                       LBCPBG........5900
         ELSE                                                            LBCPBG........6000
            IF (LIT.EQ.1) THEN                                           LBCPBG........6100
               FEXLLO(LKLO) = FEXLLO(LKLO) - QLN                         LBCPBG........6200
            ELSE                                                         LBCPBG........6300
               FGWLLO(LKLO) = FGWLLO(LKLO) - QLN                         LBCPBG........6400
            END IF                                                       LBCPBG........6500
         END IF                                                          LBCPBG........6600
         IF (QULN.GE.0D0) THEN                                           LBCPBG........6700
            IF (LIT.EQ.1) THEN                                           LBCPBG........6800
               GEXGLO(LKLO) = GEXGLO(LKLO) + QULN                        LBCPBG........6900
            ELSE                                                         LBCPBG........7000
               GGWGLO(LKLO) = GGWGLO(LKLO) + QULN                        LBCPBG........7100
            END IF                                                       LBCPBG........7200
         ELSE                                                            LBCPBG........7300
            IF (LIT.EQ.1) THEN                                           LBCPBG........7400
               GEXLLO(LKLO) = GEXLLO(LKLO) - QULN                        LBCPBG........7500
            ELSE                                                         LBCPBG........7600
               GGWLLO(LKLO) = GGWLLO(LKLO) - QULN                        LBCPBG........7700
            END IF                                                       LBCPBG........7800
         END IF                                                          LBCPBG........7900
C                                                                        LBCPBG........8000
      ELSE                                                               LBCPBG........8100
C........THIS IS NOT A LAKE NODE (NOT INUNDATED).                        LBCPBG........8200
C                                                                        LBCPBG........8300
C........GET RUNOFF AND CONC/TEMP ASSOCIATED WITH THE NON-LAKE NODE,     LBCPBG........8400
C           DEPENDING ON WHETHER GROUNDWATER IS RECHARGING OR            LBCPBG........8500
C           DISCHARGING.                                                 LBCPBG........8600
         CALL BUDPBG(INP,QPBG,QUPBG,QEXGW,QEXLK,QEXRO,                   LBCPBG........8700
     1      QUEXGW,QUEXLK,QUEXRO,UEX,PEX1,PEX2,ILN,ISURF)                LBCPBG........8800
         QLN = QEXRO                                                     LBCPBG........8900
         QULN = QUEXRO                                                   LBCPBG........9000
C........ADD INTO TOTALS.                                                LBCPBG........9100
         FROGLO(LKLO) = FROGLO(LKLO) + QLN                               LBCPBG........9200
         GROGLO(LKLO) = GROGLO(LKLO) + QULN                              LBCPBG........9300
C                                                                        LBCPBG........9400
      END IF                                                             LBCPBG........9500
C                                                                        LBCPBG........9600
999   RETURN                                                             LBCPBG........9700
      END                                                                LBCPBG........9800
C                                                                        LBCPBG........9900
C     SUBROUTINE        L  B  C  S  O  P           SUTRA VERSION 3.0     LBCSOP.........100
C                                                                        LBCSOP.........200
C *** PURPOSE :                                                          LBCSOP.........300
C ***  TO COMPUTE THE EXCHANGES OF WATER AND SOLUTE/ENERGY BETWEEN       LBCSOP.........400
C        A LAKE AND A FLUID SOURCE/SINK NODE.                            LBCSOP.........500
C                                                                        LBCSOP.........600
      SUBROUTINE LBCSOP(LK,LKLO,ILN,ISURF)                               LBCSOP.........700
      USE LARR                                                           LBCSOP.........800
      USE ALLARR, ONLY : UVEC,IQSOP,QIN,UIN,QINITR,IBCSOP                LBCSOP.........900
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               LBCSOP........1000
      LOGICAL ISBC                                                       LBCSOP........1100
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              LBCSOP........1200
     1   NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                                  LBCSOP........1300
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      LBCSOP........1400
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        LBCSOP........1500
C                                                                        LBCSOP........1600
C.....COMPUTE THE EXCHANGES OF WATER AND SOLUTE/ENERGY BETWEEN A LAKE    LBCSOP........1700
C        AND A FLUID SOURCE/SINK NODE.  ASSIGN THE EXCHANGES TO THE      LBCSOP........1800
C        LOWEST (CHILDLESS) DESCENDANT LAKE.                             LBCSOP........1900
C                                                                        LBCSOP........2000
C.....NOTE: MULTIPLYING TEMPERATURES BY CW CONVERTS THEM TO              LBCSOP........2100
C        SPECIFIC ENERGY CONTENTS IN THE CASE OF ENERGY TRANSPORT.       LBCSOP........2200
C        (ULAKE IS ALREADY AN ENERGY CONTENT, SO IT IS NOT CONVERTED.)   LBCSOP........2300
C        NO EFFECT IN THE CASE OF SOLUTE TRANSPORT, SINCE CW=1.          LBCSOP........2400
C                                                                        LBCSOP........2500
C.....FIND INDEX OF THE FLUID SOURCE/SINK NODE.  IF NOT A                LBCSOP........2600
C        FLUID SOURCE/SINK NODE, RETURN.                                 LBCSOP........2700
      ISBC = .FALSE.                                                     LBCSOP........2800
      DO 140 IP=1,NSOP                                                   LBCSOP........2900
         INP = IP                                                        LBCSOP........3000
         IF (IABS(IQSOP(INP)).EQ.ILN) THEN                               LBCSOP........3100
            ISBC = .TRUE.                                                LBCSOP........3200
            EXIT                                                         LBCSOP........3300
         END IF                                                          LBCSOP........3400
  140 CONTINUE                                                           LBCSOP........3500
      IF (.NOT.ISBC) GOTO 999                                            LBCSOP........3600
C.....IF NOT APPLIED BECAUSE OF LAKE-BOUNDARY-CONDITION                  LBCSOP........3700
C        INTERACTION, RETURN.                                            LBCSOP........3800
      IF (.NOT.LKBCSOP(INP)) GOTO 999                                    LBCSOP........3900
C                                                                        LBCSOP........4000
      IF (ISLAKE(ILN)) THEN                                              LBCSOP........4100
C........THIS IS A LAKE NODE (INUNDATED).                                LBCSOP........4200
C                                                                        LBCSOP........4300
C........GET FLOW AND CONC/TEMP ASSOCIATED WITH THE LAKE NODE,           LBCSOP........4400
C           DEPENDING ON WHETHER WATER IS RECHARGING OR                  LBCSOP........4500
C           DISCHARGING.                                                 LBCSOP........4600
         QLN = QIN(ILN)                                                  LBCSOP........4700
         IF (QINITR(ILN).GT.0D0) THEN                                    LBCSOP........4800
            QULN = QINITR(ILN)*CW*UIN(ILN)                               LBCSOP........4900
         ELSE                                                            LBCSOP........5000
            QULN = QINITR(ILN)*CW*ULK(ISURF)                             LBCSOP........5100
         END IF                                                          LBCSOP........5200
C........ADD INTO TOTALS.                                                LBCSOP........5300
         IF (QLN.GE.0D0) THEN                                            LBCSOP........5400
            FEXGLO(LKLO) = FEXGLO(LKLO) + QLN                            LBCSOP........5500
         ELSE                                                            LBCSOP........5600
            FEXLLO(LKLO) = FEXLLO(LKLO) - QLN                            LBCSOP........5700
         END IF                                                          LBCSOP........5800
         IF (QULN.GE.0D0) THEN                                           LBCSOP........5900
            GEXGLO(LKLO) = GEXGLO(LKLO) + QULN                           LBCSOP........6000
         ELSE                                                            LBCSOP........6100
            GEXLLO(LKLO) = GEXLLO(LKLO) - QULN                           LBCSOP........6200
         END IF                                                          LBCSOP........6300
C                                                                        LBCSOP........6400
      ELSE                                                               LBCSOP........6500
C........THIS IS NOT A LAKE NODE (NOT INUNDATED).                        LBCSOP........6600
C                                                                        LBCSOP........6700
C........GET RUNOFF AND CONC/TEMP ASSOCIATED WITH THE NON-LAKE NODE,     LBCSOP........6800
C           DEPENDING ON WHETHER GROUNDWATER IS RECHARGING OR            LBCSOP........6900
C           DISCHARGING.                                                 LBCSOP........7000
         IF (QIN(ILN).GT.0D0) THEN                                       LBCSOP........7100
            QLN = QIN(ILN)*FRRO(LK)                                      LBCSOP........7200
         ELSE                                                            LBCSOP........7300
            QLN = -QIN(ILN)*FDRO(LK)                                     LBCSOP........7400
         END IF                                                          LBCSOP........7500
         IF (QINITR(ILN).GT.0D0) THEN                                    LBCSOP........7600
            QULN = QINITR(ILN)*FRRO(LK)*CW*UIN(ILN)                      LBCSOP........7700
         ELSE                                                            LBCSOP........7800
            QULN = -QINITR(ILN)*FDRO(LK)*CW*UVEC(ILN)                    LBCSOP........7900
         END IF                                                          LBCSOP........8000
C........ADD INTO TOTALS.                                                LBCSOP........8100
         FROGLO(LKLO) = FROGLO(LKLO) + QLN                               LBCSOP........8200
         GROGLO(LKLO) = GROGLO(LKLO) + QULN                              LBCSOP........8300
C                                                                        LBCSOP........8400
      END IF                                                             LBCSOP........8500
C                                                                        LBCSOP........8600
999   RETURN                                                             LBCSOP........8700
      END                                                                LBCSOP........8800
C                                                                        LBCSOP........8900
C     SUBROUTINE        L  B  C  S  O  U           SUTRA VERSION 3.0     LBCSOU.........100
C                                                                        LBCSOU.........200
C *** PURPOSE :                                                          LBCSOU.........300
C ***  TO COMPUTE THE EXCHANGE SOLUTE/ENERGY BETWEEN A LAKE AND          LBCSOU.........400
C        A SOLUTE/ENERGY SOURCE/SINK NODE.                               LBCSOU.........500
C                                                                        LBCSOU.........600
      SUBROUTINE LBCSOU(LK,LKLO,ILN,ISURF)                               LBCSOU.........700
      USE LARR                                                           LBCSOU.........800
      USE ALLARR, ONLY : IQSOU,QUIN,IBCSOU                               LBCSOU.........900
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               LBCSOU........1000
      LOGICAL ISBC                                                       LBCSOU........1100
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              LBCSOU........1200
     1   NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                                  LBCSOU........1300
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      LBCSOU........1400
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        LBCSOU........1500
C                                                                        LBCSOU........1600
C.....COMPUTE THE EXCHANGE OF SOLUTE/ENERGY BETWEEN A LAKE AND A         LBCSOU........1700
C        SOLUTE/ENERGY SOURCE/SINK NODE.  ASSIGN THE EXCHANGE TO         LBCSOU........1800
C        THE LOWEST (CHILDLESS) DESCENDANT LAKE.                         LBCSOU........1900
C                                                                        LBCSOU........2000
C.....NOTE: MULTIPLYING TEMPERATURES BY CW CONVERTS THEM TO              LBCSOU........2100
C        SPECIFIC ENERGY CONTENTS IN THE CASE OF ENERGY TRANSPORT.       LBCSOU........2200
C        (ULAKE IS ALREADY AN ENERGY CONTENT, SO IT IS NOT CONVERTED.)   LBCSOU........2300
C        NO EFFECT IN THE CASE OF SOLUTE TRANSPORT, SINCE CW=1.          LBCSOU........2400
C                                                                        LBCSOU........2500
      IF (ISLAKE(ILN)) THEN                                              LBCSOU........2600
C........THIS IS A LAKE NODE (INUNDATED).                                LBCSOU........2700
C                                                                        LBCSOU........2800
C........FIND INDEX OF THE SOLUTE OR ENERGY SOURCE/SINK NODE.  IF NOT    LBCSOU........2900
C           A SOLUTE OR ENERGY SOURCE/SINK NODE, RETURN.                 LBCSOU........3000
         ISBC = .FALSE.                                                  LBCSOU........3100
         DO 140 IU=1,NUBC                                                LBCSOU........3200
            IPU = NPBC + IU                                              LBCSOU........3300
            INP = IPU                                                    LBCSOU........3400
            IF (IABS(IQSOU(INP)).EQ.ILN) THEN                            LBCSOU........3500
               ISBC = .TRUE.                                             LBCSOU........3600
               EXIT                                                      LBCSOU........3700
            END IF                                                       LBCSOU........3800
  140    CONTINUE                                                        LBCSOU........3900
         IF (.NOT.ISBC) GOTO 999                                         LBCSOU........4000
C........IF NOT APPLIED BECAUSE OF LAKE-BOUNDARY-CONDITION               LBCSOU........4100
C           INTERACTION, RETURN.                                         LBCSOU........4200
         IF (.NOT.LKBCSOU(INP)) GOTO 999                                 LBCSOU........4300
C                                                                        LBCSOU........4400
      ELSE                                                               LBCSOU........4500
C........THIS IS NOT A LAKE NODE (NOT INUNDATED).                        LBCSOU........4600
         GOTO 999                                                        LBCSOU........4700
C                                                                        LBCSOU........4800
      END IF                                                             LBCSOU........4900
C                                                                        LBCSOU........5000
C.....GET INFLOW/OUTFLOW OF SOLUTE/ENERGY.                               LBCSOU........5100
      QULN = QUIN(INP)                                                   LBCSOU........5200
C                                                                        LBCSOU........5300
C.....ADD INTO TOTALS.                                                   LBCSOU........5400
      IF (QULN.GE.0D0) THEN                                              LBCSOU........5500
         GEXGLO(LKLO) = GEXGLO(LKLO) + QULN                              LBCSOU........5600
      ELSE                                                               LBCSOU........5700
         GEXLLO(LKLO) = GEXLLO(LKLO) - QULN                              LBCSOU........5800
      END IF                                                             LBCSOU........5900
C                                                                        LBCSOU........6000
999   RETURN                                                             LBCSOU........6100
      END                                                                LBCSOU........6200
C                                                                        LBCSOU........6300
C     SUBROUTINE        L  B  C  U  B  C           SUTRA VERSION 3.0     LBCUBC.........100
C                                                                        LBCUBC.........200
C *** PURPOSE :                                                          LBCUBC.........300
C ***  TO COMPUTE THE EXCHANGE SOLUTE/ENERGY BETWEEN A LAKE AND          LBCUBC.........400
C        A SPECIFIED-CONC/TEMP NODE.                                     LBCUBC.........500
C                                                                        LBCUBC.........600
      SUBROUTINE LBCUBC(LK,LKLO,ILN,ISURF)                               LBCUBC.........700
      USE LARR                                                           LBCUBC.........800
      USE ALLARR, ONLY : IUBC,UBC,UVEC,GNUUU,IBCUBC                      LBCUBC.........900
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               LBCUBC........1000
      LOGICAL ISBC                                                       LBCUBC........1100
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              LBCUBC........1200
     1   NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                                  LBCUBC........1300
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      LBCUBC........1400
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        LBCUBC........1500
C                                                                        LBCUBC........1600
C.....COMPUTE THE EXCHANGE OF SOLUTE/ENERGY BETWEEN A LAKE AND           LBCUBC........1700
C        A SPECIFIED-U NODE.  ASSIGN THE EXCHANGE TO THE LOWEST          LBCUBC........1800
C        (CHILDLESS) DESCENDANT LAKE.                                    LBCUBC........1900
C                                                                        LBCUBC........2000
C.....NOTE: MULTIPLYING TEMPERATURES BY CW CONVERTS THEM TO              LBCUBC........2100
C        SPECIFIC ENERGY CONTENTS IN THE CASE OF ENERGY TRANSPORT.       LBCUBC........2200
C        (ULAKE IS ALREADY AN ENERGY CONTENT, SO IT IS NOT CONVERTED.)   LBCUBC........2300
C        NO EFFECT IN THE CASE OF SOLUTE TRANSPORT, SINCE CW=1.          LBCUBC........2400
C                                                                        LBCUBC........2500
      IF (ISLAKE(ILN)) THEN                                              LBCUBC........2600
C........THIS IS A LAKE NODE (INUNDATED).                                LBCUBC........2700
C                                                                        LBCUBC........2800
C........FIND INDEX OF THE SPECIFIED-U NODE.  IF NOT A                   LBCUBC........2900
C           SPECIFIED-U NODE, RETURN.                                    LBCUBC........3000
         ISBC = .FALSE.                                                  LBCUBC........3100
         DO 140 IU=1,NUBC                                                LBCUBC........3200
            IPU = NPBC + IU                                              LBCUBC........3300
            INP = IPU                                                    LBCUBC........3400
            IF (IABS(IUBC(INP)).EQ.ILN) THEN                             LBCUBC........3500
               ISBC = .TRUE.                                             LBCUBC........3600
               EXIT                                                      LBCUBC........3700
            END IF                                                       LBCUBC........3800
  140    CONTINUE                                                        LBCUBC........3900
         IF (.NOT.ISBC) GOTO 999                                         LBCUBC........4000
C........IF NOT APPLIED BECAUSE OF LAKE-BOUNDARY-CONDITION               LBCUBC........4100
C           INTERACTION OR BCS SPECIFICATION, RETURN.                    LBCUBC........4200
         IF ((IBCUBC(INP).EQ.2).OR.(.NOT.LKBCUBC(INP))) GOTO 999         LBCUBC........4300
C                                                                        LBCUBC........4400
      ELSE                                                               LBCUBC........4500
C........THIS IS NOT A LAKE NODE (NOT INUNDATED).                        LBCUBC........4600
         GOTO 999                                                        LBCUBC........4700
C                                                                        LBCUBC........4800
      END IF                                                             LBCUBC........4900
C                                                                        LBCUBC........5000
C.....GET INFLOW/OUTFLOW OF SOLUTE/ENERGY.                               LBCUBC........5100
      CALL BUDUBC(IPU,QUBC,QUEXGW,QUEXLK,UEX)                            LBCUBC........5200
      QULN = -QUEXLK                                                     LBCUBC........5300
C                                                                        LBCUBC........5400
C.....ADD INTO TOTALS.                                                   LBCUBC........5500
      IF (QULN.GE.0D0) THEN                                              LBCUBC........5600
         GGWGLO(LKLO) = GGWGLO(LKLO) + QULN                              LBCUBC........5700
      ELSE                                                               LBCUBC........5800
         GGWLLO(LKLO) = GGWLLO(LKLO) - QULN                              LBCUBC........5900
      END IF                                                             LBCUBC........6000
C                                                                        LBCUBC........6100
999   RETURN                                                             LBCUBC........6200
      END                                                                LBCUBC........6300
C                                                                        LBCUBC........6400
C     SUBROUTINE        L  B  C  U  B  G           SUTRA VERSION 3.0     LBCUBG.........100
C                                                                        LBCUBG.........200
C *** PURPOSE :                                                          LBCUBG.........300
C ***  TO COMPUTE THE EXCHANGE SOLUTE/ENERGY BETWEEN A LAKE AND          LBCUBG.........400
C        A GENERALIZED-TRANSPORT NODE.                                   LBCUBG.........500
C                                                                        LBCUBG.........600
      SUBROUTINE LBCUBG(LK,LKLO,ILN,ISURF)                               LBCUBG.........700
      USE LARR                                                           LBCUBG.........800
      USE ALLARR, ONLY : UVEC,IUBG,QUBGIC,GNUUG,IBCUBG                   LBCUBG.........900
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               LBCUBG........1000
      LOGICAL ISBC                                                       LBCUBG........1100
      CHARACTER*80 ERRCOD,CHERR(10)                                      LBCUBG........1200
      DIMENSION INERR(10),RLERR(10)                                      LBCUBG........1300
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              LBCUBG........1400
     1   NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                                  LBCUBG........1500
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      LBCUBG........1600
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        LBCUBG........1700
C                                                                        LBCUBG........1800
C.....COMPUTE THE EXCHANGE OF SOLUTE/ENERGY BETWEEN A LAKE AND           LBCUBG........1900
C        AND A GENERALIZED-TRANSPORT NODE.  ASSIGN THE EXCHANGE          LBCUBG........2000
C        TO THE LOWEST (CHILDLESS) DESCENDANT LAKE.                      LBCUBG........2100
C                                                                        LBCUBG........2200
C.....NOTE: MULTIPLYING TEMPERATURES BY CW CONVERTS THEM TO              LBCUBG........2300
C        SPECIFIC ENERGY CONTENTS IN THE CASE OF ENERGY TRANSPORT.       LBCUBG........2400
C        (ULAKE IS ALREADY AN ENERGY CONTENT, SO IT IS NOT CONVERTED.)   LBCUBG........2500
C        NO EFFECT IN THE CASE OF SOLUTE TRANSPORT, SINCE CW=1.          LBCUBG........2600
C                                                                        LBCUBG........2700
      IF (ISLAKE(ILN)) THEN                                              LBCUBG........2800
C........THIS IS A LAKE NODE (INUNDATED).                                LBCUBG........2900
C                                                                        LBCUBG........3000
C........FIND INDEX OF THE GENERALIZED-TRANSPORT NODE.  IF NOT A         LBCUBG........3100
C            GENERALIZED-TRANSPORT NODE, RETURN.                         LBCUBG........3200
         ISBC = .FALSE.                                                  LBCUBG........3300
         DO 140 IUG=1,NUBG                                               LBCUBG........3400
            INP = IUG                                                    LBCUBG........3500
            IF (IABS(IUBG(INP)).EQ.ILN) THEN                             LBCUBG........3600
               ISBC = .TRUE.                                             LBCUBG........3700
               EXIT                                                      LBCUBG........3800
            END IF                                                       LBCUBG........3900
  140    CONTINUE                                                        LBCUBG........4000
         IF (.NOT.ISBC) GOTO 999                                         LBCUBG........4100
C........IF NOT APPLIED BECAUSE OF LAKE-BOUNDARY-CONDITION               LBCUBG........4200
C           INTERACTION, RETURN.                                         LBCUBG........4300
         IF (.NOT.LKBCUBG(INP)) GOTO 999                                 LBCUBG........4400
C                                                                        LBCUBG........4500
      ELSE                                                               LBCUBG........4600
C........THIS IS NOT A LAKE NODE (NOT INUNDATED).                        LBCUBG........4700
         GOTO 999                                                        LBCUBG........4800
C                                                                        LBCUBG........4900
      END IF                                                             LBCUBG........5000
C                                                                        LBCUBG........5100
C.....GET INFLOW/OUTFLOW OF SOLUTE/ENERGY.                               LBCUBG........5200
      NFB = IBCSUG(INP)                                                  LBCUBG........5300
      LIT = ITIUG(NFB)                                                   LBCUBG........5400
      IF (LIT.EQ.1) THEN                                                 LBCUBG........5500
C........ACTS ANALOGOUSLY TO A FLUID SOURCE/SINK NODE.                   LBCUBG........5600
         QULN = QUBGIC(INP) - GNUUG(INP)*ULK(ISURF)                      LBCUBG........5700
      ELSE IF (LIT.EQ.2) THEN                                            LBCUBG........5800
C........ACTS ANALOGOUSLY TO A SPECIFIED-CONC/TEMP NODE.                 LBCUBG........5900
         QULN = -GNUUG(INP)*(ULK(ISURF) - UVEC(ILN))                     LBCUBG........6000
      ELSE                                                               LBCUBG........6100
         ERRCOD = 'COD-LBCUBG-1'                                         LBCUBG........6200
         CALL SUTERR(ERRCOD,CHERR,INERR,RLERR)                           LBCUBG........6300
      END IF                                                             LBCUBG........6400
C                                                                        LBCUBG........6500
C.....ADD INTO TOTALS.                                                   LBCUBG........6600
      IF (QULN.GE.0D0) THEN                                              LBCUBG........6700
         IF (LIT.EQ.1) THEN                                              LBCUBG........6800
            GEXGLO(LKLO) = GEXGLO(LKLO) + QULN                           LBCUBG........6900
         ELSE                                                            LBCUBG........7000
            GGWGLO(LKLO) = GGWGLO(LKLO) + QULN                           LBCUBG........7100
         END IF                                                          LBCUBG........7200
      ELSE                                                               LBCUBG........7300
         IF (LIT.EQ.1) THEN                                              LBCUBG........7400
            GEXLLO(LKLO) = GEXLLO(LKLO) - QULN                           LBCUBG........7500
         ELSE                                                            LBCUBG........7600
            GGWLLO(LKLO) = GGWLLO(LKLO) - QULN                           LBCUBG........7700
         END IF                                                          LBCUBG........7800
      END IF                                                             LBCUBG........7900
C                                                                        LBCUBG........8000
999   RETURN                                                             LBCUBG........8100
      END                                                                LBCUBG........8200
C                                                                        LBCUBG........8300
C     SUBROUTINE        O  U  T  L  K  B  U        SUTRA VERSION 3.0     OUTLKBU........100
C                                                                        OUTLKBU........200
C *** PURPOSE :                                                          OUTLKBU........300
C ***  TO OUTPUT THE LAKE BUDGETS.                                       OUTLKBU........400
C                                                                        OUTLKBU........500
      SUBROUTINE OUTLKBU(TITLE1,TITLE2)                                  OUTLKBU........600
      USE EXPINT                                                         OUTLKBU........700
      USE SCHDEF                                                         OUTLKBU........800
      USE LARR                                                           OUTLKBU........900
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               OUTLKBU.......1000
      DIMENSION DVDT(NLAKES),DSDT(NLAKES)                                OUTLKBU.......1100
      DIMENSION WMC(NLAKES),DWMDT(NLAKES),DUWMDT(NLAKES),DCTDT(NLAKES)   OUTLKBU.......1200
      CHARACTER*1  TITLE1(80),TITLE2(80)                                 OUTLKBU.......1300
      CHARACTER*2 SSYMB(-4:4)                                            OUTLKBU.......1400
      CHARACTER*15 CSTGB,CDSDT,CUW,CDCTDT,CDUWMDT,CDWMDT,CDVDT           OUTLKBU.......1500
      CHARACTER FRMT*80                                                  OUTLKBU.......1600
      CHARACTER*14 CTYPE2                                                OUTLKBU.......1700
      CHARACTER*80 LAYSTR                                                OUTLKBU.......1800
      INTEGER KTYPE(2)                                                   OUTLKBU.......1900
      LOGICAL ONCEBL, INITCOND                                           OUTLKBU.......2000
      ALLOCATABLE TT(:),ITT(:)                                           OUTLKBU.......2100
      COMMON /BUDO/ TSECO,LUTSO                                          OUTLKBU.......2200
      COMMON /CLAY/ LAYSTR                                               OUTLKBU.......2300
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  OUTLKBU.......2400
     1   NPCYC,NUCYC,NPRINT,NBCFPR,NBCSPR,NBCPPR,NBCUPR,NLAKPR,IREAD,    OUTLKBU.......2500
     1   NBGPPR,NBGUPR,ISTORE,NOUMAT,IUNSAT,IALSAT,KTYPE                 OUTLKBU.......2600
      COMMON /DIMEN/ NLAKES, NSIL, NCEL, NPLK                            OUTLKBU.......2700
      COMMON /DIMLAY/ NLAYS,NNLAY,NELAY                                  OUTLKBU.......2800
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              OUTLKBU.......2900
     1   NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                                  OUTLKBU.......3000
      COMMON /DIMSRF/ MAXCON, NNSURF, NNVERT                             OUTLKBU.......3100
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        OUTLKBU.......3200
      COMMON /DIMX2/ NELTA,NNVEC,NDIMIA,NDIMJA                           OUTLKBU.......3300
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8,K9,                 OUTLKBU.......3400
     1   K10,K11,K12,K13,K14,K15,K16,K17,K18,K19,K20,K21,K22,K23         OUTLKBU.......3500
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,                OUTLKBU.......3600
     1   KPANDS,KVEL,KCORT,KBUDG,KSCRN,KPAUSE                            OUTLKBU.......3700
      COMMON /OBL/ ONCEBL                                                OUTLKBU.......3800
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      OUTLKBU.......3900
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        OUTLKBU.......4000
      COMMON /RHOLAK/ RHOLK                                              OUTLKBU.......4100
      COMMON /SCH/ NSCH,ISCHTS,NSCHAU                                    OUTLKBU.......4200
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           OUTLKBU.......4300
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       OUTLKBU.......4400
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITBCS,ITRST,ITMAX,TSTART      OUTLKBU.......4500
      COMMON /VALLAK/ RNOLK                                              OUTLKBU.......4600
      DATA (SSYMB(M), M=-4,4)                                            OUTLKBU.......4700
     1   /'dq',' c','xx',' s','DY','PF','FL','xx','xx'/                  OUTLKBU.......4800
C                                                                        OUTLKBU.......4900
C                                                                        OUTLKBU.......5000
      INITCOND = (ISSFLO.EQ.0).AND.(IT.EQ.0)                             OUTLKBU.......5100
C                                                                        OUTLKBU.......5200
C                                                                        OUTLKBU.......5300
C.....CALCULATE HEADERS ON FIRST CALL AND CREATE OUTPUT ON EACH CALL.    OUTLKBU.......5400
C                                                                        OUTLKBU.......5500
C                                                                        OUTLKBU.......5600
      IF (.NOT. ONCEBL)  THEN                                            OUTLKBU.......5700
C.....FIRST CALL -- CREATE FILE HEADER.                                  OUTLKBU.......5800
C                                                                        OUTLKBU.......5900
C........CALCULATE THE MAXIMUM NUMBER OF TIME STEPS, KTMAX.              OUTLKBU.......6000
         IF (ISSTRA.NE.1) THEN                                           OUTLKBU.......6100
            KT = 1                                                       OUTLKBU.......6200
         ELSE                                                            OUTLKBU.......6300
            KT = 0                                                       OUTLKBU.......6400
         END IF                                                          OUTLKBU.......6500
         DO 4 JT=1,ITMAX                                                 OUTLKBU.......6600
            IF (MOD(JT,NLAKPR).EQ.0 .OR. JT.EQ.ITRST .OR.                OUTLKBU.......6700
     1         ((JT.EQ.ITRST+1).AND.((ISSTRA.NE.0).OR.(NLAKPR.GT.0))))   OUTLKBU.......6800
     2         KT = KT + 1                                               OUTLKBU.......6900
    4    CONTINUE                                                        OUTLKBU.......7000
         IF(ITMAX.GT.1 .AND. MOD(ITMAX,NLAKPR).NE.0) KT = KT + 1         OUTLKBU.......7100
         KTMAX = KT                                                      OUTLKBU.......7200
C                                                                        OUTLKBU.......7300
C........ALLOCATE LOCAL ARRAYS                                           OUTLKBU.......7400
         ALLOCATE(TT(KTMAX),ITT(KTMAX))                                  OUTLKBU.......7500
C                                                                        OUTLKBU.......7600
C........CALCULATE AND PRINT TIME STEP INFORMATION                       OUTLKBU.......7700
         TS=TSTART                                                       OUTLKBU.......7800
C........TIME STEP VALUE                                                 OUTLKBU.......7900
         JT=0                                                            OUTLKBU.......8000
C........NUMBER OF PRINTED TIME STEPS                                    OUTLKBU.......8100
         KT=0                                                            OUTLKBU.......8200
C........TIME STEP INCREMENT                                             OUTLKBU.......8300
         DELTK=DELT                                                      OUTLKBU.......8400
C........INDICATORS OF WHEN VARIABLES ARE CALCULATED AND PRINTED         OUTLKBU.......8500
         IF (ISSTRA.NE.1) THEN                                           OUTLKBU.......8600
            KT = KT + 1                                                  OUTLKBU.......8700
            TT(KT) = TS                                                  OUTLKBU.......8800
            ITT(KT) = JT                                                 OUTLKBU.......8900
         END IF                                                          OUTLKBU.......9000
         DO 10 JT=1,ITMAX                                                OUTLKBU.......9100
            JTP1 = JT + 1                                                OUTLKBU.......9200
            TS = SCHDLS(ISCHTS)%SLIST(JTP1)%DVALU1                       OUTLKBU.......9300
            IF (MOD(JT,NLAKPR).EQ.0 .OR. JT.EQ.ITRST .OR.                OUTLKBU.......9400
     1         ((JT.EQ.ITRST+1).AND.((ISSTRA.NE.0).OR.(NLAKPR.GT.0))))   OUTLKBU.......9500
     2         THEN                                                      OUTLKBU.......9600
               KT = KT + 1                                               OUTLKBU.......9700
               TT(KT) = TS                                               OUTLKBU.......9800
               ITT(KT) = JT                                              OUTLKBU.......9900
            ENDIF                                                        OUTLKBU......10000
   10    CONTINUE                                                        OUTLKBU......10100
         IF (ISSTRA.EQ.1) TT(KT) = TSTART                                OUTLKBU......10200
C                                                                        OUTLKBU......10300
C                                                                        OUTLKBU......10400
C........PRINT LAST TIME STEP ALWAYS, UNLESS ALREADY PRINTED             OUTLKBU......10500
         IF(ITMAX.GT.1 .AND. MOD(ITMAX,NLAKPR).NE.0) THEN                OUTLKBU......10600
            KT = KT + 1                                                  OUTLKBU......10700
            TT(KT) = TS                                                  OUTLKBU......10800
            ITT(KT) = ITMAX                                              OUTLKBU......10900
         ENDIF                                                           OUTLKBU......11000
C                                                                        OUTLKBU......11100
C........COMPUTE ACTUAL NUMBER OF PRINTED TIME STEPS, KTPRN; LESS THAN   OUTLKBU......11200
C           KTMAX IF RUN IS A RESTART                                    OUTLKBU......11300
         IF (IREAD.EQ.+1) THEN                                           OUTLKBU......11400
            KTPRN = KTMAX                                                OUTLKBU......11500
         ELSE                                                            OUTLKBU......11600
            KTPRN = 0                                                    OUTLKBU......11700
            DO 17 KT=1,KTMAX                                             OUTLKBU......11800
               IF (ITT(KT).GE.ITRST) KTPRN = KTPRN + 1                   OUTLKBU......11900
   17       CONTINUE                                                     OUTLKBU......12000
         END IF                                                          OUTLKBU......12100
C                                                                        OUTLKBU......12200
C........WRITE HEADER INFORMATION                                        OUTLKBU......12300
         WRITE(K19,950) TITLE1, TITLE2                                   OUTLKBU......12400
         IF (KTYPE(2).GT.1) THEN                                         OUTLKBU......12500
            IF (KTYPE(2).EQ.3) THEN                                      OUTLKBU......12600
               CTYPE2 = "BLOCKWISE MESH"                                 OUTLKBU......12700
            ELSE                                                         OUTLKBU......12800
               CTYPE2 = "REGULAR MESH  "                                 OUTLKBU......12900
            END IF                                                       OUTLKBU......13000
            IF (KTYPE(1).EQ.3) THEN                                      OUTLKBU......13100
               WRITE(K19,951) KTYPE(1),CTYPE2,NN1,NN2,NN3,NN," Nodes",   OUTLKBU......13200
     1            NE, " Elems"                                           OUTLKBU......13300
            ELSE                                                         OUTLKBU......13400
               WRITE(K19,952) KTYPE(1),CTYPE2,NN1,NN2,NN," Nodes",       OUTLKBU......13500
     1            NE, " Elems"                                           OUTLKBU......13600
            END IF                                                       OUTLKBU......13700
         ELSE IF (KTYPE(2).EQ.1) THEN                                    OUTLKBU......13800
            WRITE(K19,953) KTYPE(1), LAYSTR(1:6), NLAYS, NNLAY,          OUTLKBU......13900
     1         NN, " Nodes", NE, " Elems"                                OUTLKBU......14000
         ELSE                                                            OUTLKBU......14100
            WRITE(K19,954) KTYPE(1), NN, " Nodes", NE, " Elems"          OUTLKBU......14200
         END IF                                                          OUTLKBU......14300
         WRITE(K19,960) "LAKE BUDGET RESULTS", KTPRN                     OUTLKBU......14400
         DO 920 KT=1,KTMAX                                               OUTLKBU......14500
            IF (ITT(KT).GE.ITRST)                                        OUTLKBU......14600
     1         WRITE(K19,961) ITT(KT), TT(KT)                            OUTLKBU......14700
  920    CONTINUE                                                        OUTLKBU......14800
  950    FORMAT("## ", 80A1,                                             OUTLKBU......14900
     1         /"## ", 80A1,                                             OUTLKBU......15000
     2         /"## ")                                                   OUTLKBU......15100
  951    FORMAT("## ", I1, "-D, ", A, 2X,                                OUTLKBU......15200
     1                 "(", 2(I9, ")*("), I9, ") = ", I9, A, 1X,         OUTLKBU......15300
     2                 "(", I9, A, ")"                                   OUTLKBU......15400
     3         /"## ")                                                   OUTLKBU......15500
  952    FORMAT("## ", I1, "-D, ", A, 14X,                               OUTLKBU......15600
     1                 "(", I9, ")*(", I9, ") = ", I9, A, 1X,            OUTLKBU......15700
     2                 "(", I9, A, ")"                                   OUTLKBU......15800
     3         /"## ")                                                   OUTLKBU......15900
  953    FORMAT("## ", I1, "-D, LAYERED MESH [", A6, "]", 7X,            OUTLKBU......16000
     1                 "(", I9, ")*(", I9, ") = ", I9, A, 1X,            OUTLKBU......16100
     2                 "(", I9, A, ")"                                   OUTLKBU......16200
     3         /"## ")                                                   OUTLKBU......16300
  954    FORMAT("## ", I1, "-D, IRREGULAR MESH", 40X, I9, A, 1X,         OUTLKBU......16400
     1                 "(", I9, A, ")"                                   OUTLKBU......16500
     2         /"## ")                                                   OUTLKBU......16600
  960    FORMAT("## ", 92("="),                                          OUTLKBU......16700
     1         /"## ", A, 45X, I9, " Time steps printed",                OUTLKBU......16800
     2         /"## ", 92("="),                                          OUTLKBU......16900
     3         /"## ",                                                   OUTLKBU......17000
     4         /"## ", 4X, "Time steps"                                  OUTLKBU......17100
     6         /"## ", 3X, "in this file      Time (sec)"                OUTLKBU......17200
     8         /"## ", 2X, 14("-"), 3X, 13("-"))                         OUTLKBU......17300
  961    FORMAT ("## ", 7X, I8, 4X, 1PE13.6, 3(5X, A1, 1X, I8))          OUTLKBU......17400
C                                                                        OUTLKBU......17500
C........DEALLOCATE LOCAL ARRAYS.                                        OUTLKBU......17600
         DEALLOCATE(TT,ITT)                                              OUTLKBU......17700
C                                                                        OUTLKBU......17800
         WRITE(K19,'("## "/"## ", 93("="))')                             OUTLKBU......17900
         WRITE(K19,'(A,22(/"## ",A))') "## ",                            OUTLKBU......18000
     1     "Column headings:",                                           OUTLKBU......18100
     2     "  Lake = lake number",                                       OUTLKBU......18200
     2     "  Status = lake status indicator (described below)",         OUTLKBU......18300
     3     "  Stage = lake stage",                                       OUTLKBU......18400
     4     "  FluVol = volume of fluid in the lake",                     OUTLKBU......18500
     5     "  FluMas = mass of fluid in the lake",                       OUTLKBU......18600
     1     "  FluMasGw = net rate of fluid mass exchange with " //       OUTLKBU......18700
     1        "groundwater",                                             OUTLKBU......18800
     2     "  FluMasGw(+) = rate of fluid mass gain from groundwater",   OUTLKBU......18900
     3     "  FluMasGw(-) = rate of fluid mass loss to groundwater",     OUTLKBU......19000
     1     "  FluMasLak = net rate of fluid mass exchange with other"    OUTLKBU......19100
     1        // " lakes",                                               OUTLKBU......19200
     2     "  FluMasLak(+) = rate of fluid mass gain from other lakes",  OUTLKBU......19300
     3     "  FluMasLak(-) = rate of fluid mass loss to other lakes",    OUTLKBU......19400
     1     "  FluMasExt = net rate of fluid mass exchange with"          OUTLKBU......19500
     1        // " external sources and sinks",                          OUTLKBU......19600
     1        "    other than runoff and lake-area limits",              OUTLKBU......19700
     2     "  FluMasExt(+) = rate of fluid mass gain from"               OUTLKBU......19800
     1        // " external sources and sinks",                          OUTLKBU......19900
     1        "    other than runoff and lake-area limits",              OUTLKBU......20000
     3     "  FluMasExt(-) = rate of fluid mass loss to"                 OUTLKBU......20100
     1        // " external sources and sinks",                          OUTLKBU......20200
     1        "    other than runoff and lake-area limits",              OUTLKBU......20300
     2     "  FluMasRun = rate of fluid mass gain from runoff",          OUTLKBU......20400
     1     "  FluMasLim = rate of fluid mass loss to lake-area limits",  OUTLKBU......20500
     1     "  FluVolOvr = fluid-volume overdraft",                       OUTLKBU......20600
     1     "  FluMasOvr = fluid-mass overdraft"                          OUTLKBU......20700
         IF (ME.EQ.-1) THEN                                              OUTLKBU......20800
           WRITE(K19,'("## ",A,16(/"## ",A))')                           OUTLKBU......20900
     6     "  SolMas = mass of solute in the lake",                      OUTLKBU......21000
     7     "  Conc = solute concentration in the lake",                  OUTLKBU......21100
     4     "  SolMasGw = net rate of solute mass exchange with " //      OUTLKBU......21200
     1        "groundwater",                                             OUTLKBU......21300
     5     "  SolMasGw(+) = rate of solute mass gain from groundwater",  OUTLKBU......21400
     6     "  SolMasGw(-) = rate of solute mass loss to groundwater",    OUTLKBU......21500
     4     "  SolMasLak = net rate of solute mass exchange with other"   OUTLKBU......21600
     1        // " lakes",                                               OUTLKBU......21700
     5     "  SolMasLak(+) = rate of solute mass gain from other lakes", OUTLKBU......21800
     6     "  SolMasLak(-) = rate of solute mass loss to other lakes",   OUTLKBU......21900
     1     "  SolMasExt = net rate of solute mass exchange with"         OUTLKBU......22000
     1        // " external sources and sinks",                          OUTLKBU......22100
     1        "    other than runoff and lake-area limits",              OUTLKBU......22200
     2     "  SolMasExt(+) = rate of solute mass gain from"              OUTLKBU......22300
     1        // " external sources and sinks",                          OUTLKBU......22400
     1        "    other than runoff and lake-area limits",              OUTLKBU......22500
     3     "  SolMasExt(-) = rate of solute mass loss to"                OUTLKBU......22600
     1        // " external sources and sinks",                          OUTLKBU......22700
     1        "    other than runoff and lake-area limits",              OUTLKBU......22800
     2     "  SolMasRun = rate of solute mass gain from runoff",         OUTLKBU......22900
     1     "  SolMasLim = rate of solute mass loss to lake-area limits", OUTLKBU......23000
     1     "  SolMasOvr = solute-mass overdraft"                         OUTLKBU......23100
         ELSE                                                            OUTLKBU......23200
           WRITE(K19,'("## ",A,15(/"## ",A))')                           OUTLKBU......23300
     8     "  Energy = thermal energy content of the lake (cw*Temp)",    OUTLKBU......23400
     9     "  Temp = lake water temperature",                            OUTLKBU......23500
     7     "  EnergyGw = net rate of thermal energy exchange with " //   OUTLKBU......23600
     1        "groundwater",                                             OUTLKBU......23700
     8     "  EnergyGw(+) = rate of thermal energy gain from " //        OUTLKBU......23800
     1        "groundwater",                                             OUTLKBU......23900
     9     "  EnergyGw(-) = rate of thermal energy loss to groundwater", OUTLKBU......24000
     7     "  EnergyLak = net rate of thermal energy exchange with "     OUTLKBU......24100
     1        // "other lakes",                                          OUTLKBU......24200
     8     "  EnergyLak(+) = rate of thermal energy gain from other"     OUTLKBU......24300
     1        // " lakes",                                               OUTLKBU......24400
     9     "  EnergyLak(-) = rate of thermal energy loss to other " //   OUTLKBU......24500
     1        "lakes",                                                   OUTLKBU......24600
     1     "  EnergyExt = net rate of thermal energy exchange with"      OUTLKBU......24700
     1        // " external sources and sinks",                          OUTLKBU......24800
     1        "    other than runoff and lake-area limits",              OUTLKBU......24900
     2     "  EnergyExt(+) = rate of thermal energy gain from"           OUTLKBU......25000
     1        // " external sources and sinks",                          OUTLKBU......25100
     1        "    other than runoff and lake-area limits",              OUTLKBU......25200
     3     "  EnergyExt(-) = rate of thermal energy loss to"             OUTLKBU......25300
     1        // " external sources and sinks",                          OUTLKBU......25400
     1        "    other than runoff and lake-area limits",              OUTLKBU......25500
     2     "  EnergyRun = rate of thermal energy gain from runoff",      OUTLKBU......25600
     1     "  EnergyLim = rate of thermal energy loss" //                OUTLKBU......25700
     1        " to lake-area limits"                                     OUTLKBU......25800
         END IF                                                          OUTLKBU......25900
           WRITE(K19,'("## ",A)')                                        OUTLKBU......26000
     1     "  d()/dt = rate of change with time of the quantity in " //  OUTLKBU......26100
     2       "parentheses"                                               OUTLKBU......26200
         WRITE(K19,'(A,7(/"## ",A))') "## ",                             OUTLKBU......26300
     1     "Lake status indicators:",                                    OUTLKBU......26400
     1     "  dq = lake is disqualified by lake-area limits",            OUTLKBU......26500
     2     "   c = lake is coalesced with its sibling to form parent",   OUTLKBU......26600
     3     "   s = lake is split into its children",                     OUTLKBU......26700
     4     "  DY = dry",                                                 OUTLKBU......26800
     5     "  PF = lake is partially filled",                            OUTLKBU......26900
     6     "  FL = lake is filled to capacity"                           OUTLKBU......27000
         WRITE(K19,'(A,6(/"## ",A),1PE11.4,A,2(/"## ",A),1PE15.7,A)')    OUTLKBU......27100
     1     "## ",                                                        OUTLKBU......27200
     1     "Rates are reported in units of mass/time or energy/time " // OUTLKBU......27300
     1     "that are consistent with the units", "of mass, energy, " //  OUTLKBU......27400
     1     "and time used in the model input.  Amounts and rates " //    OUTLKBU......27500
     1     "reported for a given", "lake include amounts and rates " //  OUTLKBU......27600
     1     "associated with descendants of the lake.  Thus, amounts " // OUTLKBU......27700
     1     "and", "rates reported for lake 1 are totals for the " //     OUTLKBU......27800
     1     "entire system of lakes.  Inflows to lakes are",              OUTLKBU......27900
     1     "positive; outflows are negative.  Correspondence " //        OUTLKBU......28000
     1     "between fluid volume and fluid mass", "is based on the " //  OUTLKBU......28100
     1     "user-specified base density of lake water, RHOW0 =", RHOLK,  OUTLKBU......28200
     1     ".", "For quantities that are undefined because of " //       OUTLKBU......28300
     1     "the current or previous lake status,", "the value " //       OUTLKBU......28400
     1     "output is the user-specified value RNOLK =", RNOLK, "."      OUTLKBU......28500
C                                                                        OUTLKBU......28600
         ONCEBL = .TRUE.                                                 OUTLKBU......28700
C                                                                        OUTLKBU......28800
      END IF                                                             OUTLKBU......28900
C                                                                        OUTLKBU......29000
C.....COMPUTE CUMULATIVE WATER MASSES FROM CUMULATIVE WATER VOLUMES.     OUTLKBU......29100
      WMC = VOLW*RHOLK                                                   OUTLKBU......29200
C                                                                        OUTLKBU......29300
C.....COMPUTE RATES OF CHANGE.                                           OUTLKBU......29400
      IF (ISSFLO.EQ.0) THEN                                              OUTLKBU......29500
         DTSEC = TSEC - TSECO                                            OUTLKBU......29600
         IF (.NOT.INITCOND) DVDT = (VOLW - VOLWO)/DTSEC                  OUTLKBU......29700
         DO 87 LK=1,NLAKES                                               OUTLKBU......29800
            IF ((STGBO(LK).NE.-1D98).AND.                                OUTLKBU......29900
     1          (STGB(LK).NE.-1D98).AND.                                 OUTLKBU......30000
     2          (.NOT.INITCOND)) THEN                                    OUTLKBU......30100
               DSDT(LK) = (STGB(LK) - STGBO(LK))/DTSEC                   OUTLKBU......30200
C..............NOTE: DIVIDING BY CW CONVERTS SPECIFIC ENERGY CONTENT     OUTLKBU......30300
C                 TO TEMPERATURE IN THE CASE OF ENERGY TRANSPORT.  NO    OUTLKBU......30400
C                 EFFECT IN THE CASE OF SOLUTE TRANSPORT, SINCE CW=1.    OUTLKBU......30500
               DCTDT(LK) = ((UW(LK) - UWO(LK))/DTSEC)/CW                 OUTLKBU......30600
            END IF                                                       OUTLKBU......30700
   87    CONTINUE                                                        OUTLKBU......30800
         IF (.NOT.INITCOND) THEN                                         OUTLKBU......30900
            DWMDT = DVDT*RHOLK                                           OUTLKBU......31000
            DUWMDT = (UWMS - UWMSO)/DTSEC                                OUTLKBU......31100
         END IF                                                          OUTLKBU......31200
      ELSE                                                               OUTLKBU......31300
         DVDT = 0D0                                                      OUTLKBU......31400
         DSDT = 0D0                                                      OUTLKBU......31500
         DCTDT = 0D0                                                     OUTLKBU......31600
         DWMDT = 0D0                                                     OUTLKBU......31700
         DUWMDT = 0D0                                                    OUTLKBU......31800
      END IF                                                             OUTLKBU......31900
      DVTDT = DVDT(1)                                                    OUTLKBU......32000
      DWMTDT = DWMDT(1)                                                  OUTLKBU......32100
      DUWMTDT = DUWMDT(1)                                                OUTLKBU......32200
C                                                                        OUTLKBU......32300
C.....HEADER INFORMATION REPEATED BEFORE EACH TIME STEP                  OUTLKBU......32400
      IF ((IT.EQ.0).OR.((IT.EQ.1).AND.(ISSTRA.EQ.1))) THEN               OUTLKBU......32500
         DURN = 0D0                                                      OUTLKBU......32600
         TOUT = TSTART                                                   OUTLKBU......32700
      ELSE                                                               OUTLKBU......32800
         DURN = DELT                                                     OUTLKBU......32900
         TOUT = TSEC                                                     OUTLKBU......33000
      END IF                                                             OUTLKBU......33100
      WRITE(K19,966) IT, DURN, TOUT                                      OUTLKBU......33200
  966 FORMAT('## ',                                                      OUTLKBU......33300
     1      /'## ', 180('='),                                            OUTLKBU......33400
     2      /'## TIME STEP ', I8, 110X, 'Duration: ', 1PE11.4, ' sec',   OUTLKBU......33500
     3                            6X, 'Time: ', 1PE11.4, ' sec',         OUTLKBU......33600
     4      /'## ', 180('='))                                            OUTLKBU......33700
C                                                                        OUTLKBU......33800
C.....WRITE HEADER FOR OUTPUT OF FLUID AMOUNTS AND RATES.                OUTLKBU......33900
      WRITE(K19,191)                                                     OUTLKBU......34000
  191 FORMAT("## "/4X,"Lake",2X,"Status",2X,                             OUTLKBU......34100
     1      8X,"  Stage",8X," FluVol",8X," FluMas",                      OUTLKBU......34200
     2      3X," d(Stage)/dt",3X,"d(FluVol)/dt",3X,"d(FluMas)/dt")       OUTLKBU......34300
C                                                                        OUTLKBU......34400
C.....OUTPUT FLUID AMOUNTS AND RATES.                                    OUTLKBU......34500
      DO 200 LK=1,NLAKES                                                 OUTLKBU......34600
         IF (STGB(LK).EQ.-1D98) THEN                                     OUTLKBU......34700
            WRITE(CSTGB,'(1PE15.7)') RNOLK                               OUTLKBU......34800
         ELSE                                                            OUTLKBU......34900
            WRITE(CSTGB,'(1PE15.7)') CUTSML(STGB(LK))                    OUTLKBU......35000
         END IF                                                          OUTLKBU......35100
         IF (INITCOND.OR.(STGB(LK).EQ.-1D98).OR.                         OUTLKBU......35200
     1      (STGBO(LK).EQ.-1D98)) THEN                                   OUTLKBU......35300
            WRITE(CDSDT,'(1PE15.7)') RNOLK                               OUTLKBU......35400
         ELSE                                                            OUTLKBU......35500
            WRITE(CDSDT,'(1PE15.7)') CUTSML(DSDT(LK))                    OUTLKBU......35600
         END IF                                                          OUTLKBU......35700
         IF (INITCOND) THEN                                              OUTLKBU......35800
            WRITE(CDVDT,'(1PE15.7)') RNOLK                               OUTLKBU......35900
            WRITE(CDWMDT,'(1PE15.7)') RNOLK                              OUTLKBU......36000
         ELSE                                                            OUTLKBU......36100
            WRITE(CDVDT,'(1PE15.7)') CUTSML(DVDT(LK))                    OUTLKBU......36200
            WRITE(CDWMDT,'(1PE15.7)') CUTSML(DWMDT(LK))                  OUTLKBU......36300
         END IF                                                          OUTLKBU......36400
         WRITE(K19,195) LK,SSYMB(ISTAT(LK)),                             OUTLKBU......36500
     1      CSTGB,CUTSML(VOLW(LK)),CUTSML(WMC(LK)),CDSDT,                OUTLKBU......36600
     2      CDVDT,CDWMDT                                                 OUTLKBU......36700
  195    FORMAT(I8,6X,A2,2X,A15,2(1PE15.7),3(A15))                       OUTLKBU......36800
  200 CONTINUE                                                           OUTLKBU......36900
C                                                                        OUTLKBU......37000
C.....WRITE HEADER FOR OUTPUT OF FLUID EXCHANGE RATES.                   OUTLKBU......37100
      WRITE(K19,591)                                                     OUTLKBU......37200
  591 FORMAT(/4X,"Lake",2X,"Status",2X,                                  OUTLKBU......37300
     1      3X," FluMasGw(+)",3X," FluMasGw(-)",3X,"    FluMasGw",       OUTLKBU......37400
     1      3X,"FluMasLak(+)",3X,"FluMasLak(-)",3X,"   FluMasLak",       OUTLKBU......37500
     1      3X,"FluMasExt(+)",3X,"FluMasExt(-)",3X,"   FluMasExt",       OUTLKBU......37600
     1      3X,"   FluMasRun",3X,"   FluMasLim")                         OUTLKBU......37700
C                                                                        OUTLKBU......37800
C.....OUTPUT FLUID EXCHANGE RATES.                                       OUTLKBU......37900
      IF (IT.EQ.0) THEN                                                  OUTLKBU......38000
         FGWG = RNOLK                                                    OUTLKBU......38100
         FGWL = -RNOLK                                                   OUTLKBU......38200
         FGWN = RNOLK                                                    OUTLKBU......38300
         FLKG = RNOLK                                                    OUTLKBU......38400
         FLKL = -RNOLK                                                   OUTLKBU......38500
         FLKN = RNOLK                                                    OUTLKBU......38600
         FEXG = RNOLK                                                    OUTLKBU......38700
         FEXL = -RNOLK                                                   OUTLKBU......38800
         FEXN = RNOLK                                                    OUTLKBU......38900
         FROG = RNOLK                                                    OUTLKBU......39000
         FLLL = -RNOLK                                                   OUTLKBU......39100
      END IF                                                             OUTLKBU......39200
      DO 600 LK=1,NLAKES                                                 OUTLKBU......39300
         IF (IT.NE.0) THEN                                               OUTLKBU......39400
            FGWN = FGWG(LK) - FGWL(LK)                                   OUTLKBU......39500
            FEXN = FEXG(LK) - FEXL(LK)                                   OUTLKBU......39600
            FLKN = FLKG(LK) - FLKL(LK)                                   OUTLKBU......39700
         END IF                                                          OUTLKBU......39800
         WRITE(K19,595) LK,SSYMB(ISTAT(LK)),                             OUTLKBU......39900
     1      CUTSML(FGWG(LK)),CUTSML(-FGWL(LK)),CUTSML(FGWN),             OUTLKBU......40000
     2      CUTSML(FLKG(LK)),CUTSML(-FLKL(LK)),CUTSML(FLKN),             OUTLKBU......40100
     3      CUTSML(FEXG(LK)),CUTSML(-FEXL(LK)),CUTSML(FEXN),             OUTLKBU......40200
     4      CUTSML(FROG(LK)),CUTSML(-FLLL(LK))                           OUTLKBU......40300
  595    FORMAT(I8,6X,A2,2X,11(1PE15.7))                                 OUTLKBU......40400
  600 CONTINUE                                                           OUTLKBU......40500
C                                                                        OUTLKBU......40600
C.....WRITE HEADER FOR OUTPUT OF SOLUTE/ENERGY AMOUNTS AND RATES OF      OUTLKBU......40700
C        CHANGE.                                                         OUTLKBU......40800
      IF (ME.EQ.-1) THEN                                                 OUTLKBU......40900
         WRITE(K19,391)                                                  OUTLKBU......41000
      ELSE                                                               OUTLKBU......41100
         WRITE(K19,392)                                                  OUTLKBU......41200
      END IF                                                             OUTLKBU......41300
  391 FORMAT(/4X,"Lake",2X,"Status",2X,                                  OUTLKBU......41400
     1      8X," SolMas",8X,"   Conc",                                   OUTLKBU......41500
     2      3X,"d(SolMas)/dt",3X,"  d(Conc)/dt")                         OUTLKBU......41600
  392 FORMAT(/4X,"Lake",2X,"Status",2X,                                  OUTLKBU......41700
     1      8X," Energy",8X,"   Temp"                                    OUTLKBU......41800
     2      3X,"d(Energy)/dt",3X,"  d(Temp)/dt")                         OUTLKBU......41900
C                                                                        OUTLKBU......42000
C.....OUTPUT SOLUTE/ENERGY AMOUNTS AND RATES OF CHANGE.                  OUTLKBU......42100
      DO 400 LK=1,NLAKES                                                 OUTLKBU......42200
         IF (STGB(LK).EQ.-1D98) THEN                                     OUTLKBU......42300
            WRITE(CUW,'(1PE15.7)') RNOLK                                 OUTLKBU......42400
         ELSE                                                            OUTLKBU......42500
C...........NOTE: DIVIDING BY CW CONVERTS SPECIFIC ENERGY CONTENT TO     OUTLKBU......42600
C              TEMPERATURE IN THE CASE OF ENERGY TRANSPORT.  NO EFFECT   OUTLKBU......42700
C              IN THE CASE OF SOLUTE TRANSPORT, SINCE CW=1.              OUTLKBU......42800
            WRITE(CUW,'(1PE15.7)') CUTSML(UW(LK)/CW)                     OUTLKBU......42900
         END IF                                                          OUTLKBU......43000
         IF (INITCOND.OR.(STGB(LK).EQ.-1D98).OR.                         OUTLKBU......43100
     1      (STGBO(LK).EQ.-1D98)) THEN                                   OUTLKBU......43200
            WRITE(CDCTDT,'(1PE15.7)') RNOLK                              OUTLKBU......43300
         ELSE IF ((ISTATO(LK).NE.-3).AND.(ISTAT(LK).EQ.-3)) THEN         OUTLKBU......43400
            WRITE(CDCTDT,'(1PE15.7)') RNOLK                              OUTLKBU......43500
         ELSE                                                            OUTLKBU......43600
            WRITE(CDCTDT,'(1PE15.7)') CUTSML(DCTDT(LK))                  OUTLKBU......43700
         END IF                                                          OUTLKBU......43800
         IF (INITCOND) THEN                                              OUTLKBU......43900
            WRITE(CDUWMDT,'(1PE15.7)') RNOLK                             OUTLKBU......44000
         ELSE                                                            OUTLKBU......44100
            WRITE(CDUWMDT,'(1PE15.7)') CUTSML(DUWMDT(LK))                OUTLKBU......44200
         END IF                                                          OUTLKBU......44300
         WRITE(K19,395) LK,SSYMB(ISTAT(LK)),                             OUTLKBU......44400
     1      CUTSML(UWMS(LK)),CUW,CDUWMDT,CDCTDT                          OUTLKBU......44500
  395    FORMAT(I8,6X,A2,2X,1PE15.7,3(A15))                              OUTLKBU......44600
  400 CONTINUE                                                           OUTLKBU......44700
C                                                                        OUTLKBU......44800
C.....WRITE HEADER FOR OUTPUT OF SOLUTE/ENERGY EXCHANGE RATES.           OUTLKBU......44900
      IF (ME.EQ.-1) THEN                                                 OUTLKBU......45000
         WRITE(K19,791)                                                  OUTLKBU......45100
      ELSE                                                               OUTLKBU......45200
         WRITE(K19,792)                                                  OUTLKBU......45300
      END IF                                                             OUTLKBU......45400
  791 FORMAT(/4X,"Lake",2X,"Status",2X,                                  OUTLKBU......45500
     1      3X," SolMasGw(+)",3X," SolMasGw(-)",3X,"    SolMasGw",       OUTLKBU......45600
     1      3X,"SolMasLak(+)",3X,"SolMasLak(-)",3X,"   SolMasLak",       OUTLKBU......45700
     1      3X,"SolMasExt(+)",3X,"SolMasExt(-)",3X,"   SolMasExt",       OUTLKBU......45800
     1      3X,"   SolMasRun",3X,"   SolMasLim")                         OUTLKBU......45900
  792 FORMAT(/4X,"Lake",2X,"Status",2X,                                  OUTLKBU......46000
     1      3X," EnergyGw(+)",3X," EnergyGw(-)",3X,"    EnergyGw",       OUTLKBU......46100
     1      3X,"EnergyLak(+)",3X,"EnergyLak(-)",3X,"   EnergyLak",       OUTLKBU......46200
     1      3X,"EnergyExt(+)",3X,"EnergyExt(-)",3X,"   EnergyExt",       OUTLKBU......46300
     1      3X,"   EnergyRun",3X,"   EnergyLim")                         OUTLKBU......46400
C                                                                        OUTLKBU......46500
C.....OUTPUT SOLUTE/ENERGY EXCHANGE RATES.                               OUTLKBU......46600
      IF (IT.EQ.0) THEN                                                  OUTLKBU......46700
         GGWG = RNOLK                                                    OUTLKBU......46800
         GGWL = -RNOLK                                                   OUTLKBU......46900
         GGWN = RNOLK                                                    OUTLKBU......47000
         GLKG = RNOLK                                                    OUTLKBU......47100
         GLKL = -RNOLK                                                   OUTLKBU......47200
         GLKN = RNOLK                                                    OUTLKBU......47300
         GEXG = RNOLK                                                    OUTLKBU......47400
         GEXL = -RNOLK                                                   OUTLKBU......47500
         GEXN = RNOLK                                                    OUTLKBU......47600
         GROG = RNOLK                                                    OUTLKBU......47700
         GLLL = -RNOLK                                                   OUTLKBU......47800
      END IF                                                             OUTLKBU......47900
      DO 800 LK=1,NLAKES                                                 OUTLKBU......48000
         IF (IT.NE.0) THEN                                               OUTLKBU......48100
            GGWN = GGWG(LK) - GGWL(LK)                                   OUTLKBU......48200
            GEXN = GEXG(LK) - GEXL(LK)                                   OUTLKBU......48300
            GLKN = GLKG(LK) - GLKL(LK)                                   OUTLKBU......48400
         END IF                                                          OUTLKBU......48500
         WRITE(K19,795) LK,SSYMB(ISTAT(LK)),                             OUTLKBU......48600
     1      CUTSML(GGWG(LK)),CUTSML(-GGWL(LK)),CUTSML(GGWN),             OUTLKBU......48700
     2      CUTSML(GLKG(LK)),CUTSML(-GLKL(LK)),CUTSML(GLKN),             OUTLKBU......48800
     3      CUTSML(GEXG(LK)),CUTSML(-GEXL(LK)),CUTSML(GEXN),             OUTLKBU......48900
     4      CUTSML(GROG(LK)),CUTSML(-GLLL(LK))                           OUTLKBU......49000
  795    FORMAT(I8,6X,A2,2X,11(1PE15.7))                                 OUTLKBU......49100
  800    CONTINUE                                                        OUTLKBU......49200
C                                                                        OUTLKBU......49300
C.....OUTPUT OVERDRAFTS, IF ANY.                                         OUTLKBU......49400
      VOT = 0D0                                                          OUTLKBU......49500
      WMOT = 0D0                                                         OUTLKBU......49600
      SMOT = 0D0                                                         OUTLKBU......49700
      DO 850 LK=1,NLAKES                                                 OUTLKBU......49800
         VOT = VOT + VOVER(LK)                                           OUTLKBU......49900
         WMOT = WMOT + WMOVER(LK)                                        OUTLKBU......50000
         IF (ME.EQ.-1) SMOT = SMOT + SMOVER(LK)                          OUTLKBU......50100
  850 CONTINUE                                                           OUTLKBU......50200
      IF ((VOT.GT.0D0).OR.(SMOT.GT.0D0)) THEN                            OUTLKBU......50300
C........FINISH HEADER FOR OUTPUT OF OVERDRAFTS.                         OUTLKBU......50400
         IF (ME.EQ.-1) THEN                                              OUTLKBU......50500
            WRITE(K19,861)                                               OUTLKBU......50600
  861       FORMAT(/4X,"Lake",2X,"Status",4X," FluVolOver",              OUTLKBU......50700
     2         4X," FluMasOver",4X," SolMasOver")                        OUTLKBU......50800
         ELSE                                                            OUTLKBU......50900
            WRITE(K19,862)                                               OUTLKBU......51000
  862       FORMAT(/4X,"Lake",2X,"Status",4X," FluVolOver",              OUTLKBU......51100
     2         4X," FluMasOver")                                         OUTLKBU......51200
         END IF                                                          OUTLKBU......51300
C........OUTPUT OVERDRAFTS.                                              OUTLKBU......51400
         DO 870 LK=1,NLAKES                                              OUTLKBU......51500
            IF ((VOVER(LK).GT.0D0).OR.(SMOVER(LK).GT.0D0)) THEN          OUTLKBU......51600
               IF (ME.EQ.-1) THEN                                        OUTLKBU......51700
                  WRITE(K19,865) LK,SSYMB(ISTAT(LK)),CUTSML(VOVER(LK)),  OUTLKBU......51800
     1               CUTSML(WMOVER(LK)),CUTSML(SMOVER(LK))               OUTLKBU......51900
               ELSE                                                      OUTLKBU......52000
                  WRITE(K19,865) LK,SSYMB(ISTAT(LK)),CUTSML(VOVER(LK)),  OUTLKBU......52100
     1               CUTSML(WMOVER(LK))                                  OUTLKBU......52200
               END IF                                                    OUTLKBU......52300
  865          FORMAT(I8,6X,A2,2X,3(1PE15.7))                            OUTLKBU......52400
            END IF                                                       OUTLKBU......52500
  870    CONTINUE                                                        OUTLKBU......52600
         IF (ME.EQ.-1) THEN                                              OUTLKBU......52700
            WRITE(K19,873)                                               OUTLKBU......52800
  873       FORMAT("    An overdraft indicates insufficient ",           OUTLKBU......52900
     1         "fluid or solute mass in a lake to meet the demand ",     OUTLKBU......53000
     2         "for outflow. It represents a lake mass-balance error.")  OUTLKBU......53100
         ELSE                                                            OUTLKBU......53200
            WRITE(K19,874)                                               OUTLKBU......53300
  874       FORMAT("    An overdraft indicates insufficient ",           OUTLKBU......53400
     1         "fluid mass in a lake to meet the demand ",               OUTLKBU......53500
     2         "for outflow. It represents a lake mass-balance error.")  OUTLKBU......53600
         END IF                                                          OUTLKBU......53700
      ELSE                                                               OUTLKBU......53800
C........REPORT NO OVERDRAFTS.                                           OUTLKBU......53900
         WRITE(K19,875)                                                  OUTLKBU......54000
  875    FORMAT(/"    No overdrafts.")                                   OUTLKBU......54100
      END IF                                                             OUTLKBU......54200
C                                                                        OUTLKBU......54300
999   RETURN                                                             OUTLKBU......54400
      END                                                                OUTLKBU......54500
C                                                                        OUTLKBU......54600
C     SUBROUTINE        O  U  T  L  K  H           SUTRA VERSION 3.0     OUTLKH.........100
C                                                                        OUTLKH.........200
C *** PURPOSE :                                                          OUTLKH.........300
C ***  TO OUTPUT THE LAKE HIERARCHY.                                     OUTLKH.........400
C                                                                        OUTLKH.........500
      SUBROUTINE OUTLKH(TITLE1,TITLE2)                                   OUTLKH.........600
      USE EXPINT                                                         OUTLKH.........700
      USE SCHDEF                                                         OUTLKH.........800
      USE LARR, ONLY : LCHD, LPAR                                        OUTLKH.........900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                OUTLKH........1000
      CHARACTER*1  TITLE1(80),TITLE2(80)                                 OUTLKH........1100
      CHARACTER*14 CTYPE2                                                OUTLKH........1200
      CHARACTER*80 LAYSTR                                                OUTLKH........1300
      CHARACTER CLINE*50, CNUM*7, CDUM*1                                 OUTLKH........1400
      INTEGER, ALLOCATABLE :: LKOL(:), LKOLP(:)                          OUTLKH........1500
      CHARACTER*3, ALLOCATABLE :: CLKOL(:), CVERT(:)                     OUTLKH........1600
      CHARACTER*1, ALLOCATABLE :: CHORZ(:)                               OUTLKH........1700
      LOGICAL ONCEK5,ONCEK6,ONCEK7,ONCEK8,ONCESTG                        OUTLKH........1800
      DIMENSION KTYPE(2)                                                 OUTLKH........1900
      COMMON /CLAY/ LAYSTR                                               OUTLKH........2000
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  OUTLKH........2100
     1   NPCYC,NUCYC,NPRINT,NBCFPR,NBCSPR,NBCPPR,NBCUPR,NLAKPR,IREAD,    OUTLKH........2200
     1   NBGPPR,NBGUPR,ISTORE,NOUMAT,IUNSAT,IALSAT,KTYPE                 OUTLKH........2300
      COMMON /DIMLAY/ NLAYS,NNLAY,NELAY                                  OUTLKH........2400
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              OUTLKH........2500
     1   NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                                  OUTLKH........2600
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        OUTLKH........2700
      COMMON /DIMX2/ NELTA,NNVEC,NDIMIA,NDIMJA                           OUTLKH........2800
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8,K9,                 OUTLKH........2900
     1   K10,K11,K12,K13,K14,K15,K16,K17,K18,K19,K20,K21,K22,K23         OUTLKH........3000
      COMMON /JCOLS/ NCOLPR,LCOLPR,NCOLS5,NCOLS6,J5COL,J6COL             OUTLKH........3100
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,                OUTLKH........3200
     1   KPANDS,KVEL,KCORT,KBUDG,KSCRN,KPAUSE                            OUTLKH........3300
      COMMON /LEV/ LEVLST                                                OUTLKH........3400
      COMMON /PLT1/ ONCEK5,ONCEK6,ONCEK7,ONCEK8,ONCESTG                  OUTLKH........3500
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           OUTLKH........3600
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       OUTLKH........3700
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITBCS,ITRST,ITMAX,TSTART      OUTLKH........3800
      COMMON /DIMEN/ NLAKES, NSIL, NCEL, NPLK                            OUTLKH........3900
      COMMON /DIMSRF/ MAXCON, NNSURF, NNVERT                             OUTLKH........4000
C                                                                        OUTLKH........4100
C........WRITE HEADER INFORMATION                                        OUTLKH........4200
         WRITE(K23,950) TITLE1, TITLE2                                   OUTLKH........4300
         IF (KTYPE(2).GT.1) THEN                                         OUTLKH........4400
            IF (KTYPE(2).EQ.3) THEN                                      OUTLKH........4500
               CTYPE2 = "BLOCKWISE MESH"                                 OUTLKH........4600
            ELSE                                                         OUTLKH........4700
               CTYPE2 = "REGULAR MESH  "                                 OUTLKH........4800
            END IF                                                       OUTLKH........4900
            IF (KTYPE(1).EQ.3) THEN                                      OUTLKH........5000
               WRITE(K23,951) KTYPE(1),CTYPE2,NN1,NN2,NN3,NN," Nodes",   OUTLKH........5100
     1            NE, " Elems"                                           OUTLKH........5200
            ELSE                                                         OUTLKH........5300
               WRITE(K23,952) KTYPE(1),CTYPE2,NN1,NN2,NN," Nodes",       OUTLKH........5400
     1            NE, " Elems"                                           OUTLKH........5500
            END IF                                                       OUTLKH........5600
         ELSE IF (KTYPE(2).EQ.1) THEN                                    OUTLKH........5700
            WRITE(K23,953) KTYPE(1), LAYSTR(1:6), NLAYS, NNLAY,          OUTLKH........5800
     1         NN, " Nodes", NE, " Elems"                                OUTLKH........5900
         ELSE                                                            OUTLKH........6000
            WRITE(K23,954) KTYPE(1), NN, " Nodes", NE, " Elems"          OUTLKH........6100
         END IF                                                          OUTLKH........6200
         WRITE(K23,960) "LAKE HIERARCHY"                                 OUTLKH........6300
  950    FORMAT("## ", 80A1,                                             OUTLKH........6400
     1         /"## ", 80A1,                                             OUTLKH........6500
     2         /"## ")                                                   OUTLKH........6600
  951    FORMAT("## ", I1, "-D, ", A, 2X,                                OUTLKH........6700
     1                 "(", 2(I9, ")*("), I9, ") = ", I9, A, 1X,         OUTLKH........6800
     2                 "(", I9, A, ")"                                   OUTLKH........6900
     3         /"## ")                                                   OUTLKH........7000
  952    FORMAT("## ", I1, "-D, ", A, 14X,                               OUTLKH........7100
     1                 "(", I9, ")*(", I9, ") = ", I9, A, 1X,            OUTLKH........7200
     2                 "(", I9, A, ")"                                   OUTLKH........7300
     3         /"## ")                                                   OUTLKH........7400
  953    FORMAT("## ", I1, "-D, LAYERED MESH [", A6, "]", 7X,            OUTLKH........7500
     1                 "(", I9, ")*(", I9, ") = ", I9, A, 1X,            OUTLKH........7600
     2                 "(", I9, A, ")"                                   OUTLKH........7700
     3         /"## ")                                                   OUTLKH........7800
  954    FORMAT("## ", I1, "-D, IRREGULAR MESH", 40X, I9, A, 1X,         OUTLKH........7900
     1                 "(", I9, A, ")"                                   OUTLKH........8000
     2         /"## ")                                                   OUTLKH........8100
  960    FORMAT("## ", 92("="),                                          OUTLKH........8200
     1         /"## ", A,                                                OUTLKH........8300
     2         /"## ", 92("="),                                          OUTLKH........8400
     3         /"## ")                                                   OUTLKH........8500
C                                                                        OUTLKH........8600
C.....COMPUTE THE NUMBER OF LEVELS IN THE TREE.                          OUTLKH........8700
      LEVLST = 0                                                         OUTLKH........8800
      DO 200 LK=1,NLAKES                                                 OUTLKH........8900
         IF (LCHD(LK,1).NE.0) CYCLE                                      OUTLKH........9000
         LKK = LK                                                        OUTLKH........9100
         LEVS = 1                                                        OUTLKH........9200
         DO WHILE (LPAR(LKK).NE.1)                                       OUTLKH........9300
            LEVS = LEVS + 1                                              OUTLKH........9400
            LKK = LPAR(LKK)                                              OUTLKH........9500
         END DO                                                          OUTLKH........9600
         LEVS = LEVS + 1                                                 OUTLKH........9700
         LEVLST = MAX(LEVLST, LEVS)                                      OUTLKH........9800
  200 CONTINUE                                                           OUTLKH........9900
C                                                                        OUTLKH.......10000
C.....ALLOCATE LEVEL ARRAYS.                                             OUTLKH.......10100
      NOLMAX = 2**(LEVLST-1)                                             OUTLKH.......10200
      ALLOCATE(LKOL(NOLMAX),LKOLP(NOLMAX))                               OUTLKH.......10300
      ALLOCATE(CLKOL(NOLMAX),CVERT(NOLMAX),CHORZ(NOLMAX))                OUTLKH.......10400
C                                                                        OUTLKH.......10500
C.....LOOP THROUGH LEVELS, PRINTING LAKE NUMBERS.                        OUTLKH.......10600
      LEVEL = 1                                                          OUTLKH.......10700
      NOL = 1                                                            OUTLKH.......10800
      LKOL(1) = 1                                                        OUTLKH.......10900
      CALL BTRLVL(LEVEL,LKOL,CLKOL,CVERT,CHORZ)                          OUTLKH.......11000
      LKOLP = LKOL                                                       OUTLKH.......11100
      DO 700 LEVEL=2,LEVLST                                              OUTLKH.......11200
         NOLP = 2**(LEVEL-2)                                             OUTLKH.......11300
         NOL = 0                                                         OUTLKH.......11400
         NCH = 0                                                         OUTLKH.......11500
         DO 500 N=1,NOLP                                                 OUTLKH.......11600
            LK = LKOLP(N)                                                OUTLKH.......11700
            IF (LK.EQ.0) THEN                                            OUTLKH.......11800
               LKOL(NOL+1) = 0                                           OUTLKH.......11900
               LKOL(NOL+2) = 0                                           OUTLKH.......12000
            ELSE                                                         OUTLKH.......12100
               LKOL(NOL+1) = LCHD(LK,1)                                  OUTLKH.......12200
               LKOL(NOL+2) = LCHD(LK,2)                                  OUTLKH.......12300
               IF (LCHD(LK,1).NE.0) NCH = NCH + 1                        OUTLKH.......12400
            END IF                                                       OUTLKH.......12500
            NOL = NOL + 2                                                OUTLKH.......12600
  500    CONTINUE                                                        OUTLKH.......12700
         IF (NCH.EQ.0) EXIT                                              OUTLKH.......12800
         CALL BTRLVL(LEVEL,LKOL,CLKOL,CVERT,CHORZ)                       OUTLKH.......12900
         LKOLP = LKOL                                                    OUTLKH.......13000
  700 CONTINUE                                                           OUTLKH.......13100
C                                                                        OUTLKH.......13200
C.....DEALLOCATE LEVEL ARRAYS.                                           OUTLKH.......13300
      DEALLOCATE(LKOL,LKOLP)                                             OUTLKH.......13400
      DEALLOCATE(CLKOL,CVERT,CHORZ)                                      OUTLKH.......13500
C                                                                        OUTLKH.......13600
      RETURN                                                             OUTLKH.......13700
      END                                                                OUTLKH.......13800
C                                                                        OUTLKH.......13900
C     SUBROUTINE        O  U  T  L  K  N           SUTRA VERSION 3.0     OUTLKN.........100
C                                                                        OUTLKN.........200
C *** PURPOSE :                                                          OUTLKN.........300
C ***  TO OUTPUT THE LAKE NUMBER FOR EACH SURFACE NODE.                  OUTLKN.........400
C                                                                        OUTLKN.........500
      SUBROUTINE OUTLKN(TITLE1,TITLE2)                                   OUTLKN.........600
      USE EXPINT                                                         OUTLKN.........700
      USE SCHDEF                                                         OUTLKN.........800
      USE LARR                                                           OUTLKN.........900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                OUTLKN........1000
      CHARACTER*1  TITLE1(80),TITLE2(80)                                 OUTLKN........1100
      CHARACTER*15 COLTKK(2)                                             OUTLKN........1200
      CHARACTER*14 CTYPE2                                                OUTLKN........1300
      CHARACTER*80 LAYSTR                                                OUTLKN........1400
      LOGICAL ONCEK5,ONCEK6,ONCEK7,ONCEK8,ONCESTG                        OUTLKN........1500
      DIMENSION VVAR(2)                                                  OUTLKN........1600
      DIMENSION KTYPE(2)                                                 OUTLKN........1700
      ALLOCATABLE TT(:),ITT(:)                                           OUTLKN........1800
      COMMON /CLAY/ LAYSTR                                               OUTLKN........1900
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  OUTLKN........2000
     1   NPCYC,NUCYC,NPRINT,NBCFPR,NBCSPR,NBCPPR,NBCUPR,NLAKPR,IREAD,    OUTLKN........2100
     1   NBGPPR,NBGUPR,ISTORE,NOUMAT,IUNSAT,IALSAT,KTYPE                 OUTLKN........2200
      COMMON /DIMLAY/ NLAYS,NNLAY,NELAY                                  OUTLKN........2300
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              OUTLKN........2400
     1   NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                                  OUTLKN........2500
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        OUTLKN........2600
      COMMON /DIMX2/ NELTA,NNVEC,NDIMIA,NDIMJA                           OUTLKN........2700
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8,K9,                 OUTLKN........2800
     1   K10,K11,K12,K13,K14,K15,K16,K17,K18,K19,K20,K21,K22,K23         OUTLKN........2900
      COMMON /GRAVEC/ GRAVX,GRAVY,GRAVZ                                  OUTLKN........3000
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      OUTLKN........3100
      COMMON /JCOLS/ NCOLPR,LCOLPR,NCOLS5,NCOLS6,J5COL,J6COL             OUTLKN........3200
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,                OUTLKN........3300
     1   KPANDS,KVEL,KCORT,KBUDG,KSCRN,KPAUSE                            OUTLKN........3400
      COMMON /PLT1/ ONCEK5,ONCEK6,ONCEK7,ONCEK8,ONCESTG                  OUTLKN........3500
      COMMON /SCH/ NSCH,ISCHTS,NSCHAU                                    OUTLKN........3600
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           OUTLKN........3700
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       OUTLKN........3800
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITBCS,ITRST,ITMAX,TSTART      OUTLKN........3900
      COMMON /DIMEN/ NLAKES, NSIL, NCEL, NPLK                            OUTLKN........4000
      COMMON /DIMSRF/ MAXCON, NNSURF, NNVERT                             OUTLKN........4100
      COMMON /VALLAK/ RNOLK                                              OUTLKN........4200
      DATA (COLTKK(MM), MM=1,2) /'Node', '           Lake'/              OUTLKN........4300
      SAVE COLTKK                                                        OUTLKN........4400
C                                                                        OUTLKN........4500
C........WRITE HEADER INFORMATION                                        OUTLKN........4600
         WRITE(K22,950) TITLE1, TITLE2                                   OUTLKN........4700
         IF (KTYPE(2).GT.1) THEN                                         OUTLKN........4800
            IF (KTYPE(2).EQ.3) THEN                                      OUTLKN........4900
               CTYPE2 = "BLOCKWISE MESH"                                 OUTLKN........5000
            ELSE                                                         OUTLKN........5100
               CTYPE2 = "REGULAR MESH  "                                 OUTLKN........5200
            END IF                                                       OUTLKN........5300
            IF (KTYPE(1).EQ.3) THEN                                      OUTLKN........5400
               WRITE(K22,951) KTYPE(1),CTYPE2,NN1,NN2,NN3,NN," Nodes",   OUTLKN........5500
     1            NE, " Elems"                                           OUTLKN........5600
            ELSE                                                         OUTLKN........5700
               WRITE(K22,952) KTYPE(1),CTYPE2,NN1,NN2,NN," Nodes",       OUTLKN........5800
     1            NE, " Elems"                                           OUTLKN........5900
            END IF                                                       OUTLKN........6000
         ELSE IF (KTYPE(2).EQ.1) THEN                                    OUTLKN........6100
            WRITE(K22,953) KTYPE(1), LAYSTR(1:6), NLAYS, NNLAY,          OUTLKN........6200
     1         NN, " Nodes", NE, " Elems"                                OUTLKN........6300
         ELSE                                                            OUTLKN........6400
            WRITE(K22,954) KTYPE(1), NN, " Nodes", NE, " Elems"          OUTLKN........6500
         END IF                                                          OUTLKN........6600
         WRITE(K22,960) "LAKE NUMBER FOR EACH LAKE NODE"                 OUTLKN........6700
  950    FORMAT("## ", 80A1,                                             OUTLKN........6800
     1         /"## ", 80A1,                                             OUTLKN........6900
     2         /"## ")                                                   OUTLKN........7000
  951    FORMAT("## ", I1, "-D, ", A, 2X,                                OUTLKN........7100
     1                 "(", 2(I9, ")*("), I9, ") = ", I9, A, 1X,         OUTLKN........7200
     2                 "(", I9, A, ")"                                   OUTLKN........7300
     3         /"## ")                                                   OUTLKN........7400
  952    FORMAT("## ", I1, "-D, ", A, 14X,                               OUTLKN........7500
     1                 "(", I9, ")*(", I9, ") = ", I9, A, 1X,            OUTLKN........7600
     2                 "(", I9, A, ")"                                   OUTLKN........7700
     3         /"## ")                                                   OUTLKN........7800
  953    FORMAT("## ", I1, "-D, LAYERED MESH [", A6, "]", 7X,            OUTLKN........7900
     1                 "(", I9, ")*(", I9, ") = ", I9, A, 1X,            OUTLKN........8000
     2                 "(", I9, A, ")"                                   OUTLKN........8100
     3         /"## ")                                                   OUTLKN........8200
  954    FORMAT("## ", I1, "-D, IRREGULAR MESH", 40X, I9, A, 1X,         OUTLKN........8300
     1                 "(", I9, A, ")"                                   OUTLKN........8400
     2         /"## ")                                                   OUTLKN........8500
  960    FORMAT("## ", 92("="),                                          OUTLKN........8600
     1         /"## ", A,                                                OUTLKN........8700
     2         /"## ", 92("="),                                          OUTLKN........8800
     3         /"## ")                                                   OUTLKN........8900
C                                                                        OUTLKN........9000
C........HEADER.                                                         OUTLKN........9100
         WRITE(K22,968) (COLTKK(M), M=1,2)                               OUTLKN........9200
  968    FORMAT ("## ", 2X, A4, 2(A15))                                  OUTLKN........9300
C                                                                        OUTLKN........9400
C.....LAKE NUMBER FOR EACH LAKE NODE.                                    OUTLKN........9500
      DO 978 ISURF=1,NNSURF                                              OUTLKN........9600
         LAKE = LAKNOD(ISURF)%KLUSTR                                     OUTLKN........9700
         I = LAKNOD(ISURF)%INODE                                         OUTLKN........9800
         WRITE(K22,975) I, LAKE                                          OUTLKN........9900
  975    FORMAT (I9, 6X, I9)                                             OUTLKN.......10000
  978 CONTINUE                                                           OUTLKN.......10100
C                                                                        OUTLKN.......10200
      RETURN                                                             OUTLKN.......10300
C                                                                        OUTLKN.......10400
      END                                                                OUTLKN.......10500
C                                                                        OUTLKN.......10600
C     SUBROUTINE        O  U  T  L  K  R  S        SUTRA VERSION 3.0     OUTLKRS........100
C                                                                        OUTLKRS........200
C *** PURPOSE :                                                          OUTLKRS........300
C ***  TO WRITE LAKE INFORMATION NEEDED FOR RESTART.                     OUTLKRS........400
C                                                                        OUTLKRS........500
      SUBROUTINE OUTLKRS()                                               OUTLKRS........600
      USE LARR                                                           OUTLKRS........700
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               OUTLKRS........800
      LOGICAL LAKUP                                                      OUTLKRS........900
      COMMON /BUDO/ TSECO,LUTSO                                          OUTLKRS.......1000
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  OUTLKRS.......1100
     1   NPCYC,NUCYC,NPRINT,NBCFPR,NBCSPR,NBCPPR,NBCUPR,NLAKPR,IREAD,    OUTLKRS.......1200
     2   NBGPPR,NBGUPR,ISTORE,NOUMAT,IUNSAT,IALSAT,KTYPE                 OUTLKRS.......1300
      COMMON /DIMEN/ NLAKES, NSIL, NCEL, NPLK                            OUTLKRS.......1400
      COMMON /DIMSRF/ MAXCON, NNSURF, NNVERT                             OUTLKRS.......1500
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8,K9,                 OUTLKRS.......1600
     1   K10,K11,K12,K13,K14,K15,K16,K17,K18,K19,K20,K21,K22,K23         OUTLKRS.......1700
      COMMON /LUP/ LAKUP                                                 OUTLKRS.......1800
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      OUTLKRS.......1900
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        OUTLKRS.......2000
      COMMON /RHOLAK/ RHOLK                                              OUTLKRS.......2100
      COMMON /RISSAV/ FRROD,FDROD                                        OUTLKRS.......2200
      COMMON /VALLAK/ RNOLK                                              OUTLKRS.......2300
C                                                                        OUTLKRS.......2400
C.....WRITE INFORMATION NEEDED FOR RESTART.  (STARTING TIME IS           OUTLKRS.......2500
C        DETERMINED FROM THE RST FILE AND IS NOT REPEATED HERE.)         OUTLKRS.......2600
C                                                                        OUTLKRS.......2700
C.....WRITE PARAMETERS AND NUMBER OF SPECIFICATIONS.                     OUTLKRS.......2800
      WRITE(K21,332) NLAKPR                                              OUTLKRS.......2900
      WRITE(K21,333) NLAKES, FRROD, FDROD, RNOLK                         OUTLKRS.......3000
  332 FORMAT(3X,2I8,2(2X,1PE23.16))                                      OUTLKRS.......3100
  333 FORMAT(3X,I8,4(2X,1PE23.16))                                       OUTLKRS.......3200
C                                                                        OUTLKRS.......3300
C.....WRITE INITIAL SPECIFICATIONS.                                      OUTLKRS.......3400
      DO 800 LK=1,NLAKES                                                 OUTLKRS.......3500
         STGI = STGLAK(LK)                                               OUTLKRS.......3600
         UWI = UW(LK)/CW                                                 OUTLKRS.......3700
         WRITE(K21,777) "'LAKE'", LK, CUTSML(STGI), CUTSML(UWI),         OUTLKRS.......3800
     1      FRRO(LK), FDRO(LK)                                           OUTLKRS.......3900
  777 FORMAT(3X,A6,3X,I8,4(2X,1PE23.16))                                 OUTLKRS.......4000
  800 CONTINUE                                                           OUTLKRS.......4100
C                                                                        OUTLKRS.......4200
C.....WRITE INFORMATION TO BE READ ONLY DURING WARM RESTART.             OUTLKRS.......4300
      WRITE(K21,852) LUTSO,TSECO,LAKUP                                   OUTLKRS.......4400
      DO 850 LK=1,NLAKES                                                 OUTLKRS.......4500
         WRITE(K21,854) ISTATO(LK),CUTSML(VOLWO(LK)),                    OUTLKRS.......4600
     1      CUTSML(STGBO(LK)),CUTSML(UWO(LK)),CUTSML(UWMSO(LK))          OUTLKRS.......4700
         WRITE(K21,854) ISTAT(LK),CUTSML(VOLW(LK)),                      OUTLKRS.......4800
     1      CUTSML(STGB(LK)),CUTSML(UW(LK)),CUTSML(UWMS(LK))             OUTLKRS.......4900
         WRITE(K21,855) CUTSML(FGWG(LK)),CUTSML(FEXG(LK)),               OUTLKRS.......5000
     1      CUTSML(FLKG(LK)),CUTSML(FROG(LK))                            OUTLKRS.......5100
         WRITE(K21,855) CUTSML(FGWL(LK)),CUTSML(FEXL(LK)),               OUTLKRS.......5200
     1      CUTSML(FLKL(LK)),CUTSML(FLLL(LK))                            OUTLKRS.......5300
         WRITE(K21,855) CUTSML(GGWG(LK)),CUTSML(GEXG(LK)),               OUTLKRS.......5400
     1      CUTSML(GLKG(LK)),CUTSML(GROG(LK))                            OUTLKRS.......5500
         WRITE(K21,855) CUTSML(GGWL(LK)),CUTSML(GEXL(LK)),               OUTLKRS.......5600
     1      CUTSML(GLKL(LK)),CUTSML(GLLL(LK))                            OUTLKRS.......5700
  850 CONTINUE                                                           OUTLKRS.......5800
  852 FORMAT(3X,3X,I8,2X,1PE23.16,L4)                                    OUTLKRS.......5900
  854 FORMAT(6X,I8,6(2X,1PE23.16))                                       OUTLKRS.......6000
  855 FORMAT(6X,4(2X,1PE23.16))                                          OUTLKRS.......6100
      DO 890 ISURF=1,NNSURF                                              OUTLKRS.......6200
         ILN = LAKNOD(ISURF)%INODE                                       OUTLKRS.......6300
         WRITE(K21,895) ISLAKE(ILN),CUTSML(PLK(ISURF)),                  OUTLKRS.......6400
     1      CUTSML(ULK(ISURF))                                           OUTLKRS.......6500
  890 CONTINUE                                                           OUTLKRS.......6600
  895 FORMAT(10X,L4,2(2X,1PE23.16))                                      OUTLKRS.......6700
C                                                                        OUTLKRS.......6800
  900 RETURN                                                             OUTLKRS.......6900
      END                                                                OUTLKRS.......7000
C                                                                        OUTLKRS.......7100
C     SUBROUTINE        O  U  T  L  K  S  T        SUTRA VERSION 3.0     OUTLKST........100
C                                                                        OUTLKST........200
C *** PURPOSE :                                                          OUTLKST........300
C ***  TO OUTPUT LAKE STAGES AND DEPTHS.                                 OUTLKST........400
C                                                                        OUTLKST........500
      SUBROUTINE OUTLKST(TITLE1,TITLE2)                                  OUTLKST........600
      USE EXPINT                                                         OUTLKST........700
      USE SCHDEF                                                         OUTLKST........800
      USE LARR                                                           OUTLKST........900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                OUTLKST.......1000
      CHARACTER*1  TITLE1(80),TITLE2(80)                                 OUTLKST.......1100
      CHARACTER*15 COLTKK(3)                                             OUTLKST.......1200
      CHARACTER*14 CTYPE2                                                OUTLKST.......1300
      CHARACTER*80 LAYSTR                                                OUTLKST.......1400
      LOGICAL ONCEK5,ONCEK6,ONCEK7,ONCEK8,ONCESTG                        OUTLKST.......1500
      DIMENSION VVAR(3)                                                  OUTLKST.......1600
      DIMENSION KTYPE(2)                                                 OUTLKST.......1700
      ALLOCATABLE TT(:),ITT(:)                                           OUTLKST.......1800
      COMMON /CLAY/ LAYSTR                                               OUTLKST.......1900
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  OUTLKST.......2000
     1   NPCYC,NUCYC,NPRINT,NBCFPR,NBCSPR,NBCPPR,NBCUPR,NLAKPR,IREAD,    OUTLKST.......2100
     1   NBGPPR,NBGUPR,ISTORE,NOUMAT,IUNSAT,IALSAT,KTYPE                 OUTLKST.......2200
      COMMON /DIMLAY/ NLAYS,NNLAY,NELAY                                  OUTLKST.......2300
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              OUTLKST.......2400
     1   NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                                  OUTLKST.......2500
      COMMON /DIMX/ NWI,NWF,NWL,NELT,NNNX,NEX,N48                        OUTLKST.......2600
      COMMON /DIMX2/ NELTA,NNVEC,NDIMIA,NDIMJA                           OUTLKST.......2700
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8,K9,                 OUTLKST.......2800
     1   K10,K11,K12,K13,K14,K15,K16,K17,K18,K19,K20,K21,K22,K23         OUTLKST.......2900
      COMMON /GRAVEC/ GRAVX,GRAVY,GRAVZ                                  OUTLKST.......3000
      COMMON /ITERAT/ RPM,RPMAX,RUM,RUMAX,ITER,ITRMAX,IPWORS,IUWORS      OUTLKST.......3100
      COMMON /JCOLS/ NCOLPR,LCOLPR,NCOLS5,NCOLS6,J5COL,J6COL             OUTLKST.......3200
      COMMON /KPRINT/ KNODAL,KELMNT,KINCID,KPLOTP,KPLOTU,                OUTLKST.......3300
     1   KPANDS,KVEL,KCORT,KBUDG,KSCRN,KPAUSE                            OUTLKST.......3400
      COMMON /PLT1/ ONCEK5,ONCEK6,ONCEK7,ONCEK8,ONCESTG                  OUTLKST.......3500
      COMMON /SCH/ NSCH,ISCHTS,NSCHAU                                    OUTLKST.......3600
      COMMON /SOLVI/ KSOLVP,KSOLVU,NN1,NN2,NN3                           OUTLKST.......3700
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       OUTLKST.......3800
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITBCS,ITRST,ITMAX,TSTART      OUTLKST.......3900
      COMMON /DIMEN/ NLAKES, NSIL, NCEL, NPLK                            OUTLKST.......4000
      COMMON /DIMSRF/ MAXCON, NNSURF, NNVERT                             OUTLKST.......4100
      COMMON /VALLAK/ RNOLK                                              OUTLKST.......4200
      DATA (COLTKK(MM), MM=1,3) /'Node',                                 OUTLKST.......4300
     2   '          Stage', '          Depth'/                           OUTLKST.......4400
      SAVE COLTKK                                                        OUTLKST.......4500
C                                                                        OUTLKST.......4600
C.....CALCULATE HEADERS ON FIRST CALL AND CREATE OUTPUT ON EACH CALL.    OUTLKST.......4700
C                                                                        OUTLKST.......4800
C                                                                        OUTLKST.......4900
      IF (.NOT. ONCESTG)  THEN                                           OUTLKST.......5000
C.....FIRST CALL -- CREATE FILE HEADER.                                  OUTLKST.......5100
C                                                                        OUTLKST.......5200
C........CALCULATE THE MAXIMUM NUMBER OF TIME STEPS, KTMAX.              OUTLKST.......5300
         IF (ISSTRA.NE.1) THEN                                           OUTLKST.......5400
            KT = 1                                                       OUTLKST.......5500
         ELSE                                                            OUTLKST.......5600
            KT = 0                                                       OUTLKST.......5700
         END IF                                                          OUTLKST.......5800
         DO 4 JT=1,ITMAX                                                 OUTLKST.......5900
            IF (MOD(JT,NLAKPR).EQ.0 .OR. JT.EQ.ITRST .OR.                OUTLKST.......6000
     1         ((JT.EQ.ITRST+1).AND.((ISSTRA.NE.0).OR.(NLAKPR.GT.0))))   OUTLKST.......6100
     2         KT = KT + 1                                               OUTLKST.......6200
    4    CONTINUE                                                        OUTLKST.......6300
         IF(ITMAX.GT.1 .AND. MOD(ITMAX,NLAKPR).NE.0) KT = KT + 1         OUTLKST.......6400
         KTMAX = KT                                                      OUTLKST.......6500
C                                                                        OUTLKST.......6600
C........ALLOCATE LOCAL ARRAYS                                           OUTLKST.......6700
         ALLOCATE(TT(KTMAX),ITT(KTMAX))                                  OUTLKST.......6800
C                                                                        OUTLKST.......6900
C........CALCULATE AND PRINT TIME STEP INFORMATION                       OUTLKST.......7000
         TS=TSTART                                                       OUTLKST.......7100
C........TIME STEP VALUE                                                 OUTLKST.......7200
         JT=0                                                            OUTLKST.......7300
C........NUMBER OF PRINTED TIME STEPS                                    OUTLKST.......7400
         KT=0                                                            OUTLKST.......7500
C........TIME STEP INCREMENT                                             OUTLKST.......7600
         DELTK=DELT                                                      OUTLKST.......7700
C........INDICATORS OF WHEN VARIABLES ARE CALCULATED AND PRINTED         OUTLKST.......7800
         IF (ISSTRA.NE.1) THEN                                           OUTLKST.......7900
            KT = KT + 1                                                  OUTLKST.......8000
            TT(KT) = TS                                                  OUTLKST.......8100
            ITT(KT) = JT                                                 OUTLKST.......8200
         END IF                                                          OUTLKST.......8300
         DO 10 JT=1,ITMAX                                                OUTLKST.......8400
            JTP1 = JT + 1                                                OUTLKST.......8500
            TS = SCHDLS(ISCHTS)%SLIST(JTP1)%DVALU1                       OUTLKST.......8600
            IF (MOD(JT,NLAKPR).EQ.0 .OR. JT.EQ.ITRST .OR.                OUTLKST.......8700
     1         ((JT.EQ.ITRST+1).AND.((ISSTRA.NE.0).OR.(NLAKPR.GT.0))))   OUTLKST.......8800
     2         THEN                                                      OUTLKST.......8900
               KT = KT + 1                                               OUTLKST.......9000
               TT(KT) = TS                                               OUTLKST.......9100
               ITT(KT) = JT                                              OUTLKST.......9200
            ENDIF                                                        OUTLKST.......9300
   10    CONTINUE                                                        OUTLKST.......9400
         IF (ISSTRA.EQ.1) TT(KT) = TSTART                                OUTLKST.......9500
C                                                                        OUTLKST.......9600
C                                                                        OUTLKST.......9700
C........PRINT LAST TIME STEP ALWAYS, UNLESS ALREADY PRINTED             OUTLKST.......9800
         IF(ITMAX.GT.1 .AND. MOD(ITMAX,NLAKPR).NE.0) THEN                OUTLKST.......9900
            KT = KT + 1                                                  OUTLKST......10000
            TT(KT) = TS                                                  OUTLKST......10100
            ITT(KT) = ITMAX                                              OUTLKST......10200
         ENDIF                                                           OUTLKST......10300
C                                                                        OUTLKST......10400
C........COMPUTE ACTUAL NUMBER OF PRINTED TIME STEPS, KTPRN; LESS THAN   OUTLKST......10500
C           KTMAX IF RUN IS A RESTART                                    OUTLKST......10600
         IF (IREAD.EQ.+1) THEN                                           OUTLKST......10700
            KTPRN = KTMAX                                                OUTLKST......10800
         ELSE                                                            OUTLKST......10900
            KTPRN = 0                                                    OUTLKST......11000
            DO 17 KT=1,KTMAX                                             OUTLKST......11100
               IF (ITT(KT).GE.ITRST) KTPRN = KTPRN + 1                   OUTLKST......11200
   17       CONTINUE                                                     OUTLKST......11300
         END IF                                                          OUTLKST......11400
C                                                                        OUTLKST......11500
C........WRITE HEADER INFORMATION                                        OUTLKST......11600
         WRITE(K20,950) TITLE1, TITLE2                                   OUTLKST......11700
         IF (KTYPE(2).GT.1) THEN                                         OUTLKST......11800
            IF (KTYPE(2).EQ.3) THEN                                      OUTLKST......11900
               CTYPE2 = "BLOCKWISE MESH"                                 OUTLKST......12000
            ELSE                                                         OUTLKST......12100
               CTYPE2 = "REGULAR MESH  "                                 OUTLKST......12200
            END IF                                                       OUTLKST......12300
            IF (KTYPE(1).EQ.3) THEN                                      OUTLKST......12400
               WRITE(K20,951) KTYPE(1),CTYPE2,NN1,NN2,NN3,NN," Nodes",   OUTLKST......12500
     1            NE, " Elems"                                           OUTLKST......12600
            ELSE                                                         OUTLKST......12700
               WRITE(K20,952) KTYPE(1),CTYPE2,NN1,NN2,NN," Nodes",       OUTLKST......12800
     1            NE, " Elems"                                           OUTLKST......12900
            END IF                                                       OUTLKST......13000
         ELSE IF (KTYPE(2).EQ.1) THEN                                    OUTLKST......13100
            WRITE(K20,953) KTYPE(1), LAYSTR(1:6), NLAYS, NNLAY,          OUTLKST......13200
     1         NN, " Nodes", NE, " Elems"                                OUTLKST......13300
         ELSE                                                            OUTLKST......13400
            WRITE(K20,954) KTYPE(1), NN, " Nodes", NE, " Elems"          OUTLKST......13500
         END IF                                                          OUTLKST......13600
         WRITE(K20,960) "LAKE STAGE AND DEPTH RESULTS", KTPRN            OUTLKST......13700
         DO 920 KT=1,KTMAX                                               OUTLKST......13800
            IF (ITT(KT).GE.ITRST)                                        OUTLKST......13900
     1         WRITE(K20,961) ITT(KT), TT(KT)                            OUTLKST......14000
  920    CONTINUE                                                        OUTLKST......14100
  950    FORMAT("## ", 80A1,                                             OUTLKST......14200
     1         /"## ", 80A1,                                             OUTLKST......14300
     2         /"## ")                                                   OUTLKST......14400
  951    FORMAT("## ", I1, "-D, ", A, 2X,                                OUTLKST......14500
     1                 "(", 2(I9, ")*("), I9, ") = ", I9, A, 1X,         OUTLKST......14600
     2                 "(", I9, A, ")"                                   OUTLKST......14700
     3         /"## ")                                                   OUTLKST......14800
  952    FORMAT("## ", I1, "-D, ", A, 14X,                               OUTLKST......14900
     1                 "(", I9, ")*(", I9, ") = ", I9, A, 1X,            OUTLKST......15000
     2                 "(", I9, A, ")"                                   OUTLKST......15100
     3         /"## ")                                                   OUTLKST......15200
  953    FORMAT("## ", I1, "-D, LAYERED MESH [", A6, "]", 7X,            OUTLKST......15300
     1                 "(", I9, ")*(", I9, ") = ", I9, A, 1X,            OUTLKST......15400
     2                 "(", I9, A, ")"                                   OUTLKST......15500
     3         /"## ")                                                   OUTLKST......15600
  954    FORMAT("## ", I1, "-D, IRREGULAR MESH", 40X, I9, A, 1X,         OUTLKST......15700
     1                 "(", I9, A, ")"                                   OUTLKST......15800
     2         /"## ")                                                   OUTLKST......15900
  960    FORMAT("## ", 92("="),                                          OUTLKST......16000
     1         /"## ", A, 36X, I9, " Time steps printed",                OUTLKST......16100
     2         /"## ", 92("="),                                          OUTLKST......16200
     3         /"## ",                                                   OUTLKST......16300
     4         /"## ", 4X, "Time steps"                                  OUTLKST......16400
     6         /"## ", 3X, "in this file      Time (sec)"                OUTLKST......16500
     8         /"## ", 2X, 14("-"), 3X, 13("-"))                         OUTLKST......16600
  961    FORMAT ("## ", 7X, I8, 4X, 1PE13.6, 3(5X, A1, 1X, I8))          OUTLKST......16700
C                                                                        OUTLKST......16800
C........DEALLOCATE LOCAL ARRAYS.                                        OUTLKST......16900
         DEALLOCATE(TT,ITT)                                              OUTLKST......17000
C                                                                        OUTLKST......17100
         ONCESTG = .TRUE.                                                OUTLKST......17200
      ENDIF                                                              OUTLKST......17300
C                                                                        OUTLKST......17400
C.....NODEWISE HEADER INFORMATION REPEATED BEFORE EACH TIME STEP         OUTLKST......17500
      IF ((IT.EQ.0).OR.((IT.EQ.1).AND.(ISSTRA.EQ.1))) THEN               OUTLKST......17600
         DURN = 0D0                                                      OUTLKST......17700
         TOUT = TSTART                                                   OUTLKST......17800
      ELSE                                                               OUTLKST......17900
         DURN = DELT                                                     OUTLKST......18000
         TOUT = TSEC                                                     OUTLKST......18100
      END IF                                                             OUTLKST......18200
      WRITE(K20,966) IT, DURN, TOUT                                      OUTLKST......18300
  966 FORMAT('## ',                                                      OUTLKST......18400
     1      /'## ', 92('='),                                             OUTLKST......18500
     2      /'## TIME STEP ', I8, 22X, 'Duration: ', 1PE11.4, ' sec',    OUTLKST......18600
     3                            6X, 'Time: ', 1PE11.4, ' sec',         OUTLKST......18700
     4      /'## ', 92('='))                                             OUTLKST......18800
         WRITE(K20,968) (COLTKK(M), M=1,3)                               OUTLKST......18900
  968    FORMAT ("## ", 2X, A4, 2(A15))                                  OUTLKST......19000
C                                                                        OUTLKST......19100
C.....LAKE STAGE AND DEPTH DATA FOR THIS TIME STEP                       OUTLKST......19200
      DO 978 ISURF=1,NNSURF                                              OUTLKST......19300
         LAKE = LAKNOD(ISURF)%KLUSTR                                     OUTLKST......19400
         I = LAKNOD(ISURF)%INODE                                         OUTLKST......19500
         ELV = LAKNOD(ISURF)%ELEV                                        OUTLKST......19600
         IF (ISLAKE(I)) THEN                                             OUTLKST......19700
            STG = STGLAK(LAKE)                                           OUTLKST......19800
            DEPTH = STG - ELV                                            OUTLKST......19900
         ELSE                                                            OUTLKST......20000
            STG = RNOLK                                                  OUTLKST......20100
            DEPTH = RNOLK                                                OUTLKST......20200
         END IF                                                          OUTLKST......20300
         VVAR(2) = STG                                                   OUTLKST......20400
         VVAR(3) = DEPTH                                                 OUTLKST......20500
         WRITE(K20,975) I,(CUTSML(VVAR(M)), M=2,3)                       OUTLKST......20600
  975    FORMAT (I9, 19(1PE15.7))                                        OUTLKST......20700
  978 CONTINUE                                                           OUTLKST......20800
C                                                                        OUTLKST......20900
      RETURN                                                             OUTLKST......21000
C                                                                        OUTLKST......21100
      END                                                                OUTLKST......21200
C                                                                        OUTLKST......21300
C     SUBROUTINE        R  E  N  U  M  B           SUTRA VERSION 3.0     RENUMB.........100
C                                                                        RENUMB.........200
C *** PURPOSE :                                                          RENUMB.........300
C ***  TO RENUMBER THE LAKES SO THAT NO LEVEL OF THE LAKE HIERARCHY      RENUMB.........400
C ***  CONTAINS A LOWER LAKE NUMBER THAN DOES THE LEVEL ABOVE.           RENUMB.........500
C                                                                        RENUMB.........600
      SUBROUTINE RENUMB(LCHD0)                                           RENUMB.........700
      USE LARR                                                           RENUMB.........800
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               RENUMB.........900
      INTEGER LCHD0(999,2), LEV(999), LOLD(999)                          RENUMB........1000
      INTEGER RMVDIM,IMVDIM,I1VDIM,CMVDIM,LMVDIM,PMVDIM                  RENUMB........1100
      COMMON /DIMEN/ NLAKES, NSIL, NCEL, NPLK                            RENUMB........1200
      COMMON /DIMSRF/ MAXCON, NNSURF, NNVERT                             RENUMB........1300
      COMMON /MEMCNT/ RMVDIM,IMVDIM,I1VDIM,CMVDIM,LMVDIM,PMVDIM          RENUMB........1400
C                                                                        RENUMB........1500
C.....INITIALIZE LEVELS AND PARENT INFO.                                 RENUMB........1600
      ALLOCATE(LPAR(NLAKES))                                             RENUMB........1700
      IMVDIM = IMVDIM + NLAKES                                           RENUMB........1800
      DO 100 LAKE=1,NLAKES                                               RENUMB........1900
         LEV(LAKE) = 0                                                   RENUMB........2000
         IF (LCHD0(LAKE,1).NE.0) THEN                                    RENUMB........2100
            LPAR(LCHD0(LAKE,1)) = LAKE                                   RENUMB........2200
            LPAR(LCHD0(LAKE,2)) = LAKE                                   RENUMB........2300
         END IF                                                          RENUMB........2400
  100 CONTINUE                                                           RENUMB........2500
      LEV(1) = 1                                                         RENUMB........2600
      LPAR(1) = 1                                                        RENUMB........2700
C                                                                        RENUMB........2800
C.....COMPUTE LEVELS BY STARTING AT THE TERMINAL BRANCHES OF THE TREE    RENUMB........2900
C        (I.E., CHILDLESS LAKES) AND MOVING UP THE TREE COMPUTING        RENUMB........3000
C        RELATIVE LEVELS, THEN ADJUSTING THEM TO ABSOLUTE LEVELS ONCE    RENUMB........3100
C        A LAKE AT A KNOWN LEVEL IS ENCOUNTERED.                         RENUMB........3200
      DO 300 LAKE=1,NLAKES                                               RENUMB........3300
C........IF LAKE HAS CHILDREN, SKIP IT.                                  RENUMB........3400
         IF (LCHD0(LAKE,1).NE.0) CYCLE                                   RENUMB........3500
C........WHILE THE LEVEL IS UNKNOWN (ZERO), KEEP MOVING UP THE TREE,     RENUMB........3600
C           KEEPING TRACK OF RELATIVE LEVELS.                            RENUMB........3700
         LL = LAKE                                                       RENUMB........3800
         LEVLL = 0                                                       RENUMB........3900
         DO WHILE (LEV(LPAR(LL)).EQ.0)                                   RENUMB........4000
            LEVLL = LEVLL - 1                                            RENUMB........4100
            LEV(LPAR(LL)) = LEVLL                                        RENUMB........4200
            LL = LPAR(LL)                                                RENUMB........4300
         END DO                                                          RENUMB........4400
C........A KNOWN LEVEL HAS BEEN ENCOUNTERED.  GO BACK AND ADJUST THE     RENUMB........4500
C          RELATIVE LEVELS TO ABSOLUTE LEVELS CONSISTENT WITH THE KNOWN  RENUMB........4600
C          LEVEL.                                                        RENUMB........4700
         LLPAR1 = LPAR(LL)                                               RENUMB........4800
         LEVLL = LEVLL - 1                                               RENUMB........4900
         LEVOFF = LEV(LLPAR1) - LEVLL                                    RENUMB........5000
         LL = LAKE                                                       RENUMB........5100
         DO WHILE (LL.NE.LLPAR1)                                         RENUMB........5200
            LEV(LL) = LEV(LL) + LEVOFF                                   RENUMB........5300
            LL = LPAR(LL)                                                RENUMB........5400
         END DO                                                          RENUMB........5500
  300 CONTINUE                                                           RENUMB........5600
C                                                                        RENUMB........5700
C.....SORT THE LAKES BY LEVEL.                                           RENUMB........5800
      DO 500 LAKE=1,NLAKES                                               RENUMB........5900
         LOLD(LAKE) = LAKE                                               RENUMB........6000
  500 CONTINUE                                                           RENUMB........6100
      DO 550 LAKE=1,NLAKES-1                                             RENUMB........6200
         DO 540 LL=LAKE,NLAKES                                           RENUMB........6300
            IF (LEV(LOLD(LL)).LT.LEV(LOLD(LAKE))) THEN                   RENUMB........6400
               LDUM = LOLD(LAKE)                                         RENUMB........6500
               LOLD(LAKE) = LOLD(LL)                                     RENUMB........6600
               LOLD(LL) = LDUM                                           RENUMB........6700
             END IF                                                      RENUMB........6800
  540    CONTINUE                                                        RENUMB........6900
  550 CONTINUE                                                           RENUMB........7000
C                                                                        RENUMB........7100
C.....AND USE THE SORTED ORDER AS THE NEW NUMBERING.  CONVERT            RENUMB........7200
C        PARENT/CHILD/SIBLING INFO AND CLUSTER (LAKE) NUMBERS.           RENUMB........7300
      ALLOCATE(LCHD(NLAKES,2),LSIB(NLAKES))                              RENUMB........7400
      IMVDIM = IMVDIM + 3*NLAKES                                         RENUMB........7500
      LSIB(1) = 0                                                        RENUMB........7600
      DO 700 LAKE=1,NLAKES                                               RENUMB........7700
         LCHD(LAKE,1) = LCHD0(LOLD(LAKE),1)                              RENUMB........7800
         LCHD(LAKE,2) = LCHD0(LOLD(LAKE),2)                              RENUMB........7900
         IF (LCHD(LAKE,1).NE.0) THEN                                     RENUMB........8000
            LPAR(LCHD(LAKE,1)) = LAKE                                    RENUMB........8100
            LPAR(LCHD(LAKE,2)) = LAKE                                    RENUMB........8200
            LSIB(LCHD(LAKE,1)) = LCHD(LAKE,2)                            RENUMB........8300
            LSIB(LCHD(LAKE,2)) = LCHD(LAKE,1)                            RENUMB........8400
         END IF                                                          RENUMB........8500
  700 CONTINUE                                                           RENUMB........8600
      DO 750 ISURF=1,NNSURF                                              RENUMB........8700
         LOKO = LOLD(LAKNOD(ISURF)%KLUSTR)                               RENUMB........8800
         LAKNOD(ISURF)%KLUSTR = LOKO                                     RENUMB........8900
         LAKNOD(ISURF)%LEV = LEV(LOKO)                                   RENUMB........9000
  750 CONTINUE                                                           RENUMB........9100
C                                                                        RENUMB........9200
      RETURN                                                             RENUMB........9300
      END                                                                RENUMB........9400
C                                                                        RENUMB........9500
C     SUBROUTINE        S  L  A  K  E              SUTRA VERSION 3.0     SLAKE..........100
C                                                                        SLAKE..........200
C *** PURPOSE :                                                          SLAKE..........300
C ***  TO PERFORM LAKE CACULATIONS AND UPDATE LAKES AS THE SIMULATION    SLAKE..........400
C ***  PROGRESSES.                                                       SLAKE..........500
C                                                                        SLAKE..........600
      SUBROUTINE SLAKE()                                                 SLAKE..........700
      USE LARR                                                           SLAKE..........800
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               SLAKE..........900
      LOGICAL ISDESC                                                     SLAKE.........1000
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  SLAKE.........1100
     1   NPCYC,NUCYC,NPRINT,NBCFPR,NBCSPR,NBCPPR,NBCUPR,NLAKPR,IREAD,    SLAKE.........1200
     1   NBGPPR,NBGUPR,ISTORE,NOUMAT,IUNSAT,IALSAT,KTYPE                 SLAKE.........1300
      COMMON /DIMEN/ NLAKES, NSIL, NCEL, NPLK                            SLAKE.........1400
      COMMON /DIMSRF/ MAXCON, NNSURF, NNVERT                             SLAKE.........1500
      COMMON /LUP/ LAKUP                                                 SLAKE.........1600
      COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,SIGMAW,SIGMAS,      SLAKE.........1700
     1   RHOW0,URHOW0,VISC0,PRODF1,PRODS1,PRODF0,PRODS0,CHI1,CHI2        SLAKE.........1800
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       SLAKE.........1900
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITBCS,ITRST,ITMAX,TSTART      SLAKE.........2000
      EXTERNAL ISDESC                                                    SLAKE.........2100
C                                                                        SLAKE.........2200
C.....COMPILE BOUNDARY-CONDITION SOURCES/SINKS.                          SLAKE.........2300
      CALL LBCCMP()                                                      SLAKE.........2400
C                                                                        SLAKE.........2500
      IF (ISSTRA.EQ.1) THEN                                              SLAKE.........2600
C........STEADY-STATE, DO NOT UPDATE LAKES.                              SLAKE.........2700
         VOVER = 0D0                                                     SLAKE.........2800
         SMOVER = 0D0                                                    SLAKE.........2900
         LSPL = 0                                                        SLAKE.........3000
         FSPILL = 0D0                                                    SLAKE.........3100
         GSPILL = 0D0                                                    SLAKE.........3200
      ELSE                                                               SLAKE.........3300
C........PROPAGATE WATER VOLUME AND SOLUTE MASS OR ENERGY CUMULATIVELY   SLAKE.........3400
C           UP THE TREE.                                                 SLAKE.........3500
         DO 200 LK=NLAKES,1,-1                                           SLAKE.........3600
            IF (ISTAT(LK).EQ.-1) THEN                                    SLAKE.........3700
               LKC1 = LCHD(LK,1)                                         SLAKE.........3800
               LKC2 = LCHD(LK,2)                                         SLAKE.........3900
               VOLW(LK) = VOLW(LKC1) + VOLW(LKC2)                        SLAKE.........4000
               UWMS(LK) = UWMS(LKC1) + UWMS(LKC2)                        SLAKE.........4100
            END IF                                                       SLAKE.........4200
  200    CONTINUE                                                        SLAKE.........4300
C........UPDATE LAKES.                                                   SLAKE.........4400
         CALL LAKNEW()                                                   SLAKE.........4500
      END IF                                                             SLAKE.........4600
C                                                                        SLAKE.........4700
C.....PROPAGATE VARIOUS EXCHANGES UP THE TREE.                           SLAKE.........4800
      FGWG = FGWGLO                                                      SLAKE.........4900
      FGWL = FGWLLO                                                      SLAKE.........5000
      FEXG = FEXGLO                                                      SLAKE.........5100
      FEXL = FEXLLO                                                      SLAKE.........5200
      FROG = FROGLO                                                      SLAKE.........5300
      GGWG = GGWGLO                                                      SLAKE.........5400
      GGWL = GGWLLO                                                      SLAKE.........5500
      GEXG = GEXGLO                                                      SLAKE.........5600
      GEXL = GEXLLO                                                      SLAKE.........5700
      GROG = GROGLO                                                      SLAKE.........5800
      DO 450 LK=NLAKES,2,-1                                              SLAKE.........5900
         LKPAR = LPAR(LK)                                                SLAKE.........6000
         FGWG(LKPAR) = FGWG(LKPAR) + FGWG(LK)                            SLAKE.........6100
         FGWL(LKPAR) = FGWL(LKPAR) + FGWL(LK)                            SLAKE.........6200
         FEXG(LKPAR) = FEXG(LKPAR) + FEXG(LK)                            SLAKE.........6300
         FEXL(LKPAR) = FEXL(LKPAR) + FEXL(LK)                            SLAKE.........6400
         FROG(LKPAR) = FROG(LKPAR) + FROG(LK)                            SLAKE.........6500
         GGWG(LKPAR) = GGWG(LKPAR) + GGWG(LK)                            SLAKE.........6600
         GGWL(LKPAR) = GGWL(LKPAR) + GGWL(LK)                            SLAKE.........6700
         GEXG(LKPAR) = GEXG(LKPAR) + GEXG(LK)                            SLAKE.........6800
         GEXL(LKPAR) = GEXL(LKPAR) + GEXL(LK)                            SLAKE.........6900
         GROG(LKPAR) = GROG(LKPAR) + GROG(LK)                            SLAKE.........7000
  450 CONTINUE                                                           SLAKE.........7100
C.....COMPUTE LAKE-TO-LAKE EXCHANGES.  PROPAGATE THEM UP THE TREE,       SLAKE.........7200
C        ACCUMULATING ONLY SPILLOVERS INTO DESCENDANTS FROM              SLAKE.........7300
C        NON-DESCENDANTS.  (SPILLOVERS FROM DESCENDANTS INTO             SLAKE.........7400
C        NON-DESCENDANTS CANNOT OCCUR AND SO DO NOT NEED TO BE           SLAKE.........7500
C        ACCUMULATED.)  ALSO, RECORD SPILLOVERS TO LAKE-AREA             SLAKE.........7600
C        LIMITS.                                                         SLAKE.........7700
  690 FLKG = 0D0                                                         SLAKE.........7800
      FLKL = 0D0                                                         SLAKE.........7900
      GLKG = 0D0                                                         SLAKE.........8000
      GLKL = 0D0                                                         SLAKE.........8100
      FLLL = 0D0                                                         SLAKE.........8200
      GLLL = 0D0                                                         SLAKE.........8300
      DO 700 LKK=1,NLAKES                                                SLAKE.........8400
         LK = LSPL(LKK)                                                  SLAKE.........8500
         IF (LK.GT.0) THEN                                               SLAKE.........8600
            FLKG(LK) = FLKG(LK) + FSPILL(LKK)                            SLAKE.........8700
            FLKL(LKK) = FSPILL(LKK)                                      SLAKE.........8800
            GLKG(LK) = GLKG(LK) + GSPILL(LKK)                            SLAKE.........8900
            GLKL(LKK) = GSPILL(LKK)                                      SLAKE.........9000
            LKUP = LPAR(LK)                                              SLAKE.........9100
            DO WHILE (LKUP.NE.1)                                         SLAKE.........9200
               IF (.NOT.ISDESC(LKK, LKUP)) THEN                          SLAKE.........9300
                  FLKG(LKUP) = FLKG(LKUP) + FSPILL(LKK)                  SLAKE.........9400
                  GLKG(LKUP) = GLKG(LKUP) + GSPILL(LKK)                  SLAKE.........9500
               END IF                                                    SLAKE.........9600
               LKUP = LPAR(LKUP)                                         SLAKE.........9700
            END DO                                                       SLAKE.........9800
         ELSE IF (LK.EQ.-1) THEN                                         SLAKE.........9900
            FLLL(LKK) = FSPILL(LKK)                                      SLAKE........10000
            GLLL(LKK) = GSPILL(LKK)                                      SLAKE........10100
         END IF                                                          SLAKE........10200
  700 CONTINUE                                                           SLAKE........10300
C                                                                        SLAKE........10400
C.....SET ULK TO FINAL UW                                                SLAKE........10500
      DO 600 ISURF=1,NNSURF                                              SLAKE........10600
         ILN = LAKNOD(ISURF)%INODE                                       SLAKE........10700
  560    LKLO = LLOWN(ISURF)                                             SLAKE........10800
         LK = INCA(ISTAT,LKLO)                                           SLAKE........10900
C........NOTE: DIVIDING BY CW CONVERTS SPECIFIC ENERGY CONTENT           SLAKE........11000
C           TO TEMPERATURE IN THE CASE OF ENERGY TRANSPORT.              SLAKE........11100
C           NO EFFECT IN THE CASE OF SOLUTE TRANSPORT,                   SLAKE........11200
C           SINCE CW=1.                                                  SLAKE........11300
         ULK(ISURF) = UW(LK)/CW                                          SLAKE........11400
  600 CONTINUE                                                           SLAKE........11500
C                                                                        SLAKE........11600
C.....SET LAKUP FLAG AND LAKE STAGES FOR BUDGET.                         SLAKE........11700
      IF (ISSTRA.EQ.0) THEN                                              SLAKE........11800
         DO 800 LK=1,NLAKES                                              SLAKE........11900
            STGB(LK) = STGLAK(LK)                                        SLAKE........12000
  800    CONTINUE                                                        SLAKE........12100
         LAKUP = .TRUE.                                                  SLAKE........12200
      END IF                                                             SLAKE........12300
C                                                                        SLAKE........12400
  999 RETURN                                                             SLAKE........12500
      END                                                                SLAKE........12600
C                                                                        SLAKE........12700
C     SUBROUTINE        S  O  R  T  E  L           SUTRA VERSION 3.0     SORTEL.........100
C                                                                        SORTEL.........200
C *** PURPOSE :                                                          SORTEL.........300
C ***  TO SORT LAKE NODES BY ELEVATION.                                  SORTEL.........400
C                                                                        SORTEL.........500
      SUBROUTINE SORTEL()                                                SORTEL.........600
      USE LARR                                                           SORTEL.........700
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               SORTEL.........800
      INTEGER RMVDIM,IMVDIM,I1VDIM,CMVDIM,LMVDIM,PMVDIM                  SORTEL.........900
      COMMON /DIMSRF/ MAXCON, NNSURF, NNVERT                             SORTEL........1000
      COMMON /MEMCNT/ RMVDIM,IMVDIM,I1VDIM,CMVDIM,LMVDIM,PMVDIM          SORTEL........1100
C                                                                        SORTEL........1200
C.....ALLOCATE ARRAY FOR SORTED NODE ELEVATIONS.                         SORTEL........1300
      ALLOCATE (ELEVND(NNSURF))                                          SORTEL........1400
      RMVDIM = RMVDIM + NNSURF                                           SORTEL........1500
C                                                                        SORTEL........1600
C.....SORT LAKE NODES BY ELEVATION.  (USE INSERTION SORT FOR NOW.)       SORTEL........1700
C        EDGE NODES GO AFTER NON-EDGE NODES OF SAME ELEVATION.           SORTEL........1800
      KLAST = 0                                                          SORTEL........1900
      DO 150 ISURF=1,NNSURF                                              SORTEL........2000
         DO 140 K=1,KLAST                                                SORTEL........2100
            IF ((LAKNOD(ISURF)%ELEV.LT.ELEVND(K)%ELEV).OR.               SORTEL........2200
     1          ((LAKNOD(ISURF)%ISEDGE).AND.                             SORTEL........2300
     2           (LAKNOD(ISURF)%ELEV.EQ.ELEVND(K)%ELEV))) THEN           SORTEL........2400
               KLAST = KLAST + 1                                         SORTEL........2500
               DO 130 KK=KLAST-1,K,-1                                    SORTEL........2600
                  ELEVND(KK+1)%ELEV = ELEVND(KK)%ELEV                    SORTEL........2700
                  ELEVND(KK+1)%ISURF = ELEVND(KK)%ISURF                  SORTEL........2800
  130          CONTINUE                                                  SORTEL........2900
               ELEVND(K)%ELEV = LAKNOD(ISURF)%ELEV                       SORTEL........3000
               ELEVND(K)%ISURF = ISURF                                   SORTEL........3100
               GOTO 150                                                  SORTEL........3200
            END IF                                                       SORTEL........3300
  140    CONTINUE                                                        SORTEL........3400
         KLAST = KLAST + 1                                               SORTEL........3500
         ELEVND(KLAST)%ELEV = LAKNOD(ISURF)%ELEV                         SORTEL........3600
         ELEVND(KLAST)%ISURF = ISURF                                     SORTEL........3700
  150 CONTINUE                                                           SORTEL........3800
C                                                                        SORTEL........3900
      RETURN                                                             SORTEL........4000
      END                                                                SORTEL........4100
C                                                                        SORTEL........4200
C     FUNCTION          S  T  G  L  A  K           SUTRA VERSION 3.0     STGLAK.........100
C                                                                        STGLAK.........200
C *** PURPOSE :                                                          STGLAK.........300
C ***  TO RETURN THE STAGE OF A GIVEN LAKE.                              STGLAK.........400
C                                                                        STGLAK.........500
      FUNCTION STGLAK(LAKE)                                              STGLAK.........600
      USE LARR                                                           STGLAK.........700
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               STGLAK.........800
      COMMON /DIMEN/ NLAKES, NSIL, NCEL, NPLK                            STGLAK.........900
      EXTERNAL INCA                                                      STGLAK........1000
C                                                                        STGLAK........1100
C.....COMPUTE LAKE STAGE (STGLAK).                                       STGLAK........1200
      IF ((ISTAT(LAKE).EQ.0).OR.(ISTAT(LAKE).EQ.-1).OR.                  STGLAK........1300
     1   (ISTAT(LAKE).EQ.-4)) THEN                                       STGLAK........1400
         STGLAK = -1D98                                                  STGLAK........1500
      ELSE                                                               STGLAK........1600
C........FIND ACTIVE ANCESTOR IF NECESSARY.                              STGLAK........1700
         LK = INCA(ISTAT,LAKE)                                           STGLAK........1800
C........INTERPOLATE LAKE STAGE.                                         STGLAK........1900
         DO 200 NSV=2,NSVMAX(LK)                                         STGLAK........2000
            IF ((SVARRY(LK,NSV)%VOLW.GT.VOLW(LK)).OR.                    STGLAK........2100
     1          (NSV.EQ.NSVMAX(LK))) THEN                                STGLAK........2200
               NSV1 = NSV - 1                                            STGLAK........2300
               NSV2 = NSV                                                STGLAK........2400
               EXIT                                                      STGLAK........2500
            END IF                                                       STGLAK........2600
  200    CONTINUE                                                        STGLAK........2700
         V1 = SVARRY(LK,NSV1)%VOLW                                       STGLAK........2800
         V2 = SVARRY(LK,NSV2)%VOLW                                       STGLAK........2900
         S1 = SVARRY(LK,NSV1)%STG                                        STGLAK........3000
         S2 = SVARRY(LK,NSV2)%STG                                        STGLAK........3100
         SLOPE = (S2 - S1)/(V2 - V1)                                     STGLAK........3200
         STGLAK = SLOPE*(VOLW(LK) - V1) + S1                             STGLAK........3300
      END IF                                                             STGLAK........3400
C                                                                        STGLAK........3500
      RETURN                                                             STGLAK........3600
      END                                                                STGLAK........3700
C                                                                        STGLAK........3800
C     SUBROUTINE        S  U  R  C  O  N          SUTRA VERSION 3.0      SURCON.........100
C                                                                        SURCON.........200
C *** PURPOSE :                                                          SURCON.........300
C ***  TO SET UP SURFACE NODE CONNECTIVITY LIST FOR THE PURPOSE OF       SURCON.........400
C        IDENTIFYING CLUSTERS.                                           SURCON.........500
C                                                                        SURCON.........600
      SUBROUTINE SURCON()                                                SURCON.........700
      USE LARR                                                           SURCON.........800
      USE ALLARR, ONLY : IN                                              SURCON.........900
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               SURCON........1000
      DIMENSION KTYPE(2)                                                 SURCON........1100
      INTEGER, ALLOCATABLE :: MSRF(:),NUMELE(:)                          SURCON........1200
      INTEGER RMVDIM,IMVDIM,I1VDIM,CMVDIM,LMVDIM,PMVDIM                  SURCON........1300
      CHARACTER*80 ERRCOD,CHERR(10)                                      SURCON........1400
      DIMENSION INERR(10),RLERR(10)                                      SURCON........1500
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  SURCON........1600
     1   NPCYC,NUCYC,NPRINT,NBCFPR,NBCSPR,NBCPPR,NBCUPR,NLAKPR,IREAD,    SURCON........1700
     1   NBGPPR,NBGUPR,ISTORE,NOUMAT,IUNSAT,IALSAT,KTYPE                 SURCON........1800
      COMMON /DIMLAY/ NLAYS,NNLAY,NELAY                                  SURCON........1900
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              SURCON........2000
     1   NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                                  SURCON........2100
      COMMON /DIMSRF/ MAXCON, NNSURF, NNVERT                             SURCON........2200
      COMMON /MEMCNT/ RMVDIM,IMVDIM,I1VDIM,CMVDIM,LMVDIM,PMVDIM          SURCON........2300
      COMMON /SOLVI/ KSOLVP, KSOLVU, NN1, NN2, NN3                       SURCON........2400
C                                                                        SURCON........2500
C.....SET UP SURFACE NODE CONNECTIVITY LIST FOR THE PURPOSE OF           SURCON........2600
C        IDENTIFYING CLUSTERS.  NODES ARE CONSIDERED CONNECTED IF THEY   SURCON........2700
C        ARE JOINED BY AN ELEMENT EDGE.  ASSUME THAT NODES AND           SURCON........2800
C        ELEMENTS ARE NUMBERED VERTICALLY DOWNWARD FIRST, AND THAT IF    SURCON........2900
C        AN ELEMENT CONTAINS SURFACE NODES, THEY ARE THE LAST FOUR       SURCON........3000
C        NODES LISTED FOR THAT ELEMENT.  THIS IS CONSISTENT WITH THE     SURCON........3100
C        NUMBERING SCHEME IN SUTRAGUI.                                   SURCON........3200
      IF (KTYPE(2).GE.2) THEN                                            SURCON........3300
         NNVERT = NN1                                                    SURCON........3400
         NEVERT = NN1 - 1                                                SURCON........3500
         NNSURF = NN2*NN3                                                SURCON........3600
         NESURF = (NN2 - 1)*(NN3 - 1)                                    SURCON........3700
      ELSE IF (KTYPE(2).EQ.1) THEN                                       SURCON........3800
         NNVERT = NLAYS                                                  SURCON........3900
         NEVERT = NLAYS - 1                                              SURCON........4000
         NNSURF = NNLAY                                                  SURCON........4100
         NESURF = NELAY                                                  SURCON........4200
      ELSE                                                               SURCON........4300
         ERRCOD = 'COD-SURCON-1'                                         SURCON........4400
         CALL SUTERR(ERRCOD,CHERR,INERR,RLERR)                           SURCON........4500
      END IF                                                             SURCON........4600
      ALLOCATE(MSRF(NN),NUMELE(NNSURF))                                  SURCON........4700
      MSRF = 0                                                           SURCON........4800
      DO 250 LS=1,NESURF                                                 SURCON........4900
         L = (LS - 1)*NEVERT + 1                                         SURCON........5000
         DO 220 IL=5,8                                                   SURCON........5100
            IC = IN((L-1)*8+IL)                                          SURCON........5200
            MSRF(IC) = MSRF(IC) + 1                                      SURCON........5300
  220    CONTINUE                                                        SURCON........5400
  250 CONTINUE                                                           SURCON........5500
      MAXCON = 0                                                         SURCON........5600
      ISURF = 0                                                          SURCON........5700
      DO 260 I=1,NN                                                      SURCON........5800
         MAXCON = MAX(MAXCON, MSRF(I))                                   SURCON........5900
         IF (MSRF(I).GT.0) THEN                                          SURCON........6000
            ISURF = ISURF + 1                                            SURCON........6100
            NUMELE(ISURF) = MSRF(I)                                      SURCON........6200
            MSRF(I) = ISURF                                              SURCON........6300
         END IF                                                          SURCON........6400
  260 CONTINUE                                                           SURCON........6500
      ALLOCATE(NBR(NNSURF,MAXCON),ISURFACE(NN))                          SURCON........6600
      IMVDIM = IMVDIM + MAXCON*NNSURF + NN                               SURCON........6700
      ALLOCATE(LAKNOD(NNSURF))                                           SURCON........6800
      IMVDIM = IMVDIM + 3*NNSURF                                         SURCON........6900
      RMVDIM = RMVDIM + 4*NNSURF                                         SURCON........7000
      LMVDIM = LMVDIM + 2*NNSURF                                         SURCON........7100
      ISURFACE = 0                                                       SURCON........7200
      NBR = 0                                                            SURCON........7300
      DO 270 I=1,NN                                                      SURCON........7400
         ISURF = MSRF(I)                                                 SURCON........7500
         IF (ISURF.GT.0) THEN                                            SURCON........7600
            LAKNOD(ISURF)%INODE = I                                      SURCON........7700
            ISURFACE(I) = ISURF                                          SURCON........7800
         END IF                                                          SURCON........7900
  270 CONTINUE                                                           SURCON........8000
      DO 350 LS=1,NESURF                                                 SURCON........8100
         L = (LS - 1)*NEVERT + 1                                         SURCON........8200
         DO 320 IL=5,8                                                   SURCON........8300
            IC = IN((L-1)*8+IL)                                          SURCON........8400
            MC = MSRF(IC)                                                SURCON........8500
            DO 310 JLR=1,3,2                                             SURCON........8600
               JL = IL + JLR                                             SURCON........8700
               IF (JL.GT.8) JL = JL - 4                                  SURCON........8800
               JC = IN((L-1)*8+JL)                                       SURCON........8900
               NC = MSRF(JC)                                             SURCON........9000
               DO 300 M=1,MAXCON                                         SURCON........9100
                  IF (NC.EQ.NBR(MC,M)) THEN                              SURCON........9200
                     EXIT                                                SURCON........9300
                  ELSE IF (NBR(MC,M).EQ.0) THEN                          SURCON........9400
                     NBR(MC,M) = NC                                      SURCON........9500
                     EXIT                                                SURCON........9600
                  END IF                                                 SURCON........9700
  300          CONTINUE                                                  SURCON........9800
  310       CONTINUE                                                     SURCON........9900
  320    CONTINUE                                                        SURCON.......10000
  350 CONTINUE                                                           SURCON.......10100
C                                                                        SURCON.......10200
      DO 420 ISURF=1,NNSURF                                              SURCON.......10300
         M = MAXCON                                                      SURCON.......10400
         DO WHILE (NBR(ISURF,M).EQ.0)                                    SURCON.......10500
            M = M - 1                                                    SURCON.......10600
         END DO                                                          SURCON.......10700
         IF (M.EQ.NUMELE(ISURF)) THEN                                    SURCON.......10800
            LAKNOD(ISURF)%ISEDGE = .FALSE.                               SURCON.......10900
         ELSE                                                            SURCON.......11000
            LAKNOD(ISURF)%ISEDGE = .TRUE.                                SURCON.......11100
         END IF                                                          SURCON.......11200
  420 CONTINUE                                                           SURCON.......11300
C                                                                        SURCON.......11400
      DEALLOCATE(MSRF,NUMELE)                                            SURCON.......11500
C                                                                        SURCON.......11600
      RETURN                                                             SURCON.......11700
      END                                                                SURCON.......11800
C                                                                        SURCON.......11900
C     FUNCTION          V  O  L  L  A  K          SUTRA VERSION 3.0      VOLLAK.........100
C                                                                        VOLLAK.........200
C *** PURPOSE :                                                          VOLLAK.........300
C ***  TO RETURN THE VOLUME OF WATER IN A LAKE GIVEN ITS STAGE.          VOLLAK.........400
C                                                                        VOLLAK.........500
      FUNCTION VOLLAK(LAKE,STG)                                          VOLLAK.........600
      USE LARR                                                           VOLLAK.........700
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               VOLLAK.........800
      COMMON /DIMEN/ NLAKES, NSIL, NCEL, NPLK                            VOLLAK.........900
C                                                                        VOLLAK........1000
C.....INTERPOLATE VOLUME.  (NO BOUNDS CHECKING.)                         VOLLAK........1100
      LK = LAKE                                                          VOLLAK........1200
      DO 200 NSV=2,NSVMAX(LK)                                            VOLLAK........1300
         IF ((SVARRY(LK,NSV)%STG.GT.STG).OR.                             VOLLAK........1400
     1       (NSV.EQ.NSVMAX(LK))) THEN                                   VOLLAK........1500
            NSV1 = NSV - 1                                               VOLLAK........1600
            NSV2 = NSV                                                   VOLLAK........1700
            EXIT                                                         VOLLAK........1800
         END IF                                                          VOLLAK........1900
  200 CONTINUE                                                           VOLLAK........2000
      S1 = SVARRY(LK,NSV1)%STG                                           VOLLAK........2100
      S2 = SVARRY(LK,NSV2)%STG                                           VOLLAK........2200
      V1 = SVARRY(LK,NSV1)%VOLW                                          VOLLAK........2300
      V2 = SVARRY(LK,NSV2)%VOLW                                          VOLLAK........2400
      SLOPE = (V2 - V1)/(S2 - S1)                                        VOLLAK........2500
      VOLLAK = SLOPE*(STG - S1) + V1                                     VOLLAK........2600
C                                                                        VOLLAK........2700
      RETURN                                                             VOLLAK........2800
      END                                                                VOLLAK........2900
C                                                                        VOLLAK........3000
C     SUBROUTINE        V  S  T  A  G  E          SUTRA VERSION 3.0      VSTAGE.........100
C                                                                        VSTAGE.........200
C *** PURPOSE :                                                          VSTAGE.........300
C ***  TO ASSEMBLE AN ARRAY OF STAGE VERSUS VOLUME AND DETERMINE         VSTAGE.........400
C ***  VOLUME LIMITS FOR EACH LAKE.                                      VSTAGE.........500
C                                                                        VSTAGE.........600
      SUBROUTINE VSTAGE()                                                VSTAGE.........700
      USE LARR                                                           VSTAGE.........800
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                               VSTAGE.........900
      DIMENSION ASUM(NLAKES)                                             VSTAGE........1000
      INTEGER RMVDIM,IMVDIM,I1VDIM,CMVDIM,LMVDIM,PMVDIM                  VSTAGE........1100
      COMMON /DIMEN/ NLAKES, NSIL, NCEL, NPLK                            VSTAGE........1200
      COMMON /DIMSRF/ MAXCON, NNSURF, NNVERT                             VSTAGE........1300
      COMMON /MEMCNT/ RMVDIM,IMVDIM,I1VDIM,CMVDIM,LMVDIM,PMVDIM          VSTAGE........1400
C                                                                        VSTAGE........1500
C.....ASSEMBLE ARRAY OF STAGE VERSUS VOLUME FOR EACH LAKE.               VSTAGE........1600
      ALLOCATE(NSVMAX(NLAKES))                                           VSTAGE........1700
      IMVDIM = IMVDIM + NLAKES                                           VSTAGE........1800
      ALLOCATE(SVARRY(NLAKES,NNSURF))                                    VSTAGE........1900
      RMVDIM = RMVDIM + 3*NLAKES*NNSURF                                  VSTAGE........2000
      ASUM = 0D0                                                         VSTAGE........2100
      DO 200 ISURF=1,NNSURF                                              VSTAGE........2200
         LK = LAKNOD(ISURF)%KLUSTR                                       VSTAGE........2300
         ASUM(LK) = ASUM(LK) + LAKNOD(ISURF)%AREA                        VSTAGE........2400
  200 CONTINUE                                                           VSTAGE........2500
      DO 300 LK=NLAKES,1,-1                                              VSTAGE........2600
         IF (LCHD(LK,1).NE.0) THEN                                       VSTAGE........2700
            SVARRY(LK,1)%AREA = ASUM(LCHD(LK,1)) + ASUM(LCHD(LK,2))      VSTAGE........2800
            ASUM(LK) = ASUM(LK) + SVARRY(LK,1)%AREA                      VSTAGE........2900
         ELSE                                                            VSTAGE........3000
            SVARRY(LK,1)%AREA = 0D0                                      VSTAGE........3100
         END IF                                                          VSTAGE........3200
  300 CONTINUE                                                           VSTAGE........3300
      DO 500 LK=1,NLAKES                                                 VSTAGE........3400
         NSVMAX(LK) = 1                                                  VSTAGE........3500
         SVARRY(LK,1)%STG = ELEVND(KLOW(LK))%ELEV                        VSTAGE........3600
         SVARRY(LK,1)%VOLW = 0D0                                         VSTAGE........3700
  500 CONTINUE                                                           VSTAGE........3800
      DO 600 K=1,NNSURF                                                  VSTAGE........3900
         ISURF = ELEVND(K)%ISURF                                         VSTAGE........4000
         LK = LAKNOD(ISURF)%KLUSTR                                       VSTAGE........4100
         ELV = ELEVND(K)%ELEV                                            VSTAGE........4200
         IF (ELV.GT.SVARRY(LK,NSVMAX(LK))%STG) THEN                      VSTAGE........4300
            EDIFF = ELV - SVARRY(LK,NSVMAX(LK))%STG                      VSTAGE........4400
            NSVMAX(LK) = NSVMAX(LK) + 1                                  VSTAGE........4500
            NSV = NSVMAX(LK)                                             VSTAGE........4600
            SVARRY(LK,NSV)%VOLW =                                        VSTAGE........4700
     1         SVARRY(LK,NSV-1)%VOLW + SVARRY(LK,NSV-1)%AREA*EDIFF       VSTAGE........4800
            SVARRY(LK,NSV)%STG = ELV                                     VSTAGE........4900
            SVARRY(LK,NSV)%AREA =                                        VSTAGE........5000
     1         SVARRY(LK,NSV-1)%AREA + LAKNOD(ISURF)%AREA                VSTAGE........5100
         ELSE                                                            VSTAGE........5200
            NSV = NSVMAX(LK)                                             VSTAGE........5300
            SVARRY(LK,NSV)%AREA =                                        VSTAGE........5400
     1         SVARRY(LK,NSV)%AREA + LAKNOD(ISURF)%AREA                  VSTAGE........5500
         END IF                                                          VSTAGE........5600
  600 CONTINUE                                                           VSTAGE........5700
C.....TACK ON STAGE AT WHICH EACH LAKE COALESCES WITH SIBLING (LOWEST    VSTAGE........5800
C        ELEVATION OF PARENT).                                           VSTAGE........5900
      DO 700 LK=2,NLAKES                                                 VSTAGE........6000
         ISURF = ELEVND(KLOW(LPAR(LK)))%ISURF                            VSTAGE........6100
         ELV = ELEVND(KLOW(LPAR(LK)))%ELEV                               VSTAGE........6200
         EDIFF = ELV - SVARRY(LK,NSVMAX(LK))%STG                         VSTAGE........6300
         NSVMAX(LK) = NSVMAX(LK) + 1                                     VSTAGE........6400
         NSV = NSVMAX(LK)                                                VSTAGE........6500
         SVARRY(LK,NSV)%VOLW =                                           VSTAGE........6600
     1      SVARRY(LK,NSV-1)%VOLW + SVARRY(LK,NSV-1)%AREA*EDIFF          VSTAGE........6700
         SVARRY(LK,NSV)%STG = ELV                                        VSTAGE........6800
         SVARRY(LK,NSV)%AREA =                                           VSTAGE........6900
     1      SVARRY(LK,NSV-1)%AREA + LAKNOD(ISURF)%AREA                   VSTAGE........7000
  700 CONTINUE                                                           VSTAGE........7100
C.....IF ARRAY FOR LAKE 1 CONTAINS ONLY ONE STAGE, ADD A SECOND ONE      VSTAGE........7200
C       SO THAT (TRIVIAL) INTERPOLATION/EXTRAPOLATION CAN LATER BE DONE  VSTAGE........7300
C       BY FUNCTION STGLAK.                                              VSTAGE........7400
      LAKE = 1                                                           VSTAGE........7500
      IF (NSVMAX(LAKE).EQ.1) THEN                                        VSTAGE........7600
         ISURF = ELEVND(NNSURF)%ISURF                                    VSTAGE........7700
         EDIFF = ELEVND(NNSURF)%ELEV - ELEVND(1)%ELEV                    VSTAGE........7800
         NSVMAX(LAKE) = 2                                                VSTAGE........7900
         SVARRY(LAKE,2)%VOLW =                                           VSTAGE........8000
     1      SVARRY(LAKE,1)%VOLW + SVARRY(LAKE,1)%AREA*EDIFF              VSTAGE........8100
         SVARRY(LAKE,2)%STG = SVARRY(LAKE,1)%STG + EDIFF                 VSTAGE........8200
         SVARRY(LAKE,2)%AREA =                                           VSTAGE........8300
     1      SVARRY(LAKE,1)%AREA + LAKNOD(ISURF)%AREA                     VSTAGE........8400
      END IF                                                             VSTAGE........8500
C.....PASS VOLUMES UP TO PARENTS AND SET FULL VOLUMES.                   VSTAGE........8600
      ALLOCATE(VFUL(NLAKES))                                             VSTAGE........8700
      RMVDIM = RMVDIM + NLAKES                                           VSTAGE........8800
      DO 850 LK=NLAKES,2,-1                                              VSTAGE........8900
         LKP = LPAR(LK)                                                  VSTAGE........9000
         VFUL(LK) = SVARRY(LK,NSVMAX(LK))%VOLW                           VSTAGE........9100
         DO 800 NSV=1,NSVMAX(LKP)                                        VSTAGE........9200
            SVARRY(LKP,NSV)%VOLW = SVARRY(LKP,NSV)%VOLW + VFUL(LK)       VSTAGE........9300
  800    CONTINUE                                                        VSTAGE........9400
  850 CONTINUE                                                           VSTAGE........9500
      VFUL(1) = SVARRY(LK,NSVMAX(1))%VOLW                                VSTAGE........9600
C                                                                        VSTAGE........9700
C.....SET MAXIMUM VOLUMES LIMITED BY AREAS WHERE LAKE WATER              VSTAGE........9800
C     CAN'T RESIDE.                                                      VSTAGE........9900
      ALLOCATE(VMAX(NLAKES))                                             VSTAGE.......10000
      RMVDIM = RMVDIM + NLAKES                                           VSTAGE.......10100
      DO 900 LK=1,NLAKES                                                 VSTAGE.......10200
         STG = ELEVND(KVMAX(LK))%ELEV                                    VSTAGE.......10300
         VMAX(LK) = VOLLAK(LK,STG)                                       VSTAGE.......10400
  900 CONTINUE                                                           VSTAGE.......10500
C                                                                        VSTAGE.......10600
      RETURN                                                             VSTAGE.......10700
      END                                                                VSTAGE.......10800
