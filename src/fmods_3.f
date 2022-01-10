C     MODULE            A  L  L  A  R  R           SUTRA VERSION 3.0     ALLARR.........100
C                                                                        ALLARR.........200
C *** PURPOSE :                                                          ALLARR.........300
C ***  TO DECLARE THE MAIN ALLOCATABLE ARRAYS.                           ALLARR.........400
C                                                                        ALLARR.........500
      MODULE ALLARR                                                      ALLARR.........600
      IMPLICIT NONE                                                      ALLARR.........700
      LOGICAL ALLO1, ALLO2, ALLO3                                        ALLARR.........800
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE ::                   ALLARR.........900
     1   PMAT, UMAT                                                      ALLARR........1000
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::                     ALLARR........1100
     1   PITER, UITER, PM1, DPDTITR, UM1, PVEL, SL, SR, X, Y, Z,         ALLARR........1200
     2   VOL, POR, CS1, CS2, CS3, SW, DSWDP, RHO, SOP, QIN, UIN, QUIN,   ALLARR........1300
     3   QINITR, RCIT, RCITM1, GNUPP, GNUUU, PBG1, QPBG1, PBG2, QPBG2,   ALLARR........1400
     4   UPBGI, UBG1, QUBG1, UBG2, QUBG2, QPBGIC, GNUPG, QUBGIC, GNUUG,  ALLARR........1500
     5   QPGITR, DUDTITR, UPBGO                                          ALLARR........1600
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::                     ALLARR........1700
     1   PVEC, UVEC                                                      ALLARR........1800
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::                     ALLARR........1900
     1   ALMAX, ALMIN, ATMAX, ATMIN, VMAG, VANG1,                        ALLARR........2000
     2   PERMXX, PERMXY, PERMYX, PERMYY, PANGL1                          ALLARR........2100
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::                     ALLARR........2200
     1   ALMID, ATMID, VANG2, PERMXZ, PERMYZ,                            ALLARR........2300
     2   PERMZX, PERMZY, PERMZZ, PANGL2, PANGL3                          ALLARR........2400
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::                     ALLARR........2500
     1   PBC, UBC, QPLITR                                                ALLARR........2600
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE ::                   ALLARR........2700
     1   GXSI, GETA, GZET                                                ALLARR........2800
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::                     ALLARR........2900
     1   FWK, B, BPBC, PZERO, BUBC, UZERO, BPBG, PZERG                   ALLARR........3000
      INTEGER, DIMENSION(:), ALLOCATABLE ::                              ALLARR........3100
     1   IN, IQSOP, IQSOU, IPBC, IUBC, IPBG, IUBG,                       ALLARR........3200
     2   NREG, LREG, IWK, IA, JA, NREGAD, ISPBC, ISUBC, ISPBG            ALLARR........3300
      INTEGER(1), DIMENSION(:), ALLOCATABLE ::                           ALLARR........3400
     1   IBCPBC, IBCUBC, IBCSOP, IBCSOU, IBCPBG, IBCUBG                  ALLARR........3500
      INTEGER, DIMENSION(:), ALLOCATABLE ::                              ALLARR........3600
     1   IIDPBC,IIDUBC,IIDSOP,IIDSOU,IIDPBG,IIDUBG                       ALLARR........3700
      CHARACTER*40, DIMENSION(:), ALLOCATABLE ::                         ALLARR........3800
     1   CIDBCS                                                          ALLARR........3900
      CHARACTER*1, DIMENSION(:), ALLOCATABLE ::                          ALLARR........4000
     1   CPQL1, CPQL2                                                    ALLARR........4100
      CHARACTER*3, DIMENSION(:), ALLOCATABLE ::                          ALLARR........4200
     1   CUPBGO                                                          ALLARR........4300
      LOGICAL, DIMENSION(:), ALLOCATABLE ::                              ALLARR........4400
     1   BCSFL, BCSTR, LPBGSP                                            ALLARR........4500
      TYPE OBSDAT                                                        ALLARR........4600
         CHARACTER*40 :: NAME                                            ALLARR........4700
         CHARACTER*10 :: SCHED                                           ALLARR........4800
         CHARACTER*3 :: FRMT                                             ALLARR........4900
         INTEGER :: L                                                    ALLARR........5000
         DOUBLE PRECISION :: X, Y, Z                                     ALLARR........5100
         DOUBLE PRECISION :: XSI, ETA, ZET                               ALLARR........5200
      END TYPE OBSDAT                                                    ALLARR........5300
      TYPE (OBSDAT), DIMENSION (:), ALLOCATABLE :: OBSPTS                ALLARR........5400
C.....ARRAY SCHDLS IS DECLARED IN MODULE SCHDEF.                         ALLARR........5500
C                                                                        ALLARR........5600
      END MODULE ALLARR                                                  ALLARR........5700
C                                                                        ALLARR........5800
C     MODULE            L  L  D  E  F              SUTRA VERSION 3.0     LLDEF..........100
C                                                                        LLDEF..........200
C *** PURPOSE :                                                          LLDEF..........300
C ***  TO DEFINE THE DERIVED TYPE "LLD" (PAIR OF DOUBLE-PRECISION        LLDEF..........400
C ***  NUMBERS).                                                         LLDEF..........500
C                                                                        LLDEF..........600
      MODULE LLDEF                                                       LLDEF..........700
      IMPLICIT NONE                                                      LLDEF..........800
C                                                                        LLDEF..........900
C.....DEFINE DERIVED TYPE LLD (DOUBLE-PRECISION PAIR) WITH               LLDEF.........1000
C        TWO COMPONENTS: DVALU1 AND DVALU2 (DOUBLE-PRECISION NUMBERS).   LLDEF.........1100
      TYPE LLD                                                           LLDEF.........1200
         DOUBLE PRECISION :: DVALU1, DVALU2                              LLDEF.........1300
      END TYPE LLD                                                       LLDEF.........1400
C                                                                        LLDEF.........1500
      END MODULE LLDEF                                                   LLDEF.........1600
C                                                                        LLDEF.........1700
C     MODULE            E  X  P  I  N  T           SUTRA VERSION 3.0     EXPINT.........100
C                                                                        EXPINT.........200
C *** PURPOSE :                                                          EXPINT.........300
C ***  TO PROVIDE EXPLICIT INTERFACES FOR PROCEDURES THAT NEED THEM.     EXPINT.........400
C                                                                        EXPINT.........500
      MODULE EXPINT                                                      EXPINT.........600
      IMPLICIT NONE                                                      EXPINT.........700
C                                                                        EXPINT.........800
C.....EXPLICIT INTERFACE FOR SUBROUTINE BUDPBC                           EXPINT.........900
      INTERFACE                                                          EXPINT........1000
         SUBROUTINE BUDPBC(IP,QPBC,QUPBC,QEXGW,QEXLK,QEXRO,              EXPINT........1100
     1      QUEXGW,QUEXLK,QUEXRO,UEX,PEX,II,ISURFI)                      EXPINT........1200
            USE LARR                                                     EXPINT........1300
            USE ALLARR, ONLY : UVEC,IPBC,UBC,PBC,PVEC,GNUPP,QPLITR,      EXPINT........1400
     1         IBCPBC                                                    EXPINT........1500
            INTEGER IP                                                   EXPINT........1600
            DOUBLE PRECISION QPBC,QUPBC,QEXGW,QEXLK,QEXRO,               EXPINT........1700
     1         QUEXGW,QUEXLK,QUEXRO,UEX,PEX                              EXPINT........1800
            INTEGER, OPTIONAL :: II,ISURFI                               EXPINT........1900
            COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,        EXPINT........2000
     1         NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                            EXPINT........2100
            COMMON /DIMSRF/ MAXCON, NNSURF, NNVERT                       EXPINT........2200
            COMMON /LAKU/ LAKUSD                                         EXPINT........2300
            COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,              EXPINT........2400
     1         SIGMAW,SIGMAS,RHOW0,URHOW0,VISC0,PRODF1,PRODS1,           EXPINT........2500
     2         PRODF0,PRODS0,CHI1,CHI2                                   EXPINT........2600
         END SUBROUTINE BUDPBC                                           EXPINT........2700
      END INTERFACE                                                      EXPINT........2800
C                                                                        EXPINT........2900
C.....EXPLICIT INTERFACE FOR SUBROUTINE BUDPBG                           EXPINT........3000
      INTERFACE                                                          EXPINT........3100
         SUBROUTINE BUDPBG(IPG,QPBG,QUPBG,QEXGW,QEXLK,QEXRO,             EXPINT........3200
     1      QUEXGW,QUEXLK,QUEXRO,UEX,PEX1,PEX2,II,ISURFI)                EXPINT........3300
            USE LARR                                                     EXPINT........3400
            USE ALLARR, ONLY : UVEC,IPBG,PBG1,PBG2,QPBG1,QPBG2,          EXPINT........3500
     1         UPBGI,UPBGO,CUPBGO,CPQL1,CPQL2,PVEC,GNUPG,QPBGIC,QPGITR,  EXPINT........3600
     2         IBCPBG                                                    EXPINT........3700
            INTEGER IPG                                                  EXPINT........3800
            DOUBLE PRECISION QPBG,QUPBG,QEXGW,QEXLK,QEXRO,               EXPINT........3900
     1         QUEXGW,QUEXLK,QUEXRO,UEX,PEX1,PEX2                        EXPINT........4000
            INTEGER, OPTIONAL :: II,ISURFI                               EXPINT........4100
            COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,        EXPINT........4200
     1         NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                            EXPINT........4300
            COMMON /DIMSRF/ MAXCON, NNSURF, NNVERT                       EXPINT........4400
            COMMON /LAKU/ LAKUSD                                         EXPINT........4500
            COMMON /PARAMS/ COMPFL,COMPMA,DRWDU,CW,CS,RHOS,              EXPINT........4600
     1         SIGMAW,SIGMAS,RHOW0,URHOW0,VISC0,PRODF1,PRODS1,           EXPINT........4700
     2         PRODF0,PRODS0,CHI1,CHI2                                   EXPINT........4800
         END SUBROUTINE BUDPBG                                           EXPINT........4900
      END INTERFACE                                                      EXPINT........5000
C                                                                        EXPINT........5100
C.....EXPLICIT INTERFACE FOR FUNCTION PUSWF                              EXPINT........5200
      INTERFACE                                                          EXPINT........5300
         FUNCTION PUSWF(L,XLOC,YLOC,ZLOC,SFRAC,PM1,UM1,PVEC,UVEC,        EXPINT........5400
     1      IN,LREG)                                                     EXPINT........5500
            DOUBLE PRECISION PUSWF(3),XLOC,YLOC,ZLOC,SFRAC               EXPINT........5600
            DOUBLE PRECISION PM1(NN),UM1(NN),PVEC(NN),UVEC(NN)           EXPINT........5700
            INTEGER L,IN(NIN),LREG(NE)                                   EXPINT........5800
            INTEGER NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              EXPINT........5900
     1         NSOP,NSOU,NBCN                                            EXPINT........6000
            COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,        EXPINT........6100
     1         NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                            EXPINT........6200
         END FUNCTION                                                    EXPINT........6300
      END INTERFACE                                                      EXPINT........6400
C                                                                        EXPINT........6500
C.....EXPLICIT INTERFACE FOR FUNCTION DP3STR                             EXPINT........6600
      INTERFACE                                                          EXPINT........6700
         FUNCTION DP3STR(DPA)                                            EXPINT........6800
         DOUBLE PRECISION DPA(3)                                         EXPINT........6900
         CHARACTER DP3STR*45                                             EXPINT........7000
         END FUNCTION                                                    EXPINT........7100
      END INTERFACE                                                      EXPINT........7200
C                                                                        EXPINT........7300
C.....EXPLICIT INTERFACE FOR SUBROUTINE READIF                           EXPINT........7400
      INTERFACE                                                          EXPINT........7500
         SUBROUTINE READIF(KUU, NFB, INTFIL, ERRIN, CHERIN)              EXPINT........7600
            PARAMETER (KINMIN=10)                                        EXPINT........7700
            CHARACTER INTFIL*1000                                        EXPINT........7800
            CHARACTER*80 ERRCOD,ERRIN,CHERR(10)                          EXPINT........7900
            CHARACTER*80, DIMENSION(10), OPTIONAL :: CHERIN              EXPINT........8000
            CHARACTER*80 UNAME,FNAME                                     EXPINT........8100
            CHARACTER ERRF*3, FINS*80                                    EXPINT........8200
            LOGICAL IS                                                   EXPINT........8300
            DIMENSION INERR(10),RLERR(10)                                EXPINT........8400
            DIMENSION IUNIT(0:23)                                        EXPINT........8500
            DIMENSION FNAME(0:23)                                        EXPINT........8600
            COMMON /FNAMES/ UNAME,FNAME                                  EXPINT........8700
            COMMON /FUNIB/ NFBCS                                         EXPINT........8800
            COMMON /FUNITA/ IUNIT                                        EXPINT........8900
            COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8,K9,           EXPINT........9000
     1         K10,K11,K12,K13,K14,K15,K16,K17,K18,K19,K20,K21,K22,K23   EXPINT........9100
            COMMON /OBS/ NOBSN,NTOBS,NOBCYC,NOBLIN,NFLOMX                EXPINT........9200
         END SUBROUTINE READIF                                           EXPINT........9300
      END INTERFACE                                                      EXPINT........9400
C                                                                        EXPINT........9500
      END MODULE EXPINT                                                  EXPINT........9600
C                                                                        EXPINT........9700
C     MODULE            P  T  R  D  E  F           SUTRA VERSION 3.0     PTRDEF.........100
C                                                                        PTRDEF.........200
C *** PURPOSE :                                                          PTRDEF.........300
C ***  TO DEFINE POINTERS AND ARRAYS NEEDED TO CONSTRUCT THE             PTRDEF.........400
C ***  IA AND JA ARRAYS.                                                 PTRDEF.........500
C                                                                        PTRDEF.........600
      MODULE PTRDEF                                                      PTRDEF.........700
      IMPLICIT NONE                                                      PTRDEF.........800
C.....DEFINE DERIVED TYPE LNKLST (LINKED LIST) WITH TWO COMPONENTS:      PTRDEF.........900
C        NODNUM (NODE NUMBER) AND NENT (POINTER TO NEXT ENTRY).          PTRDEF........1000
      TYPE LNKLST                                                        PTRDEF........1100
         INTEGER :: NODNUM                                               PTRDEF........1200
         TYPE (LNKLST), POINTER :: NENT                                  PTRDEF........1300
      END TYPE LNKLST                                                    PTRDEF........1400
C.....DECLARE DENT, DENTPV, DENTPI, AND DENTNW AS GENERAL-PURPOSE        PTRDEF........1500
C        POINTERS OF TYPE LNKLST.                                        PTRDEF........1600
      TYPE (LNKLST), POINTER :: DENT, DENTPV, DENTPI, DENTNW             PTRDEF........1700
C.....DEFINE DERIVED TYPE IPOINT WITH ONE COMPONENT: A POINTER, PL,      PTRDEF........1800
C        OF TYPE LNKLST.                                                 PTRDEF........1900
      TYPE IPOINT                                                        PTRDEF........2000
         TYPE (LNKLST), POINTER :: PL                                    PTRDEF........2100
      END TYPE IPOINT                                                    PTRDEF........2200
C.....DECLARE HLIST, AN ARRAY OF POINTERS THAT WILL POINT TO THE HEAD    PTRDEF........2300
C        OF THE LINKED LIST OF NEIGHBORS FOR EACH NODE.                  PTRDEF........2400
      TYPE (IPOINT), ALLOCATABLE :: HLIST(:)                             PTRDEF........2500
C.....DECLARE ARRAY LLIST.                                               PTRDEF........2600
      INTEGER, DIMENSION(:), ALLOCATABLE :: LLIST                        PTRDEF........2700
C                                                                        PTRDEF........2800
      END MODULE PTRDEF                                                  PTRDEF........2900
C                                                                        PTRDEF........3000
C     MODULE            S  C  H  D  E  F           SUTRA VERSION 3.0     SCHDEF.........100
C                                                                        SCHDEF.........200
C *** PURPOSE :                                                          SCHDEF.........300
C ***  TO DEFINE INFORMATION ASSOCIATED WITH SCHEDULES.                  SCHDEF.........400
C                                                                        SCHDEF.........500
      MODULE SCHDEF                                                      SCHDEF.........600
      USE LLDEF                                                          SCHDEF.........700
      IMPLICIT NONE                                                      SCHDEF.........800
C                                                                        SCHDEF.........900
C.....DEFINE DERIVED TYPE SCHLST WITH THREE COMPONENTS:                  SCHDEF........1000
C        NAME (SCHEDULE NAME)                                            SCHDEF........1100
C        LLEN (LIST LENGTH)                                              SCHDEF........1200
C        SLIST (ARRAY OF TIMES AND STEPS)                                SCHDEF........1300
      TYPE SCHLST                                                        SCHDEF........1400
         CHARACTER*10 NAME                                               SCHDEF........1500
         INTEGER :: LLEN                                                 SCHDEF........1600
         TYPE (LLD), ALLOCATABLE :: SLIST(:)                             SCHDEF........1700
      END TYPE SCHLST                                                    SCHDEF........1800
C                                                                        SCHDEF........1900
C.....DECLARE SCHDLS, AN ARRAY OF INFORMATION ABOUT EACH SCHEDULE.       SCHDEF........2000
      TYPE (SCHLST), ALLOCATABLE :: SCHDLS(:)                            SCHDEF........2100
C                                                                        SCHDEF........2200
C.....DEFINE DERIVED TYPE OBSFM                                          SCHDEF........2300
      TYPE OBSFM                                                         SCHDEF........2400
         INTEGER :: ISCHED                                               SCHDEF........2500
         CHARACTER*3 :: FRMT                                             SCHDEF........2600
      END TYPE OBSFM                                                     SCHDEF........2700
C                                                                        SCHDEF........2800
C.....DEFINE DERIVED TYPE BCSFM                                          SCHDEF........2900
      TYPE BCSFM                                                         SCHDEF........3000
         INTEGER :: ISCHED                                               SCHDEF........3100
      END TYPE BCSFM                                                     SCHDEF........3200
C                                                                        SCHDEF........3300
C.....DECLARE MORE ARRAYS                                                SCHDEF........3400
      TYPE (OBSFM), ALLOCATABLE :: OFP(:)                                SCHDEF........3500
      TYPE (BCSFM), ALLOCATABLE :: BFP(:)                                SCHDEF........3600
      INTEGER, ALLOCATABLE :: IUNIO(:)                                   SCHDEF........3700
      CHARACTER*80, ALLOCATABLE :: FNAMO(:)                              SCHDEF........3800
      LOGICAL, ALLOCATABLE :: ONCK78(:)                                  SCHDEF........3900
C                                                                        SCHDEF........4000
      END MODULE SCHDEF                                                  SCHDEF........4100
C                                                                        SCHDEF........4200
C     MODULE            B  C  S  D  E  F           SUTRA VERSION 3.0     BCSDEF.........100
C                                                                        BCSDEF.........200
C *** PURPOSE :                                                          BCSDEF.........300
C ***  TO DEFINE ARRAYS ASSOCIATED WITH BCS FILES.                       BCSDEF.........400
C                                                                        BCSDEF.........500
      MODULE BCSDEF                                                      BCSDEF.........600
      USE LLDEF                                                          BCSDEF.........700
      IMPLICIT NONE                                                      BCSDEF.........800
C                                                                        BCSDEF.........900
C.....DECLARE ARRAYS                                                     BCSDEF........1000
      INTEGER, ALLOCATABLE :: IUNIB(:)                                   BCSDEF........1100
      CHARACTER*80, ALLOCATABLE :: FNAMB(:)                              BCSDEF........1200
      INTEGER, ALLOCATABLE :: LCNT(:)                                    BCSDEF........1300
      TYPE (LLD), ALLOCATABLE :: DENBCS(:)                               BCSDEF........1400
C                                                                        BCSDEF........1500
      END MODULE BCSDEF                                                  BCSDEF........1600
C                                                                        BCSDEF........1700
C     MODULE            F  I  N  D  E  F           SUTRA VERSION 3.0     FINDEF.........100
C                                                                        FINDEF.........200
C *** PURPOSE :                                                          FINDEF.........300
C ***  TO DEFINE ARRAYS ASSOCIATED FILE INSERTION.                       FINDEF.........400
C                                                                        FINDEF.........500
      MODULE FINDEF                                                      FINDEF.........600
      IMPLICIT NONE                                                      FINDEF.........700
C                                                                        FINDEF.........800
C.....DECLARE ARRAYS                                                     FINDEF.........900
      INTEGER, ALLOCATABLE :: NKS(:), KLIST(:,:)                         FINDEF........1000
      CHARACTER*80, ALLOCATABLE :: FNAIN(:,:)                            FINDEF........1100
C                                                                        FINDEF........1200
      END MODULE FINDEF                                                  FINDEF........1300
