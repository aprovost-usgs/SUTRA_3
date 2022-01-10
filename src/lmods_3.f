C     MODULE            L  A  R  R                 SUTRA VERSION 3.0     LARR...........100
C                                                                        LARR...........200
C *** PURPOSE :                                                          LARR...........300
C ***  TO DECLARE THE MAIN ALLOCATABLE ARRAYS ASSOCIATED WITH LAKES.     LARR...........400
C                                                                        LARR...........500
      MODULE LARR                                                        LARR...........600
      IMPLICIT NONE                                                      LARR...........700
C                                                                        LARR...........800
      TYPE LNOD                                                          LARR...........900
         DOUBLE PRECISION :: ELEV, AREA, X, Y                            LARR..........1000
         INTEGER :: KLUSTR, INODE, LEV                                   LARR..........1100
         LOGICAL :: LAKEOK                                               LARR..........1200
         LOGICAL :: ISEDGE                                               LARR..........1300
      END TYPE LNOD                                                      LARR..........1400
      TYPE (LNOD), ALLOCATABLE :: LAKNOD(:)                              LARR..........1500
C                                                                        LARR..........1600
      TYPE EN                                                            LARR..........1700
         DOUBLE PRECISION :: ELEV                                        LARR..........1800
         INTEGER :: ISURF                                                LARR..........1900
      END TYPE EN                                                        LARR..........2000
      TYPE (EN), ALLOCATABLE :: ELEVND(:)                                LARR..........2100
C                                                                        LARR..........2200
      TYPE SV                                                            LARR..........2300
         DOUBLE PRECISION :: STG, VOLW, AREA                             LARR..........2400
      END TYPE SV                                                        LARR..........2500
      TYPE (SV), ALLOCATABLE :: SVARRY(:,:)                              LARR..........2600
C                                                                        LARR..........2700
      DOUBLE PRECISION, ALLOCATABLE ::                                   LARR..........2800
     1   VOLW(:),VOLWO(:),VOLWM1(:),VOVER(:),WMOVER(:),                  LARR..........2900
     1   UWMS(:),UWMSO(:),UWMSM1(:),SMOVER(:),                           LARR..........3000
     1   UW(:),UWO(:),UWM1(:),STGB(:),STGBO(:),                          LARR..........3100
     1   QLO(:),QULO(:),Q(:),QU(:),                                      LARR..........3200
     1   FGWGLO(:),FGWLLO(:),FEXGLO(:),FEXLLO(:),                        LARR..........3300
     1   FROGLO(:),FGWG(:),FGWL(:),FEXG(:),                              LARR..........3400
     1   FEXL(:),FROG(:),FLKG(:),FLKL(:),                                LARR..........3500
     1   FLLL(:),FSPILL(:),                                              LARR..........3600
     1   GGWGLO(:),GGWLLO(:),GEXGLO(:),GEXLLO(:),                        LARR..........3700
     1   GROGLO(:),GGWG(:),GGWL(:),GEXG(:),                              LARR..........3800
     1   GEXL(:),GROG(:),GLKG(:),GLKL(:),                                LARR..........3900
     1   GLLL(:),GSPILL(:)                                               LARR..........4000
C                                                                        LARR..........4100
      DOUBLE PRECISION, ALLOCATABLE ::                                   LARR..........4200
     1   VFUL(:),VMAX(:),                                                LARR..........4300
     1   PLK(:),ULK(:),ULKITR(:),                                        LARR..........4400
     1   FRRO(:),FDRO(:)                                                 LARR..........4500
C                                                                        LARR..........4600
      INTEGER, ALLOCATABLE ::                                            LARR..........4700
     1   KL(:),NBR(:,:),KLOW(:),KHIGH(:),KVMAX(:),NSVMAX(:),             LARR..........4800
     1   LLOW(:),LLOWN(:),LCHD(:,:),LPAR(:),LSIB(:),LSPL(:),             LARR..........4900
     1   ISURFACE(:),ISTAT(:),ISTATO(:),ISTATM1(:)                       LARR..........5000
C                                                                        LARR..........5100
      INTEGER, ALLOCATABLE ::                                            LARR..........5200
     1   IBCSF(:),IBCSS(:),IBCSP(:),IBCSU(:),IBCSPG(:),IBCSUG(:),        LARR..........5300
     1   ILKF(:),ILKS(:),ILKP(:),ILKU(:),                                LARR..........5400
     1   ILKPG(:),ITIPG(:),ILKUG(:),ITIUG(:)                             LARR..........5500
C                                                                        LARR..........5600
      LOGICAL, ALLOCATABLE ::                                            LARR..........5700
     1   ISLAKE(:),ISLIMITED(:),ISSPLIT(:),                              LARR..........5800
     1   LKBCPBC(:),LKBCUBC(:),LKBCSOP(:),LKBCSOU(:),                    LARR..........5900
     1   LKBCPBG(:),LKBCUBG(:)                                           LARR..........6000
C                                                                        LARR..........6100
      END MODULE LARR                                                    LARR..........6200
