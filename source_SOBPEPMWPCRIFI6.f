*
*=== source ===========================================================*
*
      SUBROUTINE SOURCE ( NOMORE )
      
      INCLUDE 'dblprc.inc'
      INCLUDE 'dimpar.inc'
      INCLUDE 'iounit.inc'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 1990-2010      by    Alfredo Ferrari & Paola Sala  *
*     All Rights Reserved.                                             *
*                                                                      *
*                                                                      *
*     New source for FLUKA9x-FLUKA20xy:                                *
*                                                                      *
*     Created on 07 January 1990   by    Alfredo Ferrari & Paola Sala  *
*                                                   Infn - Milan       *
*                                                                      *
*     Last change on  17-Oct-10    by    Alfredo Ferrari               *
*                                                                      *
*  This is just an example of a possible user written source routine.  *
*  note that the beam card still has some meaning - in the scoring the *
*  maximum momentum used in deciding the binning is taken from the     *
*  beam momentum.  Other beam card parameters are obsolete.            *
*                                                                      *
*       Output variables:                                              *
*                                                                      *
*              Nomore = if > 0 the run will be terminated              *
*                                                                      *
*----------------------------------------------------------------------*
*
      INCLUDE 'caslim.inc' 
      INCLUDE 'beamcm.inc'
      INCLUDE 'fheavy.inc'
      INCLUDE 'flkstk.inc'
      INCLUDE 'ioiocm.inc'
      INCLUDE 'ltclcm.inc'
      INCLUDE 'paprop.inc'
      INCLUDE 'sourcm.inc'
      INCLUDE 'sumcou.inc'
*----------------------------------------------------------------------*
      LOGICAL LFIRST, LISNUT
      DATA LFIRST / .TRUE. /
      SAVE LFIRST
* The maxium row of phy beam plan
      INTEGER NP
      INTEGER LUNRD1
      INTEGER LUNRD
      PARAMETER (NPMAX = 150000)     !far more larger than the spotnumbers
      DIMENSION EG(NPMAX),EP(NPMAX),FX(NPMAX),FY(NPMAX)
      DIMENSION DX(NPMAX),DY(NPMAX),PX(NPMAX),PY(NPMAX),PN(NPMAX)
      DOUBLE PRECISION Eng,Espread,spotX,spotY,DivX,DivY,PosX,PosY,PNs
      SAVE NP
      SAVE EG, EP, FX, FY, DX, DY, PX, PY, PN
* The maxium probility of each beam position
      REAL(kind=8) TOTWEI
      REAL(kind=8) ENERGY,BEPOSX,BEPOSY,LAYSPD,SPOTZX,SPOTZY,DIVERX,DIVERY
*  Parameters for MWPC
      DOUBLE PRECISION ESPG
      CHARACTER*8 MWPC10,MWPC13
      DATA MWPC10 /'S1010   '/
      DATA MWPC13 /'S1013   '/
      REAL(KIND=8) MWPW1(6),MWPW2(6),MWPT1(6),MWPT2(6)
      DATA MWPW1 /-12.5,12.5,-12.5,12.5,-114.938D+00,-114.922D+00/
      DATA MWPW2 /-12.5,12.5,-12.5,12.5,-130.538D+00,-130.522D+00/
      INTEGER MW1,MW2
      SAVE MW1,MW2
      DOUBLE PRECISION TUSP,C1,C2,u1,u2,L1,L2,WET1,WET2
      DATA TUSP /0.10/
      SAVE C1,C2,L1,L2, WET1,WET2
      DOUBLE PRECISION BEAMMOM
      SAVE BEAMMOM
*   Parameters for Ripple-6                                
      INTEGER N
      PARAMETER (NMAX =1000)
      DIMENSION WET(NMAX),PCENT(NMAX)
      CHARACTER*8 BODNAME
      DATA BODNAME /'S1003   '/
      INTEGER IBODY
      REAL(KIND=8) BDYFIN(6)
      DATA BDYFIN /-12.5,12.5,-12.5,12.5,-108.88D+00,-108.58D+00/
      SAVE IBODY
      CHARACTER*250 LINE
      DOUBLE PRECISION C,W,P
      SAVE N, WET, PCENT
*  Statement function:
      LISNUT (IJ) = INDEX ( PRNAME (IJ), 'NEUTRI' ) .GT. 0
*======================================================================*
*                                                                      *
*                 BASIC VERSION                                        *
*                                                                      *
*======================================================================*
*
*----------------------------------------------------------------------*
      NOMORE = 0
*  |  First call initializations:
      IF ( LFIRST ) THEN
*  |  *** The following 3 cards are mandatory ***
         TKESUM = ZERZER
         LFIRST = .FALSE.
         LUSSRC = .TRUE.
*  |  *** User initialization ***   
*  +-----------------------------------------------------------------*
* TO COLLECT DISTRIBUTION OF RIFI6-5000
         LUNRD1 = NINT(WHASOU(1))
         N = 0
 10      CONTINUE  
             READ (LUNRD1,'(A250)', ERR=9999, END=20)LINE
             READ (LINE,'(F5.3,F11.9)',ERR=10)W,P
*             PRINT *,W
*             PRINT *,P
             N = N + 1
             IF (N .GT. NMAX) THEN
                  CALL FLABRT('SOURCE','PLEASE INCREASE NMAX')
             END IF
             IF (N .EQ. 1 .AND. ABS(W).GT. AZRZRZ) THEN
                  CALL FLABRT('SOURCE','ZERO WAS EXPECTED AS FIRST VALUE FOR RIFISIM')
             END IF
             WET(N) = W
             PCENT(N) = P
         GO TO 10
 20      CONTINUE
         CLOSE (LUNRD1)
*  +-----------------------------------------------------------------*
* To collect data from Patientphybeamplan.txt
         LUNRD = NINT(WHASOU(2))
         NP = 0
 30      CONTINUE
             READ (LUNRD,'(A250)', ERR=9999, END=40)LINE
             !PRINT *, LINE
             READ (LINE,*,ERR=30)Eng,Espread,DivX,DivY,spotX,spotY,PosX,PosY,PNs             
*             PRINT *,'Eng,Espread,spotX,spotY,DiverX,DiverY,PosX,PosY,PNs'
*             PRINT *,Eng,Espread,spotX,spotY,DivX,DivY,PosX,PosY,PNs
             NP = NP + 1
             IF (NP .GT. NPMAX) THEN
                  CALL FLABRT('SOURCE','PLEASE INCREASE NPMAX')
             END IF
             EG(NP) = Eng
             EP(NP) = Espread
             FX(NP) = spotX
             FY(NP) = spotY
             DX(NP) = DivX
             DY(NP) = DivY
             PX(NP) = PosX
             PY(NP) = PosY
             PN(NP) = PNs
*            PRINT *,'Eng:',EG(NP),'Espread:',EP(NP),'spotX:',FX(NP),'spotY:',FY(NP)
*            PRINT *,'DiverX:',DX(NP),'DiverY:',DY(NP),'PosX:',PX(NP),'PosY:',PY(NP),'PNs:',PN(NP)
         GO TO 30
40       CONTINUE
         CLOSE (LUNRD)
*         PRINT *,'NP is:',NP
      DO I = 2, NP
          PN(I) = PN(I) + PN(I-1)
      END DO 
      TOTWEI = PN(NP)
      END IF
*  +-----------------------------------------------------------------*
* replace the coordinates for Ripple according Rifi-6 geometry
      CALL NM2BDY(BODNAME,IBODY,IERR)
      C=FLRNDM(C)
      DO I = 1,N
        IF (PCENT(I) .GT. C) THEN
*           PRINT *, I 
*            PRINT *,'PCENT IS:', PCENT(I)
*            PRINT *,'WET IS:', WET(I)
            BDYFIN(6)= -108.88D+00 + WET(I)
*            PRINT *,BDYFIN
            CALL SETBDY(IBODY,9,BDYFIN,6)
            GO TO 90
        END IF
      END DO
      CALL FLABRT('SOURCE','C .GT. CMAX!')
 90   CONTINUE
*  +-----------------------------------------------------------------*
*replace the coordinates for MWPC1 to compensate tungsten in MWPC
      CALL NM2BDY(MWPC10,MW1,IERR)
      C1 = FLRNDM(C1)
      u1 = FLRNDM(u1)
      ! PRINT *,'C1:',C1
      IF (C1 .LE. TUSP) THEN
          L1 = 2 * 0.0035*SQRT (1-u1*u1)
          WET1 = L1 * 9.76D+00
          MWPW1(6)= -114.938D+00 + WET1
          CALL SETBDY(MW1,9,MWPW1,6)
          ! PRINT *,'MWPW1:',MWPW1(6)
      ELSE
          MWPW1(6) = -114.922D+00
          CALL SETBDY(MW1,9,MWPW1,6)
          ! PRINT *,'MWPC1:',MWPW1(6)
      END IF
      CONTINUE
*  +-----------------------------------------------------------------*
*replace the coordinates for MWPC2 to compensate tungsten in MWPC
      CALL NM2BDY(MWPC13,MW2,IERR)
      C2 = FLRNDM(C2)
      u2 = FLRNDM(u2)
      !PRINT *,'C2:',C2
      IF (C2 .LE. TUSP) THEN
          L2 = 2 * 0.0035*SQRT (1-u2*u2)
          WET2 = L2 * 9.76D+00
          MWPW2(6)= -130.538D+00 + WET2
          ! PRINT *,'MWPW2:',MWPW2(6)
          CALL SETBDY(MW2,9,MWPW2,6)
      ELSE
          MWPW2(6) = -130.522D+00
          CALL SETBDY(MW2,9,MWPW2,6)
          ! PRINT *,'MWPC2:',MWPW2(6)
      END IF
      CONTINUE
*  |
*  +-------------------------------------------------------------------*
*  +-------------------------------------------------------------------*
*
* In order to cover the target volume in depth the energy must be
* sampled from the ENEDGE, with cumulative weights from CUMPR
* Sample the energy group
      XI = FLRNDM(DUMMY) * TOTWEI !FLUKA random number generator, 1 is not included
      DO K = 1, NP !500 statement label
          IF(XI .LE. PN(K)) THEN !.LE. meaning <=
             ENERGY = EG(K)*12
             BEPOSX = PX(K)/10 
             BEPOSY = PY(K)/10
             LAYSPD = EP(K)
             SPOTZX = FX(K)
             SPOTZY = FY(K)
             DIVERX = DX(K)
             DIVERY = DY(K)
            GO TO 400
          END IF
      END DO
      STOP
 400  CONTINUE
*
*  Push one source particle to the stack. Note that you could as well
*  push many but this way we reserve a maximum amount of space in the
*  stack for the secondaries to be generated
*  Npflka is the stack counter: of course any time source is called it
*  must be =0
      NPFLKA = NPFLKA + 1
*  Wt is the weight of the particle
      WTFLK  (NPFLKA) = ONEONE
      WEIPRI = WEIPRI + WTFLK (NPFLKA)
*  Particle type (1=proton.....). Ijbeam is the type set by the BEAM
*  card
*  +-------------------------------------------------------------------*
*  |  (Radioactive) isotope:
      IF ( IJBEAM .EQ. -2 .AND. LRDBEA ) THEN
         IARES  = IPROA
         IZRES  = IPROZ
         IISRES = IPROM
         CALL STISBM ( IARES, IZRES, IISRES )
         IJHION = IPROZ  * 1000 + IPROA
         IJHION = IJHION * 100 + KXHEAV
         IONID  = IJHION
         CALL DCDION ( IONID )
         CALL SETION ( IONID )
*  |
*  +-------------------------------------------------------------------*
*  |  Heavy ion:
      ELSE IF ( IJBEAM .EQ. -2 ) THEN
         IJHION = IPROZ  * 1000 + IPROA
         IJHION = IJHION * 100 + KXHEAV
         IONID  = IJHION
         CALL DCDION ( IONID )
         CALL SETION ( IONID )
         ILOFLK (NPFLKA) = IJHION
*  |  Flag this is prompt radiation
         LRADDC (NPFLKA) = .FALSE.
*  |  Group number for "low" energy neutrons, set to 0 anyway
         IGROUP (NPFLKA) = 0
*  |
*  +-------------------------------------------------------------------*
*  |  Normal hadron:
      ELSE
         IONID = IJBEAM
         ILOFLK (NPFLKA) = IJBEAM
*  |  Flag this is prompt radiation
         LRADDC (NPFLKA) = .FALSE.
*  |  Group number for "low" energy neutrons, set to 0 anyway
         IGROUP (NPFLKA) = 0
      END IF
*  |
*  +-------------------------------------------------------------------*
*  From this point .....
*  Particle generation (1 for primaries)
      LOFLK  (NPFLKA) = 1
*  User dependent flag:
      LOUSE  (NPFLKA) = 0
*  No channeling:
      LCHFLK (NPFLKA) = .FALSE.
      DCHFLK (NPFLKA) = ZERZER
*  User dependent spare variables:
      DO 100 ISPR = 1, MKBMX1
         SPAREK (ISPR,NPFLKA) = ZERZER
 100  CONTINUE
*  User dependent spare flags:
      DO 200 ISPR = 1, MKBMX2
         ISPARK (ISPR,NPFLKA) = 0
 200  CONTINUE
*  Save the track number of the stack particle:
      ISPARK (MKBMX2,NPFLKA) = NPFLKA
      NPARMA = NPARMA + 1
      NUMPAR (NPFLKA) = NPARMA
      NEVENT (NPFLKA) = 0
      DFNEAR (NPFLKA) = +ZERZER
*  ... to this point: don't change anything
      AGESTK (NPFLKA) = +ZERZER
      AKNSHR (NPFLKA) = -TWOTWO
*  Particle momentum
*     PMOFLK (NPFLKA) = PBEAM
      CALL FLNRRN(RGAUSS)
      BEAMMOM = SQRT ( ENERGY * ( ENERGY + TWOTWO * AM (IONID) ) )
      ESPG = BEAMMOM * RGAUSS * 2.355D0 * LAYSPD
      PMOFLK (NPFLKA) = BEAMMOM + ESPG 
      !PRINT *,'PMOFLK (NPFLKA):',PMOFLK (NPFLKA)
*  Kinetic energy of the particle (GeV)
      TKEFLK (NPFLKA) = SQRT ( PMOFLK (NPFLKA)**2 + AM (IONID)**2 )
     &                   - AM (IONID)
      !PRINT *,'TKEFLK (NPFLKA):',TKEFLK (NPFLKA) 
*  Cosines (tx,ty,tz)
      CALL FLNRR2(RGAUS1, RGAUS2)     
      DIV_VECTORX = RGAUS1 * DIVERX         ! sigma(unit：rad)
                                        
      DIV_VECTORY = RGAUS2 * DIVERY
      TXHLP = TAN( DIV_VECTORX ) 
      TYHLP = TAN( DIV_VECTORY ) 
      THELP = SQRT( TXHLP*TXHLP + TYHLP*TYHLP + ONEONE ) 
      TXFLK (NPFLKA) = TXHLP / THELP 
      TYFLK (NPFLKA) = TYHLP / THELP 
      TZFLK (NPFLKA) = SQRT ( ONEONE - TXFLK (NPFLKA)**2
     &                       - TYFLK (NPFLKA)**2 )
      !PRINT *, 'TXFLK:',TXFLK(NPFLKA)
*  Polarization cosines:
      TXPOL  (NPFLKA) = -TWOTWO
      TYPOL  (NPFLKA) = +ZERZER
      TZPOL  (NPFLKA) = +ZERZER
*  Particle coordinates
      CALL FLNRR2(R1, R2)
      XFLK (NPFLKA) = BEPOSX + SPOTZX * R1
      YFLK (NPFLKA) = BEPOSY + SPOTZY * R2
      ZFLK (NPFLKA) = ZBEAM
	  !PRINT *,'XFLK:',XFLK
*  Calculate the total kinetic energy of the primaries: don't change
*  +-------------------------------------------------------------------*
*  |  Heavy ion:
      IF ( ILOFLK (NPFLKA) .EQ. -2 .OR. ILOFLK (NPFLKA) .GT. 100000 )
     &   THEN
         TKESUM = TKESUM + TKEFLK (NPFLKA) * WTFLK (NPFLKA)
*  |
*  +-------------------------------------------------------------------*
*  |  Standard particle:
      ELSE IF ( ILOFLK (NPFLKA) .NE. 0 ) THEN
         TKESUM = TKESUM + ( TKEFLK (NPFLKA) + AMDISC (ILOFLK(NPFLKA)) )
     &          * WTFLK (NPFLKA)
*  |
*  +-------------------------------------------------------------------*
*  |
      ELSE
         TKESUM = TKESUM + TKEFLK (NPFLKA) * WTFLK (NPFLKA)
      END IF
*  |
*  +-------------------------------------------------------------------*
      RADDLY (NPFLKA) = ZERZER
*  Here we ask for the region number of the hitting point.
*     NREG (NPFLKA) = ...
*  The following line makes the starting region search much more
*  robust if particles are starting very close to a boundary:
      CALL GEOCRS ( TXFLK (NPFLKA), TYFLK (NPFLKA), TZFLK (NPFLKA) )
      CALL GEOREG ( XFLK  (NPFLKA), YFLK  (NPFLKA), ZFLK  (NPFLKA),
     &              NRGFLK(NPFLKA), IDISC )
*  Do not change these cards:
      CALL GEOHSM ( NHSPNT (NPFLKA), 1, -11, MLATTC )
      NLATTC (NPFLKA) = MLATTC
      CMPATH (NPFLKA) = ZERZER
      CALL SOEVSV
      RETURN
9999  CALL FLABRT('SOURCE','ERROR READING Rifi FILE.')
      RETURN
*=== End of subroutine Source =========================================*
      END

