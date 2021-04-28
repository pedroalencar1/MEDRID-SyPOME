   PROGRAM SYPoME

!     PROGRAM TO SIMULATE SEDIMENT YIELD USING POME-EQUATION
!     1. VARIABLES DECLARATION

      INTEGER nprec,iprec,ncell,icell,nev,iev,irep,i
      CHARACTER arquivo1*20,arquivo2*20
	  CHARACTER*8, DIMENSION(10,3000) :: dia
	  INTEGER, DIMENSION(10,3000) :: id
	  REAL, DIMENSION(10,3000) :: D,dur,R
	  COMMON /EVENTOS/ id,D,dur,R
	  INTEGER, DIMENSION(100) :: igauge
	  REAL ds,vs,A,K,CP,S0,S,w0,fL,L0,Kv
      COMMON /CELULAS/ ds,vs,A,K,CP,S0,S,w0,fL,L0,Kv,igauge

!     2. MAIN PROGRAM

      CALL ABERTURA(arquivo1,arquivo2,nprec,ncell,nev,irep)

!     read rainfall-related data of the events
	  iprec = 0
      DO WHILE (iprec.lt.nprec)
	     iprec=iprec+1
		 READ(20,*) i
		 IF (i.ne.iprec) THEN
		    WRITE(*,*)  ' Incompatibility between indexes of gauge stations !!!'
	        WRITE(21,*) ' Incompatibility between indexes of gauge stations !!!'
		 ENDIF
		 WRITE(21,201) ' Precipitation gauge number ..................... ',iprec
	     WRITE(21,*)   '------------------------------------------------------------'
	     WRITE(21,*)   '   i     id    date      D(mm)     Dur(min)    R(MJ.mm/ha/h)      '
	     WRITE(21,*)   '------------------------------------------------------------'
		 WRITE(*,201)  ' Precipitation gauge number ....1................. ',iprec
	     WRITE(*,*)    '------------------------------------------------------------'
	     WRITE(*,*)   '   i     id    date      D(mm)     Dur(min)    R(MJ.mm/ha/h)      '
	     WRITE(*,*)    '------------------------------------------------------------'
		 iev = 0
		 DO WHILE (iev.lt.nev)
			iev=iev+1
		    READ(20,*) id(iprec,iev),dia(iprec,iev),D(iprec,iev),dur(iprec,iev),R(iprec,iev)
			WRITE(21,202) iev,id(iprec,iev),dia(iprec,iev),D(iprec,iev),dur(iprec,iev),R(iprec,iev)
		 ENDDO
	  WRITE(21,*)    '------------------------------------------------------------'
	  WRITE(*,*)    '------------------------------------------------------------'
	  ENDDO

!     read physiographic-related data of the cells and compute sediment yield
	  SSY = 0
	  SGEr = 0
      icell = 0
	  DO WHILE (icell.lt.ncell)
		 icell = icell+1
		 READ (20,*) icell,ds,vs,A,K,CP,S0,w0,Kv,igauge(icell)
		 CALL CALCSY(icell,nev,SY,GEr,irep)
		 SSY = SSY + SY
		 SGEr = SGEr + GEr
	  ENDDO

!     close program
      WRITE(21,203) ' Watershed gross erosion (kg) .............',SGEr
	  WRITE(21,203) ' Watershed sediment yield (kg) ............',SSY
	  WRITE(21,204) ' Watershed average delivery ratio .........',SSY/SGer
	  WRITE(21,*)
	  WRITE(21,*)   ' Program concluded successfully.'
      WRITE(*,203) ' Watershed gross erosion (kg) .............',SGEr
	  WRITE(*,203) ' Watershed sediment yield (kg) ............',SSY
	  WRITE(*,204) ' Watershed average delivery ratio .........',SSY/SGer
      WRITE(*,*)
	  WRITE(*,*)    ' Program concluded successfully.'
	  CLOSE(20)
	  CLOSE(21)
  201 FORMAT (a50,i4)
  202 FORMAT (i5,2x,i5,2x,a8,2x,f6.2,5x,f8.1,5x,f7.1)
  203 FORMAT (a44,e10.4)
  204 FORMAT (a44,f5.3)
      END

!     3. SUBROUTINE THAT OPENS PROGRAM

      SUBROUTINE ABERTURA(arquivo1,arquivo2,nprec,ncell,nev,irep)

      CHARACTER arquivo1*20,arquivo2*20,title*20
	  INTEGER   nprec,ncell,nev,irep

      WRITE(*,*)' SEDIMENT-YIELD ESTIMATION - SYPOME3'
	  WRITE(*,*)
	  WRITE(*,*)' * Version 3'
	  WRITE(*,*)' * SY equation based on the principle of maximum entropy'
      WRITE(*,*)' * Program can only compute up to 3000 events'
      WRITE(*,*)'                       '
      WRITE(*,*)' Universidade Federal do Ceara'
      WRITE(*,*)' Jose Carlos de Araujo'
      WRITE(*,*)' Technische Universitat Berlin'
      WRITE(*,*)' Pedro Alencar'
      WRITE(*,*)' 2019'
      WRITE(*,*)
	  WRITE(*,*) '--------------------------------------------------------------'

      WRITE(*,*)'Type the name of the input file:'     
      READ(*,302) arquivo1
      OPEN(20,file=arquivo1,status='old')      
      READ(20,*)title
!      OPEN(20,file='in.txt',status='old')
      WRITE(*,*)
      WRITE(*,*)'Type the name of the output file:'
      READ(*,302) arquivo2           
      OPEN(21,file=arquivo2,status='new')
      WRITE(*,*)
!      OPEN(21,file='out.txt',status='new')
	  WRITE(*,*)'Do you need a complete (1) or a simplified (2) report?'
	  READ(*,*) irep
	  IF(irep.ne.1.and.irep.ne.2) THEN
	     WRITE(*,*) 'The number is not an option. Default (complete) report will be provided'
		 irep = 1
	  ENDIF
!	  irep = 2

      WRITE(21,*)' SEDIMENT-YIELD ESTIMATION - SYPOME3'
	  WRITE(21,*)
	  WRITE(21,*)' * Version 3'
	  WRITE(21,*)' * SY equation based on the principle of maximum entropy'
      WRITE(21,*)' * Program can only compute up to 3000 events'
      WRITE(21,*)'                       '
      WRITE(21,*)' Universidade Federal do Ceara'
      WRITE(21,*)' Technische Unibversitat Berlin'
      WRITE(21,*)' Jose Carlos de Araujo'
      WRITE(21,*)' Pedro Alencar'
      WRITE(21,*)' 2019'
      WRITE(21,*)
	  WRITE(21,*) '--------------------------------------------------------------'
      WRITE(21,*) 'Title: ', title
      WRITE(*,*) 'Title: ', title
	  WRITE(21,*) '--------------------------------------------------------------'
      WRITE(21,301) ' Input file ..................................... ',"defaut"
      WRITE(21,301) ' Output file .................................... ',"defaut"
	  WRITE(*,*) '--------------------------------------------------------------'
      WRITE(*,301) ' Input file ..................................... ',"defaut"
      WRITE(*,301) ' Output file .................................... ',"defaut"

	  READ(20,*) nprec
	  READ(20,*) ncell
	  READ(20,*) nev
      WRITE(21,303) ' Number of precipitation gauges ................. ',nprec
      WRITE(21,303) ' Number of cells ................................ ',ncell
      WRITE(21,303) ' Number of events ............................... ',nev
	  WRITE(21,*)   '------------------------------------------------------------'
      WRITE(*,303)  ' Number of precipitation gauges ................. ',nprec
      WRITE(*,303)  ' Number of cells ................................ ',ncell
      WRITE(*,303)  ' Number of events ............................... ',nev
	  WRITE(*,*)    '------------------------------------------------------------'

  301 FORMAT (a50,a20)
  302 FORMAT (a20)
  303 FORMAT (a50,i4)
      END

!     4. SUBROUTINE THAT COMPUTES SEDIMENT YIELD

      SUBROUTINE CALCSY(icell,nev,SY,GEr,irep)

	  INTEGER icell,nev,iev,irep
	  REAL SY,SYi,GEr,GEri,beta
	  INTEGER, DIMENSION(100) :: igauge
	  REAL ds,vs,A,K,CP,S0,S,w0,fL,L0,Kv
      COMMON /CELULAS/ ds,vs,A,K,CP,S0,S,w0,fL,L0,Kv,igauge

	  L0 = 10000*A/(2*w0)
	  IF (S0.lt.0.090) THEN
	     S = 10.8*SIN(ATAN(S0))+0.03
		 ELSE
		 S = 16.8*SIN(ATAN(S0))-0.50
	  ENDIF
	  beta = 11.16*SIN(ATAN(S0))/(3*(SIN(ATAN(S0))**0.8)+0.56)
	  fL = (L0/22.1)**(beta/(beta+1))

	  WRITE(*,400) ' Cell number................................',icell
	  WRITE(*,401) ' Area (ha)..................................',A
      WRITE(*,401) ' Soil density (-)...........................',ds
	  WRITE(*,401) ' Sedimentation velocity (m/s)...............',vs
  	  WRITE(*,403) ' Drainage length w0 (m).....................',w0
	  WRITE(*,403) ' Slope length L0 (m)........................',L0
	  WRITE(*,401) ' Soil erodibility (ton.h/MJ/mm).............',K
	  WRITE(*,402) ' Land-use factor CP (-).....................',CP
	  WRITE(*,402) ' Average slope S0 (-) ......................',S0
	  WRITE(*,401) ' Slope factor S (-).........................',S
	  WRITE(*,401) ' Slope length factor L (-)..................',fL
	  WRITE(*,401) ' Vegetation parameter Kv ...................',Kv

      WRITE(21,400) ' Cell number ...............................',icell
	  WRITE(21,401) ' Area (ha)..................................',A
      WRITE(21,401) ' Soil density (-)...........................',ds
	  WRITE(21,401) ' Sedimentation velocity (m/s)...............',vs
  	  WRITE(21,403) ' Drainage length w0 (m).....................',w0
	  WRITE(21,403) ' Slope length L0 (m)........................',L0
	  WRITE(21,401) ' Soil erodibility (ton.h/MJ/mm).............',K
	  WRITE(21,402) ' Land-use factor CP (-).....................',CP
	  WRITE(21,402) ' Average slope S0 (-) ......................',S0
	  WRITE(21,401) ' Slope factor S (-).........................',S
	  WRITE(21,401) ' Slope length factor L (-)..................',fL
	  WRITE(21,401) ' Vegetation parameter Kv ...................',Kv
	  WRITE(21,400) ' Number of rainfall station ................',igauge(icell)
      IF (irep.eq.1) THEN
	     WRITE(21,*)   '-----------------------------------------------------------------------------'
	     WRITE(21,*)   ' id  gross-er(kg) Stream-pw(J/s/m2) Lambda(1/m)    Lm(m)    SDR    SY(kg/ha) '
	     WRITE(21,*)   '-----------------------------------------------------------------------------'
	  ENDIF
	  SY = 0
	  GEr = 0
	  iev = 0
	  DO WHILE (iev.lt.nev)
	     iev=iev+1
		 CALL EVENT(icell,iev,GEri,SYi,irep)
		 GEr = GEr + GEri
		 SY = SY + SYi
	  ENDDO
	  WRITE(*,*) '-----------------------------------------------------------------'
	  WRITE(*,404) ' Total gross erosion (kg) in this cell ....',GEr
	  WRITE(*,404) ' Total sediment yield (kg) ................',SY
	  WRITE(*,405) ' Global sediment delivery ratio ...........',SY/GEr
	  WRITE(*,*) '-----------------------------------------------------------------'
	  WRITE(21,*) '-----------------------------------------------------------------'
	  WRITE(21,404) ' Total gross erosion (kg) in this cell ....',GEr
	  WRITE(21,404) ' Total sediment yield (kg) ................',SY
	  WRITE(21,405) ' Global sediment delivery ratio ...........',SY/GEr
	  WRITE(21,*) '-----------------------------------------------------------------'

  400 FORMAT (a44,i6)
  401 FORMAT (a44,f9.3)
  402 FORMAT (a44,f9.4)
  403 FORMAT (a44,f9.2)
  404 FORMAT (a44,e10.4)
  405 FORMAT (a44,f5.3)
      END

!     5. SUBROUTINE THAT PROCESSES DATA FROM EACH EVENT

      SUBROUTINE EVENT(icell,iev,GEri,SYi,irep)

	  INTEGER iev,irep,icell
	  REAL Lm,SDR,eps,erosion,streamp,f2,L2
	  REAL GEri,SYi
	  INTEGER, DIMENSION(100) :: igauge
	  REAL ds,vs,A,K,CP,S0,S,w0,fL,L0,Kv
      COMMON /CELULAS/ ds,vs,A,K,CP,S0,S,w0,fL,L0,Kv,igauge
	  INTEGER, DIMENSION(10,3000) :: id
	  REAL, DIMENSION(10,3000) :: D,dur,R
	  COMMON /EVENTOS/ id,D,dur,R

!     id = event identity; D = runoff (mm/dur);
!     dur = event duration (min); R = erosivity (MJ.mm/ha/h)

	  eps = R(igauge(icell),iev)*K*CP*S*fL/10
	  erosion = 10000*A*eps
	  GEri = erosion
	  streamp = 9807*L0*S0*(D(igauge(icell),iev)/1000)/(60*dur(igauge(icell),iev))
	  f2 = Kv*(ds/(ds-1))*streamp*L0/(9.807*eps*vs)

	  CALL PARAM(L0,Lm,L2,SDR,f2)
	  SYi = erosion*SDR
	  IF (irep.eq.1) THEN
	     WRITE(21,501) id(igauge(icell),iev),erosion,streamp,L2,Lm,SDR,SYi/A
	  ENDIF
  501 FORMAT(i5,2x,e9.3,5x,e8.3,8x,e8.2,4x,f9.2,2x,f5.3,3x,f10.4)

	  END

!     6. SUBROUTINE TO COMPUTE VARIABLE SDR AND PARAMETERS Lm & L2

    SUBROUTINE PARAM(L0,Lm,L2,SDR,f2)

    INTEGER i1
	LOGICAL run1
	REAL L0,Lm,L2,SDR,f2
	REAL Lm1,Lm2,Lm3,tol1,err1,nmax1
	REAL*8 h1,h2,h3,a,b,aux_log

    Lm1 = L0/100.
	x0 = L0-Lm1
	CALL Lambda(L0,f2,x0,Lm1,L2)
	a = L2*(Lm1+x0)
	b = L2*x0
	h1 = log(a - 1. - (b - 1.)*exp(-L2*Lm1)) - log(f2*L2) - log(1. - exp(-a))

    !print*,L2, f2, f2*L2, log(f2*L2)

    Lm2 = 50*L0
	x0 = 0.
	CALL Lambda(L0,f2,x0,Lm2,L2)
    a = L2*(Lm2+x0)
	b = L2*x0
	alfa = a/b
    h2 = log(a - 1. - (b - 1.)*exp(-L2*Lm2)) - log(f2*L2) - log(1. - exp(-a))


    i1 = 0
    tol1 = 0.001
	nmax1 = 100.
	run1 = .TRUE.
    DO WHILE (run1)
        i1 = i1+1

		Lm3 = (ABS(h1)*Lm2+ABS(h2)*Lm1)/(ABS(h1)+ABS(h2))
	    x0 = MAX(0.,L0-Lm3)
        CALL Lambda(L0,f2,x0,Lm3,L2)
        a = L2*(Lm3+x0)
        b = L2*x0

        aux_log = (b-1)*exp(-L2*Lm3)
        aux_log = (a-1) - aux_log
        aux_log = abs(aux_log) 

        h3 = log(aux_log) - log(f2*L2) - log(1. - exp(-a))
		IF(h3*h2.le.0.) THEN
            Lm1 = Lm3
			h1 = h3
        ELSE
            IF(h3*h1.le.0.) THEN
                Lm2 = Lm3
                h2 = h3
			ELSE
                IF(ABS(h1).le.ABS(h2)) THEN
                    Lm2 = Lm3
                    h2 = h3
			   ELSE
                    Lm1 = Lm3
                    h1 = h3
			   ENDIF
			ENDIF
		ENDIF
		err1 = ABS(h3)
        IF (err1.le.tol1.or.i1.ge.nmax1) THEN
            run1 = .FALSE.
        ENDIF
    ENDDO
	Lm = Lm3
    x0 = max(0.,L0-Lm)
	CALL Lambda(L0,f2,x0,Lm,L2)

	SDR = (L0-x0)/L0
	SDR = SDR*(fexp(L2*Lm)-L2*(L0-x0)/2.)
    SDR = SDR/fexp(L2*(x0+Lm))

    END

!     7. SUBROUTINE TO COMPUTE PARAMETER LAMBDA-2, GIVEN L0, f2 AND Lm

    SUBROUTINE Lambda(L0,f2,x0,Lm,L2)

    INTEGER i2
    LOGICAL run2
	REAL L0,f2,x0,Lm,L2
	REAL xm,L21,L22,L23,tol2,err2,nmax2
	REAL*8 g1,g2,g3,c1, c2, c3

    i2 = 0.

    nmax2 = 100.
	tol2 = 0.001
    xm = (x0+L0)/2.

	L21 = (5E-8)/Lm
	c1 = L21*(0.5*(L0-x0) + Lm)
	g1 = log(L21*Lm) + c1 - log(1. - exp(-L21*(x0+Lm)))

	L22 = 0.01
    c2 = L22*(0.5*(L0-x0) + Lm)
	g2 = log(L22*Lm) + c2 - log(1. - exp(-L22*(x0+Lm)))

	i2 = 0.
    run2 = .TRUE.
	DO WHILE (run2)
        i2 = i2+1.
		L23 = (ABS(g1)*L22+ABS(g2)*L21)/(ABS(g1)+ABS(g2))
		L23 = MAX(L23,(5E-8)/Lm)
        c3 = L23*(0.5*(L0-x0) + Lm)
        g3 = log(L23*Lm) + c3 - log(1. - exp(-L23*(x0+Lm)))

        IF(g3*g2.le.0.) THEN
            L21 = L23
			g1 = g3
		ELSE
            IF(g3*g1.le.0.) THEN
                L22 = L23
                g2 = g3
			ELSE
                IF(ABS(g1).le.ABS(g2)) THEN
                    L22 = L23
                    g2 = g3
                ELSE
                    L21 = L23
                    g1 = g3
                ENDIF
            ENDIF
        ENDIF
        err2 = ABS(g3)
        IF (err2.le.tol2.or.i2.ge.nmax2) THEN
            run2 = .FALSE.
        ENDIF
    ENDDO
!     requirement due to numerical stability
    IF(L23*Lm.lt.5E-8) THEN
        L23 = (5E-8)/Lm
    ELSE
        IF (L23*Lm.gt.1.) THEN
            L23 = 1./Lm
        ENDIF
    ENDIF
        L2 = L23
END



!     Function that computes approximation of exp(x) - 1 using McLaurin series
	  REAL FUNCTION fexp(x)
	  REAL x
	  fexp = x+(x**2)/2+(x**3)/6+(x**4)/24+(x**5)/120
	  END