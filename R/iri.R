iri <- function(time=c(2009,7,1,11,0,0),latitude=69.5864,longitude=19.2272,heibeg=1,heiend=1000,heistp=1,messages=FALSE){


  # maximum 1000 heights
  if(floor((heiend-heibeg)/heistp)>1000) stop('Maximum 1000 heights.')

  # option switches of iri_sub
  jf <- rep(T,50)
  jf[c(4,5,21,22,28,29,30,33,34,35)] <- FALSE
  if(messages) jf[34] <- TRUE
#  jf[c(4,5,6,21,22,29,30,33,34,35)] <- F
  #
  # the following is a copied from irisub.f
  #
#C-----------------------------------------------------------------
#C
#C INPUT:  JF(1:50)      true/false switches for several options
#C         JMAG          =0 geographic   = 1 geomagnetic coordinates
#C         ALATI,ALONG   LATITUDE NORTH AND LONGITUDE EAST IN DEGREES
#C         IYYYY         Year as YYYY, e.g. 1985
#C         MMDD (-DDD)   DATE (OR DAY OF YEAR AS A NEGATIVE NUMBER)
#C         DHOUR         LOCAL TIME (OR UNIVERSAL TIME + 25) IN DECIMAL
#C                          HOURS
#C         HEIBEG,       HEIGHT RANGE IN KM; maximal 100 heights, i.e.
#C          HEIEND,HEISTP        int((heiend-heibeg)/heistp)+1.le.100
#C
#C    JF switches to turn off/on (.true./.false.) several options
#C
#C    i       .true.                  .false.          standard version
#C    -----------------------------------------------------------------
#C    1    Ne computed            Ne not computed                     t
#C    2    Te, Ti computed        Te, Ti not computed                 t
#C    3    Ne & Ni computed       Ni not computed                     t
#C    4    B0,B1 - Bil-2000       B0,B1 - other models jf(31)     false
#C    5    foF2 - C#CIR            foF2 - URSI                     false
#C    6    Ni - DS-1995 & DY-1985 Ni - RBV-2010 & TTS-2005        false
#C    7    Ne - Tops: f10.7<188   f10.7 unlimited                     t
#C    8    foF2 from model        foF2 or NmF2 - user input           t
#C    9    hmF2 from model        hmF2 or M3000F2 - user input        t
#C   10    Te - Standard          Te - Using Te/Ne correlation        t
#C   11    Ne - Standard Profile  Ne - Lay-function formalism         t
#C   12    Messages to unit 6     to messages.txt on unit 11          t
#C   13    foF1 from model        foF1 or NmF1 - user input           t
#C   14    hmF1 from model        hmF1 - user input (only Lay version)t
#C   15    foE  from model        foE or NmE - user input             t
#C   16    hmE  from model        hmE - user input                    t
#C   17    Rz12 from file         Rz12 - user input                   t
#C   18    IGRF dip, magbr, modip old FIELDG using POGO68/10 for 1973 t
#C   19    F1 probability model   critical solar zenith angle (old)   t
#C   20    standard F1            standard F1 plus L condition        t
#C   21    ion drift computed     ion drift not computed          false
#C   22    ion densities in %     ion densities in m-3                t
#C   23    Te_tops (Bil-1985)     Te_topside (TBT-2012)           false
#C   24    D-region: IRI-1990     FT-2001 and DRS-1995                t
#C   25    F107D from APF107.DAT  F107D user input (oarr(41))         t
#C   26    foF2 storm model       no storm updating                   t
#C   27    IG12 from file         IG12 - user                         t
#C   28    spread-F probability 	 not computed                    false
#C   29    IRI01-topside          new options as def. by JF(30)   false
#C   30    IRI01-topside corr.    NeQuick topside model   	     false
#C (29,30) = (t,t) IRIold, (f,t) IRIcor, (f,f) NeQuick
#C   31    B0,B1 ABT-2009	     B0 Gulyaeva-1987 h0.5               t
#C (4,31) = (t,t) Bil-00, (f,t) ABT-09, (f,f) Gul-87, (t,f) not used
#C   32    F10.7_81 from file     F10.7_81 - user input (oarr(46))    t
#C   33    Auroral boundary model on/off  true/false	             false
#C   34    Messages on            Messages off                        t
#C   35    foE storm model        no foE storm updating           false
#C   36    hmF2 w/out foF2_storm  with foF2-storm                     t
#C   37    topside w/out foF2-storm  with foF2-storm                  t
#C   38    turn WRITEs off in IRIFLIP   turn WRITEs on                t
#C   39    hmF2 (M3000F2)         new models                      false
#C   40    hmF2 AMTB-model        Shubin-COSMIC model                 t
#C   41    Use COV=F10.7_12       COV=f(IG12) (IRI before Oct 2015)   t
#C   42    Te with PF10.7 dep.	 w/o PF10.7 dependance               t
#C      ....
#C   50
#C   ------------------------------------------------------------------
#C
#C  Depending on the jf() settings additional INPUT parameters may
#c  be required:
#C
#C       Setting              INPUT parameter
#C    -----------------------------------------------------------------
#C    jf(8)  =.false.     OARR(1)=user input for foF2/MHz or NmF2/m-3
#C    jf(9)  =.false.     OARR(2)=user input for hmF2/km or M(3000)F2
#C    jf(10 )=.false.     OARR(15),OARR(16)=user input for Ne(300km),
#C       Ne(400km)/m-3. Use OARR()=-1 if one of these values is not
#C       available. If jf(23)=.false. then Ne(300km), Ne(550km)/m-3.
#C    jf(13) =.false.     OARR(3)=user input for foF1/MHz or NmF1/m-3
#C    jf(14) =.false.     OARR(4)=user input for hmF1/km
#C    jf(15) =.false.     OARR(5)=user input for foE/MHz or NmE/m-3
#C    jf(16) =.false.     OARR(6)=user input for hmE/km
#C    jf(17) =.flase.     OARR(33)=user input for Rz12
#C    jf(21) =.true.      OARR(41)=user input for daily F10.7 index
#C    jf(23) =.false.     OARR(41)=user input for daily F10.7 index
#C    jf(24) =.false.     OARR(41)=user input for daily F10.7 index
#C          optional for jf(21:24); default is F10.7D=COV
#C    jf(25) =.false.     OARR(41)=user input for daily F10.7 index
#C          if oarr(41).le.0 then 12-month running mean is
#C          taken from internal file]
#C    jf(27) =.false.     OARR(39)=user input for IG12
#C    jf(28) =.true.      OARR(41)=user input for daily F10.7 index
#C
#C
#C  OUTPUT:  OUTF(1:20,1:1000)
#C               OUTF(1,*)  ELECTRON DENSITY/M-3
#C               OUTF(2,*)  NEUTRAL TEMPERATURE/K
#C               OUTF(3,*)  ION TEMPERATURE/K
#C               OUTF(4,*)  ELECTRON TEMPERATURE/K
#C               OUTF(5,*)  O+ ION DENSITY/% or /M-3 if jf(22)=f
#C               OUTF(6,*)  H+ ION DENSITY/% or /M-3 if jf(22)=f
#C               OUTF(7,*)  HE+ ION DENSITY/% or /M-3 if jf(22)=f
#C               OUTF(8,*)  O2+ ION DENSITY/% or /M-3 if jf(22)=f
#C               OUTF(9,*)  NO+ ION DENSITY/% or /M-3 if jf(22)=f
#C                 AND, IF JF(6)=.FALSE.:
#C               OUTF(10,*)  CLUSTER IONS DEN/% or /M-3 if jf(22)=f
#C               OUTF(11,*)  N+ ION DENSITY/% or /M-3 if jf(22)=f
#C               OUTF(12,*)
#C               OUTF(13,*)
#C  if(jf(24)    OUTF(14,1:11) standard IRI-Ne for 60,65,..,110km
#C     =.false.)        12:22) Friedrich (FIRI) model at these heights
#C                      23:33) standard Danilov (SW=0, WA=0)
#C                      34:44) for minor Stratospheric Warming (SW=0.5)
#C                      45:55) for major Stratospheric Warming (SW=1)
#C                      56:66) weak Winter Anomaly (WA=0.5) conditions
#C                      67:77) strong Winter Anomaly (WA=1) conditions
#C               OUTF(15-20,*)  free
#c
#C            OARR(1:100)   ADDITIONAL OUTPUT PARAMETERS
#C
#C      #OARR(1) = NMF2/M-3           #OARR(2) = HMF2/KM
#C      #OARR(3) = NMF1/M-3           #OARR(4) = HMF1/KM
#C      #OARR(5) = NME/M-3            #OARR(6) = HME/KM
#C       OARR(7) = NMD/M-3             OARR(8) = HMD/KM
#C       OARR(9) = HHALF/KM            OARR(10) = B0/KM
#C       OARR(11) =VALLEY-BASE/M-3     OARR(12) = VALLEY-TOP/KM
#C       OARR(13) = TE-PEAK/K          OARR(14) = TE-PEAK HEIGHT/KM
#C      #OARR(15) = TE-MOD(300KM)     #OARR(16) = TE-MOD(400KM)/K
#C       OARR(17) = TE-MOD(600KM)      OARR(18) = TE-MOD(1400KM)/K
#C       OARR(19) = TE-MOD(3000KM)     OARR(20) = TE(120KM)=TN=TI/K
#C       OARR(21) = TI-MOD(430KM)      OARR(22) = X/KM, WHERE TE=TI
#C       OARR(23) = SOL ZENITH ANG/DEG OARR(24) = SUN DECLINATION/DEG
#C       OARR(25) = DIP/deg            OARR(26) = DIP LATITUDE/deg
#C       OARR(27) = MODIFIED DIP LAT.  OARR(28) = Geographic latitude
#C       OARR(29) = sunrise/dec. hours OARR(30) = sunset/dec. hours
#C       OARR(31) = ISEASON (1=spring) OARR(32) = Geographic longitude
#C      #OARR(33) = Rz12               OARR(34) = Covington Index
#C       OARR(35) = B1                 OARR(36) = M(3000)F2
#C      $OARR(37) = TEC/m-2           $OARR(38) = TEC_top/TEC*100.
#C      #OARR(39) = gind (IG12)        OARR(40) = F1 probability
#C      #OARR(41) = F10.7 daily        OARR(42) = c1 (F1 shape)
#C       OARR(43) = daynr              OARR(44) = equatorial vertical
#C       OARR(45) = foF2_storm/foF2_quiet         ion drift in m/s
#C      #OARR(46) = F10.7_81           OARR(47) = foE_storm/foE_quiet
#C       OARR(48) = spread-F probability
#C       OARR(49) = Geomag. latitude   OARR(50) = Geomag. longitude
#C       OARR(51) = ap at current time OARR(52) = daily ap
#C       OARR(53) = invdip/degree      OARR(54) = MLT-Te
#C       OARR(55) = CGM-latitude       OARR(56) = CGM-longitude
#C       OARR(57) = CGM-MLT            OARR(58) = CGM lat eq. aurl bodry
#C       OARR(59) = CGM-lati(MLT=0)    OARR(60) = CGM-lati for MLT=1
#C       OARR(61) = CGM-lati(MLT=2)    OARR(62) = CGM-lati for MLT=3
#C       OARR(63) = CGM-lati(MLT=4)    OARR(64) = CGM-lati for MLT=5
#C       OARR(65) = CGM-lati(MLT=6)    OARR(66) = CGM-lati for MLT=7
#C       OARR(67) = CGM-lati(MLT=8)    OARR(68) = CGM-lati for MLT=9
#C       OARR(69) = CGM-lati(MLT=10)   OARR(70) = CGM-lati for MLT=11
#C       OARR(71) = CGM-lati(MLT=12)   OARR(72) = CGM-lati for MLT=13
#C       OARR(73) = CGM-lati(MLT=14)   OARR(74) = CGM-lati for MLT=15
#C       OARR(75) = CGM-lati(MLT=16)   OARR(76) = CGM-lati for MLT=17
#C       OARR(77) = CGM-lati(MLT=18)   OARR(78) = CGM-lati for MLT=19
#C       OARR(79) = CGM-lati(MLT=20)   OARR(80) = CGM-lati for MLT=21
#C       OARR(81) = CGM-lati(MLT=22)   OARR(82) = CGM-lati for MLT=23
#C       OARR(83) = Kp at current time OARR(84) = magnetic declination
#C       OARR(85) = L-value            OARR(86) = dipole moment
#C                # INPUT as well as OUTPUT parameter
#C                $ special for IRIWeb (only place-holders)
#c-----------------------------------------------------------------------
#C*****************************************************************
#C*** THE ALTITUDE LIMITS ARE:  LOWER (DAY/NIGHT)  UPPER        ***
#C***     ELECTRON DENSITY         60/80 KM       1500 KM       ***
#C***     TEMPERATURES               60 KM        2500/3000 KM  ***
#C***     ION DENSITIES             100 KM        1500 KM       ***
#C*****************************************************************
#C*****************************************************************
#C*********            INTERNALLY                    **************
#C*********       ALL ANGLES ARE IN DEGREE           **************
#C*********       ALL DENSITIES ARE IN M-3           **************
#C*********       ALL ALTITUDES ARE IN KM            **************
#C*********     ALL TEMPERATURES ARE IN KELVIN       **************
#C*********     ALL TIMES ARE IN DECIMAL HOURS       **************
#C*****************************************************************
#C*****************************************************************
#C*****************************************************************

  .Fortran("read_ig_rz",
           PACKAGE="IRI2016")
  .Fortran("readapf107",
           PACKAGE="IRI2016")

  jmag  <- 0
  alati <- latitude
  along <- longitude
  iyyyy <- as.integer(time[1])
  ddd   <- ( as.numeric( ISOdate(time[1],1,1,0,0,0) ) - as.numeric( ISOdate(time[1],time[2],time[3],0,0,0) ) ) / ( 3600 * 24 ) - 1
  dhour <- ( as.numeric( ISOdate(time[1],time[2],time[3],time[4],time[5],time[6]) ) - as.numeric( ISOdate(time[1],time[2],time[3],0,0,0) ) ) / 3600 + 25
  outf  <- matrix(0,ncol=1000,nrow=20)
  oarr  <- rep(0,100)

  rlist <- .Fortran("IRI_SUB"          ,
           jf     = as.logical(jf)   ,
           jmag   = as.integer(jmag) ,
           alati  = as.single(alati) ,
           along  = as.single(along) ,
           iyyyy  = as.integer(iyyyy),
           mmdd   = as.integer(ddd)  ,
           dhour  = as.single(dhour) ,
           heibeg = as.single(heibeg),
           heiend = as.single(heiend),
           heistp = as.single(heistp),
           outf   = as.single(outf)  ,
           oarr   = as.single(oarr)  ,
           PACKAGE="IRI2016"
           )

  iri <- matrix(rlist$outf,ncol=1000,byrow=F)
  dimnames(iri)     <- list(c('e-','Tn','Ti','Te','O+','H+','He+','O2+','NO+','cluster','N+','','','','','','','','',''),seq(heibeg,heiend,by=heistp))

  # then sneak out the msis parameters, IRI contains MSIS but IRI_SUB does not return the values..
  # the IRI msis is not quite reliable, should use the real msis instead...
  imn <- as.integer(time[2])
  id <- as.integer(time[3])
  f107d <- f107pd <- f10781 <- f107365 <- iapda <- isdate <- c(0)


  # ap indices and F10.7
  apf <- .Fortran("APF_ONLY",
                  iyyyy = as.integer( iyyyy ),
                  imn = as.integer( imn ),
                  id = as.integer( id ),
                  f017d = as.single( f107d ),
                  f107pd = as.single( f107pd ),
                  f10781 = as.single( f10781 ),
                  f107365 = as.single( f107365 ),
                  iapda = as.integer( iapda ),
                  isdate = as.integer( isdate ),
                  PACKAGE="IRI2016"
                  )

  msis <- matrix( 0 , nrow=11 , ncol=1000 )
  mass <- as.integer( 48 )
  D <- rep(0,9)
  T <- rep(0,2)
  iyd <- as.integer( iyyyy%%100 * 1000 - ddd )
  sec <- ( time[4]*60 + time[5] ) * 60 + time[6]
  stl <- sec / 3600 + along / 15
  k <- 1

  # GTD7 output in m^-3 and kg
  .Fortran("METERS",
           METER = as.logical( TRUE ),
           PACKAGE="IRI2016"
           )

  ## # force the model to use daily AP only
  ## sw <- rep( 1 , 23)
  ## sw[9] <- 0
  ## .Fortran("TSELEC",
  ##          sv = sw,
  ##          PACKAGE="IRI2016"
  ##          )

  for( h in seq( heibeg , heiend , by=heistp ) ){
      if( h < 80 ){
          f107a <- 150
          f107 <- 150
          ap <- 4
      }else{
          f107a <- apf$f10781
          f107 <- apf$f107pd
          ap <- apf$iapda
      }
      olist <- .Fortran("GTD7",
                        iyd = as.integer( iyd ),
                        sec = as.single( sec ),
                        alt = as.single( h ),
                        glat = as.single( alati ),
                        glon = as.single( along ),
                        stl = as.single( stl ),
                        f107a = as.single( f107a ),
                        f107 = as.single( f107 ),
                        ap = as.single( ap ),
                        mass = as.integer( mass ),
                        D = as.single( D ),
                        T = as.single( T ),
                        PACKAGE="IRI2016"
                        )
      msis[,k] <- c( olist[["D"]] , olist[["T"]] )
      k <- k + 1
  }

  dimnames( msis ) <- list(c('He','O','N2','O2','Ar','mass','H','N','Oanom','Tex','T'),seq(heibeg,heiend,by=heistp))

  return( list(iri=iri , msis=msis ) )

} # iri



iriParams <- function(time=c(2009,7,1,11,0,0),latitude=69.5864,longitude=19.2272,heights=seq(1,1000))
    {
        if(max(heights)>1000) warning("Maximum height is 1000 km, returning iri parameters at 1000 km for heights above 1000 km.")
        if(min(heights)<0) warning("Height must be positive, returning iri parameters at zero height for heights belove zero.")

        heibeg <- min( max( min( heights ) , 0 ) , 1000 )
        heiend <- max( min( max( heights ) , 1000 ) , 0 ) + 1 # add 1 to make sure that heibeg + 999*heistp <= heiend also in the 32-bit representation used by the FORTRAN routines
        heistp <- (heiend-heibeg)/999

        irih   <- seq(heibeg,heiend,by=heistp)

        iripar <- iri( time=time , latitude=latitude , longitude=longitude , heibeg=heibeg , heiend=heiend , heistp=heistp )

        nh <- length(heights)
        parmat <- matrix(nrow=21,ncol=nh)
        dimnames(parmat) <- list(c('e-','O+','H+','He+','O2+','NO+','cluster','N+','He','O',
                                   'N2','O2','Ar','H','N','Oanom','mass','Tn','Ti','Te','Tex'))
        for(n in dimnames(iripar$iri)[[1]]){
            if(nchar(n)>0) parmat[n,] <- spline(irih,iripar$iri[n,],xout=heights)[["y"]]
        }

        for(n in dimnames(iripar$msis)[[1]]){
            if(nchar(n)>0){
                if(n!='T') parmat[n,] <- spline(irih,iripar$msis[n,],xout=heights)[["y"]]
            }
        }

        # do not use the spline below heibeg or above heiend
        if(any( linds <- (heights<heibeg ) ) | any( uinds <- ( heights>heiend ) ) ){

            for(n in dimnames(iripar$iri)[[1]]){
                if(nchar(n)>0) parmat[n,linds] <- iripar$iri[n,1]
                if(nchar(n)>0) parmat[n,uinds] <- iripar$iri[n,1000]
            }

            for(n in dimnames(iripar$msis)[[1]]){
                if(nchar(n)>0){
                    parmat[n,linds] <- iripar$msis[n,1]
                    parmat[n,linds] <- iripar$msis[n,1000]
                }
            }

        }

        # try to replace missing temperatures with the msis neutral temperature
        tmsis <- spline( irih , iripar$msis['T',] , xout=heights )[["y"]]
        for( h in seq(nh) ){
            parmat['Tn',h] <- max( parmat['Tn',h] , tmsis[h] )
            parmat['Ti',h] <- max( parmat['Ti',h] , tmsis[h] )
            parmat['Te',h] <- max( parmat['Te',h] , tmsis[h] )
        }

        return(parmat)

    }
