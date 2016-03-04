ionNeutralCollisionFrequency <- function( param=c('O+'=0,'H+'=0,'He+'=0,'O2+'=0,'NO+'=0,'N+'=0,'N2'=0,'O2'=0,'H2'=0,'O'=0,'H'=0,'He'=0,'N'=0) )
  {
    # Ion-neutral collision frequency using Eq. 10 in
    # Banks, P., "Collision Frequencies and Energy Transfer; Ions", Planet. Space Sci. 14, 1105-1122, 1966.
    # The equation is in cgs, so this function uses cgs internally, but input and output are in SI
    #
    # Notice that the equation is for non-resonant collisions, resonant (charge-exhance) collisions (O+,O), (O2+,O2), (H+,H),...
    # may have larger collision frequencies in reality.
    #
    # 
    #  INPUT:
    #    param  a vector containing any subset of the named elements O+, H+, He+, O2+, NO+. N+, N2, O", H2, O, H, He, N,
    #           where all elements are number densities in SI units. The names with '+' denote positive ions, and the others
    #           neutral atoms. Densities of missing elements are set to zero
    #
    # OUTPUT:
    #    coll    a 6 x 7 named matrix with ions in rows and neutrals in columns.Collision frequyency in s^-1
    #    
    #

    # ion names
    ions <- c('O+','H+','He+','O2+','N+','NO+')

    # neutral names
    neutrals <- c('O','H','He','O2','N','N2','H2')


    # set zeroes to places of missing species
    param[ions][is.na(param[ions])] <- 0
    param[neutrals][is.na(param[neutrals])] <- 0
    
    # neutral gas polarizabilities from Banks, 1966 (cm^3)
    alpha <- c('N2'=1.76, 'O2'=1.60, 'H2'=0.82, 'O'=0.89, 'H'=0.67, 'He'=0.21, 'N'=1.13 ) * 1e-24

    # electron charge in cgs (g^1/2 cm^3/2 s^-1)
    e <- 4.802320425e-10
    
    # atomic mass unit in g
    amu <- 1.66053886e-24
    
    # ion and neutral masses
    mion     <- c('O+'=32,'H+'=1,'He+'=4,'O2+'=32,'NO+'=30,'N+'=14       )  * amu
    mneutral <- c('N2'=28,'O2'=32,'H2'=2,'O'  =16,'H'  =1 ,'He'=4 ,'N'=14) * amu
    
    coll <- matrix(nrow=6,ncol=7)

    # put the names to dimnames of the matrix coll
    dimnames(coll) <- list( ions , neutrals )

    for(ion in ions){
      for(neutral in neutrals){
        coll[ion,neutral] <- ( param[ion]*mion[ion] + param[neutral]*mneutral[neutral] ) / (mion[ion]*mneutral[neutral]) * 2.21 * pi *
                               sqrt( alpha[neutral] * e**2 * mion[ion] * mneutral[neutral] / ( mion[ion] + mneutral[neutral] ) )
      }
    }

    # convert the result back to SI and return
    return(coll*1e-6)

  }
