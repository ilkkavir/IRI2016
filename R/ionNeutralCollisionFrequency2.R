ionNeutralCollisionFrequency2 <- function( param=c('Tn'=0,'N2'=0,'O2'=0,'O'=0) )
  {
      # Ion-neutral collision frequencies for a mixture (75/25%) of NO+/O2+, O+, and e-
      # using Eq. 10 in Brekke et Hall, 1988.
      # Known misprints in the paper have been corrected (8.9e-16->8.9e-17 in nu_en and
      # 3.42->0.342 in nu_i2n)
      #
      #
      # INPUT:
      #  param  a vector with named elements Tn, N2, O2, and O, which are the neutral temperature [k],
      #         and number densities [m^-3] of N2, O2, and O. The vector can contain unlimited number of
      #         other named elements. A row of the iriParams output list is sufficient input.
      #
      # OUTPUT:
      #  a vector with named elements
      #   i1 ion-neutral collision frequency for ion 1 (NO+&O2+)
      #   i2 ion-neutral collision frequency for ion 2 (O+)
      #   e  electron-neutral collision frequency


      coll <- c('i1'=NA,'i2'=NA,'i3'=NA)
      coll["i1"] <- ( 4.23 * param['N2'] + 4.18 * param['O2'] + 2.38 * param['O'] ) * 1e-16
      
      coll["i2"] <- ( 6.82 * param['N2'] + 6.66 * param['O2'] +
                      .342 * param['O'] * sqrt(param['Tn']) * (1.08-0.139*log10(param['Tn'])) +
                      4.51e-3 * log10(param['Tn'])^2 ) * 1e-16
      
      coll["e"] <- 2.33e-17 * param['N2'] * ( 1 - 1.21e-4 * param['Tn'] ) * param['Tn']  +
          1.82e-16 * param['O2'] * ( 1 + 3.6e-2 * sqrt(param['Tn']) ) * sqrt(param['Tn']) +
          8.9e-17 * param['O'] * ( 1 + 5.7e-4 * param['Tn'] ) * sqrt(param['Tn'])

      return(coll)

  }
