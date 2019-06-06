#' @title Estimation of global radiation based on temperature differences
#' @param day julian day (day of the year)
#' @param lat latitude in degrees
#' @param tmin,tmax minimum and maximum temperatures for the period in question.
#' @param kt regression parameter for Hargreaves formula. 0.162 (default) for interior, 0.19 for coastal regions.
#' @param trmax maximum atmospheric transmissivity (default .75) used for Bristow method. The default value of .75 is the sum of the default Ångström a (.25) and b (.5) coefficients for the direct and diffuce radiation components.
#' @param alt altitude in metres above sea level
#' @param method Method to use: One of "hargreaves" (default) or "bristow".
#' @param kmethod Method to use for estimating the kt parameter in the Hargreaves equation. One of "anadale", "allen", "samani".
#' @description Estimate solar radiation from temperature extremes. Implements some popular equations and adjustments.
#' @return Daily sum of incoming solar radiation in MJ/m²
#' @section Details:
#'   \code{temp2rad} estimates daily sum of incoming solar radiation (bottom of atmosphere) from air temperature extremes.
#'   Available methods are sensu Bristow & Campbell 1984 or sensu Hargreaves & Samani1982.
#'   For the Hargreaves method, the calibration parameter kt can be adjusted o altitude according to Allen (1995), Samani (2000), Annandale (2002).
#'   In the Bristow 1984 paper, a different method for calculating extraterrestrial radiation is used. This is also applied
#' #' temp2rad
#' @references
#' Bandyopadhyay, A., Bhadra, A., Raghuwanshi, N. S., & Singh, R. (2008). Estimation of monthly solar radiation from measured air temperature extremes. Agricultural and Forest Meteorology, 148(11), 1707–1718. https://doi.org/10.1016/j.agrformet.2008.06.002
#' Bristow, K. L., & Campbell, G. S. (1984). On the relationship between incoming solar radiation and daily maximum and minimum temperature. Agricultural and Forest Meteorology, 31(2), 159–166. https://doi.org/10.1016/0168-1923(84)90017-0
#  Ertekin, C., & Yaldız, O. (1999). Estimation of monthly average daily global radiation on horizontal surface for Antalya (Turkey). Renewable Energy, 17(1), 95–102. https://doi.org/10.1016/S0960-1481(98)00109-8
#' H. Hargreaves, G., & Samani, Z. (1985). Reference Crop Evapotranspiration From Temperature. Applied Engineering in Agriculture, 1. https://doi.org/10.13031/2013.26773
#' Maluta, E. N., Mulaudzi, T. S., & Sankaran, V. (2014). Estimation of the Global Solar Radiation on the Horizontal Surface from Temperature Data for the Vhembe District in the Limpopo Province of South Africa. International Journal of Green Energy, 11(5), 454–464. https://doi.org/10.1080/15435075.2013.772518
#' Ollila, A. (o. J.). Dynamics between Clear, Cloudy and All-Sky Conditions: Cloud Forcing Effects. 20.
#' Samani, Z. (2000). Estimating Solar Radiation and Evapotranspiration Using Minimum Climatological Data (Hargreaves-Samani equation). 13.
#'
#' @export
temp2rad<-function(lat,day,tmin,tmax,kt=.162,alt=0,trmax=.75,method=c("hargreaves"),kmethod=c("allen")){
#method="bristow"
  # see also Bristow&Campbell, 1984 AFM
  # and Bandyopadhyay 2008, AFM
  #methods <- c("hargreaves", "bristow")
  if (pmatch(method,"bristow",nom=0)==1) {
    gatesrad<-function(lat,day){
      # formula for extraterrestrial radiation used in bristow & campbell
      decl<-deg2rad(declination(day))
      lat_rad<-deg2rad(lat)
      hs<-acos(-tan(deg2rad(lat))*tan(decl))
      86400*1360*rel_dist(day)^2*(hs*sin(lat_rad)*sin(decl) +  cos(lat_rad)*cos(decl)*sin(hs))/(pi*1e6)
    }


    B=(.036*exp(-.154*(tmax-tmin)))
    return(gatesrad(lat,day)*trmax*(1-exp(-B*(tmax-tmin)^2.4)))
  }
  else if (pmatch(method,"hargreaves",nom=1))  {

    kr<-kt

    #allen (poor above 1500m)
    if (pmatch(kmethod,"allen",nom=0)==1) {
      if (abs(alt)>1500) warning ("Recalibration method not recommended for given altitude")
      kr<-(kt+.01)*sqrt(alt_pressure(alt)/alt_pressure(0))
    }

    #annandale
    if (pmatch(kmethod,"annandale",nom=0)==1) {
      cat("method anandale")
      kr<-(1+2.7e-5*alt)*kt
    }

    #samani 2000
    if (pmatch(kmethod,"samani",nom=0)==1) {
      if (abs(lat)>50 | abs(lat)<7) warning ("Method Samani not recommended for given latitude")
      cat("method samani")
      kr<-0.00185*(tmax-tmin)^2-.0433*(tmax-tmin)+.4023
    }
  } else stop("unknown kr correction method")

  return(kr*rad_toa(lat,day)*sqrt(tmax-tmin))
}

