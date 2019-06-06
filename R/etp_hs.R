#' Potential evapo-transpiration sensu Hargreaves-Samani
#' @description Calculates potential evapo-transpiration from air temperature extremes using the approach by Hargreaves&Samani (1985) and popular corrections.
#' @param tmin minimum air temperature (°C)
#' @param tmax maximum air temperature (°C)
#' @param tmean mean air temperature (°C). Deafults to (tmin+tmax)/2
#' @param alt elevation above sea level (m)
#' @param kmethod Method to use for estimating the kt parameter in the Hargreaves equation. One of "anadale", "allen", "samani".
#' @param ke calibration coefficient for Hargreaves' ETP function
#' @param kt calibration coefficient for Hargreaves' irradiance estimation.
#' @return Potential evapo-transpiration in mm per period.
#' @examples etp_hs(lat=20,day=45,tmin=1,tmax=7)
#' @export
etp_hs<-function(lat,day,tmin,tmax,tmean=(tmax+tmin)/2,alt,ke=0.0135,kt=.16,kmethod,...){

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
  return(ke*kt*rad_toa(lat,day)*(tmax-tmin)^(1/2)*(tmean+17.8))
}
#' TODO: check
