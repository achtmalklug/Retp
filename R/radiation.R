#' @title Radiation balance
#' @param angstr_a Ångström a coefficient (defaults to .25)
#' @param angstr_b Ångström b coefficient (defaults to .5)
#' @param vp atmospheric water vapour pressure (kPa)
#' @param alt altitude above sea level
#' @param day julian day (day of the year)
#' @param lat latitude in degrees
#' @description Functions to calculate the radiation balance for a given location.
#' @return Daily sum of incoming solar radiation in MJ/m²
#' @section Details:
#'   \code{rad_toa} calculates the daily sum of incoming extraterrestrial (top of atmosphere) radiation.
#' @export
rad_toa<-function(lat, day){
  #Equation 21
  #Extraterrestrial radiation
  Gsc = 0.082 #MJ/(m2day)
  decl = deg2rad(declination(day))
  Sunset = deg2rad(sunsetangle(lat, day))
  lat = deg2rad(lat)
  exrad = ((24 * 60 / pi) * Gsc * inv_rel_dist(day) * (Sunset * sin(lat) * sin(decl) + cos(lat) * cos(decl) * sin(Sunset)))
  return (exrad)
}

#' rad_clearsky
#' @return \code{rad_clearsky} calculates the daily sum of incoming radiation for a given location under clear sky conditions.
#' @examples
#' rad_boa(50,20)
#' @rdname rad_toa
#' @section Details:
#'   \code{rad_clearsky} calculates the daily sum of incoming radiation (bottom of atmosphere) for a given location under clear sky conditions.
#' @export
rad_clearsky<-function(lat, day, angstr_a=.25, angstr_b=.5, alt=0){
  if (missing(lat) | missing(day)) stop("Must provide latitude and day of the year to calculate clear sky insolation")
  #eq 36
  # Rso
  #clear sky radiation (sunshineduration=daylight hours)
  #if (alt == 0) {
  #  Rso = ((angstr_a + angstr_b) * exrad(lat, day))
  #}  else {
  #  Rso = ((angstr_a + angstr_b) * 0.00002 * Z) * exrad(lat, day)
  #}
  return(((angstr_a + angstr_b) * 0.00002 * alt) * rad_toa(lat, day))
}

#' rad_sw
#' @return \code{rad_clearsky} daily net sum of incoming shortwave radiation (MJ/m²).
#' @examples
#' rad_sw()
#' @rdname rad_toa
#' @section Details:
#'   \code{rad_nsw} #' calculates the daily net amount of shortwvae radiation. This is simply (1-albedo)*irradiance. If no irradiance measurements are available, it can alternatively be estimated from sunshine duration and Ångström coefficients (calling \code{ssd2rad} internally). Future versions will allow estimations based on daily cloud cover values.
#' @export
rad_nsw<-function(albedo=.23, rad=NULL, sunshineduration=NULL, ...){
#rad_nsw<-function(lat, day, albedo=.23, angstr_a=.25, angstr_b=.5, sunshinehours=0, rad){
  #eq 38
  #Rns
  #net reflected shortwave radiation (MJ/(m2d))
  # TODO: TODO
  #if (missing(rad)) stop("MISSING")
  if (missing(rad) & !missing(sunshineduration)) {
    rad<-ssd2rad(...)
  }
  #if (missing(rad)) rad<-ssd2rad()
  Rns = ((1 - albedo) * rad )#rs(lat, day, angstr_a, angstr_b, sunshinehours))
  return(Rns)
}

#' rad_lw_em
#' @return \code{rad_lw_em} daily sum of emitted longwave radition (MJ/m²).
#' @examples
#' ssd2rad(50,20)
#' @rdname rad_toa
#' @section Details:
#'   \code{rad_lw_em} estimates daily sum of emitted longwave radiation.
#' @export
rad_nlw<-function(tmin, tmax, vp, rad, rad_actual, rad_clearsky, rad_rel ){
  #eq 39
  #Rnl
  #net longwave radiation emitted during the day MJ/(m2d)
  #if (missing(rad_rel) 1 #& mis& any(c(missing(rad_clearsky),missing(rad_actual)))) stop ("Need either relative shortwave radiation or both actual and maximum (i.e. clear sky) incoming radiation")
  bk = 0.000000004903
  tmin = tmin + 273.2
  tmax = tmax + 273.2
  Rnl = bk * (((tmax^4) + (tmin^4)) / 2) * (0.34 - 0.14 * sqrt(vp)) * (1.35 * rs / Rso - 0.35)
  return(Rnl)
}



#' ssd2rad
#' @return \code{ssd2rad} estimates daily sum of incoming radiation (bottom of atmosphere) from sum of sunshine duration.
#' @examples
#' ssd2rad(50,20)
#' @rdname rad_toa
#' @section Details:
#'   \code{ssd2rad} estimates daily sum of incoming radiation (bottom of atmosphere) from sum of sunshine duration.
#' @export
ssd2rad<-function(lat, day, angstr_a=.25, angstr_b=.5, sunshinehours=0){
  #rs
  #eq. 35
  #daily radiation from sunshine duration (near sea level) MJ/(m2d)
  return((angstr_a + angstr_b * sunshinehours / daylight(lat, day)) * rad_toa(lat, day))
}



