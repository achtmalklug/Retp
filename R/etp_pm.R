#' Potential evapo-transpiration sensu Penman-Monteith
#' @description Calculates daily potential evapo-transpiration in mm.
#' @param rad net radiation (MJ/(m2*day))
#' @param shf soil heat flux (MJ/(m2*day))
#' @param temp air temperature (dC)
#' @param vp air water vapour pressure (kPa)
#' @param rh lative air humidity (\%)
#' @param p air pressure (kPa)
#' @param r_a canopy aerodynamic resistance in s/m
#' @param r_s canopy surface resistance (s/m)
#' @param lai leaf area index
#' @param lh latent head of vapourisation
#' @return \code{ra} Potential evapo-transpiration in mm/d
#' @examples etp_pm(rad=5,shf=0,temp=4,rh=80)
#' @export
etp_pm<-function(rad, shf=0, temp=20 , vpd, rh , p=101.3, r_s=rs(...), r_a=ra(...),  lh =2.501 - 0.002361 * temp,...){

  if ((missing(vpd)+missing(rh))==2) warning ("both rh or vpd given, using vpd.")
  if (missing(vpd)&missing(rh)) stop ("Either relative humidity or vapour pressure must be given")

  #if (missing(rh) & missing(vp)) stop ("Either relative humidity or vapour pressure must be given")
  #if (!missing(rh) & !missing(vp)) warning ("Vapour pressure given, ignoring relative humidity.")
  if (missing(vpd)&!missing(rh)) vpd<-magnus(temp)-rh2vp(temp = temp,rh=rh)

  #vpd=magnus(temp)-vp
  #daily potential evapotranspiration in mm
  #If (IsMissing(lh)) Then lh = 2.501 - 0.002361 * temp

  if (missing(r_a)) r_a<-etp::ra(...)

  airdens = (p) / (0.287 * (temp + 273.15))
  airhc = psychro(p) * 0.622 * lh / p


  numerator = satpressslope(temp) * (rad - shf) + (86400 * airdens * airhc / r_a) * vpd
  denominator = satpressslope(temp) + psychro(p) * (1 + r_s / r_a)

  return  ((numerator / denominator) / lh)
}
#' TODO: check
