#' Priestley-Taylor
#' @description WARNING: NOT CHECKED.
#' @description ...
#' @description Calculates daily potential evapo-transpiration in using the approach by Priestley-Taylor.
#' @param rad net radiation (MJ/(m²*day)).
#' @param shf soil heat flux (MJ/(m²*day)).
#' @param temp air temperature (°C).
#' @param p air pressure (kPa)
#' @return \code{ra} Potential evapo-transpiration in mm/d.
#' @examples etp_pt(rad=5,shf=0,temp=20)
#' @export
etp_pt<-function(rad, shf=0, temp=20 , alpha=1.26, p=101.3, lh =2.501 - 0.002361 * temp,...){

  s=satpressslope(temp)
  return ((1/lh)*alpha*(rad-shf)*s/(s+psychro(p)))

}
#' TODO: check
