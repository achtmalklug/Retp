#' Aerodynamic resistance
#' @description Calculation of the aerodynamic resistance for a given crop surface. Defaults to the FAO grass reference surface.
#' @param wz wind speed at height zw (default 2m/s)
#' @param zw height of wind measurements in m (default 2m).
#' @param zh height of humidity measurements in m (default 2m).
#' @param h crop height in m (defaults to 0.12m).
#' @param zrm roughness length governing momentum transfer in m (default 0.123*h)
#' @param zrh roughness length governing heat transfer in m (default 0.1*zrm)
#' @param d zero plane displacement height (default 2/3*h)
#' @param lai Leaf area index
#' @return \code{ra} returns the aerodynamic resistance
#' @export
ra<-function(wz=2,zrm=NULL, zrh=NULL, d, h  = 0.12,  zw  = 2,  zh  = 2){
    if (missing(d))  d = 2 * h / 3
    if (missing(zrm))  zrm = 0.123 * h
    if (missing(zrh))  zrh = 0.1 * zrm
    return ((log((zw - d) / zrm) * log((zh - d) / zrh)) / (0.1681 * wz))
  }

#' @export
rs<-function(rl=100,lai=2.88){
  return(rl/(lai/2))
}
