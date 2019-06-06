#' @title Thermodynamic helper functions
#' @description Various useful calculations and conversion functions.
#' @param p air pressure in kPa
#' @param vp water vapour partial pressure in kPa
#' @param temp air temperature in °C
#' @param dp dewpoint temperature in °C
#' @param rh relative air humidity in \%.
#' @param LAIact active LAI in m²/m².
#' @param rstom stomatal resistance in s/m.
#' @param zom: roughness length for momentum transfer (m)
#' @param zoh: roughness length for heat transfer (m)
#' @param d: zero plane displacement height (m)
#' @param uz: wind measured at zm (m/s)
#' @param h: crop height (m)
#' @param zm: height of wind measurement (m)
#' @param zh: height of humidity measurements (m)
#' @section Details:
#'   \code{magnus} calculates the saturation water vapour pressure (over liquid water surfaces) in kPa for a given temperature using the magnus formula.
#' @export
magnus<-function(temp){
  #kPa
  #temp in °C
  return(0.6116 * exp(17.62 * temp / (243.12 + temp)))
}

#' rh2dewpoint
#' @return \code{rh2dewpoint}: returns dewpoint temperature in °C
#' @examples
#' rh2dewpoint(50,20)
#' @rdname magnus
#' @section Details:
#'   \code{rh2dewpoint} calculates the dewpoint temperature for a given relative air humidity and temperature.
#' @export
rh2dewpoint2 <- function(temp,rh){
  243.04*(log(rh/100)+((17.625*temp)/(243.04+temp)))/(17.625-log(rh/100)-((17.625*temp)/(243.04+temp)))
}

#' rh2vp
#' @return \code{rh2vp}: water vapour pressure in kPa.
#' @examples
#' rh2vp(50,20)
#' @rdname magnus
#' @section Details:
#'   \code{rh2vp} calculates the water vapour pressure from relative air humidity and temperature.
#' @export
rh2vp <- function(temp,rh){
  return (magnus(temp)*rh/100)
}

#' vp2rh
#' @return \code{vp2rh}: relative humidity in \%.
#' @examples
#' rh2vp(50,20)
#' @rdname magnus
#' @section Details:
#'   \code{vp2rh} calculates the relative air humidity from water vapour pressure and temperature.
#' @export
vp2rh <- function(temp,vp){
  return (100*vp/(magnus(temp)))
}


#' dewpoint2rh
#' @return \code{rh2dewpoint} relative air humidity in \%.
#' @examples
#' rh2dewpoint(50,20)
#' @rdname magnus
#' @section Details:
#'   \code{rh2dewpoint} calculates the relative air humidity for a given dewpoint and air and temperature.
#' @export
dewpoint2rh<-function(temp,dp){
  return(100*(exp((17.625*dp)/(243.04+dp))/exp((17.625*temp)/(243.04+temp))))
}

#' dewpoint2vp
#' @return \code{dewpoint2vp} water vapur pressure in kPa.
#' @rdname magnus
#' @section Details:
#'   \code{dewpoint2vp} calculates the relative air humidity for a given dewpoint and air and temperature.
#' @export
dewpoint2rh<-function(temp,dp){
  return(magnus(dp))
}

#' psychro
#' @return psychrometric constant in kPa/°C
#' @rdname magnus
#' @examples
#' psychro(101.3)
#' @section Details:
#'   \code{psychro} calculates the psychrometre constant (water in air) for a given air pressure.
#' @export
psychro<-function(p){
  return((6.53 * (1+.000944)*p)/1000)
}

#' satpressslope
#' @return \code{satpressslope}: slope of the curve in kPa/°C
#' @examples
#' satpressslope(20)
#' @rdname magnus
#' @section Details:
#'   \code{satpressslope} calculates the slope of the water vapour saturation pressure curve in kPa/°C for a given temperature in °C.
#' @export
satpressslope<-function(temp ){
  #kPa/°C
  return(4098 * (0.6108 * exp((17.27 * temp) / (temp + 237.3))) / (temp + 237.3)^2)
}

#' alt_pressure
#' @return \code{alt_pressure}: air pressure in kPa.
#' @examples
#' alt_pressure(1000)
#' @rdname magnus
#' @section Details:
#'   \code{alt_pressure} calculates the atmospheric air pressure at normal conditions for a given altitude above the sea level.
#' @export
alt_pressure<-function(altitude){
  alt_pressure = 101.3 * ((293 - 0.0065 * altitude) / 293)^5.26
  return(alt_pressure)
}

#' rsurf
#' @return \code{rsurf}: canopy surface resistance in s/m (=stomatal resistance/active LAI). Default values for FAO grass reference surface.
#' @examples
#' rsurf()
#' @rdname magnus
#' @section Details:
#'   \code{rsurf}
#' @export
rsurf_<-function(LAIact=1.44,rstom=100){return(rstom/LAIact)}

#' ra
#' @return \code{ra}: aerodynamic resistance in s/m. Default arguments for canopy represent the FAO grass reference surface.
#' @examples
#' ra()
#' @rdname magnus
#' @section Details:
#'   \code{rsurf}
#' @export
ra_<-function(zom, zoh, d=2*h/3 , uz = 2.8,  h = 0.12,  zm  = 2, zh= 2){
  if (missing(zom)) zom<-.123*h
  if (missing(zoh)) zoh<-.1*zom
  return((log((zm - d) / zom) * log((zh - d) / zoh)) / (0.1681 * uz))
}

#' wind2m
#' @return \code{wind2m}:
#' @rdname magnus
#' @section Details:
#'   \code{wind2m}
#' @export
wind2m<-function(windz,z){4.87*windz/(log(67.8*z-5.42))}
