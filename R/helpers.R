##' magnus
##' calculates the water vapour pressure (over liquid water surfaces) in kPa for a given temprature.
##' @param temp temperature in °C
##' @param p pressure in kPa
##' @return \code{magnus} .
##' @examples
##' magnus(100)
##' @rdname elpers
##' @export
magnus<-function(temp){
  #kPa
  #temp in °C
  return(0.6116 * exp(17.62 * temp / (243.12 + temp)))
}

##' psychro
##' calculates the psychrometric constant (water in air)
##' @return
##' @examples
##' psychro(3)
##' @rdname funs
##' @export

psychro<-function(p){
  return((6.53 * (1+.000944)*p)/1000)
}

deg2rad<-function(deg){
  deg2rad = deg * pi / 180
}

##' satpressslope
##' calculates the slope of the water vapour saturation pressure for a given temperature
##' @return slope of the curve in kPa/°C
##' @examples
##' psychro(20)
##' @rdname funs
##' @export
satpressslope<-function(temp ){
  #kPa/°C
  return(4098 * (0.6108 * exp((17.27 * temp) / (temp + 237.3))) / (temp + 237.3)^2)
}
