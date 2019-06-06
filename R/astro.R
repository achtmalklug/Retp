#' Astronomic helper functions
#' @title Astronomic helper functions
#' @description Some useful calculations of the astronomic configuration of earth in relation to the sun.
#' @param lat latitude
#' @param day julian day (day of the year)
#' @param deg angle in degrees
#' @section Details:
#'   \code{daylight} calculates the duration of the daylight period for a given date and location. It is based purely on geometrical calculation (i.e. does account for dispersion etc.).
#' @export
daylight<-function(lat, day){
  return(24 * sunsetangle(lat, day) / 180)
}

#' sunsetangle
#' @title Sunset hour angle
#' @return \code{sunsetangle}: sunset angle in degrees.
#' @examples
#' sunsetangle(50,1)
#' @rdname daylight
#' @section Details:
#'   \code{rh2dewpoint} calculates the sunset hour angle for a given latitude and date.
#' @export
sunsetangle<-function(lat, day){
  #Equation 25
  #Sunset hour angle
  c = (-tan(deg2rad(lat)) * tan(deg2rad(declination(day))))
  c = acos(c)
  sunsetangle = (c * 180 / pi)
  return(sunsetangle)
}

#' declination
#' @return \code{declination}: solar declination in degrees.
#' @examples
#' sunsetangle(50,1)
#' @rdname daylight
#' @section Details:
#'   \code{rh2dewpoint} calculates the solar declination angle in degrees for a given day.
#' @export
declination<-function(day){
  #Equation 24
  #solar declination
  return((0.409 * sin(2 * pi * day / 365 - 1.39)) * 180 / pi)
}


deg2rad<-function(deg){
  return(deg * pi / 180)
}

#' rel_dist
#' @return \code{rel_dist}: relative distance earth-sun.
#' @examples
#' rel_dist(50,1)
#' @rdname daylight
#' @section Details:
#'   \code{rel_dist} calculates the relative distance of earth from sun in relation to the mean distance.
#' @export
rel_dist<-function(day){
  #Equation 23
  #inverse relative distance earth-sun
  return (1/(1 + 0.033 * cos(2 * pi * day / 365)))
}

inv_rel_dist<-function(day){
  #Equation 23
  #inverse relative distance earth-sun
  return(1 + 0.033 * cos(2 * pi * day / 365))

}
