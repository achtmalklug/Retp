#' Estimate potential evapo-transpiration from weather data (1948)
#' @description NOT YET IMPLEMENTED!
#' @description ...
#' @description Estimation method can selected manually or will be chosen automatically based on availability of weather data
#' @description ...
#' @param lat latitude (used for calculating daylight hours)
#' @param weather weather in a data.frame
#' @param columns identification of columns in the weather data.frame
#' @param frequency output time unit: daily or monthly
#' @examples bla()
#' @export
etp_estimate<-function(...){
  stop("Not yet implemented!")
}
# # et0_penmon<-function(Rnet , SHF , temp , vpd , pressure , rsurf , ra , lh=NULL){
# #   #daily potential evapotranspiration in mm
# #   if (is.null(lh))  lh = 2.501 - 0.002361 * temp
# #   airdens = (pressure) / (0.287 * (temp + 273.15))
# #   airhc = psychro(pressure) * 0.622 * lh / pressure
# #   numerator = satpressslope(temp) * (Rnet - SHF) + (86400 * airdens * airhc / ra) * vpd
# #   denominator = satpressslope(temp) + psychro(pressure) * (1 + rsurf / ra)
# #   et0_penmon = (numerator / denominator) / lh
# #   return(et0_penmon)
# # }
# #
# # et0_penmon(10,0,10,1,10.3,100,70,2.45)
#
# library(data.table)
# weather=data.table(date=as.Date(c("2019-01-01","2019-01-02","2019-01-04")),year=2019,doy=c(NA,2,4),tmin=c(10,11,12),maxt=c(20,21,21))
#
# colnames=list(date="date")
# defaultcols=list(date="date",year="year",doy="doy",tmin="tmin",tmax="2",rad="rad")
#
# colnames<-lapply(defaultcols,function(x){ifelse(!is.null(colnames[[x]]),colnames[[x]],x)})
#
# #check dates
# weather[,colnames$date:=as.Date(get(colnames$date))]
# if (any(is.na(weather[,get(colnames$date)]))) stop ("NA values in date column")
#
# period<-1
#
# if (length(unique(diff(as.numeric(weather[,get(colnames$date)]))))>1) warning("Non-equidistant dates")
#
# newweather<-data.table(date=seq(min(weather[,get(colnames$date)]),max(weather[,get(colnames$date)]),by=period))
# newweather<-merge(newweather,weather,by.x="date",by.y=colnames$date,all.x=T)
# newweather[,year:=format(get(colnames$date),"%Y")]
# newweather[,doy:=format(get(colnames$date),"%j")]
#
# introw<-apply(newweather,1,function(x)any(is.na(x)))
#
# newweather[,(colnames$tmin):=na.approx(get(colnames$tmin))]
# newweather[,(colnames$tmin):=na.approx(get(colnames$tmin))]
#
# library(zoo)
# z <- zoo(c(2,NA,1,4,5,2), c(1,3,4,6,7,8))
#
# ## use underlying time scale for interpolation
# na.approx(z)
# ## use equidistant spacing
# na.approx(z, 1:6)
#
# na.approx(zoo(c(NA,9,3,2,3,2)))
# z
#
# baddate<-as.Date(weather[,get(variables$date)])
# weather[,get(variables$date)]<-as.Date(weather[,variables$date])
# weather[variables$date]
# if (!all(apply(weather,1,function(x){!is.na(x[variables$date]) | (!is.na(x[variables$year]) & !is.na(x[variables$doy]))}))) stop("missing date information")
#
# apply (weather,1, function(x) if(x[variables$date]))
#
#
# )weather[variables$tmax]
# etp_estimate<-function(weather,variables=list(tmin=1,tmax=2))
#
# }or

#' TODO: implement
