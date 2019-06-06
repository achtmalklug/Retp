#' Potential evapo-transpiration sensu Thorntwaite (1948)
#' @description WARNING: IMPLEMENTATION IS PRELIMINARY. RESULTS PROBABLY WRONG. Especially for higher latitudes.
#' @description Calculates monthly potential evapo-transpiration in using the approach by Thorntwaite (1948).
#' @param lat latitude (used for calculating daylight hours)
#' @param date either a vector of dates (class "Date") or a julian day (numeric). Since the Thorntwaite method is based on an annual heat index, dates should cover the whole year.
#' @param temp air temperature (°C) for the given dates.
#' @param fillmethod na.approx or na.spline, used inter- and extrapolate insufficient values to the whole year.
#' @return \code{ra} Potential evapo-transpiration in mm/d.
#' @examples etp_th(lat=54,temp=c(1,14,1),dates=c(1,210,365))

#' @export
etp_th<-function(lat,dates,temp,fillmethod=na.approx){
  if(length(dates)!=length(temp)) stop ("Geht nich. Len(temp)!=len(dates)")
  if (class(dates)=="Date") {
    #dates<-strptime(dates,tz="")
    dates_full<-seq.Date(from=min(dates),to=max(dates),by=1)
  } else {
    dates<-as.Date(strptime(dates,format="%j",tz = ""))
    dates_full<-seq.Date(from=min(dates),to=max(dates),by=1)
  }
  if (length(dates_full)<365) stop("Original Thontwait only works if temperature data spans a whole year. ")
  dt<-data.table(date=dates_full)
  dt[match(dates,dates_full),t:=temp]
  dt[,month:=month(date)]
  dt_monthly<-dt[,.(t=mean(t,na.rm=TRUE)),by=month]
  fillmethod<-na.approx
  dt_monthly$t_filled<-fillmethod(dt_monthly$t)

  monthdays<-c(31,28,31, 30,31,30, 31,31,30, 31,30,31)
  #day<-1
  #lat=54
  #perday=F
  #rad<-rad_toa(lat,day)
  #month=as.numeric(format(strptime(day,"%j"),"%m"))
  #jdays1<-sum(monthdays[1:month])
  #jdays2<-sum(monthdays[1:(month+1)])

  i<-(dt_monthly$t_filled/5)^1.541

  #i<-(temp[month]/(5/12))^(1.541)
  #i<-(temp/5)^1.541
  #i<-(temp[month]*12/5)^1.541

  #if (perday){
  dt_monthly$L=daylight(lat,dt_monthly$month*monthdays-0.5*monthdays)

  #} else {
  #  L=daylight(lat,day)
  #}
  #temp<-c(0,4,5,6,8,11,13,12,11,8,6,3)
  #temp<-rep(11,12)
  i<-(dt_monthly$t_filled/5)^1.541
  I=sum(i)
  alpha=6.751e-7*I^3+7.71e-5*I^2+1.792e-2*I+0.49239
  #alpha= 675e-9*i^3+771e-7*i^2+1792e-5*i+0.49239
  #day=40
  #month=2
  #if (daily){
  #  etp=16*(L/12)*(1/30)*(10*temp[month]/i[month])^alpha[month]
  #  etp
  #  }
  #else{

  etp=16*(dt_monthly$L/12)*(monthdays/30)*(10*dt_monthly$t_filled/I)^alpha
  dt_monthly$ETP<-etp
  #etp=16*(dt_monthly$L[6]/12)*(monthdays[6]/30)*(10*temp[6]/I)^alpha
  #  }

  return(dt_monthly[,.(t_filled,month,daylight=L,ETP=etp)])
  #return(ke*rad*sqrt(dtemp)*(temp+17.8))
}
#' TODO: check
