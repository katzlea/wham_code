date_convert<-function(x) {
  substr(x,1,2)->day_str
  substr(x,4,5)->month_str
  substr(x,7,10)->year_str
  substr(x,12,19)->hour_str
  date_str<-paste0(year_str,"-",month_str,"-",day_str," ",hour_str)
  return(date_str)
}