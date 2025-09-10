date_convert2<-function(x) {
  substr(x,1,2)->day_str
  substr(x,3,4)->month_str
  substr(x,5,8)->year_str
  date_str<-paste0(year_str,"-",month_str,"-",day_str)
  return(date_str)
}