time_convert<-function(x) {
  substr(x,1,2)->hour_str
  substr(x,4,5)->min_str
  substr(x,7,8)->sec_str
  time_str<-paste0(hour_str,":",min_str,":",sec_str)
  return(time_str)
}