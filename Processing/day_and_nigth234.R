#D?termination du temps de couch? de soleil et du lev? du soleil(Ham,2005)
require(lubridate)
day_and_nigth<-function(lon,lat,TOTO){
  #Latitude en deg et en radiant  1.60457,9.74484,Data_test
  Latitude_deg=lat
  Latitude_rad=Latitude_deg*pi/180
  
  #Declinaisons du Soleil
  #Argument du sin
  argument_sin<-((2*pi/365)*yday(ymd_hms(TOTO$DateUTC))-1.39)
  # Angle [rad]
  angle<-sin(argument_sin) 
  # d?clinaison [rad]
  #Declinaison maximale en rad
  declinaison_rad<-0.409*angle
  declinaison_deg<-declinaison_rad*180/pi
  # DUREE DU JOUR [h]
  day_pi<-(24/(2*pi))
  
  length_day<-acos(-tan(Latitude_rad)*tan(declinaison_rad))
  length_day_deg<-length_day*180/pi
  length_day_deg1<-2*length_day_deg/15
  
  #Equation du temps by Carruthers et al .(1990)
  B<-(2*pi/364)*(yday(ymd_hms(TOTO$DateUTC))-81)
  time_argument<-((2*pi*yday(ymd_hms(TOTO$DateUTC))/366)+4.8718)
  
  Eq_time<-(5.0323-430.847*cos(time_argument)+12.5024*cos(2*time_argument)+18.25*cos(3*time_argument)-100.976*sin(time_argument)+595.275*sin(2*time_argument)+3.6858*sin(3*time_argument)-12.47*sin(4*time_argument))/60
  
  Eq_time1<-(9.87*sin(2*B)-7.53*cos(B)-1.5*sin(B))
  
  #Longititude en deg et en radiant 
  Longititude_deg=lon
  Longititude_rad=Longititude_deg*pi/180
  
  #Local Standard Time Meridian (LSTM)
  
  LSTM<-15*1 #1 pour fuseau horaire benin
  
  #Local standard time solar noon (SN)
  SN<-(12-(Eq_time1/60)-((LSTM-Longititude_deg)/15))
  
  # TEMPS  DE LEVER ET COUCHER DU SOLEIL [h]
  
  Tcs=((length_day_deg1/2)+SN)
  Tls=(SN-(length_day_deg1/2))
  
  # TEMPS CIVIL(LOCAL) DE LEVER ET COUCHER DU SOLEIL [h]
  TCcs<-Tcs+0.882
  TCls<-Tls+0.882
  
  #Etiquette des journ?es
  Etiquette<-c(rep(0,length(TOTO$DateUTC)))
  heure<-hour(ymd_hms(TOTO$DateUTC))
  TOTO$hour <- heure
  minute<-minute(ymd_hms(TOTO$DateUTC))
  TOTO$minute <- minute
  for (n in 1:length(TOTO$DateUTC)) {
    if(is.nan(TOTO$Swin[n])==TRUE & TOTO$Swin[n]>=5 ){
      Etiquette[n]=1
    }
    
  }
  for (n in 1:length(TOTO$DateUTC)) {
    if(TCls[n]<(TOTO$hour[n]+TOTO$minute[n]/60) & TCcs[n]>(TOTO$hour[n]+TOTO$minute[n]/60) ){
      Etiquette[n]=1
    }
    
  }
  
  return(Etiquette)
  
  
}
