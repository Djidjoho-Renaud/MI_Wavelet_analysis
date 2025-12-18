
midpoints <-function (x, dp = 2) {
  lower <- as.numeric(gsub(",.*", "", gsub("\\(|\\[|\\)|\\]", 
                                           "", x)))
  upper <- as.numeric(gsub(".*,", "", gsub("\\(|\\[|\\)|\\]", 
                                           "", x)))
  return(round(lower + (upper - lower)/2, dp))
}




ustarThreshold <-function (data) {
  data1 <- data
  data_night= filter(data1,Day_or_nigth_1.2==0)      
  data_night$Day=yday(data_night$DateUTC)
  temp.breaks = as.vector(quantile(data_night$T_meteo, 
                                   seq(0, 1, length = 8), na.rm = TRUE))
  temp.breaks=unique(temp.breaks)
  data_night$temp.class = cut(data_night$T_meteo, breaks = temp.breaks, 
                              include.lowest = TRUE)
  unique.tc = unique(data_night$temp.class)
  t = data.frame(Day = numeric(), ustra.class = factor())
  options(warn = -1)
  for (i in 1:length(unique.tc)) {
    index <- which(data_night$temp.class == unique.tc[i])
    if (length(index) != 0) {
      t.df = data_night[index, c("Day", "Ustar_OK", "temp.class")]
      ustar.breaks = as.vector(quantile(t.df$Ustar_OK, seq(0, 
                                                           1, length = 21), na.rm = TRUE))
      t.df$ustar.class = cut(t.df$Ustar_OK, breaks = ustar.breaks, 
                             include.lowest = TRUE)
      t = rbind(t, t.df[, c("Day", "ustar.class")])
    }
  }
  data_night = merge(data_night, t, by.x = "Day", by.y = "Day", 
                     all.x = TRUE)
  df = aggregate(cbind(NEE, Ustar_OK) ~ temp.class + ustar.class, 
                 data = data_night, mean)
  untc = unique(df$temp.class)
  ust = array(NA, dim = c(0, length(untc)))
  for (i in 1:length(untc)) {
    index <- which(df$temp.class == untc[i])
    sub.df <- df[index, ]
    sub.df$ustar.class.midpoints <- midpoints(sub.df$ustar.class)
    higher <- which(sub.df$Ustar_OK > quantile(sub.df$Ustar_OK, probs = 0.1, 
                                               na.rm = TRUE))
    M = mean(sub.df$NEE[higher], na.rm = TRUE)
    G <- which(sub.df$NEE >= M * 0.99)
    
    if(identical(G,integer(0))){
      ust[i] <- NA
    }
    if(!identical(G,integer(0))){
      ust[i] = sub.df$Ustar_OK[which(sub.df$NEE[which(sub.df$NEE >= M * 0.99)] == max(sub.df$NEE[which(sub.df$NEE >= 
                                                                                                         M * 0.99)]))]
    }
    
    
    
  }
 # ustar.threshold = median(ust, na.rm = TRUE)
  return(ust)
}