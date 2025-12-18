# ============================================================================
# METEOROLOGICAL DATA VISUALIZATION SCRIPT
# Site: Bellefoungou (BJ-Bfg)
#Author: Renaud KOUKOUI
#email: renaud.koukoui@gmail.com
# Figure_3
# ============================================================================

# Clear workspace and load libraries
rm(list = ls(all=TRUE))
library(lubridate)
library(dplyr)
library(zoo)

# ============================================================================
# 1. DATA LOADING
# ============================================================================

Site <- c("Nalohou","Bellefoungou")
ID <- c("Nal","Bel")

# Load meteorological and rainfall data
Data_test_eddyproc_bel <- read.csv(paste("./Table/RF/",Site[2],"/",ID[2],"_FLX_MET_MDS_RF_output_qc_1_new_.csv",sep = ""),sep=",")
Data_test_rainfall_bel <- read.csv(paste("./Table/Rainfall/",Site[2],"/",ID[2],"_Rainfall_halfhour_new_.csv",sep = ""),sep=",")

# Remove rainfall outliers
Data_test_rainfall_bel$Rain[Data_test_rainfall_bel$Rain > 100] <- NA

# Select relevant variables
Dataset_b <- subset(Data_test_eddyproc_bel, select = c("Date","Day_or_nigth_1.2","Rg_orig","Rg_f","Tair_orig","Tair_f","VPD_orig","VPD_f","Tsoil1_orig","Tsoil1_f","Hsoil1_orig","Hsoil1_f","Tsoil2_orig","Tsoil2_f","Hsoil2_orig","Hsoil2_f"))
Dataset_b$Rain <- Data_test_rainfall_bel$Rain[-1]

# ============================================================================
# 2. SEASONAL PERIOD DEFINITIONS 
# ============================================================================

date_debut <- "2008-01-01"
date_fin <- "2008-12-31"
dates_correctes <- seq(from = as.Date(date_debut), to = as.Date(date_fin), by = "1 day")

# Define seasonal periods
Dry_moyenne <- as.Date(c("2008-12-21","2008-12-31"))  
Moistening_moyenne <- as.Date(c("2008-02-04","2008-04-08"))
Wet_moyenne <- as.Date(c("2008-04-09","2008-10-27"))
Drying_moyenne <- as.Date(c("2008-10-28","2008-12-20"))
Dry_moyenne_2 <- as.Date(c("2008-01-01","2008-02-03"))

# Get indices for seasonal periods
Dry_index <- which(is.element(dates_correctes, Dry_moyenne))
Wet_index <- which(is.element(dates_correctes, Wet_moyenne))
Dry_index_2 <- which(is.element(dates_correctes, Dry_moyenne_2))

# ============================================================================
# 3. DATA FILTERING (Keep only nighttime temperatures)
# ============================================================================

Dataset_b$Tair_f[Dataset_b$Day_or_nigth_1.2 == 1] <- NA
Dataset_b$Tsoil1_f[Dataset_b$Day_or_nigth_1.2 == 1] <- NA
Dataset_b$Tsoil2_f[Dataset_b$Day_or_nigth_1.2 == 1] <- NA
Dataset_b$Tsoil1_orig[Dataset_b$Day_or_nigth_1.2 == 1] <- NA
Dataset_b$Tsoil2_orig[Dataset_b$Day_or_nigth_1.2 == 1] <- NA

# Remove problematic soil temperature measurements
Dataset_b$Tsoil1_f[53966:56720] <- NA
Dataset_b$Tsoil2_f[53966:56720] <- NA
Dataset_b$Tsoil1_orig[53966:56720] <- NA
Dataset_b$Tsoil2_orig[53966:56720] <- NA

# Add temporal variables
Day <- yday(ymd_hms(Dataset_b$Date))
Year <- year(Dataset_b$Date)
Dataset_b <- cbind(Year, Day, Dataset_b)

# ============================================================================
# 4. DAILY AGGREGATION
# ============================================================================

# Aggregate daily rainfall
Rain_data_b <- subset(Dataset_b, select = c("Year","Day","Rain"))
daily_rain <- Rain_data_b %>%
  group_by(Year, Day) %>%
  dplyr::summarise(daily_rain = sum(Rain, na.rm = TRUE), .groups = "drop")

df_rain_mean_b <- daily_rain %>%
  group_by(Day) %>%
  dplyr::summarise(Rain = mean(daily_rain, na.rm = TRUE), .groups = "drop") %>%
  arrange(Day)

# Aggregate meteorological variables by day of year
df_met_mean_b <- Dataset_b %>%
  group_by(Day) %>%
  dplyr::summarise(
    n = n(),
    Rg = mean(Rg_f, na.rm = TRUE),
    Rg_min = min(Rg_f, na.rm = TRUE),
    Rg_max = max(Rg_f, na.rm = TRUE),
    Tair = mean(Tair_f, na.rm = TRUE),
    Tair_min = min(Tair_f, na.rm = TRUE),
    Tair_max = max(Tair_f, na.rm = TRUE),
    Tsoil1 = mean(Tsoil1_f, na.rm = TRUE),
    Tsoil1_min = min(Tsoil1_f, na.rm = TRUE),
    Tsoil1_max = max(Tsoil1_f, na.rm = TRUE),
    Tsoil2 = mean(Tsoil2_f, na.rm = TRUE),
    Tsoil2_min = min(Tsoil2_f, na.rm = TRUE),
    Tsoil2_max = max(Tsoil2_f, na.rm = TRUE),
    Hsoil1 = mean(Hsoil1_f, na.rm = TRUE),
    Hsoil2 = mean(Hsoil2_f, na.rm = TRUE)
  )

df_hsoil_mean_b <- Dataset_b[1:105215,] %>%
  group_by(Day) %>%
  dplyr::summarise(
    Hsoil1 = mean(Hsoil1_f, na.rm = TRUE),
    Hsoil2 = mean(Hsoil2_f, na.rm = TRUE)
  )

# ============================================================================
# 5. GAP-FILLING SOIL MOISTURE WITH LOESS
# ============================================================================

# Remove unrealistic values
df_met_mean_b$Hsoil1[1:200][df_met_mean_b$Hsoil1[1:200] >= 0.15] <- NA

df_loess <- df_met_mean_b
original_data <- df_met_mean_b$Hsoil1
valid_indices <- which(!is.na(original_data))
valid_values <- original_data[valid_indices]

if(length(valid_values) > 10) {
  loess_model <- loess(valid_values ~ valid_indices, span = 0.3, degree = 2)
  na_indices <- which(is.na(original_data))
  
  if(length(na_indices) > 0) {
    predicted_values <- predict(loess_model, newdata = data.frame(valid_indices = na_indices))
    df_loess$Hsoil1[na_indices] <- predicted_values
  }
} else {
  df_loess$Hsoil1[is.na(df_loess$Hsoil1)] <- mean(original_data, na.rm = TRUE)
}

X <- df_loess

# ============================================================================
# 6. PLOTTING PARAMETERS
# ============================================================================

cex_pts <- 2
cex_axis <- 2.5
cex_text_axe <- 1.8
cex_leg2 <- 3

lab_Swin <- seq(0,1200,400)
lab_Tair <- seq(10,30,10)

# Define seasonal transition dates (2008-2017)
dates_ss_1 <- paste0(c("2008-01-01", "2008-12-27", "2009-12-27","2010-12-29","2011-12-24","2012-12-22","2013-12-05","2014-12-19","2015-12-09","2016-12-20"), " 00:00:00")
dates_tr_1 <- paste0(c("2008-02-24", "2009-01-27", "2010-02-06","2011-01-21","2012-01-19","2013-02-12","2014-01-21","2015-01-31","2016-02-15","2017-02-23"), " 00:00:00")
dates_sh <- paste0(c("2008-05-01", "2009-04-15", "2010-04-20","2011-04-24","2012-04-07","2013-03-06","2014-04-10","2015-04-30","2016-03-11","2017-04-02"), " 00:00:00")
dates_tr_2 <- paste0(c("2008-10-20", "2009-11-01", "2010-11-02","2011-10-29","2012-11-10","2013-10-26","2014-10-20","2015-11-01","2016-10-24","2017-11-01"), " 00:00:00")

dates_ss_1[1] <- "2008-01-01 00:30:00"

# Get indices for seasonal transitions
indices_ss_1 <- which(Dataset_b$Date %in% dates_ss_1)
indices_tr_1 <- which(Dataset_b$Date %in% dates_tr_1)
indices_sh <- which(Dataset_b$Date %in% dates_sh)
indices_tr_2 <- which(Dataset_b$Date %in% dates_tr_2)

Fin_an <- c(1,365,730,1095,1460,1826,2191,2556,2921,3286) * 48
Fin_an_lab <- c('2008','2009','2010','2011','2012','2013','2014','2015','2016','2017')
ID <- 1:366

# ============================================================================
# 7. CREATE FIGURE
# ============================================================================

png(paste("./","Figure_3.png", sep=""), width=1900, height=1200)

par(mar=c(1.5,4,0.5,5), oma=c(4,6,3,0), bg="white")

# Layout: 5 rows × 2 columns (left wider than right)
layout(matrix(1:10, nrow=5, ncol=2, byrow=TRUE), widths=c(2,1), heights=rep(1,5))

# Helper function to add seasonal shading (wet season)
add_wet_shading <- function() {
  for(i in 1:10) {
    rect(indices_sh[i],-15000,indices_tr_2[i],15000,col=adjustcolor("blue", 0.1),border=adjustcolor("blue", 0.1))
  }
}

# Helper function to add seasonal shading (dry season)
add_dry_shading <- function() {
  for(i in 1:10) {
    rect(indices_ss_1[i],-15000,indices_tr_1[i],15000,col=adjustcolor("red", 0.1),border=adjustcolor("red", 0.1))
  }
}

# --- Panel 1: Rainfall time series ---
barplot(Dataset_b$Rain, col="blue", space=0, border="blue", ylim=c(0,70), ylab="", xlab="", xaxt="n", yaxt="n", lwd=3)
text(x=75000, y=60, "2) Bellefoungou (BJ-Bfg)", cex=2.5, pos=4)
mtext(expression("Rain [mm]"), line=6, side=2, cex=cex_text_axe)
axis(side=2, at=seq(0,60,20), cex.axis=cex_axis, las=1, tck=0.05)
add_wet_shading()
add_dry_shading()
text(x=3, y=50, "a)", cex=4)
abline(v=Fin_an, col="gray40", lty=2)
box()

# --- Panel 2: Average daily rainfall ---
barplot(df_rain_mean_b$Rain, col="blue", space=0, border="blue", ylim=c(0,30), ylab="", xlab="", xaxt="n", yaxt="n", lwd=3)
rect(Wet_index[1],-15000,Wet_index[2],15000,col=adjustcolor("blue", 0.1),border=adjustcolor("blue", 0.1))
rect(Dry_index[1],-15000,Dry_index[2],15000,col=adjustcolor("red", 0.1),border=adjustcolor("red", 0.1))
rect(Dry_index_2[1],-15000,Dry_index_2[2],15000,col=adjustcolor("red", 0.1),border=adjustcolor("red", 0.1))
axis(side=2, at=seq(0,30,10), cex.axis=cex_axis, las=1, tck=0.05)
box()

# --- Panel 3: Radiation time series ---
plot(Dataset_b$Rg_f, type="p", pch=20, ylim=c(-200,1200), col='black', ylab="", xlab="", xaxt="n", yaxt="n", cex=1.5)
mtext("Swin ["*W~m^-2*"]", line=6, side=2, cex=cex_text_axe)
axis(side=2, at=lab_Swin, cex.axis=cex_axis, las=1, tck=0.05)
add_wet_shading()
add_dry_shading()
text(x=3, y=1100, "b)", cex=4)
abline(v=Fin_an, col="gray40", lty=2)
box()

# --- Panel 4: Average daily radiation ---
plot(df_met_mean_b$Rg, type="p", pch=20, ylim=c(130,300), col='black', ylab="", xlab="", xaxt="n", yaxt="n", cex=2.5)
rect(Wet_index[1],-15000,Wet_index[2],15000,col=adjustcolor("blue", 0.1),border=adjustcolor("blue", 0.1))
rect(Dry_index[1],-15000,Dry_index[2],15000,col=adjustcolor("red", 0.1),border=adjustcolor("red", 0.1))
rect(Dry_index_2[1],-15000,Dry_index_2[2],15000,col=adjustcolor("red", 0.1),border=adjustcolor("red", 0.1))
axis(side=2, at=seq(150,300,50), cex.axis=cex_axis, las=1, tck=0.05)
abline(v=c(90,180,270,360), col="gray40", lty=2)
box()

# --- Panel 5: Air temperature time series ---
plot(Dataset_b$Tair_f, type="p", pch=20, col="black", ylim=c(10,38), ylab="", xlab="", xaxt="n", yaxt="n", cex=1.5)
mtext(expression(Tair[night]~"[°C]"), line=6, side=2, cex=cex_text_axe)
axis(side=2, at=lab_Tair, cex.axis=cex_axis, las=1, tck=0.05)
add_wet_shading()
add_dry_shading()
text(x=3, y=36, "c)", cex=4)
abline(v=Fin_an, col="gray40", lty=2)
box()

# --- Panel 6: Average daily air temperature with range ---
plot(c(1, length(df_met_mean_b$Tair)), c(10,38), col="white", xlab="", ylab="", xaxt="n", yaxt="n")
polygon(c(1:length(df_met_mean_b$Tair), length(df_met_mean_b$Tair):1), 
        c(df_met_mean_b$Tair_min, rev(df_met_mean_b$Tair_max)), 
        col=adjustcolor("azure3", 0.3), border=adjustcolor("azure3", 0.8))
lines(ID, df_met_mean_b$Tair, lwd=2, col="black")
rect(Wet_index[1],-15000,Wet_index[2],15000,col=adjustcolor("blue", 0.1),border=adjustcolor("blue", 0.1))
rect(Dry_index[1],-15000,Dry_index[2],15000,col=adjustcolor("red", 0.1),border=adjustcolor("red", 0.1))
rect(Dry_index_2[1],-15000,Dry_index_2[2],15000,col=adjustcolor("red", 0.1),border=adjustcolor("red", 0.1))
axis(side=2, at=seq(10,30,10), cex.axis=cex_axis, las=1, tck=1, lty="dotted", col="lightgray")
abline(v=c(90,180,270,360), col="gray40", lty=2)
box()

# --- Panel 7: Soil temperature time series ---
plot(Dataset_b$Tsoil1_orig, type="l", lwd=3, col="black", ylim=c(20,45), ylab="", xlab="", xaxt="n", yaxt="n")
lines(Dataset_b$Tsoil2_f, lwd=3, col="forestgreen")
lines(Dataset_b$Tsoil2_orig, lwd=3, col="forestgreen")
mtext(expression(Tsoil[night]~"[°C]"), line=6, side=2, cex=cex_text_axe)
axis(side=2, at=seq(20,40,10), cex.axis=cex_axis, las=1, tck=0.05)
add_wet_shading()
add_dry_shading()
legend("topleft", bty="n", xpd=NA, cex=cex_leg2, 
       legend=c(expression(Tsoil[1]), expression(Tsoil[2])), 
       col=c("black","forestgreen"), lty=1, horiz=T)
text(x=3, y=43, "d)", cex=4)
abline(v=Fin_an, col="gray40", lty=2)
box()

# --- Panel 8: Average daily soil temperature with range ---
plot(c(1, length(df_met_mean_b$Tsoil1)), c(20,45), col="white", xlab="", ylab="", xaxt="n", yaxt="n")
polygon(c(1:length(df_met_mean_b$Tsoil1), length(df_met_mean_b$Tsoil1):1), 
        c(df_met_mean_b$Tsoil1_min, rev(df_met_mean_b$Tsoil1_max)), 
        col=adjustcolor("azure3", 0.3), border=adjustcolor("azure3", 0.8))
lines(ID, df_met_mean_b$Tsoil1, lwd=4, col="black")
polygon(c(1:length(df_met_mean_b$Tsoil2), length(df_met_mean_b$Tsoil2):1), 
        c(df_met_mean_b$Tsoil2_min, rev(df_met_mean_b$Tsoil2_max)), 
        col=adjustcolor("forestgreen", 0.3), border=adjustcolor("forestgreen", 0.8))
lines(ID, df_met_mean_b$Tsoil2, lwd=4, col="forestgreen")
rect(Wet_index[1],-15000,Wet_index[2],15000,col=adjustcolor("blue", 0.1),border=adjustcolor("blue", 0.1))
rect(Dry_index[1],-15000,Dry_index[2],15000,col=adjustcolor("red", 0.1),border=adjustcolor("red", 0.1))
rect(Dry_index_2[1],-15000,Dry_index_2[2],15000,col=adjustcolor("red", 0.1),border=adjustcolor("red", 0.1))
axis(side=2, at=seq(20,40,10), cex.axis=cex_axis, las=1, tck=0.05)
abline(v=c(90,180,270,360), col="gray40", lty=2)
box()

# --- Panel 9: Soil moisture time series ---
plot(Dataset_b$Hsoil2_f, type="p", pch=16, ylim=c(0,0.3), col="forestgreen", ylab="", xlab="", xaxt="n", yaxt="n", cex=1.5)
lines(Dataset_b$Hsoil2_orig, type="p", pch=16, col="forestgreen", cex=1.5)
lines(Dataset_b$Hsoil1_f, type="p", pch=16, col="black", cex=1.5)
lines(Dataset_b$Hsoil1_orig, type="p", pch=16, col="black", cex=1.5)
mtext(expression(theta ~ (cm^3 ~ cm^-3)), line=6, side=2, cex=cex_text_axe)
mtext("Year", line=3.5, side=1, cex=cex_text_axe)
axis(side=1, at=Fin_an, cex.axis=cex_axis, las=1, tck=1, lty=1, col="lightgray", labels=Fin_an_lab)
axis(side=2, at=seq(0,0.3,0.1), cex.axis=cex_axis, las=1, tck=0.05)
add_wet_shading()
add_dry_shading()
legend("topleft", bty="n", xpd=NA, cex=cex_leg2, 
       legend=c(expression(theta[1]), expression(theta[2])), 
       col=c("black","forestgreen"), pch=16, horiz=T)
text(x=3, y=0.2, "e)", cex=4)
abline(v=Fin_an, col="gray40", lty=2)
box()

# --- Panel 10: Average daily soil moisture ---
plot(X$Hsoil1, type="l", lwd=3, ylim=c(0.03,0.20), col='black', ylab="", xlab="", xaxt="n", yaxt="n")
lines(df_hsoil_mean_b$Hsoil2, lwd=3, col="forestgreen")
rect(Wet_index[1],-15000,Wet_index[2],15000,col=adjustcolor("blue", 0.1),border=adjustcolor("blue", 0.1))
rect(Dry_index[1],-15000,Dry_index[2],15000,col=adjustcolor("red", 0.1),border=adjustcolor("red", 0.1))
rect(Dry_index_2[1],-15000,Dry_index_2[2],15000,col=adjustcolor("red", 0.1),border=adjustcolor("red", 0.1))
mtext("DOY", line=3.5, side=1, cex=cex_text_axe)
axis(side=1, at=seq(0,366,90), cex.axis=cex_axis, las=1, tck=1, lty=1, col="lightgray", labels=as.character(seq(0,366,90)))
axis(side=2, at=seq(0.05,0.2,0.05), cex.axis=cex_axis, las=1, tck=0.05)
abline(v=c(90,180,270,360), col="gray40", lty=2)
box()

dev.off()