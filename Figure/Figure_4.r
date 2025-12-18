# ============================================================================
# NOCTURNAL NEE DATA VISUALIZATION SCRIPT
# Sites: Nalohou & Bellefoungou
#Author: Renaud KOUKOUI
#email: renaud.koukoui@gmail.com
# Figure_4
# ============================================================================

# Clear workspace and load libraries
rm(list = ls(all=TRUE))

# Load required libraries
library(lubridate)
library(dplyr)
library(ggplot2)

# ============================================================================
# 1. CONFIGURATION
# ============================================================================

# Site information
Site <- c("Nalohou", "Bellefoungou")
ID <- c("Nal", "Bel")

# Define paths
base_path <- "./Table/RF/"
output_path <- "./Figure/Dynamique meteo/"

# Define seasonal transition dates
dates_ss_1 <- c("2007-01-01","2007-12-20","2008-12-27","2009-12-27","2010-12-29",
                "2011-12-24","2012-12-22","2013-12-05","2014-12-19","2015-12-09","2016-12-20")
dates_tr_1 <- c("2007-01-31","2008-02-24","2009-01-27","2010-02-06","2011-01-21",
                "2012-01-19","2013-02-12","2014-01-21","2015-01-31","2016-02-15","2017-02-23")
dates_sh <- c("2007-04-12","2008-05-01","2009-04-15","2010-04-20","2011-04-24",
              "2012-04-07","2013-03-06","2014-04-10","2015-04-30","2016-03-11","2017-04-02")
dates_tr_2 <- c("2007-10-27","2008-10-20","2009-11-01","2010-11-02","2011-10-29",
                "2012-11-10","2013-10-26","2014-10-20","2015-11-01","2016-10-24","2017-11-01")

# Plotting parameters
cex_pts <- 2
cex_axis <- 2.5
cex_text_axe <- 1.8
cex_leg <- 2.25
cex_leg2 <- 3

# ============================================================================
# 2. DATA LOADING AND PREPROCESSING
# ============================================================================

# Function to load and process site data
process_site_data <- function(site_name, site_id, start_id = 1) {
  
  # Load data
  file_suffix <- ifelse(site_name == "Nalohou", "_new_2007.csv", "_new.csv")
  filepath <- paste0(base_path, site_name, "/", site_id, "_FLX_MET_MDS_RF_output_qc_1", file_suffix)
  df <- read.csv(filepath)
  
  # Filter nighttime data only
  df_night <- filter(df, Day_or_nigth_1.2 == 0)
  
  # Add temporal variables
  df_night$Day <- yday(ymd_hms(df_night$Date))
  df_night$Year <- year(df_night$Date)
  
  # Remove negative NEE values (keeping only respiration)
  df_night$NEE_RF_filled[df_night$NEE_RF_filled < 0] <- NA
  
  # Set factor levels for seasons
  df_night$Saison <- factor(df_night$Saison, levels = c("SS", "ST1", "SH", "ST2"))
  
  # Calculate daily statistics
  df_nee_mean <- df_night %>%
    group_by(Year, Day) %>%
    dplyr::summarise(
      n = n(),
      DateUTC = unique(lubridate::date(Date)),
      min = ifelse(all(is.na(NEE_RF_filled)), NA, min(NEE_RF_filled, na.rm = TRUE)),
      max = ifelse(all(is.na(NEE_RF_filled)), NA, max(NEE_RF_filled, na.rm = TRUE)),
      mean = mean(NEE_RF_filled, na.rm = TRUE),
      mean_orig = mean(NEE_orig, na.rm = TRUE),
      median = median(NEE_RF_filled, na.rm = TRUE),
      std = sd(NEE_RF_filled, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Add ID column
  end_id <- start_id + nrow(df_nee_mean) - 1
  df_nee_mean$ID <- start_id:end_id
  
  return(list(
    night = df_night,
    daily = df_nee_mean
  ))
}

# Process both sites
cat("Loading Nalohou data...\n")
data_n <- process_site_data(Site[1], ID[1], start_id = 1)

cat("Loading Bellefoungou data...\n")
data_b <- process_site_data(Site[2], ID[2], start_id = 366)

# ============================================================================
# 3. CALCULATE Y-AXIS LIMITS (WITHOUT OUTLIERS)
# ============================================================================

calc_limits_no_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower_fence <- q1 - 1.5 * iqr
  upper_fence <- q3 + 1.5 * iqr
  c(lower_fence, upper_fence)
}

limits_n <- calc_limits_no_outliers(data_n$night$NEE_RF_filled)
limits_b <- calc_limits_no_outliers(data_b$night$NEE_RF_filled)
y_min <- min(limits_n[1], limits_b[1], na.rm = TRUE)
y_max <- max(limits_n[2], limits_b[2], na.rm = TRUE)

# ============================================================================
# 4. GET INDICES FOR SEASONAL SHADING
# ============================================================================

indices_ss_1 <- which(as.character(data_n$daily$DateUTC) %in% dates_ss_1)
indices_tr_1 <- which(as.character(data_n$daily$DateUTC) %in% dates_tr_1)
indices_sh <- which(as.character(data_n$daily$DateUTC) %in% dates_sh)
indices_tr_2 <- which(as.character(data_n$daily$DateUTC) %in% dates_tr_2)

# Get year indices for x-axis
indices_premiere_occurrence_n <- which(
  ave(seq_along(data_n$daily$Year), data_n$daily$Year, 
      FUN = function(x) x == min(x)) == 1
)

# ============================================================================
# 5. CREATE FIGURE
# ============================================================================

cat("Creating figure...\n")

# Open PNG device
png(paste0(output_path, "Figure_4.png"), 
    width = 1600, height = 1000)

par(mar = c(4, 4, 4, 5), oma = c(4, 6, 3, 0), bg = "white")
par(mfrow = c(2, 1))

# --- Panel A: Time series ---

# Define colors (red for gap-filled, black/gray for original)
col_n <- ifelse(is.na(data_n$daily$mean_orig), "#FF6B6B", "gray")
col_b <- ifelse(is.na(data_b$daily$mean_orig), "#8B0000", "black")

# Create base plot
plot(data_n$daily$ID, data_n$daily$mean,
     type = "p", col = col_n, pch = 16, cex = 1.5,
     xlab = "", ylab = "", xaxt = "n", yaxt = "n",
     ylim = c(0, 13))

# Add Bellefoungou data
points(data_b$daily$ID, data_b$daily$mean,
       col = col_b, pch = 16, cex = 1.5)

# Add seasonal shading - Wet season (blue)
for(i in 1:length(indices_sh)) {
  rect(indices_sh[i], -15000, indices_tr_2[i], 15000,
       col = adjustcolor("blue", 0.1), border = adjustcolor("blue", 0.1))
}

# Add seasonal shading - Dry season (red)
for(i in 1:length(indices_ss_1)) {
  rect(indices_ss_1[i], -15000, indices_tr_1[i], 15000,
       col = adjustcolor("red", 0.1), border = adjustcolor("red", 0.1))
}

# Add axes and labels
text(x = 100, y = 9, "a)", cex = 3)
axis(side = 1, at = indices_premiere_occurrence_n, 
     labels = as.character(2007:2017), cex.axis = cex_axis, padj = 1, tck = -0.05)
axis(side = 2, at = seq(0, 12, 3), cex.axis = cex_axis, las = 1, tck = 0.05)
mtext(expression("NEE"[night]~"["*mu*mol~m^-2~s^-1*"]"), 
      line = 4, side = 2, cex = 2.5)
mtext("Year", line = 5.5, side = 1, cex = 2.5)

# Add legend
legend("top",
       legend = c("Cultivated Savannah", "Clear forest", 
                  "Cultivated Savannah (NA)", "Clear forest (NA)"),
       col = c("gray", "black", "#FF6B6B", "#8B0000"),
       pch = 16, pt.cex = 2,
       horiz = TRUE, bg = NA, box.lty = 0, cex = 1.8)

box()

# --- Panel B: Seasonal boxplots ---

# Create boxplots for Nalohou
boxplot(NEE_RF_filled ~ Saison,
        data = data_n$night,
        outline = FALSE,
        col = c("red", "lightblue", "blue", "orange"),
        border = "black",
        ylim = c(y_min, 10),
        at = c(0.90, 1.90, 2.90, 3.90),
        boxwex = 0.15,
        xaxt = "n", yaxt = "n",
        xlab = "", ylab = "")

# Add boxplots for Bellefoungou
boxplot(NEE_RF_filled ~ Saison,
        data = data_b$night,
        outline = FALSE,
        col = c("red", "lightblue", "blue", "orange"),
        border = "black",
        lty = "longdash",
        at = c(1.10, 2.10, 3.10, 4.10),
        boxwex = 0.15,
        add = TRUE,
        xaxt = "n", yaxt = "n")

# Add means for Nalohou
moyennes_n <- aggregate(NEE_RF_filled ~ Saison, data = data_n$night, 
                        FUN = mean, na.rm = TRUE)
points(x = c(0.90, 1.90, 2.90, 3.90),
       y = moyennes_n$NEE_RF_filled, 
       col = "black", pch = 16, cex = 2)

# Add means for Bellefoungou
moyennes_b <- aggregate(NEE_RF_filled ~ Saison, data = data_b$night, 
                        FUN = mean, na.rm = TRUE)
points(x = c(1.10, 2.10, 3.10, 4.10),
       y = moyennes_b$NEE_RF_filled, 
       col = "black", pch = 16, cex = 2)

# Add axes and labels
text(x = 0.5, y = 9, "b)", cex = 3)
axis(1, at = c(1, 2, 3, 4), 
     labels = c("dry", "moistening", "wet", "drying"),
     cex.axis = 3, padj = 1, tck = -0.05)
axis(side = 2, at = seq(0, 12, 3), cex.axis = cex_axis, las = 1, tck = 0.05)
mtext(expression("NEE"[night]~"["*mu*mol~m^-2~s^-1*"]"), 
      line = 4, side = 2, cex = 2.5)
mtext("Season", line = 5.5, side = 1, cex = 2.5)

# Add legend
legend("top", 
       legend = c("Cultivated savannah", "Clear forest"),
       lty = c("solid", "longdash"),
       col = "black",
       bty = "n", horiz = TRUE, cex = 2)

dev.off()

cat("Figure saved successfully!\n")