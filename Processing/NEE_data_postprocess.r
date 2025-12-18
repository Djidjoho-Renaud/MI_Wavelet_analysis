# ==============================================================================
# NEE (Net Ecosystem Exchange) Data Processing and Quality Control
# Author: Renaud KOUKOUI
# Description: Process meteorological and flux data, calculate NEE with storage,
#              apply quality filters (Ustar threshold, physical bounds)
# ==============================================================================

# Clear workspace
rm(list = ls(all = TRUE))

# Load required libraries
library(lubridate)
library(dplyr)
library(zoo)

# Load custom functions
source("./day_and_nigth234.R")
source("./ustar_threshold_modified.R")

# ==============================================================================
# 1. SITE AND PATH CONFIGURATION
# ==============================================================================

# Site definitions
Site <- c("Nalohou", "Bellefoungou")
ID <- c("Nal", "Bel")
Years <- as.character(2007:2017)

cat("1-Nalohou\n", "2-Bellefoungou\n")
n <- 1  # Select site (1 = Nalohou, 2 = Bellefoungou)

# ==============================================================================
# 2. LOAD METEOROLOGICAL DATA
# ==============================================================================

Imput <- paste0("./New/Imput/MET/", Site[n])
setwd(Imput)
fichiers <- list.files(Imput)

# Initialize data storage
Big_Data <- NULL

# Season boundaries for 2008-2017 (from David GNONLONFOU, 2020)
# SS = Dry season, ST = Transition, SH = Wet season
SS_d1 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
SS_f1 <- c(1440, 2592, 1248, 1728, 960, 864, 2016, 960, 1440, 2160, 2544)
ST_d1 <- c(1441, 2593, 1249, 1729, 961, 865, 2017, 961, 1441, 2161, 2545)
ST_f1 <- c(4848, 5808, 4992, 5232, 5426, 4656, 3072, 4752, 5712, 3360, 4368)
SH_d <- c(4849, 5809, 4993, 5233, 5425, 4657, 3073, 4753, 5713, 3361, 4369)
SH_f <- c(14352, 14064, 14592, 14640, 14448, 15072, 14304, 14016, 14592, 14256, 14592)
ST_d2 <- c(14353, 14065, 14593, 14641, 14449, 15073, 14305, 14017, 14593, 14257, 14593)
ST_f2 <- c(16944, 17328, 17280, 17376, 17136, 17088, 16224, 16896, 16416, 16992, 17424)
SS_d2 <- c(16945, 17329, 17281, 17377, 17137, 17089, 16225, 16897, 16417, 16993, 17425)
SS_f2 <- c(17520, 17568, 17520, 17520, 17520, 17568, 17520, 17520, 17520, 17568, 17520)

# Required meteorological columns
colonnes_requises <- c("heure", "DateUTC", "Swin", "Tair", "RH", 
                       "Tsoil1", "Tsoil2", "Hsoil1", "Hsoil2", "P", "Rnet")

# Process each meteorological file
for (file_idx in seq_along(fichiers)) {
  file <- fichiers[file_idx]
  print(paste("Processing file:", file))
  
  # Read first lines to detect separator
  tmp <- readLines(file, n = 5)
  tmpFile <- tempfile()
  writeLines(tmp, tmpFile)
  on.exit(unlink(tmpFile), add = TRUE)
  
  # Detect separator (comma or semicolon)
  premiere_ligne <- tmp[1]
  nb_virgules <- sum(gregexpr(",", premiere_ligne)[[1]] > 0)
  nb_pointvirgules <- sum(gregexpr(";", premiere_ligne)[[1]] > 0)
  sep_char <- if(nb_virgules > nb_pointvirgules) "," else ";"
  
  # Read complete file
  tmp <- readLines(file)
  writeLines(tmp, tmpFile)
  
  tryCatch({
    Data <- read.csv(tmpFile, 
                     sep = sep_char,
                     header = TRUE, 
                     na.strings = c("NA", "-999"), 
                     dec = ".",
                     strip.white = TRUE)
    
    # Check for missing columns
    colonnes_manquantes <- colonnes_requises[!colonnes_requises %in% names(Data)]
    
    # Try alternate separator if columns are missing
    if(length(colonnes_manquantes) > 0) {
      autre_sep <- if(sep_char == ",") ";" else ","
      Data_autre <- tryCatch({
        read.csv(tmpFile, sep = autre_sep, header = TRUE, 
                 na.strings = c("NA", "-999"), dec = ".", strip.white = TRUE)
      }, error = function(e) NULL)
      
      if(!is.null(Data_autre) && all(colonnes_requises %in% names(Data_autre))) {
        Data <- Data_autre
        sep_char <- autre_sep
        colonnes_manquantes <- c()
      }
    }
    
    # Add season labels
    Saison <- rep("SS", times = SS_f1[file_idx])
    Saison[ST_d1[file_idx]:ST_f1[file_idx]] <- "ST1"
    Saison[SH_d[file_idx]:SH_f[file_idx]] <- "SH"
    Saison[ST_d2[file_idx]:ST_f2[file_idx]] <- "ST2"
    Saison[SS_d2[file_idx]:SS_f2[file_idx]] <- "SS"
    
    # Process only if all required columns are present
    if(length(colonnes_manquantes) == 0) {
      Data <- Data[, colonnes_requises]
      Data$Saison <- Saison
      
      # Append to main dataset
      Big_Data <- if(is.null(Big_Data)) Data else rbind(Big_Data, Data)
      print(paste("File imported successfully. Separator:", sep_char))
    } else {
      warning(paste("File ignored due to missing columns:", 
                    paste(colonnes_manquantes, collapse = ", ")))
    }
    
  }, error = function(e) {
    warning(paste("Error processing file:", e$message))
  })
}

# Verify data was imported
if(is.null(Big_Data)) {
  stop("No data was imported successfully!")
} else {
  print(paste("Import complete:", nrow(Big_Data), "rows total"))
}

# ==============================================================================
# 3. CALCULATE VPD (Vapor Pressure Deficit)
# ==============================================================================

esat <- 0.6108 * exp((17.27 * Big_Data$Tair) / (Big_Data$Tair + 237.3))
ea <- (Big_Data$RH * esat) / 100
VPD <- esat - ea
Big_Data$VPD <- VPD

# ==============================================================================
# 4. APPLY PHYSICAL BOUNDS TO METEOROLOGICAL DATA
# ==============================================================================

# Filter unrealistic soil temperature values
Big_Data$Tsoil1[!is.na(Big_Data$Tsoil1) & Big_Data$Tsoil1 < 20.5] <- NA
Big_Data$Tsoil2[!is.na(Big_Data$Tsoil2) & Big_Data$Tsoil2 < 22.5] <- NA

# Filter unrealistic precipitation values
Big_Data$P[!is.na(Big_Data$P) & Big_Data$P > 80] <- NA

# ==============================================================================
# 5. LOAD FLUX DATA (NEE)
# ==============================================================================

Imput <- paste0("./New/Imput/FLX/", Site[n])

if (!dir.exists(Imput)) {
  stop(paste("Directory does not exist:", Imput))
}

setwd(Imput)
fichiers <- list.files(Imput, full.names = TRUE)

if (length(fichiers) == 0) {
  stop(paste("No files found in", Imput))
}

# Required flux columns
colonnes_requises <- c("DateUTC", "Ustar_OK", "co2_licor_mean", 
                       "FCO2_OK", "q_meteo", "wdir_meteo")
Big_Data_NEE <- NULL

# Process each flux file
for (file in fichiers) {
  tryCatch({
    print(paste("Processing file:", basename(file)))
    
    # Detect separator
    tmp <- readLines(file, n = 5)
    premiere_ligne <- tmp[1]
    separateurs <- c(",", ";", "\t")
    comptes <- sapply(separateurs, function(sep) sum(gregexpr(sep, premiere_ligne)[[1]] > 0))
    sep_char <- separateurs[which.max(comptes)]
    
    tmpFile <- tempfile()
    tmp <- readLines(file)
    writeLines(tmp, tmpFile)
    on.exit(unlink(tmpFile), add = TRUE)
    
    Data_NEE <- read.csv(tmpFile, 
                         sep = sep_char, 
                         header = TRUE, 
                         na.strings = c("NA", "-999", "-6999"), 
                         dec = ".", 
                         strip.white = TRUE)
    
    # Check for missing columns and try alternate separators
    colonnes_manquantes <- colonnes_requises[!colonnes_requises %in% names(Data_NEE)]
    
    if (length(colonnes_manquantes) > 0) {
      for (autre_sep in separateurs[separateurs != sep_char]) {
        Data_autre <- tryCatch({
          read.csv(tmpFile, sep = autre_sep, header = TRUE, 
                   na.strings = c("NA", "-999", "-6999"), 
                   dec = ".", strip.white = TRUE)
        }, error = function(e) NULL)
        
        if (!is.null(Data_autre) && all(colonnes_requises %in% names(Data_autre))) {
          Data_NEE <- Data_autre
          colonnes_manquantes <- c()
          break
        }
      }
    }
    
    # Process only if all required columns are present
    if (length(colonnes_manquantes) == 0) {
      Data_NEE <- Data_NEE[, colonnes_requises]
      Big_Data_NEE <- if (is.null(Big_Data_NEE)) Data_NEE else rbind(Big_Data_NEE, Data_NEE)
      print(paste("File imported successfully"))
    }
  }, error = function(e) {
    warning(paste("Error processing file:", e$message))
  })
}

if (is.null(Big_Data_NEE)) {
  stop("No flux data was imported successfully!")
} else {
  print(paste("Import complete:", nrow(Big_Data_NEE), "rows total"))
}

# Filter zero flux values
Big_Data_NEE$FCO2_OK[Big_Data_NEE$FCO2_OK == 0 & !is.na(Big_Data_NEE$FCO2_OK)] <- NA

# ==============================================================================
# 6. CALCULATE CO2 STORAGE FLUX
# ==============================================================================

# Constants
R <- 8.314         # Gas constant [J/mol/K]
MMa <- 0.02896     # Molar mass of air [kg/mol]
time_step <- 1800  # Time step [s]
z_meas <- 4.95     # Measurement height at Nalohou [m]
PaStandard <- 1000 # Standard pressure [hPa]

# Function to calculate CO2 storage flux with height adjustment
F_CO2_stor_v2 <- function(co2_licor_mean, DateUTC) {
  z_meas_avant <- 4.95  # Height before change
  z_meas_apres <- 5.4   # Height after change
  
  # Convert dates
  dates <- dmy_hm(DateUTC)
  date_changement <- dmy_hm("07/11/2015 00:00")
  
  # Initialize storage flux
  F_s <- rep(0, length(co2_licor_mean))
  z <- length(co2_licor_mean) - 1
  
  # Calculate storage flux for each time step
  for (i in 1:z) {
    # Determine current measurement height
    z_meas_current <- if (dates[i + 1] < date_changement) z_meas_avant else z_meas_apres
    
    # Calculate storage flux [µmol/m²/s]
    F_s[i + 1] <- 1000 * z_meas_current * 
                  (co2_licor_mean[i + 1] - co2_licor_mean[i]) / time_step
  }
  
  F_s[1] <- NA
  return(F_s)
}

# Calculate storage flux
F_sCo2 <- F_CO2_stor_v2(Big_Data_NEE$co2_licor_mean, Big_Data_NEE$DateUTC)

# Calculate NEE = Flux + Storage
NEE <- ifelse(is.na(F_sCo2), 
              Big_Data_NEE$FCO2_OK, 
              Big_Data_NEE$FCO2_OK + F_sCo2)

# ==============================================================================
# 7. COMBINE METEOROLOGICAL AND FLUX DATA
# ==============================================================================

# Add year information
Big_Data_NEE$DateUTC <- dmy_hm(Big_Data_NEE$DateUTC)
Big_Data_NEE$year <- year(Big_Data_NEE$DateUTC)
Big_Data$DateUTC <- dmy_hm(Big_Data$DateUTC)

# Combine datasets
Data_test_NEE <- data.frame(NEE = NEE,
                            Ustar_OK = Big_Data_NEE$Ustar_OK,
                            q_meteo = Big_Data_NEE$q_meteo,
                            wdir_meteo = Big_Data_NEE$wdir_meteo)

Data_test <- cbind(Big_Data[, c("heure", "DateUTC", "Saison", "Swin", "Tair", "RH",
                                 "Tsoil1", "Tsoil2", "Hsoil1", "Hsoil2", "VPD", "P", "Rnet")],
                   Data_test_NEE)

# Rename columns for clarity
colnames(Data_test) <- c("heure", "DateUTC", "Saison", "Swin", "Tair", "RH",
                         "Tsol_1_10", "Tsol_1_20", "Hv1", "Hv2", "VPD", "Rain", "Rnet",
                         "NEE", "Ustar_OK", "q_a", "wdir")

# Add Year and Day of Year
Year <- year(Data_test$DateUTC)
Day <- yday(Data_test$DateUTC)
Data_test <- cbind(Year, Day, Data_test)

# ==============================================================================
# 8. ADD DAY/NIGHT FLAG
# ==============================================================================

Day_or_nigth_1.2 <- day_and_nigth(1.60457, 9.74484, Data_test)
Data_test <- cbind(Day_or_nigth_1.2, Data_test)
colnames(Data_test)[1] <- "Day_or_nigth_1.2"

# Update column names
colnames(Data_test) <- c("Day_or_nigth_1.2", "Year", "Day", "heure", "DateUTC", "Saison",
                         "Swin", "T_meteo", "RH", "Tsol_1_10", "Tsol_1_20", "Hv1", "Hv2",
                         "VPD", "Rain", "Rnet", "NEE", "Ustar_OK", "q_a", "wdir")

# ==============================================================================
# 9. APPLY PHYSICAL BOUNDS TO NEE
# ==============================================================================

# Create copy for filtered data
Data_test_1 <- Data_test

# Remove physically unrealistic NEE values
Data_test_1$NEE[!is.na(Data_test_1$NEE) & Data_test_1$NEE > 20] <- NA
Data_test_1$NEE[!is.na(Data_test_1$NEE) & Data_test_1$NEE < -40] <- NA
Data_test$NEE[!is.na(Data_test$NEE) & Data_test$NEE > 20] <- NA
Data_test$NEE[!is.na(Data_test$NEE) & Data_test$NEE < -40] <- NA

# ==============================================================================
# 10. CALCULATE PERCENTAGE OF MISSING DATA BY YEAR
# ==============================================================================

pct_by_year <- tapply(Data_test_1$NEE, Data_test_1$Year, 
                      function(x) round(sum(is.na(x)) / length(x) * 100, 2))

pourcentage_NA <- data.frame(
  Year = as.numeric(names(pct_by_year)),
  Pourcentage_NA = as.numeric(pct_by_year)
)

# ==============================================================================
# 11. CALCULATE USTAR THRESHOLD
# ==============================================================================

Year_onlys <- seq(min(Data_test$Year), max(Data_test$Year), 1)
Ustart_Th <- numeric(length(Year_onlys))

# Calculate threshold for each year
for (l in seq_along(Year_onlys)) {
  data <- filter(Data_test, Year == Year_onlys[l])
  Ustart_Th[l] <- ustarThreshold(data)
}

# Use median threshold across all years
Ustat_Th_ok <- median(Ustart_Th)
print(paste("Median Ustar threshold:", Ustat_Th_ok))

# ==============================================================================
# 12. APPLY USTAR FILTER TO NIGHTTIME NEE
# ==============================================================================

# Filter nighttime NEE below Ustar threshold
for (k in 1:nrow(Data_test_1)) {
  if(!is.na(Data_test_1$NEE[k]) && Data_test_1$Day_or_nigth_1.2[k] == 0) {
    if(!is.na(Data_test_1$Ustar_OK[k]) && Data_test_1$Ustar_OK[k] < Ustat_Th_ok) {
      Data_test_1$NEE[k] <- NA
    }
  }
}

# ==============================================================================
# 13. CREATE VERSION WITHOUT NEGATIVE NIGHTTIME NEE
# ==============================================================================

Data_test_1x <- Data_test_1

# Remove negative NEE during nighttime
for (k in 1:nrow(Data_test_1x)) {
  if(!is.na(Data_test_1x$NEE[k]) && Data_test_1x$Day_or_nigth_1.2[k] == 0) {
    if(Data_test_1x$NEE[k] < 0) {
      Data_test_1x$NEE[k] <- NA
    }
  }
}

# ==============================================================================
# 14. SAVE OUTPUT FILES
# ==============================================================================

output_dir <- paste0("./Table/Filtrage/", Site[n], "/")

# Save filtered data with Ustar threshold
write.table(Data_test_1,
            file = paste0(output_dir, ID[n], "_Filtrage_vers_ _ustar.csv"),
            row.names = FALSE, sep = ";")

# Save filtered data without negative nighttime NEE
write.table(Data_test_1x,
            file = paste0(output_dir, ID[n], 
                         "_Filtrage_vers_ _ustar_without_neg_night.csv"),
            row.names = FALSE, sep = ";")

# Save raw combined data
Data_brut <- cbind(Big_Data, Data_test_NEE)
write.table(Data_brut,
            file = paste0(output_dir, ID[n], "_Brut_vers_.csv"),
            row.names = FALSE, sep = ";")

# Save percentage of missing data by year
write.table(pourcentage_NA,
            file = paste0(output_dir, ID[n], "_pourcentage_NA_vers_.csv"),
            row.names = FALSE, sep = ";")

print("Processing complete. All files saved successfully.")