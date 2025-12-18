# ==============================================================================
# NEE Gap-Filling using MDS (Marginal Distribution Sampling)
# Author: Renaud KOUKOUI
# Description: Gap-fill NEE and meteorological data using REddyProc package
#              with Marginal Distribution Sampling method
# ==============================================================================

# Clear workspace
rm(list = ls(all = TRUE))

# Load required libraries
library(lubridate)
library(dplyr)
library(REddyProc)
library(openeddy)
# Load custom functions (if needed)
# source("G:/DOSSIERS RENAUD/anon-ms-example/Station_Base/day_and_nigth234.R")
# source("G:/DOSSIERS RENAUD/anon-ms-example/Station_Base/ustar_threshold_modified.R")

# ==============================================================================
# 1. SITE CONFIGURATION
# ==============================================================================

# Site definitions
Site <- c("Nalohou", "Bellefoungou")
ID <- c("Nal", "Bel")
Years <- as.character(2008:2017)

cat("1-Nalohou\n", "2-Bellefoungou\n")
n <- 1  # Select site (1 = Nalohou, 2 = Bellefoungou)

# Site location parameters
lat <- 9.74484   # Site latitude
long <- 1.60457  # Site longitude
tz <- 1          # Timezone (UTC+1)

# ==============================================================================
# 2. LOAD FILTERED DATA
# ==============================================================================

input_file <- paste0("./", Site[n], "/",
                     ID[n], "_Filtrage_vers_ _ustar_without_neg_night.csv")

Data_test_1 <- read.csv(input_file, sep = ";")

print(paste("Loaded data:", nrow(Data_test_1), "rows"))

# ==============================================================================
# 3. PREPARE DATA FOR MDS GAP-FILLING
# ==============================================================================

TOTO <- Data_test_1

# Parse dates and extract time components
TOTO$Date <- dmy_hm(TOTO$DateUTC)
TOTO$Year <- year(TOTO$Date)
TOTO$Month <- month(TOTO$Date)
TOTO$Day <- day(TOTO$Date)  # Day of year
TOTO$hour <- hour(TOTO$Date)
TOTO$minute <- minute(TOTO$Date)

# Convert VPD from kPa to hPa (required by REddyProc)
TOTO$VPD <- TOTO$VPD * 10

# ==============================================================================
# 4. SELECT AND RENAME VARIABLES FOR REDDYPROC FORMAT
# ==============================================================================

# Extract required variables
V_1 <- subset(TOTO, select = c("Year", "Day", "heure", "minute", "Month",
                                "NEE", "Swin", "Tair", "Tsol_1_10", "Tsol_1_20",
                                "RH", "VPD", "Hv1", "Hv2", "Rain", "Ustar_OK"))

# Rename columns to REddyProc standard names
colnames(V_1) <- c("Year", "Day", "Hour", "Minute", "Month",
                   "NEE", "Rg", "Tair", "Tsoil1", "Tsoil2",
                   "Rh", "VPD", "Hsoil1", "Hsoil2", "Rain", "Ustar")

# Remove first row (if needed)
V_1 <- V_1[-1, ]

# Add units row (required by REddyProc format)
units_row <- c("--", "--", "--", "--", "--",
               "umolm-2s-1", "Wm-2", "degC", "degC", "degC",
               "%", "hPa", "cm3cm-3", "cm3cm-3", "mm", "ms-1")
V_1 <- rbind(units_row, V_1)

# ==============================================================================
# 5. SAVE PREPARED DATA FOR MDS INPUT
# ==============================================================================

output_dir <- paste0("./Table/MDS/", Site[n])

# Create directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

output_file <- paste0(output_dir, "/", ID[n], "_MDS_Imput_new_vers_.csv")
write.table(V_1, file = output_file, row.names = FALSE, sep = ";")

print(paste("Prepared data saved to:", output_file))

# ==============================================================================
# 6. SET PATHS AND PARAMETERS FOR GAP-FILLING
# ==============================================================================

# Set working directory
setwd("G:/DOSSIERS RENAUD/anon-ms-example/Carbone/New/Article_MI_version_0.5/")

# Define paths
path_table <- paste0("./Table/MDS/", Site[n])
path_plots <- paste0("./Figure/MDS/", Site[n])

# Create plot directory if it doesn't exist
if (!dir.exists(path_plots)) {
  dir.create(path_plots, recursive = TRUE)
}

# Define variables to gap-fill and plot
meteo <- c('Rg', 'Tair', 'Tsoil1', 'Tsoil2', 'Hsoil1', 'Hsoil2', 
           'Rh', 'VPD', "Rain", "Ustar")
variables <- c("NEE", meteo)

# Plot settings
plot_to_console <- TRUE  # Show plots in console
plot_as <- "png"         # Save plots as PNG or PDF

# Site identifier for output
siteyear <- paste0(ID[n], "_filter_new_vers_")

print(paste("REddyProc package version:", packageVersion("REddyProc")))

# ==============================================================================
# 7. LOAD DATA INTO REDDYPROC
# ==============================================================================

# Load data with header and unit row
EddyData.F <- read_eddy(output_file, sep = ";")

# Convert time columns to POSIXct format
EddyDataWithPosix.F <- fConvertTimeToPosix(
  EddyData.F, 'YMDH', Year = 'Year', Day = 'Day', Hour = 'Hour', Month = 'Month')
# ==============================================================================
# 8. INITIALIZE REDDYPROC CLASS
# ==============================================================================

# Create sEddyProc object
EddyProc.C <- sEddyProc$new(siteyear, EddyDataWithPosix.F, variables)

# Set site location information
EddyProc.C$sSetLocationInfo(lat, long, tz)

# Display structure and preview
print("EddyProc object structure:")
str(EddyProc.C)
print("First 6 rows of data:")
EddyProc.C$sPrintFrames(NumRows.i = 6L)

# ==============================================================================
# 9. GENERATE DIAGNOSTIC PLOTS (BEFORE GAP-FILLING)
# ==============================================================================

if (plot_to_console) {
  print("Generating fingerprint plots...")
  
  # Create fingerprint plots for each variable (excluding Rain and Ustar)
  for (Var in variables[!variables %in% c("Rain", "Ustar")]) {
    EddyProc.C$sPlotFingerprint(Var, Dir = path_plots, Format = plot_as)
  }
  
  print("Generating half-hourly flux plots...")
  
  # Create half-hourly flux plots
  for (Var in variables[!variables %in% c("Rain", "Ustar")]) {
    EddyProc.C$sPlotHHFluxes(Var, Dir = path_plots, Format = plot_as)
  }
}

# ==============================================================================
# 10. PERFORM MDS GAP-FILLING
# ==============================================================================

print("Starting MDS gap-filling...")

# Gap-fill meteorological variables (excluding Rain and Ustar)
meteo_to_fill <- meteo[!meteo %in% c("Rain", "Ustar")]

for (met_var in meteo_to_fill) {
  print(paste("Gap-filling:", met_var))
  EddyProc.C$sMDSGapFill(met_var, FillAll = TRUE)
}

# Gap-fill NEE
print("Gap-filling: NEE")
EddyProc.C$sMDSGapFill('NEE', FillAll = TRUE)

print("Gap-filling completed successfully")

# ==============================================================================
# 11. GENERATE DIAGNOSTIC PLOTS (AFTER GAP-FILLING)
# ==============================================================================

print("Generating post-gap-filling fingerprint plots...")

# Create fingerprint plots for gap-filled variables
FP_vars <- c("NEE_f", paste0(meteo_to_fill, "_f"))

for (Var in FP_vars) {
  EddyProc.C$sPlotFingerprint(Var, Dir = path_plots, Format = plot_as)
}

# ==============================================================================
# 12. EXPORT RESULTS
# ==============================================================================

print("Exporting results...")

# Export gap-filled data
Export_Brut <- EddyProc.C$sExportResults()

# Add original timestamp, season, and day/night information
Date <- EddyDataWithPosix.F$DateTime
Saison <- Data_test_1$Saison[-1]  # Remove first row to match
Day_or_nigth_1.2 <- Data_test_1$Day_or_nigth_1.2[-1]

# Combine all data
Export_Brut <- cbind(Date, Saison, Day_or_nigth_1.2, Export_Brut)

# Save final output
output_file_final <- paste0("./Table/MDS/", Site[n], "/",
                            ID[n], "_FLX_MET_MDS_output_new_vers_.csv")

write.table(Export_Brut, 
            file = output_file_final, 
            row.names = FALSE, 
            sep = ";")

print(paste("Results saved to:", output_file_final))
print(paste("Total rows exported:", nrow(Export_Brut)))

# ==============================================================================
# 13. SUMMARY STATISTICS
# ==============================================================================

print("=== Gap-Filling Summary ===")
print(paste("Site:", Site[n]))
print(paste("Original data points:", nrow(Data_test_1) - 1))
print(paste("Gap-filled data points:", nrow(Export_Brut)))

# Calculate gap-filling statistics for NEE
original_na <- sum(is.na(EddyDataWithPosix.F$NEE))
filled_na <- sum(is.na(Export_Brut$NEE_f))
filled_points <- original_na - filled_na

print(paste("NEE - Original missing:", original_na, 
            sprintf("(%.1f%%)", 100 * original_na / nrow(EddyDataWithPosix.F))))
print(paste("NEE - Still missing after gap-filling:", filled_na,
            sprintf("(%.1f%%)", 100 * filled_na / nrow(Export_Brut))))
print(paste("NEE - Points filled:", filled_points,
            sprintf("(%.1f%%)", 100 * filled_points / original_na)))

print("Processing complete!")