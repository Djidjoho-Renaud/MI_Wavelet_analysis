# ==============================================================================
# NEE Gap-Filling using Random Forest (RF)
# Author: Renaud KOUKOUI
# Description: Apply Random Forest machine learning to gap-fill NEE data
#              using MDS output as input, validate model, and generate reports
# ==============================================================================

# Clear workspace
rm(list = ls(all = TRUE))

# Load required libraries
library(lubridate)
library(dplyr)
library(ggplot2)
library(caret)
library(doParallel)

# ==============================================================================
# 1. SITE CONFIGURATION
# ==============================================================================

# Site definitions
Site <- c("Nalohou", "Bellefoungou")
ID <- c("Nal", "Bel")
Years <- as.character(2008:2017)

cat("1-Nalohou\n", "2-Bellefoungou\n")
n <- 1  # Select site (1 = Nalohou, 2 = Bellefoungou)

# ==============================================================================
# 2. LOAD DATA
# ==============================================================================

# Load MDS gap-filled data
mds_file <- paste0("./Table/MDS/", Site[n], "/",
                   ID[n], "_FLX_MET_MDS_output_new_vers_.csv")

Data_test_eddyproc <- read.csv(mds_file, sep = ";")

# Load original filtered data for day/night information
filter_file <- paste0("./Table/Filtrage/", Site[n], "/",
                      ID[n], "_Filtrage_vers_ _ustar_without_neg_night.csv")

TOTO <- read.csv(filter_file, sep = ";")
TOTO <- TOTO[-1, ]  # Remove first row
Day_or_nigth_1.2 <- TOTO$Day_or_nigth_1.2

print(paste("Loaded data:", nrow(Data_test_eddyproc), "rows"))

# ==============================================================================
# 3. PREPARE NEE DATA WITH QUALITY FILTERS
# ==============================================================================

# Create filtered NEE variable based on quality flags
NEE_filter <- Data_test_eddyproc$NEE_f

# Remove low quality data (quality flag > 1)
NEE_filter[which(Data_test_eddyproc$NEE_fqc > 1)] <- NA

# Remove negative nighttime NEE values
NEE_filter[which(Data_test_eddyproc$Day_or_nigth_1.2 == 0 & 
                 Data_test_eddyproc$NEE_f < 0)] <- NA

Data_test_eddyproc$NEE_filter <- NEE_filter

print(paste("NEE filtering complete. Missing values:", 
            sum(is.na(NEE_filter)), 
            sprintf("(%.1f%%)", 100 * sum(is.na(NEE_filter)) / length(NEE_filter))))

# ==============================================================================
# 4. PREPARE PREDICTOR VARIABLES
# ==============================================================================

# Select predictor variables for RF model
predictors <- c("NEE_filter", "Rg_f", "VPD_f", "Tair_f", 
                "Tsoil1_f", "Tsoil2_f", "Hsoil1_f", "Hsoil2_f")

ML.df <- subset(Data_test_eddyproc, select = predictors)

# Keep only complete cases (rows without NA in NEE_filter)
wm_only <- ML.df[!is.na(ML.df$NEE_filter), ]

print(paste("Complete cases for training:", nrow(wm_only)))

# ==============================================================================
# 5. SPLIT DATA INTO TRAINING AND TEST SETS
# ==============================================================================

# 75% of data for training, 25% for testing
set.seed(123)
index <- createDataPartition(wm_only$NEE_filter, p = 0.75, list = FALSE)
train_set <- wm_only[index, ]
test_set <- wm_only[-index, ]

print(paste("Training set:", nrow(train_set), "rows"))
print(paste("Test set:", nrow(test_set), "rows"))

# ==============================================================================
# 6. TRAIN RANDOM FOREST MODEL
# ==============================================================================

train_RF_NEE_optimized <- function(train_set, predictors, seed = 123) {
  # Start timer
  start_time <- Sys.time()
  
  # Check number of available variables
  n_vars <- length(predictors)
  mtry_value <- min(9, n_vars)  # Adjust mtry to not exceed number of variables
  
  cat("\n=== Random Forest Training Configuration ===\n")
  cat("Number of available variables:", n_vars, "\n")
  cat("mtry value used:", mtry_value, "\n")
  
  # Configure parallel processing (use 80% of available cores)
  n_cores <- max(1, floor(parallel::detectCores() * 0.8))
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  cat("Number of cores used:", n_cores, "\n")
  
  # Define optimized training controls
  ctrl <- trainControl(
    method = "none",           # No resampling for speed
    allowParallel = TRUE,
    verboseIter = TRUE
  )
  
  # Configure model with ranger (faster than randomForest)
  rf_grid <- expand.grid(
    mtry = mtry_value,
    splitrule = "variance",
    min.node.size = 5
  )
  
  # Prepare training data
  train_data <- train_set[, c(predictors, "NEE_filter")]
  
  cat("Training set dimensions:", dim(train_data), "\n\n")
  
  # Train model with error handling
  tryCatch({
    set.seed(seed)
    RF_NEE <- train(
      NEE_filter ~ .,
      data = train_data,
      method = "ranger",
      trControl = ctrl,
      tuneGrid = rf_grid,
      num.trees = 1000,
      importance = "permutation",
      preProcess = c("medianImpute"),
      num.threads = n_cores,
      verbose = TRUE
    )
    
    # Calculate execution time
    end_time <- Sys.time()
    execution_time <- difftime(end_time, start_time, units = "mins")
    
    # Display training statistics
    cat("\n=== Random Forest Training Report ===\n")
    cat("Total execution time:", round(as.numeric(execution_time), 2), "minutes\n")
    cat("Number of trees:", 1000, "\n")
    cat("Predictors per split (mtry):", mtry_value, "\n")
    
    return(RF_NEE)
    
  }, error = function(e) {
    cat("Training error:", e$message, "\n")
    stopCluster(cl)
    stop(e)
  }, finally = {
    # Ensure cluster is stopped
    stopCluster(cl)
  })
}

# Train the model
predictors_names <- names(train_set)[names(train_set) != "NEE_filter"]
RF_NEE <- train_RF_NEE_optimized(train_set, predictors_names)

# ==============================================================================
# 7. EXTRACT AND VISUALIZE VARIABLE IMPORTANCE
# ==============================================================================

get_variable_importance <- function(RF_model, scale = FALSE) {
  # Extract variable importance
  imp <- varImp(RF_model, scale = scale)
  
  # Convert to dataframe
  imp_df <- as.data.frame(imp$importance)
  
  # Create named vector with importance values
  importance_values <- imp_df$Overall
  names(importance_values) <- rownames(imp_df)
  
  # Sort values
  importance_values <- sort(importance_values, decreasing = TRUE)
  
  return(importance_values)
}

# Get variable importance
importance_results <- get_variable_importance(RF_NEE, scale = FALSE)

print("Variable Importance:")
print(importance_results)

# Create importance dataframe
importance_df <- data.frame(
  Variable = names(importance_results),
  Importance = importance_results,
  row.names = NULL
)

# Create and save importance plot
output_dir_fig <- paste0("./Figure/RF/", Site[n])
if (!dir.exists(output_dir_fig)) {
  dir.create(output_dir_fig, recursive = TRUE)
}

importance_plot_file <- paste0(output_dir_fig, "/importance_variables_", 
                               Site[n], "_new_vers_.png")

png(importance_plot_file, width = 800, height = 600)
par(mar = c(5, 10, 4, 2))
barplot(importance_results, 
        horiz = TRUE, 
        las = 1, 
        main = "Variable Importance",
        xlab = "Importance")
dev.off()

print(paste("Variable importance plot saved to:", importance_plot_file))

# ==============================================================================
# 8. VALIDATE MODEL ON TEST SET
# ==============================================================================

# Predict on test set
test_set$NEE_rf <- predict(RF_NEE, test_set, na.action = na.pass)

# Calculate regression statistics
regrRF <- lm(NEE_rf ~ NEE_filter, data = test_set)
print("Test Set Validation:")
print(summary(regrRF))

# Extract coefficients and R²
coef <- coefficients(regrRF)
r2 <- round(summary(regrRF)$r.squared, 3)
slope <- round(coef[2], 3)
intercept <- round(coef[1], 3)

# Create validation plot
validation_plot_file <- paste0(output_dir_fig, "/", ID[n], 
                               "_RF_gapfilled_test_new_vers_.png")

png(validation_plot_file, width = 1500, height = 800)
par(mar = c(5, 5, 0.3, 2), oma = c(2, 0, 2, 0), bg = "white")
par(cex.lab = 1.6, cex.axis = 1.5)

plot(test_set$NEE_filter, test_set$NEE_rf,
     main = "",
     xlab = "NEE_filter (observed)",
     ylab = "NEE_rf (predicted)",
     pch = 16,
     cex = 2)

# Add 1:1 line (red)
abline(a = 0, b = 1, col = "red", lwd = 3)

# Add regression line (blue)
abline(regrRF, col = "blue", lwd = 3)

# Add legend with statistics
legend("topleft", 
       legend = c(paste("R² =", r2),
                  paste("Slope =", slope),
                  paste("Intercept =", intercept)),
       bty = "n",
       cex = 1.6)

dev.off()

print(paste("Test validation plot saved to:", validation_plot_file))

# ==============================================================================
# 9. APPLY MODEL TO ENTIRE DATASET
# ==============================================================================

# Create results dataframe
result <- data.frame(NEE = ML.df$NEE_filter)
result$NEE_RF_model <- predict(RF_NEE, ML.df, na.action = na.pass)

# Gap-filled column (true value when available, predicted when missing)
result$NEE_RF_filled <- ifelse(is.na(result$NEE), 
                               result$NEE_RF_model, 
                               result$NEE)

# Residual (model - observation) for uncertainty analysis
result$NEE_RF_residual <- ifelse(is.na(result$NEE), 
                                 NA, 
                                 result$NEE_RF_model - result$NEE)

# Add to main dataset
Data_test_eddyproc$NEE_RF_filled <- result$NEE_RF_filled
Data_test_eddyproc$NEE_RF_residual <- result$NEE_RF_residual

# Parse datetime
Data_test_eddyproc$DateTime <- as.POSIXct(Data_test_eddyproc$Date, 
                                          format = "%Y-%m-%d %H:%M")

# ==============================================================================
# 10. CREATE TIME SERIES COMPARISON PLOT
# ==============================================================================

timeseries_plot_file <- paste0(output_dir_fig, "/", ID[n], 
                               "_NEE_RF_gapfilled_test_new_vers_.png")

png(timeseries_plot_file, width = 2000, height = 800)
par(cex.axis = 1.8)
par(mar = c(2, 12, 0.3, 4), oma = c(3, 0, 2, 0), bg = "white")

# Plot RF gap-filled data
plot(Data_test_eddyproc$DateTime, Data_test_eddyproc$NEE_RF_filled,
     col = "red",
     pch = 16,
     xlab = "DateTime",
     ylab = expression(paste("NEE (µmol ", m^-2, s^-1, ")")),
     cex = 1,
     cex.lab = 1.7)

# Add other series
points(Data_test_eddyproc$DateTime, Data_test_eddyproc$NEE_f,
       col = "blue", pch = 16, cex = 1)

points(Data_test_eddyproc$DateTime, Data_test_eddyproc$NEE_filter,
       col = "green", pch = 16, cex = 1)

points(Data_test_eddyproc$DateTime, Data_test_eddyproc$NEE_orig,
       col = "black", pch = 16, cex = 1)

# Add legend
legend("topright", 
       legend = c("RF", "MDS", "MDS_qc", "Real"),
       col = c("red", "blue", "green", "black"),
       pch = 16,
       cex = 2,
       horiz = TRUE,
       bty = "n")

dev.off()

print(paste("Time series plot saved to:", timeseries_plot_file))

# ==============================================================================
# 11. CALCULATE MISSING DATA PERCENTAGE BY YEAR
# ==============================================================================

pourcentage_na <- Data_test_eddyproc %>%
  group_by(year(ymd_hms(Date))) %>%
  dplyr::summarise(
    Real = mean(is.na(NEE_orig)) * 100,
    MDS = mean(is.na(NEE_f)) * 100,
    MDS_qc = mean(is.na(NEE_filter)) * 100,
    RF = mean(is.na(NEE_RF_filled)) * 100
  )

colnames(pourcentage_na) <- c("Year", "Real", "MDS", "MDS_qc", "RF")

print("Missing Data Percentage by Year:")
print(pourcentage_na)

# ==============================================================================
# 12. WHOLE DATASET VALIDATION
# ==============================================================================

# Regression on complete dataset
regrRF_whole <- lm(NEE_RF_model ~ NEE, data = result)
print("Whole Dataset Validation:")
print(summary(regrRF_whole))

# Extract statistics
coef_whole <- coefficients(regrRF_whole)
r2_whole <- round(summary(regrRF_whole)$r.squared, 3)
slope_whole <- round(coef_whole[2], 3)
intercept_whole <- round(coef_whole[1], 3)

# Create whole dataset validation plot
whole_validation_file <- paste0(output_dir_fig, "/", ID[n], 
                                "_RF_gapfilled_validation_new_vers_.png")

png(whole_validation_file, width = 1500, height = 800)
par(mar = c(5, 5, 0.3, 2), oma = c(2, 0, 2, 0), bg = "white")
par(cex.lab = 1.6, cex.axis = 1.5)

plot(result$NEE_RF_model, result$NEE,
     main = "",
     xlab = "NEE_RF_model (predicted)",
     ylab = "NEE (observed)",
     pch = 16,
     cex = 2)

abline(a = 0, b = 1, col = "red", lwd = 3)
abline(regrRF_whole, col = "blue", lwd = 3)

legend("topleft", 
       legend = c(paste("R² =", r2_whole),
                  paste("Slope =", slope_whole),
                  paste("Intercept =", intercept_whole)),
       bty = "n",
       cex = 1.6)

dev.off()

print(paste("Whole dataset validation plot saved to:", whole_validation_file))

# ==============================================================================
# 13. PREPARE NIGHTTIME AGGREGATED DATA
# ==============================================================================

# Filter nighttime data only
Nighttime_data <- filter(Data_test_eddyproc, Day_or_nigth_1.2 == 0)

# Add date components
Year <- year(Nighttime_data$Date)
Day <- yday(Nighttime_data$Date)
Date_X <- lubridate::date(Nighttime_data$Date)
Nighttime_data <- cbind(Year, Day, Date_X, Nighttime_data)

# Select relevant variables
Nighttime_data_filter <- subset(Nighttime_data, 
                                select = c("Year", "Day", "Date_X",
                                          "Hsoil1_orig", "Hsoil1_f",
                                          "Hsoil2_orig", "Hsoil2_f",
                                          "Tsoil1_orig", "Tsoil1_f",
                                          "Tsoil2_orig", "Tsoil2_f",
                                          "NEE_orig", "NEE_RF_filled"))

# ==============================================================================
# 14. CREATE YEAR-SPECIFIC DATASETS
# ==============================================================================

# Define year categories based on precipitation
Extreme_year <- c('2009')     # High precipitation
Normal_year <- c('2008')      # Normal precipitation
Deficient_year <- c('2013')   # Low precipitation

# Filter by year category
Nighttime_data_filter_extreme <- filter(Nighttime_data_filter, 
                                        Year %in% Extreme_year)
Nighttime_data_filter_normal <- filter(Nighttime_data_filter, 
                                       Year %in% Normal_year)
Nighttime_data_filter_deficient <- filter(Nighttime_data_filter, 
                                          Year %in% Deficient_year)

# ==============================================================================
# 15. AGGREGATE NIGHTTIME DATA BY DAY
# ==============================================================================

# Function to aggregate nighttime data
aggregate_nighttime <- function(data, by_year = TRUE) {
  if (by_year) {
    grouped_data <- data %>%
      group_by(Year, Day) %>%
      dplyr::summarise(
        n = n(),
        Date = unique(Date_X),
        Tsoil1_f = mean(Tsoil1_f, na.rm = TRUE),
        Tsoil2_f = mean(Tsoil2_f, na.rm = TRUE),
        Hsoil1_f = mean(Hsoil1_f, na.rm = TRUE),
        Hsoil2_f = mean(Hsoil2_f, na.rm = TRUE),
        NEE_f = mean(NEE_RF_filled, na.rm = TRUE),
        Tsoil1 = mean(Tsoil1_orig, na.rm = TRUE),
        Tsoil2 = mean(Tsoil2_orig, na.rm = TRUE),
        Hsoil1 = mean(Hsoil1_orig, na.rm = TRUE),
        Hsoil2 = mean(Hsoil2_orig, na.rm = TRUE),
        NEE = mean(NEE_orig, na.rm = TRUE),
        .groups = 'drop'
      )
  } else {
    grouped_data <- data %>%
      group_by(Day) %>%
      dplyr::summarise(
        n = n(),
        Tsoil1_f = mean(Tsoil1_f, na.rm = TRUE),
        Tsoil2_f = mean(Tsoil2_f, na.rm = TRUE),
        Hsoil1_f = mean(Hsoil1_f, na.rm = TRUE),
        Hsoil2_f = mean(Hsoil2_f, na.rm = TRUE),
        NEE_f = mean(NEE_RF_filled, na.rm = TRUE),
        Tsoil1 = mean(Tsoil1_orig, na.rm = TRUE),
        Tsoil2 = mean(Tsoil2_orig, na.rm = TRUE),
        Hsoil1 = mean(Hsoil1_orig, na.rm = TRUE),
        Hsoil2 = mean(Hsoil2_orig, na.rm = TRUE),
        NEE = mean(NEE_orig, na.rm = TRUE),
        .groups = 'drop'
      )
  }
  
  # Add ID column
  ID <- 1:nrow(grouped_data)
  grouped_data <- cbind(ID, grouped_data)
  
  return(grouped_data)
}

# Aggregate all nighttime data by year and day
df_tidy_1x <- aggregate_nighttime(Nighttime_data_filter, by_year = TRUE)

# Aggregate by day only (averaged across all years)
df_tidy_1y <- aggregate_nighttime(Nighttime_data_filter, by_year = FALSE)

# Aggregate by year category
df_tidy_1x_extreme <- aggregate_nighttime(Nighttime_data_filter_extreme, 
                                          by_year = FALSE)
df_tidy_1x_normal <- aggregate_nighttime(Nighttime_data_filter_normal, 
                                         by_year = FALSE)
df_tidy_1x_deficient <- aggregate_nighttime(Nighttime_data_filter_deficient, 
                                            by_year = FALSE)

# ==============================================================================
# 16. SAVE ALL OUTPUT FILES
# ==============================================================================

output_dir_table <- paste0("./Table/RF/", Site[n])

if (!dir.exists(output_dir_table)) {
  dir.create(output_dir_table, recursive = TRUE)
}

# Save variable importance
write.csv(importance_df,
          paste0(output_dir_table, "/", ID[n], 
                 "_Importance_variable_new_vers_.csv"),
          row.names = FALSE)

# Save complete gap-filled dataset
write.csv(Data_test_eddyproc,
          paste0(output_dir_table, "/", ID[n], 
                 "_FLX_MET_MDS_RF_output_qc_1_new_vers_.csv"),
          row.names = FALSE)

# Save missing data percentage
write.csv(pourcentage_na,
          paste0(output_dir_table, "/", ID[n], 
                 "_FLX_MET_MDS_RF_output_qc_1_percentage_vers_.csv"),
          row.names = FALSE)

# Save aggregated nighttime data
write.table(df_tidy_1x,
            file = paste0(output_dir_table, "/", ID[n], 
                         "_night_rfxxx_vers_.csv"),
            row.names = FALSE, sep = ";")

write.table(df_tidy_1x_extreme,
            file = paste0(output_dir_table, "/", ID[n], 
                         "_night_rfxxx_extreme_vers_.csv"),
            row.names = FALSE, sep = ";")

write.table(df_tidy_1x_deficient,
            file = paste0(output_dir_table, "/", ID[n], 
                         "_night_rfxxx_deficient_vers_.csv"),
            row.names = FALSE, sep = ";")

write.table(df_tidy_1x_normal,
            file = paste0(output_dir_table, "/", ID[n], 
                         "_night_rfxxx_normal_vers_.csv"),
            row.names = FALSE, sep = ";")

# Save day-averaged data
write.table(df_tidy_1y,
            file = paste0(output_dir_table, "/", ID[n], 
                         "_daynight_rfxxx_vers_.csv"),
            row.names = FALSE, sep = ";")

print("\n=== Processing Complete ===")
print(paste("All output files saved to:", output_dir_table))
print(paste("Total gap-filled rows:", nrow(Data_test_eddyproc)))
print(paste("RF model R² (test set):", r2))
print(paste("RF model R² (whole dataset):", r2_whole))