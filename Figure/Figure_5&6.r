# ============================================================================
# MUTUAL INFORMATION ANALYSIS SCRIPT
# Sites: Nalohou & Bellefoungou
#Author: Renaud KOUKOUI
#email: renaud.koukoui@gmail.com
# Purpose: Analyze variable importance using mutual information with common scale
# Figure_5&6
# ============================================================================

# Clear workspace and load libraries
rm(list = ls(all=TRUE))

# Load required libraries
library(dplyr)
library(varrank)
library(EntropyExplorer)

# ============================================================================
# 1. CONFIGURATION
# ============================================================================

# Site information
Site <- c("Nalohou", "Bellefoungou")
ID <- c("Nal", "Bel")

# Season definitions
Season <- c("SS", "ST1", "SH", "ST2")
Season_name <- c("Dry", "Transition_1", "Wet", "Transition_2")
season_labels <- c("dry", "moistening", "wet", "drying")

# Define paths
base_path <- "./Table/Filtrage/"
output_path <- "./Figure/Correlation/"

# Variables to analyze
vars_to_keep <- c("NEE", "Hv1", "Hv2", "Tsol_1_10", "Tsol_1_20")
vars_labels <- c("NEE", expression(theta[1]), expression(theta[2]), "Tsoil1", "Tsoil2")

# ============================================================================
# 2. CUSTOM PLOTTING FUNCTIONS
# ============================================================================

## Modified plot function for varrank with common scale
plot.varrank_r <- function(x,
                          common_breaks = NULL,
                          common_palette = NULL,
                          colsep = TRUE,
                          rowsep = TRUE,
                          sepcol = "white",
                          sepwidth = c(0.005, 0.005),
                          cellnote = TRUE,
                          notecex = 1.5,
                          notecol = "black",
                          digitcell = 3,
                          margins = c(6, 6, 4, 2),
                          labelscex = 1.2,
                          colkey = NULL,
                          densadj = 0.25,
                          textlines = 2,
                          main = NULL,
                          maincex = 1,
                          ...
){
  
  # Scaling function
  scale201 <- function(x, low = min(x), high = max(x)) {
    return((x - low) / (high - low))
  }
  
  x.algo <- x$algorithm
  x.scheme <- x$scheme
  x <- x[[2]]
  
  if(length(dimx <- dim(x)) != 2 || !is.numeric(x))
    stop("varrank object 'x' must be a numeric matrix.")
  
  n <- dimx[1]
  n.2 <- dimx[2]
  
  if(n <= 1)
    stop("varrank object 'x' must have at least 2 rows and 2 columns.")
  
  if(!is.numeric(margins) || length(margins) != 4)
    stop("'margins' must be a numeric vector of length 4.")
  
  # Use common palette if provided
  if(is.null(common_palette)){
    if(is.null(colkey)){
      cool <- rainbow(50, start = rgb2hsv(col2rgb('cyan'))[1], 
                     end = rgb2hsv(col2rgb('blue'))[1])
      warm <- rainbow(50, start = rgb2hsv(col2rgb('red'))[1], 
                     end = rgb2hsv(col2rgb('yellow'))[1])
      cols <- c(rev(cool), rev(warm))
      mypalette <- colorRampPalette(cols)(255)
    } else {
      mypalette <- colkey
    }
  } else {
    mypalette <- common_palette
  }
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  # Layout setup
  if(x.algo == "forward"){
    layout(matrix(c(1,1,1,1,1,1,2,2,2,2,
                    1,1,1,1,1,1,2,2,2,2,
                    1,1,1,1,1,1,2,2,2,2,
                    1,1,1,1,1,1,2,2,2,2,
                    1,1,1,1,1,1,1,1,1,1,
                    1,1,1,1,1,1,1,1,1,1,
                    1,1,1,1,1,1,1,1,1,1,
                    1,1,1,1,1,1,1,1,1,1,
                    1,1,1,1,1,1,1,1,1,1,
                    1,1,1,1,1,1,1,1,1,1), nrow = 10, ncol = 10, byrow = TRUE))
  }
  if(x.algo == "backward"){
    layout(matrix(c(1,1,1,1,1,1,1,1,1,1,
                    1,1,1,1,1,1,1,1,1,1,
                    1,1,1,1,1,1,1,1,1,1,
                    1,1,1,1,1,1,1,1,1,1,
                    1,1,1,1,1,1,1,1,1,1,
                    1,1,1,1,1,1,1,1,1,1,
                    1,1,1,1,1,1,2,2,2,2,
                    1,1,1,1,1,1,2,2,2,2,
                    1,1,1,1,1,1,2,2,2,2,
                    1,1,1,1,1,1,2,2,2,2), nrow = 10, ncol = 10, byrow = TRUE))
  }
  
  par(mar = margins)
  
  # Use common breaks if provided
  if(is.null(common_breaks)){
    extreme <- max(abs(x), na.rm = TRUE)
    breaks <- seq(-extreme, extreme, length = length(mypalette) + 1)
  } else {
    breaks <- common_breaks
  }
  
  image(1:n.2, 1:n, t(x[n:1,]), xlim = 0.5 + c(0, n), ylim = 0.5 + c(0, n),
        axes = FALSE, xlab = "", ylab = "", col = mypalette, breaks = breaks, ...)
  
  # Draw separators
  if(x.algo == "forward"){
    if(colsep) {
      rect(xleft = .5, ybottom = 0, xright = .5 + sepwidth[1], ytop = nrow(x) + 1.5,
           lty = 1, lwd = 1, col = sepcol, border = sepcol)
      for(csep in 1:min(n.2, n - 1)){
        rect(xleft = csep + 0.5, ybottom = 0, xright = csep + 0.5 + sepwidth[1], 
             ytop = nrow(x) + 1.5 - csep, lty = 1, lwd = 1, col = sepcol, border = sepcol)
      }
    }
    if(rowsep) {
      for(rsep in 1:n){
        rect(xleft = 0.5, ybottom = (nrow(x) + 1 - rsep) - 0.5,
             xright = 1.5 + min(rsep, n.2 - 1, n - 2), 
             ytop = (nrow(x) + 1 - rsep) - 0.5 - sepwidth[2],
             lty = 1, lwd = 1, col = sepcol, border = sepcol)
      }
      rect(xleft = 0.5, ybottom = (nrow(x) + 1) - 0.5, xright = 1.5, 
           ytop = (nrow(x) + 1) - 0.5 - sepwidth[2], lty = 1, lwd = 1, 
           col = sepcol, border = sepcol)
    }
    axis(1, 1:n, labels = rownames(x[1:n,]), las = 2, tick = 0, cex.axis = labelscex)
  }
  
  if(x.algo == "backward"){
    n.2 <- n.2 - 1
    if(colsep){
      rect(xleft = .5, ybottom = 0.5, xright = .5 + sepwidth[1], ytop = nrow(x) + .5,
           lty = 1, lwd = 1, col = sepcol, border = sepcol)
      for(csep in 1:min(n.2, n - 1)){
        rect(xleft = csep + 0.5, ybottom = -0.5 + csep, 
             xright = csep + 0.5 + sepwidth[1], 
             ytop = nrow(x) + .5, lty = 1, lwd = 1, col = sepcol, border = sepcol)
      }
    }
    if(rowsep) {
      for(rsep in 1:n){
        rect(xleft = 0.5, ybottom = (nrow(x) + 1 - rsep) - 0.5,
             xright = 1.5 + min(n - rsep, n.2 - 1), 
             ytop = (nrow(x) + 1 - rsep) - 0.5 - sepwidth[2],
             lty = 1, lwd = 1, col = sepcol, border = sepcol)
      }
      rect(xleft = 0.5, ybottom = (nrow(x) + 1) - 0.5, xright = n.2 + .5, 
           ytop = (nrow(x) + 1) - 0.5 - sepwidth[2], lty = 1, lwd = 1, 
           col = sepcol, border = sepcol)
    }
    axis(3, 1:n, labels = rownames(x[n:1,]), las = 2, tick = 0, cex.axis = labelscex)
  }
  
  axis(2, 1:n, labels = rownames(x[n:1,]), las = 2, tick = 0, cex.axis = labelscex)
  
  # Add cell notes
  if(cellnote){
    cellnote_matrix <- x[n:1,]
    cellnote <- sprintf(paste0("%.", digitcell, "f"), cellnote_matrix)
    cellnote[is.na(cellnote_matrix)] <- ""
    dim(cellnote) <- dim(cellnote_matrix)
    text(x = c(col(cellnote_matrix)), y = c(row(cellnote_matrix)), 
         labels = c(cellnote), col = notecol, cex = notecex)
  }
  
  if(!is.null(main)) title(main, cex.main = 1.5 * maincex)
  
  # Color key
  zlim <- max(abs(min(breaks)), abs(max(breaks)))
  z <- seq(from = -zlim, to = zlim, length = length(mypalette))
  image(z = matrix(z, ncol = 1), col = mypalette, breaks = breaks,
        xaxt = "n", yaxt = "n", ylim = c(0, 1))
  
  # Density plot
  dens <- density(x, adjust = densadj, na.rm = TRUE, 
                 from = min(breaks), to = max(breaks))
  dens$x <- scale201(dens$x, min(breaks), max(breaks))
  
  labelscex <- 2
  lv <- pretty(breaks)
  xv <- scale201(as.numeric(lv), min(breaks), max(breaks))
  xargs <- list(at = xv, labels = lv, cex.axis = labelscex)
  xargs$side <- 1
  do.call(axis, xargs)
  mtext(side = 1, "Redundancy        Relevancy", line = textlines, cex = 0.8 * maincex)
  invisible()
}

## Function to compute common scale across multiple varrank objects
compute_common_scale <- function(varrank_objects) {
  all_matrices <- lapply(varrank_objects, function(obj) obj[[2]])
  global_extreme <- max(sapply(all_matrices, function(mat) max(abs(mat), na.rm = TRUE)), 
                        na.rm = TRUE)
  
  cool <- rainbow(50, start = rgb2hsv(col2rgb('cyan'))[1], 
                 end = rgb2hsv(col2rgb('blue'))[1])
  warm <- rainbow(50, start = rgb2hsv(col2rgb('red'))[1], 
                 end = rgb2hsv(col2rgb('yellow'))[1])
  cols <- c(rev(cool), rev(warm))
  common_palette <- colorRampPalette(cols)(255)
  common_breaks <- seq(-global_extreme, global_extreme, length = length(common_palette) + 1)
  
  return(list(
    breaks = common_breaks,
    palette = common_palette,
    extreme = global_extreme
  ))
}

# ============================================================================
# 3. DATA LOADING AND PREPROCESSING
# ============================================================================

cat("Loading data...\n")

# Load data for both sites
Data_test_n <- read.csv(
  paste0(base_path, Site[1], "/", ID[1], "_Filtrage_vers_ _ustar_without_neg_night.csv"),
  sep = ";"
)

Data_test_b <- read.csv(
  paste0(base_path, Site[2], "/", ID[2], "_Filtrage_vers_ _ustar_without_neg_night.csv"),
  sep = ";"
)

# Filter nighttime data
Nalohou_MI <- filter(Data_test_n, Day_or_nigth_1.2 == 0)
Bellefoungou_MI <- filter(Data_test_b, Day_or_nigth_1.2 == 0)

# ============================================================================
# 4. PREPARE DATASETS BY SEASON AND SITE
# ============================================================================

# Function to prepare datasets for a site
prepare_datasets <- function(data, site_name) {
  cat(paste0("Preparing datasets for ", site_name, "...\n"))
  
  # Overall dataset
  data_all <- subset(data, select = vars_to_keep)
  colnames(data_all) <- vars_labels
  
  # Seasonal datasets
  datasets <- list(
    all = data_all,
    dry = subset(filter(data, Saison == Season[1]), select = vars_to_keep),
    transition_1 = subset(filter(data, Saison == Season[2]), select = vars_to_keep),
    wet = subset(filter(data, Saison == Season[3]), select = vars_to_keep),
    transition_2 = subset(filter(data, Saison == Season[4]), select = vars_to_keep)
  )
  
  # Rename columns for all datasets
  for(i in 2:5) {
    colnames(datasets[[i]]) <- vars_labels
  }
  
  return(datasets)
}

# Prepare datasets for both sites
data_n <- prepare_datasets(Nalohou_MI, "Nalohou")
data_b <- prepare_datasets(Bellefoungou_MI, "Bellefoungou")

# ============================================================================
# 5. MUTUAL INFORMATION CALCULATION
# ============================================================================

cat("Computing mutual information...\n")

# Function to compute MI for a dataset
compute_mi <- function(data, label) {
  cat(paste0("  Processing: ", label, "\n"))
  
  mi_result <- varrank(
    data.df = data, 
    method = 'peng', 
    variable.important = "NEE", 
    discretization.method = 'sturges', 
    algorithm = "forward", 
    scheme = "mid", 
    verbose = FALSE
  )
  
  return(mi_result)
}

# Compute MI for Nalohou
MI_n <- list(
  all = compute_mi(data_n$all, "Nalohou - All"),
  dry = compute_mi(data_n$dry, "Nalohou - Dry"),
  transition_1 = compute_mi(data_n$transition_1, "Nalohou - Transition 1"),
  wet = compute_mi(data_n$wet, "Nalohou - Wet"),
  transition_2 = compute_mi(data_n$transition_2, "Nalohou - Transition 2")
)

# Compute MI for Bellefoungou
MI_b <- list(
  all = compute_mi(data_b$all, "Bellefoungou - All"),
  dry = compute_mi(data_b$dry, "Bellefoungou - Dry"),
  transition_1 = compute_mi(data_b$transition_1, "Bellefoungou - Transition 1"),
  wet = compute_mi(data_b$wet, "Bellefoungou - Wet"),
  transition_2 = compute_mi(data_b$transition_2, "Bellefoungou - Transition 2")
)

# ============================================================================
# 6. CALCULATE LAST VARIABLE GAIN (g_last)
# ============================================================================

cat("Calculating last variable gain...\n")

# Function to calculate g_last
calculate_g_last <- function(data, mi_result) {
  ordered_var <- mi_result$ordered.var
  
  g_last <- mi.data(X = data$NEE, Y = data[, ordered_var[4]], 
                   discretization.method = "sturges") - 
            (mi.data(X = data[, ordered_var[1]], Y = data[, ordered_var[4]], 
                    discretization.method = "sturges") +
             mi.data(X = data[, ordered_var[2]], Y = data[, ordered_var[4]], 
                    discretization.method = "sturges") +
             mi.data(X = data[, ordered_var[3]], Y = data[, ordered_var[4]], 
                    discretization.method = "sturges")) / 3
  
  return(g_last)
}

# Calculate and update g_last for Nalohou
for(season in names(MI_n)) {
  g_last <- calculate_g_last(data_n[[season]], MI_n[[season]])
  MI_n[[season]]$distance.m[4, 4] <- g_last
}

# Calculate and update g_last for Bellefoungou
for(season in names(MI_b)) {
  g_last <- calculate_g_last(data_b[[season]], MI_b[[season]])
  MI_b[[season]]$distance.m[4, 4] <- g_last
}

# Print summaries
cat("\nSummary for Nalohou (all years):\n")
print(summary(MI_n$all))

cat("\nSummary for Bellefoungou (all years):\n")
print(summary(MI_b$all))

# ============================================================================
# 7. COMPUTE COMMON SCALE
# ============================================================================

cat("\nComputing common scale across all analyses...\n")

varrank_list <- c(MI_n, MI_b)
common_scale <- compute_common_scale(varrank_list)

cat(paste0("Global extreme value: ", round(common_scale$extreme, 3), "\n"))

# ============================================================================
# 8. GENERATE PLOTS
# ============================================================================

cat("\nGenerating plots...\n")

# Plot parameters
plot_params <- list(
  sepwidth = c(0.005, 0.005),
  notecex = 4,
  digitcell = 2,
  labelscex = 4,
  margins = c(12, 12, 5, 5),
  densadj = 0.25,
  textlines = 4,
  maincex = 3,
  common_breaks = common_scale$breaks,
  common_palette = common_scale$palette
)

# Function to create and save plot
create_plot <- function(mi_obj, site, site_id, season, label, letter) {
  filename <- paste0(output_path, site, "/MI_", site_id, "_", season, "_new_vers.svg")
  
  cat(paste0("  Creating: ", filename, "\n"))
  
  svg(filename, width = 18, height = 8)
  plot.new()
  
  title_text <- paste0("(", letter, ") ", label)
  
  do.call(plot.varrank_r, c(list(x = mi_obj, main = title_text), plot_params))
  
  dev.off()
}

# Define plot configurations
plot_config <- data.frame(
  season = c("all", "dry", "transition_1", "wet", "transition_2"),
  season_file = c("all", "dry", "transition_1", "wet", "transition_2"),
  season_label = c("all years", paste0("all ", season_labels)),
  letter = c("a", "b", "c", "d", "e"),
  stringsAsFactors = FALSE
)

# Generate plots for both sites
for(i in 1:nrow(plot_config)) {
  season <- plot_config$season[i]
  season_file <- plot_config$season_file[i]
  season_label <- plot_config$season_label[i]
  letter <- plot_config$letter[i]
  
  # Nalohou
  create_plot(MI_n[[season]], Site[1], ID[1], season_file, 
             paste0("Cultivated savanna/", season_label), letter)
  
  # Bellefoungou
  create_plot(MI_b[[season]], Site[2], ID[2], season_file, 
             paste0("Clear forest/", season_label), letter)
}

cat("\n=== Analysis complete! ===\n")
cat(paste0("Plots saved to: ", output_path, "\n"))