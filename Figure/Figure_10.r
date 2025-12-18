# ============================================================================
# PHASE COHERENCE ANALYSIS SCRIPT
# Sites: Nalohou & Bellefoungou
#Author: Renaud KOUKOUI
#email: renaud.koukoui@gmail.com
# Purpose: Analyze Lead or lag of Tsoil and Hsoil on NEE using phase coherence
# Figure_10
# ============================================================================


rm(list = ls(all=TRUE))
library(lubridate)
library('dplyr')
library('tidyverse')


Table_angle_nalohou_new_NeevsHs_1 <- read.csv("./Table/Phase/Nalohou/Table_angle_nalohou_new_NeevsHs_1_new_category_precipitation_.csv")
Table_angle_nalohou_new_NeevsTs_1 <- read.csv("./Table/Phase/Nalohou/Table_angle_nalohou_new_NeevsTs_1_new_category_precipitation_.csv")
Table_angle_nalohou_new_NeevsHs_2 <- read.csv("./Table/Phase/Nalohou/Table_angle_nalohou_new_NeevsHs_2_new_category_precipitation_.csv")
Table_angle_nalohou_new_NeevsTs_2 <- read.csv("./Table/Phase/Nalohou/Table_angle_nalohou_new_NeevsTs_2_new_category_precipitation_.csv")

Table_angle_bellefoungou_new_NeevsHs_1 <- read.csv("./Table/Phase/Bellefoungou/Table_angle_bellefoungou_new_NeevsHs_1_new_category_precipitation_.csv")
Table_angle_bellefoungou_new_NeevsTs_1 <- read.csv("./Table/Phase/Bellefoungou/Table_angle_bellefoungou_new_NeevsTs_1_new_category_precipitation_.csv")
Table_angle_bellefoungou_new_NeevsHs_2 <- read.csv("./Table/Phase/Bellefoungou/Table_angle_bellefoungou_new_NeevsHs_2_new_category_precipitation_.csv")
Table_angle_bellefoungou_new_NeevsTs_2 <- read.csv("./Table/Phase/Bellefoungou/Table_angle_bellefoungou_new_NeevsTs_2_new_category_precipitation_.csv")



# Définir les couleurs en fonction de Years

cex_pts = 3; cex_axis = 4; cex_text_axe = 3; cex_leg=3; cex_leg2 = 3
colors_n <- as.factor(Table_angle_nalohou_new_NeevsHs_1$Years)
# Imposer l'ordre des niveaux
colors_n <- factor(colors_n, levels = c("Deficient","Normal","Extreme"), ordered = TRUE)

# Définir manuellement les couleurs souhaitées
# Rouge pour Deficient, bleu-ciel pour Normal, bleu pour Extreme
color_palette_n <- c("red", "skyblue", "blue")

# Obtenir les niveaux du facteur dans leur ordre original
levels_order_n <- levels(colors_n)

# Créer un vecteur nommé pour l'attribution des couleurs
color_mapping_n <- setNames(color_palette_n[1:length(levels_order_n)], levels_order_n)

# Utiliser le mapping pour attribuer les couleurs
selected_colors_n <- color_mapping_n[as.character(colors_n)]

Data_selected<-filter(Table_angle_nalohou_new_NeevsHs_1,Years=="Extreme")


plot(Data_selected$Phase, Data_selected$Period,
     col = 'blue',
     pch = 19,  # Points pleins
     cex = 6,
     ylab = " ",
     xlab = " ",
     main = " ",
     xlim = c(-pi, pi), ylim = c(0, 100), xaxt = "n", yaxt = "n")

png(paste("./Figure/Phase/", "Figure_10.png", sep=""), width=2000, height=1800)

#pdf(paste("./Figure/Phase/","Nal_phase_cate_precipitation_101225_coh_greater_than_20.pdf", sep=""), width=25, height=20)

#mar(bas, gauche, haut, droite)par(mar=c(3,14,5,8), oma=c(3,0,3,0), bg="white")
par(mar=c(3,7,5,3), oma=c(3,5,3,2), bg="white")
par(mfrow=c(2,2))


# Tracer le graphique initial
plot(Table_angle_nalohou_new_NeevsHs_1$Phase, Table_angle_nalohou_new_NeevsHs_1$Period,
     col = selected_colors_n,
     pch = 19,  # Points pleins
     cex = 6,
     ylab = " ",
     xlab = " ",
     main = " ",
     xlim = c(-pi, pi), ylim = c(0, 100), xaxt = "n", yaxt = "n")

# Ajouter les points creux (non remplis) avec bordure épaisse
points(Table_angle_nalohou_new_NeevsHs_2$Phase, Table_angle_nalohou_new_NeevsHs_2$Period,
       col = selected_colors_n,  # Ou une autre palette de couleurs si nécessaire
       pch = 21,  # Points creux avec bordure
       cex = 6,
       bg = "white",  # Fond blanc pour les points creux
       lwd = 3)  # Épaisseur de la bordure (ajustez selon vos besoins)

axis(side = 2, at = c(4, 16, 28, 40, 52, 64, 76, 88, 100), label = T, cex.axis = cex_axis, las = 1, tck = 0.05, tick = T)
axis(side = 1, at = c(-pi, -pi/2, 0, pi/2, pi), labels = expression(-pi, -pi/2, 0, pi/2, pi), cex.axis = cex_axis, las = 1, tck = 0.06, tick = T, line = 2, lwd = 0, lwd.ticks = 1, lty = 1, label = T)

mtext("Periods", line = 7.5, side = 2, cex = cex_text_axe)
rect(-pi/2, -15000, pi/2, 15000, col = adjustcolor("blue", 0.1), border = adjustcolor("blue", 0.1))
abline(v = 0, col = "red", lty = 2, lwd = 2)
text(0, 100,
     expression(bold("a) NEE vs " * theta * "(nalohou)")),
     cex = 4)
box()

# Tracer le graphique initial
plot(Table_angle_bellefoungou_new_NeevsHs_1$Phase, Table_angle_bellefoungou_new_NeevsHs_1$Period,
     col = selected_colors_n,
     pch = 19,  # Points pleins
     cex = 6,
     ylab = " ",
     xlab = " ",
     main = " ",
     xlim = c(-pi, pi), ylim = c(0, 100), xaxt = "n", yaxt = "n")

# Ajouter les points creux (non remplis) avec bordure épaisse
points(Table_angle_bellefoungou_new_NeevsHs_2$Phase, Table_angle_bellefoungou_new_NeevsHs_2$Period,
       col = selected_colors_n,  # Ou une autre palette de couleurs si nécessaire
       pch = 21,  # Points creux avec bordure
       cex = 6,
       bg = "white",  # Fond blanc pour les points creux
       lwd = 3)  # Épaisseur de la bordure

axis(side = 2, at = c(4, 16, 28, 40, 52, 64, 76, 88, 100), label = T, cex.axis = cex_axis, las = 1, tck = 0.05, tick = T)
axis(side = 1, at = c(-pi, -pi/2, 0, pi/2, pi), labels = expression(-pi, -pi/2, 0, pi/2, pi), cex.axis = cex_axis, las = 1, tck = 0.06, tick = T, line = 2, lwd = 0, lwd.ticks = 1, lty = 1, label = T)

# Légende combinée pour les couleurs et les profondeurs
legend("topright", 
       legend = c('Deficient', 'Normal', 'Surplus', '', 'Depth 1', 'Depth 2'),
       col = c(color_palette_n, NA, 'black', 'black'), 
       pch = c(19, 19, 19, NA, 19, 21),
       pt.bg = c(NA, NA, NA, NA, 'black', 'white'),
       pt.lwd = c(1, 1, 1, NA, 1, 3),
       cex = 3.3, 
       horiz = FALSE, 
       bty = "n")

rect(-pi/2, -15000, pi/2, 15000, col = adjustcolor("blue", 0.1), border = adjustcolor("blue", 0.1))
abline(v = 0, col = "red", lty = 2, lwd = 2)
text(0, 100,
     expression(bold("b) NEE vs " * theta * "(bellefoungou)")),
     cex = 4)
box()

# Tracer le graphique initial
plot(Table_angle_nalohou_new_NeevsTs_1$Phase, Table_angle_nalohou_new_NeevsTs_1$Period,
     col = selected_colors_n,
     pch = 19,  # Points pleins
     cex = 6,
     ylab = " ",
     xlab = " ",
     main = " ",
     xlim = c(-pi, pi), ylim = c(0, 100), xaxt = "n", yaxt = "n")

# Ajouter les points creux (non remplis) avec bordure épaisse
points(Table_angle_nalohou_new_NeevsTs_2$Phase, Table_angle_nalohou_new_NeevsTs_2$Period,
       col = selected_colors_n,  # Ou une autre palette de couleurs si nécessaire
       pch = 21,  # Points creux avec bordure
       cex = 6,
       bg = "white",  # Fond blanc pour les points creux
       lwd = 3)  # Épaisseur de la bordure (ajustez selon vos besoins)

axis(side = 2, at = c(4, 16, 28, 40, 52, 64, 76, 88, 100), labels = TRUE, cex.axis = cex_axis, las = 1, tck = 0.05, tick = T)
axis(side = 1, at = c(-pi, -pi/2, 0, pi/2, pi), labels = expression(-pi, -pi/2, 0, pi/2, pi), cex.axis = cex_axis, las = 1, tck = 0.06, tick = T, line = 2, lwd = 0, lwd.ticks = 1, lty = 1)

mtext("Periods", line = 7.5, side = 2, cex = cex_text_axe)
rect(-pi/2, -15000, pi/2, 15000, col = adjustcolor("blue", 0.1), border = adjustcolor("blue", 0.1))
abline(v = 0, col = "red", lty = 2, lwd = 2)
text(0, 100, 
     expression(bold("c) NEE vs " * Tsoil * "(nalohou)")),
     cex = 4)

# Tracer le graphique initial
plot(Table_angle_bellefoungou_new_NeevsTs_1$Phase, Table_angle_bellefoungou_new_NeevsTs_1$Period,
     col = selected_colors_n,
     pch = 19,  # Points pleins
     cex = 6,
     ylab = " ",
     xlab = " ",
     main = " ",
     xlim = c(-pi, pi), ylim = c(0, 100), xaxt = "n", yaxt = "n")

# Ajouter les points creux (non remplis) avec bordure épaisse
points(Table_angle_bellefoungou_new_NeevsTs_2$Phase, Table_angle_bellefoungou_new_NeevsTs_2$Period,
       col = selected_colors_n,  # Ou une autre palette de couleurs si nécessaire
       pch = 21,  # Points creux avec bordure
       cex = 6,
       bg = "white",  # Fond blanc pour les points creux
       lwd = 3)  # Épaisseur de la bordure

axis(side = 2, at = c(4, 16, 28, 40, 52, 64, 76, 88, 100), labels = TRUE, cex.axis = cex_axis, las = 1, tck = 0.05, tick = T)
axis(side = 1, at = c(-pi, -pi/2, 0, pi/2, pi), labels = expression(-pi, -pi/2, 0, pi/2, pi), cex.axis = cex_axis, las = 1, tck = 0.06, tick = T, line = 2, lwd = 0, lwd.ticks = 1, lty = 1)

rect(-pi/2, -15000, pi/2, 15000, col = adjustcolor("blue", 0.1), border = adjustcolor("blue", 0.1))
abline(v = 0, col = "red", lty = 2, lwd = 2)
text(0, 100,
     expression(bold("d) NEE vs " * Tsoil * "(bellefoungou)")),
     cex = 4)
box()

dev.off()
