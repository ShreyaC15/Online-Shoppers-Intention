# Load libraries
library(tidyverse)
library(mclust)

# Load in data
data <- read.csv("online_shoppers_intention.csv")

##### Generate exploratory plots of page visit data #########
# Set plotting parameters
par(mfrow = c(2, 3), mar = c(4, 3, 1, 1), cex.lab = 0.8, cex.axis = 0.65,
    mgp = c(1.1, 0.1, 0), tcl = -0.2)

# Plot administrative page visits based on purchase status
hist(data$Administrative[data$Revenue == TRUE], freq = FALSE, breaks = 0:27,
     right = FALSE, col = "blue", density = 30, angle = 45, xlim = c(0, 20),
     ylim = c(0, 0.6),xlab = "Administrative Visits",  ylab = "Proportional Frequency",
     main = "")
hist(data$Administrative[data$Revenue == FALSE], freq = FALSE, breaks = 0:27,
     right = FALSE,
     col = "red", density = 30, angle = 315, xlim = c(0, 20), main = "", add = TRUE)
legend(x = 10.5, y = 0.61, c("Purchase", "No Purchase"), bty = "n", cex = 0.8,
       fill = c("blue", "red"))
box(which = "plot")

# Plot informational page visits based on purchase status
hist(data$Informational[data$Revenue == TRUE], freq = FALSE, breaks = 0:24,
     right = FALSE, col = "blue", density = 30, angle = 45, xlim = c(0, 8),
     ylim = c(0, 1), xlab = "Informational Visits",  ylab = "Proportional Frequency",
     main = "")
hist(data$Informational[data$Revenue == FALSE], freq = FALSE, breaks = 0:24,
     right = FALSE,
     col = "red", density = 30, angle = 315, xlim = c(0, 8), main = "", add = TRUE)
box(which = "plot")

# Plot product-related page visits based on purchase status
hist(data$ProductRelated[data$Revenue == TRUE], freq = FALSE, breaks = seq(0, 710, 10),
     right = FALSE, col = "blue", density = 30, angle = 45, xlim = c(0, 200),
     ylim = c(0, 0.05), xlab = "Product-Related Visits",  ylab = "Proportional Frequency",
     main = "")
hist(data$ProductRelated[data$Revenue == FALSE], freq = FALSE, breaks = seq(0, 710, 10),
     right = FALSE, col = "red", density = 30, angle = 315, xlim = c(0, 200), main = "",
     add = TRUE)
box(which = "plot")

# Plot administrative page visit time based on purchase status
plot(density(data$Administrative_Duration[data$Revenue == FALSE], from = 0, to = 3300),
     lwd = 2, col = "red", xlim = c(0, 500), ylim = c(0, 0.025),
     xlab = "Administrative Visit Time (s)",
     ylab = "Probability Density", main = "")
lines(density(data$Administrative_Duration[data$Revenue == TRUE], from = 0, to = 3300),
      lwd = 2, col = "blue")

# Plot informational page visit time based on purchase status
plot(density(data$Informational_Duration[data$Revenue == FALSE], from = 0, to = 2500),
     lwd = 2, col = "red", xlim = c(0, 250), ylim = c(0, 0.05),
     xlab = "Informational Visit Time (s)",
     ylab = "Probability Density", main = "")
lines(density(data$Informational_Duration[data$Revenue == TRUE], from = 0, to = 2500),
      lwd = 2, col = "blue")

# Plot product-related page visit time based on purchase status
plot(density(data$ProductRelated_Duration[data$Revenue == FALSE], from = 0, to = 60000),
     lwd = 2, col = "red", xlim = c(0, 10000), ylim = c(0, 0.001),
     xlab = "Product-Related Visit Time (s)",
     ylab = "Probability Density", main = "")
lines(density(data$ProductRelated_Duration[data$Revenue == TRUE], from = 0, to = 60000),
      lwd = 2, col = "blue")

##### Generate exploratory plots of page visit data ############
# Set plotting parameters
par(mfrow = c(1, 3), mar = c(4, 3, 1, 1), cex.lab = 0.8, cex.axis = 0.65,
    mgp = c(1.1, 0.1, 0), tcl = -0.2)

# Plot bounce rates based on purchase status
plot(density(data$BounceRates[data$Revenue == FALSE], from = 0, to = 0.2), lwd = 2,
     col = "red", xlim = c(0, 0.2), ylim = c(0, 40),
     xlab = "Bounce Rate",
     ylab = "Probability Density", main = "")
lines(density(data$BounceRates[data$Revenue == TRUE], from = 0, to = 0.2),
      lwd = 2, col = "blue")
legend(x = 0.105, y = 40, c("Purchase", "No Purchase"), bty = "n", cex = 0.8,
       fill = c("blue", "red"))

# Plot exit rates based on purchase status
plot(density(data$ExitRates[data$Revenue == FALSE], from = 0, to = 0.2), lwd = 2,
     col = "red", xlim = c(0, 0.2), ylim = c(0, 40),
     xlab = "Exit Rate",
     ylab = "Probability Density", main = "")
lines(density(data$ExitRates[data$Revenue == TRUE], from = 0, to = 0.2),
      lwd = 2, col = "blue")

# Plot page value based on purchase status
plot(density(data$PageValues[data$Revenue == FALSE], from = 0, to = 360), lwd = 2,
     col = "red", xlim = c(0, 200), ylim = c(0, 0.04),
     xlab = "Page Value",
     ylab = "Probability Density", main = "")
lines(density(data$PageValues[data$Revenue == TRUE], from = 0, to = 360),
      lwd = 2, col = "blue")

# Function to calculate standard error
se <- function(x){
  return(sd(x)/sqrt(length(x)))}

# Function to calculate SS and ARI from a single k-means run
clustK <- function(k){
  km.out <- kmeans(data[, 1:10], k, nstart = 1)
  wss <- km.out$tot.withinss
  ari <- adjustedRandIndex(data$Revenue, km.out$cluster)
  return(list(wss = wss, ari = ari))}

# Function to calculate ARI from a single GMM run
clustM <- function(k){
  gm.out <- Mclust(data[, 1:10], k)
  ari <- adjustedRandIndex(data$Revenue, gm.out$classification)
  return(ari)}

# Calculate mean and SE of SS and ARI for 100 k-means runs
kwM <- c()
kwS <- c()
kaM <- c()
kaS <- c()
set.seed(927383477)
for(i in 1:15){
  vals <- replicate(100, clustK(i))
  kwM[i] <- mean(unlist(vals[1, ]))
  kwS[i] <- se(unlist(vals[1, ]))
  kaM[i] <- mean(unlist(vals[2, ]))
  kaS[i] <- se(unlist(vals[2, ]))}

# Mean and SE of ARI for 100 GMM runs, calculated from code in appendix
maM1 <- c(0, 0.0510, 0.0125, 0.0887, 0.0838, 0.1158, 0.1632, 0.1871,
          0.1544, 0.1539, 0.1309, 0.0812, 0.0536, 0.0424, 0.0245)
maS1 <- c(0, 0.0049, 0.0038, 0.0045, 0.0087, 0.0106, 0.0122, 0.0114,
          0.0119, 0.0115, 0.0121, 0.0102, 0.0077, 0.0067, 0.0046)

# Set plotting parameters
par(mfrow = c(1, 3), mar = c(4, 3, 1, 1), cex.lab = 0.8, cex.axis = 0.65,
    mgp = c(1.1, 0.1, 0), tcl = -0.2)

# Plot SS and ARI for k-means, and ARI for GMM
plot(kwM, type = "o", pch = 16, xlim = c(0, 15), ylim = c(0, 5e10), cex = 0.7,
     xlab = "Number of Clusters", ylab = "Mean Within-Cluster Sum of Squares")
segments(x0 = 1:15, y0 = kwM - kwS, x1 = 1:15, y1 = kwM + kwS)
text(x = 15, y = 4.8e10, "KM", adj = 1)
plot(kaM, type = "o", pch = 16, xlim = c(0, 15), ylim = c(0, 0.1), cex = 0.7,
     xlab = "Number of Clusters", ylab = "Mean Adjusted Rand Index")
segments(x0 = 1:15, y0 = kaM - kaS, x1 = 1:15, y1 = kaM + kaS)
text(x = 15, y = 0.096, "KM", adj = 1)
plot(maM1, type = "o", pch = 16, xlim = c(0, 15), ylim = c(0, 0.25), cex = 0.7,
     xlab = "Number of Clusters", ylab = "Mean Adjusted Rand Index")
segments(x0 = 1:15, y0 = maM1 - maS1, x1 = 1:15, y1 = maM1 + maS1)
text(x = 15, y = 0.24, "GM", adj = 1)

# Perform PCA on non-categorical data
pc.out <- prcomp(data[, 1:10], scale = TRUE)
pc.pve <- 100*pc.out$sdev^2/sum(pc.out$sdev^2)

# Function to calculate SS and ARI from a single k-means run (w/ PCA)
clustKPC <- function(k){
  km.out <- kmeans(pc.out$x[, 1:2], k, nstart = 1)
  wss <- km.out$tot.withinss
  ari <- adjustedRandIndex(data$Revenue, km.out$cluster)
  return(list(wss = wss, ari = ari))}

# Function to calculate ARI from a single GMM run (w/ PCA)
clustMPC <- function(k){
  gm.out <- Mclust(pc.out$x[, 1:2], k)
  ari <- adjustedRandIndex(data$Revenue, gm.out$classification)
  return(ari)}

# Calculate mean and SE of SS and ARI for 100 k-means runs
kwM <- c()
kwS <- c()
kaM <- c()
kaS <- c()
set.seed(927383477)
for(i in 1:15){
  vals <- replicate(100, clustKPC(i))
  kwM[i] <- mean(unlist(vals[1, ]))
  kwS[i] <- se(unlist(vals[1, ]))
  kaM[i] <- mean(unlist(vals[2, ]))
  kaS[i] <- se(unlist(vals[2, ]))}

#Calculate mean and SE of SS and ARI for 100 GMM runs
maM2 <- c()
maS2 <- c()
set.seed(110885672)
for(i in 1:15){
 vals <- replicate(100, clustMPC(i))
 maM2[i] <- mean(vals)
 maS2[i] <- se(vals)}

# Mean and SE of ARI for 100 GMM runs, calculated from code in appendix
maM2 <- c(0, 0.0058, 0.0048, 0.0062, 0.0026, 0.0056, 0.0078, 0.0106, 0.0107,
          0.0100, 0.0098, 0.0085, 0.0085, 0.0079, 0.0084)
maS2 <- c(0, 4.9e-5, 7.0e-5, 2.0e-5, 1.5e-4, 2.5e-4, 4.5e-4, 4.9e-4, 3.9e-4,
          3.6e-4, 3.3e-4, 3.2e-4, 2.4e-4, 3.1e-4, 3.4e-4)

# Set plotting parameters
par(mfrow = c(1, 3), mar = c(4, 3, 1, 1), cex.lab = 0.8, cex.axis = 0.65,
    mgp = c(1.1, 0.1, 0), tcl = -0.2)

# Plot SS and ARI for k-means, and ARI for GMM
plot(kwM, type = "o", pch = 16, xlim = c(0, 15), ylim = c(0, 1e5), cex = 0.7,
     xlab = "Number of Clusters", ylab = "Mean Within-Cluster Sum of Squares")
segments(x0 = 1:15, y0 = kwM - kwS, x1 = 1:15, y1 = kwM + kwS)
text(x = 15, y = 9.6e4, "KM", adj = 1)
plot(kaM, type = "o", pch = 16, xlim = c(0, 15), ylim = c(0, 0.05), cex = 0.7,
     xlab = "Number of Clusters", ylab = "Mean Adjusted Rand Index")
segments(x0 = 1:15, y0 = kaM - kaS, x1 = 1:15, y1 = kaM + kaS)
text(x = 15, y = 0.048, "KM", adj = 1)
plot(maM2, type = "o", pch = 16, xlim = c(0, 15), ylim = c(0, 0.025), cex = 0.7,
     xlab = "Number of Clusters", ylab = "Mean Adjusted Rand Index")
segments(x0 = 1:15, y0 = maM2 - maS2, x1 = 1:15, y1 = maM2 + maS2)
text(x = 15, y = 0.024, "GM", adj = 1)

# Set plotting parameters
par(mfrow = c(1, 2), mar = c(4, 3, 1, 1), cex.lab = 0.8, cex.axis = 0.65,
    mgp = c(1.1, 0.1, 0), tcl = -0.2)

# Generate Scree plot
plot(pc.pve, type = "o", ylab = "PVE", xlab = "Principal Component",
     pch = 16, cex = 0.7)
plot(cumsum(pc.pve), type = "o", ylab = "Cumulative PVE", xlab = "Principal Component",
     pch = 16, cex = 0.7)

# Set plotting parameters
par(mfrow = c(1, 3), mar = c(4, 3, 1, 1), cex.lab = 0.8, cex.axis = 0.65,
    mgp = c(1.1, 0.1, 0), tcl = -0.2)

# Plot actual classes in PC1-PC2 space
colours <- rep(rgb(1, 0, 0, 0.5), nrow(data))
colours[data$Revenue == "TRUE"] <- rgb(0, 0, 1, 0.5)
plot(pc.out$x[, 1], pc.out$x[, 2], col = colours, xlim = c(-5, 10), ylim = c(-3, 5), pch = 16,
     xlab = "PC1", ylab = "PC2")
text(x = 10, y = -2.8, "Observed", adj = 1)

# Plot clusters from k-means
set.seed(121116609)
km.out <- kmeans(pc.out$x[, 1:2], 2, nstart = 1)
colours <- rep(rgb(1, 0, 0, 0.5), nrow(data))
colours[km.out$cluster == 2] <- rgb(0, 0, 1, 0.5)
plot(pc.out$x[, 1], pc.out$x[, 2], col = colours, xlim = c(-5, 10), ylim = c(-3, 5), pch = 16,
     xlab = "PC1", ylab = "PC2")
text(x = 10, y = -2.8, "KM, 2", adj = 1)

# Plot clusters from GMM
set.seed(818495743)
gm.out <- Mclust(pc.out$x[, 1:2], 2)
colours <- rep(rgb(1, 0, 0, 0.5), nrow(data))
colours[gm.out$classification == 2] <- rgb(0, 0, 1, 0.5)
plot(pc.out$x[, 1], pc.out$x[, 2], col = colours, xlim = c(-5, 10), ylim = c(-3, 5), pch = 16,
     xlab = "PC1", ylab = "PC2")
text(x = 10, y = -2.8, "GM, 2", adj = 1)

# Set plotting parameters
par(mfrow = c(2, 3), mar = c(4, 3, 1, 1), cex.lab = 0.8, cex.axis = 0.65,
    mgp = c(1.1, 0.1, 0), tcl = -0.2)

# Plot clusters for k-means w/ k=3
set.seed(121116609)
km.out <- kmeans(pc.out$x[, 1:2], 3, nstart = 1)
colours <- rep(rgb(0, 0, 1, 0.5), nrow(data))
colours[km.out$cluster == 2] <- rgb(0, 1, 0, 0.5)
colours[km.out$cluster == 3] <- rgb(1, 0, 0, 0.5)
plot(pc.out$x[, 1], pc.out$x[, 2], col = colours, xlim = c(-5, 10), ylim = c(-3, 5), pch = 16,
     xlab = "PC1", ylab = "PC2")
text(x = 10, y = -2.8, "KM, 3", adj = 1)

# Plot clusters for k-means w/ k=4
set.seed(121116609)
km.out <- kmeans(pc.out$x[, 1:2], 4, nstart = 1)
colours <- rep(rgb(1, 1, 0, 0.5), nrow(data))
colours[km.out$cluster == 2] <- rgb(0, 0, 1, 0.5)
colours[km.out$cluster == 3] <- rgb(1, 0, 0, 0.5)
colours[km.out$cluster == 4] <- rgb(0, 1, 0, 0.5)
plot(pc.out$x[, 1], pc.out$x[, 2], col = colours, xlim = c(-5, 10), ylim = c(-3, 5), pch = 16,
     xlab = "PC1", ylab = "PC2")
text(x = 10, y = -2.8, "KM, 4", adj = 1)

# Plot clusters for k-means w/ k=5
set.seed(121116609)
km.out <- kmeans(pc.out$x[, 1:2], 5, nstart = 1)
colours <- rep(rgb(0, 0, 1, 0.5), nrow(data))
colours[km.out$cluster == 2] <- rgb(1, 1, 0, 0.5)
colours[km.out$cluster == 3] <- rgb(1, 0, 0, 0.5)
colours[km.out$cluster == 4] <- rgb(0, 1, 0, 0.5)
colours[km.out$cluster == 5] <- rgb(0.5, 0, 0.5, 0.5)
plot(pc.out$x[, 1], pc.out$x[, 2], col = colours, xlim = c(-5, 10), ylim = c(-3, 5), pch = 16,
     xlab = "PC1", ylab = "PC2")
text(x = 10, y = -2.8, "KM, 5", adj = 1)

# Plot clusters for GMM w/ k=3
set.seed(818495743)
gm.out <- Mclust(pc.out$x[, 1:2], 3)
colours <- rep(rgb(1, 0, 0, 0.5), nrow(data))
colours[gm.out$classification == 2] <- rgb(0, 0, 1, 0.5)
colours[gm.out$classification == 3] <- rgb(0, 1, 0, 0.5)
plot(pc.out$x[, 1], pc.out$x[, 2], col = colours, xlim = c(-5, 10), ylim = c(-3, 5), pch = 16,
     xlab = "PC1", ylab = "PC2")
text(x = 10, y = -2.8, "GM, 3", adj = 1)

#pc.out$x[, 1][gm.out$classification == 3]
#pc.out$x[, 2][gm.out$classification == 3]

# Plot clusters for GMM w/ k=4
set.seed(818495743)
gm.out <- Mclust(pc.out$x[, 1:2], 4)
colours <- rep(rgb(1, 0, 0, 0.5), nrow(data))
colours[gm.out$classification == 2] <- rgb(0, 0, 1, 0.5)
colours[gm.out$classification == 3] <- rgb(0, 1, 0, 0.5)
colours[gm.out$classification == 4] <- rgb(1, 1, 0, 0.5)
plot(pc.out$x[, 1], pc.out$x[, 2], col = colours, xlim = c(-5, 10), ylim = c(-3, 5), pch = 16,
     xlab = "PC1", ylab = "PC2")
text(x = 10, y = -2.8, "GM, 4", adj = 1)

# Plot clusters for GMM w/ k=5
set.seed(818495743)
gm.out <- Mclust(pc.out$x[, 1:2], 5)
colours <- rep(rgb(1, 1, 0, 0.5), nrow(data))
colours[gm.out$classification == 2] <- rgb(0, 0, 1, 0.5)
colours[gm.out$classification == 3] <- rgb(1, 0, 0, 0.5)
colours[gm.out$classification == 4] <- rgb(0, 1, 0, 0.5)
colours[gm.out$classification == 5] <- rgb(0.5, 0, 0.5, 0.5)
plot(pc.out$x[, 1], pc.out$x[, 2], col = colours, xlim = c(-5, 10), ylim = c(-3, 5), pch = 16,
     xlab = "PC1", ylab = "PC2")
text(x = 10, y = -2.8, "GM, 5", adj = 1)

