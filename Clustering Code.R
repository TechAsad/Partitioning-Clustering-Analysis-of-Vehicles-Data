# Step 1: Pre-processing data

# Load required packages
library(ggplot2)
library(reshape2)
library(fpc)
library(MASS)
library(caret)

#Import and preview the vehicles data
v_data <- read.csv("vehicles.csv")
head(v_data)
summary(v_data)


# Remove outliers using z-score method
remove_outliers <- function(data, threshold = 3) {
  z_scores <- apply(data, 2, function(x) abs((x - mean(x)) / sd(x)))
  data[apply(z_scores, 1, max) < threshold, ]
}

# Remove outliers from numeric attributes
numeric_data <- v_data[, 2:19]
data2 <- remove_outliers(numeric_data)

summary(data2)


# Dimensionality Reduction with Correlation-based approach

cor_matrix <- cor(data2)

highlyCorrelated <- findCorrelation(cor_matrix, cutoff=0.9)
#print indexes of highly correlated attributes
print(highlyCorrelated)
#remove 
dataNew <- data2[,-c(2, 3, 7, 8, 9,  12, 18)]

str(dataNew)

# now we have 11 attributes, 7 were dropped due to high correlation (> 0.9)


# Scaling and normalization
data_scaled <- scale(dataNew)
summary(data_scaled)

v_data_cleaned <- data_scaled


#Step 2: Find the ideal number of clusters

#a. NbCluster Method
set.seed(1234)
nc1 <- NbClust(v_data_cleaned, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")

#Bar plots to visualize number of clusters
barplot(table(nc1$Best.n[1,]),  xlab = "Number of Clusters - Euclidean", ylab = "Number of Criteria")

#b. Gap Statistics Method
gap_stat <- clusGap(v_data_cleaned, FUN = kmeans, nstart = 10, K.max = 10, B = 50)

# Plot Gap Statistics
plot(gap_stat, main = "Gap Statistics")


#c. Elbow Method
k <- 2:10
wss <- sapply(k, function(k) kmeans(v_data_cleaned, centers = k)$tot.withinss)
plot(k, wss, type = "b", main = "Elbow Plot",xlab = "Number of Clusters", ylab = "Within groups sum of squares")




#Step 3: Fit the K-means models with the best clusters

#three plots in one frame
par(mfrow = c(1, 3))

k2<- kmeans(v_data_cleaned, 2)
plotcluster(v_data_cleaned, k2$cluster,main= "K=2")
k2$size

k3<- kmeans(v_data_cleaned, 3)
plotcluster(v_data_cleaned, k3$cluster, main= "K=3")
k3$size


k4<- kmeans(v_data_cleaned, 4)
plotcluster(v_data_cleaned, k4$cluster, main= "K=4")
k4$size


#Step 4: Evaluate the K-means models


k_values <- c(2, 3, 4)
kmeans_results <- list()

for (k in k_values) {
  kmeans_results[[as.character(k)]] <- kmeans(v_data_cleaned, centers = k, nstart = 25)
}

# Calculate BSS, BSS/TSS, and WSS indices for each k-means attempt
results_summary <- data.frame(K = k_values)
for (i in 1:length(k_values)) {
  k <- k_values[i]
  kmeans_obj <- kmeans_results[[as.character(k)]]
  
  bss <- sum(kmeans_obj$size * ((colMeans(v_data_cleaned) - kmeans_obj$centers[kmeans_obj$cluster, ])^2))
  tss <- sum(kmeans_obj$size * apply(v_data_cleaned, 2, var))
  wss <- sum(kmeans_obj$withinss)
  
  results_summary$BSS[i] <- bss
  results_summary$BSS_TSS_Ratio[i] <- bss / tss
  results_summary$WSS[i] <- wss
}

#print the results summary to evaluate and compare
print(results_summary)




