
# Load required libraries
#install.packages("rattle")
library(rattle)       # For Wine dataset
library(ggplot2)      # For visualization
library(cluster)      # For silhouette scores
#install.packages("factoextra")
library(factoextra)   # For PCA and clustering visualization

# Normalize function
normalize <- function(data) {
  return((data - min(data)) / (max(data) - min(data)))
}

# Step 1: Load Wine dataset and normalize
wine <- wine
wine_data <- wine[, -1]  # Remove the class label
wine_norm <- as.data.frame(lapply(wine_data, normalize))

# Step 2: Apply PCA
wine_pca <- prcomp(wine_norm, scale. = TRUE)
summary(wine_pca)

# Reduce to top 2 principal components
wine_pca_data <- as.data.frame(wine_pca$x[, 1:2])

# Step 3: Determine the optimal number of clusters (Elbow method)
elbow_wine <- fviz_nbclust(wine_pca_data, kmeans, method = "wss")
print(elbow_wine)

# Step 4: Silhouette analysis
silhouette_wine <- fviz_nbclust(wine_pca_data, kmeans, method = "silhouette")
print(silhouette_wine)

# Step 5: Apply K-means clustering
set.seed(123)
wine_kmeans <- kmeans(wine_pca_data, centers = 3, nstart = 25)

# Add cluster assignments to PCA data
wine_pca_data$cluster <- as.factor(wine_kmeans$cluster)

# Step 6: Visualize clusters
p1 <- ggplot(wine_pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "K-Means Clustering on Wine Dataset")
print(p1)

# Step 7: Interpret results
cat("Wine Dataset Clustering Results:\n")
cat("Cluster Sizes:", wine_kmeans$size, "\n")

# Step 8: Repeat for Breast Cancer Wisconsin dataset
# Load dataset from UCI repository
bc_data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data", header = FALSE)
bc_features <- bc_data[, -c(1, 2)]  # Exclude ID and class columns
bc_norm <- as.data.frame(lapply(bc_features, normalize))

# Apply PCA
bc_pca <- prcomp(bc_norm, scale. = TRUE)
summary(bc_pca)

# Reduce to top 2 principal components
bc_pca_data <- as.data.frame(bc_pca$x[, 1:2])

# Optimal number of clusters
elbow_bc <- fviz_nbclust(bc_pca_data, kmeans, method = "wss")
print(elbow_bc)

silhouette_bc <- fviz_nbclust(bc_pca_data, kmeans, method = "silhouette")
print(silhouette_bc)

# Apply K-means clustering
set.seed(123)
bc_kmeans <- kmeans(bc_pca_data, centers = 2, nstart = 25)

# Add cluster assignments to PCA data
bc_pca_data$cluster <- as.factor(bc_kmeans$cluster)

# Visualize clusters
p2 <- ggplot(bc_pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "K-Means Clustering on Breast Cancer Dataset")
print(p2)

# Interpret results
cat("Breast Cancer Dataset Clustering Results:\n")
cat("Cluster Sizes:", bc_kmeans$size, "\n")
