library(dplyr)
library(ggplot2)
library(tibble)

# Identify all numeric columns except the recommendation variable
num_cols <- names(siva)[sapply(siva, is.numeric)]
clust_df <- siva %>%
  select(any_of(setdiff(num_cols, "Recom_mend_Siva"))) %>%
  na.omit()

# Scale the data for clustering
X <- scale(clust_df)

# K-means clustering (set.seed for reproducibility, 3 clusters)
set.seed(42)
km <- kmeans(X, centers = 3, nstart = 25)

# Principal Component Analysis for visualization
pca <- prcomp(X, center = TRUE, scale. = TRUE)
pc_scores <- as_tibble(pca$x[, 1:2], .name_repair = "minimal")
pc_scores$cluster <- factor(km$cluster)

# Plot clusters along first two principal components
ggplot(pc_scores, aes(PC1, PC2, color = cluster)) +
  geom_point(alpha = 0.7) +
  labs(title = "K-means clusters (k=3) visualized on first two PCs",
       x = "PC1", y = "PC2") +
  theme_minimal()
