library(ggplot2)
library(dplyr)

#library(tidyverse) # Includes ggplot2, dplyr, tidyr, readr
library(corrplot)
library(cluster) 
library(factoextra)

# --- Data Loading and Preparation ---

# Load the dataset
# Assuming 'siva.csv' is in your current working directory.
data <- read_csv("/Users/chintanjikkar/Desktop/PACE/Fall 25/Visual Analytics/Data/siva.csv", na = c("NA", ""))

# Select the numerical variables relevant to the assignment's focus (ratings and charges).
# The variables selected here align with the assignment's suggestion and common practice.
numerical_vars <- data %>%
  select(
    Recom_mend_Siva, # Dependent variable for regression
    Staff_Courtesy,
    Speed_of_Service,
    Veh_Equip_Condition,
    Trans_Billing_as_Expected,
    Value_for_the_Money,
    Total_charge_USD,
    Survey_checkout_diff
  ) %>%
  # Coerce selected columns to numeric, handling potential mixed types
  mutate(across(everything(), as.numeric))

# Remove rows with any missing (NA) values for consistent analysis
clean_data <- na.omit(numerical_vars)

cat("\nSummary of Cleaned Data (first 6 rows):\n")
print(head(clean_data))
cat("\nStructure of Cleaned Data:\n")
print(str(clean_data))

# ==============================================================================
## PART 1: CORRELATION PLOT
# ==============================================================================

# Calculate the correlation matrix
correlation_matrix <- cor(clean_data)

cat("\n--- Generating Correlation Plot ---\n")

# Use corrplot for a professional-looking visualization
corrplot(
  correlation_matrix,
  method = "circle",
  type = "upper",      # Only show upper triangle
  order = "hclust",    # Cluster variables by similarity
  tl.col = "black",    # Text label color
  tl.srt = 45,         # Text label rotation
  diag = FALSE,        # Hide diagonal
  title = "Correlation Matrix of Customer Survey Variables",
  mar = c(0, 0, 1, 0) # Adjust margins for plot title
)

# ==============================================================================
## PART 2: VISUALIZATION FOR A REGRESSION MODEL
#          Dependent Variable: Recom_mend_Siva
# ==============================================================================

# Build the multiple linear regression model
# Using key satisfaction ratings as predictors
regression_model <- lm(
  Recom_mend_Siva ~ Staff_Courtesy + Speed_of_Service + 
    Veh_Equip_Condition + Trans_Billing_as_Expected + 
    Value_for_the_Money,
  data = clean_data
)

# Store model predictions in the data frame
clean_data$Predicted_Recom <- predict(regression_model)

cat("\n--- Generating Regression Plot (Actual vs. Predicted) ---\n")

# Visualization: Scatter plot of Actual vs. Predicted Values
print(
  ggplot(clean_data, aes(x = Predicted_Recom, y = Recom_mend_Siva)) +
    geom_point(alpha = 0.6, color = "steelblue") +
    # Add the ideal diagonal line (y=x)
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 1) +
    labs(
      title = "Regression Model Fit: Actual vs. Predicted Customer Recommendation Score",
      x = "Predicted Recommendation Score",
      y = "Actual Recommendation Score"
    ) +
    theme_minimal()
)


# ==============================================================================
## PART 3: CLUSTER VISUALIZATION 2 (PCA Scatter Plot)
# ==============================================================================

cat("\n--- Generating Alternative Cluster Plot (PCA Scatter Plot) ---\n")

# Visualization: PCA Scatter Plot of Clusters
# This plots the clusters in the 2 principal components space (PC1 vs PC2).
fviz_cluster(
  kmeans_result,
  data = scaled_data,
  palette = c("#2E9FDF", "#00AFBB", "#E7B800"), # Custom colors
  geom = "point",
  pointsize = 1.5,
  ellipse.type = "convex", # Draw a convex hull around clusters
  star.plot = TRUE,        # Connect cluster center to points
  ggtheme = theme_minimal(),
  main = paste("Cluster Visualization using Principal Components (k=", k_value, ")")
)

