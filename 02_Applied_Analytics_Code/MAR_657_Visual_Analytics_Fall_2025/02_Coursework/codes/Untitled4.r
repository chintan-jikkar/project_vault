# ==============================================================================
# MAR657: Visual Analytics - Individual Assignment 3 EDA Script
# This script performs the 8 required types of Exploratory Data Analysis (EDA)
# on the 'siva.csv' dataset.
#
# Libraries included: ggplot2, dplyr, corrplot, foreign, readxl, haven,
# sjlabelled, memisc, jtools, broom.mixed, ggpubr, factoextra
# ==============================================================================

# 1. Setup: Load Libraries
# Only dplyr, ggplot2, and tidyr are strictly necessary for the 8 required EDAs,
# but all requested libraries are listed for completeness.
# You may need to install these packages if you haven't already:
# install.packages(c("dplyr", "ggplot2", "tidyr", "corrplot", "foreign",
#                    "readxl", "haven", "sjlabelled", "memisc", "jtools",
#                    "broom.mixed", "ggpubr", "factoextra"))

library(dplyr)
library(ggplot2)
library(tidyr)
library(corrplot)    # For future use, e.g., complex quantitative multivariate analysis
library(foreign)     # For reading specific file formats
library(readxl)      # For reading specific file formats
library(haven)       # For reading specific file formats
library(sjlabelled)  # For working with labels
library(memisc)      # For data documentation
library(jtools)      # For visualization and presentation of regression
library(broom.mixed) # For tidying mixed models
library(ggpubr)      # For easily arranging and publishing ggplots
library(factoextra)  # For clustering and PCA visualization

# 2. Data Loading
# Load the 'siva.csv' file. Ensure the file is in your working directory,
# or provide the full file path.
data <- read.csv("/Users/chintanjikkar/Desktop/PACE/Fall 25/Visual Analytics/Data/siva.csv", stringsAsFactors = FALSE)

# 3. Data Cleaning and Preparation
# Select and clean the variables we will analyze:
# - Purpose_of_Rental (Categorical)
# - Survey_Type (Categorical)
# - Total_charge_USD (Quantitative)
# - Recom_mend_Siva (Quantitative - we will treat the score numerically)

df_clean <- data %>%
  # Select key columns and rename for simplicity
  select(Purpose_of_Rental, Survey_Type, Total_charge_USD, Recom_mend_Siva) %>%
  # Remove rows where selected variables have missing values (NA)
  drop_na() %>%
  # Ensure Total_charge_USD is numeric (it might load as character if there are symbols)
  mutate(Total_charge_USD = as.numeric(Total_charge_USD)) %>%
  # Filter out rows with non-finite or 0 charges for a meaningful analysis
  filter(is.finite(Total_charge_USD) & Total_charge_USD > 0)


# ==============================================================================
# PART 1: NON-GRAPHICAL EDA
# ==============================================================================

# 1. Univariate Non-Graphical (Categorical: Purpose_of_Rental)
# Action: Frequency and Proportion Table
cat_table <- df_clean %>%
  count(Purpose_of_Rental, sort = TRUE) %>%
  mutate(Proportion = n / sum(n))

print("1. Univariate Non-Graphical (Categorical): Purpose of Rental")
print(cat_table)
# Finding: You can observe the primary rental purpose (Business vs. Leisure/Personal)
# and see which category dominates the customer base.
# ------------------------------------------------------------------------------

# 2. Univariate Non-Graphical (Quantitative: Total_charge_USD)
# Action: Summary Statistics (Min, Max, Mean, Median, Quartiles)
quant_summary <- df_clean %>%
  summarise(
    N = n(),
    Mean_Charge = mean(Total_charge_USD),
    Median_Charge = median(Total_charge_USD),
    Min_Charge = min(Total_charge_USD),
    Max_Charge = max(Total_charge_USD),
    SD_Charge = sd(Total_charge_USD)
  )

print("2. Univariate Non-Graphical (Quantitative): Total Charge (USD)")
print(quant_summary)
# Finding: The mean and median charges will show if the distribution is skewed.
# The large difference between Max and 75th percentile (not shown here, but
# visible with summary(df_clean$Total_charge_USD)) indicates potential outliers.
# ------------------------------------------------------------------------------

# 3. Multivariate Non-Graphical (Categorical vs. Categorical: Purpose vs. Survey_Type)
# Action: Cross-Tabulation (Contingency Table)
cross_tab <- table(df_clean$Purpose_of_Rental, df_clean$Survey_Type)
print("3. Multivariate Non-Graphical (Categorical vs. Categorical): Cross-Tab")
print(cross_tab)
# Finding: This table shows how many customers in each rental purpose category (rows)
# were contacted via each survey type (columns). This helps identify if certain
# survey channels are biased toward business or leisure customers.
# ------------------------------------------------------------------------------

# 4. Multivariate Non-Graphical (Quantitative vs. Quantitative: Charge vs. Recommendation)
# Action: Correlation Coefficient
# Note: Recom_mend_Siva is a score, so correlation is appropriate.
correlation <- cor(df_clean$Total_charge_USD, df_clean$Recom_mend_Siva)
print("4. Multivariate Non-Graphical (Quantitative vs. Quantitative): Correlation")
cat("Correlation between Total Charge and Recommendation Score:", correlation, "\n")
# Finding: A correlation close to +1 or -1 indicates a strong relationship.
# A finding might be that higher total charges lead to slightly lower
# recommendation scores (negative correlation), suggesting dissatisfaction with cost.
# ------------------------------------------------------------------------------


# ==============================================================================
# PART 2: GRAPHICAL EDA
# ==============================================================================

# 5. Univariate Graphical (Categorical: Purpose_of_Rental)
# Action: Bar Chart
plot_cat_univar <- ggplot(df_clean, aes(x = Purpose_of_Rental, fill = Purpose_of_Rental)) +
  geom_bar() +
  labs(
    title = "5. Distribution of Rental Purpose",
    x = "Purpose of Rental",
    y = "Count"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

print(plot_cat_univar)
# Finding: Visually confirms the dominant segment (e.g., Business vs. Leisure),
# making it easy to see where marketing resources should be focused.
# ------------------------------------------------------------------------------

# 6. Univariate Graphical (Quantitative: Total_charge_USD)
# Action: Histogram
plot_quant_univar <- ggplot(df_clean, aes(x = Total_charge_USD)) +
  geom_histogram(binwidth = 50, fill = "#3399FF", color = "white") +
  labs(
    title = "6. Distribution of Total Charge (USD)",
    x = "Total Charge (USD)",
    y = "Frequency"
  ) +
  # Limit x-axis to focus on the bulk of the data (e.g., charges under $1000)
  xlim(0, 1000) +
  theme_minimal()

print(plot_quant_univar)
# Finding: Shows the shape of the data. Often, charges are right-skewed,
# meaning most transactions are low-value, with a few high-value outliers.
# ------------------------------------------------------------------------------

# 7. Multivariate Graphical (Categorical vs. Quantitative: Purpose vs. Total_charge_USD)
# Action: Box Plot
plot_multi_cat_quant <- ggplot(df_clean, aes(x = Purpose_of_Rental, y = Total_charge_USD, fill = Purpose_of_Rental)) +
  # Set y-axis limit to handle extreme outliers for better visualization
  geom_boxplot(outlier.shape = 8) +
  ylim(0, 1000) +
  labs(
    title = "7. Total Charge Distribution by Rental Purpose (Box Plot)",
    x = "Purpose of Rental",
    y = "Total Charge (USD)"
  ) +
  theme_minimal()

print(plot_multi_cat_quant)
# Finding: Compares the median charge and spread (IQR) between the categories.
# You might find that Business rentals have a higher median charge and/or
# a larger spread than Leisure rentals.
# ------------------------------------------------------------------------------

# 8. Multivariate Graphical (Quantitative vs. Quantitative: Charge vs. Recommendation)
# Action: Scatter Plot
plot_multi_quant_quant <- ggplot(df_clean, aes(x = Total_charge_USD, y = Recom_mend_Siva)) +
  geom_point(alpha = 0.5, color = "#FF6600") + # Use alpha for transparency with many points
  geom_smooth(method = "lm", color = "black", se = FALSE) + # Add linear trend line
  xlim(0, 1000) +
  labs(
    title = "8. Relationship between Total Charge and Recommendation Score (Scatter Plot)",
    x = "Total Charge (USD)",
    y = "Recommendation Score"
  ) +
  theme_minimal()

print(plot_multi_quant_quant)
# Finding: Visually confirms the relationship (or lack thereof) seen in the
# correlation coefficient (EDA #4). If the line slopes down, the higher the charge,
# the lower the recommendation.
# ------------------------------------------------------------------------------
