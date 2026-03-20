library(ggplot2)
library(dplyr)
library(corrplot)
library(cluster) 
library(factoextra)
library(RColorBrewer)

siva <- read.csv("/Users/chintanjikkar/Desktop/PACE/Fall 25/Visual Analytics/Data/siva.csv", na = c("NA", ""))
str(siva)



numerical_vars <- siva %>%
  select(
    Recom_mend_Siva,
    Staff_Courtesy,
    Speed_of_Service,
    Veh_Equip_Condition,
    Trans_Billing_as_Expected,
    Value_for_the_Money,
    Total_charge_USD,
    Survey_checkout_diff
  ) %>%
  mutate(across(everything(), as.numeric))

siva_clean <- na.omit(numerical_vars)



correlation_matrix <- cor(siva_clean)
corr_colors <- colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))(200)

corrplot(
  correlation_matrix,
  method = "color",
  col = corr_colors,
  addCoef.col = "black",
  tl.cex = 0.7,
  number.cex = 0.8,
  diag = FALSE,
  title = "Correlation Matrix of Customer Survey Variables",
  mar = c(0, 0, 2, 0)
)



regression_model <- lm(
  Recom_mend_Siva ~ Staff_Courtesy + Speed_of_Service + 
    Veh_Equip_Condition + Trans_Billing_as_Expected + 
    Value_for_the_Money,
  data = siva_clean
)

summary(regression_model)

siva_clean$Predicted_Recom <- predict(regression_model)

ggplot(siva_clean, aes(x = Predicted_Recom, y = Recom_mend_Siva)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 1) +
  labs(
    title = "Regression Model Fit: Actual vs. Predicted Customer Recommendation Score",
    x = "Predicted Recommendation Score",
    y = "Actual Recommendation Score"
  ) +
  theme_minimal()


set.seed(2101)
cluster_vars <- siva_clean %>%
  dplyr::select(
    Staff_Courtesy,
    Speed_of_Service,
    Veh_Equip_Condition,
    Trans_Billing_as_Expected,
    Value_for_the_Money,
    Total_charge_USD,
    Survey_checkout_diff
  )
siva_clean_scaled <- scale(cluster_vars)
siva_df <- as.data.frame(siva_clean_scaled)
k_value <- 3
siva_final <- kmeans(siva_clean_scaled, k_value, nstart=25)
fviz_cluster(
  siva_final,
  data = scaled_data,
  palette = c("#FF6F61", "#FFE082", "#4DB6AC"), 
  geom = "point",
  pointsize = 1.5,
  ellipse.type = "convex", 
  star.plot = TRUE,        
  ggtheme = theme_minimal(),
  main = paste("Cluster Visualization using Principal Components (k =", k_value, ")")
)

