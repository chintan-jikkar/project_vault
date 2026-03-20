num_cols <- c("Recom_mend_Siva",
              "Staff_Courtesy",
              "Speed_of_Service",
              "Veh_Equip_Condition",
              "Trans_Billing_as_Expected",
              "Value_for_the_Money")


siva_num <- siva |> select(any_of(num_cols))


cor_mat <- cor(siva_num, use = "pairwise.complete.obs")


cor_df <- as.data.frame(as.table(cor_mat))
names(cor_df) <- c("Var1", "Var2", "Correlation")

ggplot(cor_df, aes(Var1, Var2, fill = Correlation)) +
  geom_tile() +
  geom_text(aes(label = round(Correlation, 2)), size = 3) +
  scale_fill_gradient2(limits = c(-1, 1)) +
  labs(title = "Correlation Heatmap (SIVA numeric variables)",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

reg_df <- siva |> select(any_of(num_cols)) |> na.omit()

model <- lm(Recom_mend_Siva ~ Staff_Courtesy + Speed_of_Service +
              Veh_Equip_Condition + Trans_Billing_as_Expected +
              Value_for_the_Money,
            data = reg_df)


viz_reg <- tibble(
  actual = reg_df$Recom_mend_Siva,
  fitted = fitted(model)
)

ggplot(viz_reg, aes(fitted, actual)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Regression Visualization: Actual vs Fitted",
       x = "Fitted (from linear model)",
       y = "Actual Recom_mend_Siva") +
  theme_minimal()

clust_df <- siva |> select(any_of(setdiff(num_cols, "Recom_mend_Siva"))) |> na.omit()
X <- scale(clust_df)

set.seed(42)
km <- kmeans(X, centers = 3, nstart = 25)


pca <- prcomp(X, center = TRUE, scale. = TRUE)
pc_scores <- as_tibble(pca$x[, 1:2], .name_repair = "minimal")
pc_scores$cluster <- factor(km$cluster)

ggplot(pc_scores, aes(PC1, PC2, color = cluster)) +
  geom_point(alpha = 0.7) +
  labs(title = "K-means clusters (k=3) visualized on first two PCs",
       x = "PC1", y = "PC2") +
  theme_minimal()

