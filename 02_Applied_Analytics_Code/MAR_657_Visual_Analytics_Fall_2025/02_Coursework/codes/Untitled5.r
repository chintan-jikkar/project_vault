library(ggplot2)
library(dplyr)
library(corrplot)
library(factoextra)
library(readr)
library(RColorBrewer)

siva <- read.csv("/Users/chintanjikkar/Desktop/PACE/Fall 25/Visual Analytics/Data/siva.csv")

str(siva)

num_vars <- siva %>% select(where(is.numeric))
num_vars_ready <- na.omit(num_vars)
siva_clean <- siva[as.numeric(rownames(num_vars_ready)), ]

corr_mat <- cor(num_vars, use = "complete.obs")
corrplot(corr_mat, 
         method = "color",
         col = colorRampPalette(brewer.pal(11, "Spectral"))(200),
         addCoef.col = "black",
         tl.cex = 0.7,
         number.cex = 0.8,
         diag = FALSE)

model <- lm(Recom_mend_Siva ~ Staff_Courtesy + Speed_of_Service + Veh_Equip_Condition, data=siva_clean)
summary(model)

plot(siva_clean$Recom_mend_Siva, fitted(model), xlab="Actual RecommendSiva", ylab="Predicted RecommendSiva", main="Regression: Actual vs Predicted")
abline(0,1,col="red",lty=2)

##XOXOX similar to above one BUT USE THIS ONES OUTPUT####
ggplot(siva_clean, aes(x=Recom_mend_Siva, y=model$fitted.values)) +
  geom_point() +
  geom_abline(slope=1, intercept=0, color="red", linetype="dashed") +
  labs(title="Regression: Actual vs Predicted RecommendSiva")

num_vars_scaled <- scale(num_vars_ready)
set.seed(123)
km <- kmeans(num_vars_scaled, centers=3, nstart=25)
siva_clean$cluster <- factor(km$cluster)
fviz_cluster(km, data=num_vars_scaled, geom="point",
             ellipse.type="norm", ggtheme=theme_minimal(),
             main="K-means Cluster Visualization")


