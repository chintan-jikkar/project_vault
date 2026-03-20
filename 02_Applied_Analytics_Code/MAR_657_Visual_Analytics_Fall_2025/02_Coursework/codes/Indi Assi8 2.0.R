library(usmap)
library(ggplot2)
library(dplyr)
library(scales)
library(reshape2) 
library(plotly) 

# Load & join population data
state2025 <- read.csv("/Users/chintanjikkar/Desktop/PACE/Fall 25/Visual Analytics/Data/us-states---ranking-by-population-2025.csv")
state2025_small <- state2025 %>% select(state, pop2025) %>% rename(full = state)
merged_pop <- merge(usmap::statepop, state2025_small, by = "full", all.x = TRUE)

# US map plot
plot_usmap(data = merged_pop, values = "pop2025") +
  scale_fill_continuous(name = "2025 Population", label = comma, low = "lightblue", high = "darkblue") +
  theme(legend.position = "right") +
  labs(title = "US State Populations (2025 Estimates)")

# Load marketing data
campaign <- read.csv("/Users/chintanjikkar/Desktop/PACE/Fall 25/Visual Analytics/Data/marketing_campaign.csv")
campaign$Age <- 2022 - campaign$Year_Birth

# Interactive scatter plot
fig1 <- plot_ly(campaign, x = ~Age, y = ~Income, type = 'scatter', mode = 'markers', color = ~Education)
fig1

# Static correlation matrix heatmap with custom colors and value labels
numeric_data <- campaign %>% select_if(is.numeric) %>% select(-ID)
cor_mat <- cor(numeric_data, use = "complete.obs")
cor_mat_melt <- melt(cor_mat)

# Set color gradient using same codes as your previous palette
ggplot(cor_mat_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", value)), size = 2.65, color = "black") + 
  scale_fill_gradient2(
    low = "#228B8D",   
    mid = "#F8C471",   
    high = "#7D3C98",  
    midpoint = 0,
    limit = c(-1,1),
    name = "Correlation"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = "right"
  ) +
  labs(
    title = "Correlation Matrix of Numeric Variables",
    x = NULL,
    y = NULL
  )
