library(usmap)
library(ggplot2)
library(dplyr)
library(scales)
       
state2025 <- read.csv("/Users/chintanjikkar/Desktop/PACE/Fall 25/Visual Analytics/Data/us-states---ranking-by-population-2025.csv")
state2025_small <- state2025 %>% select(state, pop2025) %>% rename(full = state)
merged_pop <- merge(usmap::statepop, state2025_small, by = "full", all.x = TRUE)

us_states_map <- usmap::us_map()
data_for_map <- left_join(us_states_map, state2025_small, by = "full")

plot_usmap(data = merged_pop, values = "pop2025") +
  scale_fill_continuous(name = "2025 Population", label = comma, low = "lightblue", high = "darkblue") +
  theme(legend.position = "right") +
  labs(title = "US State Populations (2025 Estimates)")



campaign <- read.csv("/Users/chintanjikkar/Desktop/PACE/Fall 25/Visual Analytics/Data/marketing_campaign.csv")

campaign$Age <- 2022 - campaign$Year_Birth

fig1 <- plot_ly(campaign, x = ~Age, y = ~Income, type = 'scatter', mode = 'markers', color = ~Education)
fig1

numeric_data <- campaign %>% select_if(is.numeric) %>% select(-ID)

cor_mat <- cor(numeric_data, use = "complete.obs")

plot_ly(
  x = colnames(cor_mat),
  y = rownames(cor_mat),
  z = cor_mat,
  type = "heatmap",
  colorscale = list(
    list(0, "#228B8D"),   
    list(0.5, "#F8C471"),  
    list(1, "#7D3C98")     
  ), 
  colorbar = list(title = "Correlation")
)
