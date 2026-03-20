
##title: "Presenting results from HT and DA"
##author: "Emily Ko"
##output: html_document

######### Package: ggplot2 ########################################
## Load ggplot2
library(ggplot2)

## Package: ggplot2 – Set a background
ggplot(data = mpg, aes(x = displ, y = hwy))

## Package: ggplot2 – Add a graph
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()

## Package: Define a setting (axis range, color, label)
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point() + xlim(3, 6)

ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  xlim(3,  6) +
  ylim(10, 30)

# In-class exercise: scatter plot
ggplot(data = mpg, aes(x = cty, y = hwy)) +
  geom_point() +
  xlim(0,  40) +
  ylim(0, 50)
 
## Package:ggplot2 - Average bar chart
library(dplyr)

df_mpg <- mpg %>%
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy))

df_mpg

# Creat a bar chart for the average
ggplot(data = df_mpg, aes(x = drv, y = mean_hwy)) + geom_col()

# Sort by size
ggplot(data = df_mpg, aes(x = reorder(drv, -mean_hwy), y = mean_hwy)) + 
  geom_col()


## Package: ggplot2 – Frequency bar chart
ggplot(data = mpg, aes(x = drv)) + geom_bar()
ggplot(data = mpg, aes(x = hwy)) + geom_bar()

# In-class exercise: bar charts
# Creat a bar chart for the average
library(dplyr)

df_cty <- mpg %>%
  group_by(class) %>%
  summarise(mean_cty = mean(cty))

ggplot(data = df_cty, aes(x = class, y = mean_cty)) + geom_col()

# Sort by size
ggplot(data = df_cty, aes(x = reorder(class, -mean_cty), y = mean_cty)) + 
  geom_col()

## Package: ggplot2 – Line graphs
data("economics")
ggplot(data = economics, aes(x = date, y = unemploy)) + geom_line()

# In-class exercise: line graphs
ggplot(data = economics, aes(x = date, y = pop)) + geom_line()

######### Hypothesis testing ###########################################

## T-test for city fuel economy (cty) of 'compact' and 'suv' class
mpg <- as.data.frame(ggplot2::mpg)

# Preparing data
library(dplyr)
mpg_diff <- mpg %>%
  select(class, cty) %>%
  filter(class %in% c("compact", "suv"))

head(mpg_diff)  

# t-test
t.test(data = mpg_diff, cty ~ class, var.equal = T)

## T-test for city fuel economy (cty) of ‘regular gasoline and ‘premium gasoline’
# Preparing data
mpg_diff2 <- mpg %>%
  select(fl, cty) %>%
  filter(fl %in% c("r", "p")) #r:regular, p:premium

table(mpg_diff2$fl)

#t-test
t.test(data = mpg_diff2, cty ~ fl, var.equal = T)

## Correlation between the number of unemployment and dprivate consumption expenditure
economics <- as.data.frame(ggplot2::economics)
cor.test(economics$unemploy, economics$pce)

## Create a heatmap of a correlation matrix
head(mtcars)

car_cor <- cor(mtcars)
round(car_cor,2)

## Create a correlation heatmap
install.packages("corrplot")
library(corrplot)

corrplot(car_cor)

corrplot(car_cor, method = "number")

## Set corrplot parameters
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA" ))

corrplot(car_cor,
         method = "color",
         col = col(200),
         type = "lower",
         order = "hclust",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         diag = F)


corrplot(car_cor,
         method = "color",
         col = col(50), #Number decides the variation of color saturation
         type = "lower",
         order = "hclust",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         diag = T)

corrplot(car_cor,
         method = "color",
         col = COL1('YlOrBr'),
         type = "lower",
         order = "hclust",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         diag = F)


corrplot(car_cor,
         method = "color",
         type = "lower",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 50,
         diag = F)

## In-class exercise: correlation plot

fitness <- read.csv(file = "/Users/chintanjikkar/Desktop/PACE/Fall 25/Visual Analytics/Data/Fitness.csv")

#fitness_cor <- fitness %>%
#  select(Age,Weight,Oxy,Runtime,RunPulse, RstPulse,MaxPulse)


fitness_sub <- fitness[,c(3:9)]
 
fitness_cor <- cor(fitness_sub)
corrplot(fitness_cor)

######### Data analysis and visualization #####################################
### Descriptive analysis ##############
## Load packages
library(foreign)
library(dplyr)
library(ggplot2)
install.packages("readxl")
library(readxl)
install.packages("haven")
library(haven)
install.packages("sjlabelled")
library(sjlabelled)
install.packages("memisc")
library(memisc)

## Read data
#   raw_welfare <- read.spss("/Users/chintanjikkar/Desktop/PACE/Fall 25/Visual Analytics/Data/Koweps_hpc10_2015_beta1.sav", to.data.frame = T)
raw_welfare <- sjlabelled::read_spss("/Users/chintanjikkar/Desktop/PACE/Fall 25/Visual Analytics/Data/Koweps_hpc10_2015_beta1.sav")

welfare <- raw_welfare

## Create a subset
var <- c("h10_g3", "h10_g4",
         "h10_g10", "h10_g11", 
         "p1002_8aq1", "h10_eco9", 
         "h10_reg7")
welfare_sub <- welfare[var]

## Rename variables
names(welfare_sub)[names(welfare_sub) == "h10_g3"] <- "sex"
names(welfare_sub)[names(welfare_sub) == "h10_g4"] <- "birth"
names(welfare_sub)[names(welfare_sub) == "h10_g10"] <- "marriage"
names(welfare_sub)[names(welfare_sub) == "h10_g11"] <- "religion"
names(welfare_sub)[names(welfare_sub) == "p1002_8aq1"] <- "income"
names(welfare_sub)[names(welfare_sub) == "h10_eco9"] <- "code_job"
names(welfare_sub)[names(welfare_sub) == "h10_reg7"] <- "code_religion"

## Check variables (religion) in welfare
class(welfare_sub$religion)

table(welfare_sub$religion)

## Preprocessing – label if a religion exists or not
welfare_sub$religion <- ifelse(welfare_sub$religion == 1, "yes", "no")
table(welfare_sub$religion)

class(welfare_sub$marriage)
table(welfare_sub$marriage)

## Preprocessing – create a derived variable “group marriage” to add a “divorce” case
welfare_sub$group_marriage <- ifelse(welfare_sub$marriage == 1, "marriage",
                              ifelse(welfare_sub$marriage == 3, "divorce", NA))

table(welfare_sub$group_marriage)

table(is.na(welfare_sub$group_marriage))

## Check divorce and marriage using qplot
qplot(welfare_sub$group_marriage)

## Create a table of divorce rates based on religious status
library(dplyr)

religion_marriage <- welfare_sub %>%
  filter(!is.na(group_marriage)) %>%
  group_by(religion, group_marriage) %>%
  summarise(n = n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(pct = round(n/tot_group*100,1))

religion_marriage

## Use count() in dplyr package
religion_marriage <- welfare_sub %>%
  filter(!is.na(group_marriage)) %>%
  count(religion, group_marriage) %>%
  group_by(religion) %>%
  mutate(pct = round(n/sum(n)*100, 1))

religion_marriage

## Create a divorce rate table
divorce <- religion_marriage %>%
  dplyr::filter(group_marriage == "divorce") %>%
  dplyr::select(,religion, pct)

divorce

# Create a graph
ggplot(data = divorce, aes(x = religion, y = pct)) + geom_col()

### Regression models #################################
# Load the package
install.packages("datarium")
data("marketing", package = "datarium")
head(marketing, 8)

# Scatter plot between youtube and sales (descriptive)
ggplot(marketing, aes(x = youtube, y = sales)) +
  geom_point()

# Add smooth line
ggplot(marketing, aes(x = youtube, y = sales)) +
  geom_point() +
  stat_smooth()

# Regression models with marketing data
marketing_model <- lm(sales ~ youtube + facebook + newspaper, data = marketing)
summary(marketing_model)

# Predicted line with y = sales: simple regression
ggplot(marketing, aes(x=youtube, y=sales)) + 
  geom_point()+
  geom_smooth(method="lm")

ggplot(marketing, aes(x=facebook, y=sales)) + 
  geom_point()+
  geom_smooth(method="lm")

ggplot(marketing, aes(x=newspaper, y=sales)) + 
  geom_point()+
  geom_smooth(method="lm")

# Predicted line with y = sales: multiple regression 
library(ggplot2)
install.packages("jtools")
library(jtools)

# Predicted line for x = youtube
effect_plot(marketing_model, pred = youtube)

# Predicted line for x = youtube (confidence interval)
effect_plot(marketing_model, pred = youtube, interval = TRUE)

# Predicted line for x = youtube (confidence interval & data distributions)
effect_plot(marketing_model, pred = youtube, interval = TRUE, rug = TRUE)

# Predicted line for x = youtube (confidence interval & data distributions & actual data points)
effect_plot(marketing_model, pred = youtube, interval = TRUE, plot.points = TRUE)

# Predicted line for x = facebook
effect_plot(marketing_model, pred = facebook)

# Predicted line for x = facebook (confidence interval)
effect_plot(marketing_model, pred = facebook, interval = TRUE)

# Predicted line for x = facebook (confidence interval & data distributions)
effect_plot(marketing_model, pred = facebook, interval = TRUE, rug = TRUE)

# Predicted line for x = facebook (confidence interval & data distributions & actual data points)
effect_plot(marketing_model, pred = facebook, interval = TRUE, plot.points = TRUE)

# Predicted line for x = newspaper
effect_plot(marketing_model, pred = newspaper)

# Predicted line for x = newspaper (confidence interval)
effect_plot(marketing_model, pred = newspaper, interval = TRUE)

# Predicted line for x = newspaper (confidence interval & data distributions)
effect_plot(marketing_model, pred = newspaper, interval = TRUE, rug = TRUE)

# Predicted line for x = newspaper (confidence interval & data distributions & actual data points)
effect_plot(marketing_model, pred = newspaper, interval = TRUE, plot.points = TRUE)

# Beta coefficients representation
library(jtools)
library(broom.mixed)
plot_summs(marketing_model,
           coefs = c("Youtube" = "youtube", "Facebook" = "facebook",
                     "Newspaper" = "newspaper"),
           robust = TRUE, inner_ci_level = .1)

plot_summs(marketing_model,
           coefs = c("Youtube" = "youtube", "Facebook" = "facebook",
                     "Newspaper" = "newspaper"),
           robust = F, inner_ci_level = .8)

# Beta coefficients representation with distributions
plot_summs(marketing_model,
           coefs = c("Youtube" = "youtube", "Facebook" = "facebook",
                     "Newspaper" = "newspaper"),
           robust = TRUE, plot.distributions = TRUE, inner_ci_level = .8)

## In-class exercise: regression models
fitness <- read.csv(file = "/Users/chintanjikkar/Desktop/PACE/Fall 25/Visual Analytics/Data/Fitness.csv", header = T)
str(fitness)

lmFitness <- lm(RunPulse ~ Age + Weight + Oxy + Runtime + RstPulse + MaxPulse, data = fitness)
summary(lmFitness)

ggplot(fitness, aes(x=Age, y=RunPulse)) + 
  geom_point()+
  geom_smooth(method = "lm")

ggplot(fitness, aes(x=Weight, y=Runtime)) + 
  geom_point()+
  geom_smooth(method = "lm") #lm doesn't work

plot_summs(lmFitness,
           robust = TRUE, inner_ci_level = .8)

### Cluster analysis ########################################
data(iris)
iris2 <- iris
iris2$Species <- NULL #remove Species column
head(iris2)

(kmeans.result <- kmeans(iris2, 3))

# Plot clustering results 1
plot(iris2[c("Sepal.Length", "Sepal.Width")], col = kmeans.result$cluster)
points(kmeans.result$centers[,c("Sepal.Length", "Sepal.Width")], 
       col = 1:3, pch = 8, cex = 2) #col: change the color of the cnetroid mark/ pch: mark type/ cex: mark size

# Ploting clustering results 2
install.packages("ggpubr")
install.packages("factoextra")
library(ggpubr)
library(factoextra)
 
fviz_cluster(kmeans.result, data = iris2,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

## In-class exercise: cluster analysis
pizza <- read.csv(file = "/Users/chintanjikkar/Desktop/PACE/Fall 25/Visual Analytics/Data/pizza_relative_importance.csv", header = T)

(kmeans.result1 <- kmeans(pizza[,-1], 3))
(kmeans.result2 <- kmeans(pizza[,-1], 4))
(kmeans.result3 <- kmeans(pizza[,-1], 5))

pizza_sub <- pizza[,-1]

# Plot clustering results 1
plot(pizza_sub[c("crust", "topping")], col = kmeans.result1$cluster)
points(kmeans.result1$centers[,c("crust", "topping")], col = 1:3, pch = 8, cex = 2)

fviz_cluster(kmeans.result1, data = pizza[,-1],
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

fviz_cluster(kmeans.result2, data = pizza[,-1],
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "red"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

fviz_cluster(kmeans.result3, data = pizza[,-1],
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "red", "purple"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

