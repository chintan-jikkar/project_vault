#---
#title: "Data manipulation, filtering, and Visualizations"
#author: "Emily Ko"
#output: html_document

#---
##############################################################################
########### Data manipulation, filtering, and visualization ##################
##############################################################################

######################## 6 common functions ##################################
install.packages("dplyr")
## Preparing data
newProduct <- read.csv(file='/Users/chintanjikkar/Desktop/PACE/Fall 25/Visual Analytics/new_product.csv', header=T)

## 'head()' function
head(newProduct)
head(newProduct,10)

## 'tail()' function
tail(newProduct)
tail(newProduct,10)

## 'view()' function
View(newProduct)

## 'dim()' function
dim(newProduct)

## 'str()' function
str(newProduct)

## 'summary()' function
summary(newProduct)

######################## ifelse statement ##################################
## Create a data frame with two variables
df_raw  <-  data.frame(var1  = c(1, 2, 1),
                       var2  =  c(2, 3, 2))

df_raw 

## Create a copy of data frame
df_new <- df_raw
df_new

## Modify a column name
library(dplyr)
df_new <- rename(df_new, v2 = var2)
df_new

## Comparison before and after modified the second column name using rename()
df_raw
df_new

## Create derived variables
df  <-  data.frame(var1  = c(4, 3, 8),
                   var2  =  c(2, 6, 1))

df

## Create derived variables:Sum
df$var_sum <- df$var1 + df$var2

df

## Create derived variables:Mean
df$var_mean <- (df$var1 + df$var2)/2

df

## Create derived variables using conditional statements
mpg <- as.data.frame(ggplot2::mpg) #install ggplot2 if you don't have it in your R

View(mpg)
str(mpg)

mpg$total <- (mpg$cty + mpg$hwy)/2
summary(mpg$total)

hist(mpg$total)

## Create derived variables using conditional statements
# If greater than 20, pass, other fail
mpg$test <- ifelse(mpg$total >= 20, "pass", "fail")

View(mpg)

## View the number of vehicles that have been passed fuel efficiency by the frequency chart
table(mpg$test) #Create a contingency table of the counts at the feature

## Plot a frequency bar graph
library(ggplot2)
qplot(mpg$test)

## Create a variable using nested conditional statements
mpg$grade <- ifelse(mpg$total >= 30, "A",
                     ifelse(mpg$total >=20, "B","C"))
                          
head(mpg,20)

View(mpg)
table(mpg$grade)
qplot(mpg$grade)

## Create a many rating as you want
# Assign four rating (A, B, C, D) to grade
mpg$grade2 <- ifelse(mpg$total >= 30, "A",
                     ifelse(mpg$total >=25, "B",
                            ifelse(mpg$total >=20, "C","D")))

View(mpg)

## Visualization 1 with categorical variables: Create a frequency chart and bar graph
table(mpg$grade2)
qplot(mpg$grade2)

############### dplyr: processing data in a desired form #####################
library(dplyr)
## Preparing data: exam
exam <- read.csv(file='/Users/chintanjikkar/Desktop/PACE/Fall 25/Visual Analytics/csv_exam.csv', header=T)
exam

# Extract and print only if class is 1 in exam
exam %>% filter(class == 1)

# Extract and print only if class is 2 in exam
exam %>% filter(class == 2)

# Extract and print only if class is NOT 1 in exam
exam %>% filter(class != 1)

# Extract and print only if math is greater than 50 in exam
exam %>% filter(math > 50)

# Extract and print only if math is less than 50 in exam
exam %>% filter(math < 50)

# Extract and print only if english is greater than or equal to 80 in exam
exam %>% filter(english >= 80)

# Extract rows that meet multiple conditions
exam %>% filter(class == 1 & math >= 50)

# Extract rows that meet multiple conditions
exam %>% filter(class == 2 & math >= 80)

# Extracting rows that meet one or more of the different conditions
exam %>% filter(math >= 90 | english >=90)

# Extracting rows that meet one or more of the different conditions
exam %>% filter(english < 90 | science < 90)

# Extracting rows that correspond to a column value
exam %>% filter(class == 1 | class == 3 | class == 5)

# Operator %in%: identify if an element belongs to a vector
exam %>% filter(class %in% c(1,3,5))

# Create data from extracted rows
class1 <- exam %>% filter(class == 1)
class2 <- exam %>% filter(class == 2)

mean(class1$math)
mean(class2$math)

## select()
exam %>% select(math)
exam %>% select(english)

# Select multiple variables
exam %>% select(class, math, english)

# Exclude a specific variable
exam %>% select(-math)
exam %>% select(-math, -english)

# Combine the dplyr functions
exam %>% filter(class == 1) %>% select (english)

# Print a part
exam %>% 
  select(id, math) %>% 
  head

exam %>% 
  select(id, math) %>% 
  head(10)

## arrange()
# Sort in ascending order

exam %>% arrange(math)
    ## FOR descend add (desc(math))
# Specify multiple variables for sorting
exam %>% arrange(class,math)

## mutate()
# Add a derived variable
exam %>% 
  mutate(total = math + english + science) %>% 
  head

# Add multiple derived variables
exam %>% 
  mutate(total = math + english + science,
         mean = (math + english + science)/3) %>% 
  head

## I WROTE THIS 
exam %>% 
  mutate(total = math + english + science,
         mean = ((math + english + science)/3)) %>% 
  head


# Apply ifelse() to the function mutate()
exam %>% 
  mutate(test = ifelse(science >=60, "pass", "fail")) %>% 
  head

# Apply arrange() to the derived variable
exam %>% 
  mutate(total = math + english + science) %>% 
  arrange(total) %>% 
  head

## summarise()
## group_by()
# To summarize by a variable
exam %>% summarise(mean_math = mean(math))

# To summarize by group
exam %>% 
  group_by(class) %>% 
  summarise(mean_math = mean(math))

# To summarize multiple summary statistics at once
exam %>% 
  group_by(class) %>% 
  summarise(mean_math = mean(math),
            sum_math = sum(math),
            median_math = median(math),
            n = n())

# To group again by each group
mpg %>% 
  group_by(manufacturer, drv) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  head(10)

# Combine dplyr functions
mpg %>% 
  group_by(manufacturer) %>% 
  filter(class == "suv") %>% 
  mutate(tot = (cty + hwy)/2) %>% 
  summarise(mean_tot = mean(tot)) %>% 
  arrange(desc(mean_tot)) %>% 
  head(5)

## Left_join
# Merge by column
# Create two data
test1 <- data.frame(id = c(1,2,3,4,5),
                    midterm = c(60,80,70,90,85))

test2 <- data.frame(id = c(1,2,3,4,5),
                    final = c(70,83,65,95,80))

test1
test2

# Merge by ID
total <- left_join(test1, test2, by = "id")
total

# To add variables using different data: using 'exam' data
name <- data.frame(class = c(1,2,3,4,5),
                   teacher = c("kim", "lee", "park", "choi", "jung"))

# Merge by class
exam_new <- left_join(exam, name, by = "class")
exam_new

# Merge by row
group_a <- data.frame(id = c(1,2,3,4,5),
                      test = c(60,80,70,90,85))

group_b <- data.frame(id = c(6,7,8,9,10),
                      test = c(70,83,65,95,80))

group_a
group_b

group_all <- bind_rows(group_a, group_b)
group_all

######################## Bar plot ######################################
library(ggplot2)
# Basic barplot
ggplot(data=mpg, aes(x=class)) +
  geom_bar(stat="count")

ggplot(data=mpg, aes(x=class)) +
  geom_bar() #this works too (default)

ggplot(data=mpg, aes(x=cyl)) +
  geom_bar(stat="count")

# Basic barplot: change colors
ggplot(data=mpg, aes(x=class)) +
  geom_bar(fill="blue")

ggplot(data=mpg, aes(x=cyl)) +
  geom_bar(fill="blue")

# Horizontal bar plot
ggplot(data=mpg, aes(x=cyl)) +
  geom_bar(fill="blue") + 
  coord_flip()

# Change colors with different line color
ggplot(data=mpg, aes(x=class)) +
  geom_bar(color="purple", fill="white")

# Change the width of bars
ggplot(data=mpg, aes(x=class)) +
  geom_bar(width=0.5)

ggplot(data=mpg, aes(x=cyl)) +
  geom_bar(width=0.5)

# Minimal theme + blue fill color
ggplot(data=mpg, aes(x=class)) +
  geom_bar(stat="count", fill="steelblue")+
  theme_minimal()

# Bar plots with labels
# Outside bars
ggplot(data=mpg, aes(x=class)) +
  geom_bar(fill="steelblue")+
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1)+
  theme_minimal()

# Inside bars
ggplot(data=mpg, aes(x=class)) +
  geom_bar(fill="steelblue")+
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = 1.6, color="white")+
  theme_minimal()

# Change fill colors by groups
ggplot(data=mpg, aes(x=class, fill =class)) +
  geom_bar()+
  theme_minimal()
 
# Use custom color palettes
p <- ggplot(data=mpg, aes(x=class, fill =class)) +
  geom_bar(stat="count")+
  theme_minimal()

p+scale_fill_manual(values=c("grey", "yellow", "darkgreen", "darkblue", "darkred", "black", "hotpink"))

# Use brewer color palettes
p+scale_fill_brewer(palette="Dark2")

# Use grey scale
p + scale_fill_grey()

# Stacked bars
# Stacked barplot with multiple groups
ggplot(data=mpg, aes(x=class, fill=drv)) +
  geom_bar()

# Use position=position_dodge()
ggplot(data=mpg, aes(x=class, fill=drv)) +
  geom_bar(position=position_dodge())

# Change bar fill colors to blues
ggplot(data=mpg, aes(x=class, fill=drv)) +
  geom_bar() +
  theme_minimal() +
  scale_fill_brewer(palette="Blues")

# Top legend
p + theme(legend.position="top")

# Bottom legend
p + theme(legend.position="bottom")

# Add the title
p + labs(title="Frequency of Class by Drv", 
         x="Class", y = "Count")

################### Density plot ###############################
## Basic density plot
ggplot(mpg, aes(x = displ)) +
  geom_density()

## Density plot by grade using default colors
ggplot(mpg, aes(x = displ, colour = grade)) +
  geom_density()

# Assign colors and change the width and line types of the 
#density plot
cols <- c("#F76D5E", "#FFFFBF", "#72D8FF")

ggplot(mpg, aes(x = displ, colour = grade)) +
  geom_density(lwd = 1.2, linetype = 1) + 
  scale_color_manual(values = cols)

# Fill in the density plot
ggplot(mpg, aes(x = displ, colour = grade, fill = grade)) +
  geom_density()

## Assign new colors and adjust transparency
cols <- c("#F76D5E", "#FFFFBF", "#72D8FF")

ggplot(mpg, aes(x = displ, fill = grade)) +
  geom_density(alpha = 0.7) + 
  scale_fill_manual(values = cols)

# Remove the line of the density plot
ggplot(mpg, aes(x = displ, fill = grade)) +
  geom_density(alpha = 0.8, color = NA) + 
  scale_fill_manual(values = cols)

# Customize legend
ggplot(mpg, aes(x = displ, fill = grade)) +
  geom_density(alpha = 0.8) + 
  scale_fill_manual(values = cols) + 
  guides(fill = guide_legend(title = "Grade"))

# Customer legend labels
ggplot(mpg, aes(x = displ, fill = grade)) +
  geom_density() + 
  guides(fill = guide_legend(title = "Grade")) +
  scale_fill_hue(labels = c("G1", "G2", "G3"))


########################### Box plot #########################
p <- ggplot(mpg, aes(x=class, y=hwy)) + 
  geom_boxplot()
p

# Rotate box plot
p + coord_flip()

# Notched box plot
ggplot(mpg, aes(x=class, y=hwy)) + 
  geom_boxplot(notch=TRUE)

# Change outlier, color, shape and size
ggplot(mpg, aes(x=class, y=hwy)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)

# Change outlier, color, shape and size with notched box plot
ggplot(mpg, aes(x=class, y=hwy)) + 
  geom_boxplot(notch = T, outlier.colour="red", outlier.shape=8,
               outlier.size=4)

# Add titles (General title, x-axis and y-axis)
ggplot(mpg, aes(x=class, y=hwy)) + 
  geom_boxplot(notch = T, outlier.colour="red", outlier.shape=8, outlier.size=4) + 
  labs(title="Plot of speed per car class",
       x="Types of Car", y = "Highway Speed (mph)") 

# Add titles (General title, x-axis and y-axis) with minimal background
ggplot(mpg, aes(x=class, y=hwy)) + 
  geom_boxplot(notch = T, outlier.colour="red", outlier.shape=8, outlier.size=4) + 
  labs(title="Plot of speed per car class",
       x="Types of Car", y = "Highway Speed (mph)") +
  theme_minimal()  

# With classic background
ggplot(mpg, aes(x=class, y=hwy)) + 
  geom_boxplot(notch = T, outlier.colour="red", outlier.shape=8,
               outlier.size=4) + 
  labs(title="Plot of speed per car class",x="Types of Car", y = "Highway Speed (mph)")+
  theme_classic()  

## Box plot with dots
library(ggplot2)
ggplot(mpg, aes(x=class, y=hwy)) + 
  geom_boxplot(notch = T, outlier.colour="red", outlier.shape=8,
               outlier.size=4) + 
  labs(title="Plot of speed per car class",x="Types of Car", y = "Highway Speed (mph)")+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
  theme_classic() #smaller dots?
 
## Decrease dot size
ggplot(mpg, aes(x=class, y=hwy)) + 
  geom_boxplot(notch = T, outlier.colour="red", outlier.shape=8,
               outlier.size=4) + 
  labs(title="Plot of speed per car class",x="Types of Car", y = "Highway Speed (mph)")+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.4) +
  theme_classic()  

## Change box plot line with colors
ggplot(mpg, aes(x=class, y=hwy, color=class)) +
  geom_boxplot() #looks good enough --> change to custom colors from default

p <- ggplot(mpg, aes(x=class, y=hwy, color=class)) +
  geom_boxplot()  
p

# Use custom color palettes
p+scale_color_manual(values=c("grey", "gold", "lightblue", "lightgreen", "aquamarine", "coral","lavender"))
## Color charts

# Use brewer color palettes
p+scale_color_brewer(palette="Dark2")
## color palette (??)

# Use grey scale
p + scale_color_grey() + theme_classic()

# Fill box plots with colors
p<-ggplot(mpg, aes(x=class, y=hwy, fill=class)) +
  geom_boxplot()
p

# Use custom color palettes
p+scale_fill_manual(values=c("grey", "gold", "lightblue", "lightgreen", "aquamarine", "coral","lavender"))
## Color charts

# Use brewer color palettes
p+scale_fill_brewer(palette="Dark2")
## color palette (??)

# Use grey scale
p + scale_fill_grey() + theme_classic()

## Change legend position
p + theme(legend.position="top")
p + theme(legend.position="bottom")
p + theme(legend.position="none") # Remove legend

## Change the order of items
p + scale_x_discrete(limits=c("subcompact", "compact", "midsize",
                              "2seater", "minivan", "suv", "pickup"))

## Box plots with multiple groups
# Change box plot colors by groups
ggplot(mpg, aes(x=class, y=hwy, fill=grade)) +
  geom_boxplot()

## Backgrounds themes
# classic theme
p + scale_fill_brewer(palette="Blues") + theme_classic()

# minimal theme
p + scale_fill_brewer(palette="Blues") + theme_minimal()

# default theme
p + scale_fill_brewer(palette="Blues")  

# void theme
p + scale_fill_brewer(palette="Blues") + theme_void()

# dark theme
p + scale_fill_brewer(palette="Blues") + theme_dark()

# black and white theme
p + scale_fill_brewer(palette="Blues") + theme_bw()

# linedraw theme
p + scale_fill_brewer(palette="Blues") + theme_linedraw()


## Visualization with 2 categorical variables
# mpg
ggplot(data = mpg, mapping = aes(x = hwy)) +
  geom_freqpoly(mapping = aes(color = grade2), binwidth = 1)

ggplot(mpg, aes(x=factor(grade2)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()  

ggplot(mpg, aes(x=factor(grade2)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal() +
  coord_flip()
#https://r-charts.com/colors/


## Aesthetic Visualization:Scatter plot
# Scatter plots: Look at the relationship between two numerical variables
library(ggplot2)
# Create a scatter plot between subjects
ggplot(data = exam) + 
  geom_point(mapping = aes(x = math, y = science))

ggplot(data = exam) + 
  geom_point(mapping = aes(x = math, y = english))

ggplot(data = exam) + 
  geom_point(mapping = aes(x = english, y = science))

# Increase size
ggplot(data = exam) + 
  geom_point(mapping = aes(x = math, y = science), size = 3)

ggplot(data = exam) + 
  geom_point(mapping = aes(x = math, y = english), size = 3)

ggplot(data = exam) + 
  geom_point(mapping = aes(x = english, y = science), size = 3)

# Group the observations based on class
ggplot(data = exam) + 
  geom_point(mapping = aes(x = math, y = science, color = class))

ggplot(data = exam) + 
  geom_point(mapping = aes(x = math, y = english, color = class))

ggplot(data = exam) + 
  geom_point(mapping = aes(x = science, y = english, color = class))

# Group the observations based on class: Increase size
ggplot(data = exam) + 
  geom_point(mapping = aes(x = math, y = science, color = class), size = 4)

# Change the color manually
exam$class <- as.factor(exam$class)
sp <- ggplot(data = exam) + 
  geom_point(mapping = aes(x = english, y = science, color = class, size = 4))

sp + scale_color_manual(breaks = c("1", "2", "3", "4", "5"),
                        values=c("red", "blue", "green", "yellow", "brown"))

# Group the observations based on shape
ggplot(data = exam) + 
  geom_point(mapping = aes(x = math, y = science, shape = class, size = 4))

ggplot(data = exam) + 
  geom_point(mapping = aes(x = math, y = english, shape = class, size = 4))

ggplot(data = exam) + 
  geom_point(mapping = aes(x = science, y = english, shape = class, size = 4))
