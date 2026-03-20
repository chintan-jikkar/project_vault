2+2 
#the coursor should be on the last line of the code being run

1:100
#this prints numbers from 1 to 100

print("I am Iron Man")
#prints hello world as is

a <- 1 #run this line first else the next line wont work
a #this prints the value and not the word itself

2 -> b # same but the other way around less commonly used
b

b <- 3
b

c <- d <- e <- 3 #this assigns the same value to all the variables
c
d
e

c+d+e

#use of c function combine/concatenate
x <- c(1, 2, 5, 9)
x

0:10

10:0

seq(10)
#seq is sequence function starting from 1 all the way to the number mentioned

seq(30, 0, by = -3)
#in this it starts from 30 to 0 but it has a differene of -3 in between

x <- c(1,2,5,9)
x
(y <- c(5,0,1,10))

x+y
#this sums all the elements of x to those of y

x*2

f <- c(2,4 ,6 ,9)
g <- c(1,3,5,7,9)
f+g

2^6
#2 to the power of 6

sqrt(64)
#square root

log(100)

log10(100)

##DATA TYPES : NUMERICS, CHARACTERS, LOGICAL

n1 <- 15
n1
typeof(n1)
#this tells you the data type of the object
#DOUBLE means numerical value in R

n2 <- 1.5
n2
typeof(n2)

c1 <- "c"
#double quotes so that it knows its a character value and not a typo/script itself
c1
typeof(c1)
#character as in wordings

c2 <- "2"
typeof(c2)
#c2 becomes a character value coz we used double quotes so its a string

c3 <- "a string of text"
c3
typeof(c3)

l1 <- True # this one wont work all the characters should be UPPER CASE
l1 <- TRUE
typeof(l1)

l2 <- FALSE
l2
typeof(l2)

## DATA STRUCTURES : SCALAR, VECTORS, MATRIX

a1 <- 9 #this creates a scalar named a1 that stores value 9

a2 <- c(1,2,5,.3,6,-2,4) #this links the list of items to the same variable 
#all member should be of the same data type within a vector
a2

b1 <- c("one","two", "three")
#character vector

C <- c(T,T,TRUE, FALSE, TRUE, FALSE)
#legical vector

is.vector(a2)
#function to check the data type for vector

m1 <- matrix(c(T,T,F,F,T,F), nrow = 2)
#nrow is for the number of rows we want in the matrix
m1

m2 <- matrix(c(T,T,F,F,T,F), ncol = 2)
#ncol is for number of columns
m2

is.matrix(m1)
#this is to check if m1 is a matrix or not

typeof(m1)
#just tells the type and not the specific one 

m3 <- matrix(c("a", "b", "c", "d"),
             nrow = 2, byrow = T)
# by row true argument means it will print it int the left to right format like a Z
m3

matrix(1:6, nrow = 3, ncol = 2, byrow= T)
matrix(1:6, nrow = 3, ncol = 2)
# this is how it shows by default if the byrow is not mentioned, in a inverted N format

## DATA FRAME

#a 2D rectangular layout
#column is a variable and rows represent a unit of observation
#this allows you to store different data type in different columns
# but they have to be of the same time in a particular column

vNumeric <- c(1, 2, 3)
vCharacter <- c("a","b","c")
vLogical <- c(T, F, T)

#cbind function combines the three vectors that we created 
df1 <- cbind(vNumeric, vCharacter, vLogical)
df1
is(df1)
#check what it is
#array: more general structure, data with 1 or more dimensions
#vector: matric or array is ultimately a vector with attributes

df2 <- data.frame(vNumeric, vCharacter, vLogical)
#data.frame function creates a data frame and not a matrix
df2
str(df2)
#str tells you the structure if the data frame

is(df2$vNumeric)
#$ sign tells you the data type of each vaiable within the data frame, it separates the data, the data frame, and the variable
# it is used to extract elements by name from list or data frame in R

is(df2$vCharacter)
is(df2$vLogical)

names(df2) <- c("number", "character", "logical") 
#this function is used to change the name of the varable sin the data frame

length(df2)
#number of columns
#you can also use it for specific elements
length(df2$number)
str(df2)
#structure of the object
class(df2)
#class or type of an object
names(df2)


Name <- c("Suzy", "Nate", "Michael")
Age <- c(13,15,19)
Favourites <- c("math", "science", "math")

df3 <- cbind(Name, Age, Favourites)
df3

df3 <- data.frame(df3)
df3

is(df3)

df3 <- rbind(Name,Age,Favourites)
df3

names <- c("Minjo","Evo")
age <- c(17,17)
favourites <- c("science","social studies")

df4 <- data.frame(names,age,favourites)
df4

df5 <- rbind(df3,df4)


###DOESNT WORK FOR SOME REASON


#< less than
#<= less than equal to
#> greater than
#>= greater than equal to


x <- c(1:10)
x
x[(x>8) | (x<5)] # the symbol int he middel is used as OR OPERATOR

x[(x>8)]

x <- c(12,4,14)
y <- c(3,4,15)
x != y
#! this signifies IS NOT i.e. x is not equal to y


#It is what it is!!
#Example of how to use comments 

cbind
rbind

x != y
#this is called negation

#Installing packages for R

#Method 1
install.packages("lattice")
install.packages("stringr")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("foreign")
install.packages("MASS")
install.packages("plotly")


#Method 2
#in packages, plots, files pane, manually do it

#Now you have to load it since it is installed and you have to do it eveyrtime
library(ggplot2)
library(dplyr)
library(foreign)
require(foreign)


#importing data set to use in R

#Delimiter
#eg. Comma Semicolon Pipes Slashes or TAB key but they need to be used uniformly

DF1 <- read.table(file="/Users/chintanjikkar/Desktop/PACE/Fall 25/Visual Analytics/Data/data1.txt", header = T)
DF1
View(DF1)


###24 SEPTEMBER 

DF1

DF3 <- read.csv("/Users/chintanjikkar/Desktop/PACE/Fall 25/Visual Analytics/data3.csv")
DF3

write.table(DF3, file = "/Users/chintanjikkar/Desktop/PACE/Fall 25/Visual Analytics/DF3.txt", 
            row.names = TRUE)

titanic <- read.csv("/Users/chintanjikkar/Desktop/PACE/Fall 25/Visual Analytics/titanic.csv", 
                    header = TRUE)

myvars <- c("Survived","Pclass","Name")

col_titanic_sub2 <- titanic[c(1,5:8)]

col_titanic_sub3 <- titanic[c(-3,-5)] 
#this excludes 3rd and 5th variables

row_titanic_sub1 <- titanic[c(1:5)]

row_titanic_sub2 <- titanic[which(titanic$Sex=="female" 
                                  & titanic$Age > "35"),]

row_titanic_sub3 <- subset(titanic, Age >= 20 | Age < 10,
                           select = c(Survived, Pclass, Age))

row_titanic_sub4 
  
  
## EDA

mpg <- as.data.frame(ggplot2::mpg)
str(mpg)

mTable <- table(mpg$manufacturer)
mcTable <- table(mpg$cTable)

prop.table(mTable)
prop.table(mcTable)

summary(mpg$displ)

library(psych)
describe(mpg$displ)
?mpg

library(ggplot2)
ggplot(mpg, aes(x= factor(cyl))) + geom_bar()

ggplot(mpg, aes(x= factor(manufacturer))) + geom_bar()

