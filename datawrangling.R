
# Name: Eva Roth
# Date: 18.12.2017
# Final assignment - data wrangling in preperation for multiple regression and linear discriminant analysis
# Data source: IODS-course, MOOC platform

# loading the cvs tables "Human development" and "Gender inequality" into R. 
hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)
gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

# Exploring the structures and dimensions.
dim(hd)
str(hd)
summary(hd)
# "Human development" consists of 195 obs. of 8 variables

dim(gii)
str(gii)
summary(gii)
# Gender inequality consists of 195 observations of 10 variables

# Renaming the variables
colnames(hd)
colnames(hd)[1]<-"HDI.rank"
colnames(hd)[2]<-"Country"
colnames(hd)[3]<-"HDI"
colnames(hd)[4]<-"Life.Exp"
colnames(hd)[5]<-"Edu.Exp"
colnames(hd)[6]<-"Mean.Edu"
colnames(hd)[7]<-"GNI"
colnames(hd)[8]<-"GNI.rank-HDI.rank"
colnames(hd)

colnames(gii)
colnames(gii)[1]<-"GII.rank"
colnames(gii)[2]<-"Country"
colnames(gii)[3]<-"GII"
colnames(gii)[4]<-"Mat.Mor"
colnames(gii)[5]<-"Ado.Birth"
colnames(gii)[6]<-"Parli.F"
colnames(gii)[7]<-"edu2F"
colnames(gii)[8]<-"edu2M"
colnames(gii)[9]<-"labF"
colnames(gii)[10]<-"labM" 
colnames(gii)  

#Mutating the "Gender inequality" data in order to create a new variable.
#First accessing the tidyverse package dplyr
library(dplyr)
gii <- mutate(gii, Edu2.FM = gii$edu2F/gii$edu2M)

colnames(gii)  
summary(gii$ratio.edu2)  
summary(gii$ratio.lab)  

#Joining the two datasets using the variable country as the identifier.  
human <- inner_join(hd, gii, by = "Country")
dim(human)
#The dataset human has 195 observations of 19 variables
glimpse(human)
#Mutating data. Transforming Gross National Income (GNI) variable to numeric, using string manipulation
dim(human)
str(human$GNI)
library(stringr)
GNI2 <- str_replace(human$GNI, pattern=",", replace ="") %>% as.numeric
human <- mutate(human, GNI2)
str(human$GNI2)
dim(human)
colnames(human)

#Selecting certain variables and excluding the rest.
keep <- c("HDI", "Country", "Edu2.FM", "Life.Exp", "Mean.Edu", "GNI2", "Mat.Mor", "Ado.Birth", "Parli.F")
human <- select(human, one_of(keep))
colnames(human)
#Renaming the column GNI2 (the variable GNI, transformed into numeric) back to GNI
colnames(human)[6]<-"GNI"
colnames(human)

#Removing all the rows with missing values
complete.cases(human)
data.frame(human[-1], comp = complete.cases(human))
human_ <- filter(human, complete.cases(human))
str(human_)
# 162 observations of 9 variables

#The last seven observations are not concerning countries, but regions. We want to exclude them from the dataset.
tail(human_, n=10)
last <- nrow(human) -7
human_ <- human_ [1:155,]
str(human_)

#Defining the row names by the country names and removing the country name column from the data.
rownames(human_) <- human_$Country
str(human_)
human_ <- select(human_, -Country)
str(human_)
#155 observations of 8 variables.

#Setting the working directory
setwd("/Users/eva-mariaroth/Documents/GitHub/IODS-final")
#Saving the data
write.table(human_, file = "human_.csv", sep = ",", row.names = TRUE, col.names = TRUE)
#Reopening it to see if everything worked out fine.
test <- read.csv("/Users/eva-mariaroth/Documents/GitHub/IODS-final/human_.csv", header = TRUE, sep= ",")
glimpse(test)
#155 observations of 8 variables

# Excluding the variables HDI and GNI for the LDA as they are not going to be assessed in that analysis.
# Saving the second dataset.
keep <- c("Edu2.FM", "Life.Exp", "Mean.Edu", "Mat.Mor", "Ado.Birth", "Parli.F")
human2 <- select(human, one_of(keep))
write.table(human2, file = "human2.csv", sep = ",", row.names = TRUE, col.names = TRUE)


