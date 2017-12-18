human <- read.csv("/Users/eva-mariaroth/Documents/GitHub/IODS-final/human_.csv", header = TRUE, sep= ",")
glimpse(human)
library(GGally)
library(ggplot2)
#checking the correlations
p <- ggpairs(human, mapping = aes(), lower = list(combo = wrap("facethist", bins = 20)))
p

#creating a scatter plot of Life.Exp versus mean.edu with a linear regression line
qplot(Life.Exp, mean.edu, data = human) + geom_smooth(method = "lm")
#fitting a linear model where Life.Exp is the target, whereas mean. edu is the explanatory variable
my_model <- lm(Life.Exp ~ mean.edu, data = human)
summary(my_model)
#fitting a multiple regression model with several explanatory variables
my_model2 <- lm(Life.Exp ~ mean.edu + Edu2.FM + Parli.F, data =human)
summary(my_model2)
#exploring validity of the model by drawing diagnostic plots
par(mfrow = c(2,2))
plot(my_model2, which = c(1,2,5))
#predicting
m <- lm(Life.Exp ~ mean.edu, data = human)

# print out a summary of the model
summary(m)

# New observations
new_mean.edu <- c("Niger" = 12.1, "Chad"= 11.5, "Mali" = 10.3, "Nepal" = 10.5, "Haiti" =11,7, "Yemen" = 10)
new_data <- data.frame(mean.edu = new_mean.edu)

# Print out the new data
new_data
predict(m, newdata = new_data)

#K-means
#clustering
#scaling the dataset
human_scaled <- scale(human)
# summaries of the scaled variables
summary(human_scaled)

# class of the boston_scaled object
class(human_scaled)

# change the object to data frame
human_scaled <- as.data.frame(human_scaled)
summary(human_scaled$Life.Exp)
bins <- quantile(human_scaled$Life.Exp)
bins
#create a categorical variable crime
Lifeexp <- cut(human_scaled$Life.Exp, breaks = bins, include.lowest = TRUE, labels = c("low", "med_low", "med_high", "high"))
table(Lifeexp)
#remove original Life.Exp variable from the dataset
human_scaled <- dplyr::select(human_scaled, -Life.Exp)
#add the new categorical value to scaled data
human_scaled <- data.frame(human_scaled, Lifeexp)
glimpse(human_scaled)

#K means
km <-kmeans(human, centers = 3)
pairs(human[2:4], col = km$cluster)
str(human)
library(MASS)
#calculate optimal number of clusters
set.seed(123)
#determine the number of clusters
k_max <-10
# calculating the total within sum of squares
twcss <- sapply(1:k_max, function(k){kmeans(human, k)$tot.withinss})
#visualize the results
qplot(x = 1:k_max, y = twcss, geom = 'line')
