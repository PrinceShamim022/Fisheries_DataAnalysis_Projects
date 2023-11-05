# Analysis Fish dataset

# Importing The Dataset
fish = read.csv("https://raw.githubusercontent.com/PrinceShamim022/Fisheries_DataAnalysis_Projects/main/Marine%20Fish/Fish.csv")
head(fish)
str(fish)
dim(fish)

# Checking for Missing Variables
any(is.na(fish))

summary(fish)


# Filtering Dataset
weight0<- fish %>%
  filter(Weight == 0)

weight0

# Filtering Dataset
fish <- fish%>%
  filter(Weight>0)

summary(fish)


# Outlier Detection
boxplot(fish$Weight, main="Weight")
boxplot(fish[,c(3,4,5)], main="Lengths")
boxplot(fish$Height, main="Height")
boxplot(fish$Width, main="Width")

# Removing Outliers
fish <- fish%>%
  filter(Weight<1500)
b1 = boxplot(fish$Weight, main="Weight")
b2 = boxplot(fish[,c(3,4,5)], main="Lengths")
b3 = boxplot(fish$Height, main="Height")
b4 = boxplot(fish$Width, main="Width")


# Correlation Table
cor(fish[,-1])
pairs(fish[,-1])

round(cor(fish[,-1]),3)

# Data Partition
# Spliting model into training and testing
set.seed(1234)
ind = sample(2,nrow(fish),replace=TRUE,prob=c(0.7,0.3))
training = fish[ind == 1,]
testing = fish[ind == 2,]
head(training)
head(testing)

# installed.packages("faraway")
library(faraway)


model <- lm(Weight ~ LengthV + LengthD + LengthC + Width + Height,
            data = training)
summary(model)
vif(model)



model1 <- lm(Weight ~ LengthV + LengthD + LengthC,
             data = training)
summary(model1)
vif(model1)

# Again we found that the LengthC has a lower value.
model2 <- lm(Weight ~ LengthC + Width + Height,
             data = training)
summary(model2)
vif(model2)


# PLOT
plot(Weight~ LengthC + Width + Height, training)
abline(model2,col="blue")
par(mfrow=c(2,2))
plot(model1)


pred = predict(model2, testing)
head(pred)
head(testing)

tail(pred)
tail(testing)


