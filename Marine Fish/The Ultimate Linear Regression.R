# Weight Prediction - LogLog Linear Regression

# Libraries
library(ggplot2)
library(GGally)
library(dplyr)
library(caTools)
library(lmtest)
library(caret)

# Importing Data and first Analysis
fish <- read.csv("Fish.csv")

str(fish)


# Rename columns
names(fish) <- c("Species","Weight","LengthV","LengthD","LengthC","Height","Width")

summary(fish)


cat("Are there any missing value in the dataset?" ,any(is.na(fish)))

# any problem in row 41
fish[41,]

# The fish at row 41 weights 0 g. So, to avoid errors when using the logarithmic transformation, 
# it's better to drop this observation.
fish <- fish%>%
  filter(Weight>0)

# Analysis and Visulization
options(repr.plot.width = 14, repr.plot.height = 8)

ggplot(fish,aes(x=Weight,fill=Species))+
  geom_histogram(alpha=0.8,col="black",bins=30)


# We can also use the following script to study the distribution of Weight, 
# grouped by Species, with a density plot.
fish.means <- fish %>%
  group_by(Species)%>%
  summarise(Mean_weight = mean(log(Weight)))


ggplot(fish,aes(x=log(Weight),fill=Species,color=Species,group=Species)) +
  geom_density(alpha=0.4, lwd=1) +
  geom_vline(data=fish.means,aes(xintercept = Mean_weight,color=Species),lwd=0.3 ,lty= "dashed")+
  scale_x_continuous(limits = c(1.5,8.5))


#  multivariate plot
# To rapidly visualize all of our continuous data, We can use the function ggpairs().
ggpairs(fish[2:7])


Height_Weight <- ggplot(fish,aes(x=Height,y=Weight,col=Species))+
  geom_point(size=5,alpha=0.6)+
  geom_smooth(col="grey40",method = "lm",se=F,lty="dashed",lwd=1,formula="y~x")

LengthV_Weight <- ggplot(fish,aes(x=LengthV,y=Weight,col=Species))+
  geom_point(size=5,alpha=0.6)+
  geom_smooth(col="grey40",method = "lm",se=F,lty="dashed",lwd=1,formula="y~x")

Width_Weight <- ggplot(fish,aes(x=Width,y=Weight,col=Species))+
  geom_point(size=5,alpha=0.6)+
  geom_smooth(col="grey40",method = "lm",se=F,lty="dashed",lwd=1,formula="y~x")

Height_Weight
LengthV_Weight
Width_Weight


# These relationships aren't linear.


# log-height vs. log-weight
Height_Weight_log <- ggplot(fish,aes(x=log(Height),y=log(Weight),col=Species))+
  geom_point(size=5,alpha=0.6)+
  geom_smooth(col="grey40",method = "lm",se=F,lty="dashed",lwd=1,,formula="y~x")

LengthV_Weight_log <- ggplot(fish,aes(x=log(LengthV),y=log(Weight),col=Species))+
  geom_point(size=5,alpha=0.6)+
  geom_smooth(col="grey40",method = "lm",se=F,lty="dashed",lwd=1,,formula="y~x")

Width_Weight_log  <- ggplot(fish,aes(x=log(Width),y=log(Weight),col=Species))+
  geom_point(size=5,alpha=0.6)+
  geom_smooth(col="grey40",method = "lm",se=F,lty="dashed",lwd=1,,formula="y~x")

Height_Weight_log
LengthV_Weight_log
Width_Weight_log


# correlation
# The correlation between LengthV, LengthD and LengthC is almost equal to 1
fish_mat <- fish%>%
  select(LengthV,LengthD,LengthC,Height,Width)

cor(fish_mat)


# Multiple Linear Regression
fish.reg <- lm(Weight ~ LengthV + LengthD + LengthC + Height + Width, data = fish)
summary(fish.reg)


# removing redundant variables we don't lose too much informations. 
# So,we can remove LengthD and LengthC, and just consider LengthV
fish.reg.red <- lm(Weight ~ LengthV + Height + Width,data = fish)
summary(fish.reg.red)


# To understand which model performs better on our data, 
# we use three useful tools: F-test,AIC and BIC
cat("F-TEST:")
anova(fish.reg.red,fish.reg)

cat("AIC:")
AIC(fish.reg.red,fish.reg)

cat("BIC:")
BIC(fish.reg.red,fish.reg)


#  distribution of residuals:
residplot.lin <- data.frame(resid = fish.reg.red$residuals,
                            Weight = fish$Weight)

ggplot(residplot.lin,aes(x = Weight,y = resid))+
  geom_hline(yintercept = 0, size=1, lty="dashed", alpha=0.4)+
  geom_point(size= 4, alpha=0.7)


# transform our data and fit a new model
fish.reg.ll <- lm(log(Weight) ~ log(LengthV) + log(Height) + log(Width),data = fish)
summary(fish.reg.ll)


# distribution of residuals after distribution of residuals
residplot.ll <- data.frame(resid = fish.reg.ll$residuals, Weight = fish$Weight)

ggplot(residplot.ll,aes(x = log(Weight), y = resid))+
  geom_hline(yintercept = 0, size = 1,lty = "dashed",alpha = 0.4)+
  geom_point(size = 3,alpha = 0.7)
# Great! Now the residuals don't follow a scheme


# we can use a Breusch & Pagan Test to check 
# if the residuals are heteroskedastical
bptest(fish.reg.ll)


# Prediction
# we split our data in Train set and Test set
fish$train <- sample.split(fish$Weight, SplitRatio = 0.7)
fish.train <- fish[fish$train == TRUE,]
fish.test <- fish[fish$train == FALSE,]

# And fit our two models only using the observations that are in the training set
fish.reg.red<- lm(Weight ~ LengthV + Height + Width,data = fish.train)
fish.reg.ll <- lm(log(Weight) ~ log(LengthV) + log(Height) + log(Width),data=fish.train)


# prediction for Non-Transformed Linear Model
pred.lin <- predict(fish.reg.red,fish.test)
cat("R2 of the Non-Transformed Linear Model:",R2(pred.lin,fish.test$Weight))

# prediction for Log-Log Linear Model
pred.ll <- exp(predict(fish.reg.ll,fish.test))
cat("\nR2 of the Log-Log Linear Model:",R2(pred.ll,fish.test$Weight))

# The Log-Log model is better at predicting than the fish.reg.red model!


# visualize the 
pred_plot <- data.frame(pred_ll = pred.ll, pred_lin = pred.lin,
                        real_values = fish.test$Weight, unit = seq(1,length(fish.test$Weight),by = 1))

ggplot(pred_plot,aes(x=unit,y=real_values))+
  geom_point(col="red",size=7,alpha=0.7)+
  geom_point(aes(x=unit,y=pred_lin),col="green",size=3,alpha=0.7)+
  geom_point(aes(x=unit,y=pred_ll),col="blue",size=5,alpha=0.7)+
  scale_x_continuous(breaks=seq(1,48,by=1),labels  =seq(1,48,by=1),minor_breaks = NULL)
# The predictions made with the Log-Log model (blue dots) are clearly closer to the real values (red dots) 
# then the predictions made with the non-transformed linear model (green dots).


pred_plot[pred_plot$pred_lin<0,c(1,2,3)]


