# Remove all objects from enviroment
rm(list=ls())

# Set Working Directory
setwd("/Users/ad/Desktop/Project 2")

# Check the set directory
getwd()

# Loading Required Libraries
x= c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "MASS", "rpart", "gbm",
    "ROSE","dummies", "inTrees", "e1071")

# Install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

# Importing dataframe
data<- read.csv("Absenteeism_at_work_Project.csv", header = TRUE)

# Exploring Data
colnames(data)
str(data)
summary(data)

histogram(data$Reason.for.absence) # Factors influencing health status and contact with health services
histogram(data$Month.of.absence)
histogram(data$Day.of.the.week)
histogram(data$Seasons)
histogram(data$Transportation.expense)
hist(data$Distance.from.Residence.to.Work)
hist(data$Service.time)
hist(data$Age)
hist(data$Work.load.per.day)
hist(data$Hit.target)
hist(data$Disciplinary.failure)
hist(data$Education)
hist(data$Son)
hist(data$Social.drinker)
hist(data$Social.smoker)
hist(data$Pet)
hist(data$Weight)
hist(data$Height)
hist(data$Body.mass.index)
hist(data$Absenteeism.time.in.hours)

### Missing Value Analysis ###
# Create dataframe with missing value count
missing_val = data.frame(apply(data, 2, function(x){ sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
row.names(missing_val) = NULL
#names(missing_val)[1]= "Missing_Percentage"

# Converting missing values to percentage
missing_val$Missing_Percentage <- missing_val$apply.data..2..function.x...
missing_val$Missing_Percentage = (missing_val$Missing_Percentage/nrow(data)) * 100

# Rearranging Columns
missing_val = missing_val[,c(2,1,3)]

# Arranging missing_percentage in decending order
missing_val = missing_val[order(-missing_val$Missing_Percentage),]

# Plot bar-graph for missing values
ggplot(data = missing_val[1:10,], aes(x= reorder(Columns, -Missing_Percentage), y= Missing_Percentage))+
  geom_bar(stat = "identity", fill= "grey")+ xlab("Parameter")+
  ggtitle("Missing Data Percentage")+ theme_bw()

# Changing data type of "Work.load.Average.per.day" to numeric
data$Work.load.per.day = as.numeric(data$Work.load.per.day)

## Imputing Missing Values
data$Body.mass.index[is.na(data$Body.mass.index)] = median(data$Body.mass.index, na.rm = T)
data$Absenteeism.time.in.hours[is.na(data$Absenteeism.time.in.hours)] = median(data$Absenteeism.time.in.hours, na.rm = T)
data$Height[is.na(data$Height)] = median(data$Height, na.rm = T)
data$Work.load.per.day[is.na(data$Work.load.per.day)] = median(data$Work.load.per.day , na.rm = T)
data$Education[is.na(data$Education)] = median(data$Education, na.rm = T)
data$Transportation.expense[is.na(data$Transportation.expense)] = median(data$Transportation.expense, na.rm = T)
data$Hit.target[is.na(data$Hit.target)] = median(data$Hit.target, na.rm = T)
data$Education[is.na(data$Education)] = median(data$Education, na.rm = T)
data$Disciplinary.failure[is.na(data$Disciplinary.failure)] = median(data$Disciplinary.failure, na.rm = T)
data$Son[is.na(data$Son)] = median(data$Son, na.rm = T)
data$Social.smoker[is.na(data$Social.smoker)] = median(data$Social.smoker , na.rm = T)
data$Reason.for.absence[is.na(data$Reason.for.absence)] = median(data$Reason.for.absence, na.rm = T)
data$Distance.from.Residence.to.Work[is.na(data$Distance.from.Residence.to.Work)] = median(data$Distance.from.Residence.to.Work, na.rm = T)
data$Service.time[is.na(data$Service.time)] = median(data$Service.time, na.rm = T)
data$Age[is.na(data$Age)] = median(data$Age, na.rm = T)
data$Social.drinker[is.na(data$Social.drinker)] = median(data$Social.drinker, na.rm = T)
data$Pet[is.na(data$Pet)] = median(data$Pet, na.rm = T)
data$Month.of.absence[is.na(data$Month.of.absence)] = median(data$Month.of.absence, na.rm = T)
data$Weight[is.na(data$Weight)] = median(data$Weight, na.rm = T)

# Data Manipulation
data$Month.of.absence = as.factor(data$Month.of.absence)
data$Reason.for.absence = as.factor(data$Reason.for.absence)
data$Day.of.the.week = as.factor(data$Day.of.the.week)
data$Seasons = as.factor(data$Seasons)
data$Disciplinary.failure = as.factor(data$Disciplinary.failure)
data$Education = as.factor(data$Education)
data$Social.drinker = as.factor(data$Social.drinker)
data$Social.smoker = as.factor(data$Social.smoker)


# Separating continuous and categorical variables
numeric_index = sapply(data, is.numeric)
numeric_data = data[,numeric_index]
factor_index = sapply(data, is.factor)
factor_data = data[,factor_index]

### Outlier Analysis ###
cnames = colnames(numeric_data)

for (i in 1:length(cnames)){
  assign(paste0("gn", i), ggplot(aes_string(y = (cnames[i]), x = "Absenteeism.time.in.hours"), data = numeric_data)+
  stat_boxplot(geom = "errorbar", width = 0.5 ) +
  geom_boxplot(outlier.colour = "red", fill = "grey", outlier.shape = 18, outlier.size = 3, notch = FALSE) +
  theme(legend.position = "bottom") +
  labs(y=cnames[i], x= "Absenteeism.time.in.hours") +
  ggtitle(paste("Boxplot for", cnames[i])))}

## Plotting plots together
gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
gridExtra::grid.arrange(gn4,gn5,gn6, ncol=3)
gridExtra::grid.arrange(gn7,gn8,gn9, ncol=3)
gridExtra::grid.arrange(gn10,gn11,gn12, ncol=3)

# Saving dataframe as backup
df<- data
#data <- df

# Replace outliers with NA and imputing
for (i in cnames){
      val = data[,i][data[,i] %in% boxplot.stats(data[,i])$out]
      #print(length(val))
      data[,i][data[,i] %in% val] = NA
      }

# Impute the NA's using KNN imputation
data = knnImputation(data, k=3)

### Feature Scaling ###
qqnorm(data$Distance.from.Residence.to.Work)
qqnorm(data$Transportation.expense)
qqnorm(data$Service.time)
qqnorm(data$Age)
qqnorm(data$Work.load.per.day) # Normal data
qqline(data$Work.load.per.day)
qqnorm(data$Hit.target)
qqnorm(data$Pet)
qqnorm(data$Weight)
qqnorm(data$Height)
qqnorm(data$Body.mass.index)
qqnorm(data$Absenteeism.time.in.hours)

### Feature Selection ###

## Correlation Plot
corrgram(data[,numeric_index], order = FALSE,
         upper.panel= panel.conf, text.panel= panel.txt, main = "Correlation Plot")


## ANOVA Test on Categorical Variables
aov1 <- aov(data$Absenteeism.time.in.hours~data$Reason.for.absence)
aov2 <- aov(data$Absenteeism.time.in.hours~data$Month.of.absence)
aov3 <- aov(data$Absenteeism.time.in.hours~data$Day.of.the.week)
aov4 <- aov(data$Absenteeism.time.in.hours~data$Seasons)
aov5 <- aov(data$Absenteeism.time.in.hours~data$Disciplinary.failure)
aov6 <- aov(data$Absenteeism.time.in.hours~data$Education)
aov7 <- aov(data$Absenteeism.time.in.hours~data$Social.drinker)
aov8 <- aov(data$Absenteeism.time.in.hours~data$Social.smoker)
  
# Summary of ANOVA test
summary(aov1)
summary(aov2)
summary(aov3)
summary(aov4)
summary(aov5)
summary(aov6)
summary(aov7)
summary(aov8)

# Remove "Weight", "Day.of.week", "seasons","education", "social.smoker"
numeric_data <- subset(numeric_data, select = -c(Weight))
factor_data <- subset(factor_data, select= -c(Day.of.the.week, Seasons, Education, Social.smoker))

## Creating Dummy Variables for Catagorical Variables
factor_new = dummy.data.frame(factor_data, sep = "_")

### Feature Scaling ###
## Normalizing the numeric variables
for (i in 1:12){
  #print(i)
  numeric_data[,i] = (numeric_data[,i] - min(numeric_data[,i]))/
      (max(numeric_data[,i]) - min(numeric_data[,i])) 
}

### Sampling ###
df <- cbind(factor_new, numeric_data)
train_index <- sample(1:nrow(df), 0.8 * nrow(df))
train <- df[train_index,]
test <- df[-train_index,]

### Principal Component Analysis ###
pc <- prcomp(train,center = F,scale. = F )
names(pc)

# Output the mean of variables
pc$center
std_dev <- pc$sdev

# Compute variance
pc_var <- std_dev^2
prop_varex <- pc_var/sum(pc_var)

# Plot the resultant principal components.
biplot(pc, scale = 0)

# Scree plot
plot(prop_varex, xlab = "Principal Component", ylab = "Proportion of Variance Explained", type = "b")

# Cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")

train.data <- data.frame(Absenteeism.time.in.hours = train$Absenteeism.time.in.hours, pc$x)
train.data <- train.data[,1:43]
test.data <- data.frame(Absenteeism.time.in.hours = test$Absenteeism.time.in.hours, pc$x)
test.data <- test.data[,1:43]

### Decision Tree Model ###

DT <- rpart(Absenteeism.time.in.hours~., data = train.data, method = "anova")
# Predict test data
prediction_DT <- predict(DT, test.data[,-1])
# Error Metric
regr.eval(test.data[,1], prediction_DT, stats = c('mae','rmse','mape', 'mse'))

### Linear Regression Model ###

library(usdm)
vif(train.data[,-1]) 
vifcor(train.data[,-1], th = 0.9)
LM <- lm(Absenteeism.time.in.hours~., data= train.data)
summary(LM)

# Predict Test Data
prediction_lm <- predict(LM, test.data[,-1])

# Error Metric
regr.eval(test.data[,1], prediction_lm, stats = c('mae','rmse','mape', 'mse'))

### Random Forest Model ###

RF <- randomForest(Absenteeism.time.in.hours~., train.data, importance= TRUE, ntree = 500)

# Predict Test Data
prediction_RF <- predict(RF, test.data[,-1])

# Error Metric
regr.eval(test.data[,1], prediction_RF, stats = c('mae','rmse','mape', 'mse'))

### Extra Plots ###
library("scales")
library("psych")
library("gplots")

ggplot(data, aes_string(x= data$Reason.for.absence, y= data$Absenteeism.time.in.hours)) +
  geom_bar(stat = "identity", fill = "Darkslateblue") + theme_bw() +
  xlab("Reason for absence") + ylab("Absent Hours") + scale_y_continuous(breaks = pretty_breaks(n=10)) +
  ggtitle("Absenteeism Analysis") + theme(text = element_text(size = 15))

ggplot(data, aes_string(x= data$Reason.for.absence)) +
  geom_bar(stat = "Count", fill = "Darkslateblue") + theme_bw() +
  xlab("Reason for absence") + ylab("Count") + scale_y_continuous(breaks = pretty_breaks(n=10)) +
  ggtitle("Absenteeism Analysis") + theme(text = element_text(size = 15))

ggplot(data, aes_string(x= data$Month.of.absence, y= data$Absenteeism.time.in.hours)) +
  geom_bar(stat = "identity", fill = "Darkslateblue") + theme_bw() +
  xlab("Months") + ylab("Absent Hours") + scale_y_continuous(breaks = pretty_breaks(n=10)) +
  ggtitle("Absenteeism Analysis") + theme(text = element_text(size = 15))

ggplot(data, aes_string(x= data$Day.of.the.week, y= data$Absenteeism.time.in.hours)) +
  geom_bar(stat = "identity", fill = "Darkslateblue") + theme_bw() +
  xlab("Day of Week") + ylab("Absent Hours") + scale_y_continuous(breaks = pretty_breaks(n=10)) +
  ggtitle("Absenteeism Analysis") + theme(text = element_text(size = 15))

ggplot(data, aes_string(x= data$Disciplinary.failure, y= data$Absenteeism.time.in.hours)) +
  geom_bar(stat = "identity", fill = "Darkslateblue") + theme_bw() +
  xlab("Disciplinary Failure") + ylab("Absent Hours") + scale_y_continuous(breaks = pretty_breaks(n=5)) +
  ggtitle("Absenteeism Analysis") + theme(text = element_text(size = 12))

ggplot(data, aes_string(x= data$Social.smoker, y= data$Absenteeism.time.in.hours)) +
  geom_bar(stat = "identity", fill = "Darkslateblue") + theme_bw() +
  xlab("Social Smoker") + ylab("Absent Hours") + scale_y_continuous(breaks = pretty_breaks(n=5)) +
  ggtitle("Absenteeism Analysis") + theme(text = element_text(size = 12))

ggplot(data, aes_string(x= data$Social.drinker, y= data$Absenteeism.time.in.hours)) +
  geom_bar(stat = "identity", fill = "Darkslateblue") + theme_bw() +
  xlab("Social Drinker") + ylab("Absent Hours") + scale_y_continuous(breaks = pretty_breaks(n=5)) +
  ggtitle("Absenteeism Analysis") + theme(text = element_text(size = 12))

ggplot(data, aes_string(x= data$Education, y= data$Absenteeism.time.in.hours)) +
  geom_bar(stat = "identity", fill = "Darkslateblue") + theme_bw() +
  xlab("Education") + ylab("Absent Hours") + scale_y_continuous(breaks = pretty_breaks(n=5)) +
  ggtitle("Absenteeism Analysis") + theme(text = element_text(size = 12))

ggplot(data, aes_string(x= data$Education, y= data$Work.load.per.day)) +
  geom_bar(stat = "identity", fill = "Darkslateblue") + theme_bw() +
  xlab("Education") + ylab("Work load per day") + scale_y_continuous(breaks = pretty_breaks(n=5)) +
  ggtitle("Absenteeism Analysis") + theme(text = element_text(size = 12))


