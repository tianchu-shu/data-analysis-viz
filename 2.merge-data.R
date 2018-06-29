#Merge Data

#Load data for vds invitation info
library(caret)
library(readxl)
library(plyr)
library(dummies)
library(tidyverse)
library(klaR)

raw <- read_excel("C:/Users/tshu/Downloads/data.xls")
head(raw)

#count how many unique post(country) in the dataset
unique(raw$Post)

post_med <- read_excel("C:/Users/tshu/Downloads/post.xls")

#drop the first row of all countries and the last row of total
#only keep the post name and freq columns
post_med <- post_med[-c(1, 79), -c(2,3)]

#merge the two dataset by Post name
df <- merge(raw, post_med, by = "Post", all.data = TRUE)
df <- unique(df)

names(df)
colnames(df)[10] <- "inv_at_ddl"
colnames(df)[12] <- "enter_on_duty"

#filter the data by Enter on duty
df <- df[df$enter_on_duty > 0 ,]


#Create the fill rate column
df$fill_rate <- df$enter_on_duty/df$VR

#Make fill_rate a probabiliy 
df <- transform(df, fill_rate = ifelse(fill_rate > 1, 1, fill_rate))

summary(df)
#Select the columns for dummify
#dcol <- c("FY", "Q", "AA")

#Create dummy vars
#df1 <- cbind(df, dummy(df$FY, sep = "_"))
#df1 <- cbind(df, dummy.data.frame(df, names=dcol, sep="_",  drop = TRUE))

#Drop the old vars
#df1 <- df1[, !(names(df1) %in% dcol)]


#1.Time series
#Train/Test split, using 2016, 2017 to predict 2018
train <- df[df$FY != "FY2018",]
test <- df[df$FY == "FY2018",]

#Sanity check
nrow(train) + nrow(test) == nrow(df)

#Discretize the freq in five bins
df$bin <- cut(df$freq, breaks = seq(0.5, 1, by = .1), labels = 1:5)

#convert AA & bin to a factor to indicate that rank should be treated as a categorical variable.
df$AA <- factor(df$AA)
df$bin <- factor(df$bin)

#Train the model
ols <- lm(enter_on_duty ~ VR + Q + AA + Post, data=train)
y_pred = predict(ols, test)

#Test the accuracy
accuracy <- sum((y_pred - test$VR) >= 0 & (y_pred - test$VR)/test$VR <= 0.06)/length(test$VR)
accuracy

summary(ols)

library(ggplot2)
ggplot(test, aes(y = enter_on_duty, x = VR)) +
       geom_point(size = 2, col = "red") +
       geom_smooth(method = lm, se = TRUE) +
       theme(aspect.ratio = 0.80)


df %>% ggplot(aes(y = enter_on_duty, x = VR)) +
  geom_point(color= "blue", alpha = 0.3) +
  ggtitle("Enter on duty vs VR") +
  xlab("VR") +
  ylab("Enter on Duty") +
  theme(plot.title = element_text(color="darkred",
                                  size=18,hjust = 0.5),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))

library(corrplot)
correlations = cor(df)
corrplot(correlations, method="color")
