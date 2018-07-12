# OAE Data Mining Project
# Written by Tianchu Shu
# Last Updated June 2018

#Load data for vds invitation info
library(caret)
library(readxl)
library(plyr)
library(dummies)
library(tidyverse)
library(klaR)
library(tidyquant)
library(timetk)
library(broom)

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

#filter the na for inv_at_ddl
df <- df %>% drop_na(inv_at_ddl)

#Create the fill rate column
df$fill_rate <- df$enter_on_duty/df$VR

summary(df)

#1.Time series
#Train/Test split, using 2016, 2017 to predict 2018
train <- df[df$FY != "FY2018",]
test <- df[df$FY == "FY2018",]

#Sanity check
nrow(train) + nrow(test) == nrow(df)

#Discretize the freq in five bins
#df$bin <- cut(df$freq, breaks = seq(0.5, 1, by = .1), labels = 1:5)

#convert AA & bin to a factor to indicate that rank should be treated as a categorical variable.
df$AA <- factor(df$AA)
#df$bin <- factor(df$bin)

#Train the modells.
ols <- lm(enter_on_duty ~ VR + inv_at_ddl + AA + Post, data=train)
y_pred = predict(ols, test)

#Test the accuracy
accuracy <- sum(((round(y_pred) >= test$enter_on_duty -2) & (round(y_pred) <= test$enter_on_duty + 2)))/length(test$enter_on_duty)
accuracy

summary(ols)
plot(y_pred, test$enter_on_duty, ylab="Observed EOD number", xlab="Predicted EOD number based on ols")

pred_test <- test %>%
  add_column(yhat = y_pred) %>%
  # Compute the residuals
  mutate(.resid = test$enter_on_duty - yhat)
pred_test

# Plot actual v residual values
library(ggplot2)
pred_test %>%
  ggplot(aes(enter_on_duty, .resid)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  stat_smooth(method = "loess") +
  theme_minimal()


library(ggplot2)
ggplot(pred_test, aes(x = enter_on_duty,y = yhat)) +
       geom_point(size = 2, col = "red") +
       geom_smooth(method = lm, se = TRUE) +
       theme(aspect.ratio = 0.80)


pred_test %>% ggplot(aes(x = enter_on_duty , y = yhat)) +
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



#Finalize the model using all the data
fit <- lm(enter_on_duty ~ VR + inv_at_ddl + AA + Post, data=df)
summary(fit)
write.csv(as.data.frame(summary(fit)$coef), file="lm_coef.csv")

write.csv(df, file="df.csv")
