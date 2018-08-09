#OAE Data Mining Project
#Written by Tianchu Shu
#Last Updated August 7 2018

#Some of these libraries require the latest version of R
#Make sure your R Version is at least upgraded to 3.5.0 before running the script

#Merge Data
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

#Combine the new and old FY col
raw %>% mutate(FY = coalesce("OLD FY", "FY")) %>%select(-c("OLD FY"))

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
#df <- transform(df, fill_rate = ifelse(df$fill_rate >= 1, TRUE, fill_rate))

#filter the data by fill_rate > 1
ideal <- df[df$fill_rate>= 1 ,]
summary(ideal)


#1.Time series
#Train/Test split, using 2016Q3-2017Q3 to predict 2017Q4-2018Q3
test <- df[df$FY == "FY2018",]
df1 <- subset(df, FY == "FY2017" & Q == "Quarter 4")
test <- rbind(df1, test)
train <- df[!rownames(df) %in% rownames(test),]

#Sanity check
nrow(train) + nrow(test) == nrow(df)

#Discretize the freq in five bins
#df$bin <- cut(df$freq, breaks = seq(0.5, 1, by = .1), labels = 1:5)

#convert AA & bin to a factor to indicate that rank should be treated as a categorical variable.
df$AA <- factor(df$AA)
#df$bin <- factor(df$bin)

#Train the models.
test1<-test[test$Post != "Kyrgyz Republic",]
#Error in model factor Post has new levels Kyrgyz Republic, drop the unseen level from test set
ols1 <- lm(inv_at_ddl~ VR  +AA + Post, data=train, na.action=na.pass)
y_pred = predict(ols1, test1)
accuracy1 <- sum(((round(y_pred) >= test1$inv_at_ddl -2) & (round(y_pred) <= test1$inv_at_ddl + 2)))/length(test1$inv_at_ddl)

ols2 <- lm(inv_at_ddl~ VR  +AA + freq, data=train)
y_pred = predict(ols2, test)

#mylogit <- glm(inv_at_ddl ~ VR + enter_on_duty + AA + Post, data=train, family = "binomial")
#yl = predict(mylogit, test)
#accuracy <- sum(((yl >= test$fill_rate -0.2) & (yl <= test$fill_rate + 0.2)))/length(test$enter_on_duty)


#Test the accuracy
accuracy2 <- sum(((round(y_pred) >= test$inv_at_ddl -2) & (round(y_pred) <= test$inv_at_ddl + 2)))/length(test$inv_at_ddl)
accuracy2
#0.8218623 is better than 0.8072289 which is using freq instead of Post effect

summary(ols2)

plot(y_pred, test$inv_at_ddl, ylab="Observed inv_at_ddl number", xlab="Predicted inv_at_ddl")

pred_test <- test %>%
  add_column(yhat = y_pred) %>%
  # Compute the residuals
  mutate(.resid = test$inv_at_ddl - yhat)
pred_test

# Plot actual v residual values
library(ggplot2)
pred_test %>%
  ggplot(aes(inv_at_ddl, .resid)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  stat_smooth(method = "loess") +
  theme_minimal()


library(ggplot2)
ggplot(pred_test, aes(x = enter_on_duty,y = yhat)) +
  geom_point(size = 2, col = "red") +
  geom_smooth(method = lm, se = TRUE) +
  theme(aspect.ratio = 0.80)

ggplot(pred_test, aes(x = VR, y = inv_at_ddl)) +
  geom_point(size = 2, col = "red") +
  geom_smooth(method = lm, se = TRUE) +
  theme(aspect.ratio = 0.80)


pred_test %>% ggplot(aes(x = inv_at_ddl , y = yhat)) +
  geom_point(color= "blue", alpha = 0.3) +
  ggtitle("inv_at_ddl vs VR") +
  xlab("VR") +
  ylab("inv_at_ddl") +
  theme(plot.title = element_text(color="darkred",
                                  size=18,hjust = 0.5),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))



#Finalize the model using all the data
fit <- lm(inv_at_ddl~ VR + enter_on_duty + AA + Post, data=df)
summary(fit)
write.csv(as.data.frame(summary(fit)$coef), file="lm_coef.csv")

#submit <- data.frame(VR = data$VR, Invited_at_ddl =  data$inv_at_ddl, actual_EOD = test_y, pred_EOD = round(y_pred_rf))
write.csv(submit, file = "rf.csv", row.names = FALSE)

write.csv(df, file="df.csv")
