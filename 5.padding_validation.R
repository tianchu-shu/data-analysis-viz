# Written by Tianchu Shu
# Last Updated August 16 2018

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

#only keep the features we are going to use
useful <- c("FY", "Q", "Sector", "Post", "AA", "VR", "freq", "inv_at_ddl", "enter_on_duty")
df<- df[, (names(df) %in% useful)]

#Create the padding rate column and the presence rate
df$padding_rate <- df$inv_at_ddl/df$VR
df$pres <- df$enter_on_duty/df$inv_at_ddl
df$fill_rate <- df$enter_on_duty / df$VR

#convert AA to a factor to indicate that rank should be treated as a categorical variable.
df$AA <- factor(df$AA)

#Create the ideal inv_at_ddl whose fill_rate is between 1-1.06
df <- transform(df, dif = ifelse(df$fill_rate >= 1 & df$fill_rate <= 1.06, 0, VR-enter_on_duty))
df$floor <- df$inv_at_ddl + df$dif 

#1.Time series
#Train/Test split, using 2016Q3-2017Q3 to predict 2017Q4-2018Q3
test <- df[df$FY == "FY2018",]
df1 <- subset(df, FY == "FY2017" & Q == "Quarter 4")
test <- rbind(df1, test)
train <- df[!rownames(df) %in% rownames(test),]

#Sanity check
nrow(train) + nrow(test) == nrow(df)

#Train the models.
test1<-test[test$Post != "Kyrgyz Republic",]
# drop the unseen level from test set: Kyrgyz Republic
ols1 <- lm(pres ~ padding_rate  +AA+Post, data=train)
test1$pres_pred = predict(ols1, test1)
test1$inv_at_ddl_pred = round(test1$VR / test1$pres_pred )
test1$EOD_pred = test1$inv_at_ddl_pred - test1$inv_at_ddl + test1$enter_on_duty

sum(((test1$EOD_pred >=test1$enter_on_duty -2) & (test1$EOD_pred <= test1$enter_on_duty  + 2)))/length(test1$enter_on_duty)
sum(((test1$EOD_pred >=test1$enter_on_duty -1) & (test1$EOD_pred <= test1$enter_on_duty  + 1)))/length(test1$enter_on_duty)
# 0.9190283
# 0.7287449

ols2 <- glm(pres ~ padding_rate+AA +Post+ Sector, data=train)
test$pres_pred = predict(ols2, test)
test$inv_at_ddl_pred = round( test$VR / test$pres_pred )
test$EOD_pred = test$inv_at_ddl_pred - test$inv_at_ddl + test$enter_on_duty

sum(((test$EOD_pred >=test$enter_on_duty -2) & (test$EOD_pred <= test$enter_on_duty  + 2)))/length(test$enter_on_duty)
sum(((test$EOD_pred >=test$enter_on_duty -1) & (test$EOD_pred <= test$enter_on_duty  + 1)))/length(test$enter_on_duty)
#Test the accuracy
#0.8995984
#0.7309237

########################################################################################
##################This part is how to apply this model to future data###################
#Finalize the model using all the data
final <- lm(pres ~ padding_rate  +AA , data=df, na.action=na.pass)
summary(final)
write.csv(as.data.frame(summary(final)$coef), file="padding_validation.csv")


library(readxl)
July2018 <- read_excel("~/July 2018 Departing JSR Validation Data Set.xlsm")
View(July2018)
#only keep the features we are going to use
useful <- c("Post", "Sector", "AA", "VR",  "INV at Deadline" , "Enter on Duty"  )
July2018<- July2018[, (names(July2018) %in% useful)]

colnames(July2018)[5] <- "inv_at_ddl"
colnames(July2018)[6] <- "enter_on_duty"

July2018$padding_rate <- July2018$inv_at_ddl/July2018$VR
July2018$pres <- July2018$enter_on_duty/July2018$inv_at_ddl
July2018$AA <- factor(July2018$AA)

July2018$pres_pred <- predict(final, July2018)
July2018$inv_at_ddl_pred = round( July2018$VR/July2018$pres_pred)
July2018$EOD_pred = July2018$inv_at_ddl_pred - July2018$inv_at_ddl + July2018$enter_on_duty
July2018$fill_rate = July2018$enter_on_duty/July2018$VR

July2018$fill_rate_pred = July2018$EOD_pred/July2018$VR
#July2018$dif = July2018$EOD_pred - July2018$enter_on_duty

sum(((July2018$EOD_pred >=July2018$enter_on_duty -2) & (July2018$EOD_pred <= July2018$enter_on_duty  + 2)))/length(July2018$enter_on_duty)
sum(((July2018$EOD_pred >=July2018$enter_on_duty -1) & (July2018$EOD_pred <= July2018$enter_on_duty  + 1)))/length(July2018$enter_on_duty)
# 0.9565217
# 0.7826087


#Write the result as csv for comparison
write.csv(July2018, file = "July2018_comp.csv", row.names = FALSE)

