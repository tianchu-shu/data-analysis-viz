# OAE Data Mining Project
# Written by Tianchu Shu
#Last Updated July 2018

#Some of these libraries require the latest version of R
#Make sure your R Version is at least upgraded to 3.5.0 before running the script

#Candidate level data 
library(dplyr)
library(tidyverse)
library(readr)
library(caret)
library(data.table)
library("randomForest")
library("dummies")
library(class)
library(e1071)
library(rpart)
library(pROC)
df <- read_csv("C:/Users/tshu/Downloads/Candidate Attrition _05Jul_1401.CSV")

###########################################
########## Data Pre-processing ############
###########################################
#Convert the old IRT score to the new one
df$"IRT Score old"<- as.numeric(df$"IRT Score old")
df$"IRT Score old" <- round(df$"IRT Score old"*30/48)

colnames(df)[1] <- "OLD_FY"
#Combine the new and old FY, state and IRT score col
df$FY[!is.na(df$OLD_FY)] = df$OLD_FY[!is.na(df$OLD_FY)] 

df$State[!is.na(df$"HOR State")] = df$"HOR State"[!is.na(df$"HOR State")]
df$"IRT Score"[!is.na(df$"IRT Score old")] = df$"IRT Score old"[!is.na(df$"IRT Score old")]
names(df)
#Drop the cols: OLD_FY, HOR State, and summary
df<-df[, -c(1, 11, 29, 30, 35)]

#Changing col names
colnames(df)[13] <- "Hispanic_Latino"
colnames(df)[15] <- "Married_DP"
colnames(df)[16] <- "Serving_Spouse"
colnames(df)[18] <- "Have_you_been_arres"
colnames(df)[22] <- "Major"
colnames(df)[27] <- "Prev_Sub_PC_App"
colnames(df)[28] <- "PreviouslyServedAsPC"
colnames(df)[32] <- "Inv_Acpted_Date"

names(df) <- sub(" ", "_", names(df))
names(df)

#Extract the data of from FY2015 Q4 to FY2018 Q3
df1 <- subset(df, FY == "FY2018" & Q != "Quarter 4")
df2 <- subset(df, FY == "FY2015" & Q == "Quarter 4")
df3 <- subset(df, FY == "FY2017" |FY == "FY2016")
df <- rbind(df1, df2, df3)

#Diversity logic
df$Diversity[!is.na(df$Race)] <- df$Race[!is.na(df$Race)]
df$Diversity[df$'Hispanic_Latino' == "Hispanic or Latino"] <- "Hispanic or Latino"
df$race_count <- sapply(strsplit(df$Race, ","), length)
df[df$race_count >= 2, ]$Diversity = "Two or More Races"
sum(is.na(df$Diversity))

#Replace NA
df$State <- df$State %>% replace_na("No State")
df$Diversity <- df$Diversity %>% replace_na("unknown")
df$Have_you_been_arres <- df$Have_you_been_arres %>% replace_na("unknown")
df$`Married_DP` <- df$`Married_DP` %>% replace_na("No")
df$`Serving_Spouse` <- df$`Serving_Spouse` %>% replace_na("No")
df$`Language_Level` <- df$`Language_Level` %>% replace_na("None")
df$Med_Sort <- df$Med_Sort %>% replace_na("Nomination Cleared")
#Using the mode 22 to fill the NA for IRT_Score
df$IRT_Score <- df$IRT_Score %>% replace_na(22)

#Convert the date col to correct type
df$DOB <- as.Date(df$DOB, "%m/%d/%Y")
df$EOD_Date <- as.Date(df$EOD_Date, "%m/%d/%Y")
df$Inv_Acpted_Date <- as.Date(df$Inv_Acpted_Date, "%m/%d/%Y")

#Calculate the candidates' age at the time of accepted their invitation
x = as.numeric((df$Inv_Acpted_Date - df$DOB) / 365.25)
df$Age <- floor(x) 
df <- df[(df$Age >= 18),]
rm(df1, df2, df3, x)

#df$Sex <- ifelse(df$Sex =="Male", 1,0)

#Drop the old IRT_Scores
df<-df[, -c(29)]
#count the NA in specific column
sum(is.na(df$EOD_Date))

#Calulate the candidate attrition rate for FY15Q4 - FY18Q3
sum(is.na(df$EOD_Date))/length(df$EOD_Date)
#0.3878271 of the candidates who got invited ended up not enter on duty

#Created the dummy dependant variable EOD indicate the candidate EOD or not
df$EOD <- ifelse(is.na(df$EOD_Date), 0 ,1)

sum(df$EOD)/length(df$EOD)
#0.6121729 of the candidates EOD

#Catagorize the degree type
df <- as.data.frame(df)
df[df$Degree_Type %like% "Bachelor", ]$Degree_Type = "Bachelor"
df[df$Degree_Type %like% "Doctor", ]$Degree_Type = "Doctorate"
df[df$Degree_Type %like% "Master", ]$Degree_Type = "Master"
df[df$Degree_Type %like% "Licensed", ]$Degree_Type = "Master"
df[df$Degree_Type %like% "Associate", ]$Degree_Type = "Associate"
df$Degree_Type[is.na(df$Degree_Type)] = "No Degree"

#Matching the education level
df[df$Education_Level==2, ]$Education_Level = "High school graduate/GED"
df[df$Education_Level==3, ]$Education_Level = "Incomplete college study"
df[df$Education_Level==4, ]$Education_Level = "A.A. degree or equivalent"
df[df$Education_Level==5, ]$Education_Level = "Third year of college completed"
df[df$Education_Level==6, ]$Education_Level = "College graduate"
df[df$Education_Level==7, ]$Education_Level = "Graduate study"
df[df$Education_Level==8, ]$Education_Level = "Graduate degree"
df[df$Education_Level==9, ]$Education_Level = "Technical school graduate"
df[df$Education_Level==99, ]$Education_Level = "Other"

#Combine Med_sort
df[df$Med_Sort == 'Nomination Clear',]$Med_Sort ="Nomination Cleared"
df[df$Med_Sort=="Triage"| df$Med_Sort=="Nomination Medical Pending",]$Med_Sort = "Medical Pending"
df[df$Med_Sort=="No additional validation required, cleared for all countries"| df$Med_Sort=="No additional validation required, support required",]$Med_Sort = "Nomination Cleared"
df[df$Med_Sort=="Additional validation required, support required"| df$Med_Sort=="null",]$Med_Sort = "Nomination Validation Required"

#Merge "Yes" & "Yes_DP"
df[df$`Serving_Spouse`=="Yes_DP", ]$`Serving_Spouse` = "Yes"

#df$MI <- ifelse(df$MI =="NO", 0, 1)

#Bucket the IRT_Score
df$IRT <- cut(df$IRT_Score, c(-1,5,10,15,20,25,30))
summary(df$IRT)
df$Agebin <- cut(df$Age, c(15,25,30,35,50, 81))
summary(df$Agebin)

#only keep the features we are going to use
useful <- c("FY","Q", "Post", "Sector", "State", "Sex", "Diversity", "Married_DP", "Serving_Spouse", "Med_Sort", "Have_you_been_arres", "Degree_Type", "Education_Level", "Age", "Agebin", "IRT", "IRT_Score", "Language_Level", "EOD" )
df <- df[, (names(df) %in% useful)]

#######################################################
########## Machine Learning Testing begins ############
#######################################################
head(df)
features <- c("FY", "Q", "Post", "Sector", "State", "Sex", "Diversity", "Married_DP", "Serving_Spouse", "Med_Sort", "Have_you_been_arres", "Education_Level",  "Age", "IRT_Score", "Language_Level", "EOD" )

mni <- df[, (names(df) %in% features)]
mni <-as.data.frame(mni)
#Select the columns for dummify
dcol <- c("Post", "Sector", "State", "Sex", "Diversity", "Married_DP", "Serving_Spouse", "Language_Level",  "Med_Sort", "Have_you_been_arres", "Education_Level")

#Create dummy vars
#df1 <- cbind(mni, dummy(df$Q, sep = "_"))
mydata <- dummy.data.frame(mni, names=dcol, sep="_",  drop = TRUE)

#Drop the old vars
mydata <- mydata[, !(names(mydata) %in% dcol)]
names(mydata)

#Train/Test split, using 2015-2017 to predict 2018
dtest <- subset(mydata, FY == "FY2018" | FY == "FY2017" & Q == "Quarter 4")
dtrain <- mydata[!rownames(mydata) %in% rownames(dtest),]

#Drop the FY Q column
dtrain <- dtrain[, -c(1, 2)]
dtest <- dtest[, -c(1,2)]

#rf_model = randomForest(target~. , data = train, ntree =500, importance = TRUE)
train_x = dtrain[, names(dtrain) !='EOD']
train_y = dtrain[, names(dtrain) == 'EOD']
test_x = dtest[, names(dtest) != 'EOD']
test_y = dtest[, names(dtest) == 'EOD']

#### K-Nearest Neighbor #####
knn_pred <- knn(train = train_x, test = test_x,cl = train_y, k=20, prob=TRUE)
kp=attr(knn_pred,"prob") 
knnp <- ifelse(kp >=0.612, 1, 0)
result_k <- confusionMatrix(factor(knnp), factor(test_y), mode = "prec_recall", positive="1")
result_k <-as.matrix(result_k, what = "classes")
#Precision            0.6797020
#Recall               0.5991792
#F1                   0.6369056
# ROC area under the curve
auc(knnp,test_y)
#Area under the curve: 0.5684

#########################
#### Random Foreset #####
##########################
rf_model = randomForest(x = train_x, y = train_y , ntree = 100, importance = TRUE)
#these are all the different things you can call from the model.
#view results
print(rf_model)
#importance of each predictor
#varImpPlot(rf_model)

rf_pred = predict(rf_model , test_x)
rfp <- ifelse(rf_pred >=0.612, 1, 0)
result_rf <- confusionMatrix(factor(rfp), factor(test_y), mode = "prec_recall", positive="1")
result_rf
result_rf <-as.matrix(result_rf, what = "classes")
#Precision            0.6887817
#Recall               0.3124487
#F1                   0.4298890
# ROC area under the curve
auc(rfp, test_y)
#Area under the curve: 0.5496

###################
####### SVM #######
###################
svmodel <- svm(x = train_x, y = train_y)
svm_pred <- predict(svmodel, test_x)
svmp <- ifelse(svm_pred >=0.612, 1, 0)
result_svm <-confusionMatrix(factor(svmp), factor(test_y), mode = "prec_recall", positive="1")
result_svm <-as.matrix(result_svm, what = "classes")
#Precision            0.6479510
#Recall               0.5493844
#F1                   0.5946106
# ROC area under the curve
auc(svmp, test_y)
#Area under the curve: 0.5321


#################################################################################
#Train/Test split, using 2015, 2016, 2017 to predict 2018
test <- subset(df, FY == "FY2018" | FY == "FY2017" & Q == "Quarter 4")
train <- df[!rownames(mydata) %in% rownames(dtest),]

head(test)
#Drop the FY, Q columns
train <- train[, -c(1,2)]
test <- test[, -c(1,2)]

######################
#### Naive Bayes #####
######################
#Tried adding post and sector, didnt have no differences
nb <- naiveBayes(as.factor(EOD) ~  State + Sex + Married_DP + Serving_Spouse + Med_Sort + Have_you_been_arres+ Degree_Type + Language_Level + Agebin + IRT, data=train, laplace = 1, threshold=0.612, eps =1, subset, na.action = na.pass)
nb_pred <- predict(nb, test, type="class")
result_nb <-confusionMatrix(factor(nb_pred), factor(test$EOD), mode = "prec_recall", positive="1")
result_nb <-as.matrix(result_nb, what = "classes")
#Precision            0.63122864
#Recall               0.96005472
#F1                   0.76166703
# ROC area under the curve
auc(nb_pred, test$EOD)
#Area under the curve: 0.6134


##############################
#### Logistic Regression #####
##############################
mylogit <- lm(EOD ~ Sector+State + Sex + Married_DP + Serving_Spouse + Med_Sort + Have_you_been_arres+ Degree_Type + Language_Level + Age + IRT_Score, data=train)
yl = predict(mylogit, test)
ylp <- ifelse(yl >=0.612, 1, 0)
result_logit <- confusionMatrix(factor(ylp), factor(test$EOD), mode = "prec_recall", positive="1")
result_logit <-as.matrix(result_logit, what = "classes")
#Precision            0.7010919
#Recall               0.4216142
#F1                   0.5265676
# ROC area under the curve
auc(test$EOD, ylp)
#Area under the curve: 0.5658


########################
#### Decision Tree #####
########################
# Create the tree.
tree <- rpart(EOD ~ State + Sex + Married_DP + Serving_Spouse + Med_Sort + Have_you_been_arres+ Degree_Type + Language_Level + Age + IRT_Score,
              data = train,
             method="class")
# Plot the tree.
#fancyRpartPlot(tree)

dt_pred <- as.data.frame(predict(tree, test, type = "p"))
dtp <- ifelse(dt_pred$`1` >=0.612, 1, 0)
result_dt <- confusionMatrix(factor(dtp), factor(test$EOD), mode = "prec_recall", positive="1")
result_dt <-as.matrix(result_dt, what = "classes")
#Precision            0.6734835
#Recall               0.8019152
#F1                   0.7321094
# ROC area under the curve
auc(dtp, test$EOD)
#Area under the curve: 0.6059

#####################################################
########## Machine Learning Testing ends ############
#####################################################
ml_result <- cbind(result_k, result_svm, result_nb, result_logit, result_rf, result_dt)
colnames(ml_result) <- c("KNN", "SVM", "Naive_Bayes", "Logit", "Random_forest", "Decision_tree")
write.csv(ml_result, file = "ml_result.csv")



mni %>% 
      select(Agebin, IRT, Diversity, Sex, Med_Sort, Degree_Type, Language_Level) %>% 
      gather(metric, value) %>% 
      ggplot(aes(value, fill = metric)) + 
      geom_density(show.legend = FALSE) + 
      facet_wrap(~ metric, scales = "free")
