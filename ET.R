#OAE Data Mining Project
#Written by Tianchu Shu
#Last Updated August 2018

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
library(readxl)
library(stringr)
library(stringi)

ET <- read_excel("C:/Users/tshu/Downloads/ET data FY 15 to 2018.07.26.xlsx")
lookup <- read_excel("C:/Users/tshu/Downloads/VOL ID CRN look up table.xlsx")
lookup2 <- read_excel("C:/Users/tshu/Downloads/VOL ID CRN look up table FY 13-14.xlsx")
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

#Extract the data of from FY2013 to FY2016 Q4
df1 <- subset(df, FY == "FY2016" & Q != "Quarter 4")
df2 <- subset(df, FY == "FY2015" | FY == "FY2014" | FY == "FY2013")

etdata <- rbind(df1, df2)
df <- etdata

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

#Merge "Yes" & "Yes_DP"
df[df$`Serving_Spouse`=="Yes_DP", ]$`Serving_Spouse` = "Yes"

#df$MI <- ifelse(df$MI =="NO", 0, 1)

#Bucket the age
df$Agebin <- cut(df$Age, c(15,25,30,35,50,81))
summary(df$Agebin)

#only keep the features we are going to use
useful <- c("FY","Q", "CRN", "Post", "Sector", "Sex", "Diversity", "Have_you_been_arres", "Degree_Type", "Age", "Agebin" )
df <- df[, (names(df) %in% useful)]
df <- unique(df)

#Get rid of the number in post name
ET$Post <- str_extract(ET$Post, "[A-Z]+")
ET$Post <- tolower(ET$Post)
ET$Post <- stri_trans_totitle(ET$Post)
sum(ET$AS)/length(ET$AS)
#0.2037412 is Admin Sep percentage of the ET Data since FY15
#220 are ET because of Admin Sep

ET$ETR <- ifelse(ET$"Assignment Status" =="ET-Resignation", 1,0)
sum(ET$ETR)/length(ET$ETR)
#0.201714 is ET-Resignation, 2189

key <- rbind(lookup, lookup2)
key <- key[, -c(1, 2, 3)]
m <- merge(ET, key, by.x = "Vol Id", by.y = "Volunteer ID")
et <- merge(m, df, by.x = "Candidate ID", by.y = "CRN", all.x = TRUE)
et <- merge(m, df, by.x = "Candidate ID", by.y = "CRN")
#drop the duplicates
et <- et[!rev(duplicated(rev(et$"Vol Id"))),]

AS <- subset(ET, `Resignation Reason Primary`=="Resig in lieu of Admin Sep" | `Assignment Status`=="ET-Administrative Separation")
as <- subset(et, `Resignation Reason Primary`=="Resig in lieu of Admin Sep" | `Assignment Status`=="ET-Administrative Separation")
#220 are ET because of Admin Sep

write.csv(table(AS$Post), file ="ass.csv")

#Data-viz for ET-resignation
library(ggplot2)
library(ggvis)
library(tidyverse)
library(reshape2)

ggplot(as, aes(Age, IRT_Score)) +
  geom_point(aes(color = FY)) +
  geom_smooth(se = FALSE) +
  labs(title = "")


ggplot(data = melt(as), mapping = aes(x = value)) + 
  geom_histogram(bins = 30) + facet_wrap(~variable, scales = 'free_x')


ggplot(data = as) + geom_point(mapping = aes(x=Diversity, y=Degree_Type, color=Age))

ggplot(data = as) + 
  + geom_bar(mapping = aes(x = Diversity, fill = Degree_Type), 
             position="dodge")

##
bar <- ggplot(data = as) + 
  geom_bar(mapping = aes(x = Agebin, fill = Diversity), 
           show.legend = FALSE,
           width = 1)  
theme(aspect.ratio = 1) + labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()

##
ggplot(as, aes(Agebin, Diversity)) +
  geom_point(aes(colour = Degree_Type))


as %>% ggvis(~EOD, ~Sector, fill = ~Agebin) %>% layer_points()
as %>% ggvis(~fill_rate, ~freq, fill = ~Sector) %>% layer_points()

#######################################################
########## Machine Learning Testing begins ############
#######################################################
head(et)
features <- c("FY", "Q", "Sector", "Sex", "Post.y", "Diversity",  "Have_you_been_arres", "Education_Level",  "Agebin", "ETR" )

mni <- et[, (names(et) %in% features)]
mni <-as.data.frame(mni)
#Select the columns for dummify
dcol <- c("Agebin", "Sector", "Sex", "Post.y","Diversity", "Married_DP", "Serving_Spouse", "Language_Level",  "Med_Sort", "Have_you_been_arres", "Education_Level")

#Create dummy vars
#df1 <- cbind(mni, dummy(df$Q, sep = "_"))
mydata <- dummy.data.frame(mni, names=dcol, sep="_",  drop = TRUE)

#Drop the old vars
mydata <- mydata[, !(names(mydata) %in% dcol)]
names(mydata)

#Train/Test split, using 2013-2015 to predict 2016
dtest <- subset(mydata, FY == "FY2016" | FY == "FY2015" & Q == "Quarter 4")
dtrain <- mydata[!rownames(mydata) %in% rownames(dtest),]

#Drop the FY Q column
dtrain <- dtrain[, -c(2, 3)]
dtest <- dtest[, -c(2,3)]
train_x = dtrain[, names(dtrain) !='ETR']
train_y = dtrain[, names(dtrain) == 'ETR']
test_x = dtest[, names(dtest) != 'ETR']
test_y = dtest[, names(dtest) == 'ETR']

sum(dtrain$ETR)/length(dtrain$ETR)
# 0.1070144 of the candidates in the trainset ET-resignation
# 0.1360176 for the whole et dataset

#WARNING: SVM, Random forest and KNN will take longer time than other models to run

#### K-Nearest Neighbor #####
start_time <- Sys.time()
knn_pred <- knn(train = train_x, test = test_x,cl = train_y, k=20, prob=TRUE)
kp=attr(knn_pred, "prob") 
end_time <- Sys.time()
#Knn running time: 3.776 secs
end_time - start_time

knnp <- ifelse(kp >=0.85, 1, 0)
result_k <- confusionMatrix(factor(knnp), factor(test_y), mode = "prec_recall", positive="1")
result_k
result_k <-as.matrix(result_k, what = "classes")
#Precision : 0.1821          
#Recall :    0.7969          
#F1 :        0.2965
# ROC area under the curve
auc(knnp,test_y)
#Area under the curve: 0.4761

#########################
#### Random Foreset #####
##########################
start_time <- Sys.time()
rf_model = randomForest(x = train_x, y = train_y , ntree = 100, importance = TRUE)
rf_pred = predict(rf_model , test_x)
end_time <- Sys.time()
#Random Forest running time:  3.244388 mins
end_time - start_time

#view results
print(rf_model)
#importance of each predictor
#varImpPlot(rf_model)

rfp <- ifelse(rf_pred <=0.1, 0, 1)
result_rf <- confusionMatrix(factor(rfp), factor(test_y), mode = "prec_recall", positive="1")
result_rf
result_rf <-as.matrix(result_rf, what = "classes")
#Precision : 0.2033          
#Recall :    0.3532          
#F1 :        0.2580
# ROC area under the curve
auc(rfp, test_y)
#Area under the curve: 0.5098

###################
####### SVM #######
###################
start_time <- Sys.time()
svmodel <- svm(x = train_x, y = train_y)
svm_pred <- predict(svmodel, test_x)
end_time <- Sys.time()
#SVM running time: 2.987182 mins
end_time - start_time

svmp <- ifelse(svm_pred <=0.1, 0, 1)
result_svm <-confusionMatrix(factor(svmp), factor(test_y), mode = "prec_recall", positive="1")
result_svm <-as.matrix(result_svm, what = "classes")
#Precision : 0.1902        
#Recall :    0.3620         
#F1 :        0.2494
# ROC area under the curve
auc(svmp, test_y)
#Area under the curve: 0.5001


#################################################################################
#Train/Test split, using 2013, 2014, 2015 to predict 2016
test <- subset(mni, FY == "FY2016" | FY == "FY2015" & Q == "Quarter 4")
train <- mni[!rownames(mni) %in% rownames(dtest),]

head(test)
#Drop the FY, Q columns
train <- train[, -c(3,2)]
test <- test[, -c(3,2)]

######################
#### Naive Bayes #####
######################
#Tried adding post and sector, didnt have no differences
start_time <- Sys.time()
nb <- naiveBayes(as.factor(ETR) ~ Post.y + Sector + Sex + Have_you_been_arres + Education_Level + Diversity + Agebin, data=train, laplace = 1, threshold=0.5, eps =1, subset, na.action = na.pass)
nb_pred <- predict(nb, test, type="class")
end_time <- Sys.time()
#Naive Bayes running time:  0.829 secs
end_time - start_time
result_nb <-confusionMatrix(factor(nb_pred), factor(test$ETR), mode = "prec_recall", positive="1")
result_nb
result_nb <-as.matrix(result_nb, what = "classes")
#Precision : 0.2812         
#Recall :    0.040          
#F1 :        0.070
# ROC area under the curve
auc(nb_pred, test$ETR)
#Area under the curve: 0.5468

##############################
#### Logistic Regression #####
##############################
test1<-test[test$Post.y != "OLD FIELD Micronesia" & test$Post.y != "Timor-Leste" & test$Post.y != "The Peoples Republic of China",]
start_time <- Sys.time()
mylogit <- lm(ETR ~ Post.y + Sector + Sex + Have_you_been_arres + Education_Level + Diversity + Agebin, data=train)
yl = predict(mylogit, test1)
end_time <- Sys.time()
#Logistic Regression running time: 0.2440238 secs
end_time - start_time
ylp <- ifelse(yl >=0.13, 1, 0)
result_logit <- confusionMatrix(factor(ylp), factor(test1$ETR), mode = "prec_recall", positive="1")
result_logit <-as.matrix(result_logit, what = "classes")
result_logit
#Precision : 0.2425         
#Recall :    0.2604          
#F1 :        0.2511

# ROC area under the curve
auc(test1$ETR, ylp)
#Area under the curve: 0.5344


########################
#### Decision Tree #####
########################
# Create the tree
start_time <- Sys.time()
tree <- rpart(ETR ~ Post.y + Sector + Sex + Have_you_been_arres + Education_Level + Diversity + Agebin,
              data = train,
             method="class")
#Plot the tree.
rpart.plot(tree)

dt_pred <- as.data.frame(predict(tree, test1, type = "p"))
end_time <- Sys.time()
#Decision Tree running time: 0.854085 secs
end_time - start_time
dtp <- ifelse(dt_pred$`1` >=0.87, 1, 0)
result_dt <- confusionMatrix(factor(dtp), factor(test$EOD), mode = "prec_recall", positive="1")
result_dt
result_dt <-as.matrix(result_dt, what = "classes")
#Precision : 0.6735          
#Recall :    0.8019          
#F1 :        0.7321

#ROC area under the curve
auc(dtp, test$EOD)
#Area under the curve: 0.6059

#####################################################
########## Machine Learning Testing ends ############
#####################################################
ml_result <- cbind(result_k, result_rf, result_svm, result_nb, result_logit,result_dt)
colnames(ml_result) <- c("KNN", "Random_forest","SVM", "Naive_Bayes", "Logit", "Decision_tree")
write.csv(ml_result, file = "result.csv")



mni %>% 
      select(Agebin, IRT, Diversity, Sex, Med_Sort, Degree_Type, Language_Level) %>% 
      gather(metric, value) %>% 
      ggplot(aes(value, fill = metric)) + 
      geom_density(show.legend = FALSE) + 
      facet_wrap(~ metric, scales = "free")
