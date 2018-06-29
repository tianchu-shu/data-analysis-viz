library("randomForest")
library("dummies")

names(df)
#only keep the features we are going to use
features <- c("Post", "Q", "AA", "VR", "FY", "enter_on_duty")
df1 <- df[, (names(df) %in% features)]

#Select the columns for dummify
dcol <- c("Post", "Q", "AA")

#Create dummy vars
#df1 <- cbind(df, dummy(df$FY, sep = "_"))
df1 <- cbind(df1, dummy.data.frame(df1, names=dcol, sep="_",  drop = TRUE))

#Drop the old vars
df1 <- df1[, !(names(df1) %in% dcol)]
names(df1)
df1 <- df1[, -c(65, 94, 95)]

#1.Time series
#Train/Test split, using 2016, 2017 to predict 2018
train <- df1[df1$FY != "FY2018",]
test <- df1[df1$FY == "FY2018",]

#Drop the FY column
train <- train[, -c(1)]
test <- test[, -c(1)]

#rf_model = randomForest(enter_on_duty~. , data = train, ntree =500, importance = TRUE)
train_x = train[, names(train) !='enter_on_duty']
train_y = train[, names(train) =='enter_on_duty']

rf_model = randomForest(x = train_x, y = train_y , ntree = 1000, importance = TRUE)
names(rf_model) #these are all the different things you can call from the model.

test_x = test[, names(test) !='enter_on_duty']
test_y = test[, names(test) =='enter_on_duty']

y_pred = predict(rf_model , test_x)

#Test the accuracy
accuracy <- sum((y_pred - test$VR) >= 0 & (y_pred - test$VR)/test$VR <= 0.06)/length(test$VR)
accuracy


simple_roc <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
}

# Classification Tree with rpart
