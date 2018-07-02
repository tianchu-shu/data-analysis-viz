library("randomForest")
library("dummies")

names(df)
#only keep the features we are going to use
features <- c("Post", "AA", "VR", "FY", "enter_on_duty", "inv_at_ddl")
df1 <- df[, (names(df) %in% features)]

#Select the columns for dummify
dcol <- c("Post", "AA")

#Create dummy vars
#df1 <- cbind(df, dummy(df$Q, sep = "_"))
df1 <- dummy.data.frame(df1, names=dcol, sep="_",  drop = TRUE)

#Drop the old vars
df1 <- df1[, !(names(df1) %in% dcol)]
names(df1)

#Make a target var based on fill_rate which is EOD/VR
#df1 <- transform(df1, target = ifelse((df$fill_rate >= 1) & (df$fill_rate <= 1.06), TRUE, fill_rate))
#df1 <- transform(df1, target = ifelse((df$fill_rate < 1) | (df$fill_rate > 1.06), FALSE, fill_rate))


#1.Time series
#Train/Test split, using 2016, 2017 to predict 2018
train <- df1[df1$FY != "FY2018",]
test <- df1[df1$FY == "FY2018",]

#Drop the FY column
train <- train[, -c(62)]
test <- test[, -c(62)]

#rf_model = randomForest(target~. , data = train, ntree =500, importance = TRUE)
train_x = train[, names(train) !='enter_on_duty']
train_y = train[, names(train) =='enter_on_duty']

rf_model = randomForest(x = train_x, y = train_y , ntree = 500, importance = TRUE)
#these are all the different things you can call from the model.
names(rf_model)

#view results
print(rf_model)
#importance of each predictor
varImpPlot(rf_model)

test_x = test[, names(test) != 'enter_on_duty']
test_y = test[, names(test) == 'enter_on_duty']

y_pred_rf = predict(rf_model , test_x)

#Test the accuracy
accuracy <- sum(((round(y_pred_rf) >= test_y -2) & (round(y_pred_rf) <= test_y + 2)))/length(test_y)
accuracy

plot(y_pred_rf, test_y, ylab="Observed EOD number", xlab="Predicted EOD number based on Random forest")

submit <- data.frame(VR = test_x$VR, Invited_at_ddl =  test_x$inv_at_ddl, actual_EOD = test_y, pred_EOD = round(y_pred_rf))
write.csv(submit, file = "rf.csv", row.names = FALSE)

simple_roc <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
}

# Classification Tree with rpart

# Create the tree.
#tree <- ctree(enter_on_duty ~ VR + Q + as.factor(AA) + Post, data=train)

#df$target <- cut(df$fill_rate,c(0,0.99,10),labels=c(0,1))
