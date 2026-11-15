#Heart Disease Risk Prediction(DSCI 726)
#Samita Pandey

library(readxl)

#Import the Excel dataset into R
heart_data <- read_excel("C:/Users/samit/OneDrive/Desktop/DSCI726_Project/UCI_Heart_Disease_Cleveland.xlsx")

#Checking row and column
nrow(heart_data)
ncol(heart_data)

#Cleaning the data
heart_data_clean<-heart_data[!is.na(heart_data$ca)&!is.na(heart_data$thal),]#excluding rows with missing ca or thal

#Checking row and column on cleaned data
nrow(heart_data_clean)
colSums(is.na(heart_data_clean))

#Predicting disease vs no disease
heart_data_clean$disease<-ifelse(heart_data_clean$num==0, "No disease", "Disease")

table(heart_data_clean$disease)

#Convert categorical columns to factors
group_columns<-c("sex","cp","fbs","restecg","exang","slope","thal")
heart_data_clean[group_columns]<-lapply(heart_data_clean[group_columns],factor)
str(heart_data_clean)

#Setting levels
# "No disease" first so the model predicts the probability of "Disease"
heart_data_clean$disease<-factor(heart_data_clean$disease, levels=c("No disease","Disease"))
levels(heart_data_clean$disease)
table(heart_data_clean$disease)

#Training and testing split
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
library(caret)
set.seed(19)
train_rows<-createDataPartition(heart_data_clean$disease, p=0.7, list=FALSE)
train_data<-heart_data_clean[train_rows,]
test_data<-heart_data_clean[-train_rows,]
nrow(train_data)
nrow(test_data)
table(train_data$disease)
table(test_data$disease)

#Checking collinearity
cor(train_data[c("age","trestbps","chol","thalach","oldpeak","ca")])

#Logistic Regression
# num is excluded because the disease outcome was created from it
model<-glm(disease~.-num, data=train_data, family=binomial)
summary(model)

#Installing packages
if (!requireNamespace("car", quietly = TRUE)) 
install.packages("car")
library(car)

#Running VIF model
vif(model)

# Cumulative distribution of patient age
plot(ecdf(heart_data_clean$age),
     main = "Cumulative distribution of patient age",
     xlab = "Age (years)",
     ylab = "Proportion of patients at or below this age")

#Facts reading from the curve
median(heart_data_clean$age)
mean(heart_data_clean$age < 50)




# Scatterplot of ST depression (oldpeak) against maximum heart rate (thalach),
# colored by disease status
plot(heart_data_clean$thalach, heart_data_clean$oldpeak,
     col = ifelse(heart_data_clean$disease == "Disease", "red", "blue"),
     pch = 19,
     main = "ST depression vs. maximum heart rate by disease status",
     xlab = "Maximum heart rate (beats per minute)",
     ylab = "ST depression (oldpeak)")
legend("topright", legend = c("Disease", "No disease"),
       col = c("red", "blue"), pch = 19)


cor(heart_data_clean$thalach, heart_data_clean$oldpeak)


#Prediction on the test set
test_probabilities<-predict(model, newdata=test_data, type="response")
test_predictions<-ifelse(test_probabilities>0.5,"Disease","No disease")
test_predictions<-factor(test_predictions,levels=c("No disease", "Disease"))
table(test_predictions)

#Evaluation
logistic_results<-confusionMatrix(test_predictions, test_data$disease, positive="Disease")
logistic_results
# evaluate_model(): takes a model's predicted classes and the test data,
# and returns the confusion matrix with accuracy, sensitivity, and
# specificity, treating "Disease" as the positive class.

#Building reusable evaluation function
evaluate_model<-function(predicted_classes, test_data){
  evaluation_results<-confusionMatrix(predicted_classes,test_data$disease, positive="Disease")
  return(evaluation_results)
}

#Fit the decision tree
library(rpart)
tree_model<-rpart(disease~.-num,data=train_data,method="class")
tree_predictions<-predict(tree_model,newdata=test_data,type="class")
table(tree_predictions)
evaluate_model(tree_predictions,test_data)

#Random Forest
library(randomForest)
set.seed(21)
forest_model<-randomForest(disease~.-num,data=train_data)
forest_predictions<-predict(forest_model,newdata=test_data,type="response")
table(forest_predictions)
evaluate_model(forest_predictions,test_data)

#Chest pain against outcome
table(heart_data_clean$cp,heart_data_clean$disease)

#Share of each chest pain type with disease
prop.table(table(heart_data_clean$cp,heart_data_clean$disease),margin=1)

#Cross Validation 
cv_settings<-trainControl(method="cv",number=10)
# Same seed before each model so all three use the same 10 folds
set.seed(19)
cv_logistic<-train(disease~.-num,data=train_data,method="glm",family="binomial",trControl=cv_settings)
cv_logistic

set.seed(19)
cv_tree<-train(disease~.-num,data=train_data,trControl=cv_settings,method="rpart")
cv_tree

set.seed(19)
cv_forest<-train(disease~.-num,data=train_data,trControl=cv_settings,method="rf")
cv_forest

cv_comparision<-resamples(list(Logistic=cv_logistic,Tree=cv_tree,Forest=cv_forest))
summary(cv_comparision)

#Visuals
bwplot(cv_comparision,metric="Accuracy")

#ROC Step 1
library(pROC)
roc_logistic<-roc(response=test_data$disease,predictor=test_probabilities,levels=c("No disease","Disease"))

auc(roc_logistic)

#Tree's probabilities
tree_probabilities<-predict(tree_model,newdata=test_data,type="prob")[,"Disease"]
roc_tree<-roc(response=test_data$disease,predictor=tree_probabilities,levels=c("No disease","Disease"))
auc(roc_tree)

#Forest Probabilities
forest_probabilities<-predict(forest_model,newdata=test_data,type="prob")[,"Disease"]
roc_forest<-roc(response=test_data$disease,predictor=forest_probabilities,levels=c("No disease","Disease"))
auc(roc_forest)

#Chart for memo
varImpPlot(forest_model)

#ROC plot
plot(roc_logistic)
plot(roc_tree,add=TRUE,col="red")

#ROC forest
plot(roc_forest,add=TRUE,col="blue")
legend("bottomright",legend=c("Logistic","Tree","Forest"),col=c("black","red","blue"),lwd=2)


tree_pred <- predict(cv_tree, newdata = test_data)
evaluate_model(tree_pred, test_data)


forest_pred <- predict(cv_forest, newdata = test_data)
evaluate_model(forest_pred, test_data)


roc_tuned_tree<-roc(test_data$disease,tuned_tree_probabilities,levels=c("No disease","Disease"))

tuned_tree_probabilities<-predict(cv_tree,newdata=test_data,type="prob")[,"Disease"]

roc_tuned_tree<-roc(test_data$disease,tuned_tree_probabilities,levels=c("No disease","Disease"))
auc(roc_tuned_tree)



tuned_forest_probabilities<-predict(cv_forest,newdata=test_data,type="prob")[,"Disease"]
roc_tuned_forest<-roc(test_data$disease,tuned_forest_probabilities,levels=c("No disease","Disease"))
auc(roc_tuned_forest)
\

cv_tree$bestTune
cv_forest$bestTune
summary(cv_comparision)

prop.table(table(heart_data_clean$cp, 
heart_data_clean$disease), margin=1)

auc(roc_logistic)
logistic_results

cor(heart_data_clean[c("age","trestbps","chol","thalach","oldpeak","ca")])




# Summary statistics for the continuous variables (all 303 patients)
cont_vars <- c("age", "trestbps", "chol", "thalach", "oldpeak")
round(data.frame(
  Mean   = sapply(heart_data[cont_vars], mean),
  Median = sapply(heart_data[cont_vars], median),
  SD     = sapply(heart_data[cont_vars], sd),
  IQR    = sapply(heart_data[cont_vars], IQR)
), 2)


# Count outliers in each continuous variable by z-score (|z| > 3) and IQR (1.5 x IQR) rules
cont_vars <- c("age", "trestbps", "chol", "thalach", "oldpeak")
sapply(heart_data[cont_vars], function(x) {
  z <- (x - mean(x)) / sd(x)
  q <- quantile(x, c(0.25, 0.75))
  c(z_score = sum(abs(z) > 3),
    iqr     = sum(x < q[1] - 1.5 * IQR(x) | x > q[2] + 1.5 * IQR(x)))
})



# ROC for the tuned tree and forest (from caret cross-validation)
tuned_tree_probabilities <- predict(cv_tree, newdata=test_data, type="prob")[,"Disease"]
roc_tuned_tree <- roc(response=test_data$disease, predictor=tuned_tree_probabilities, levels=c("No disease","Disease"))

tuned_forest_probabilities <- predict(cv_forest, newdata=test_data, type="prob")[,"Disease"]
roc_tuned_forest <- roc(response=test_data$disease, predictor=tuned_forest_probabilities, levels=c("No disease","Disease"))

#ROC plot
plot(roc_logistic)
plot(roc_tuned_tree, add=TRUE, col="red")
plot(roc_tuned_forest, add=TRUE, col="blue")
legend


# Variable importance for the tuned random forest
varImp(cv_forest)

# Plot of the top 10 most important predictors
plot(varImp(cv_forest), top = 10)