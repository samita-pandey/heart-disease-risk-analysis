# Prescriptive Analysis of Heart Disease Patients
# Purpose:To use predictions to help prioritize which patients should receive 
#        additional screening or follow-up when health care resources are limited

#Loading two libraries
library(readxl)
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
library(caret)

#Location of the heart disease data file
data_path<-"C:/Users/samit/OneDrive/Desktop/DSCI726_Project/UCI_Heart_Disease_Cleveland.xlsx"
heart_data<-read_excel(data_path)

#Rows 
nrow(heart_data)

#Cleaning the data
heart_data_clean<-heart_data[!is.na(heart_data$ca)&!is.na(heart_data$thal),]#excluding rows with missing ca or thal

nrow(heart_data_clean)#Checking rows after cleaning

#Predicting disease vs no disease
heart_data_clean$disease<-ifelse(heart_data_clean$num==0, "No disease", "Disease")
table(heart_data_clean$disease)

#Converting categorical into factors
group_columns<-c("sex","cp","fbs","restecg","exang","slope","thal")
heart_data_clean[group_columns]<-lapply(heart_data_clean[group_columns],factor)
str(heart_data_clean)

# "No disease" first so the model predicts the probability of "Disease"
heart_data_clean$disease<-factor(heart_data_clean$disease, levels=c("No disease","Disease"))
levels(heart_data_clean$disease)
table(heart_data_clean$disease)

#Training and split data
set.seed(19)
train_rows<-createDataPartition(heart_data_clean$disease, p=0.7, list=FALSE)
train_data<-heart_data_clean[train_rows,]
test_data<-heart_data_clean[-train_rows,]

nrow(train_data)
nrow(test_data)

table(test_data$disease)

#intake-variables vector
intake_variables<-c("age","sex","cp","trestbps","chol","fbs","restecg")

#Formula:Building model formula
disease_formula<-reformulate(intake_variables,response="disease")

#cv_settings
cv_settings<-trainControl(method="cv",number=10)

set.seed(19)

#Model line
intake_model<-train(disease_formula,data=train_data,method="glm",family="binomial",trControl=cv_settings)

intake_model

#Getting each test patient's predicted risk
patient_risk<-predict(intake_model,newdata=test_data,type="prob")[,"Disease"]

length(patient_risk)

summary(patient_risk)

#Building a data frame
patients<-data.frame(patient_id=1:89,risk=patient_risk,has_disease=ifelse(test_data$disease=="Disease",1,0))

head(patients)

table(test_data$restecg)

table(test_data$cp)

#Adding cost column to the patients table
patients$cost<-ifelse(test_data$cp=="4",400,150)

table(patients$cost)

#Loading IpSolve
if (!requireNamespace("lpSolve", quietly = TRUE)) install.packages("lpSolve")
library(lpSolve)

#Setting up pieces the optimizer needs
objective_values<-patients$risk
length(objective_values)

#Constraints (row 1 is slow limit and row 2 is the cost)
constraint_matrix<-rbind(rep(1,89),patients$cost)
dim(constraint_matrix)

#Limits in two objects
constraint_directions<-c("<=","<=")
constraint_limits<-c(35,9000)

#Calling lp()
optimal_plan<-lp(direction="max",objective.in=objective_values,const.mat=constraint_matrix,const.dir=constraint_directions,const.rhs=constraint_limits,all.bin=TRUE)
optimal_plan$status
optimal_plan$objval

#looking at who the plan scheduled
patients$scheduled <- optimal_plan$solution

#How many slots the plans are using
sum(patients$scheduled)

#How much money is being used
sum(patients$scheduled * patients$cost)

#Checking out our real outcomes
sum(patients$scheduled*patients$has_disease)

#Conditions cause of limits
patients$fcfs<-as.integer(patients$patient_id<=35&cumsum(patients$cost)<=9000)

sum(patients$fcfs)
sum(patients$fcfs*patients$cost)

#Measuring what each plan found
sum(patients$fcfs * patients$risk)
sum(patients$fcfs * patients$has_disease)


#Calling function for different inputs
solve_plan<-function(slots,budget){
  plan<-lp(direction="max",objective.in=objective_values,const.mat=constraint_matrix,const.dir=constraint_directions,const.rhs=c(slots,budget),all.bin=TRUE)
  return(plan$objval)
}

solve_plan(35, 9000)

#Sensitivity analysis on budget
budget_values<-seq(from=5000,to=14000,by=500)
budget_values

#Printing the results
cases_by_budget<-sapply(budget_values,function(budget)solve_plan(35,budget))
cases_by_budget

#FCFS Function
fcfs_cases <- function(slots, budget) {
  scheduled <- as.integer(patients$patient_id <= slots & cumsum(patients$cost) <= budget)
  return(sum(scheduled * patients$risk))
}
fcfs_cases(35,9000)

#Running FCFS for every budget
fcfs_by_budget <- sapply(budget_values, function(budget) fcfs_cases(35, budget))
fcfs_by_budget

#Optimized line
plot(budget_values, cases_by_budget, type = "o", xlab = "Weekly budget", ylab = "expected cases found", main = "Expected disease cases found by weekly budget",ylim=range(c(cases_by_budget, fcfs_by_budget)))
lines(budget_values, fcfs_by_budget, type = "o", col = "red")
legend("bottomright", legend = c("Optimized", "First-come-first-served"), col = c("black", "red"), lwd = 2)


#Sensitivity analysis on slots
slot_values<-seq(from=10,to=50,by=5)
slot_values

#Printing the results
cases_by_slots<-sapply(slot_values,function(slots)solve_plan(slots, 9000))
cases_by_slots

#FCFS Function
fcfs_cases <- function(slots, budget) {
  scheduled <- as.integer(patients$patient_id <= slots & cumsum(patients$cost) <= budget)
  return(sum(scheduled * patients$risk))
}
fcfs_cases(35,9000)

#Running FCFS for every number of slots
fcfs_by_slots <- sapply(slot_values, function(slots) fcfs_cases(slots, 9000))
fcfs_by_slots

#Optimized line
plot(slot_values, cases_by_slots, type = "o", xlab = "Weekly appointment slots", ylab = "expected cases found", main = "Expected disease cases found by weekly slots",ylim=range(c(cases_by_slots, fcfs_by_slots)))
lines(slot_values, fcfs_by_slots, type = "o", col = "red")
legend("bottomright", legend = c("Optimized", "First-come-first-served"), col = c("black", "red"), lwd = 2)

#How many expensive patients
sum(patients$scheduled * (patients$cost == 400))
sum(patients$fcfs * (patients$cost == 400))

#Average predicted risk of the patients
sum(patients$scheduled * patients$risk) / sum(patients$scheduled)
sum(patients$fcfs * patients$risk) / sum(patients$fcfs)

cor(patients$patient_id, patients$risk)


# Row positions of patients, sorted from highest to lowest predicted risk
risk_order <- order(patients$risk, decreasing = TRUE)
head(risk_order)




# Starting values for the greedy plan: nobody scheduled, no money or slots used
patients$greedy <- 0
spent <- 0
used <- 0


# Greedy plan: go through patients from highest to lowest risk and schedule
# each one if a slot is free and their cost still fits in the budget
for (i in risk_order) {
  if (used < 35 && spent + patients$cost[i] <= 9000) {
    patients$greedy[i] <- 1
    spent <- spent + patients$cost[i]
    used <- used + 1
  }
}


sum(patients$greedy)                    # patients scheduled
sum(patients$greedy * patients$cost)    # money spent (should be <= 9000)
sum(patients$greedy * patients$risk)    # expected cases found
sum(patients$greedy * patients$has_disease)


# Average predicted risk of the patients the greedy plan scheduled
mean(patients$risk[patients$greedy == 1])

# Extended-tier ($400) patients scheduled
sum(patients$greedy * (patients$cost == 400))

# Basic-tier ($150) patients scheduled
sum(patients$greedy * (patients$cost == 150))


#ROC plot
plot(roc_logistic)
plot(roc_tuned_tree, add=TRUE, col="red")
plot(roc_tuned_forest, add=TRUE, col="blue")
legend("bottomright",
       legend=c("Logistic (AUC = 0.908)", "Tuned tree (AUC = 0.774)", "Tuned forest (AUC = 0.907)"),
       col=c("black","red","blue"), lwd=2)
