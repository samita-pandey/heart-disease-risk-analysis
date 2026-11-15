#Import the Excel dataset into R
heart_data <- read_excel("C:/Users/samit/OneDrive/Desktop/DSCI726_Project/UCI_Heart_Disease_Cleveland.xlsx")

#View the data
View(heart_data)

#Checking structure
str(heart_data)

#First checking central tendency and dispersion on one variable before (age) before running for all
#Calculating mean of age
mean(heart_data$age)
#Calculating median of age
median(heart_data$age)
#Frequency table of age for mode
table(heart_data$age)
#Standard deviation of age
sd(heart_data$age)
#IQR of age
IQR(heart_data$age)

#Now calculating for all continuous variable
#Mean for all variables
sapply(heart_data[c("age","trestbps","chol","thalach","oldpeak")],mean)
#Median for all
sapply(heart_data[c("age","trestbps","chol","thalach","oldpeak")],median)
#Standard deviation for all
sapply(heart_data[c("age","trestbps","chol","thalach","oldpeak")],sd)
#IQR for all
sapply(heart_data[c("age","trestbps","chol","thalach","oldpeak")],IQR)

#Frequency tables for all variables for mode
table(heart_data$trestbps)
table(heart_data$chol)
table(heart_data$thalach)
table(heart_data$oldpeak)

#Now frequency count for categorical variables
#Frequency count and percentage for sex
table(heart_data$sex)
prop.table(table(heart_data$sex))
#frequency count and percentage for cp
table(heart_data$cp)
prop.table(table(heart_data$cp))
#frequency count and percentage for fbs
table(heart_data$fbs)
prop.table(table(heart_data$fbs))
#frequency count and percentage for restecg
table(heart_data$restecg)
prop.table(table(heart_data$restecg))
#frequency and percentage for exang
table(heart_data$exang)
prop.table(table(heart_data$exang))
#Frequency count and percentage for slope
table(heart_data$slope)
prop.table(table(heart_data$slope))
#Frequency count and percentage for ca
table(heart_data$ca)
prop.table(table(heart_data$ca))
#Frequency count and percentage for thal
table(heart_data$thal)
prop.table(table(heart_data$thal))
#Frequency count and percentage for num
table(heart_data$num)
prop.table(table(heart_data$num))

#Charts
#Histograms for all
hist(heart_data$age)
hist(heart_data$trestbps)
hist(heart_data$chol)
hist(heart_data$thalach)
hist(heart_data$oldpeak)

#Boxplots for all
boxplot(heart_data$age)
boxplot(heart_data$trestbps)
boxplot(heart_data$chol)
boxplot(heart_data$thalach)
boxplot(heart_data$oldpeak)

#Starting outlier detection with chol and oldpeak,
#since both showed signs of extreme values in earlier histograms and boxplots.
#CHOL
#Converting to z-score
z_chol<-scale(heart_data$chol)
#Flagging with z score > 3 as outliers
heart_data$chol[abs(z_chol)>3]
#Calculating Q1,Q3, and IQR
Q1<-quantile(heart_data,0.25)
Q3<-quantile(heart_data$chol,0.75)
IQR_chol<-IQR(heart_data$chol)
#Calculating Upper and lower bounds
lower<-Q1-1.5*IQR_chol#(using 1.5*IQR rule)
upper<-Q3+1.5*IQR_chol
#Flagging chol outside the bounds
heart_data$chol[heart_data$chol<lower|heart_data$chol>upper]

#For oldpeak
#Converting to z-score
z_oldpeak<-scale(heart_data$olpeak)
#Flagging with z score > 3 as outliers
heart_data$oldpeak[abs(z_oldpeak)>3]
#Calculating Q1,Q3, and IQR
Q1<-quantile(heart_data,0.25)
Q3<-quantile(heart_data$oldpeak,0.75)
IQR_oldpeak<-IQR(heart_data$oldpeak)
#Calculating Upper and lower bounds
lower<-Q1-1.5*IQR_oldpeak#(using 1.5*IQR rule)
upper<-Q3+1.5*IQR_oldpeak
#Flagging oldpeak outside the bounds
heart_data$oldpeak[heart_data$oldpeak<lower|heart_data$oldpeak>upper]

#For age
#Converting to z-score
z_chol<-scale(heart_data$age)
#Flagging with z score > 3 as outliers
heart_data$age[abs(z_age)>3]
#Calculating Q1,Q3, and IQR
Q1<-quantile(heart_data,0.25)
Q3<-quantile(heart_data$age,0.75)
IQR_age<-IQR(heart_data$age)
#Calculating Upper and lower bounds
lower<-Q1-1.5*IQR_age#(using 1.5*IQR rule)
upper<-Q3+1.5*IQR_age
#Flagging age outside the bounds
heart_data$age[heart_data$age<lower|heart_data$age>upper]

#For trestbps
#Converting to z-score
z_chol<-scale(heart_data$trestbps)
#Flagging with z score > 3 as outliers
heart_data$trestbps[abs(z_trestbps)>3]
#Calculating Q1,Q3, and IQR
Q1<-quantile(heart_data,0.25)
Q3<-quantile(heart_data$trestbps,0.75)
IQR_chol<-IQR(heart_data$trestbps)
#Calculating Upper and lower bounds
lower<-Q1-1.5*IQR_trestbps#(using 1.5*IQR rule)
upper<-Q3+1.5*IQR_trestbps
#Flagging trestbps outside the bounds
heart_data$trestbps[heart_data$trestbps<lower|heart_data$trestbps>upper]

#For thalach
#Converting to z-score
z_chol<-scale(heart_data$thalach)
#Flagging with z score > 3 as outliers
heart_data$thalach[abs(z_thalach)>3]
#Calculating Q1,Q3, and IQR
Q1<-quantile(heart_data,0.25)
Q3<-quantile(heart_data$thalach,0.75)
IQR_thalach<-IQR(heart_data$thalach)
#Calculating Upper and lower bounds
lower<-Q1-1.5*IQR_thalach#(using 1.5*IQR rule)
upper<-Q3+1.5*IQR_thalach
#Flagging thalach outside the bounds
heart_data$thalach[heart_data$thalach<lower|heart_data$thalach>upper]

#CORRELATION ANALYSIS
#Correlation of variables
cor(heart_data[c("age","trestbps","chol","thalach","oldpeak","num")])#Between variables and outcomes
#Installing package
install.packages("corrplot")
#Load package
library(corrplot)
#Generating visual plot
corrplot(cor(heart_data[c("age","trestbps","chol","thalach","oldpeak","num")]))

#MISSING DATA
#Counting missing data per column
colSums(is.na(heart_data))
#Counting total rows with missing ca or thal
sum(is.na(heart_data$ca)|is.na(heart_data$thal))
#Creating cleaned dataset
heart_data_clean<-heart_data[!is.na(heart_data$ca)&!is.na(heart_data$thal),]#excluding rows with missing ca or thal
#Confirming new row after cleaning
nrow(heart_data_clean)



