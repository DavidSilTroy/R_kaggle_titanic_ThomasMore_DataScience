#Hello! just starting..

#R --vanilla
#install.packages(c("randomForest","Rcpp", "readr","caret","dplyr"))

#Information get it from:
#https://www.kaggle.com/code/larsen0966/getting-started-with-titanic-using-r

#Loading the Packages
library(randomForest)
library(dplyr)
library(caret)
library(readr)

setwd(getwd()) #Set the new directory to the script directory
rm(list=ls()) #Clear the Global Enviorement

#Loading the Test file and reading the columns.
test_data <- read.csv("./kaggle/titanic/test.csv", stringsAsFactors = FALSE, header = TRUE)
head(test_data)

#Loading the Training file and reading the columns.
train_data <- read.csv("./kaggle/titanic/train.csv", stringsAsFactors = FALSE, header = TRUE)
head(train_data)

#Missing Values from the test_data
nrow(test_data)
sum(is.na(test_data))
sum(is.na(test_data$Age))


#Filling missing values for Age
median(train_data$Age, na.rm=TRUE)
median(test_data$Age, na.rm=TRUE)
train_data$Age  <- ifelse(is.na(train_data$Age), 28, train_data$Age)
test_data$Age  <- ifelse(is.na(test_data$Age), 27, test_data$Age)

#Identify missing Fare and fill missing value
subset(test_data, is.na(test_data$Fare))
thrd_cl_fr <- subset(test_data, c(test_data$Pclass==3, test_data$Embarked=="S"))
m_fare <- round(median(thrd_cl_fr$Fare, na.rm=TRUE),2)
m_fare
test_data$Fare <- ifelse(is.na(test_data$Fare), m_fare, test_data$Fare)


#Identify and fill missing Embark values
table(train_data$Embarked)
table(test_data$Embarked)

m_embarked <-subset(train_data, train_data$Embarked=="")
m_embarked

train_data[train_data$Embarked=="", "Embarked"] <- "S"


#Looking at the Categories of each column
str(test_data)
str(train_data)

#Changing certain colums to factors
train_data$Pclass <- as.factor(train_data$Pclass)
test_data$Pclass <- as.factor(test_data$Pclass)
train_data$Sex <- as.factor(train_data$Sex)
test_data$Sex <- as.factor(test_data$Sex)
train_data$Embarked <- as.factor(train_data$Embarked)
test_data$Embarked <- as.factor(test_data$Embarked)

#Making sure Survived is a facotr to do and Orgnaizational analysis and not a Regression Analysis.
train_data$Survived <- as.factor(train_data$Survived)

#Creating a Oclass ordinal Category in a new column 
train_data <- mutate(train_data, Oclass=Pclass)
train_data$Oclass <- gsub("1", "first", train_data$Oclass)
train_data$Oclass <- gsub("2", "second", train_data$Oclass)
train_data$Oclass <- gsub("3", "third", train_data$Oclass)
test_data <- mutate(test_data, Oclass=Pclass)
test_data$Oclass <- gsub("1", "first", test_data$Oclass)
test_data$Oclass <- gsub("2", "second", test_data$Oclass)
test_data$Oclass <- gsub("3", "third", test_data$Oclass)

train_data$Oclass <- factor(train_data$Oclass, order = TRUE, levels = c("first", "second", "third"))
test_data$Oclass <- factor(test_data$Oclass, order = TRUE, levels = c("first", "second", "third"))

str(test_data)
str(train_data)

women <- subset(train_data, train_data$Sex=="female")
count(women)
n_women <- count(women)

sur_women <- subset(women, women$Survived==1)
count(sur_women)
n_sur_women <- count(sur_women)

n_sur_women/n_women

men <- subset(train_data, train_data$Sex=="male")
count(men)
n_men <- count(men)

sur_men <- subset(men, men$Survived==1)
count(sur_men)
n_sur_men <- count(sur_men)

n_sur_men/n_men



# Example model
set.seed(123)  # for reproducibility
survived_equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived_formula <- as.formula(survived_equation)
titanic_model <- randomForest(formula = survived_formula, data = train_data, ntree =  500, mtry = 3, nodesize = .01 * nrow(train_data))

# Building Features
features_equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"

# Make a Predition
survived <- predict(titanic_model, newdata = test_data)
table(survived)

# Binding the Passenger ID
PassengerId <- test_data$PassengerId

output_df <- as.data.frame(PassengerId)
output_df$survived <- survived

