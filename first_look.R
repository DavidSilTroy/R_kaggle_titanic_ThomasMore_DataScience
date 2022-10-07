#Hello! just starting..

# Install required packages
# install.packages("readr")
# install.packages("dplyr")
# install.packages("ggplot2")

#Setup
rm(list=ls()) #Clear the Global Enviorement
setwd(getwd()) #Set the new directory to the script directory

# Load required libraries
library(readr)
library(dplyr)
library(ggplot2)

# Read the data set (uses readr)
column_types <- cols(
  Survived = col_factor(),
  Pclass = col_factor(ordered = TRUE),
  Sex = col_factor(),
  Embarked = col_factor()
)

train <- read_csv("./kaggle/titanic/train.csv", col_types = column_types)

# Rename the factors to be human readable (uses dplyr)
train$Survived <- recode_factor(train$Survived,
                                "0" = "No",
                                "1" = "Yes")

train$Pclass <- recode_factor(train$Pclass,
                              "1" = "1st",
                              "2" = "2nd",
                              "3" = "3rd",
                              .ordered = TRUE)

train$Embarked <- recode_factor(train$Embarked,
                                "C" = "Cherbourg",
                                "S" = "Southampton",
                                "Q" = "Queenstown")

# View 'train' tibble
train

# Plots and stuff (uses ggplot2)
ggplot(data = train) + geom_point(mapping = aes(
  x = Age,
  y = Fare,
  shape = Sex,
  color = Survived
))

