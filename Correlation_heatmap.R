#Hello! just starting..
Sys.setenv(LANG = "en")


# Install required packages
# install.packages("readr")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("reshape2")

# Load required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(ggcorrplot)


# Read the data set (uses readr)
column_types <- cols(
  Survived = col_factor(),
  Pclass = col_factor(ordered = TRUE),
  Sex = col_factor(),
  Embarked = col_factor()
)
setwd(getwd()) #Set the new directory to the script directory
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
ggcorrplot::ggcorrplot(cor(train))
