#Hello! just starting..

################################################################################
# Setup packages and settings
################################################################################
Sys.setenv(LANG = "en") # Set language to English
setwd(getwd()) # Set the working directory to the script directory
rm(list = ls()) # Clears the Global Env

# Install required packages
# install.packages("tidyverse")
# install.packages("ggcorrplot")

# Load required libraries
library(tidyverse) # Contains all tidyverse packages (ggplot2, dplyr, ...)
library(ggcorrplot)


################################################################################
# Read and import the data set
################################################################################

# Read the data set (uses readr)
column_types <- cols(
  Survived = col_factor(),
  Pclass = col_factor(include_na = TRUE, ordered = TRUE),
  Sex = col_factor(),
  Embarked = col_factor(include_na = TRUE, ordered = TRUE)
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
                              .default = "Unknown", # NA -> Unknown
                              .ordered = TRUE)

train$Embarked <- recode_factor(train$Embarked,
                                "S" = "Southampton (England)",
                                "C" = "Cherbourg (France)",
                                "Q" = "Queenstown (Ireland)",
                                .default = "Unknown", # NA -> Unknown
                                .ordered = TRUE)

################################################################################
# Filtering and cleaning
################################################################################
# Check for the number of NA's in each column
sanity_check <- function(my_df) {
  for (j in 1:ncol(my_df)) {
    print(paste(names(my_df[j]), ":", sum(is.na(my_df[, j]))))
  }
}

sanity_check(train)


# View 'train' tibble
train

################################################################################
# Adding useful columns
################################################################################
# Add a total Family size column
train <- mutate(train, FamilySize = SibSp + Parch)

# Group the cabin label into has cabin and has no cabin
train <- mutate(train, CabinGroups = ifelse(is.na(train$Cabin),
                                            "No cabin",
                                            "Cabin"))

tail(train)

################################################################################
# Plots and stuff (uses ggplot2)
################################################################################

train_numeric <- select(train, Age, SibSp, Parch, FamilySize, Fare)
train_numeric_corr <- cor(train_numeric, use = "complete.obs") # Use only non NA
ggcorrplot::ggcorrplot(train_numeric_corr)
