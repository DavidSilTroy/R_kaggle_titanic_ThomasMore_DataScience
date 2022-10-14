#Hello! just starting..

################################################################################
# Setup packages and settings
################################################################################
Sys.setenv(LANG = "en") # Set language to English
setwd(getwd()) # Set the working directory to the script directory
rm(list = ls()) # Clears the Global Env

# Install required packages
# install.packages("tidyverse")

# Load required libraries
library(tidyverse) # Contains all tidyverse packages (ggplot2, dplyr, ...)


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
tail(train)

################################################################################
# Plots and stuff (uses ggplot2)
################################################################################

# Embarked and Fare prices
ggplot(data = train, mapping = aes(x = Embarked, y = Fare))+
  geom_boxplot()

# Fare bigger than 500
FareEnough = filter(train, Fare > 500)

# Pclass, Family size and Survived bigger than fare 500
ggplot(data = FareEnough, mapping = aes(x = Pclass, y=FamilySize))+
  geom_point(aes(shape=Survived))

# Count of family size who paid over 500
ggplot(data = FareEnough, mapping = aes(x=FamilySize))+
  geom_histogram()

# Male Female survival percentage
ggplot(data = train, mapping = aes(x = Sex, fill = Survived)) +
  geom_bar(position = "fill")


# Male Pclass survival percentage
ggplot(data = train, mapping = aes(x = Pclass, fill = Survived)) +
  geom_bar(position = "fill") 


# Male FamilySize survival percentage
ggplot(data = train, mapping = aes(x = FamilySize, fill = Survived)) +
  geom_bar(position = "fill") +
  facet_wrap(~ Sex) + 
  scale_x_continuous(breaks = unique(train$FamilySize))
