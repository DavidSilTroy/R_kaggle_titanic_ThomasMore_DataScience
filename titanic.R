#Hello! just starting..

################################################################################
# Setup packages and settings
################################################################################
# --- Install required packages ---
# install.packages("tidyverse")
# install.packages("ggcorrplot")

# --- Load required libraries ---
library(tidyverse) # Contains all tidyverse packages (ggplot2, dplyr, ...)
library(ggcorrplot) # Used for generating correlation heatmaps (uses ggplot2)

Sys.setenv(LANG = "en") # Set language to English
setwd(getwd()) # Set the working directory to the script directory
rm(list = ls()) # Clears the Global Env
theme_update(plot.title = element_text(hjust = 0.5)) # Center all plot titles



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
# Correlation heatmap (uses ggcorrplot)
################################################################################
train_numeric <- select(train, Age, SibSp, Parch, FamilySize, Fare)
train_numeric_corr <- cor(train_numeric, use = "complete.obs") # Use only non NA
ggcorrplot::ggcorrplot(train_numeric_corr,
                       lab = TRUE, # Show correlation coefficients
                       colors = c("darkturquoise", "white", "salmon"),
                       title = "Correlation between the numeric values")



################################################################################
# Plots and stuff (uses ggplot2)
################################################################################
# Embarked and Fare prices
ggplot(data = train, mapping = aes(x = Embarked, y = Fare)) +
  geom_boxplot() +
  ggtitle("Embarked and Fare prices")


# Pclass, Family size and Survived bigger than fare 500
FareEnough <- filter(train, Fare > 500) # Fare bigger than 500

ggplot(data = FareEnough, mapping = aes(x = Pclass, y = FamilySize)) +
  geom_point(aes(shape=Survived)) +
  ggtitle("Pclass, Family size and Survived bigger than fare 500")


# Count of family size who paid over 500
ggplot(data = FareEnough, mapping = aes(x = FamilySize)) +
  geom_histogram() +
  ggtitle("Count of family size who paid over 500")


# Male Female survival percentage
ggplot(data = train, mapping = aes(x = Sex, fill = Survived)) +
  geom_bar(position = "fill") +
  ggtitle("Male Female survival percentage")


# Pclass survival percentage
ggplot(data = train, mapping = aes(x = Pclass, fill = Survived)) +
  geom_bar(position = "fill") +
  ggtitle("Pclass survival percentage")


# Pclass and cabin label percentage
ggplot(data = train, mapping = aes(x = Pclass, fill = CabinGroups)) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  scale_fill_manual(values = c("darkturquoise",
                               "salmon")) +
  ggtitle("Pclass and cabin label percentage")


# FamilySize survival percentage by Sex
ggplot(data = train, mapping = aes(x = FamilySize, fill = Survived)) +
  geom_bar(position = "fill") +
  facet_wrap(~ Sex) +
  scale_x_continuous(breaks = unique(train$FamilySize)) +
  ggtitle("FamilySize survival percentage by Sex")
