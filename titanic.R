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
  Name = col_character(),
  Sex = col_factor(),
  Age = col_double(),
  SibSp = col_integer(),
  Parch = col_integer(),
  Ticket = col_character(),
  Fare = col_double(),
  Cabin = col_character(),
  Embarked = col_factor(include_na = TRUE, ordered = TRUE)
)
train <- read_csv("./kaggle/titanic/train.csv",
                  col_types = column_types,
                  col_select = -c(PassengerId))


# Rename the factors to be human readable (uses dplyr)
train$Survived <- recode_factor(train$Survived,
                                "0" = "No",
                                "1" = "Yes",)

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

# Move the comma separated surname to the end of the name
train <- separate(train, Name, c("SurName", "FirstName"), sep = ", ") %>%
  mutate(Name = str_c(FirstName, SurName, sep = " "))

# Drop the temporary columns
train <- select(train, -c(SurName, FirstName))

# Clear not needed variables
rm(column_types)



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
#view(train) #checking the train data

################################################################################
# Adding useful columns
################################################################################
# Add a total Family size column
train <- mutate(train, FamilySize = SibSp + Parch)

# Group the cabin label into has cabin and has no cabin
train <- mutate(train, CabinGroups = ifelse(is.na(train$Cabin),
                                            "No cabin",
                                            "Cabin"))

# Add Married column, only works for female passangers
train <- mutate(train,
                Married = ifelse(Sex == "female",
                                 stringr::str_detect(Name, "^[Mm]rs"), NA))

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

# Clear not needed variables
rm(train_numeric, train_numeric_corr)


################################################################################
# Plots and stuff (uses ggplot2)
################################################################################
# Age of people on-board the Titanic grouped by gender
ggplot(data = train, mapping = aes(x = Sex, y = Age)) +
  geom_boxplot() +
  ggtitle("Age per sex of people on-board the Titanic")


# Fare prices grouped by embarkment
ggplot(data = train, mapping = aes(x = Embarked, y = Fare)) +
  geom_boxplot() +
  ggtitle("Fare prices grouped by embarkment")


# Fare prices grouped by passenger class
ggplot(data = train, mapping = aes(x = Pclass, y = Fare)) +
  geom_boxplot() +
  xlab("Passenger class") +
  ggtitle("Fare prices grouped by passenger class")


# Family size & Survived who paid over 500 grouped by Age
FareEnough <- filter(train, Fare > 500) # Fare bigger than 500

ggplot(data = FareEnough, mapping = aes(x = Age, y = FamilySize)) +
  geom_point(aes(shape = Survived)) +
  xlab("Age") +
  ylab("Family size") +
  scale_x_continuous(breaks = scales::breaks_width(1)) +
  scale_y_continuous(breaks = scales::breaks_width(1)) +
  ggtitle("Family size & Survived who paid over 500 grouped by Age")


# Count of family size who paid over 500
ggplot(data = FareEnough, mapping = aes(x = FamilySize)) +
  geom_histogram() +
  xlab("Family size") +
  scale_y_continuous(breaks = scales::breaks_width(1)) +
  scale_x_continuous(breaks = scales::breaks_width(1)) +
  ggtitle("Count of family size who paid over 500")


# Male Female survival percentage
ggplot(data = train, mapping = aes(x = Sex, fill = Survived)) +
  geom_bar(position = "fill") +
  ylab("Percentage") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Male / Female survival percentage")


# Passenger class survival percentage
ggplot(data = train, mapping = aes(x = Pclass, fill = Survived)) +
  geom_bar(position = "fill") +
  xlab("Passenger class") +
  ylab("Percentage") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Passenger class survival percentage")


# Passenger class has cabin label percentage
ggplot(data = train, mapping = aes(x = Pclass, fill = CabinGroups)) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  scale_fill_manual(values = c("darkturquoise",
                               "salmon")) +
  xlab("Passenger class") +
  ylab("Percentage") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Passenger class has cabin label percentage")


# Family Size survival percentage grouped by gender
ggplot(data = train, mapping = aes(x = FamilySize, fill = Survived)) +
  geom_bar(position = "fill") +
  facet_wrap(~ Sex) +
  scale_x_continuous(breaks = min(train$FamilySize):max(train$FamilySize)) +
  xlab("Family size") +
  ylab("Percentage") +
  ggtitle("Family Size survival percentage grouped by gender")


################################################################################
# References
################################################################################
# Correlation heatmap: 
# http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2

###