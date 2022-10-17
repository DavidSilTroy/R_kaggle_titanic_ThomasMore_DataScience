#Hello! just starting.. The second edition

################################################################################
# Setup packages and settings
################################################################################
# --- Install required packages ---
# install.packages("tidyverse")
# install.packages("ggcorrplot")

# --- Load required libraries ---
library(tidyverse) # Contains all tidyverse packages (ggplot2, dplyr, ...)
library(readxl) #  Need to load explicitly (not a core tidyverse package)
library(ggcorrplot) # Used for generating correlation heatmaps (uses ggplot2)

Sys.setenv(LANG = "en") # Set language to English
setwd(getwd()) # Set the working directory to the script directory
rm(list = ls()) # Clears the Global Env
theme_update(plot.title = element_text(hjust = 0.5)) # Center all plot titles



################################################################################
# Read and import the data set
################################################################################
# Read the data set (uses readxl)
crops <- read_excel(
  path = "./crops/food-twentieth-century-crop-statistics-1900-2017-xlsx.xlsx",
  sheet = "CropStats")



################################################################################
# Filtering and cleaning
################################################################################
# Check for the number of NA's in each column
sanity_check <- function(my_df) {
  for (j in 1:ncol(my_df)) {
    print(paste(names(my_df[j]), ":", sum(is.na(my_df[, j]))))
  }
}

sanity_check(crops)


# View 'crops' tibble
crops


################################################################################
# Adding useful columns
################################################################################
tail(crops)



################################################################################
# Correlation heatmap (uses ggcorrplot)
################################################################################
crops_numeric <- select(crops,
                        Harvest_year,
                        `hectares (ha)`,
                        `production (tonnes)`,
                        year ,
                        `yield(tonnes/ha)`)

crops_numeric_corr <- cor(crops_numeric, use = "complete.obs") # Use only non NA

ggcorrplot::ggcorrplot(crops_numeric_corr,
                       lab = TRUE, # Show correlation coefficients
                       colors = c("darkturquoise", "white", "salmon"),
                       title = "Correlation between the numeric values")



################################################################################
# Plots and stuff (uses ggplot2)
################################################################################

# Production x years
ggplot(data = crops, mapping = aes(x = year, y = `production (tonnes)`)) +
  geom_point() +
  ggtitle("Production over the years")

# Yield outliers
ggplot(data = crops, mapping = aes(x = crop, y = `yield(tonnes/ha)`)) +
  geom_boxplot() +
  ggtitle("Yield outliers")

# Count of crops
ggplot(data = crops, mapping = aes(x = crop)) +
  geom_bar() +
  ggtitle("Amount of crops")