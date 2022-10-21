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
rm(list = ls()) # Clears the Global Env
theme_update(plot.title = element_text(hjust = 0.5)) # Center all plot titles



################################################################################
# Read and import the data set
################################################################################
# Read the data set (uses readxl)
column_types <- c(
  "numeric", # ...1
  "numeric", # Harvest_year
  "text",    # admin0
  "text",    # admin1
  "text",    # crop
  "numeric", # hectares (ha)
  "numeric", # production (tonnes)
  "numeric", # year
  "numeric", # yield(tonnes/ha)
  "text",    # admin2
  "text"     # notes
)
crops <- read_xlsx(
  path = "./crops/food-twentieth-century-crop-statistics-1900-2017-xlsx.xlsx",
  sheet = "CropStats",
  col_types = column_types)

# Drop not needed columns
crops <- select(crops, -c(...1, admin2, notes, Harvest_year))


crops <- crops %>% mutate(crop = factor(crop,
                                        levels = c("wheat", "winter wheat",
                                                   "spring wheat", "maize",
                                                   "cereals"),
                                        ordered = TRUE))

crops <- crops %>% mutate(year = as.integer(year))

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
                        `hectares (ha)`,
                        `production (tonnes)`,
                        year ,
                        `yield(tonnes/ha)`)

crops_numeric_corr <- cor(crops_numeric, use = "complete.obs") # Use only non NA

ggcorrplot::ggcorrplot(crops_numeric_corr,
                       lab = TRUE, # Show correlation coefficients
                       colors = c("darkturquoise", "white", "salmon"),
                       title = "Correlation between the numeric values")

# Clear not needed variables
rm(crops_numeric, crops_numeric_corr)



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



################################################################################
# References
################################################################################
# Correlation heatmap: 
# http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2