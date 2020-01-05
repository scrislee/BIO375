### Playing with Swirl

library(tidyverse)

# Get most recent version of swirl
# install.packages("devtools")
# library(devtools)
# install_github("swirldev/swirl", ref = "dev")
library(swirl)
swirl()

Sarah
install_course("Subsetting Data in R")
chairdataset <- read_csv("data/R Subsetting Tutorial Chair Dataset-5005.csv")

# OK, R Subsetting Tutorial uses base R commands instead of tidyverse commands.

# Do basic R Programming tutorial instead
swirl()
Sarah
install_course("R Programming")
Sarah

# Lesson 1, basic building blocks is useful, DO
# Lesson 2, workspace and files--would be confusing early on, SKIP
# Lesson 3, sequences of numbers is useful and short, DO before tidy data?
# Lesson 4, vectors includes intro to logical operators, DO before subsetting
# Lesson 5, missing values
# Lesson 6, subsetting vectors