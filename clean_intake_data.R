library(stringr)
library(data.table)
library(plyr)
library(stringdist)
library(dplyr)

intake <- read.csv('tables/Office_of_Returning_Citizens_Intake_Form_2019-02-06_145515_bec13ce6.csv') %>%
  data.table()

intake [ intake == "" ] <- NA
  
# Remove all that don't have a first name
intake <- intake [ !(is.na(intake$First.name.)), ]

# Remove all that don't have a last name and an email
intake <- intake [ !(is.na(intake$Last.name.) & is.na(intake$Email.)), ]

# Create new data column to order
intake$date <- gsub(' PM EDT', '', x=intake$Submission.Created)
intake$date <- gsub(' PM EST', '', x=intake$date)
intake$date <- gsub(' AM EDT', '', x=intake$date)
intake$date <- gsub(' AM EST', '', x=intake$date)

# Order by date with newer on top
intake.ordered <- intake[order(as.Date(intake$date, format = "%Y-%m-%d h:m"), decreasing = TRUE)]

# Find duplicated over first and last name
intake$dup <- duplicated(intake[,3:4])
dupes <- intake[duplicated(intake[,3:4])]





# Plot in window
plot(gvisTable(dupes))
