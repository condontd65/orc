library(stringr)
library(data.table)
library(plyr)
library(stringdist)
library(dplyr)
library(googlesheets)
library(anytime)

##### Input Data and Begin Clean #####
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

intake$First.name. <- gsub(' ','', intake$First.name.)
intake$Last.name. <- gsub(' ','', intake$Last.name.)


##### Remove Dupes #####
# Order by date with newer on top
intake.ordered <- intake[order(as.Date(intake$date, format = "%Y-%m-%d h:m"), decreasing = TRUE)]

# Find duplicated over first and last name
intake.ordered$dup <- duplicated(intake.ordered[,3:4],fromLast = TRUE)
dupes <- intake.ordered[duplicated(intake.ordered[,3:4],fromLast = TRUE)]

# Verify google to keep track of duplicates
#gs_auth(new_user = TRUE)
#write.csv(dupes, 'dupes.csv', row.names = FALSE)
#gs_upload('dupes.csv', sheet_title = 'orc_intake_duplicates')

# Correct versions with misspelt differences leading to not being detected by "duplicated"
intake.ordered[405,42] <- TRUE
intake.ordered[543,42] <- TRUE
intake.ordered[517,42] <- TRUE
intake.ordered[367,42] <- TRUE
intake.ordered[379,42] <- TRUE
intake.ordered[505,42] <- TRUE
intake.ordered[190,42] <- TRUE
intake.ordered[448,42] <- TRUE
intake.ordered[425,42] <- TRUE
intake.ordered[520,42] <- TRUE
intake.ordered[422,42] <- TRUE
intake.ordered[487,42] <- TRUE
intake.ordered[197,42] <- TRUE
intake.ordered[221,42] <- TRUE

# Subset to be sure
intake.true <- intake.ordered[ intake.ordered$dup == TRUE ]

# Take out all dupes
intake.deduped <- intake.ordered[ intake.ordered$dup == FALSE ]


##### Fix Dates ##### 
# String manipulation to clean up start dates
start.date <- intake.deduped$Start.date.of.your.most.recent.period.of.Incarceration.
start.date <- gsub('   ', '', start.date)
start.date <- gsub('  ', ' ', start.date)
start.date <- gsub(' ', '-', start.date)

# Write function to remove from the end of a string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# Remove and replace '-' at the beginning and end of strings
start.date <- ifelse(substr(start.date, 1, 1) == '-', sub("^.", "", start.date), start.date)
start.date <- ifelse(substrRight(start.date, 1) == '-', sub(".$", "", start.date), start.date)

# Format longer strings as dates "%m-%d-%Y"
start.date.t1 <- ifelse(nchar(start.date) > 7, as.Date(start.date,format = "%m-%d-%Y"), start.date)



test5 <- anydate(test4)
eval <- data.frame(test, test2, test3, test4)

test <- as.Date(test, format = "%m %d %Y")
test <- gsub(' ', '-', test)



# Plot in window
plot(gvisTable(dupes))
