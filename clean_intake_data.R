library(stringr)
library(data.table)
library(plyr)
library(stringdist)
library(dplyr)
library(googlesheets)
library(anytime)
library(tidyr)
library(FedData)
library(xlsx)

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
intake.ordered$dup_keep <- duplicated(intake.ordered[,3:4],fromLast = FALSE)
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

intake.ordered[434,43] <- TRUE
intake.ordered[555,43] <- TRUE
intake.ordered[527,43] <- TRUE
intake.ordered[533,43] <- TRUE
intake.ordered[384,43] <- TRUE
intake.ordered[524,43] <- TRUE
intake.ordered[263,43] <- TRUE
intake.ordered[458,43] <- TRUE
intake.ordered[523,43] <- TRUE
intake.ordered[537,43] <- TRUE
intake.ordered[431,43] <- TRUE
intake.ordered[508,43] <- TRUE
intake.ordered[206,43] <- TRUE
intake.ordered[230,43] <- TRUE

# Make final table of duplicates
dupes.final <- intake.ordered [ intake.ordered$dup == TRUE | intake.ordered$dup_keep == TRUE ]

#intake.lastname <- intake.ordered[order(intake$Last.name., decreasing = FALSE)]
# Subset to be sure
intake.true <- intake.ordered[ intake.ordered$dup == TRUE ]


# Take out all dupes
intake.deduped <- intake.ordered[ intake.ordered$dup == FALSE ]

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
##### Bad Fix Dates ##### 
.comment <- function(){
# String manipulation to clean up start dates
start.date <- intake.deduped$Start.date.of.your.most.recent.period.of.Incarceration.
start.date <- gsub('   ', '', start.date)
start.date <- gsub('  ', ' ', start.date)
start.date <- gsub(' ', '-', start.date)

# Write function to remove from the end of a string


# Remove and replace '-' at the beginning and end of strings
start.date <- ifelse(substr(start.date, 1, 1) == '-', sub("^.", "", start.date), start.date)
start.date <- ifelse(substrRight(start.date, 1) == '-', sub(".$", "", start.date), start.date)

# Format longer strings as dates "%m-%d-%Y"
start.date.t <- gsub('-','/',start.date)
start.date.t1 <- ifelse(nchar(start.date.t) > 7, as.Date(start.date.t,format = "%m/%d/%Y"),start.date.t)



test5 <- anydate(test4)
eval <- data.frame(test, test2, test3, test4)

test <- as.Date(test, format = "%m %d %Y")
test <- gsub(' ', '-', test)
}

##### Fix Dates #####

# Create new dataframe from intake.deduped to allow more analysis
it <- intake.deduped

# Use separate to split months, days, years
it <- separate(it, col = Start.date.of.your.most.recent.period.of.Incarceration., 
         into = c('start.month','start.day','start.year'), 
         sep = ' ')

# Change NA to nothing
it$start.day [ is.na(it$start.day) ] <- ''
it$start.month [ is.na(it$start.month) ] <- ''
it$start.year [ is.na(it$start.year) ] <- ''

# Pad day and month to add leading zeros
it$start.day <- ifelse(nchar(it$start.day) == 1,
                       str_pad(it$start.day,
                               width = 2,
                               side = 'left',
                               pad = '0'),
                       it$start.day)
it$start.month <- ifelse(nchar(it$start.month) == 1,
                       str_pad(it$start.month,
                               width = 2,
                               side = 'left',
                               pad = '0'),
                       it$start.month)

# Combine all into a date,seperated by "-"
it$Start.date.of.your.most.recent.period.of.Incarceration. <- paste(it$start.month,
                                                                                it$start.day,
                                                                                it$start.year,
                                                                    sep = "-")

# Remove starting "-"                                                                                                                                                    sep = "-")
it$Start.date.of.your.most.recent.period.of.Incarceration. <- ifelse(substr(it$Start.date.of.your.most.recent.period.of.Incarceration., 1, 1) == '-', 
                                                                     sub("^.", "", it$Start.date.of.your.most.recent.period.of.Incarceration.), 
                                                                     it$Start.date.of.your.most.recent.period.of.Incarceration.)

it$Start.date.of.your.most.recent.period.of.Incarceration. <- ifelse(substr(it$Start.date.of.your.most.recent.period.of.Incarceration., 1, 1) == '-', 
                                                                     sub("^.", "", it$Start.date.of.your.most.recent.period.of.Incarceration.), 
                                                                     it$Start.date.of.your.most.recent.period.of.Incarceration.)
# Remove ending "-"
it$Start.date.of.your.most.recent.period.of.Incarceration. <- ifelse(substrRight(it$Start.date.of.your.most.recent.period.of.Incarceration., 1) == '-', 
                                                                     sub(".$", "", it$Start.date.of.your.most.recent.period.of.Incarceration.), 
                                                                     it$Start.date.of.your.most.recent.period.of.Incarceration.)
# Remove ending "-"
it$Start.date.of.your.most.recent.period.of.Incarceration. <- ifelse(substrRight(it$Start.date.of.your.most.recent.period.of.Incarceration., 1) == '-', 
                                                                     sub(".$", "", it$Start.date.of.your.most.recent.period.of.Incarceration.), 
                                                                     it$Start.date.of.your.most.recent.period.of.Incarceration.)

## Now do the same thing for end date
it <- separate(it, col = End.date.of.your.most.recent.period.of.Incarceration., 
               into = c('end.month','end.day','end.year'), 
               sep = ' ')

# Change NA to nothing
it$end.day [ is.na(it$end.day) ] <- ''
it$end.month [ is.na(it$end.month) ] <- ''
it$end.year [ is.na(it$end.year) ] <- ''

# Pad day and month to add leading zeros
it$end.day <- ifelse(nchar(it$end.day) == 1,
                       str_pad(it$end.day,
                               width = 2,
                               side = 'left',
                               pad = '0'),
                       it$end.day)
it$end.month <- ifelse(nchar(it$end.month) == 1,
                         str_pad(it$end.month,
                                 width = 2,
                                 side = 'left',
                                 pad = '0'),
                         it$end.month)

# Combine all into a date,seperated by "-"
it$End.date.of.your.most.recent.period.of.Incarceration. <- paste(it$end.month,
                                                                    it$end.day,
                                                                    it$end.year,
                                                                    sep = "-")

# Remove Ending "-"                                                                                                                                                    sep = "-")
it$End.date.of.your.most.recent.period.of.Incarceration. <- ifelse(substr(it$End.date.of.your.most.recent.period.of.Incarceration., 1, 1) == '-', 
                                                                     sub("^.", "", it$End.date.of.your.most.recent.period.of.Incarceration.), 
                                                                     it$End.date.of.your.most.recent.period.of.Incarceration.)

it$End.date.of.your.most.recent.period.of.Incarceration. <- ifelse(substr(it$End.date.of.your.most.recent.period.of.Incarceration., 1, 1) == '-', 
                                                                   sub("^.", "", it$End.date.of.your.most.recent.period.of.Incarceration.), 
                                                                   it$End.date.of.your.most.recent.period.of.Incarceration.)
# Remove ending "-"
it$End.date.of.your.most.recent.period.of.Incarceration. <- ifelse(substrRight(it$End.date.of.your.most.recent.period.of.Incarceration., 1) == '-', 
                                                                     sub(".$", "", it$End.date.of.your.most.recent.period.of.Incarceration.), 
                                                                     it$End.date.of.your.most.recent.period.of.Incarceration.)
# Remove ending "-"
it$End.date.of.your.most.recent.period.of.Incarceration. <- ifelse(substrRight(it$End.date.of.your.most.recent.period.of.Incarceration., 1) == '-', 
                                                                     sub(".$", "", it$End.date.of.your.most.recent.period.of.Incarceration.), 
                                                                     it$End.date.of.your.most.recent.period.of.Incarceration.)


## Now do the same thing for date completed
it <- separate(it, col = Date.completed., 
               into = c('month','day','year'), 
               sep = ' ')

# Change NA to nothing
it$day [ is.na(it$day) ] <- ''
it$month [ is.na(it$month) ] <- ''
it$year [ is.na(it$year) ] <- ''

# Pad day and month to add leading zeros
it$day <- ifelse(nchar(it$day) == 1,
                     str_pad(it$day,
                             width = 2,
                             side = 'left',
                             pad = '0'),
                     it$day)
it$month <- ifelse(nchar(it$month) == 1,
                       str_pad(it$month,
                               width = 2,
                               side = 'left',
                               pad = '0'),
                       it$month)

# Combine all into a date,seperated by "-"
it$Date.completed. <- paste(it$month,
                            it$day,
                            it$year,
                            sep = "-")

# Remove Ending "-"
it$Date.completed. <- ifelse(substr(it$Date.completed., 1, 1) == '-', 
                                                                   sub("^.", "", it$Date.completed.), 
                                                                   it$Date.completed.)

it$Date.completed. <- ifelse(substr(it$Date.completed., 1, 1) == '-', 
                             sub("^.", "", it$Date.completed.), 
                             it$Date.completed.)
# Remove ending "-"
it$Date.completed. <- ifelse(substrRight(it$Date.completed., 1) == '-', 
                                                                   sub(".$", "", it$Date.completed.), 
                                                                   it$Date.completed.)
# Remove ending "-"
it$Date.completed. <- ifelse(substrRight(it$Date.completed., 1) == '-', 
                                                                   sub(".$", "", it$Date.completed.), 
                                                                   it$Date.completed.)

it$Date.completed. <- gsub('--','-',it$Start.date.of.your.most.recent.period.of.Incarceration.)
it$Date.completed. <- gsub('--','-',it$End.date.of.your.most.recent.period.of.Incarceration.)
it$Date.completed. <- gsub('--','-',it$Date.completed.)


##### Other Data Manipulation #####
# Drop dataframe columns that are unecessary
#drops <- c('start.year','start.month','start.day','end.year','end.month','end.day','month','day','year')

it <- it[, !c('start.year','start.month','start.day','end.year','end.month','end.day','month','day','year')]

column.order <- colnames(intake.deduped)

# Organize table in same way as original input
it <- it [ , c('Reference.ID', 'Submission.Created', 'First.name.',
         'Last.name.', 'Phone.Number.', 'Email.', 'Street.address.',
         'City.', 'State.', 'Zip.', 'Highest.level.of.education.completed.',
         'Year.completed.', 'Have.you.served.in.the.U.S..military.',
         'Marital.status.', 'Do.you.have.children.', 'How.many.children.do.you.have.',
         'Please.select.the.age.range.s..of.your.children...Select.all.that.apply..',
         'Are.you.receiving.SNAP.benefits.', 'Are.you.getting.any.cash.benefits.',
         'What.level.was.your.incarceration.', 'What.state.were.you.incarcerated.in.',
         'Start.date.of.your.most.recent.period.of.Incarceration.',
         'End.date.of.your.most.recent.period.of.Incarceration.', 
         'Are.you.receiving.case.management.services.', 'Intake.coordinator.',
         'Where.are.you.receiving.case.management.services.', 'Case.manager.s.name.',
         'Case.manager.s.phone.number.', 'Are.you.currently.under.supervision.',
         'Name.of.CSO.', 'What.services.can.we.help.you.with...Select.all.that.apply..',
         'What.is.your.date.of.birth.', 'Do.you.identify.as.Hispanic.or.Latino.',
         'How.do.you.identify.your.race..Check.all.that.apply.',
         'What.is.your.gender.identity.', 'Halfway.house', 'Date.completed.',
         'Have.you.served.in.the.U.S..military..1', 'What.is.your.age.range.',
         'Prison...Jail.ID.Number.') ]

# Write function to fix capitalization of certain fields
simpleCap <- function(x) {
  s <- strsplit(x,' ')[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = '', collapse = ' ')
}

city <- it$City.
it$City. <- lapply(as.character(it$City.), simpleCap)

dupes.final$Last.name. <- lapply(as.character(dupes.final$Last.name.), simpleCap)
dupes.final$Last.name. <- as.character(dupes.final$Last.name.)
dupes.final.ord <- dupes.final[order(dupes.final$Last.name., decreasing = FALSE)]
dupes.final.ord$keep <- dupes.final.ord$dup_keep
dupes.final.ord <- dupes.final.ord[, !c('dup','dup_keep')]


# Fix column names
colnames(it) <- c('Reference ID', 'Submission Created', 'First name:', 'Last name:', 'Phone Number:',
                  'Email:', 'Street address:', 'City:', 'State:', 'Zip:', 'Highest level of education completed:',
                  'Year completed:', 'Have you served in the U.S. military?', 'Marital status:',
                  'Do you have children?', 'How many children do you have?',
                  'Please select the age range(s) of your children. (Select all that apply.)',
                  'Are you receiving SNAP benefits?', 'Are you getting any cash benefits?', 
                  'What level was your incarceration?', 'What state were you incarcerated in?',
                  'Start date of your most recent period of Incarceration:', 
                  'End date of your most recent period of Incarceration:', 'Are you receiving case management services?',
                  'Intake coordinator:', 'Where are you receiving case management services?', "Case manager's name:",
                  "Case manager's phone number:", 'Are you currently under supervision?', 'Name of CSO:',
                  'What services can we help you with? (Select all that apply.)','What is your date of birth?',
                  'Do you identify as Hispanic or Latino?', 'How do you identify your race? Check all that apply.',
                  'What is your gender identity?', 'Halfway house', 'Date completed:', 'Have you served in the U.S. military?',
                  'What is your age range?', 'Prison / Jail ID Number:')

# Convert zip to a character
it$`Zip:` <- as.character(it$`Zip:`)
it$`City:` <- as.character(it$`City:`)

# Remove NA for export and just give null value
it [ is.na(it) ] <- ''

# Write to csv
write.csv(it, 'ORC_Intake_cleaned.csv', row.names = FALSE)
write.xlsx(it, 'ORC_Intake_cleaned.xlsx', row.names = FALSE)

dupes.final.ord [ is.na(dupes.final.ord) ] <- ''
write.xlsx(dupes.final.ord, 'ORC_Intake_duplicates.xlsx', row.names = FALSE)









# Random prk stuff
prk.function <- function (x) {
old.count <- 12302
new.count <- 19659
dif <- new.count - old.count

old.area <- 562393464
new.area <- 737097723
dif <- new.area - old.area
dif.miles <- dif / 27880000

rm(old.count,new.count,dif,old.area,new.area,dif.miles)
}









