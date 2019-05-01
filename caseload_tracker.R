library(stringr)
library(data.table)
library(plyr)
library(stringdist)
library(dplyr)
library(anytime)
library(tidyr)
library(xlsx)

# Remove older intake variables that aren't super important
rm(dupes,dupes.final, intake, intake.deduped, intake.ordered, intake.true)

# Load in caseload and case notes
cl <- read.xlsx('tables/ORC Caseload Tracker 2019 Master.xlsx', sheetName = 'Caseload')
cn <- read.xlsx('tables/ORC Caseload Tracker 2019 Master.xlsx', sheetName = 'Case Notes')

# Remove caseload and casenotes where name is null
cl <- subset(cl, !is.na(cl$FIRST.NAME))
cn <- subset(cn, !is.na(cn$FIRST.NAME))

# Trim the end of the tables
cl <- cl [ , 1:12 ]
cn <- cn [ , 1:4 ]

cl$drop <- duplicated(cl[,1:3],fromLast = TRUE ) # Finds first in record
cl$keep <- duplicated(cl[,1:3],fromLast = FALSE)
cl$dupe <- NA

# Create new dupe column and populate it with TRUES
cl$dupe <- cl$drop
cl$dupe <- ifelse(cl$keep == TRUE, cl$keep, cl$dupe)

# Take just dupes
dupes.cl <- subset(cl, cl$dupe == TRUE)

# Do again with cn
cn$drop <- duplicated(cn[,1:3],fromLast = TRUE ) # Finds first in record
cn$keep <- duplicated(cn[,1:3],fromLast = FALSE)
cn$dupe <- NA

# Create new dupe column and populate it with TRUES
cn$dupe <- cn$drop
cn$dupe <- ifelse(cn$keep == TRUE, cn$keep, cn$dupe)

# Take just dupes
dupes.cn <- subset(cn, cn$dupe == TRUE)

##### Save all Dupes #####
write.csv(dupes.cl, 'caseload_duplicates.csv', row.names = FALSE)
write.csv(dupes.cn, 'casenotes_duplicates.csv', row.names = FALSE)
write.xlsx(dupes.cl, 'caseload_duplicates.xlsx', row.names = FALSE)
write.xlsx(dupes.cn, 'casenotes_duplicates.xlsx', row.names = FALSE)

##### Remove all duplicates in cn and cl and consider export #####
cl <- subset(cl, cl$drop != TRUE) %>%
  subset(select = -c(keep,drop,dupe))

cn <- subset(cn, cn$drop != TRUE) %>%
  subset(select = -c(keep,drop,dupe))


#write.xlsx(cl, 'caseload_deduped.xlsx',row.names = FALSE)
#write.xlsx(cn, 'casenotes)deduped.xlsx',row.names = FALSE)

##### Merge various forms #####


# Add in intake form
it <- read.csv('ORC_Intake_cleaned.csv')








