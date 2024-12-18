# Metadata ---------------------------------------------------------------------

# Code written by: JB Buckner
# Last Update: 2024-10-02
# Filename: Replication-Project-LoadDataset.R

# Purpose of this script:Begin cleaning the variables data to remove
# unnecessary years, as well as converting missing observations into NAs
# 


# Setup ------------------------------------------------------------------------


rm(list=ls())
options(width = 80)

filename <-"Replication-Project-V2DataCleaning"

sink(paste(filename, ".log", sep = "")) # create log file to store output

# R version
R.version.string
library(dplyr)
library(naniar)
## Load in the GSS dataset with variables, saved as an RDS
MyGSS <-  readRDS("GSS.Variables.DataV1.RDS")

# Filter the data to only contain observations from the years 1973-1994
MyGSS1 <- MyGSS[which(MyGSS$year >= 1973 & MyGSS$year <= 1994 ), ]
comment(MyGSS1)  <- "JB Buckner / Filtered dataset that contains obsrvations
 only from 1973-1994 / 2024-10-08"
comment(MyGSS1)


# Convert all inapplicable/missing observations into N/As
values_to_replace <- c(".i", ".d", ".n", ".s",".y")

# Loop through each column in MyGSS1 and replace the specified values with "N/A"
for (col in names(MyGSS1)) {
  MyGSS1[[col]][MyGSS1[[col]] %in% values_to_replace] <- NA
}

#Convert all observations to numeric/integer
char_columns <- sapply(MyGSS1  , class) == "character"
MyGSS1[,char_columns] <- sapply(MyGSS1[, char_columns], as.numeric)

#heck that observations are noew stored as numerical values
mean(MyGSS1$year, na.rm=TRUE)
mean(MyGSS1$helpful, na.rm=TRUE)
mean(MyGSS1$raclive, na.rm=TRUE)

#Check that all missing values are 
table(MyGSS1$fair)
table(MyGSS1$raclive)
table(MyGSS1$gunage)
table(MyGSS1$gun)
table(MyGSS1$trust)
comment(MyGSS1) <- c(comment(MyGSS1) , "Updated GSS data that replaced any 
inapplicable or missing values as NA's | Jbuckner5 2024-10-09")
comment(MyGSS1)

# Save MyGSS1 as MyGSS2 to be used in next script file, will be removing
MyGSS2 <- MyGSS1

saveRDS(MyGSS2, "GSS.Data_to_be_cleaned.RDS")


sink()





