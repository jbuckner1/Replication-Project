# Metadata ---------------------------------------------------------------------

# Code written by: JB Buckner
# Last Update: 2024-10-02
# Filename: Replication-Project-LoadDataset.R

# Purpose of this script: Load in the GSS data and begin doing data management
# 


# Setup ------------------------------------------------------------------------


rm(list=ls())
options(width = 80)

filename <-"Replication-Project-V1LoadDataset"

sink(paste(filename, ".log", sep = "")) # create log file to store output

# R version
R.version.string

# 0 #Load in the GSS dataset

GSS <-  readRDS(file="gss7222_r3a.RDS")


# 1 # Create a new dataset that contains the variables needed for replication

MyGSS <- GSS[c("year","gunage","gun", "trust","helpful", "fair", "incom16","raclive", 
               "educ","coninc", "satfin", "sex", "age", "race",
               "marital","region")]

# Save my GSS dataset as an RDS to be used for data cleaning and management
saveRDS(MyGSS, "GSS.Variables.DataV1.RDS")

sink()



#######


