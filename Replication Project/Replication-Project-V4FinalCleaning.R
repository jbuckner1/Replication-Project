# Metadata ---------------------------------------------------------------------

# Code written by: JB Buckner
# Last Update: 2024-10-16
# Filename: Replication-Project-V4FinalCleaning.R

# Purpose of this script: Finish creating new/neccessary variables for 
# Socioeconomic achievement and Demographics
# 


# Setup ------------------------------------------------------------------------


rm(list=ls())
options(width = 80)

filename <-"Replication-Project-V4FinalCleaning"

sink(paste(filename, ".log", sep = "")) # create log file to store output


library(dplyr)
library(tidyverse)
#Import the RDS data from script file 3, and rename in MyGSS4
MyGSS4 <- readRDS("GSS.Data_to_be_cleanedV2.RDS")

#---------------SOCIOECONOMIC ACHIEVEMENT VARIABLES-----------------------------
#create new Education variable and rename it for descriptive statistics
# EDUCATION
MyGSS4$`Education In years (0-20)`<- MyGSS4$Education_in_years
comment(MyGSS4$`Education In years (0-20)`) <- "Variable for Education in years
 | Jb Buckner | 2024-10-20"
comment(MyGSS4$`Education In years (0-20)`)


#---------------------------------------------------------------------
#FAMILY INCOME IN CONSTANT DOLLARS
#Create new variable for family income 
MyGSS4$`Family Income In Constant Dollars` <-
  MyGSS4$`Family_income_in_constant dollars`
#Check for accuracy of mean total
mean(MyGSS4$`Family Income In Constant Dollars`, na.rm = T)
#  Article Total mean is 41371, mine is 41150.86
MyGSS4 %>%
  group_by(`Threatened with gun`) %>%
  summarise(mean_income = mean(as.numeric(`Family Income In Constant Dollars`), 
                               na.rm = TRUE))
#

#-------------------------------------------------------------------------------
#FINANCIAL SATISFIED
#Create three variables that represent financial satisfaction
#Financial satisfied, Financial more or less satisfied, financial not satisfied
MyGSS4$`Financial Satisfied (0=no,1=yes)` <- 
  ifelse(MyGSS4$Financial_satisfaction== 1,1,0)

mean(MyGSS4$`Financial Satisfied (0=no,1=yes)`,na.rm = TRUE)
table(MyGSS4$`Financial Satisfied (0=no,1=yes)`)
#-------------------------------------------------------------------------------
#FINANCIAL MORE OR LESS SATISFIED
MyGSS4$`Financial More or Less Satisfied (0=no,1=yes)` <- 
  ifelse(MyGSS4$Financial_satisfaction==2,1,0)
                                                    
mean(MyGSS4$`Financial More or Less Satisfied (0=no,1=yes)`, na.rm=TRUE)
table(MyGSS4$`Financial More or Less Satisfied (0=no,1=yes)`)

#-------------------------------------------------------------------------------
#FINANCIAL NOT SATISFIED
MyGSS4$`Financial Not Satisfied (0=no,1=yes)` <- 
  ifelse(MyGSS4$Financial_satisfaction== 3,1,0)
mean(MyGSS4$`Financial Not Satisfied (0=no,1=yes)`, na.rm=TRUE)
table(MyGSS4$`Financial Not Satisfied (0=no,1=yes)`)


#-----------------DEMOGRAPHICS VARIABLES----------------------------------------

#-------------------------------------------------------------------------------
#FEMALE (0=male,1=female)
#Create new female variable that corresponds with the responses from gender_
# In gender-, 1= male and 2=female, will have to change that to 0=male, 1=female
MyGSS4$`Female (0=male,1=female)` <- ifelse(MyGSS4$Gender_== 1,0,1)

mean(MyGSS4$`Female (0=male,1=female)`, na.rm=T)
table(MyGSS4$`Female (0=male,1=female)`)

#-------------------------------------------------------------------------------
#AGE (in years, 18-89 or older)
MyGSS4$`Age (18-89 or older)` <- MyGSS4$Age_
mean(MyGSS4$`Age (18-89 or older)`,na.rm=T)

#-------------------------------------------------------------------------------
#WHITE
# New variable that represents if the respondent was white or not, 0=no,1=yes
#Based on the variable Race_
MyGSS4$`White (0=no,1=yes)` <- ifelse(MyGSS4$Race_==1,1,0)

mean(MyGSS4$`White (0=no,1=yes)`,na.rm=T)

#BLACK, new variable that represents if the respondent was black, 0=no,1=yes
MyGSS4$`Black (0=no,1=yes)` <- ifelse(MyGSS4$Race_==2,1,0)

mean(MyGSS4$`Black (0=no,1=yes)`)

#OTHER, variable that represents if the respondent was another race 0=no,1=yes
MyGSS4$`Other (0=no,1=yes)` <-  ifelse(MyGSS4$Race_== 3,1,0)

mean(MyGSS4$`Other (0=no,1=yes)`,na.rm=T)

#-------------------------------------------------------------------------------
#NEVER MARRIED, New variable that shows the respondents marital status based
# on the variable marriage_status_, in this 5=never married

MyGSS4$`Never Married (0=no,1=yes)` <- ifelse(MyGSS4$Marriage_status_==
                                                5,1,0)
mean(MyGSS4$`Never Married (0=no,1=yes)`,na.rm=T)

#MARRIED, 0=no,1=yes, Based on respondents from variable marriage_status_
# In marriage_status_, 1=married
MyGSS4$`Married (0=no,1=yes)` <- ifelse(MyGSS4$Marriage_status_==1,1,0)

mean(MyGSS4$`Married (0=no,1=yes)`,na.rm=T)

#WIDOWED, new variables based on marriage_status_, in this 2=widowed
MyGSS4$`Widowed (0=no,1=yes)` <- ifelse(MyGSS4$Marriage_status_==2,1,0)

mean(MyGSS4$`Widowed (0=no,1=yes)`,na.rm=T)

#DIVORCED/SEPARATED, new variable based on the responses 3 and 4
# I will have to create a new variable that combines the responses 3 and 4 from
# marriage_status and makes both of those a 1 in the new variable 
# DIVORCED/SEPARATED

MyGSS4$`Divorced/separated (0=no,1=yes)` <- ifelse(MyGSS4$Marriage_status_ %in%
                                                     c(3,4),1,0)
                                    
mean(MyGSS4$`Divorced/separated (0=no,1=yes)`,na.rm=T)


#-------------------------------------------------------------------------------
# Save all new variables into a new data frame to be saved and used in next
# script file to check accuracy of descriptive statistics

names(MyGSS4)

MyGSS5 <- MyGSS4 %>% 
  select("year","region","Threatened with gun",
         "People can be trusted (0=no,1=yes)","People are helpful (0=no,1=yes)",
         "People are fair (0=no,1=yes)","Family income at 16 (scale,1-5)",
         "Family income at 16 (scale,1-5)",
         "Living in mixed neighborhood (0=no,1=yes)",
         "Education In years (0-20)","Family Income In Constant Dollars",
         "Financial Satisfied (0=no,1=yes)",
         "Financial More or Less Satisfied (0=no,1=yes)",
         "Financial Not Satisfied (0=no,1=yes)","Female (0=male,1=female)",
         "Age (18-89 or older)","White (0=no,1=yes)","Black (0=no,1=yes)",
         "Other (0=no,1=yes)","Never Married (0=no,1=yes)",
         "Married (0=no,1=yes)","Widowed (0=no,1=yes)",
         "Divorced/separated (0=no,1=yes)"
         )
names(MyGSS5)
comment(MyGSS5) <- "New data set that contains all the neccessary new variables
to be used in generating descriptive statistics grouped by 'Threatened with gun'
 | JB Buckner sts3300 | 2024-10-23"
comment(MyGSS5)

#Save as an RDS file to be used for descriptive statistics
saveRDS(MyGSS5, "GSS.DescriptiveStatisticsV5.RDS")

sink()
