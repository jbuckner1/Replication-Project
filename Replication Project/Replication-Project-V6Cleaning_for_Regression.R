# Metadata ---------------------------------------------------------------------

# Code written by: JB Buckner
# Last Update: 2024-10-16
# Filename: Replication-Project-V6Cleaning_for_Regression.R

# Purpose of this script: Clean up add ad neccesary variables for regression 
# models



# Setup ------------------------------------------------------------------------


rm(list=ls())
options(width = 80)

filename <-"Replication-Project-V6Cleaning_for_Regression"

sink(paste(filename, ".log", sep = "")) # create log file to store output


library(tidyverse)

#Import the RDS data from script file 4, and rename in MyGSS5
MyGSS5 <- readRDS("GSS.DescriptiveStatisticsV5.RDS")

#Rename my GSS5-MyGSS6 to be used for re-leveling and linear regression
MyGSS6 <- MyGSS5

#Rename variables as easier to understand for linear regression
MyGSS6 <- MyGSS6 %>% 
  rename("Ever threatened with a gun or shot at?"= `Threatened with gun`,
         "People can be trusted"= `People can be trusted (0=no,1=yes)`,
         "People are helpful"=`People are helpful (0=no,1=yes)`,
         "People are fair"=`People are fair (0=no,1=yes)`,
         "Family income when 16 years old"=`Family income at 16 (scale,1-5)`,
         "Living in mixed neighborhood"= `Living in mixed neighborhood (0=no,1=yes)`,
         "Education in years"=`Education In years (0-20)`,
         "Family income in constant dollars"=`Family Income In Constant Dollars`,
         "Financial satisfied"=`Financial Satisfied (0=no,1=yes)`,
         "More or less"=`Financial More or Less Satisfied (0=no,1=yes)`,
         "Not at all satisfied"=`Financial Not Satisfied (0=no,1=yes)`,
         "Female"=`Female (0=male,1=female)`,
         "Age in years"=`Age (18-89 or older)`,
         "White"=`White (0=no,1=yes)`,
         "Black"=`Black (0=no,1=yes)`,
         "Other"=`Other (0=no,1=yes)`,
         "Never married"=`Never Married (0=no,1=yes)`,
         "Married"=`Married (0=no,1=yes)`,
         "Widowed"=`Widowed (0=no,1=yes)`,
         "Divorced/separated"= `Divorced/separated (0=no,1=yes)`
  )


#-------------------------------------------------------------------------------
#---CLEAN UP VARIABLES SO SOME OF THEM CAN BE USED AS REFERNCES IN LINEAR 
# REGRESSION


#For question "Threatened with gun", set reference level as "No"
MyGSS6$`Ever threatened with a gun or shot at?` <- factor(
  MyGSS6$`Ever threatened with a gun or shot at?`)

MyGSS6$`Ever threatened with a gun or shot at?` <- relevel(
  MyGSS6$`Ever threatened with a gun or shot at?`, ref = "No")
#-------------------------------------------------------------------------------
#Create new variable for Financial Satisfaction, and then set satisfied as 
#the reference level
MyGSS6$`Financial satisfaction` <- case_when(
  MyGSS6$`Financial satisfied`== 1~ "Satisfied",
  MyGSS6$`More or less`== 1 ~"More or less",
  MyGSS6$`Not at all satisfied`== 1 ~"Not at all satisfied"
)

MyGSS6$`Financial satisfaction` <- factor(MyGSS6$`Financial satisfaction`)
MyGSS6$`Financial satisfaction` <- relevel(MyGSS6$`Financial satisfaction`, 
                                           ref="Satisfied")
comment(MyGSS6$`Financial satisfaction`) <- "New variable that combines
 the financial 'satisifed', 'more or less', and 'not satisfied', and then 
 converted to a factor and set 'Satisfied' as the reference level | JB Buckner
 Rep project | 2024-11-08"
comment(MyGSS6$`Financial satisfaction`)
#-------------------------------------------------------------------------------
### For Race variable, add white, black, and other together, then set white as 
# reference level
MyGSS6$Race <- case_when(
  MyGSS6$White==1~" White ",
  MyGSS6$Black==1~" Black ",
  MyGSS6$Other==1~" Other "
)
MyGSS6$Race <- factor(MyGSS6$Race)
MyGSS6$Race <- relevel(MyGSS6$Race,ref=" White ")
comment(MyGSS6$Race) <- "New Race variable that combines the yes responses from 
 'White', 'Black' and 'Other', and makes it a factor and sets white as the 
 reference | JB Buckner Rep project | 2024-11-08"
comment(MyGSS6$Race)
#-------------------------------------------------------------------------------
### For married variables, add never married, married, widowed, and 
# divorced/separated. set never married as the reference level 
MyGSS6$`Marital status` <- case_when(
  MyGSS6$`Never married`==1~" Never married",
  MyGSS6$Married==1~" Married",
  MyGSS6$Widowed==1~" Widowed",
  MyGSS6$`Divorced/separated`==1~" Divorced/separated"
)
# tested around trying to order the factors, ultimately did not work for 
# regression models
#MyGSS6$`Marital status` <- factor(MyGSS6$`Marital status`, ordered=TRUE,
#                                  levels = c(" Never married"," Married",
#                                            " Widowed"," Divorced/separated"))

MyGSS6$`Marital status` <- factor(MyGSS6$`Marital status`)
MyGSS6$`Marital status` <- relevel(MyGSS6$`Marital status`, ref=" Never married")

comment(MyGSS6$`Marital status`) <- "New Marital status variable that combines 
 the yes responses from variables 'Never married', 'Married', 'Widowed',
 and 'Divorced/separated', converts it to a factor and sets Never Married as 
 the reference level | JB Buckner Rep Project | 2024-11-08"
comment(MyGSS6$`Marital status`)

#-------------------------------------------------------------------------------
# save as RDS to be used in next files for linear regression
saveRDS(MyGSS6, "Data.For.LR.Final.RDS")
sink()











