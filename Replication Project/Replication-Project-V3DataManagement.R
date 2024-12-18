# Metadata ---------------------------------------------------------------------

# Code written by: JB Buckner
# Last Update: 2024-10-16
# Filename: Replication-Project-V3DataManagement.R

# Purpose of this script: Begin managing data by adding constructed variables 
# based on the source variables
# 


# Setup ------------------------------------------------------------------------


rm(list=ls())
options(width = 80)

filename <-"Replication-Project-V3DataManagement"

sink(paste(filename, ".log", sep = "")) # create log file to store output



library(dplyr)
#Import the RDS data from script file#2
MyGSS3 <- readRDS("GSS.Data_to_be_cleaned.RDS")

#Begin Data Management
#Rename variables for easier descriptions, new dataset for next steps
MyGSS3 <- MyGSS3 %>% 
  rename("People_can_be_trusted" = trust,
         "People_are_helpful" = helpful,
         "People_are_fair" = fair,
         "Family_income_at_16" = incom16,
         "Living_in_mixed_neighborhood" = raclive,
         "Education_in_years" = educ,
         "Family_income_in_constant dollars" = coninc,
         "Financial_satisfaction" = satfin,
         "Gender_" = sex,
         "Age_" = age,
         "Race_" = race,
         "Marriage_status_" = marital
  )

#-------------GUN VIOLENCE VARIABLES--------------------------------------------

#Create a new variable "gun_threat" that changes the responses "2"(No) from the 
# variable "gun", and change them to a 0 to be used in next step
MyGSS3$gun2 <- ifelse(
  MyGSS3$gun == 2, 0,  # If gun == 2, set gun_threat to 0
  MyGSS3$gun    # Otherwise, keep the existing value of gun
)
comment(MyGSS3$gun2) <- "Variable gun2 that reflects if someone have been shot 
with a gun, 0=no, 1=yes"
comment(MyGSS3$gun2)

#-------------------------------------------------------------------------------
#Create new variable "Threatened with gun", that combines the responses "0"
# from the variable "gun2". These respond to the answer "No".
#Have to combine the responses with the variable "gunage.
MyGSS3$`Threatened with gun` <- case_when(
  MyGSS3$gun2 == 0  ~ "No",
  MyGSS3$gunage == 1 ~ "As Child",
  MyGSS3$gunage == 2 ~ "As Adult",
  MyGSS3$gunage == 3 ~ "Both", 
)

comment(MyGSS3$`Threatened with gun`) <- "New variable gun_threatened that 
 reflects if a person has been threatened with a gun, and when"
comment(MyGSS3$`Threatened with gun`)
  
table(MyGSS3$`Threatened with gun`) #Check numbers for threatened with a gun to 
# compare to article table

#----------- GENERALIZED TRUST VARIABLES----------------------------------------

# 1 Create new variable for can people_be_trusted (0=no, 1=yes)
#There are observations "3" that reflect the answer "depends", I have decided to 
# count those as "No"(0).
MyGSS3$`People can be trusted (0=no,1=yes)` <- ifelse(
  MyGSS3$People_can_be_trusted==1,1,0)

comment(MyGSS3$`People can be trusted (0=no,1=yes)`) <-
"New variable `Can people be trusted`, this relects that 0=no and 1=yes |
2024-10-09"
comment(MyGSS3$`People can be trusted (0=no,1=yes`)

table(MyGSS3$`People can be trusted (0=no,1=yes`)
#test accuracy of "yes responses"
8040/(11934+8040) # =  40.25%, articles is 41.46%

#-------------------------------------------------------------------------------
# 2 New variable for people are helpful.If the original responses in "People_are
# _helpful" are a 1, then it will stay a 1 and every other will become a 0,
#There are observations "3" that reflect the answer "depends", I have decided to 
# count those as "No"(0).
MyGSS3$`People are helpful (0=no,1=yes)` <- ifelse(MyGSS3$People_are_helpful==
                                                     1,1,0)

comment(MyGSS3$`People are helpful (0=no,1=yes)`) <- "New variable 
`People are helpful`,this relects that 0=no and 1=yes | 2024-10-09"
comment(MyGSS3$`People are helpful (0=no,1=yes)`)

table(MyGSS3$`People are helpful (0=no,1=yes)`)
#again test accuracy
10544/(10544+10170) # = 50.9%, articles is 51.69%
#-------------------------------------------------------------------------------
# 3 New variable for people_are_fair. If the original responses = 2 
# (try to be fair), then they will become a 1=yes in the new variable column.
#There are observations "3" that reflect the answer "depends", I have decided to 
# count those as "No"(0).
MyGSS3$`People are fair (0=no,1=yes)` <- ifelse(MyGSS3$People_are_fair==2,1,0)
comment(MyGSS3$`People are fair (0=no,1=yes)`) <-"New variable 
`People are fair`, this relects that 0=no and 1=yes | 2024-10-09"
comment(MyGSS3$`People are fair (0=no,1=yes)`)

table(MyGSS3$`People are fair (0=no,1=yes)`)
#Check for accuracy
12215/(12215+8455) # = 59.10%, articles is 60.18%
#--------FAMILY AND NEIGHBORHOOD BACKGROUND VARIABLES---------------------------

#Family Income when 16 years old (scale 1-5). There are observations that are 
# labeled as 7 and outside the range. I will have to remove them and label them 
# as NA. Will create another variable `Family income at 16` , a cleaner
# variable name for my descriptive statistics table
# statistics and regression
table(MyGSS3$Family_income_at_16)
MyGSS3$`Family income at 16 (scale,1-5)` <- ifelse(MyGSS3$Family_income_at_16 == 
                                                     7, NA,
                                     MyGSS3$Family_income_at_16)
#Check that new variable does not contain 7s
table(MyGSS3$`Family income at 16 (scale,1-5)`)
mean(MyGSS3$`Family income at 16 (scale,1-5)`,na.rm=T)

#-----------------------------------------------------------------------
# New variable for if living in a mixed neighborhood. Original observations
# are as 1=yes, 2=no. I will have to change 2 ot a 0 in the new variable.
table(MyGSS3$Living_in_mixed_neighborhood)
MyGSS3$`Living in mixed neighborhood (0=no,1=yes)` <- 
  ifelse(MyGSS3$Living_in_mixed_neighborhood==2,
                     0, MyGSS3$Living_in_mixed_neighborhood)
#Check to ensure the "2" responses became "0"
table(MyGSS3$`Living in mixed neighborhood (0=no,1=yes)`)
mean(MyGSS3$`Living in mixed neighborhood (0=no,1=yes)`,na.rm=T)

#-------------------------------------------------------------------------------
# Save the data so far with the new variables, and rename it to MyGSS4

comment(MyGSS3) <- "Saved dataset with new accurate variables for Generalized
trust and Family and neighborhood background. The rest of the variables for 
Socioeconomic achievement and Demographics will be cleaned and added within the 
fourth script file | JB Buckner | 2024-10-16"
comment(MyGSS3)

saveRDS(MyGSS3, "GSS.Data_to_be_cleanedV2.RDS")

sink()



