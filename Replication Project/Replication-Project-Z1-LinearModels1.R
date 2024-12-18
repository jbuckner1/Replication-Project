# Metadata ---------------------------------------------------------------------

# Code written by: JB Buckner
# Last Update: 2024-10-16
# Filename: Replication-Project-Z1-LinearModels1.R

# Purpose of this script: Begin running linear regression models in an attempt
# to replicate the authors numbers



# Setup ------------------------------------------------------------------------


rm(list=ls())
options(width = 80)

filename <-"Replication-Project-Z1-LinearModels1"

sink(paste(filename, ".log", sep = "")) # create log file to store output



library(tidyverse)
#Install package pscl
library(pscl)
#Install stargazer
library(stargazer)

#-------------------------------------------------------------------------------
# Load in final dataset for linear regression and test accuracy of linear 
# regression models
MyGSS7 <- readRDS("Data.For.LR.Final.RDS")

#-------------------------------------------------------------------------------
#Begin replicating models 1-4 for "People can be trusted" based on articles 
# linear regression tables
model_1 <- glm(`People can be trusted`~ 
                 `Ever threatened with a gun or shot at?`+ 
                 `Female`+ `Age in years`+ `Race`+`Marital status`+
                 `region`+`year`,data=MyGSS7)
 
summary(model_1)
# Number of observations
N_model_1 <- nobs(model_1)
comment(model_1) <- "Model 1 for People can be trusted that includes the 
 variables: Ever threatened with a gun or shot at?, Female, Age in years, 
 Race, Marital status, Region and Year | JB Buckner |
 Replication Project 2024-11-09"
comment(model_1)
# use stargazer to grab the estimates more accurately, summary will be used for 
# the t-statsitics 
stargazer(model_1, type = "text", 
          star.cutoffs = c(0.05, 0.01, 0.001), 
          title = "Logistic Regression: Trust in People")
#Calculate the Pseudo R-sq
pR2(model_1)["McFadden"]
#------------------Model 2------------------------------------------------------
model_2 <- glm(`People can be trusted`~ `Ever threatened with a gun or shot at?`+
                 `Family income when 16 years old`+
                 `Living in mixed neighborhood`+ `Female`+ `Age in years` +
                 `Race` + `Marital status`+ `region`+`year`,data=MyGSS7)
summary(model_2)
N_model_2 <- nobs(model_2)
comment(model_2) <-  "Model 2 for People can be trusted that includes the
 variables: Ever threatened with a gun or shot at?, Family income when 16 years 
 old, Living in mixed neighborhood, Female, Age in years, Race,
 Marital status, Region and Year | JB Buckner |
 Replication Project 2024-11-09"
comment(model_2)
# use stargazer to grab the estimates more accurately, summary will be used for 
# the t-statsitics 
stargazer(model_2, type = "text", 
          star.cutoffs = c(0.05, 0.01, 0.001), 
          title = "Logistic Regression: Trust in People")
#Calculate the Pseudo R-sq
pR2(model_2)["McFadden"]
#------------------------Model 3------------------------------------------------
model_3 <- glm(`People can be trusted`~ `Ever threatened with a gun or shot at?`+
                 `Education in years`+ `Family income in constant dollars`+
                 `Financial satisfaction` + `Female`+`Age in years` + `Race` +
                 `Marital status`+`region`+`year`,data=MyGSS7)
summary(model_3)
N_model_3 <- nobs(model_3)
comment(model_3) <- "Model 3 for People can be trusted that includes the 
 variables: Ever threatened with a gun or shot at?, Education in years, Family 
 income in constant dollars, Financial satisfaction, Female, Age in years, 
 Race, Marital status, Region and Year | JB Buckner | 
 Replication Project 2024-11-09"
comment(model_3)
# use stargazer to grab the estimates more accurately, summary will be used for 
# the t-statsitics 
stargazer(model_3, type = "text", 
          star.cutoffs = c(0.05, 0.01, 0.001), 
          title = "Logistic Regression: Trust in People")

#Calculate the Pseudo R-sq
pR2(model_3)["McFadden"]
#-----------------Model 4-------------------------------------------------------
model_4 <- glm(`People can be trusted`~ `Ever threatened with a gun or shot at?`+
  `Family income when 16 years old`+ `Living in mixed neighborhood`+
    `Education in years`+ `Family income in constant dollars`+
    `Financial satisfaction` + `Female`+`Age in years` +`Race`+ 
    `Marital status`+`region`+`year`,data=MyGSS7)
summary(model_4)
#Calculate # observations
N_model_4 <- nobs(model_4)
comment(model_4) <- "Model 4 for People can be trusted. It includes all possible
 variables for the model | JB Buckner | Replication project 2024-11-09"

# use stargazer to grab the estimates more accurately, summary will be used for 
# the t-statsitics 
stargazer(model_4, type = "text", 
          star.cutoffs = c(0.05, 0.01, 0.001), 
          title = "Logistic Regression: Trust in People")
#Calculate the Pseudo R-sq
pR2(model_4)["McFadden"]

#-------------------------------------------------------------------------------
sink()






