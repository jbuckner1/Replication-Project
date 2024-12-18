# Metadata ---------------------------------------------------------------------

# Code written by: JB Buckner
# Last Update: 2024-10-16
# Filename:Replication-Project-Z2-LinearModels2.Rr

# Purpose of this script: Begin running linear regression models in an attempt
# to replicate the authors numbers



# Setup ------------------------------------------------------------------------
rm(list=ls())
options(width = 80)

filename <-"Replication-Project-Z2-LinearModels2"

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
#Begin replicating models 1-4 for "People are helpful" based on articles 
# linear regression tables
model_5 <- glm(`People are helpful`~ 
                 `Ever threatened with a gun or shot at?`+ 
                 `Female`+ `Age in years`+ `Race`+`Marital status`+
                 `region`+`year`,data=MyGSS7)

summary(model_5)
N_model_5 <- nobs(model_5)
comment(model_5) <- "Model 5 for People are helpful that includes the 
 variables: Ever threatened with a gun or shot at?, Female, Age in years, 
 Race, Marital status, Region and Year | JB Buckner |
 Replication Project 2024-11-09"
comment(model_5)
# use stargazer to grab the estimates more accurately, summary will be used for 
# the t-statsitics 
stargazer(model_5, type = "text", 
          star.cutoffs = c(0.05, 0.01, 0.001), 
          title = "Logistic Regression: Trust in People")
#Calculate the Pseudo R-sq
pR2(model_5)["McFadden"]

#------------------Model 2------------------------------------------------------
model_6 <- glm(`People are helpful`~ `Ever threatened with a gun or shot at?`+
                 `Family income when 16 years old`+
                 `Living in mixed neighborhood`+ `Female`+ `Age in years` +
                 `Race` + `Marital status`+ `region`+`year`,data=MyGSS7)
summary(model_6)
N_model_6 <- nobs(model_6)
comment(model_6) <-  "Model 6 for People are helpful that includes the
 variables: Ever threatened with a gun or shot at?, Family income when 16 years 
 old, Living in mixed neighborhood, Female, Age in years, Race,
 Marital status, Region and Year | JB Buckner |
 Replication Project 2024-11-09"
comment(model_6)
# use stargazer to grab the estimates more accurately, summary will be used for 
# the t-statsitics 
stargazer(model_6, type = "text", 
          star.cutoffs = c(0.05, 0.01, 0.001), 
          title = "Logistic Regression: Trust in People")
#Calculate the Pseudo R-sq
pR2(model_6)["McFadden"]

#------------------------Model 3------------------------------------------------
model_7 <- glm(`People are helpful`~ `Ever threatened with a gun or shot at?`+
                 `Education in years`+ `Family income in constant dollars`+
                 `Financial satisfaction` + `Female`+`Age in years` + `Race` +
                 `Marital status`+`region`+`year`,data=MyGSS7)
summary(model_7)
N_model_7 <- nobs(model_7)
comment(model_7) <- "Model 7 for People are helpful that includes the 
 variables: Ever threatened with a gun or shot at?, Education in years, Family 
 income in constant dollars, Financial satisfaction, Female, Age in years, 
 Race, Marital status, Region and Year | JB Buckner | 
 Replication Project 2024-11-09"
comment(model_7)
stargazer(model_7, type = "text", 
          star.cutoffs = c(0.05, 0.01, 0.001), 
          title = "Logistic Regression: Trust in People")
#Calculate the Pseudo R-sq
pR2(model_7)["McFadden"]

#-----------------Model 4-------------------------------------------------------
model_8 <- glm(`People are helpful`~ `Ever threatened with a gun or shot at?`+
                 `Family income when 16 years old`+ `Living in mixed neighborhood`+
                 `Education in years`+ `Family income in constant dollars`+
                 `Financial satisfaction` + `Female`+`Age in years` +`Race`+ 
                 `Marital status`+`region`+`year`,data=MyGSS7)
summary(model_8)
N_model_8 <- nobs(model_8)
comment(model_8) <- "Model 8 for People are helfpul. It includes all possible
 variables for the model | JB Buckner | Replication project 2024-11-09"
comment(model_8)
stargazer(model_8, type = "text", 
          star.cutoffs = c(0.05, 0.01, 0.001), 
          title = "Logistic Regression: Trust in People")
#Calculate the Pseudo R-sq
pR2(model_8)["McFadden"]

sink()






