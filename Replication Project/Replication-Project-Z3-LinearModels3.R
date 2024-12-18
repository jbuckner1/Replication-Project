# Metadata ---------------------------------------------------------------------

# Code written by: JB Buckner
# Last Update: 2024-10-16
# Filename:Replication-Project-Z3-LinearModels3.R

# Purpose of this script: Begin running linear regression models in an attempt
# to replicate the authors numbers



# Setup ------------------------------------------------------------------------
rm(list=ls())

options(width = 80)

filename <-"Replication-Project-Z3-LinearModels3"

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
#Begin replicating models 1-4 for "People are fair " based on articles 
# linear regression tables
model_9 <- glm(`People are fair`~ 
                 `Ever threatened with a gun or shot at?`+ 
                 `Female`+ `Age in years`+ `Race`+`Marital status`+
                 `region`+`year`,data=MyGSS7)

summary(model_9)
N_model_9 <- nobs(model_9)
comment(model_9) <- "Model 9 for People are fair that includes the 
 variables: Ever threatened with a gun or shot at?, Female, Age in years, 
 Race, Marital status, Region and Year | JB Buckner |
 Replication Project 2024-11-09"
comment(model_9)
stargazer(model_9, type = "text", 
          star.cutoffs = c(0.05, 0.01, 0.001), 
          title = "Logistic Regression: Trust in People")
#Calculate the Pseudo R-sq
pR2(model_9)["McFadden"]

#------------------Model 2------------------------------------------------------
model_10 <- glm(`People are fair`~ `Ever threatened with a gun or shot at?`+
                 `Family income when 16 years old`+
                 `Living in mixed neighborhood`+ `Female`+ `Age in years` +
                 `Race` + `Marital status`+ `region`+`year`,data=MyGSS7)
summary(model_10)
N_model_10 <- nobs(model_10)
comment(model_10) <-  "Model 10 for People are fair that includes the
 variables: Ever threatened with a gun or shot at?, Family income when 16 years 
 old, Living in mixed neighborhood, Female, Age in years, Race,
 Marital status, Region and Year | JB Buckner |
 Replication Project 2024-11-09"
comment(model_10)
stargazer(model_10, type = "text", 
          star.cutoffs = c(0.05, 0.01, 0.001), 
          title = "Logistic Regression: Trust in People")
#Calculate the Pseudo R-sq
pR2(model_10)["McFadden"]

#------------------------Model 3------------------------------------------------
model_11 <- glm(`People are fair`~ `Ever threatened with a gun or shot at?`+
                 `Education in years`+ `Family income in constant dollars`+
                 `Financial satisfaction` + `Female`+`Age in years` + `Race` +
                 `Marital status`+`region`+`year`,data=MyGSS7)
summary(model_11)
N_model_11 <- nobs(model_11)
comment(model_11) <- "Model 11 for People are fair that includes the 
 variables: Ever threatened with a gun or shot at?, Education in years, Family 
 income in constant dollars, Financial satisfaction, Female, Age in years, 
 Race, Marital status, Region and Year | JB Buckner | 
 Replication Project 2024-11-09"
comment(model_11)
stargazer(model_11, type = "text", 
          star.cutoffs = c(0.05, 0.01, 0.001), 
          title = "Logistic Regression: Trust in People")
#Calculate the Pseudo R-sq
pR2(model_11)["McFadden"]

#-----------------Model 4-------------------------------------------------------
model_12 <- glm(`People are fair`~ `Ever threatened with a gun or shot at?`+
                 `Family income when 16 years old`+ `Living in mixed neighborhood`+
                 `Education in years`+ `Family income in constant dollars`+
                 `Financial satisfaction` + `Female`+`Age in years` +`Race`+ 
                 `Marital status`+`region`+`year`,data=MyGSS7)
summary(model_12)
N_model_12 <- nobs(model_12)
comment(model_12) <- "Model 12 for People are fair. It includes all possible
 variables for the model | JB Buckner | Replication project 2024-11-09"
comment(model_12)
stargazer(model_12, type = "text", 
          star.cutoffs = c(0.05, 0.01, 0.001), 
          title = "Logistic Regression: Trust in People")
#Calculate the Pseudo R-sq
pR2(model_12)["McFadden"]




sink()






