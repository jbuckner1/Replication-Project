# Metadata ---------------------------------------------------------------------

# Code written by: JB Buckner
# Last Update: 2024-10-16
# Filename: Replication-Project-V5Analyses.R

# Purpose of this script: Begin testing the accuracy of the descriptive 
#statistics grouped by variable 'Threatened with gun'
# 


# Setup ------------------------------------------------------------------------


rm(list=ls())
options(width = 80)

filename <-"Replication-Project-V5Analyses"

sink(paste(filename, ".log", sep = "")) # create log file to store output



library(tidyverse)
#Import the RDS data from script file 4, and rename in MyGSS5
MyGSS5 <- readRDS("GSS.DescriptiveStatisticsV5.RDS")

#-------------------------------------------------------------------------------
#Check accuracy of means for GENERALIZED TRUST----------------------------------
#CAN PEOPLE BE TRUSTED

TrustMeans <- MyGSS5 %>%
  filter(!is.na(`Threatened with gun`)) %>%
  mutate(`Threatened with gun` = factor(`Threatened with gun`, 
                                        levels = c("No", "As Adult", 
                                                   "As Child", "Both"))) %>%
  group_by(`Threatened with gun`) %>%
  summarize(mean_trust = mean(`People can be trusted (0=no,1=yes)`, 
                              na.rm = TRUE) * 100)

# Calculate total mean (without grouping)
trust_total_mean <- MyGSS5 %>%
  summarize(`Threatened with gun` = "Total", 
            mean_trust = mean(`People can be trusted (0=no,1=yes)`,
                              na.rm = TRUE) * 100)

# Combine the two dataframes
trust_combined_data <- bind_rows(TrustMeans, trust_total_mean)

# Reshape the data and include variable names on the left
trust_final_result <- trust_combined_data %>%
  pivot_wider(names_from = `Threatened with gun`, values_from = mean_trust) %>%
  mutate(Variable = "People can be trusted (0=no,1=yes)") %>%  
  select(Variable, everything())  # Reorder columns to have Variable first

trust_final_result

#PEOPLE ARE HELPFUL----------------------------------------------------------------
Helpfulmeans <- MyGSS5 %>%
  filter(!is.na(`Threatened with gun`)) %>%
  mutate(`Threatened with gun` = factor(`Threatened with gun`, 
                                        levels = c("No", "As Adult", 
                                                   "As Child", "Both"))) %>%
  group_by(`Threatened with gun`) %>%
  summarize(mean_trust = mean(`People are helpful (0=no,1=yes)`, 
                              na.rm = TRUE) * 100)

# Calculate total mean (without grouping)
helpful_total_mean <- MyGSS5 %>%
  summarize(`Threatened with gun` = "Total", 
            mean_trust = mean(`People are helpful (0=no,1=yes)`,
                              na.rm = TRUE) * 100)

# Combine the two dataframes
helpful_combined_data <- bind_rows(Helpfulmeans, helpful_total_mean)

# Reshape the data and include variable names on the left
helpful_final_result <- helpful_combined_data %>%
  pivot_wider(names_from = `Threatened with gun`, values_from = mean_trust) %>%
  mutate(Variable = "People are helpful (0=no,1=yes)") %>%  
  select(Variable, everything())  # Reorder columns to have Variable first

helpful_final_result

#PEOPLE ARE FAIR----------------------------------------------------------------
Fairmeans <- MyGSS5 %>%
  filter(!is.na(`Threatened with gun`)) %>%
  mutate(`Threatened with gun` = factor(`Threatened with gun`, 
                                        levels = c("No", "As Adult", 
                                                   "As Child", "Both"))) %>%
  group_by(`Threatened with gun`) %>%
  summarize(mean_trust = mean(`People are fair (0=no,1=yes)`, 
                              na.rm = TRUE) * 100)

# Calculate total mean (without grouping)
fair_total_mean <- MyGSS5 %>%
  summarize(`Threatened with gun` = "Total", 
            mean_trust = mean(`People are fair (0=no,1=yes)`,
                              na.rm = TRUE) * 100)

# Combine the two dataframes
fair_combined_data <- bind_rows(Fairmeans, fair_total_mean)

# Reshape the data and include variable names on the left
fair_final_result <- fair_combined_data %>%
  pivot_wider(names_from = `Threatened with gun`, values_from = mean_trust) %>%
  mutate(Variable = "People are fair (0=no,1=yes)") %>%  
  select(Variable, everything())  # Reorder columns to have Variable first

fair_final_result

#TRY TO COMBINE THESE

'Generalized trust' <- rbind(trust_final_result,helpful_final_result,
                             fair_final_result)

`Generalized trust`

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#USE A FOR LOOP TP GENERATE A DESC TABLE WITH THE MEANS FOR ALL VARIABLES

############
# Identify binary and non-binary variables in MyGSS5
binary_vars <- names(select(MyGSS5, where(~ all(. %in% c(0, 1, NA)))))
non_binary_vars <- setdiff(names(MyGSS5), binary_vars)  # Automatically identify non-binary variables

# Exclude specific unwanted variables
excluded_vars <- c("year", "region", "Threatened with gun")
binary_vars <- setdiff(binary_vars, excluded_vars)
non_binary_vars <- setdiff(non_binary_vars, excluded_vars)

# Initialize data frames to store results
desc_table <- as.data.frame(matrix(NA, nrow = length(binary_vars) + length(non_binary_vars), ncol = 6))
colnames(desc_table) <- c("Variable", "No", "As Adult", "As Child", "Both", "Total")

# Set the levels for "Threatened with gun"
threatened_vals <- c("No", "As Adult", "As Child", "Both")

# Calculate means for binary variables (multiplied by 100)
for (var in 1:length(binary_vars)) {
  temp_name <- binary_vars[var]
  
  for (threat in 1:length(threatened_vals)) {
    temp_var <- MyGSS5[MyGSS5$`Threatened with gun` == threatened_vals[threat], temp_name]
    mean_value <- mean(temp_var, na.rm = TRUE) * 100  # Multiply by 100 for binary
    desc_table[var, threat + 1] <- mean_value
  }
  
  desc_table[var, 1] <- temp_name
  desc_table[var, 6] <- mean(MyGSS5[[temp_name]], na.rm = TRUE) * 100  # Total mean
}

# Start filling in from where binary variables end for non-binary variables
start_index_non_binary <- length(binary_vars) + 1

# Calculate means for non-binary variables (without multiplying by 100)
for (var in 1:length(non_binary_vars)) {
  temp_name <- non_binary_vars[var]
  
  # Ensure the column is numeric, converting if necessary
  MyGSS5[[temp_name]] <- as.numeric(MyGSS5[[temp_name]])
  
  for (threat in 1:length(threatened_vals)) {
    temp_var <- MyGSS5[MyGSS5$`Threatened with gun` == threatened_vals[threat], temp_name]
    mean_value <- mean(temp_var, na.rm = TRUE)  # No multiplication for non-binary
    desc_table[start_index_non_binary + var - 1, threat + 1] <- mean_value
  }
  
  desc_table[start_index_non_binary + var - 1, 1] <- temp_name  # Store variable name
  desc_table[start_index_non_binary + var - 1, 6] <- mean(MyGSS5[[temp_name]], 
                                                          na.rm = TRUE)  # Total mean
}

# View the results
desc_table



comment(desc_table) <- "Data table that contains the descriptive statistics 
 means for all variables, grouped by Threated with Gun as well as Total |
 Non-binary variables were stored at bottom rather than in order, will be 
 corrected in Google Sheets | JBuckner5 | sts3300 2024-10-27"
comment(desc_table)
write.csv(desc_table , file = "FP_Desc_Stats_Table.csv")

#Check for N
#Get # of responses for Threatened by gun,"No, Yes As Adult, Yes as child, Both"
table(MyGSS5$`Threatened with gun`)
sink()
