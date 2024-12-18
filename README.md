ReadMe file

The replication document that this readme file accompanies was constructed to produce computational replication of tables 1 & 2 in the article “How does gun violence affect Americans’ trust in each other?” by Cary Wu. The code of this project was written in R Studio version 4.4.0 (2024-04-24).

The documentation consists of one folder, with all necessary data stored within

•	Data management FP MDA: This folder contains all necessary files for replication
o	Original Data: This is the gss7222_r3a.RDS data file that contains the original data used in this paper. Imported into R to be used for the project.
o	Original Metadata: This is the GSS 2022 Codebook.pdf that contains all metadata on necessary variables for replication 
•	Processing Analysis: These are the files created throughout the replication process, used in separate R script files
o	R files: These are the script files used in the replication project for data cleaning, management, and analyses. 
o	RDS Files: These are the RDS files used to store new data subsets used throughout the replication process
o	XLSX File: This is the Excel file that contains final replication tables based on source articles tables

To reproduce the results of this paper:
1.	Download all contents of the folder "Jbuckner5 R7 Script & Log Files Repl” to your computer. Leave the contents of the folder intact. All the script files are in the correct order for running in R. Download GSS dataset into the folder as well.
2.	Open R Studio and set the folder as a working directory 
3.	Packages dplyr, tidyverse, naniar, pscl, and stargazer are used and should be downloaded 
4.	Run the code in the file based on structured order (V1-V6, Z1-Z3)

