# Script to create LoS statistical parameters tables

#Library
library(readxl)
library(dplyr)

#Import data
load("data/data_covid.Rdata")
str(entry_data)


# 1 # LoS statistical distribution analysis=====================================
#Import functions
source("functions/LoSstats_table.R")

# For LoS-----------------------------------------------------------------------
LoS_stats <- LoSstats_table(entry_data)
write.csv2(LoS_stats,'data/LoS_tables/LoS_stats.csv')
save(LoS_stats, file = "data/LoS_tables/LoS_stats.RData")

#For TuD------------------------------------------------------------------------
TuD_stats <- LoSstats_table(filter(entry_data,entry_data$Outcome == "Deceased"))
write.csv2(TuD_stats,'data/LoS_tables/TuD_stats.csv')
save(TuD_stats, file = "data/LoS_tables/TuD_stats.RData")

#For TuDisc---------------------------------------------------------------------
TuDisc_stats <- LoSstats_table(filter(entry_data,entry_data$Outcome == "Discharged"))
write.csv2(TuDisc_stats,'data/LoS_tables/TuDisc_stats.csv')
save(TuDisc_stats, file = "data/LoS_tables/TuDisc_stats.RData")


# 2 # Entries, fatalities, population numbers and related percentages===========
#Import Population numbers table
load("data/Pop_table/Pop_table.Rdata")

#Import functions
source("functions/PopVar_table.R")

df_PopVar <- PopVar_table(entry_data,Pop_table)
write.csv2(df_PopVar,'data/PopVar_table.csv')
save(df_PopVar, file = "data/PopVar_table.RData")

# 3 # By day table==============================================================
#Import functions
source("functions/byDay_table.R")

df_byDay <- byDay_table(entry_data)
write.csv2(df_byDay,'data/df_byDay.csv')
save(df_byDay, file = "data/df_byDay.RData")
