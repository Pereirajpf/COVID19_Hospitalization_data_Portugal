# Data fitting

#Library
library(readxl)
library(dplyr)
library(fitdistrplus)

#Import data
load("data/data_covid.Rdata")
str(entry_data)

#Import functions
source("functions/distFit.R")

# Cut 0 days stays
entry_data <- subset(entry_data,entry_data$Time.stay != 0)

#Divide non-ICU and ICU patients
data_nonICU <- subset(entry_data, Type == "nonICU")
data_ICU    <- subset(entry_data, Type == "ICU")

#Colours
colourTable_Type <- list(ICU = "tomato1", nonICU = "#56B4E9")


#LoS datafitting==============================================================
#nonICU----
#Finding the best statistical distributons to fit to the data
plotdist(data_nonICU$Time.stay, hist = T, demp = T)
descdist(data_nonICU$Time.stay, boot = 1000)

#Perform the fitting
distFit_nonICU <- distFit(data_nonICU, plot_colour = colourTable_Type$nonICU, plot_title = "Non-ICu LoS distribution fitting")
#save
save(distFit_nonICU, file = "data/data_fitting/distFit_nonICU.RData")

#Fit results
distFit_nonICU$fitdist

#GoF statistics and criteria
distFit_nonICU$GoF_table

#Density plot of the data and all fitted distributions
distFit_nonICU_plot <- distFit_nonICU$plot
ggsave(
  'plots/data_fitting/distFit_nonICU.jpeg',
  plot = distFit_nonICU_plot,
  width = 16,
  height = 9,
  units = "cm",
  dpi = 600
)

#ICU----
#Finding the best statistical distributons to fit to the data
plotdist(data_ICU$Time.stay, hist = T, demp = T)
descdist(data_ICU$Time.stay, boot = 1000)

#Perform the fitting
distFit_ICU <- distFit(data_ICU, plot_colour = colourTable_Type$ICU, plot_title = "ICU LoS Distribution fitting", ylim = 102)
#save
save(distFit_ICU, file = "data/data_fitting/distFit_ICU.RData")

#Fit results
distFit_ICU$fitdist

#GoF statistics and criteria
distFit_ICU$GoF_table

#Density plot of the data and all fitted distributions
distFit_ICU_plot <- distFit_ICU$plot
ggsave(
  'plots/data_fitting/distFit_ICU.jpeg',
  plot = distFit_ICU_plot,
  width = 16,
  height = 9,
  units = "cm",
  dpi = 600
)


#TuD datafitting==============================================================
#Divide non-ICU and ICU patients
data_death_nonICU <- subset(entry_data, Type == "nonICU" & Outcome == "Deceased")
data_death_ICU    <- subset(entry_data, Type == "ICU" & Outcome == "Deceased")

#nonICU----
#Perform the fitting
distFit_TuD_nonICU <- distFit(data_death_nonICU,plot_colour = colourTable_Type$nonICU, plot_title = "Non-ICU TuD Distribution fitting")
#save
save(distFit_TuD_nonICU , file = "data/data_fitting/distFit_TuD_nonICU .RData")

#Fit results
distFit_TuD_nonICU$fitdist

#GoF statistics and criteria
distFit_TuD_nonICU$GoF_table

#Density plot of the data and all fitted distributions
distFit_TuD_nonICU_plot <- distFit_TuD_nonICU$plot
ggsave(
  'plots/data_fitting/distFit_TuD_nonICU.jpeg',
  plot = distFit_TuD_nonICU_plot,
  width = 16,
  height = 9,
  units = "cm",
  dpi = 600
)


#ICU----
#Perform the fitting
distFit_TuD_ICU <- distFit(data_death_ICU, plot_colour = colourTable_Type$ICU, plot_title = "ICU TuD Distribution fitting",ylim = 102)
#save
save(distFit_TuD_ICU  , file = "data/data_fitting/distFit_TuD_ICU.RData")

#Fit results
distFit_TuD_ICU $fitdist

#GoF statistics and criteria
distFit_TuD_ICU $GoF_table

#Density plot of the data and all fitted distributions
distFit_TuD_ICU_plot <- distFit_TuD_ICU$plot
ggsave(
  'plots/data_fitting/distFit_TuD_ICU.jpeg',
  plot = distFit_TuD_ICU_plot,
  width = 16,
  height = 9,
  units = "cm",
  dpi = 600
)

#NOT WORKING
#Age datafitting==============================================================
#nonICU
distFit_Age_nonICU <- distFit(data_ICU, var = "Age", plot_colour = colourTable_Type$nonICU, plot_title = "Distribution fitting non-ICU")
#save
save(distFit_Age_nonICU, file = "data/data_fitting/distFit_Age_nonICU.RData")

#Fit results
distFit_Age_nonICU$fitdist

#GoF statistics and criteria
distFit_Age_nonICU$GoF_table

#Density plot of the data and all fitted distributions
distFit_Age_nonICU_plot <- distFit_Age_nonICU$plot
ggsave(
  'plots/data_fitting/distFit_Age_nonICU.jpeg',
  plot = distFit_Age_nonICU_plot,
  width = 16,
  height = 9,
  units = "cm",
  dpi = 600
)