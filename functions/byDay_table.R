# Creating the by day table

byDay_table <- function(data){
  #Libraries
  library(dplyr)
  library(lubridate)
  
  #subset by icu and Hosp
  data_ICU <- subset(data,data$Type == 'ICU')
  data_nonICU <- subset(data,data$Type == 'nonICU')
  
  #Df initialization
  start_date <- min(data$Entry.date)
  #start_date <- as.Date("01/03/2020",  format = "%d/%m/%Y")
  end_date <- max(data$Entry.date)
  data_byday<- data.frame(date = seq(start_date, end_date, by="days"))
  
  #All entryes
  entries_day_all <- as.data.frame(table(data$Entry.date))
  if(nrow(entries_day_all)!=0){
    entries_day_all$Var1 <- as.Date(entries_day_all$Var1)
  } else {
    entries_day_all <- data.frame(Var1=NA,Freq=NA)
  }
  colnames(entries_day_all) <- c("date","entries")
  entries_day_all <- entries_day_all[order(entries_day_all$date),]
  
  #All discharges
  discharges_day_all <- as.data.frame(table(data$Discharge.date))
  if(nrow(discharges_day_all)!=0){
    discharges_day_all$Var1 <- as.Date(discharges_day_all$Var1)
  } else {
    discharges_day_all <- data.frame(Var1=NA,Freq=NA)
  }
  colnames(discharges_day_all) <- c("date","discharges")
  discharges_day_all <- discharges_day_all[order(entries_day_all$date),]
  
  # hospitalization entries
  entries_day_nonICU <- as.data.frame(table(data_nonICU$Entry.date))
  if(nrow(entries_day_nonICU)!=0){
    entries_day_nonICU$Var1 <- as.Date(entries_day_nonICU$Var1)
  } else {
    entries_day_nonICU <- data.frame(Var1=NA,Freq=NA)
  }
  colnames(entries_day_nonICU) <- c("date","entries_nonICU")
  entries_day_nonICU <- entries_day_nonICU[order(entries_day_nonICU$date),]
  
  # hospitalization discharges
  discharges_day_nonICU <- as.data.frame(table(data_nonICU$Discharge.date))
  if(nrow(discharges_day_nonICU)!=0){
    discharges_day_nonICU$Var1 <- as.Date(discharges_day_nonICU$Var1)
  } else {
    discharges_day_nonICU <- data.frame(Var1=NA,Freq=NA)
  }
  colnames(discharges_day_nonICU) <- c("date","discharges_nonICU")
  discharges_day_nonICU <- discharges_day_nonICU[order(entries_day_nonICU$date),]
  
  # icu entries
  entries_day_ICU <- data.frame(table(data_ICU$Entry.date))
  if(nrow(entries_day_ICU)!=0){
    entries_day_ICU$Var1 <- as.Date(entries_day_ICU$Var1)
  } else {
    entries_day_ICU <- data.frame(Var1=NA,Freq=NA)
  }
  colnames(entries_day_ICU) <- c("date","entries_ICU")
  entries_day_ICU <- entries_day_ICU[order(entries_day_ICU$date),]
  
  # icu discharges
  discharges_day_ICU <- as.data.frame(table(data_ICU$Discharge.date))
  if(nrow(discharges_day_ICU)!=0){
    discharges_day_ICU$Var1 <- as.Date(discharges_day_ICU$Var1)
  } else {
    discharges_day_ICU <- data.frame(Var1=NA,Freq=NA)
  }
  colnames(discharges_day_ICU) <- c("date","discharges_ICU")
  discharges_day_ICU <- discharges_day_ICU[order(discharges_day_ICU$date),]
  
  # hospitalization deaths
  deaths_day_nonICU <- as.data.frame(table(data_nonICU[data_nonICU$Outcome=="Deceased",]$Discharge.date))
  if(nrow(deaths_day_nonICU)!=0){
    deaths_day_nonICU$Var1 <- as.Date(deaths_day_nonICU$Var1)
  } else {
    deaths_day_nonICU <- data.frame(Var1=NA,Freq=NA)
  }
  colnames(deaths_day_nonICU) <- c("date","deaths_nonICU")
  deaths_day_nonICU <- deaths_day_nonICU[order(deaths_day_nonICU$date),]
  
  
  # icu deaths
  deaths_day_ICU <- as.data.frame(table(data_ICU[data_ICU$Outcome=="Deceased",]$Discharge.date))
  if(nrow(deaths_day_ICU)!=0){
    deaths_day_ICU$Var1 <- as.Date(deaths_day_ICU$Var1)
  } else {
    deaths_day_ICU <- data.frame(Var1=NA,Freq=NA)
  }
  colnames(deaths_day_ICU) <- c("date","deaths_ICU")
  deaths_day_ICU <- deaths_day_ICU[order(deaths_day_ICU$date),]
  
  #Joining data ================================================================
  #Joining All data
  data_byday <- data_byday %>% left_join(entries_day_all,by="date")
  data_byday <- data_byday %>% left_join(discharges_day_all,by="date")
  data_byday[is.na(data_byday)] <- 0
  
  #jJoining Hosp data
  data_byday <- data_byday %>% left_join(entries_day_nonICU,by="date")
  data_byday <- data_byday %>% left_join(discharges_day_nonICU,by="date")
  data_byday[is.na(data_byday)] <- 0
  
  #entries - discharges Hosp
  data_byday$occupancy_nonICU <- (cumsum(data_byday$entries_nonICU) - cumsum(data_byday$discharges_nonICU))
  
  #Joining ICU data
  data_byday <- data_byday %>% left_join(entries_day_ICU,by="date")
  data_byday <- data_byday %>% left_join(discharges_day_ICU,by="date")
  data_byday[is.na(data_byday)] <- 0
  
  #entries - discharges ICU
  data_byday$occupancy_ICU <- (cumsum(data_byday$entries_ICU) - cumsum(data_byday$discharges_ICU))
  
  data_byday <- data_byday %>% left_join(deaths_day_nonICU,by="date")
  data_byday <- data_byday %>% left_join(deaths_day_ICU,by="date")
  data_byday[is.na(data_byday)] <- 0
  
  #sum deaths hosp
  data_byday$sum_deaths_nonICU <- cumsum(data_byday$deaths_nonICU)
  #sum deaths icu
  data_byday$sum_deaths_ICU <- cumsum(data_byday$deaths_ICU)
  
  # Reorder and rename columns ====
  #data_byday <- data_byday %>% select(1,2,3,10,4,5,11,6,7,8,9)  
  #colnames(data_byday) <- c("date")
  
  
  data_byday
}
