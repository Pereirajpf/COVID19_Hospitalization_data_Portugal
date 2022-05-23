# Function that creates a tables with entries, fatalities, population numbers and related percentages
#imputs: data <- the entry data
#        pop_data <- table with population numbers by groups and levels

PopVar_table <- function(data,pop_table){
  #Check if dplyr is in use, if not upload it
  if (!require(dplyr)) {library(dplyr)}
  
  #Functions-------------------------------------------------------------------
  #Calculates number of entries and fatalities, by Type, for an input data
  get_entries_fatal <- function(data,group,level){
    # Entries 
    Entries_nonICU <-  nrow(filter(data,data$Type == "nonICU"))
    Entries_ICU    <-  nrow(filter(data,data$Type == "ICU"))
    Entries_Total  <-  Entries_nonICU + Entries_ICU
    
    # Fatalities 
    Fatalities_nonICU <-  nrow(filter(data,data$Type == "nonICU" & data$Outcome == "Deceased"))
    Fatalities_ICU    <-  nrow(filter(data,data$Type == "ICU" & data$Outcome == "Deceased"))
    Fatalities_Total  <-  Fatalities_nonICU + Fatalities_ICU
    
    #Build df
    df_entries_fatal_level <- data.frame(Group = group,
                                         Level = level,
                                         Entries_nonICU     = Entries_nonICU,
                                         Entries_ICU        = Entries_ICU,
                                         Entries_Total      = Entries_Total,
                                         Prop_ICU           = Entries_ICU / (Entries_Total),
                                         Fatalities_nonICU  = Fatalities_nonICU,
                                         Fatalities_ICU     = Fatalities_ICU,
                                         Fatalities_Total   = Fatalities_Total,
                                         Fatal_rate_nonICU  = Fatalities_nonICU / Entries_nonICU,
                                         Fatal_rate_ICU     = Fatalities_ICU / Entries_ICU,
                                         Fatal_rate_Total   = Fatalities_Total / Entries_Total)
    
    
  }
  
  #MAIN-------------------------------------------------------------------------
  # Convert Entry and Discharge month column to factor for use
  data <- data %>%
    mutate(Entry.month=as.factor(Entry.month))
  data <- data %>% 
    mutate(Discharge.month=as.factor(Discharge.month))
  
  data_groups <- c("Region", "Gender","Group","Entry.month","Discharge.month")
  data_levels <- sapply(data, levels)
  
  # Total row
  df_entries_fatal <- get_entries_fatal(data,group='Total',level='Total')
  
  
  #Create df with entries and deaths by group and level
  for (group in data_groups){
    
    levels <- unlist(data_levels[toString(group)],use.names = F)
    
    for(level in levels){
      data_level <- filter(data,data[group] == level)
      #Calculates entries and fatalities, by type and for a level, and appends it to the df
      df_entries_fatal <- rbind(df_entries_fatal,get_entries_fatal(data_level,group=group,level=level))
    }
  }
  
  #Add Proportion of entries
  df_entries_fatal$Prop_Entries_nonICU    = df_entries_fatal$Entries_nonICU / df_entries_fatal$Entries_nonICU[1]
  df_entries_fatal$Prop_Entries_ICU       = df_entries_fatal$Entries_ICU / df_entries_fatal$Entries_ICU[1]
  df_entries_fatal$Prop_Entries_Total     = df_entries_fatal$Entries_Total / df_entries_fatal$Entries_Total[1]
  
  #Add population and related columns
  PopVar_table <- merge(x =  df_entries_fatal, y= pop_table, by = c("Group","Level"), all.x = T, sort = F)
  PopVar_table$Prop_Entries_Pop_nonICU <- PopVar_table$Entries_nonICU /  PopVar_table$Pop
  PopVar_table$Prop_Entries_Pop_ICU <- PopVar_table$Entries_ICU /  PopVar_table$Pop
  PopVar_table$Prop_Entries_Pop_Total <- PopVar_table$Entries_Total /  PopVar_table$Pop
  PopVar_table$Prop100T_Entries_Pop_nonICU <- PopVar_table$Prop_Entries_Pop_nonICU * 1000000
  PopVar_table$Prop100T_Entries_Pop_ICU <- PopVar_table$Prop_Entries_Pop_ICU * 1000000
  PopVar_table$Prop100T_Entries_Pop_Total <- PopVar_table$Prop_Entries_Pop_Total * 1000000
  
  
  PopVar_table  
}
