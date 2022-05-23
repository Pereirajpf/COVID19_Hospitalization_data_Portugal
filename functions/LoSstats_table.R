# Function to create LoS statistical analysis table from entries data

# Creates a table with LoS statistical parameters for each group (column) in the entry data,
# distinguishing nonICU and ICU patients.
LoSstats_table <- function(data){
  #Check if dplyr is in use
  if (!require(dplyr)) {library(dplyr)}
  
  #Functions-------------------------------------------------------------------
    # Calculate the LoS statistical parameters
    LoSstats <- function(data){
      df_LoS <- as.data.frame(cbind(
        mean = mean(data$Time.stay),
        median = median(data$Time.stay),
        t(quantile(data$Time.stay,c(0,0.25,0.50,0.75,0.90,0.95,0.99,1))),
        max = max(data$Time.stay),
        IQR = IQR(data$Time.stay)))
      
      df_LoS
    }
  
    #Calls the LoSstats function for nonICU and ICU patients
    LoSstats_byType<- function(data){
      df_LoS <- data.frame()
      
      for (type in c("nonICU","ICU")){
        df_LoS_group <- cbind(Type=type,LoSstats(filter(data,Type == type)))
        df_LoS <- rbind(df_LoS,df_LoS_group)
      }
      
      df_LoS
    }
  
  #MAIN-------------------------------------------------------------------------
  
  # Convert Entry month column to factor for use
  data <- data %>%
    mutate(Entry.month=as.factor(Entry.month))
  
  
  data_levels <- sapply(data, levels)
  data_groups <- c("Region", "Gender","Group","Entry.month")
  
  #Initialization and Totals row
  df_LoS <- cbind(Group="Total",Level="Total",LoSstats_byType(data))
  
  
  # Calls LoSstats_byType for each level in each group(column in the data)
  for (group in data_groups){
    levels <- unlist(data_levels[toString(group)],use.names = F)
    
    df_LoS_group <- data.frame()
    for (level in levels){
      df_LoS_level <- cbind(Group=group,Level=level,LoSstats_byType(filter(data,data[toString(group)] == level)))
      df_LoS_group <- rbind(df_LoS_group,df_LoS_level)
    }
    
    df_LoS <- rbind(df_LoS,df_LoS_group)
    
  }
  
  df_LoS
}