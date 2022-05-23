#Function to calculate Percentage of total entries by zVar
#to be used with the histPlot() function

Perc_total_byZVar <- function(df_Vars){

  Total <- aggregate(df_Vars[[3]], by=list(df_Vars[[2]]), FUN=sum)
  colnames(Total) <- c(colnames(df_Vars)[2],"Total")

  df_Vars <- merge(df_Vars,Total, all.x=TRUE)
  #colnames(df_Vars)[colnames(df_Vars) == "x"] <- "Total_byType"
  
  df_Vars$Perc <- df_Vars[[3]]/df_Vars$Total
  
  df_Vars$Perc
}
