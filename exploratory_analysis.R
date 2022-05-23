# Exploratory analysis
# File where the exploratory analysis plots are build

#Library
library(readxl)
library(dplyr)
library(ggplot2)


#Import data
#Entry data
load("data/data_covid.Rdata")
str(entry_data)

#ByDay data
load("data/df_byDay.Rdata")
str(df_byDay)

#PopVar data
load("data/PopVar_table.RData")
df_PopVar <- as_tibble(df_PopVar)
str(df_PopVar)

#Import functions
source("functions/Plots_func.R")


#MAIN===========================================================================
#Colours
colourTable <- list(Type = c("tomato1","#56B4E9"),
                   Gender = c("#8600FC", "#0AC4A9"),
                   Region = c("orange2","deepskyblue3","brown3","limegreen","lightskyblue"))


# Violin/Box plots====
#LoS by Type
violinBox_Type <- violinBoxPlot(entry_data,xvar = "Type", colours = colourTable$Type, legend = F)
violinBox_Type
ggsave(
  'plots/ViolinBox_plots/violinBox_Type.jpeg',
  plot = violinBox_Type,
  width = 16,
  height = 9,
  units = "cm",
  dpi = 600
)

#LoS by Gender and Type
violinBox_Gender <- violinBoxPlot(entry_data,xvar = "Type",zvar="Gender",colours = colourTable$Gender)
violinBox_Gender
ggsave(
  'plots/ViolinBox_plots/violinBox_Gender.jpeg',
  plot = violinBox_Gender,
  width = 16,
  height = 9,
  units = "cm",
  dpi = 600
)

#LoS by Group and Type
violinBox_Group <- violinBoxPlot(entry_data,xvar = "Group",zvar="Type", colours = colourTable$Type)
violinBox_Group
ggsave(
  'plots/ViolinBox_plots/violinBox_Group.jpeg',
  plot = violinBox_Group,
  width = 50,
  height = 18,
  units = "cm",
  dpi = 600
)

#LoS by Month and Type
violinBox_Month <- violinBoxPlot(entry_data,xvar = "Entry.month",zvar="Type", colours = colourTable$Type)
violinBox_Month
ggsave(
  'plots/ViolinBox_plots/violinBox_Month.jpeg',
  plot = violinBox_Month,
  width = 50,
  height = 18,
  units = "cm",
  dpi = 600
)

#LoS by Region and Type
violinBox_Region <- violinBoxPlot(entry_data,xvar = "Type",zvar="Region", colours = colourTable$Region)
violinBox_Region
ggsave(
  'plots/ViolinBox_plots/violinBox_Region.jpeg',
  plot = violinBox_Region,
  width = 16,
  height = 9,
  units = "cm",
  dpi = 600
)

#LoS by Outcome and Type
violinBox_Outcome <- violinBoxPlot(entry_data,xvar = "Outcome", zvar="Type", colours = colourTable$Type)
violinBox_Outcome
ggsave(
  'plots/ViolinBox_plots/violinBox_Outcome.jpeg',
  plot = violinBox_Outcome,
  width = 16,
  height = 9,
  units = "cm",
  dpi = 600
)

# Linegraphs====
  #ByDay linegraphs----
#Entries by Type by day
byDayPlot_entries_discharges <- linePlot_2var(df_byDay, xvar = "date", "entries_nonICU","entries_ICU")
byDayPlot_entries_discharges
ggsave(
  'plots/byDay_linegraphs/byDayPlot_entries.jpeg',
  plot = byDayPlot_entries_discharges,
  width = 16,
  height = 9,
  units = "cm",
  dpi = 600
)
#Com a mesma escala
byDayPlot_entries_discharges <- linePlot_2var(df_byDay, xvar = "date", "entries_nonICU","entries_ICU",coeff = 1)
byDayPlot_entries_discharges
ggsave(
  'plots/byDay_linegraphs/byDayPlot_entries2.jpeg',
  plot = byDayPlot_entries_discharges,
  width = 16,
  height = 9,
  units = "cm",
  dpi = 600
)

#Entries and discharges by day
byDayPlot_entries_discharges <- linePlot_2var(df_byDay, xvar = "date", "entries","discharges")
byDayPlot_entries_discharges
ggsave(
  'plots/byDay_linegraphs/byDayPlot_entries_discharges.jpeg',
  plot = byDayPlot_entries_discharges,
  width = 16,
  height = 9,
  units = "cm",
  dpi = 600
)

##Entries and discharges by day for nonICU and ICU patients
byDayPlot_entries_discharges_nonICU <- linePlot_2var(df_byDay, xvar = "date", "entries_nonICU","discharges_nonICU")
byDayPlot_entries_discharges_nonICU
ggsave(
  'plots/byDay_linegraphs/byDayPlot_entries_discharges_nonICU.jpeg',
  plot = byDayPlot_entries_discharges_nonICU,
  width = 16,
  height = 9,
  units = "cm",
  dpi = 600
)
byDayPlot_entries_discharges_ICU <- linePlot_2var(df_byDay, xvar = "date", "entries_ICU","discharges_ICU")
byDayPlot_entries_discharges_ICU
ggsave(
  'plots/byDay_linegraphs/byDayPlot_entries_discharges_ICU.jpeg',
  plot = byDayPlot_entries_discharges_ICU,
  width = 16,
  height = 9,
  units = "cm",
  dpi = 600
)

#In hospital care by Type and day
byDayPlot_inHosp <- linePlot_2var(df_byDay, xvar = "date", "occupancy_nonICU","ocupancy_ICU")
byDayPlot_inHosp
ggsave(
  'plots/byDay_linegraphs/byDayPlot_inHosp.jpeg',
  plot = byDayPlot_inHosp,
  width = 16,
  height = 9,
  units = "cm",
  dpi = 600
)

  #PopVar linegraphs----
#Entries vs fatalities by month
PopVarPlot_entries_fatal <- linePlot_2var(filter(df_PopVar,df_PopVar$Group == "Entry.month"), xvar = "Level","Entries_Total","Fatalities_Total",coeff = 2.5)
PopVarPlot_entries_fatal
ggsave(
  'plots/PopVar_linegraphs/PopVarPlot_entries_fatal.jpeg',
  plot = PopVarPlot_entries_fatal,
  width = 16,
  height = 9,
  units = "cm",
  dpi = 600
)
#nonICU
PopVarPlot_entries_fatal_nonICU <- linePlot_2var(filter(df_PopVar,df_PopVar$Group == "Entry.month"), xvar = "Level","Entries_nonICU","Fatalities_nonICU",coeff = 2.5)
PopVarPlot_entries_fatal_nonICU
ggsave(
  'plots/PopVar_linegraphs/PopVarPlot_entries_fatal_nonICU.jpeg',
  plot = PopVarPlot_entries_fatal_nonICU,
  width = 16,
  height = 9,
  units = "cm",
  dpi = 600
)
#ICU
PopVarPlot_entries_fatal_ICU <- linePlot_2var(filter(df_PopVar,df_PopVar$Group == "Entry.month"), xvar = "Level","Entries_ICU","Fatalities_ICU",coeff = 2.5)
PopVarPlot_entries_fatal_ICU
ggsave(
  'plots/PopVar_linegraphs/PopVarPlot_entries_fatal_ICU.jpeg',
  plot = PopVarPlot_entries_fatal_ICU,
  width = 16,
  height = 9,
  units = "cm",
  dpi = 600
)

#Fatality rate by month and Type
PopVarPlot_fatalRate <- linePlot(filter(df_PopVar,df_PopVar$Group == "Entry.month"), xvar = "Level",yvar = "Fatal_rate_nonICU",yvar2 = "Fatal_rate_ICU", yaxis_name = "Fatality rate",grapPerc = T)
PopVarPlot_fatalRate
ggsave(
  'plots/PopVar_linegraphs/PopVarPlot_fatalRate.jpeg',
  plot = PopVarPlot_fatalRate,
  width = 16,
  height = 9,
  units = "cm",
  dpi = 600
)

#By group
#to order correctly the groups
df_PopVar_Group <- filter(df_PopVar,df_PopVar$Group == "Group")
df_PopVar_Group$Level <-  c("[0,5)","[05,10)","[10,15)","[15,20)","[20,25)","[25,30)","[30,35)","[35,40)","[40,45)","[45,50)","[50,55)","[55,60)","[60,65)","[65,70)","[70,75)","[75,80)","[80,150)")

#Entries by group and Type
PopVarPlot_Entries_byGroup <- linePlot_2var(df_PopVar_Group, xvar = "Level","Entries_nonICU","Entries_ICU")
PopVarPlot_Entries_byGroup
ggsave(
  'plots/PopVar_linegraphs/PopVarPlot_Entries_byGroup.jpeg',
  plot = PopVarPlot_Entries_byGroup,
  width = 16,
  height = 9,
  units = "cm",
  dpi = 600
)

#Fatalities by group and Type
PopVarPlot_Fatalities_byGroup <- linePlot_2var(df_PopVar_Group, xvar = "Level","Fatalities_nonICU","Fatalities_ICU")
PopVarPlot_Fatalities_byGroup
ggsave(
  'plots/PopVar_linegraphs/PopVarPlot_Fatalities_byGroup.jpeg',
  plot = PopVarPlot_Fatalities_byGroup,
  width = 16,
  height = 9,
  units = "cm",
  dpi = 600
)

#Fatality rate by group and Type
PopVarPlot_fatalRate_byGroup <- linePlot(df_PopVar_Group, xvar = "Level","Fatal_rate_nonICU","Fatal_rate_ICU", grapPerc = T)
PopVarPlot_fatalRate_byGroup
ggsave(
  'plots/PopVar_linegraphs/PopVarPlot_fatalRate_byGroup.jpeg',
  plot = PopVarPlot_fatalRate_byGroup,
  width = 16,
  height = 9,
  units = "cm",
  dpi = 600
)

#Percentage of the Population by group
PopVarPlot_percPop_byGroup <- linePlot_2var(df_PopVar_Group, xvar = "Level","Prop_Entries_Pop_nonICU","Prop_Entries_Pop_ICU",coeff = 10, grapPerc = T)
PopVarPlot_percPop_byGroup
ggsave(
  'plots/PopVar_linegraphs/PopVarPlot_percPop_byGroup.jpeg',
  plot = PopVarPlot_percPop_byGroup,
  width = 16,
  height = 9,
  units = "cm",
  dpi = 600
)

#Proportion of the total entries, by type
PopVarPlot_PropEntries_Group <- linePlot_2var(df_PopVar_Group, xvar = "Level",yvar = "Prop_Entries_nonICU",yvar2 = "Prop_Entries_ICU", yaxis_name = "Proporcion of entries",grapPerc = T)
PopVarPlot_PropEntries_Group
ggsave(
  'plots/PopVar_linegraphs/PopVarPlot_PropEntries_Group.jpeg',
  plot = PopVarPlot_PropEntries_Group,
  width = 16,
  height = 9,
  units = "cm",
  dpi = 600
)

#Proportion of patients to ICU
PopVarPlot_PropICU_Group <- linePlot(df_PopVar_Group, xvar = "Level", "Prop_ICU",yaxis_name = "ICU patients proporcion",grapPerc = T)
PopVarPlot_PropICU_Group
ggsave(
  'plots/PopVar_linegraphs/PopVarPlot_PropICU_Group.jpeg',
  plot = PopVarPlot_PropICU_Group,
  width = 16,
  height = 9,
  units = "cm",
  dpi = 600
)

#ICU percentage
PopVarPlot_PropICU <- linePlot(filter(df_PopVar,df_PopVar$Group == "Entry.month",), xvar = "Level", "Prop_ICU", grapPerc = T)
PopVarPlot_PropICU
ggsave(
  'plots/PopVar_linegraphs/PopVarPlot_PropICU.jpeg',
  plot = PopVarPlot_PropICU,
  width = 16,
  height = 9,
  units = "cm",
  dpi = 600
)

#Proportion of patients to ICU
PopVarPlot_PropICU <- linePlot(filter(df_PopVar,df_PopVar$Group == "Entry.month",), xvar = "Level", "Prop_ICU",yaxis_name = "ICU patients proporcion", grapPerc = T)
PopVarPlot_PropICU
ggsave(
  'plots/PopVar_linegraphs/PopVarPlot_PropICU.jpeg',
  plot = PopVarPlot_PropICU,
  width = 16,
  height = 9,
  units = "cm",
  dpi = 600
)


#Entry_data HistPlot----

#function of mortality rate
deathRate_func = function(df_Vars){df_Vars[[3]] / (df_Vars[[4]])}

#By Type
  #and Group, no caption
    histPlot_Group_Type <- histPlot(entry_data,xvar = "Group", zvar = "Type", colours = colourTable$Type,calc_Perc = F)
    histPlot_Group_Type$table
    write.csv2(histPlot_Group_Type$table,'plots/histPlot_tables/histPlot_Group_Type_nocaption.csv')
    histPlot_Group_Type$plot
    ggsave(
      'plots/histPlot_plots/histPlot_Group_Type_nocaption.jpeg',
      plot = histPlot_Group_Type$plot,
      width = 16,
      height = 9,
      units = "cm",
      dpi = 600
    )
    #and Group
    histPlot_Group_Type <- histPlot(entry_data,xvar = "Group", zvar = "Type", colours = colourTable$Type)
    histPlot_Group_Type$table
    write.csv2(histPlot_Group_Type$table,'plots/histPlot_tables/histPlot_Group_Type.csv')
    histPlot_Group_Type$plot
    ggsave(
      'plots/histPlot_plots/histPlot_Group_Type.jpeg',
      plot = histPlot_Group_Type$plot,
      width = 16,
      height = 9,
      units = "cm",
      dpi = 600
    )
    #and Group and mortality
    histPlot_Group_Type_Death <- histPlot(entry_data,xvar = "Group", zvar = "Type", filter_var = c("Outcome","Deceased"), colours = colourTable$Type,
                                          Perc_func = deathRate_func, plot_caption = "*Fatality rate by group")
    write.csv2(histPlot_Group_Type_Death$table,'plots/histPlot_tables/histPlot_Group_Type_Death.csv')
    histPlot_Group_Type_Death$plot
    ggsave(
      'plots/histPlot_plots/histPlot_Group_Type_Death.jpeg',
      plot = histPlot_Group_Type_Death$plot,
      width = 16,
      height = 9,
      units = "cm",
      dpi = 600
    )
    
    #and Entry/Discharge month
    histPlot_Entry_month_Type <- histPlot(entry_data,xvar = "Entry.month", zvar = "Type", colours = colourTable$Type)
    write.csv2(histPlot_Entry_month_Type$table,'plots/histPlot_tables/histPlot_Entry_month_Type.csv')
    histPlot_Entry_month_Type$plot
    ggsave(
      'plots/histPlot_plots/histPlot_Entry_month_Type.jpeg',
      plot = histPlot_Entry_month_Type$plot,
      width = 16,
      height = 9,
      units = "cm",
      dpi = 600
    )
    #and Discharge.month and mortality
    histPlot_Discharge_month_Type_Death <- histPlot(filter(entry_data,entry_data$Discharge.month <= "2021-03"),
                                                    xvar = "Discharge.month", zvar = "Type", filter_var = c("Outcome","Deceased"), colours = colourTable$Type,
                                                    Perc_func = deathRate_func, plot_caption = "*Fatality rate by group")
    write.csv2(histPlot_Discharge_month_Type_Death$table,'plots/histPlot_tables/histPlot_Discharge_month_Type_Death.csv')
    histPlot_Discharge_month_Type_Death$plot
    ggsave(
      'plots/histPlot_plots/histPlot_Discharge_month_Type_Death.jpeg',
      plot = histPlot_Discharge_month_Type_Death$plot,
      width = 16,
      height = 9,
      units = "cm",
      dpi = 600
    )
    
    #and Region
    histPlot_Region_Type <- histPlot(entry_data,xvar = "Region", zvar = "Type", colours = colourTable$Type)
    write.csv2(histPlot_Region_Type$table,'plots/histPlot_tables/histPlot_Region_Type.csv')
    histPlot_Region_Type$plot
    ggsave(
      'plots/histPlot_plots/histPlot_Region_Type.jpeg',
      plot = histPlot_Region_Type$plot,
      width = 16,
      height = 9,
      units = "cm",
      dpi = 600
    )
    #and Region mortality
    histPlot_Region_Type_Death <- histPlot(entry_data,xvar = "Region", zvar = "Type", filter_var = c("Outcome","Deceased"), colours = colourTable$Type,
                                           Perc_func = deathRate_func, plot_caption = "*Fatality rate by group")
    write.csv2(histPlot_Region_Type_Death$table,'plots/histPlot_tables/histPlot_Region_Type_Death.csv')
    histPlot_Region_Type_Death$plot
    ggsave(
      'plots/histPlot_plots/histPlot_Region_Type_Death.jpeg',
      plot = histPlot_Region_Type_Death$plot,
      width = 16,
      height = 9,
      units = "cm",
      dpi = 600
    )
    
    #and Gender
    histPlot_Gender_Type <- histPlot(entry_data,xvar = "Gender", zvar = "Type", colours = colourTable$Type)
    write.csv2(histPlot_Gender_Type$table,'plots/histPlot_tables/histPlot_Gender_Type.csv')
    histPlot_Gender_Type$plot
    ggsave(
      'plots/histPlot_plots/histPlot_Gender_Type.jpeg',
      plot = histPlot_Gender_Type$plot,
      width = 12,
      height = 9,
      units = "cm",
      dpi = 600
    )
    #and Gender mortality
    histPlot_Gender_Type_Death <- histPlot(entry_data, xvar = "Gender", zvar = "Type", filter_var = c("Outcome","Deceased"), colours = colourTable$Type,
                                           Perc_func = deathRate_func, plot_caption = "*Fatality rate by group")
    write.csv2(histPlot_Gender_Type_Death$table,'plots/histPlot_tables/histPlot_Gender_Type_Death.csv')
    histPlot_Gender_Type_Death$plot
    ggsave(
      'plots/histPlot_plots/histPlot_Gender_Type_Death.jpeg',
      plot = histPlot_Gender_Type_Death$plot,
      width = 12,
      height = 9,
      units = "cm",
      dpi = 600
    )
    
    
    #By Gender and Type----
    #and Group
    for (type in levels(entry_data$Type)) {
    histPlot_Group_Gender <- histPlot(filter(entry_data,entry_data$Type == type), plot_title = type,
                                             xvar = "Group", zvar = "Gender", colours = colourTable$Gender)
    write.csv2(histPlot_Group_Gender$table, paste('plots/histPlot_tables/histPlot_Group_Gender_',type,'.csv'))
    histPlot_Group_Gender$plot
    ggsave(
      paste('plots/histPlot_plots/histPlot_Group_Gender_',type,'.jpeg'),      
      plot = histPlot_Group_Gender$plot,
      width = 16,
      height = 9,
      units = "cm",
      dpi = 600
    )
    }
    #and Group and mortality
    for (type in levels(entry_data$Type)) {
    histPlot_Group_Gender_Death <- histPlot(filter(entry_data,entry_data$Type == type), plot_title = type,
                                            xvar = "Group", zvar = "Gender", filter_var = c("Outcome","Deceased"), colours = colourTable$Gender,
                                            Perc_func = deathRate_func, plot_caption = "*Fatality rate by group")
    write.csv2(histPlot_Group_Gender_Death$table,paste('plots/histPlot_tables/histPlot_Group_Gender_',type,'_Death.csv'))
    histPlot_Group_Gender_Death$plot
    ggsave(
      paste('plots/histPlot_plots/histPlot_Group_Gender_',type,'_Death.jpeg'),
      plot = histPlot_Group_Gender_Death$plot,
      width = 16,
      height = 9,
      units = "cm",
      dpi = 600
    )
    }
    
    #and Entry/Discharge month
    for (type in levels(entry_data$Type)) {
      histPlot_Entry_Month_Gender <- histPlot(filter(entry_data,entry_data$Type == type), plot_title = type,
                                        xvar = "Entry.month", zvar = "Gender", colours = colourTable$Gender)
      write.csv2(histPlot_Entry_Month_Gender$table, paste('plots/histPlot_tables/histPlot_Entry_Month_Gender_',type,'.csv'))
      histPlot_Entry_Month_Gender$plot
      ggsave(
        paste('plots/histPlot_plots/histPlot_Entry_Month_Gender_',type,'.jpeg'),      
        plot = histPlot_Entry_Month_Gender$plot,
        width = 16,
        height = 9,
        units = "cm",
        dpi = 600
      )
    }
    #and Discharge.month and mortality
    for (type in levels(entry_data$Type)){
    histPlot_Discharge_month_Gender_Death <- histPlot(filter(entry_data,entry_data$Discharge.month <= "2021-03" &
                                                               entry_data$Type == type), plot_title = type,
                                                    xvar = "Discharge.month", zvar = "Gender", filter_var = c("Outcome","Deceased"), colours = colourTable$Gender,
                                                    Perc_func = deathRate_func, plot_caption = "*Fatality rate by group")
    write.csv2(histPlot_Discharge_month_Gender_Death$table,paste('plots/histPlot_tables/histPlot_Discharge_month_Gender_',type,'_Death.csv'))
    histPlot_Discharge_month_Gender_Death$plot
    ggsave(
      paste('plots/histPlot_plots/histPlot_Discharge_month_Gender_',type,'_Death.jpeg'),
      plot = histPlot_Discharge_month_Gender_Death$plot,
      width = 16,
      height = 9,
      units = "cm",
      dpi = 600
    )
    }
        
    #and Region
    for (type in levels(entry_data$Type)) {
    histPlot_Region_Gender <- histPlot(filter(entry_data,entry_data$Type == type), plot_title = type,
                                       xvar = "Region", zvar = "Gender", colours = colourTable$Gender)
    write.csv2(histPlot_Region_Gender$table, paste('plots/histPlot_tables/histPlot_Region_Gender_',type,'.csv'))
    histPlot_Region_Gender$plot
    ggsave(
      paste('plots/histPlot_plots/histPlot_Region_Gender',type,'.jpeg'),
      plot = histPlot_Region_Gender$plot,
      width = 16,
      height = 9,
      units = "cm",
      dpi = 600
    )
    }
    #and Region mortality
    for (type in levels(entry_data$Type)) {
    histPlot_Region_Gender_Death <- histPlot(filter(entry_data,entry_data$Type == type), plot_title = type
                                             , xvar = "Region", zvar = "Gender", filter_var = c("Outcome","Deceased"), colours = colourTable$Gender,
                                             Perc_func = deathRate_func, plot_caption = "*Fatality rate by group")
    write.csv2(histPlot_Region_Gender_Death$table, paste('plots/histPlot_tables/histPlot_Region_Gender_',type,'_Death.csv'))
    histPlot_Region_Gender_Death$plot
    ggsave(
      paste('plots/histPlot_plots/histPlot_Region_Gender_',type,'Death.jpeg'),
      plot = histPlot_Region_Gender_Death$plot,
      width = 16,
      height = 9,
      units = "cm",
      dpi = 600
    )
    }
    
    
    
    #Other interesting plots---
    #Region and group by Type
    for (type in levels(entry_data$Type)) {
      histPlot_Group_Region <- histPlot(filter(entry_data,entry_data$Type == type), plot_title = type,
                                         xvar = "Group", zvar = "Region", colours = colourTable$Region)
      write.csv2(histPlot_Group_Region$table, paste('plots/histPlot_tables/histPlot_Group_Region_',type,'.csv'))
      histPlot_Group_Region$plot
      ggsave(
        paste('plots/histPlot_plots/histPlot_Group_Region',type,'.jpeg'),
        plot = histPlot_Group_Region$plot,
        width = 26,
        height = 12,
        units = "cm",
        dpi = 600
      )
    }
    #and Group mortality
    for (type in levels(entry_data$Type)) {
      histPlot_Group_Region_Death <- histPlot(filter(entry_data,entry_data$Type == type), plot_title = type
                                               , xvar = "Group", zvar = "Region", filter_var = c("Outcome","Deceased"), colours = colourTable$Region,
                                              Perc_func = deathRate_func, plot_caption = "*Fatality rate by group")
      write.csv2(histPlot_Group_Region_Death$table, paste('plots/histPlot_tables/histPlot_Group_Region_',type,'_Death.csv'))
      histPlot_Group_Region_Death$plot
      ggsave(
        paste('plots/histPlot_plots/histPlot_Group_Region_',type,'Death.jpeg'),
        plot = histPlot_Group_Region_Death$plot,
        width = 26,
        height = 9,
        units = "cm",
        dpi = 600
      )
    }


    # By Month and Region
    histPlot_Region_Month <- histPlot(entry_data,
                                            xvar = "Entry.month", zvar = "Region", colours = colourTable$Region)
    write.csv2(histPlot_Region_Month$table, paste('plots/histPlot_tables/histPlot_Region_Month.csv'))
    histPlot_Region_Month$plot
    ggsave(
      paste('plots/histPlot_plots/histPlot_Region_Month.jpeg'),
      plot = histPlot_Region_Month$plot,
      width = 26,
      height = 9,
      units = "cm",
      dpi = 600
    )
    
    
