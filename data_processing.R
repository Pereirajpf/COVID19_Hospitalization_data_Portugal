# Data processing

#Library
library(readxl)
library(dplyr)
library(lubridate)


#Import data
data <- read_excel("data/DRGdata_COVID19.xlsx", sheet = 1)
str(data)


# Renaming to changing designations----
#Change names of columns
colnames(data) <- c("Institution","Gender","Date.birth","Process","Episode",
                    "Outcome","Discharge.date","Entry.date",
                    "Type","Length","Code","Diagnosis","Order")

#Gender
data$Gender[data$Gender == "Feminino"] <- "Feminine"
data$Gender[data$Gender == "Masculino"] <- "Masculine"
#Type
data$Type[data$Type == "Sim"] <- "ICU"
data$Type[data$Type == "Não"] <- "nonICU"
#Outcome
data$Outcome[data$Outcome == "Falecido"] <- "Deceased"
data$Outcome[data$Outcome != "Deceased"] <- "Discharged"


# Configuration of dates and factors----
data <- data %>%
  mutate(Discharge.date=as.Date(Discharge.date, format = "%d/%m/%Y"),
         Entry.date=as.Date(Entry.date, format = "%d/%m/%Y"),
         Date.birth=as.Date(Date.birth,  format = "%d/%m/%Y"),
         Gender = factor(Gender),
         Institution = factor(Institution),
         Length = factor(Length),
         Type = factor(Type),
         Outcome = factor(Outcome)
  )


# Age and Age groups columns----
#calculate days from date of birth to entry date
data$Age <- as.integer(as.period(interval(start = data$Date.birth, end = data$Entry.date))$year)

# Set Age groups
data$Group <- cut(data$Age, breaks = c(seq(0,80,by=5),150), right = FALSE)


#Time of stay----
data$Time.stay <- as.integer(data$Discharge.date - data$Entry.date)


# Adding ARS----
#Read ARS table
ars_table <- read.csv("data/ARS/Inst_ARS.csv",sep=";", encoding="latin-1")
ars_table <- ars_table %>% select(-X)

#Join tables
data <- data %>% left_join(ars_table,by="Institution")
data$Region <- factor(data$Region)

# Remove Açores and Madeira Regions
data <- filter(data,data$Region %in% c("Alentejo","Algarve","Centro","LVT","Norte"))
data$Region <- droplevels(data$Region) 


#Entry and Discharge Months----
data$Entry.month <- format(as.Date(data$Entry.date), "%Y-%m")
data$Discharge.month <- format(as.Date(data$Discharge.date), "%Y-%m")


# Data trimming---- 
# To ensure data completion, as explained in the "Representation problem chapter"
data <- subset(data,data$Entry.date < (as.Date(format(max(data$Discharge.date),"%Y-%m-01")) - months(2)))
# Start date at 1/03/2020
data <- subset(data,data$Entry.date >= as.Date("1/03/2020",  format = "%d/%m/%Y"))


# Reorder and rename columns----
entry_data <- data %>% select(Region,Type,Age,Group,Gender,Entry.date,Discharge.date,Outcome,Time.stay,Entry.month,Discharge.month)  



#Save data as csv and RData file==========================================================
save(entry_data, file = "data/data_covid.RData")
write.csv2(entry_data,'data/data_covid.csv')
