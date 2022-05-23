# Script to create the Population numbers table

library(readxl)
library(dplyr)

# Population table--------------------------------------------------------------

total_pop   <- data.frame(Group = 'Total',
                          Level = 'Total',
                          Pop   = 9857593)

regions_pop <- data.frame(Group = 'Region',
                          Level = c("Alentejo","Algarve","Centro","LVT","Norte"),
                          Pop   = c(166726,451006,1480664,3659871,3125804))

gender_pop  <- data.frame(Group = 'Gender',
                          Level = c("Feminine","Masculine"),
                          Pop   = c(5169608, 4687985))

groups_pop  <- data.frame(Group = 'Group',
                          Level = c("[0,5)","[5,10)","[10,15)","[15,20)","[20,25)","[25,30)","[30,35)","[35,40)","[40,45)","[45,50)","[50,55)","[55,60)","[60,65)","[65,70)","[70,75)","[75,80)","[80,150)"),
                          Pop   = c(415549, 422628, 475629, 506433, 531810, 515282, 533250, 615083, 724463, 763202, 708709, 706171, 655391, 602210, 536388, 428706, 661224))

Pop_table <- rbind(total_pop,regions_pop,gender_pop,groups_pop)

#Save population table
save(Pop_table, file = "data/Pop_table/Pop_table.RData")
write.csv2(Pop_table,'data/Pop_table/Pop_table.csv')
