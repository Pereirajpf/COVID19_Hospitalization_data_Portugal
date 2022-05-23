# Script to construct the ARS table

#Import data====
library(readr)
library(dplyr)
library(lubridate)

region_list <- list(Norte = c("Centro Hospitalar de Entre Douro e Vouga, E.P.E.",
                              "Centro Hospitalar de Tras-os-Montes e Alto Douro, E.P.E.",
                              "Centro Hospitalar de Vila Nova de Gaia/Espinho, E.P.E.",
                              "Centro Hospitalar do Médio Ave, E.P.E.",
                              "Centro Hospitalar Povoa do Varzim/Vila do Conde, E.P.E.",
                              "Centro Hospitalar Tâmega e Sousa, E.P.E.",
                              "Centro Hospitalar Universitário de S. João, E.P.E.",
                              "Centro Hospitalar Universitário do Porto, E.P.E.",
                              "Hospital da Prelada",
                              "Hospital de Braga, E.P.E.",
                              "Hospital Magalhães de Lemos, E.P.E.",
                              "Hospital Santa Maria Maior, E.P.E. - Barcelos",
                              "Hospital Senhora da Oliveira, E.P.E. - Guimarães",
                              "Instituto Português Oncologia  F. Gentil - Porto, E.P.E.",
                              "Unidade Local de Saúde de Matosinhos, E.P.E.",
                              "Unidade Local de Saúde do Alto Minho, E.P.E.",
                              "Unidade Local de Saúde Nordeste, E.P.E."),
                    
                    Centro = c("Centro Hospitalar de Leiria, E.P.E.",
                               "Centro Hospitalar do Baixo Vouga, E.P.E.",
                               "Centro Hospitalar e Universitário de Coimbra, E.P.E.",
                               "Centro Hospitalar Tondela-Viseu, E.P.E.",
                               "Centro Hospitalar Universitário Cova da Beira, E.P.E.",
                               "Hospital Distrital da Figueira da Foz, E.P.E.",
                               "Hospital Dr. Francisco Zagalo - Ovar",
                               "Unidade Local de Saúde da Guarda, E.P.E.",
                               "Unidade Local de Saúde de Castelo Branco, E.P.E.",
                               "Hospital Arcebispo João Crisostomo - Cantanhede",
                               "Instituto Português Oncologia  F. Gentil - Centro, E.P.E."),
                    
                    LVT = c("Centro Hospitalar Barreiro\\Montijo, E.P.E.",
                            "Centro Hospitalar do Médio Tejo, E.P.E.",
                            "Centro Hospitalar do Oeste, E.P.E.",
                            "Centro Hospitalar Lisboa Ocidental, E.P.E.",
                            "Centro Hospitalar Psiquiátrico de Lisboa",
                            "Centro Hospitalar Setúbal, E.P.E.",
                            "Centro Hospitalar Universitário de Lisboa Norte, E.P.E.",
                            "Centro Hospitalar Universitário Lisboa Central, E.P.E.",
                            "Hospital Beatriz Ângelo - Loures",
                            "Hospital de Vila Franca de Xira",
                            "Hospital Distrital de Santarém, E.P.E.",
                            "Hospital Garcia de Orta, E.P.E. - Almada",
                            "Hospital Professor Dr. Fernando Fonseca, E.P.E.",
                            "HPP Hospital de Cascais - Dr. José de Almeida, E.P.E.",
                            "Instituto Português Oncologia  F. Gentil - Lisboa, E.P.E.",
                            "Centro Hospitalar Setúbal, E.P.E",
                            "HPP Hospital de Cascais, Dr. José de Almeida",
                            "Hospital de Vila Franca de Xira, E.P.E."),
                    
                    Alentejo = c("Hospital do Espírito Santo - Évora, E.P.E.",
                                 "Unidade Local de Saúde do Baixo Alentejo, E.P.E.",
                                 "Unidade Local de Saúde do Litoral Alentejano, E.P.E.",
                                 "Unidade Local de Saúde do Norte Alentejano E. P. E."),
                    
                    Algarve = c("Hospital de Faro, E.P.E.",
                                "Hospital do Barlavento Algarvio, E.P.E.",
                                "Centro Hospitalar Universitário do Algarve, E.P.E."),
                    
                    Acores = c("Hospital da Horta, E.P.E.",
                               "Hospital de Santo Espírito da Ilha Terceira, EPER",
                               "Hospital do Divino Espírito Santo de Ponta Delgada, EPER"),
                    
                    Madeira = c("Serviço de Saúde da RAM, E.P.E.")
)
#Nota: Hospital da Horta with wrong designation, comes from the data (should be EPER)

region_norte <- data.frame(Institution = region_list$Norte, Region = "Norte")
region_centro <- data.frame(Institution = region_list$Centro, Region = "Centro")
region_lvt <- data.frame(Institution = region_list$LVT, Region = "LVT")
region_alentejo <- data.frame(Institution = region_list$Alentejo, Region = "Alentejo")
region_algarve <- data.frame(Institution = region_list$Algarve, Region = "Algarve")
region_acores <- data.frame(Institution = region_list$Acores, Region = "Acores")
region_madeira <- data.frame(Institution = region_list$Madeira, Region = "Madeira")

ARS_tb <- rbind(region_norte,region_centro,region_lvt,region_alentejo,region_algarve,region_acores,region_madeira)


#Save hosp_data
write.csv2(ARS_tb,'data/Inst_ARS.csv')