setwd("C:/Users/johan/Git_Elm/data")
Fussballer <- read.csv("fifa21_male2.csv")
library(tidyverse)

#Anpassen der Größe in CM
Fussballer$Height <- (separate(data = Fussballer, col = Height, into = c("H1", "H2"), convert = TRUE) %>%
                        transmute(Height = H1 * 30.48 + H2 * 2.54))

Fussballer$Height <- round(Fussballer$Height)
#Löschen der Beschriftung am Ende von Weight (lbs) 
Fussballer$Weight <-  parse_number(Fussballer$Weight)

#Durch Löschen von Leerwerten können nicht aktive Fussballer und fehlende Werte gefiltert werden
Fussballer <- na.omit(Fussballer)

FussballerScatterplot <- data.frame(Fussballer$Name,
                                    Fussballer$Age,
                                    Fussballer$Club,
                                    Fussballer$OVA,
                                    Fussballer$POT,
                                    Fussballer$Height,
                                    Fussballer$Weight,
                                    Fussballer$Nationality,
                                    Fussballer$BP,
                                    Fussballer$PAC,
                                    Fussballer$SHO,
                                    Fussballer$PAS,
                                    Fussballer$DRI,
                                    Fussballer$DEF,
                                    Fussballer$PHY
                                    )
FussballerScatterplot <-  FussballerScatterplot[order(-FussballerScatterplot$Fussballer.OVA),]
  
write.csv(FussballerScatterplot,"spData")


FussballerPara <- <- data.frame(Fussballer$Name,
                                Fussballer$Age,
                                Fussballer$Club,
                                Fussballer$OVA,
                                Fussballer$POT,
                                Fussballer$Height,
                                Fussballer$Nationality,
                                Fussballer$BP,
                                Fussballer$PAC,
                                Fussballer$SHO,
                                Fussballer$PAS,
                                Fussballer$DRI,
                                Fussballer$DEF,
                                Fussballer$PHY
)
  

FussballerBaum <-  data.frame(Fussballer$Name,
                              Fussballer$Age,
                              Fussballer$Club)

#Filtern von Fussballern, die nicht in den gewünschten Ligen spielen

filterligen <-  c("VfL Wolfsburg", 
                  "Bayer 04 Leverkusen", 
                  "SC Freiburg", 
                  "1. FC Köln", 
                  "1. FSV Mainz 05", 
                  "1. FC Union Berlin", 
                  "Hertha BSC", 
                  "TSG 1899 Hoffenheim", 
                  "RB Leipzig", 
                  "Borussia MÃ¶nchengladbach", 
                  "Eintracht Frankfurt", 
                  "DSC Arminia Bielefeld", 
                  "SV Werder Bremen", 
                  "FC Schalke 04", 
                  "FC Augsburg", 
                  "Borussia Dortmund",
                  "FC Bayern MÃ¼nchen",
                  "VfB Stuttgart",
                  "Real Madrid",
                  "FC Barcelona",
                  "AtlÃ©tico Madrid",
                  "Athletic Bilbao",
                  "Betis Sevilla",
                  "CA Osasuna",
                  "Celta Vigo",
                  "Deportivo AlavÃ©s",
                  "CÃ¡diz CF",
                  "Elche CF",
                  "Getafe FC",
                  "Sevilla FC",
                  "Valencia CF",
                  "Villarreal CF",
                  "Granada CF",
                  "Real Sociedad",
                  "Real Valladolid",
                  "SD Eibar",
                  "SD Huesca",
                  "Levante UD")

FussballerBaum <- subset(FussballerBaum, Fussballer.Club %in% filterligen)
write.csv(FussballerBaum,"baumData")