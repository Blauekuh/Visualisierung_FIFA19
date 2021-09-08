setwd("C:/Users/johan/Git_Elm/data")
Fussballer <- read.csv("fifa21_male2.csv")#, encoding = "UTF-8")
library(tidyverse)
library(stringi)

#Durch Löschen von Leerwerten können nicht aktive Fussballer und fehlende Werte gefiltert werden
Fussballer <- na.omit(Fussballer)


#Anpassen der Größe in CM
Fussballer$Height <- (separate(data = Fussballer, col = Height, into = c("H1", "H2"), convert = TRUE) %>%
                        transmute(Height = H1 * 30.48 + H2 * 2.54))

Fussballer$Height <- round(Fussballer$Height)
#Löschen der Beschriftung am Ende von Weight (lbs) 
Fussballer$Weight <-  parse_number(Fussballer$Weight)




#Umwandeln des Gehaltes in einen numerischen Wert
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

Fussballer$Wage <- gsub("[â,¬]", "",Fussballer$Wage)
#Fussballer$Wage <- parse_number(Fussballer$Wage)

test1 <- substrRight(Fussballer$Wage,1)

for(i in 1:length(Fussballer$Wage)){
  if (Fussballer$Wage[i]==0){ Fussballer$Wage[i] = 0
  }else if(grepl(test1[i],"M",fixed=TRUE )){ Fussballer$Wage[i] = parse_number(Fussballer$Wage[i])*1000
  }else if(grepl((test1[i]),"K",fixed=TRUE )){ Fussballer$Wage[i] = parse_number(Fussballer$Wage[i])
  }else {Fussballer$Wage[i] = parse_number(Fussballer$Wage[i])/1000
  }
}

#Umwandeln des Wertes in einen numerischen Wert
Fussballer$Value <- gsub("[â,¬]", "",Fussballer$Value)


test2 <- substrRight(Fussballer$Value,1)

for(i in 1:length(Fussballer$Value)){
              if (Fussballer$Value[i]==0){ Fussballer$Value[i] = 0
              }else if(grepl(test2[i],"M",fixed=TRUE )){ Fussballer$Value[i] = parse_number(Fussballer$Value[i])*1000
              }else if(grepl((test2[i]),"K",fixed=TRUE )){ Fussballer$Value[i] = parse_number(Fussballer$Value[i])
              }else {Fussballer$Value[i] = parse_number(Fussballer$Value[i])/1000
              }
              }

      

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
                                    Fussballer$PHY,
                                    Fussballer$Wage,
                                    Fussballer$Value
                                    )
FussballerScatterplot <-  FussballerScatterplot[order(-FussballerScatterplot$Fussballer.OVA),]
  
write.csv(FussballerScatterplot,"spData")


FussballerPara    <- data.frame(Fussballer$Name,
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
 
write.csv(FussballerPara,"pkData")
 

FussballerBaum <-  data.frame(Fussballer$Name,
                              Fussballer$Age,
                              Fussballer$Club,
                              Fussballer$BP)

#Filtern von Fussballern, die nicht in den gewünschten Ligen spielen

filterligen <-  c("VfL Wolfsburg", 
                  "Bayer 04 Leverkusen", 
                  "SC Freiburg", 
                  "1. FC KÃ¶ln", 
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
                  "Athletic Club de Bilbao",
                  "Real Betis",
                  "CA Osasuna",
                  "RC Celta",
                  "Deportivo AlavÃ©s",
                  "CÃ¡diz CF",
                  "Elche CF",
                  "Getafe CF",
                  "Sevilla FC",
                  "Valencia CF",
                  "Villarreal CF",
                  "Granada CF",
                  "Real Sociedad",
                  "Real Valladolid CF",
                  "SD Eibar",
                  "SD Huesca",
                  "Levante UD",
                  "Juventus",
                  "Napoli",
                  "Milan",
                  "Inter",
                  "Lazio",
                  "Roma",
                  "Atalanta",
                  "Sassuolo",
                  "Sampdoria",
                  "Hellas Verona",
                  "Genoa",
                  "Bologna",
                  "Fiorentina",
                  "Udinese",
                  "Spezia",
                  "Cagliari",
                  "Torino",
                  "Benevento",
                  "Crotone",
                  "Parma",
                  "Arsenal",
                  "Aston Villa",
                  "Brighton & Hove Albion",
                  "Burnley",
                  "Chelsea",
                  "Crystal Palace",
                  "Everton",
                  "Fulham",
                  "Leeds United",
                  "Leicester City",
                  "Liverpool",
                  "Manchester City",
                  "Manchester United",
                  "Sheffield United",
                  "Newcastle United",
                  "Southampton",
                  "Tottenham Hotspur",
                  "West Bromwich Albion",
                  "West Ham United",
                  "Wolverhampton Wanderers"
                  )

FussballerBaum <- subset(FussballerBaum, Fussballer.Club %in% filterligen)
write.csv(FussballerBaum,"baumData")