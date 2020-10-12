library(dplyr)
library(ggplot2)
library(caret)
library(tidyverse)
setwd("~/Desktop/Denison/Senior Year/Fall 2020/DA 401/Player-Market-Value-in-2017_18-EPL-season")
FIFA18data <- read.csv("FIFA 18 Data.csv")

# examining dataset
str(FIFA18data)

FIFA18data1 <- FIFA18data%>%
  select(-X,-Flag,-Club.Logo, -Photo, -ID, # removing nonstatsitical player info
         -GK.diving, -GK.handling, -GK.kicking, -GK.positioning, -GK.reflexes,
         -CAM, -CB, -CDM, -CF,-CM,
         -LAM, -LB, -LCB, -LCM, -LDM,-LF, -LM, -LS, -LW, -LWB, 
         -Preferred.Positions, 
         -RAM, -RB, -RCB, -RCM, -RDM, -RF, -RM, -RS, -RW, -RWB,
         -ST)%>% # removing other position stats
  rename("Market_Value_in_M" = Value,
         "Wage_in_K" = Wage)%>%
  filter(Club == "Arsenal"|
           Club == "Bournemouth"|
           Club == "Brighton & Hove Albion"|
           Club == "Burnley"|
           Club == "Chelsea"|
           Club == "Crystal Palace"|
           Club == "Everton"|
           Club == "Huddersfield Town"|
           Club == "Leicester City"|
           Club == "Liverpool"|
           Club == "Manchester City"|
           Club == "Manchester United"|
           Club == "Newcastle United"|
           Club == "Southampton"|
           Club == "Stoke City"|
           Club == "Swansea City"|
           Club == "Tottenham Hotspur"|
           Club == "Watford"|
           Club == "West Bromwich Albion"|
           Club == "West Ham United")

# removing euro curency symbol
FIFA18data1$Market_Value_in_M <- gsub("[^0-9A-Za-z///' ]"," "  ,FIFA18data1$Market_Value_in_M,ignore.case = TRUE)
FIFA18data1$Wage_in_K <- gsub("[^0-9A-Za-z///' ]"," "  ,FIFA18data1$Wage_in_K,ignore.case = TRUE)

# trimming leading/trailing spaces
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
FIFA18data1$Market_Value_in_M <- trim(FIFA18data1$Market_Value_in_M)
FIFA18data1$Wage_in_K <- trim(FIFA18data1$Wage_in_K)

# getting rid of M & K so only numbers
FIFA18data1$Market_Value_in_M <- gsub(" ","."  ,FIFA18data1$Market_Value_in_M,ignore.case = TRUE)
FIFA18data1$Market_Value_in_M <- gsub("M",""  ,FIFA18data1$Market_Value_in_M,ignore.case = TRUE)
FIFA18data1$Wage_in_K <- gsub("K",""  ,FIFA18data1$Wage_in_K,ignore.case = TRUE)

# counting 20 clubs to make sure there 
unique(FIFA18data1$Club)

# str of variables to convert all to numeric that are character
str(FIFA18data1)

#MAKE ALL VARIABLES QUANTITATIVE - ecept nationality & club

# DESCRIPTIVE / EXPLORATORY ANALYSIS
# distribution of palyer market value in EPL
hist(FIFA18data1$Value_in_M)


