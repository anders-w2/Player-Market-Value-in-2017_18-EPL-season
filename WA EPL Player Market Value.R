library(dplyr)
library(ggplot2)
library(caret)
library(tidyverse)
setwd("~/Desktop/Denison/Senior Year/Fall 2020/DA 401/Player-Market-Value-in-2017_18-EPL-season")
FIFA18data <- read.csv("FIFA 18 Data.csv")

# examining dataset
str(FIFA18data)

EPLdata <- FIFA18data%>%
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
EPLdata$Market_Value_in_M <- gsub("[^0-9A-Za-z///' ]"," "  ,EPLdata$Market_Value_in_M,ignore.case = TRUE)
EPLdata$Wage_in_K <- gsub("[^0-9A-Za-z///' ]"," "  ,EPLdata$Wage_in_K,ignore.case = TRUE)

# trimming leading/trailing spaces
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
EPLdata$Market_Value_in_M <- trim(EPLdata$Market_Value_in_M)
EPLdata$Wage_in_K <- trim(EPLdata$Wage_in_K)

# getting rid of M & K so only numbers
EPLdata$Market_Value_in_M <- gsub(" ","."  ,EPLdata$Market_Value_in_M,ignore.case = TRUE)
EPLdata$Market_Value_in_M <- gsub("M",""  ,EPLdata$Market_Value_in_M,ignore.case = TRUE)
EPLdata$Wage_in_K <- gsub("K",""  ,EPLdata$Wage_in_K,ignore.case = TRUE)

# counting 20 clubs to make sure there 
unique(EPLdata$Club)

# str of variables to convert all to numeric that are character
str(EPLdata)

# making all variables except club and nationality and name numeric
EPLdata2 <- EPLdata
EPLdata2[cols] <- lapply(EPLdata2[cols], as.numeric)
str(EPLdata2)

# checking for missing data values = 167
sum(is.na(EPLdata2)) 

sapply(EPLdata2, function(x) sum(is.na(x))) # all in Market value so not good

EPLdata3 <- EPLdata2%>%
  filter(Market_Value_in_M != "NA")

# DESCRIPTIVE / EXPLORATORY ANALYSIS

# GENERAL DISTRIBUTIONS
str(EPLdata3$Market_Value_in_M)
hist(as.numeric(EPLdata3$Market_Value_in_M)) # make look nicer

hist(as.numeric(EPLdata3$Wage_in_K)) # make look nicer

hist(as.numeric(EPLdata3$Age)) # make look nicer

hist(as.numeric(EPLdata3$Overall)) # make look nicer / make 2nd colored 

ggplot(EPLdata3, aes(as.numeric(Overall), fill = Club, color = "Black"))+
  geom_histogram()

# DO TOP 6 clubs graphs 
ggplot(EPLdata3, aes(as.numeric(EPLdata3$Marketv))


