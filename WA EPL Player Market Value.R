library(dplyr)
library(ggplot2)
library(caret)
library(tidyverse)
library(tibble)
setwd("~/Desktop/Denison/Senior Year/Fall 2020/DA 401/Player-Market-Value-in-2017_18-EPL-season")
FIFA18data <- read.csv("FIFA 18 Data.csv")

# DATA PRE-PROCESSING

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

# removing "+[0-9]" - + sign/- followed by any digits b/c was recognizing as NA before
EPLdata <- data.frame(lapply(EPLdata, function(x) {
  gsub("\\+[0-9]", "", x)
  }))
EPLdata <- data.frame(lapply(EPLdata, function(x) {
  gsub("\\-[0-9]", "", x)
}))

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

# need to adjust for MV if has K in it, place a (.) @ begining b/c in terms of millions
EPLdata$Market_Value_in_M <- if_else(grepl("K", EPLdata$Market_Value_in_M, fixed = TRUE)
                                     ,paste0(".", EPLdata$Market_Value_in_M),EPLdata$Market_Value_in_M )
EPLdata$Wage_in_K <- gsub("K",""  ,EPLdata$Wage_in_K,ignore.case = TRUE)
EPLdata$Market_Value_in_M <- gsub("K",""  ,EPLdata$Market_Value_in_M,ignore.case = TRUE)

# counting 20 clubs to make sure there 
unique(EPLdata$Club)

# str of variables to convert all to numeric that are character
str(EPLdata)

# making all variables except club and nationality and name numeric
EPLdata2 <- EPLdata

cols = c(2, 4, 5, 7:38)    
EPLdata2[,cols] = apply(EPLdata2[,cols], 2, function(x) as.numeric(as.character(x)));

# checking for missing data values = 167
sum(is.na(EPLdata2)) 
sapply(EPLdata2, function(x) sum(is.na(x))) # all in Market value so not good

# str of variables to convert all to numeric that are character
str(EPLdata2)

# adding if one of Top 6 teams as new column
EPLdata3 <- EPLdata2%>%
  mutate(top6 = ifelse(Club == "Arsenal"|
           Club == "Chelsea"|
           Club == "Liverpool"|
           Club == "Manchester City"|
           Club == "Manchester United"|
           Club == "Tottenham Hotspur", 1, 0))

# checking to make sure new column worked
"EPLdataTest <- EPLdata3%>%
  select(Club, EPLtop6)

EPLdataTest1 <- EPLdataTest%>%
  group_by(Club)%>%
  summarise(Check = mean(EPLtop6))
"

# EXPLORATORY ANALYSIS
# descriptive statistics for age, overal, wage, mv
AgeOverallWageMV <- EPLdata3%>%
  select(Age, Overall, Wage_in_K, Market_Value_in_M)

summary(AgeOverallWageMV)

#club market value table
clubMVtable <- EPLdata3%>%
  group_by(Club)%>%
  summarise(medianMV = median(Market_Value_in_M),
            avgMV = mean(Market_Value_in_M),
            maxMV = max(Market_Value_in_M),
            minMV = min(Market_Value_in_M))

#club market value table
ageMVtable <- EPLdata3%>%
  group_by(Age)%>%
  summarise(medianMV = median(Market_Value_in_M),
            avgMV = mean(Market_Value_in_M),
            maxMV = max(Market_Value_in_M),
            minMV = min(Market_Value_in_M))

# epl top 6 table
EPLtop6 <- EPLdata3%>%
  filter(Club == "Arsenal"|
           Club == "Chelsea"|
           Club == "Liverpool"|
           Club == "Manchester City"|
           Club == "Manchester United"|
           Club == "Tottenham Hotspur")

EPLtop6mvTable <- EPLtop6%>%
  group_by(Club)%>%
  summarise(medianMV = median(Market_Value_in_M),
            avgMV = mean(Market_Value_in_M),
            maxMV = max(Market_Value_in_M),
            minMV = min(Market_Value_in_M))

# EPl other 14 table
EPLtop6vother14 <- EPLdata3%>%
  group_by(top6)%>%
  summarise(medianMV = median(Market_Value_in_M),
            avgMV = mean(Market_Value_in_M),
            maxMV = max(Market_Value_in_M),
            minMV = min(Market_Value_in_M))%>%
  mutate(ClubType = if_else(top6 == 1, "Top6", "Other14"))%>%
  select(-top6)
EPLtop6vother14 <- EPLtop6vother14[, c(5, 1, 2, 3, 4)]

# market value overall
genMVdist <- hist(as.numeric(EPLdata3$Market_Value_in_M),col='grey',alphborder=T, freq = TRUE, main = "Market Value in the 2017-18 EPL Season",xlab = "Market Value (Millions)")
top6MVdist <- hist(as.numeric(EPLtop6$Market_Value_in_M),add=T,col=scales::alpha('green', .5),border=T, freq = TRUE)

# age
genAGEdist <- hist(as.numeric(EPLdata3$Age),col='skyblue',alphborder=T, freq = TRUE, main = "Age in the 2017-18 EPL Season",xlab = "Age (Years)")

# wages
wageDistPrep <- EPLdata3%>%
  group_by(top6)%>%
  summarise(total_wage = sum(Wage_in_K))%>%
  mutate(type = if_else(top6 == 1, "top6", "other14"))

# wages % table
wageDist <- wageDistPrep%>%
  group_by(type)%>%
  summarise(perct_of_entire_leage_wages = round((total_wage/(19421+18192))*(100)))

# remove goalkepers for models

