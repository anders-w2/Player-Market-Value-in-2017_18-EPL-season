library(dplyr)
library(ggplot2)
library(caret)
library(tidyverse)
setwd("~/Desktop/Denison/Senior Year/Fall 2020/DA 401/Player-Market-Value-in-2017_18-EPL-season")
FIFA18data <- read.csv("FIFA 18 Data.csv")

# examining dataset
str(FIFA18data)

FIFA18data1 <- FIFA18data%>%
  select(-Flag,-Club.Logo, -Photo)%>%
  rename("Value_in_M" = Value,
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

FIFA18data1$Value_in_M <- gsub("[^0-9A-Za-z///' ]"," "  ,FIFA18data1$Value_in_M,ignore.case = TRUE)
FIFA18data1$Wage_in_K <- gsub("[^0-9A-Za-z///' ]"," "  ,FIFA18data1$Wage_in_K,ignore.case = TRUE)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
FIFA18data1$Value_in_M <- trim(FIFA18data1$Value_in_M)
FIFA18data1$Wage_in_K <- trim(FIFA18data1$Wage_in_K)

FIFA18data1$Value_in_M <- gsub(" ","."  ,FIFA18data1$Value_in_M,ignore.case = TRUE)
FIFA18data1$Value_in_M <- gsub("M",""  ,FIFA18data1$Value_in_M,ignore.case = TRUE)
FIFA18data1$Wage_in_K <- gsub("K",""  ,FIFA18data1$Wage_in_K,ignore.case = TRUE)







unique(FIFA18data1$Club)



#---OLD CODE BELOW HERE

# renaming columns from both so can merge / keeping naming conventions consistent
EPLdata1 <- EPLdata%>%
  rename("Name" = name, "Nationality" = nationality, "Club" = club)

# removingt special characters
EPLdata1$Club <- gsub("[+]", " ", EPLdata1$Club)
FIFA18data1$Name <- gsub("[.]", " ", FIFA18data1$Name)

FIFA18data1$Name <- gsub("[^0-9A-Za-z///' ]"," "  ,FIFA18data1$Name,ignore.case = TRUE)
EPLdata1$Name <- gsub("[^0-9A-Za-z///' ]"," "  ,EPLdata1$Name,ignore.case = TRUE)

EPLdata1$Name <- toupper(EPLdata1$Name)
FIFA18data1$Name <- toupper(FIFA18data1$Name)



# converting all factors to characters in both datasets for merge
EPLdata2 <- EPLdata1
FIFA18data2 <- FIFA18data1

FIFA18data2$Club <- as.character(FIFA18data2$Club)

# removing everything before space because naming not consistent in both datasets
# split on space and only have first name
EPLdata2$Name <- sub(".*? ", "", EPLdata2$Name)
FIFA18data2$Name <- sub(".*? ", "", FIFA18data2$Name)

# replacing +'s as spaces


Test <-
  inner_join(EPLdata2, FIFA18data2, by = c("Name" = "Name", "Club" = "Club"))

EPLdata3 <- unnest(EPLdata2)

View(EPLdata2)
View(FIFA18data2)
