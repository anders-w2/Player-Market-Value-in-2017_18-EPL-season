library(dplyr)
library(ggplot2)
library(caret)
library(tidyverse)
library(tibble)
library(rpart.plot)
library(knitr)
library(glmnet)
library(car)

setwd("~/Desktop/Denison/Senior Year/Fall 2020/DA 401/Player-Market-Value-in-2017_18-EPL-season")
EPLdata <- read.csv("EPLdata.csv")

# DATA PRE-PROCESSING

# examining dataset
str(EPLdata)

# seeing positons
'
EPLdatTest <- EPLdata%>%
  group_by(Preferred.Positions)%>%
  summarise(n = n())
'
  
EPLdata <- EPLdata%>%
  rename("Position" = Preferred.Positions)%>%
  select(-X,-Flag,-Club.Logo, -Photo, -ID,
         -GK.diving, -GK.handling, -GK.kicking, -GK.positioning, -GK.reflexes,
         -CAM, -CB, -CDM, -CF,-CM,
         -LAM, -LB, -LCB, -LCM, -LDM,-LF, -LM, -LS, -LW, -LWB,
         -RAM, -RB, -RCB, -RCM, -RDM, -RF, -RM, -RS, -RW, -RWB,-ST)

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

# checking for missing data values = 0
sum(is.na(EPLdata2)) 
sapply(EPLdata2, function(x) sum(is.na(x))) 

# checking to see if duplicate player entries / removing 5 duplicates
sum(duplicated(EPLdata2$Name)) 
EPLdata3 <- EPLdata2[!duplicated(EPLdata2$Name), ]

# str of variables to convert all to numeric that are character
str(EPLdata3)

# adding if one of Top 6 teams as new column
EPLdata4 <- EPLdata3%>%
  mutate(top6 = ifelse(Club == "Arsenal"|
           Club == "Chelsea"|
           Club == "Liverpool"|
           Club == "Manchester City"|
           Club == "Manchester United"|
           Club == "Tottenham Hotspur", 1, 0))

# checking to make sure new column worked
"EPLdataTest <- EPLdata4%>%
  select(Club, EPLtop6)

EPLdataTest1 <- EPLdataTest%>%
  group_by(Club)%>%
  summarise(Check = mean(EPLtop6))
"

# EXPLORATORY ANALYSIS
# descriptive statistics for age, overal, wage, mv
AgeOverallWageMV <- EPLdata4%>%
  select(Age, Overall, Wage_in_K, Market_Value_in_M)

summary(AgeOverallWageMV)

#club market value table
clubMVtable <- EPLdata4%>%
  group_by(Club)%>%
  summarise(medianMV = median(Market_Value_in_M),
            avgMV = mean(Market_Value_in_M),
            maxMV = max(Market_Value_in_M),
            minMV = min(Market_Value_in_M))

#club market value table
ageMVtable <- EPLdata4%>%
  group_by(Age)%>%
  summarise(medianMV = median(Market_Value_in_M),
            avgMV = mean(Market_Value_in_M),
            maxMV = max(Market_Value_in_M),
            minMV = min(Market_Value_in_M))

# epl top 6 table
EPLtop6 <- EPLdata4%>%
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
EPLtop6vother14 <- EPLdata4%>%
  group_by(top6)%>%
  summarise(medianMV = median(Market_Value_in_M),
            avgMV = mean(Market_Value_in_M),
            maxMV = max(Market_Value_in_M),
            minMV = min(Market_Value_in_M))%>%
  mutate(ClubType = if_else(top6 == 1, "Top6", "Other14"))%>%
  select(-top6)
EPLtop6vother14 <- EPLtop6vother14[, c(5, 1, 2, 3, 4)]

# market value overall
genMVdist <- hist(as.numeric(EPLdata4$Market_Value_in_M),col='grey',alphborder=T, freq = TRUE, main = "Market Value in the 2017-18 EPL Season",xlab = "Market Value (Millions)")
top6MVdist <- hist(as.numeric(EPLtop6$Market_Value_in_M),add=T,col=scales::alpha('green', .5),border=T, freq = TRUE)

# age
genAGEdist <- hist(as.numeric(EPLdata4$Age),col='skyblue',alphborder=T, freq = TRUE, main = "Age in the 2017-18 EPL Season",xlab = "Age (Years)")

# wages
wageDistPrep <- EPLdata4%>%
  group_by(top6)%>%
  summarise(total_wage = sum(Wage_in_K))%>%
  mutate(type = if_else(top6 == 1, "top6", "other14"))

# wages % table
wageDist <- wageDistPrep%>%
  group_by(type)%>%
  summarise(perct_of_entire_leage_wages = round((total_wage/(19421+18192))*(100)))

# MODEL BUILDING

#removing 71 goalkeepers after exploratory analysis
allgks <- c("D. Ospina"
            ,'A. Boruc'
            ,'P. Čech'
            ,'A. Begović'
            ,'A. Federici'
            ,'A. Ramsdale'
            ,'J. Holmes'
            ,"P. O'Flaherty"
            ,'M. Ryan'
            ,'N. Mäenpää'
            ,'Robert Sanchez'
            ,'T. Krul'
            , 'A. Legzdins'
            , 'A. Lindegaard'
            , 'M. Howarth'
            , 'N. Pope'
            , 'Eduardo'
            , 'M. Delač'
            , 'T. Courtois'
            , 'W. Caballero'
            , 'J. Speroni'
            , 'W. Hennessey'
            , 'J. Pickford'
            , 'Joel Robles'
            , 'M. Stekelenburg'
            , 'J. Lössl'
            , 'R. Schofield'
            , 'B. Hamer'
            , 'D. Iversen'
            , 'E. Jakupović'
            , 'K. Schmeichel'
            , 'A. Bogdán'
            , 'L. Karius'
            , 'S. Mignolet'
            , 'D. Ward'
            , 'A. Muric'
            , 'C. Bravo'
            , 'Ederson'
            , 'De Gea'
            , 'Joel Pereira'
            , 'S. Romero'
            , 'R. Elliot'
            , 'K. Darlow'
            , 'F. Woodman'
            , 'F. Forster'
            , 'A. McCarthy'
            , 'S. Taylor'
            , 'J. Butland'
            , 'L. Grant'
            , 'J. Haugaard'
            , 'D. Gyollai'
            , 'L. Fabiański'
            , 'K. Nordfeldt'
            , 'E. Mulder'
            , 'G. Zabret'
            , 'H. Lloris'
            , 'M. Vorm'
            , 'P. Gazzaniga'
            , 'B. Austin'
            , 'A. Whiteman'
            , 'O. Karnezis'
            , 'Gomes'
            , 'D. Bachmann'
            , 'B. Foster'
            , 'B. Myhill'
            , 'A. Palmer'
            , 'B. House'
            , 'E. Ross'
            , 'J. Hart'
            , 'Adrián'
            , 'N. Trott')

EPLdata5 <-  filter(EPLdata4, !(Name %in% allgks))

# checking to make sure removed all 71 (based on obs length in both datasets)
#645-574 = 71

#checking to see if Club and Top6 are correlated but aren't (-0.1)

# **NEED TO MAKE 3 SEPARATE MODELS: DF + MF + FW***

# VIF for feature selection
fitvif <- lm(Market_Value_in_M ~. -Name -top6, data =EPLdata5)
kable(vif(fitvif),align = 'c')


# create training + test set 
set.seed(1) #Set seed for replicability
part = createDataPartition(EPLdata5$Market_Value_in_M, p = 0.7, list=FALSE)
training = EPLdata5[part,]
test = EPLdata5[-part,]

# linear multivariate regression
lm_model = train(salary ~ R + H + HR + RBI + SO + X2B + X3B + SB + IBB, 
                 data = training, 
                 method = "lm")
lm_pred = predict(lm_model,test)
sqrt(mean((lm_pred-test$salary)^2)) #RMSE for linear regression

# Random forrests
ctrl = trainControl(method="repeatedcv",number=10,repeats=3)

# Ridge regression i


