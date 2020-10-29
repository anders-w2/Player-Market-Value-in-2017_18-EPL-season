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

# removing variables
EPLdata <- EPLdata%>%
  rename("Position" = Preferred.Positions)%>%
  select(-X,-Flag,-Club.Logo, -Photo, -ID,
         -GK.diving, -GK.handling, -GK.kicking, -GK.positioning, -GK.reflexes,
         -CAM, -CB, -CDM, -CF,-CM,
         -LAM, -LB, -LCB, -LCM, -LDM,-LF, -LM, -LS, -LW, -LWB,
         -RAM, -RB, -RCB, -RCM, -RDM, -RF, -RM, -RS, -RW, -RWB,-ST, -X.1)

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

cols = c(3, 5,6 ,8:38)    
EPLdata2[,cols] = apply(EPLdata2[,cols], 2, function(x) as.numeric(as.character(x)));

# checking results of manipulation
str(EPLdata2)

# still need to convert volley's from factor to numeric
EPLdata2$Volleys <- as.numeric(as.character(EPLdata2$Volleys))

# checking results of conversion
str(EPLdata2$Volleys)

# checking for missing data values = 0
sum(is.na(EPLdata2)) 
sapply(EPLdata2, function(x) sum(is.na(x))) 

# checking to see if duplicate player entries / removing 5 duplicates
sum(duplicated(EPLdata2$Name)) 
EPLdata3 <- EPLdata2[!duplicated(EPLdata2$Name), ]

# str of variables to convert all to numeric that are character
str(EPLdata3)

# -----EXPLORATORY ANALYSIS------

# seeing distrtibution
PositionDistTable <- EPLdata3%>%
  group_by(Position)%>%
  summarise(n = n())

# descriptive statistics for age, overal, wage, mv
AgeOverallWageMV <- EPLdata3%>%
  select(Age, Overall, Wage_in_K, Market_Value_in_M)

summary(AgeOverallWageMV)

# variance in MV by Position
EPLdata3%>%
  ggplot(aes(x = reorder(Position, Market_Value_in_M, FUN = median), y = Market_Value_in_M))+
  geom_boxplot()

# variance in MV by Age
EPLdata3%>%
  ggplot(aes(x = reorder(Age, Market_Value_in_M, FUN = median), y = Market_Value_in_M))+
  geom_boxplot()

# market value overall vs top 6 clubs
EPLtop6 <- EPLdata3%>%
  filter(Club == "Arsenal"|
           Club == "Chelsea"|
           Club == "Liverpool"|
           Club == "Manchester City"|
           Club == "Manchester United"|
           Club == "Tottenham Hotspur")

genMVdist <- hist(as.numeric(EPLdata3$Market_Value_in_M),col='grey',alphborder=T, freq = TRUE, main = "Market Value in the 2017-18 EPL Season",xlab = "Market Value (Millions)")
top6MVdist <- hist(as.numeric(EPLtop6$Market_Value_in_M),add=T,col=scales::alpha('green', .5),border=T, freq = TRUE)


#club market value table
clubMVtable <- EPLdata3%>%
  group_by(Club)%>%
  summarise(medianMV = median(Market_Value_in_M))

# ------MODEL BUILDING------

#removing 71 goalkeepers after exploratory analysis
EPLdata3$Position <- trim(EPLdata3$Position)

EPLdata4 <-  EPLdata3%>%
  filter(Position != "GK")

# checking to make sure removed all 71 (based on obs length in both datasets)
#645-574 = 71

# create training + test set 
set.seed(1) #Set seed for replicability
part = createDataPartition(EPLdata4$Market_Value_in_M, p = 0.7, list=FALSE)
training = EPLdata4[part,]
test = EPLdata4[-part,]

# full model
fullMod <- lm(Market_Value_in_M ~. -Name, data =training)
summary(fullMod)

# sample multivariate regression exploring relationships: Club / MV , Nationality / MV
clubMod <- lm (Market_Value_in_M ~ Club, data = training)
summary(clubMod)

nationalityMod <- lm (Market_Value_in_M ~ Nationality, data = training)
summary(nationalityMod)

# full multivariate model w/out Club & Nationality
training1 <- training%>%
  select(-Name, -Club, -Nationality)

test1 <- test%>%
  select(-Name, -Club, -Nationality)

lm_mod <- lm(Market_Value_in_M ~., data =training1)
summary(lm_mod)

# making predictions w/multivariate model
lm_pred = predict(lm_mod,test1)
sqrt(mean((lm_pred-test1$Market_Value_in_M)^2)) #RMSE for linear regression

# Random forrest
trctrl = trainControl(method = "oob") 
rf_mod = train(Market_Value_in_M ~., 
                            data =training1,
                            method = "rf", # rf
                            trControl = trctrl,
                            tuneLength = 8, 
                            ntree = 200, 
                            importance = TRUE) 


plot(rf_mod$results$mtry,rf_mod$results$RMSE)

rf_pred = predict(rf_mod,test1)
#RMSE for random forest
sqrt(mean((rf_pred-test1$Market_Value_in_M)^2)) 
# > no-free lunch to see what method works better for each problem
rf_importance = varImp(rf_mod)
plot(Importance2)


# Ridge regression ---



# Make table of predictions for each and how accurate are / include RMSE ---



# do assumption checks / plots & outputs of each model for validations



