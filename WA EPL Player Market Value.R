library(dplyr)
library(ggplot2)
library(caret)
library(tidyverse)
library(tibble)
library(rpart.plot)
library(knitr)
library(glmnet)
library(car)
library(ridge)

setwd("~/Desktop/Denison/Senior Year/Fall 2020/DA 401/Player-Market-Value-in-2017_18-EPL-season")
EPLdata <- read.csv("EPLdata.csv")

#------------------------------------------------
#
# DATA PRE-PROCESSING
#
#------------------------------------------------

# removing variables
str(EPLdata)

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
str(EPLdata)

# making all variables except club and nationality and name numeric
EPLdata2 <- EPLdata
cols = c(3, 5,6 ,8:38)    
EPLdata2[,cols] = apply(EPLdata2[,cols], 2, function(x) as.numeric(as.character(x)));
str(EPLdata2)

# still need to convert volley's from factor to numeric
EPLdata2$Volleys <- as.numeric(as.character(EPLdata2$Volleys))
str(EPLdata2$Volleys)

# checking for missing data values = 0
sum(is.na(EPLdata2)) 
sapply(EPLdata2, function(x) sum(is.na(x))) 

# checking to see if duplicate player entries / removing 5 duplicates
sum(duplicated(EPLdata2$Name)) 
EPLdata3 <- EPLdata2[!duplicated(EPLdata2$Name), ]
str(EPLdata3)

#------------------------------------------------
#
# EXPLORATORY ANALYSIS
#
#------------------------------------------------

# player distrtibution
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

# MV overall vs top 6 clubs
EPLtop6 <- EPLdata3%>%
  filter(Club == "Arsenal"|
           Club == "Chelsea"|
           Club == "Liverpool"|
           Club == "Manchester City"|
           Club == "Manchester United"|
           Club == "Tottenham Hotspur")
genMVdist <- hist(as.numeric(EPLdata3$Market_Value_in_M),col='grey',alphborder=T, freq = TRUE, main = "Market Value in the 2017-18 EPL Season",xlab = "Market Value (Millions)")
top6MVdist <- hist(as.numeric(EPLtop6$Market_Value_in_M),add=T,col=scales::alpha('green', .5),border=T, freq = TRUE)

# MV by club table
clubMVtable <- EPLdata3%>%
  group_by(Club)%>%
  summarise(medianMV = median(Market_Value_in_M))

#------------------------------------------------
#
# MODEL BUILDING
#
#------------------------------------------------

# removing 71 goalkeepers after exploratory analysis
EPLdata3$Position <- trim(EPLdata3$Position)
EPLdata4 <-  EPLdata3%>%
  filter(Position != "GK")

# create training + test set 
set.seed(1) 
part = createDataPartition(EPLdata4$Market_Value_in_M, p = 0.7, list=FALSE)
training = EPLdata4[part,]
test = EPLdata4[-part,]

  # full multivariate model
  fullMod <- lm(Market_Value_in_M ~. -Name, data =training)
  summary(fullMod)
  
  # multivariate models exploring relationships: Club / MV , Nationality / MV
  clubMod <- lm (Market_Value_in_M ~ Club, data = training)
  summary(clubMod)
  nationalityMod <- lm (Market_Value_in_M ~ Nationality, data = training)
  summary(nationalityMod)

# training + test sets w/out Club & Nationality & Wage, Overall, Potential (b/c correlated)
training1 <- training%>%
  select(-Name, -Club, -Nationality, -Wage_in_K, -Overall, -Potential)
test1 <- test%>%
  select(-Name, -Club, -Nationality, -Wage_in_K, -Overall, -Potential)

# forward training + test sets
fw_training <- training1%>%
  filter(Position == "FW")%>%
  select(-Position)
fw_test <- test1%>%
  filter(Position == "FW")%>%
  select(-Position)

# midfielder training + test sets
mf_training <- training1%>%
  filter(Position == "MF")%>%
  select(-Position)
mf_test <- test1%>%
  filter(Position == "MF")%>%
  select(-Position)

# defender training + test sets
df_training <- training1%>%
  filter(Position == "DF")%>%
  select(-Position)
df_test <- test1%>%
  filter(Position == "DF")%>%
  select(-Position)  

## Multivariate regression models

  # new all positions linear model
  lm_mod <- lm(Market_Value_in_M ~., data =training1)
  summary(lm_mod)

    # checking all positions model assumptions
    hist(lm_mod$residuals)
    lm_training_w_res <- cbind(training1,lm_mod$residuals)
    lm_training_w_res%>%
      ggplot(aes(Market_Value_in_M, `lm_mod$residuals`))+
      geom_jitter(width = 0.5)+
      geom_smooth(method = "lm")+
      ggtitle("Scatterplot of residuals from all positions linear model vs. Market Value (M)")+
      xlab("Market Value (M)")+
      ylab("residuals from all positions linear model")
    
    # all positions model predictions, accuracy, rmse
    lm_pred <- predict (lm_mod, test1)  
    lm_compare <- cbind (actual=test1$Market_Value_in_M, lm_pred)
    lm_compare <- as.data.frame(lm_compare)
    sqrt(mean((lm_pred-test1$Market_Value_in_M)^2)) 
    lm_rmse <- sqrt(mean((lm_pred-test1$Market_Value_in_M)^2)) 
    lm_accuracy <- mean(apply(lm_compare, 1, min)/apply(lm_compare, 1, max))
    
  # forward linear model
  fw_lm_mod <- lm(Market_Value_in_M ~., data =fw_training)
  summary(fw_lm_mod)
      
    # checking forward linear model assumptions
    hist(fw_lm_mod$residuals)
    fw_training_w_res <- cbind(fw_training,fw_lm_mod$residuals)
    fw_training_w_res%>%
      ggplot(aes(Market_Value_in_M, `fw_lm_mod$residuals`))+
      geom_jitter(width = 0.5)+
      geom_smooth(method = "lm")+
      ggtitle("Scatterplot of residuals from fw linear model vs. Market Value (M)")+
      xlab("Market Value (M)")+
      ylab("residuals from fw linear model")
    
    # forward model predictions, accuracy, rmse
    fw_lm_pred <- predict (fw_lm_mod, fw_test)  
    fw_lm_compare <- cbind (actual=fw_test$Market_Value_in_M, fw_lm_pred)
    fw_lm_compare <- as.data.frame(fw_lm_compare)
    sqrt(mean((fw_lm_pred-fw_test$Market_Value_in_M)^2)) 
    fw_lm_rmse <- sqrt(mean((fw_lm_pred-fw_test$Market_Value_in_M)^2))
    fw_lm_accuracy <- mean(apply(fw_lm_compare, 1, min)/apply(fw_lm_compare, 1, max))
    
  # midfielder linear model
  mf_lm_mod <- lm(Market_Value_in_M ~., data =mf_training)
  summary(mf_lm_mod)
  
    # checking midfielder linear model assumptions
    hist(mf_lm_mod$residuals)
    mf_training_w_res <- cbind(mf_training,mf_lm_mod$residuals)
    mf_training_w_res%>%
      ggplot(aes(Market_Value_in_M, `mf_lm_mod$residuals`))+
      geom_jitter(width = 0.5)+
      geom_smooth(method = "lm")+
      ggtitle("Scatterplot of residuals from mf linear model vs. Market Value (M)")+
      xlab("Market Value (M)")+
      ylab("residuals from mf linear model")
    
    # midfielder model predictions, accuracy, rmse
    mf_lm_pred <- predict (mf_lm_mod, mf_test)  
    mf_lm_compare <- cbind (actual=mf_test$Market_Value_in_M, mf_lm_pred)
    mf_lm_compare <- as.data.frame(mf_lm_compare)
    sqrt(mean((mf_lm_pred-mf_test$Market_Value_in_M)^2)) 
    mf_lm_rmse <- sqrt(mean((mf_lm_pred-mf_test$Market_Value_in_M)^2))
    mf_lm_accuracy <- mean(apply(mf_lm_compare, 1, min)/apply(mf_lm_compare, 1, max))
  
  # defender linear model  
  df_lm_mod <- lm(Market_Value_in_M ~., data =df_training)
  summary(df_lm_mod)
    
    # checking defender linear model assumptions
    hist(df_lm_mod$residuals)
    df_training_w_res <- cbind(df_training,df_lm_mod$residuals)
    df_training_w_res%>%
      ggplot(aes(Market_Value_in_M, `df_lm_mod$residuals`))+
      geom_jitter(width = 0.5)+
      geom_smooth(method = "lm")+
      ggtitle("Scatterplot of residuals from df linear model vs. Market Value (M)")+
      xlab("Market Value (M)")+
      ylab("residuals from df linear model")
    
    # midfielder model predictions, accuracy, rmse
    df_lm_pred <- predict (df_lm_mod, df_test)  
    df_lm_compare <- cbind (actual=df_test$Market_Value_in_M, df_lm_pred)
    df_lm_compare <- as.data.frame(df_lm_compare)
    sqrt(mean((df_lm_pred-df_test$Market_Value_in_M)^2)) 
    df_lm_rmse <- sqrt(mean((df_lm_pred-df_test$Market_Value_in_M)^2))
    df_lm_accuracy <- mean(apply(df_lm_compare, 1, min)/apply(df_lm_compare, 1, max))
    
## RF models    
      
  # Random forrest all position model 
  trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
  rf_mod = train(Market_Value_in_M ~., 
                              data =training1,
                              method = "rf", 
                              trControl = trctrl,
                              tuneLength = 8, 
                              ntree = 200, 
                             importance = TRUE) 
        
    # all position random forrest model outputs
    print(rf_mod)
    plot(rf_mod)
    
    # all position Random forrest predictions, accuracy, rmse
    rf_pred = predict(rf_mod,test1)
    rf_compare <- cbind (actual=test1$Market_Value_in_M, rf_pred)
    rf_compare <- as.data.frame(rf_compare)
    rf_importance = varImp(rf_mod)
    plot(rf_importance)
    rf_rmse <- sqrt(mean((rf_pred-test1$Market_Value_in_M)^2)) 
    rf_accuracy <- mean (apply(rf_compare, 1, min)/apply(rf_compare, 1, max))
        
  # forward Random forrest foward model 
  fw_rf_mod = train(Market_Value_in_M ~., 
                    data =fw_training,
                    method = "rf", 
                    trControl = trctrl,
                    tuneLength = 8, 
                    ntree = 200, 
                    importance = TRUE) 
          
    # forward random forrest model outputs
    print(fw_rf_mod)
    plot(fw_rf_mod)
      
    # forward Random forrest predictions, accuracy, rmse
    fw_rf_pred = predict(fw_rf_mod,fw_test)
    fw_rf_compare <- cbind (actual=fw_test$Market_Value_in_M, fw_rf_pred)
    fw_rf_compare <- as.data.frame(fw_rf_compare)
    fw_rf_importance = varImp(fw_rf_mod)
    plot(fw_rf_importance)
    fw_rf_rmse <- sqrt(mean((fw_rf_pred-fw_test$Market_Value_in_M)^2)) 
    fw_rf_accuracy <- mean (apply(fw_rf_compare, 1, min)/apply(fw_rf_compare, 1, max))
  
  # midfielder Random forrest model 
  mf_rf_mod = train(Market_Value_in_M ~., 
                    data =mf_training,
                    method = "rf", 
                    trControl = trctrl,
                    tuneLength = 8, 
                    ntree = 200, 
                    importance = TRUE)
    
    # midfielder random forrest model outputs
    print(mf_rf_mod)
    plot(mf_rf_mod)
  
    # midfielder Random forrest predictions, accuracy, rmse
    mf_rf_pred = predict(mf_rf_mod,mf_test)
    mf_rf_compare <- cbind (actual=mf_test$Market_Value_in_M, mf_rf_pred)
    mf_rf_compare <- as.data.frame(mf_rf_compare)
    mf_rf_importance = varImp(mf_rf_mod)
    plot(mf_rf_importance)
    mf_rf_rmse <- sqrt(mean((mf_rf_pred-mf_test$Market_Value_in_M)^2)) 
    mf_rf_accuracy <- mean (apply(mf_rf_compare, 1, min)/apply(mf_rf_compare, 1, max))
      
  # defender Random forrest model
  df_rf_mod = train(Market_Value_in_M ~., 
                      data =df_training,
                      method = "rf", 
                      trControl = trctrl,
                      tuneLength = 8, 
                      ntree = 200, 
                      importance = TRUE)
  
    # defender random forrest model outputs
    print(df_rf_mod)
    plot(df_rf_mod)
  
    # defender Random forrest predictions, accuracy, rmse
    df_rf_pred = predict(df_rf_mod,df_test)
    df_rf_compare <- cbind (actual=df_test$Market_Value_in_M, df_rf_pred)
    df_rf_compare <- as.data.frame(df_rf_compare)
    df_rf_importance = varImp(df_rf_mod)
    plot(df_rf_importance)
    df_rf_rmse <- sqrt(mean((df_rf_pred-df_test$Market_Value_in_M)^2)) 
    df_rf_accuracy <- mean (apply(df_rf_compare, 1, min)/apply(df_rf_compare, 1, max))
    
  # all positions Ridge regression model
  ridge_mod <- linearRidge(Market_Value_in_M ~ ., data = training1)
    
    # all positions Ridge regression predictions, accuracy, rmse
    ridge_pred <- predict(ridge_mod, test1)  # predict on test data
    ridge_compare <- cbind (actual=test1$Market_Value_in_M, ridge_pred)
    ridge_compare <- as.data.frame(ridge_compare)
    ridge_rmse <- sqrt(mean((ridge_pred-test1$Market_Value_in_M)^2)) 
    ridge_accuracy <- mean (apply(ridge_compare, 1, min)/apply(ridge_compare, 1, max)) 
  
  # forward Ridge regression model
  fw_ridge_mod <- linearRidge(Market_Value_in_M ~ ., data = fw_training)
    
    # forward Ridge regression predictions, accuracy, rmse
    fw_ridge_pred <- predict(fw_ridge_mod, fw_test)  
    fw_ridge_compare <- cbind (actual=fw_test$Market_Value_in_M, fw_ridge_pred)
    fw_ridge_compare <- as.data.frame(fw_ridge_compare)
    fw_ridge_rmse <- sqrt(mean((fw_ridge_pred-fw_test$Market_Value_in_M)^2))
    fw_ridge_accuracy <- mean (apply(fw_ridge_compare, 1, min)/apply(fw_ridge_compare, 1, max)) 

  # midfielder Ridge regression model
  mf_ridge_mod <- linearRidge(Market_Value_in_M ~ ., data = mf_training)
    
    # midfielder Ridge regression predictions, accuracy, rmse
    mf_ridge_pred <- predict(mf_ridge_mod, mf_test)  
    mf_ridge_compare <- cbind (actual=mf_test$Market_Value_in_M, mf_ridge_pred)
    mf_ridge_compare <- as.data.frame(mf_ridge_compare)
    mf_ridge_rmse <- sqrt(mean((mf_ridge_pred-mf_test$Market_Value_in_M)^2))
    mf_ridge_accuracy <- mean (apply(mf_ridge_compare, 1, min)/apply(mf_ridge_compare, 1, max)) 
  
  # defender Ridge regression model
  df_ridge_mod <- linearRidge(Market_Value_in_M ~ ., data = df_training)
    
    # defender Ridge regression predictions, accuracy, rmse
    df_ridge_pred <- predict(df_ridge_mod, df_test) 
    df_ridge_compare <- cbind (actual=df_test$Market_Value_in_M, df_ridge_pred)
    df_ridge_compare <- as.data.frame(df_ridge_compare)
    df_ridge_rmse <- sqrt(mean((df_ridge_pred-df_test$Market_Value_in_M)^2))
    df_ridge_accuracy <- mean (apply(df_ridge_compare, 1, min)/apply(df_ridge_compare, 1, max))
    
# table for all position 3 models' accuracy / RMSE
all_Comparison = data.frame(matrix(ncol = 3, nrow = 3))
colnames(all_Comparison) = c('Model','RMSE', 'Accuracy')
all_Comparison[1,1] = 'Linear Regression'
all_Comparison[1,2] = lm_rmse
all_Comparison[1,3] = lm_accuracy
all_Comparison[2,1] = 'Ridge Regression'
all_Comparison[2,2] = ridge_rmse
all_Comparison[2,3] = ridge_accuracy
all_Comparison[3,1] = 'Random Forrest'
all_Comparison[3,2] = rf_rmse
all_Comparison[3,3] = rf_accuracy

# table for forward only models 
fw_Comparison = data.frame(matrix(ncol = 3, nrow = 3))
colnames(fw_Comparison) = c('FW Model','RMSE', 'Accuracy')
fw_Comparison[1,1] = 'fw Linear Regression'
fw_Comparison[1,2] = fw_lm_rmse
fw_Comparison[1,3] = fw_lm_accuracy
fw_Comparison[2,1] = 'fw Ridge Regression'
fw_Comparison[2,2] = fw_ridge_rmse
fw_Comparison[2,3] = fw_ridge_accuracy
fw_Comparison[3,1] = 'fw Random Forrest'
fw_Comparison[3,2] = fw_rf_rmse
fw_Comparison[3,3] = fw_rf_accuracy

# table for midfielder only models 
mf_Comparison = data.frame(matrix(ncol = 3, nrow = 3))
colnames(mf_Comparison) = c('MF Model','RMSE', 'Accuracy')
mf_Comparison[1,1] = 'mf Linear Regression'
mf_Comparison[1,2] = mf_lm_rmse
mf_Comparison[1,3] = mf_lm_accuracy
mf_Comparison[2,1] = 'mf Ridge Regression'
mf_Comparison[2,2] = mf_ridge_rmse
mf_Comparison[2,3] = mf_ridge_accuracy
mf_Comparison[3,1] = 'mf Random Forrest'
mf_Comparison[3,2] = mf_rf_rmse
mf_Comparison[3,3] = mf_rf_accuracy

# table for defender only models 
df_Comparison = data.frame(matrix(ncol = 3, nrow = 3))
colnames(df_Comparison) = c('DF Model','RMSE', 'Accuracy')
df_Comparison[1,1] = 'df Linear Regression'
df_Comparison[1,2] = df_lm_rmse
df_Comparison[1,3] = df_lm_accuracy
df_Comparison[2,1] = 'df Ridge Regression'
df_Comparison[2,2] = df_ridge_rmse
df_Comparison[2,3] = df_ridge_accuracy
df_Comparison[3,1] = 'df Random Forrest'
df_Comparison[3,2] = df_rf_rmse
df_Comparison[3,3] = df_rf_accuracy
