# Set environs ####
library(data.table)
library(dplyr)
library(car)
library(caret)
library(ROCR)
library(MLmetrics)
library(corrplot)

dtemp = fread("Data.csv", na.strings = c('NR', "", 'N/A'))
dates = dtemp$Date
dates = as.Date(dates, format = '%d-%m-%y')
dtemp$Date = dates
rm(dates)
ds = dfinal

# Sort by dates
setkey(dtemp, Date, ATP)
data_raw = dtemp
rm(dtemp)
colnames(data_raw) = make.names(colnames(data_raw))

# Functions ####
rocc = function(actual.values,pred.model){
  pred = prediction(pred.model,actual.values)
  perf = performance(pred,"tpr","fpr")
  plot(perf,main="ROC Curve",colorize=T)
  grid()
  auc.perf = performance(pred, measure = "auc")
  auc.p=auc.perf
  print(auc.perf@y.values)
}
#

# Pre-process ####
data_stats = data_raw %>% select_if(function(col) is.integer(col) || is.character(col))
data_stats$Date = data_odds$Date
odds_preds = (setdiff(colnames(data_raw), colnames(data_stats)))
data_odds = data_raw %>% select(one_of(odds_preds))
rm(odds_preds)
# add index to each subset table
data_stats$Index = seq(from=1, to=nrow(data_stats), by=1)
data_odds$Index = seq(from=1, to=nrow(data_odds), by=1)

# recode variables ####
# Series
data_stats$Series = data_stats$Series %>% recode('International' = 'ATP250', 'International Gold' = 'ATP500',
                                                 'Masters Cup' = 'ATPFinals', 
                                                 'International Series' = 'ATP250',
                                                 'Masters')
data_stats$Series = recode(data_stats$Series, "c('International', 'International Series')='ATP250';
                           'International Gold'='ATP500';
                           c('Masters','Masters 1000')='Masters1000';
                           'Masters Cup'='ATPFinals'; 'Grand Slam'='GrandSlam'")

# Round
data_stats$Round = recode(data_stats$Round, "'0th Round'='1st Round'")

# Best.Of
data_stats$Best.of = recode(data_stats$Best.of, "-1:2 = 3")
summary(as.factor(data_stats$Best.of))

# Comment
data_stats$Comment = recode(data_stats$Comment, "c('Compleed', 'Full Time','NSY', 'Sched')='Completed';
                            c('R_Bag','Retied','retired')='Retired';
                            c('Walover','Disqualified')='Walkover'")


# Replace NAs in Wrank and LRank
data_stats$WRank[is.na(data_stats$WRank)] = 1900
data_stats$LRank[is.na(data_stats$LRank)] = 2200

# Store Higher Ranked player as Player1
for(i in 1:nrow(data_stats)){
  if( i%%1000 == 0){
    print(i)
    flush.console()
  }
  
  data_stats$P1[i] = ifelse((data_stats$WRank[i] - data_stats$LRank[i]) < 0, data_stats$Winner[i], 
                            data_stats$Loser[i])
  data_stats$P2[i] = ifelse((data_stats$WRank[i] - data_stats$LRank[i]) > 0, data_stats$Winner[i], 
                            data_stats$Loser[i])
}


# Feature Engineering ####
# Dummy to code for Player 1 win
data_stats[, P1Win := ifelse(WRank - LRank < 0, 1, 0)]

# PLayer1 and Player2 wins till date
for(k in 1:nrow(data_stats)){
  if(k%%1000 == 0){
    print(k)
    flush.console()
  }
  data_stats$P1WinsSoFar[k] = length(which(data_stats$Winner[1:(k-1)] == data_stats$P1[k]))
  data_stats$P1LossSoFar[k] = length(which(data_stats$Loser[1:(k-1)] == data_stats$P1[k]))
  data_stats$P2WinsSoFar[k] = length(which(data_stats$Winner[1:(k-1)] == data_stats$P2[k]))
  data_stats$P2LossSoFar[k] = length(which(data_stats$Loser[1:(k-1)] == data_stats$P2[k]))
  
  data_stats$P1WinsSoFarSurface[k] = length(which(data_stats$Winner[1:(k-1)] == data_stats$P1[k] & 
                                                 data_stats$Surface[1:(k-1)] == data_stats$Surface[k]))
  data_stats$P1LossSoFarSurface[k] = length(which(data_stats$Loser[1:(k-1)] == data_stats$P1[k] &
                                                 data_stats$Surface[1:(k-1)] == data_stats$Surface[k]))
  data_stats$P2WinsSoFarSurface[k] = length(which(data_stats$Winner[1:(k-1)] == data_stats$P2[k] &
                                                 data_stats$Surface[1:(k-1)] == data_stats$Surface[k]))
  data_stats$P2LossSoFarSurface[k] = length(which(data_stats$Loser[1:(k-1)] == data_stats$P2[k] &
                                                 data_stats$Surface[1:(k-1)] == data_stats$Surface[k]))
  
  data_stats$P1WinsSoFarSeries[k] = length(which(data_stats$Winner[1:(k-1)] == data_stats$P1[k] &
                                                data_stats$Series[1:(k-1)] == data_stats$Series[k]))
  data_stats$P1LossSoFarSeries[k] = length(which(data_stats$Loser[1:(k-1)] == data_stats$P1[k] &
                                                data_stats$Series[1:(k-1)] == data_stats$Series[k]))
  data_stats$P2WinsSoFarSeries[k] = length(which(data_stats$Winner[1:(k-1)] == data_stats$P2[k] &
                                                data_stats$Series[1:(k-1)] == data_stats$Series[k]))
  data_stats$P2LossSoFarSeries[k] = length(which(data_stats$Loser[1:(k-1)] == data_stats$P2[k] &
                                                data_stats$Series[1:(k-1)] == data_stats$Series[k]))
  
  data_stats$P1WinsSoFarCourt[k] = length(which(data_stats$Winner[1:(k-1)] == data_stats$P1[k] &
                                               data_stats$Court[1:(k-1)] == data_stats$Court[k]))
  data_stats$P1LossSoFarCourt[k] = length(which(data_stats$Loser[1:(k-1)] == data_stats$P1[k] &
                                               data_stats$Court[1:(k-1)] == data_stats$Court[k]))
  data_stats$P2WinsSoFarCourt[k] = length(which(data_stats$Winner[1:(k-1)] == data_stats$P2[k] &
                                               data_stats$Court[1:(k-1)] == data_stats$Court[k]))
  data_stats$P2LossSoFarCourt[k] = length(which(data_stats$Loser[1:(k-1)] == data_stats$P2[k] &
                                               data_stats$Court[1:(k-1)] == data_stats$Court[k]))
}
# Player1 wins and loss over P2
for(k in 1:nrow(data_stats)){
  if(k%%1000 == 0){
    print(k)
    flush.console()
  }
  data_stats$P1WinsOverP2SoFar[k] = length(which(data_stats$Winner[1:(k-1)] == data_stats$P1[k] &
                                                   data_stats$Loser[1:(k-1)] == data_stats$P2[k]))
  data_stats$P1LossToP2SoFar[k] = length(which(data_stats$Loser[1:(k-1)] == data_stats$P1[k] &
                                                 data_stats$Winner[1:(k-1)] == data_stats$P2[k]))

  data_stats$P1WinsOverP2SoFarSurface[k] = length(which(data_stats$Winner[1:(k-1)] == data_stats$P1[k] &
                                                   data_stats$Loser[1:(k-1)] == data_stats$P2[k] &
                                                   data_stats$Surface[1:(k-1)] == data_stats$Surface[k]))
  data_stats$P1LossToP2SoFarSurface[k] = length(which(data_stats$Loser[1:(k-1)] == data_stats$P1[k] &
                                                 data_stats$Winner[1:(k-1)] == data_stats$P2[k] &
                                                 data_stats$Surface[1:(k-1)] == data_stats$Surface[k]))

  data_stats$P1WinsOverP2SoFarSeries[k] = length(which(data_stats$Winner[1:(k-1)] == data_stats$P1[k] &
                                                   data_stats$Loser[1:(k-1)] == data_stats$P2[k] &
                                                   data_stats$Series[1:(k-1)] == data_stats$Series[k]))
  data_stats$P1LossToP2SoFarSeries[k] = length(which(data_stats$Loser[1:(k-1)] == data_stats$P1[k] &
                                                 data_stats$Winner[1:(k-1)] == data_stats$P2[k] &
                                                 data_stats$Series[1:(k-1)] == data_stats$Series[k]))

  data_stats$P1WinsOverP2SoFarCourt[k] = length(which(data_stats$Winner[1:(k-1)] == data_stats$P1[k] &
                                                   data_stats$Loser[1:(k-1)] == data_stats$P2[k] &
                                                   data_stats$Court[1:(k-1)] == data_stats$Court[k]))
  data_stats$P1LossToP2SoFarCourt[k] = length(which(data_stats$Loser[1:(k-1)] == data_stats$P1[k] &
                                                 data_stats$Winner[1:(k-1)] == data_stats$P2[k] &
                                                 data_stats$Court[1:(k-1)] == data_stats$Court[k]))

 }

col.c = c('P2WinsOverP1SoFarSurface','P2LossToP1SoFarSurface', 'P2WinsOverP1SoFarSeries','P2LossToP1SoFar',
          'P2LossToP1SoFarSeries', 'P2WinsOverP1SoFarCourt', 'P2LossToP1SoFarCourt','P2WinsOverP1SoFar')

# Winner and Loser overall results in last year
for(i in 1:nrow(data_stats)){
  if(i%%1000 == 0){
    print(i)
    flush.console()
  }
  date_req = data_stats$Date[i] - 365
  index = which(abs(data_stats$Date - date_req) == min(abs(data_stats$Date - date_req)))
  data_stats$P1WinsLastYear[i] = length(which(data_stats$Winner[index:i] == data_stats$P1[i]))
  data_stats$P1LossLastYear[i] = length(which(data_stats$Loser[index:i] == data_stats$P1[i]))
  data_stats$P2WinsLastYear[i] = length(which(data_stats$Winner[index:i] == data_stats$P2[i]))
  data_stats$P2LossLastYear[i] = length(which(data_stats$Loser[index:i] == data_stats$P2[i]))
  
}

# Win Rate for Player1 and Player2 separately
data_stats[, ':=' (P1TotalMatches = P1WinsSoFar + P1LossSoFar,
                   P1WinRate = P1WinsSoFar/(P1WinsSoFar + P1LossSoFar),
                   P1WinRateSurface = P1WinsSoFarSurface/(P1WinsSoFarSurface + P1LossSoFarSurface),
                   P1WinRateSeries = P1WinsSoFarSeries/(P1WinsSoFarSeries + P1LossSoFarSeries),
                   P1WinRateCourt = P1WinsSoFarCourt/(P1WinsSoFarCourt + P1LossSoFarCourt),
                   P2TotalMatches = P2WinsSoFar + P2LossSoFar,
                   P2WinRate = P2WinsSoFar/(P2WinsSoFar + P2LossSoFar),
                   P2WinRateSurface = P2WinsSoFarSurface/(P2WinsSoFarSurface + P2LossSoFarSurface),
                   P2WinRateSeries = P2WinsSoFarSeries/(P2WinsSoFarSeries + P2LossSoFarSeries),
                   P2WinRateCourt = P2WinsSoFarCourt/(P2WinsSoFarCourt + P2LossSoFarCourt))]

# Win rate for Player 1 over Player 2
data_stats[, ':=' (P1OverP2WinRate = P1WinsOverP2SoFar/(P1WinsOverP2SoFar + P1LossToP2SoFar),
                   P1OverP2WinRateSurface = 
                     P1WinsOverP2SoFarSurface/(P1WinsOverP2SoFarSurface + P1LossToP2SoFarSurface),
                   P1OverP2WinRateSeries = 
                     P1WinsOverP2SoFarSeries/(P1WinsOverP2SoFarSeries + P1LossToP2SoFarSeries),
                   P1OverP2WinRateCourt = 
                     P1WinsOverP2SoFarCourt/(P1WinsOverP2SoFarCourt + P1LossToP2SoFarCourt))]

# Win rate for players in Last year
data_stats[, ':=' (P1WinRateLastYear = P1WinsLastYear/(P1WinsLastYear + P1LossLastYear),
                   P2WinRateLastYear = P2WinsLastYear/(P2WinsLastYear + P2LossLastYear))]

# Difference in rank
data_stats[, ':=' (RankRatio = ifelse(P1Win==1, LRank/WRank, WRank/LRank),
                   RankDiff = ifelse(P1Win==1, LRank - WRank, WRank - LRank))]

# Extract Predictors ####
# Subset dataset to extract predictors and response. Remove data from first year to reduce NaN values
dfinal = data_stats[year(Date) > 2000 , c(32, 61:78)]

# Replace NaNs with 0
for (j in seq_len(ncol(dfinal))){
  set(dfinal,which(is.nan(dfinal[[j]])),j,0)
}
  
summary(dfinal)

# Normalize all columns by dividing by max values
dfinal = dfinal[, lapply(.SD, function(x) x/max(x)),]

# 
# Divide into train and test sets ####
# sample rows
set.seed(713)
prt = sample(nrow(dfinal), 0.7*nrow(dfinal), replace = F)
# Divide response into test and validation
respt = dfinal$P1Win[prt]
respv = dfinal$P1Win[-prt]
# remove response from predictor set
dfinal[, P1Win := NULL]
# Divide predictor set into train and test
dt = dfinal[prt,]
dv = dfinal[-prt,]

# Fit models ####
# XGB Linear
set.seed(713)
cvCtrl =trainControl(method = "repeatedcv", repeats = 5, number = 5, verboseIter = T)
xgbmod = train(x = dt, y = respt, method = "xgbLinear", trControl = cvCtrl, tuneLength = 4)
# build final predictions
xgbmod.v = predict(xgbmod, newdata = dv, type = 'raw')
rocc(actual.values = respv, pred.model = xgbmod.v)

# XGB Tree
set.seed(713)
cvCtrl =trainControl(method = "repeatedcv", repeats = 3, number = 3, verboseIter = T)
xgtmod = train(x = dt, y = as.factor(respt), method = "xgbTree", trControl = cvCtrl, tuneLength = 3)
# build final predictions
xgtmod.v = predict(xgtmod, newdata = dv, type = 'prob')
rocc(actual.values = respv, pred.model = xgtmod.v$`1`)
xgtmod.v.p = ifelse(xgtmod.v$`1` > 0.67, 1, 0)
F1_Score(y_true = respv, y_pred = xgtmod.v.p)

# Lgistic
set.seed(713)
cvCtrl =trainControl(method = "repeatedcv", repeats = 3, number = 3, verboseIter = T)
logmod = train(x = dt, y = as.factor(respt), method = "glm", trControl = cvCtrl, tuneLength = 3)
# build final predictions
logmod.v = predict(logmod, newdata = dv, type = 'prob')
rocc(actual.values = respv, pred.model = logmod.v$`1`)
logmod.v.p = ifelse(logmod.v$`1` > 0.67, 1, 0)
F1_Score(y_true = respv, y_pred = logmod.v.p)

# SVM 
set.seed(713)
cvCtrl =trainControl(method = "repeatedcv", repeats = 2, number = 2, verboseIter = T)
svmmod = train(x = dt, y = respt, method = "svmRadial", trControl = cvCtrl, tuneLength = 2)
# build final predictions
svmmod.v = predict(svmmod, newdata = dv, type = 'prob')
rocc(actual.values = respv, pred.model = svmmod.v$`1`)
svmmod.v.p = ifelse(svmmod.v$`1` > 0.67, 1, 0)
F1_Score(y_true = respv, y_pred = svmmod.v.p)

# RF 
set.seed(713)
cvCtrl =trainControl(method = "repeatedcv", repeats = 2, number = 2, verboseIter = T)
rafmod = train(x = dt, y = as.factor(respt), method = "rf", trControl = cvCtrl, tuneLength = 2)
# build final predictions
rafmod.v = predict(rafmod, newdata = dv, type = 'prob')
rocc(actual.values = respv, pred.model = rafmod.v$`1`)
rafmod.v.p = ifelse(rafmod.v$`1` > 0.67, 1, 0)
F1_Score(y_true = respv, y_pred = rafmod.v.p)

varImpPlot(rfmod)
rfmod = randomForest(x = dt, y = respt)
corrplot(cor(dt))
