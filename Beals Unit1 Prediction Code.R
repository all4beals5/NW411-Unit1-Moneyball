##################################################
### Unit 1 Assignment - Moneyball
### MSDS 411-DL-56
### Brandi Beals

##################################################
### Load packages
library(corrplot)
library(PerformanceAnalytics)
library(mice)
library(car)

##################################################
### Set working directory & read data
setwd("C:/Users/Brara/Dropbox/Masters in Predictive Analytics/411-DL-56/W-unit 1 Weeks 1 to 3/Unit 1 - Moneyball/4 Homework")
moneyball=read.csv("moneyball_test.csv",header=T)

### Create new fields
moneyball$TEAM_BATTING_1B <- moneyball$TEAM_BATTING_H-moneyball$TEAM_BATTING_2B-moneyball$TEAM_BATTING_3B-moneyball$TEAM_BATTING_HR
moneyball$TEAM_BASERUN_SB_RATIO <- moneyball$TEAM_BASERUN_SB/(moneyball$TEAM_BASERUN_SB+moneyball$TEAM_BASERUN_CS)
moneyball$TEAM_BASERUN_CS_RATIO <- moneyball$TEAM_BASERUN_CS/(moneyball$TEAM_BASERUN_SB+moneyball$TEAM_BASERUN_CS)
moneyball$TEAM_BATTING_WALK <- moneyball$TEAM_BATTING_BB+moneyball$TEAM_BATTING_HBP

### Create indicator variables for NA values
moneyball$TEAM_BATTING_SO_IMP <- ifelse(is.na(moneyball$TEAM_BATTING_SO), 1, 0)
moneyball$TEAM_BASERUN_SB_IMP <- ifelse(is.na(moneyball$TEAM_BASERUN_SB), 1, 0)
moneyball$TEAM_BASERUN_CS_IMP <- ifelse(is.na(moneyball$TEAM_BASERUN_CS), 1, 0)
moneyball$TEAM_BATTING_HBP_IMP <- ifelse(is.na(moneyball$TEAM_BATTING_HBP), 1, 0)
moneyball$TEAM_PITCHING_SO_IMP <- ifelse(is.na(moneyball$TEAM_PITCHING_SO), 1, 0)
moneyball$TEAM_FIELDING_DP_IMP <- ifelse(is.na(moneyball$TEAM_FIELDING_DP), 1, 0)
moneyball$TEAM_BASERUN_SB_RATIO_IMP <- ifelse(is.na(moneyball$TEAM_BASERUN_SB_RATIO), 1, 0)
moneyball$TEAM_BASERUN_CS_RATIO_IMP <- ifelse(is.na(moneyball$TEAM_BASERUN_CS_RATIO), 1, 0)
moneyball$TEAM_BATTING_WALK_IMP <- ifelse(is.na(moneyball$TEAM_BATTING_WALK), 1, 0)

### Impute with mean
moneyballmean <- moneyball

moneyballmean$TEAM_BATTING_SO[is.na(moneyballmean$TEAM_BATTING_SO)==TRUE] <- mean(moneyballmean$TEAM_BATTING_SO, na.rm = TRUE)
moneyballmean$TEAM_BASERUN_SB[is.na(moneyballmean$TEAM_BASERUN_SB)==TRUE] <- mean(moneyballmean$TEAM_BASERUN_SB, na.rm = TRUE)
moneyballmean$TEAM_BASERUN_CS[is.na(moneyballmean$TEAM_BASERUN_CS)==TRUE] <- mean(moneyballmean$TEAM_BASERUN_CS, na.rm = TRUE)
moneyballmean$TEAM_BATTING_HBP[is.na(moneyballmean$TEAM_BATTING_HBP)==TRUE] <- mean(moneyballmean$TEAM_BATTING_HBP, na.rm = TRUE)
moneyballmean$TEAM_PITCHING_SO[is.na(moneyballmean$TEAM_PITCHING_SO)==TRUE] <- mean(moneyballmean$TEAM_PITCHING_SO, na.rm = TRUE)
moneyballmean$TEAM_FIELDING_DP[is.na(moneyballmean$TEAM_FIELDING_DP)==TRUE] <- mean(moneyballmean$TEAM_FIELDING_DP, na.rm = TRUE)
moneyballmean$TEAM_BASERUN_SB_RATIO[is.na(moneyballmean$TEAM_BASERUN_SB_RATIO)==TRUE] <- mean(moneyballmean$TEAM_BASERUN_SB_RATIO, na.rm = TRUE)
moneyballmean$TEAM_BASERUN_CS_RATIO[is.na(moneyballmean$TEAM_BASERUN_CS_RATIO)==TRUE] <- mean(moneyballmean$TEAM_BASERUN_CS_RATIO, na.rm = TRUE)
moneyballmean$TEAM_BATTING_WALK[is.na(moneyballmean$TEAM_BATTING_WALK)==TRUE] <- mean(moneyballmean$TEAM_BATTING_WALK, na.rm = TRUE)

### Log transformation
moneyballlog <- moneyballmean
moneyballlog$TEAM_FIELDING_E <- log(moneyballlog$TEAM_FIELDING_E)
moneyballlog$TEAM_PITCHING_H <- log(moneyballlog$TEAM_PITCHING_H)
moneyballlog$TEAM_PITCHING_SO[moneyballlog$TEAM_PITCHING_SO==0] <- 1
moneyballlog$TEAM_PITCHING_SO <- log(moneyballlog$TEAM_PITCHING_SO)
moneyballlog$TEAM_PITCHING_BB[moneyballlog$TEAM_PITCHING_BB==0] <- 1
moneyballlog$TEAM_PITCHING_BB <- log(moneyballlog$TEAM_PITCHING_BB)





# Fixing na's
moneyball_test$TEAM_BATTING_1B <- moneyball_test$TEAM_BATTING_H - moneyball_test$TEAM_BATTING_HR -
  moneyball_test$TEAM_BATTING_3B -moneyball_test$TEAM_BATTING_2B
moneyball_test$TEAM_BATTING_SO[is.na(moneyball_test$TEAM_BATTING_SO)] = mean(moneyball_test$TEAM_BATTING_SO, na.rm = TRUE)
moneyball_test$TEAM_BATTING_HBP[is.na(moneyball_test$TEAM_BATTING_HBP)] = mean(moneyball_test$TEAM_BATTING_HBP, na.rm = TRUE)
moneyball_test$TEAM_BASERUN_SB[is.na(moneyball_test$TEAM_BASERUN_SB)] = mean(moneyball_test$TEAM_BASERUN_SB, na.rm = TRUE)
moneyball_test$TEAM_BASERUN_CS[is.na(moneyball_test$TEAM_BASERUN_CS)] = mean(moneyball_test$TEAM_BASERUN_CS, na.rm = TRUE)
moneyball_test$TEAM_FIELDING_DP[is.na(moneyball_test$TEAM_FIELDING_DP)] = mean(moneyball_test$TEAM_FIELDING_DP, na.rm = TRUE)
moneyball_test$TEAM_PITCHING_SO[is.na(moneyball_test$TEAM_PITCHING_SO)] = mean(moneyball_test$TEAM_PITCHING_SO, na.rm = TRUE)
moneyball_test$TEAM_BASERUN_CS[moneyball_test$TEAM_BASERUN_CS < 1] = 1
moneyball_test$SB_PCT <- moneyball_test$TEAM_BASERUN_SB/(1.0*moneyball_test$TEAM_BASERUN_SB+moneyball_test$TEAM_BASERUN_CS)
moneyball_test$SB_PCT[is.na(moneyball_test$SB_PCT)] = mean(moneyball_test$SB_PCT)
moneyball_test$log_TEAM_BASERUN_CS <- log(moneyball_test$TEAM_BASERUN_CS)


# Stand Alone Scoring
moneyball_test$P_TARGET_WINS <- 68.453769 + 0.036145 * moneyball_test$TEAM_BATTING_1B -
  0.011591* moneyball_test$TEAM_BATTING_2B + 
  0.203495* moneyball_test$TEAM_BATTING_3B +
  0.618848* moneyball_test$TEAM_BATTING_HR +
  0.154710* moneyball_test$TEAM_BATTING_BB -
  0.166438* moneyball_test$TEAM_BATTING_SO + 
  0.100967* moneyball_test$TEAM_BASERUN_SB -
  0.487091* moneyball_test$TEAM_PITCHING_HR - 
  0.115184* moneyball_test$TEAM_PITCHING_BB + 
  0.142082* moneyball_test$TEAM_PITCHING_SO -
  0.110246* moneyball_test$TEAM_FIELDING_E - 
  0.111298* moneyball_test$TEAM_FIELDING_DP -
  7.868975* moneyball_test$SB_PCT -
  4.986598* moneyball_test$log_TEAM_BASERUN_CS

#subset of data set for the deliverable "Scored data file"
prediction <- moneyball_test[c("INDEX","P_TARGET_WINS")]

### Prediction output 
write.xlsx(prediction, file = "write.xlsx", sheetName = "Predictions",
           col.names = TRUE)