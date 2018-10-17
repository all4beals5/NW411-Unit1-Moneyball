##################################################
### Unit 1 Assignment - Moneyball
### MSDS 411-DL-56
### Brandi Beals

##################################################
### Load packages
library(openxlsx)

##################################################
### Set working directory & read data
# home setwd("C:/Users/Brara/Dropbox/Masters in Predictive Analytics/411-DL-56/W-unit 1 Weeks 1 to 3/Unit 1 - Moneyball/4 Homework")
setwd("C:/Users/bbeals/Dropbox (Personal)/Masters in Predictive Analytics/411-DL-56/W-unit 1 Weeks 1 to 3/Unit 1 - Moneyball/4 Homework")
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
moneyball$TEAM_BATTING_SO[is.na(moneyball$TEAM_BATTING_SO)==TRUE] <- 735.6053
moneyball$TEAM_BASERUN_SB[is.na(moneyball$TEAM_BASERUN_SB)==TRUE] <- 124.7618
moneyball$TEAM_BASERUN_CS[is.na(moneyball$TEAM_BASERUN_CS)==TRUE] <- 52.80386
moneyball$TEAM_BATTING_HBP[is.na(moneyball$TEAM_BATTING_HBP)==TRUE] <- 59.35602
moneyball$TEAM_PITCHING_SO[is.na(moneyball$TEAM_PITCHING_SO)==TRUE] <- 817.7305
moneyball$TEAM_FIELDING_DP[is.na(moneyball$TEAM_FIELDING_DP)==TRUE] <- 146.3879
moneyball$TEAM_BASERUN_SB_RATIO[is.na(moneyball$TEAM_BASERUN_SB_RATIO)==TRUE] <- 0.6327151
moneyball$TEAM_BASERUN_CS_RATIO[is.na(moneyball$TEAM_BASERUN_CS_RATIO)==TRUE] <- 0.3672849
moneyball$TEAM_BATTING_WALK[is.na(moneyball$TEAM_BATTING_WALK)==TRUE] <- 602.6754

### Log transformation
moneyball$TEAM_FIELDING_E <- log(moneyball$TEAM_FIELDING_E)
moneyball$TEAM_PITCHING_H <- log(moneyball$TEAM_PITCHING_H)
moneyball$TEAM_PITCHING_SO[moneyball$TEAM_PITCHING_SO==0] <- 1
moneyball$TEAM_PITCHING_SO <- log(moneyball$TEAM_PITCHING_SO)
moneyball$TEAM_PITCHING_BB[moneyball$TEAM_PITCHING_BB==0] <- 1
moneyball$TEAM_PITCHING_BB <- log(moneyball$TEAM_PITCHING_BB)

### Model scoring
moneyball$P_TARGET_WINS <- 206.09627163 + 
  0.04180158 * moneyball$TEAM_BATTING_H -
  0.03462849 * moneyball$TEAM_BATTING_2B + 
  0.17849720 * moneyball$TEAM_BATTING_3B +
  0.07855182 * moneyball$TEAM_BATTING_HR +
  0.08272586 * moneyball$TEAM_BASERUN_SB -
  0.04521347 * moneyball$TEAM_BASERUN_CS -
  12.64165775 * moneyball$TEAM_PITCHING_H +
  15.23820204 * moneyball$TEAM_PITCHING_BB -
  10.31248690 * moneyball$TEAM_PITCHING_SO -
  26.59985393 * moneyball$TEAM_FIELDING_E -
  0.10039003 * moneyball$TEAM_FIELDING_DP -
  21.88531997 * moneyball$TEAM_BASERUN_SB_RATIO +
  0.03424967 * moneyball$TEAM_BATTING_WALK +
  7.74191090 * moneyball$TEAM_BATTING_SO_IMP +
  33.48713640 * moneyball$TEAM_BASERUN_SB_IMP +
  5.31395778 * moneyball$TEAM_BASERUN_CS_IMP +
  5.66188510 * moneyball$TEAM_BATTING_HBP_IMP

### Subset output
prediction <- moneyball[c("INDEX","P_TARGET_WINS")]

### Prediction output 
write.xlsx(prediction, file = "Brandi Beals Unit1 Predictions.xlsx", sheetName = "Predictions", col.names = TRUE)
