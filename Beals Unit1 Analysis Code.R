##################################################
### Unit 1 Assignment – Moneyball
### MSDS 411-DL-56
### Brandi Beals

##################################################
### Install packages
library(corrplot)
library(PerformanceAnalytics)
library(mice)

##################################################
### Set working directory & read data
setwd("C:/Users/Brara/Dropbox/Masters in Predictive Analytics/411-DL-56/W-unit 1 Weeks 1 to 3/Unit 1 - Moneyball/4 Homework")
moneyball=read.csv("moneyball.csv",header=T)

##################################################
### Exploratory data analysis
str(moneyball)
summary(moneyball)

hist(moneyball$TARGET_WINS, main="Likely Values of Target Wins", breaks=30) # know the range of likely values
par(mar=c(5, 10, 4, 2) + 0.1)
boxplot(moneyball[3:17], horizontal=TRUE,las=2) # get a visual sense for values of predictor variables
plot(moneyball[3:17])

### Create new fields
moneyball$TEAM_BATTING_1B <- moneyball$TEAM_BATTING_H-moneyball$TEAM_BATTING_2B-moneyball$TEAM_BATTING_3B-moneyball$TEAM_BATTING_HR
moneyball$TEAM_BASERUN_SB_RATIO <- moneyball$TEAM_BASERUN_SB/(moneyball$TEAM_BASERUN_SB+moneyball$TEAM_BASERUN_CS)
moneyball$TEAM_BASERUN_CS_RATIO <- moneyball$TEAM_BASERUN_CS/(moneyball$TEAM_BASERUN_SB+moneyball$TEAM_BASERUN_CS)

### Defense
### Fielding
par(mfrow=c(2,2), mai=c(0.5,0.5,0.5,0.2))
par(fig=c(0,0.5,0.25,1))
hist(moneyball$TEAM_FIELDING_E, main="Fielding Errors", breaks=30, col="firebrick")
par(fig=c(0.5,1,0.25,1), new=TRUE)
hist(moneyball$TEAM_FIELDING_DP, main="Double Plays", breaks=30, col="seagreen")
par(fig=c(0,0.5,0,0.3), new=TRUE)
boxplot(moneyball$TEAM_FIELDING_E, horizontal=TRUE, width=1, col="firebrick")
par(fig=c(0.5,1,0,0.3), new=TRUE)
boxplot(moneyball$TEAM_FIELDING_DP, horizontal=TRUE, width=1, col="seagreen")
par(mfrow=c(1,1))

### Pitching
par(mfrow=c(2,2), mai=c(0.5,0.5,0.5,0.2))
par(fig=c(0,0.5,0.25,1))
hist(moneyball$TEAM_PITCHING_H, main="Hits Allowed", breaks=30, col="firebrick")
par(fig=c(0.5,1,0.25,1), new=TRUE)
hist(moneyball$TEAM_PITCHING_SO, main="Strike Outs", breaks=30, col="seagreen")
par(fig=c(0,0.5,0,0.3), new=TRUE)
boxplot(moneyball$TEAM_PITCHING_H, horizontal=TRUE, width=1, col="firebrick")
par(fig=c(0.5,1,0,0.3), new=TRUE)
boxplot(moneyball$TEAM_PITCHING_SO, horizontal=TRUE, width=1, col="seagreen")
par(mfrow=c(1,1))

par(mfrow=c(2,2), mai=c(0.5,0.5,0.5,0.2))
par(fig=c(0,0.5,0.25,1))
hist(moneyball$TEAM_PITCHING_BB, main="Walks Allowed", breaks=30, col="firebrick")
par(fig=c(0.5,1,0.25,1), new=TRUE)
hist(moneyball$TEAM_PITCHING_HR, main="Homeruns Allowed", breaks=30, col="firebrick")
par(fig=c(0,0.5,0,0.3), new=TRUE)
boxplot(moneyball$TEAM_PITCHING_BB, horizontal=TRUE, width=1, col="firebrick")
par(fig=c(0.5,1,0,0.3), new=TRUE)
boxplot(moneyball$TEAM_PITCHING_HR, horizontal=TRUE, width=1, col="firebrick")
par(mfrow=c(1,1))

### Offense
### Baserunning
par(mfrow=c(2,2), mai=c(0.5,0.5,0.5,0.2))
par(fig=c(0,0.5,0.25,1))
hist(moneyball$TEAM_BASERUN_CS, main="Caught Stealing", breaks=30, col="firebrick")
par(fig=c(0.5,1,0.25,1), new=TRUE)
hist(moneyball$TEAM_BASERUN_SB, main="Stolen Bases", breaks=30, col="seagreen")
par(fig=c(0,0.5,0,0.3), new=TRUE)
boxplot(moneyball$TEAM_BASERUN_CS, horizontal=TRUE, width=1, col="firebrick")
par(fig=c(0.5,1,0,0.3), new=TRUE)
boxplot(moneyball$TEAM_BASERUN_SB, horizontal=TRUE, width=1, col="seagreen")
par(mfrow=c(1,1))

### Batting
par(mfrow=c(2,2), mai=c(0.5,0.5,0.5,0.2))
par(fig=c(0,0.5,0.25,1))
hist(moneyball$TEAM_BATTING_BB, main="Walks", breaks=30, col="seagreen")
par(fig=c(0.5,1,0.25,1), new=TRUE)
hist(moneyball$TEAM_BATTING_HBP, main="Hit by Pitch", breaks=30, col="seagreen")
par(fig=c(0,0.5,0,0.3), new=TRUE)
boxplot(moneyball$TEAM_BATTING_BB, horizontal=TRUE, width=1, col="seagreen")
par(fig=c(0.5,1,0,0.3), new=TRUE)
boxplot(moneyball$TEAM_BATTING_HBP, horizontal=TRUE, width=1, col="seagreen")
par(mfrow=c(1,1))

par(mfrow=c(2,2), mai=c(0.5,0.5,0.5,0.2))
par(fig=c(0,0.5,0.25,1))
hist(moneyball$TEAM_BATTING_SO, main="Stikeouts", breaks=30, col="firebrick")
par(fig=c(0.5,1,0.25,1), new=TRUE)
hist(moneyball$TEAM_BATTING_HR, main="Home Runs", breaks=30, col="seagreen")
par(fig=c(0,0.5,0,0.3), new=TRUE)
boxplot(moneyball$TEAM_BATTING_SO, horizontal=TRUE, width=1, col="firebrick")
par(fig=c(0.5,1,0,0.3), new=TRUE)
boxplot(moneyball$TEAM_BATTING_HR, horizontal=TRUE, width=1, col="seagreen")
par(mfrow=c(1,1))

par(mfrow=c(2,2), mai=c(0.5,0.5,0.5,0.2))
par(fig=c(0,0.5,0.25,1))
hist(moneyball$TEAM_BATTING_2B, main="Doubles", breaks=30, col="seagreen")
par(fig=c(0.5,1,0.25,1), new=TRUE)
hist(moneyball$TEAM_BATTING_3B, main="Triples", breaks=30, col="seagreen")
par(fig=c(0,0.5,0,0.3), new=TRUE)
boxplot(moneyball$TEAM_BATTING_2B, horizontal=TRUE, width=1, col="seagreen")
par(fig=c(0.5,1,0,0.3), new=TRUE)
boxplot(moneyball$TEAM_BATTING_3B, horizontal=TRUE, width=1, col="seagreen")
par(mfrow=c(1,1))

par(mfrow=c(2,2), mai=c(0.5,0.5,0.5,0.2))
par(fig=c(0,0.5,0.25,1))
hist(moneyball$TEAM_BATTING_H, main="Total Hits", breaks=30, col="seagreen")
par(fig=c(0.5,1,0.25,1), new=TRUE)
hist(moneyball$TEAM_BATTING_1B, main="Singles", breaks=30, col="seagreen")
par(fig=c(0,0.5,0,0.3), new=TRUE)
boxplot(moneyball$TEAM_BATTING_H, horizontal=TRUE, width=1, col="seagreen")
par(fig=c(0.5,1,0,0.3), new=TRUE)
boxplot(moneyball$TEAM_BATTING_1B, horizontal=TRUE, width=1, col="seagreen")
par(mfrow=c(1,1))

### Correlation matrix
### NAs cause issues
corrplot(cor(moneyball[2:17]), method="color", type="upper", tl.col="black", tl.cex=.7, 
         addCoef.col="black", number.cex=.8)
### NAs removed
corrplot(cor(moneyball[2:17], use="complete.obs"), method="color", type="upper", tl.col="black", tl.cex=.7, 
         addCoef.col="black", number.cex=.8)

chart.Correlation(moneyball[3:17])
chart.Correlation(moneyball[2:7])
chart.Correlation(moneyball[,c(2,8:12)])
chart.Correlation(moneyball[,c(2,13:17)])

##################################################
### Preparation and transformations
### Review NA Values by Rows
NAobservations <- rowSums(is.na(moneyball[3:17]))
hist(NAobservations, breaks=c(0,1,2,3,4,5), right=FALSE)

### Review NA Values by Columns
sapply(lapply(moneyball, is.na), sum)/nrow(moneyball)*100
sapply(lapply(moneyball, is.na), sum)/nrow(moneyball)*100 > 5
#************ Will definitely want to remove TEAM_BATTING_HBP from analysis

md.pattern(moneyball) # https://www.r-bloggers.com/imputing-missing-data-with-r-mice-package/

### Recoding NAs to zero (baseline)
moneyballzero <- moneyball
moneyballzero$TEAM_BATTING_SO[is.na(moneyballzero$TEAM_BATTING_SO)==TRUE] <- 0
moneyballzero$TEAM_BASERUN_SB[is.na(moneyballzero$TEAM_BASERUN_SB)==TRUE] <- 0
moneyballzero$TEAM_BASERUN_CS[is.na(moneyballzero$TEAM_BASERUN_CS)==TRUE] <- 0
moneyballzero$TEAM_BATTING_HBP[is.na(moneyballzero$TEAM_BATTING_HBP)==TRUE] <- 0
moneyballzero$TEAM_PITCHING_SO[is.na(moneyballzero$TEAM_PITCHING_SO)==TRUE] <- 0
moneyballzero$TEAM_FIELDING_DP[is.na(moneyballzero$TEAM_FIELDING_DP)==TRUE] <- 0
summary(moneyballzero)

### NAs set to zero
corrplot(cor(moneyballzero[2:17]), method="color", type="upper", tl.col="black", tl.cex=.7, 
         addCoef.col="black", number.cex=.8)

chart.Correlation(moneyballzero[3:11])
chart.Correlation(moneyball[3:11])
chart.Correlation(moneyballzero[12:17])
chart.Correlation(moneyball[12:17])

### Impute with mean
moneyballmean <- moneyball
moneyballmean$TEAM_BATTING_SO[is.na(moneyballmean$TEAM_BATTING_SO)==TRUE] <- mean(moneyballmean$TEAM_BATTING_SO, na.rm = TRUE)
moneyballmean$TEAM_BASERUN_SB[is.na(moneyballmean$TEAM_BASERUN_SB)==TRUE] <- mean(moneyballmean$TEAM_BASERUN_SB, na.rm = TRUE)
moneyballmean$TEAM_BASERUN_CS[is.na(moneyballmean$TEAM_BASERUN_CS)==TRUE] <- mean(moneyballmean$TEAM_BASERUN_CS, na.rm = TRUE)
moneyballmean$TEAM_BATTING_HBP[is.na(moneyballmean$TEAM_BATTING_HBP)==TRUE] <- mean(moneyballmean$TEAM_BATTING_HBP, na.rm = TRUE)
moneyballmean$TEAM_PITCHING_SO[is.na(moneyballmean$TEAM_PITCHING_SO)==TRUE] <- mean(moneyballmean$TEAM_PITCHING_SO, na.rm = TRUE)
moneyballmean$TEAM_FIELDING_DP[is.na(moneyballmean$TEAM_FIELDING_DP)==TRUE] <- mean(moneyballmean$TEAM_FIELDING_DP, na.rm = TRUE)
corrplot(cor(moneyballmean[2:17]), method="color", type="upper", tl.col="black", tl.cex=.7, 
         addCoef.col="black", number.cex=.8)

### Impute with median
moneyballmedian <- moneyball
moneyballmedian$TEAM_BATTING_SO[is.na(moneyballmedian$TEAM_BATTING_SO)==TRUE] <- median(moneyballmedian$TEAM_BATTING_SO, na.rm = TRUE)
moneyballmedian$TEAM_BASERUN_SB[is.na(moneyballmedian$TEAM_BASERUN_SB)==TRUE] <- median(moneyballmedian$TEAM_BASERUN_SB, na.rm = TRUE)
moneyballmedian$TEAM_BASERUN_CS[is.na(moneyballmedian$TEAM_BASERUN_CS)==TRUE] <- median(moneyballmedian$TEAM_BASERUN_CS, na.rm = TRUE)
moneyballmedian$TEAM_BATTING_HBP[is.na(moneyballmedian$TEAM_BATTING_HBP)==TRUE] <- median(moneyballmedian$TEAM_BATTING_HBP, na.rm = TRUE)
moneyballmedian$TEAM_PITCHING_SO[is.na(moneyballmedian$TEAM_PITCHING_SO)==TRUE] <- median(moneyballmedian$TEAM_PITCHING_SO, na.rm = TRUE)
moneyballmedian$TEAM_FIELDING_DP[is.na(moneyballmedian$TEAM_FIELDING_DP)==TRUE] <- median(moneyballmedian$TEAM_FIELDING_DP, na.rm = TRUE)
corrplot(cor(moneyballmedian[2:17]), method="color", type="upper", tl.col="black", tl.cex=.7, 
         addCoef.col="black", number.cex=.8)

### Impute with MICE
miceimputationstemp <- mice(moneyball, m=5, maxit=50, method="pmm", seed=500)
summary(miceimputationstemp)
imp_moneyball <- complete(miceimputationstemp,1)
summary(imp_moneyball)
corrplot(cor(imp_moneyball[2:17]), method="color", type="upper", tl.col="black", tl.cex=.7, 
         addCoef.col="black", number.cex=.8)
densityplot(miceimputationstemp)
stripplot(miceimputationstemp)

### Transformation of Predictor Variables


##################################################
### Model creation
fullmodel <- lm(TARGET_WINS ~ TEAM_BATTING_H+TEAM_BATTING_2B+TEAM_BATTING_3B+TEAM_BATTING_HR+
     TEAM_BATTING_BB+TEAM_BATTING_HBP+TEAM_BATTING_SO+TEAM_BASERUN_SB+TEAM_BASERUN_CS+
     TEAM_PITCHING_H+TEAM_PITCHING_HR+TEAM_PITCHING_BB+TEAM_PITCHING_SO+
     TEAM_FIELDING_E+TEAM_FIELDING_DP, data=moneyball)

fullmodelzero <- lm(TARGET_WINS ~ TEAM_BATTING_H+TEAM_BATTING_2B+TEAM_BATTING_3B+TEAM_BATTING_HR+
                  TEAM_BATTING_BB+TEAM_BATTING_HBP+TEAM_BATTING_SO+TEAM_BASERUN_SB+TEAM_BASERUN_CS+
                  TEAM_PITCHING_H+TEAM_PITCHING_HR+TEAM_PITCHING_BB+TEAM_PITCHING_SO+
                  TEAM_FIELDING_E+TEAM_FIELDING_DP, data=moneyballzero)

### Forward Selection

### Backward Selection

### Stepwise

### Goodness of Fit Tests

### Outliers and Leverage Points

##################################################
### Model selection

### AIC, BIC, MAE, MAPE

### Cross Validation (confidence intervals, predition intervals)

