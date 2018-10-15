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
library(usdm)

##################################################
### Set working directory & read data
setwd("C:/Users/Brara/Dropbox/Masters in Predictive Analytics/411-DL-56/W-unit 1 Weeks 1 to 3/Unit 1 - Moneyball/4 Homework")
moneyball=read.csv("moneyball.csv",header=T)

##################################################
### Exploratory data analysis
str(moneyball)
summary(moneyball)

hist(moneyball$TARGET_WINS, main="Likely Values of Target Wins", xlab="", breaks=30) # know the range of likely values
par(mar=c(5, 10, 4, 2) + 0.1)
boxplot(moneyball[3:17], horizontal=TRUE,las=2) # get a visual sense for values of predictor variables
plot(moneyball[3:17]) # check for outliers and correlations across variables

### Create new fields
moneyball$TEAM_BATTING_1B <- moneyball$TEAM_BATTING_H-moneyball$TEAM_BATTING_2B-moneyball$TEAM_BATTING_3B-moneyball$TEAM_BATTING_HR
moneyball$TEAM_BASERUN_SB_RATIO <- moneyball$TEAM_BASERUN_SB/(moneyball$TEAM_BASERUN_SB+moneyball$TEAM_BASERUN_CS)
moneyball$TEAM_BASERUN_CS_RATIO <- moneyball$TEAM_BASERUN_CS/(moneyball$TEAM_BASERUN_SB+moneyball$TEAM_BASERUN_CS)
moneyball$TEAM_BATTING_WALK <- moneyball$TEAM_BATTING_BB+moneyball$TEAM_BATTING_HBP

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

par(mfrow=c(2,2), mai=c(0.5,0.5,0.5,0.2))
par(fig=c(0,0.5,0.25,1))
hist(moneyball$TEAM_BASERUN_CS_RATIO, main="Caught Stealing Ratio", breaks=30, col="firebrick")
par(fig=c(0.5,1,0.25,1), new=TRUE)
hist(moneyball$TEAM_BASERUN_SB_RATIO, main="Stolen Bases Ratio", breaks=30, col="seagreen")
par(fig=c(0,0.5,0,0.3), new=TRUE)
boxplot(moneyball$TEAM_BASERUN_CS_RATIO, horizontal=TRUE, width=1, col="firebrick")
par(fig=c(0.5,1,0,0.3), new=TRUE)
boxplot(moneyball$TEAM_BASERUN_SB_RATIO, horizontal=TRUE, width=1, col="seagreen")
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
corrplot(cor(moneyball[2:21]), method="color", type="upper", tl.col="black", tl.cex=.7, 
         addCoef.col="black", number.cex=.8)
### NAs removed
corrplot(cor(moneyball[2:21], use="complete.obs"), method="color", type="upper", tl.col="black", tl.cex=.7, 
         addCoef.col="black", number.cex=.8)

chart.Correlation(moneyball[2:21])

##################################################
### Preparation and transformations
### Review NA Values by Rows
NAobservations <- rowSums(is.na(moneyball[3:21])) # count columns with NA
hist(NAobservations/19, right=FALSE) # is percent over 0.5?

### Review NA Values by Columns
sapply(lapply(moneyball, is.na), sum)/nrow(moneyball)*100 # calculate percent of NA in a column
sapply(lapply(moneyball, is.na), sum)/nrow(moneyball)*100 > 5 # is it over 5%?

### Breakdown of missing values
md.pattern(moneyball) # https://www.r-bloggers.com/imputing-missing-data-with-r-mice-package/

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

corrplot(cor(moneyballmean[2:21]), method="color", type="upper", tl.col="black", tl.cex=.7, 
         addCoef.col="black", number.cex=.8)

### Impute with median
moneyballmedian <- moneyball

moneyballmedian$TEAM_BATTING_SO[is.na(moneyballmedian$TEAM_BATTING_SO)==TRUE] <- median(moneyballmedian$TEAM_BATTING_SO, na.rm = TRUE)
moneyballmedian$TEAM_BASERUN_SB[is.na(moneyballmedian$TEAM_BASERUN_SB)==TRUE] <- median(moneyballmedian$TEAM_BASERUN_SB, na.rm = TRUE)
moneyballmedian$TEAM_BASERUN_CS[is.na(moneyballmedian$TEAM_BASERUN_CS)==TRUE] <- median(moneyballmedian$TEAM_BASERUN_CS, na.rm = TRUE)
moneyballmedian$TEAM_BATTING_HBP[is.na(moneyballmedian$TEAM_BATTING_HBP)==TRUE] <- median(moneyballmedian$TEAM_BATTING_HBP, na.rm = TRUE)
moneyballmedian$TEAM_PITCHING_SO[is.na(moneyballmedian$TEAM_PITCHING_SO)==TRUE] <- median(moneyballmedian$TEAM_PITCHING_SO, na.rm = TRUE)
moneyballmedian$TEAM_FIELDING_DP[is.na(moneyballmedian$TEAM_FIELDING_DP)==TRUE] <- median(moneyballmedian$TEAM_FIELDING_DP, na.rm = TRUE)
moneyballmedian$TEAM_BASERUN_SB_RATIO[is.na(moneyballmedian$TEAM_BASERUN_SB_RATIO)==TRUE] <- median(moneyballmedian$TEAM_BASERUN_SB_RATIO, na.rm = TRUE)
moneyballmedian$TEAM_BASERUN_CS_RATIO[is.na(moneyballmedian$TEAM_BASERUN_CS_RATIO)==TRUE] <- median(moneyballmedian$TEAM_BASERUN_CS_RATIO, na.rm = TRUE)
moneyballmedian$TEAM_BATTING_WALK[is.na(moneyballmedian$TEAM_BATTING_WALK)==TRUE] <- median(moneyballmedian$TEAM_BATTING_WALK, na.rm = TRUE)

corrplot(cor(moneyballmedian[2:21]), method="color", type="upper", tl.col="black", tl.cex=.7, 
         addCoef.col="black", number.cex=.8)

### Impute with MICE
miceimputationstemp <- mice(moneyball, m=5, maxit=50, method="pmm", seed=500)
summary(miceimputationstemp)
moneyballmice <- complete(miceimputationstemp,5)
summary(moneyballmice)
corrplot(cor(moneyballmice[2:21]), method="color", type="upper", tl.col="black", tl.cex=.7, 
         addCoef.col="black", number.cex=.8)
densityplot(miceimputationstemp)
stripplot(miceimputationstemp)

### Transformation of Predictor Variables
### Standardization (X-mean)/standard deviation
moneyballstandard <- moneyballmean
mean(moneyballmean$TEAM_FIELDING_E)
sd(moneyballmean$TEAM_FIELDING_E)
moneyballstandard$TEAM_FIELDING_E <- (moneyballstandard$TEAM_FIELDING_E-246.4807)/227.771
mean(moneyballmean$TEAM_PITCHING_H)
sd(moneyballmean$TEAM_PITCHING_H)
moneyballstandard$TEAM_PITCHING_H <- (moneyballstandard$TEAM_PITCHING_H-1779.21)/1406.843
mean(moneyballmean$TEAM_PITCHING_SO)
sd(moneyballmean$TEAM_PITCHING_SO)
moneyballstandard$TEAM_PITCHING_SO <- (moneyballstandard$TEAM_PITCHING_SO-817.7305)/540.544
mean(moneyballmean$TEAM_PITCHING_BB)
sd(moneyballmean$TEAM_PITCHING_BB)
moneyballstandard$TEAM_PITCHING_BB <- (moneyballstandard$TEAM_PITCHING_BB-553.0079)/166.3574

### Log transformation
moneyballlog <- moneyballmean
moneyballlog$TEAM_FIELDING_E <- log(moneyballlog$TEAM_FIELDING_E)
moneyballlog$TEAM_PITCHING_H <- log(moneyballlog$TEAM_PITCHING_H)
moneyballlog$TEAM_PITCHING_SO[moneyballlog$TEAM_PITCHING_SO==0] <- 1
moneyballlog$TEAM_PITCHING_SO <- log(moneyballlog$TEAM_PITCHING_SO)
moneyballlog$TEAM_PITCHING_BB[moneyballlog$TEAM_PITCHING_BB==0] <- 1
moneyballlog$TEAM_PITCHING_BB <- log(moneyballlog$TEAM_PITCHING_BB)

### Determine trimming percentile values
quantile(moneyballmean$TEAM_FIELDING_E, probs=c(0.01,0.05,0.95,0.99))
quantile(moneyballmean$TEAM_PITCHING_H, probs=c(0.01,0.05,0.95,0.99))
quantile(moneyballmean$TEAM_PITCHING_SO, probs=c(0.01,0.05,0.95,0.99))
quantile(moneyballmean$TEAM_PITCHING_BB, probs=c(0.01,0.05,0.95,0.99))

### Trim data by 5th & 95th percentile
moneyball95trim <- moneyballmean
moneyball95trim$TEAM_FIELDING_E[moneyball95trim$TEAM_FIELDING_E>716] <- 716
moneyball95trim$TEAM_PITCHING_H[moneyball95trim$TEAM_PITCHING_H>2563] <- 2563
moneyball95trim$TEAM_PITCHING_SO[moneyball95trim$TEAM_PITCHING_SO>1168.25] <- 1168.25
moneyball95trim$TEAM_PITCHING_BB[moneyball95trim$TEAM_PITCHING_BB>757] <- 757

### Trim data by 1st & 99th percentile
moneyball99trim <- moneyballmean
moneyball99trim$TEAM_FIELDING_E[moneyball95trim$TEAM_FIELDING_E>1228] <- 1228
moneyball99trim$TEAM_PITCHING_H[moneyball95trim$TEAM_PITCHING_H>7054] <- 7054
moneyball99trim$TEAM_PITCHING_SO[moneyball95trim$TEAM_PITCHING_SO>1461.75] <- 1461.75
moneyball99trim$TEAM_PITCHING_BB[moneyball95trim$TEAM_PITCHING_BB>921] <- 921

### Visualize
par(mfrow=c(2,2))
hist(moneyballstandard$TEAM_FIELDING_E, breaks=30)
hist(moneyballlog$TEAM_FIELDING_E, breaks=30)
hist(moneyball95trim$TEAM_FIELDING_E, breaks=30)
hist(moneyball99trim$TEAM_FIELDING_E, breaks=30)
par(mfrow=c(1,1)) # log or 95th percentile seems best

par(mfrow=c(2,2))
hist(moneyballstandard$TEAM_PITCHING_H, breaks=30)
hist(moneyballlog$TEAM_PITCHING_H, breaks=30)
hist(moneyball95trim$TEAM_PITCHING_H, breaks=30)
hist(moneyball99trim$TEAM_PITCHING_H, breaks=30)
par(mfrow=c(1,1)) # 95th percentile trim seems best

par(mfrow=c(2,2))
hist(moneyballstandard$TEAM_PITCHING_SO, breaks=30)
hist(moneyballlog$TEAM_PITCHING_SO, breaks=30)
hist(moneyball95trim$TEAM_PITCHING_SO, breaks=30)
hist(moneyball99trim$TEAM_PITCHING_SO, breaks=30)
par(mfrow=c(1,1)) # log or 95th percentile seems best

par(mfrow=c(2,2))
hist(moneyballstandard$TEAM_PITCHING_BB, breaks=30)
hist(moneyballlog$TEAM_PITCHING_BB, breaks=30)
hist(moneyball95trim$TEAM_PITCHING_BB, breaks=30)
hist(moneyball99trim$TEAM_PITCHING_BB, breaks=30)
par(mfrow=c(1,1)) # log or 95th percentile seems best

chart.Correlation(moneyball95trim[2:21])

##################################################
### Model creation

### Multicollinearity checks
vifstep(moneyball, th=10)
detach("package:usdm",unload=TRUE)

### Model 1
fullmodel <- lm(TARGET_WINS ~ ., data=moneyball)
summary(fullmodel)
fullmodel <- lm(TARGET_WINS ~ ., data=moneyball[c(2,4,5,6,8,10,11,14,16,17)])
summary(fullmodel)
vif(fullmodel)

par(mfrow=c(2,2))
plot(fullmodel)
par(mfrow=c(1,1))

### Model 1
meanmodel <- lm(TARGET_WINS ~ ., data=moneyballmean)
summary(meanmodel)
meanmodel <- lm(TARGET_WINS ~ ., data=moneyballmean[c(2,4,5,6,8,10,11,14,16,17)])
summary(meanmodel)
vif(meanmodel)

par(mfrow=c(2,2))
plot(meanmodel)
par(mfrow=c(1,1))

### Model 2
medianmodel <- lm(TARGET_WINS ~ ., data=moneyballmedian)
summary(medianmodel)
vif(medianmodel)

par(mfrow=c(2,2))
plot(medianmodel)
par(mfrow=c(1,1))

### Model 3
micemodel <- lm(TARGET_WINS ~ ., data=moneyballmice)
summary(micemodel)
vif(micemodel)

par(mfrow=c(2,2))
plot(micemodel)
par(mfrow=c(1,1))

### Model 4
standardmodel <- lm(TARGET_WINS ~ ., data=moneyballstandard)
summary(standardmodel)
vif(standardmodel)

par(mfrow=c(2,2))
plot(standardmodel)
par(mfrow=c(1,1))

### Model 5
logmodel <- lm(TARGET_WINS ~ ., data=moneyballlog)
summary(logmodel)
vif(logmodel)

par(mfrow=c(2,2))
plot(logmodel)
par(mfrow=c(1,1))

### Model 6
trim95model <- lm(TARGET_WINS ~ ., data=moneyball95trim)
summary(trim95model)
vif(trim95model)

par(mfrow=c(2,2))
plot(trim95model)
par(mfrow=c(1,1))

### Model 7
trim99model <- lm(TARGET_WINS ~ ., data=moneyball99trim)
summary(trim99model)
vif(trim99model)

par(mfrow=c(2,2))
plot(trim99model)
par(mfrow=c(1,1))

### Forward Selection

### Backward Selection

### Stepwise
stepwise <- stepAIC(stepwisemodel, direction = "both")
summary(stepwise)
vif(stepwise)

### Outliers and Leverage Points

##################################################
### Model selection

### AIC, BIC, MAE, MAPE
AIC(stepwisemodel)
AIC(subset)
AIC(model3)
mse(stepwisemodel)
mse(subset)
mse(model3)

### Cross Validation (confidence intervals, predition intervals)

