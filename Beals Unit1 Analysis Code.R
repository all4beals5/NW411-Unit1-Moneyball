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
library(MASS)
library(stats4)

##################################################
### Set working directory & read data
# home setwd("C:/Users/Brara/Dropbox/Masters in Predictive Analytics/411-DL-56/W-unit 1 Weeks 1 to 3/Unit 1 - Moneyball/4 Homework")
setwd("C:/Users/bbeals/Dropbox (Personal)/Masters in Predictive Analytics/411-DL-56/W-unit 1 Weeks 1 to 3/Unit 1 - Moneyball/4 Homework")
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
moneyballmice <- complete(miceimputationstemp,1)
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
fullmodel <- lm(TARGET_WINS ~ ., data=moneyball[-c(11,21)])
summary(fullmodel)
fullmodel <- lm(TARGET_WINS ~ ., data=moneyball[c(2,4,5,6,8,10,11,14,16,17)])
summary(fullmodel)
vif(fullmodel)

par(mfrow=c(2,2))
plot(fullmodel)
par(mfrow=c(1,1))

### Model 2
meanmodel <- lm(TARGET_WINS ~ ., data=moneyballmean)
summary(meanmodel)
meanmodel <- lm(TARGET_WINS ~ ., data=moneyballmean[c(2,3,4,5,6,7,8,9,12,16,17,22,23,25,27)])
summary(meanmodel)
vif(meanmodel)

par(mfrow=c(2,2))
plot(meanmodel)
par(mfrow=c(1,1))

### Model 3
medianmodel <- lm(TARGET_WINS ~ ., data=moneyballmedian)
summary(medianmodel)
medianmodel <- lm(TARGET_WINS ~ ., data=moneyballmedian[c(2,3,4,5,6,7,8,9,10,11,14,16,17,22,23,25,27)])
summary(medianmodel)
vif(medianmodel)

par(mfrow=c(2,2))
plot(medianmodel)
par(mfrow=c(1,1))

### Model 4
micemodel <- lm(TARGET_WINS ~ ., data=moneyballmice)
summary(micemodel)
micemodel <- lm(TARGET_WINS ~ ., data=moneyballmice[c(2,3,4,5,6,10,12,16,17,25,27)])
summary(micemodel)
vif(micemodel)

par(mfrow=c(2,2))
plot(micemodel)
par(mfrow=c(1,1))

### Model 5
standardmodel <- lm(TARGET_WINS ~ ., data=moneyballstandard)
summary(standardmodel)
standardmodel <- lm(TARGET_WINS ~ ., data=moneyballstandard[c(2,3,4,5,6,7,8,9,12,16,17,21,22,23,25,27)])
summary(standardmodel)
vif(standardmodel)

par(mfrow=c(2,2))
plot(standardmodel)
par(mfrow=c(1,1))

### Model 6
logmodel <- lm(TARGET_WINS ~ ., data=moneyballlog)
summary(logmodel)
logmodel <- lm(TARGET_WINS ~ ., data=moneyballlog[c(2,3,4,5,6,7,8,9,16,17,19,21,22,23,25,27)])
summary(logmodel)
vif(logmodel)

par(mfrow=c(2,2))
plot(logmodel)
par(mfrow=c(1,1))

### Model 7
trim95model <- lm(TARGET_WINS ~ ., data=moneyball95trim)
summary(trim95model)
trim95model <- lm(TARGET_WINS ~ ., data=moneyball95trim[c(2,3,4,5,7,9,10,12,13,15,16,17,19,21,22,23,24,25,27)])
summary(trim95model)
vif(trim95model)

# detach("package:stats4",unload=TRUE)
# library(usdm)
# vifstep(moneyball95trim[c(2,3,4,5,7,8,9,10,12,13,15,16,17,19,21,22,23,24,25,27,28)], th=10)
# detach("package:usdm",unload=TRUE)

par(mfrow=c(2,2))
plot(trim95model)
par(mfrow=c(1,1))

### Model 8
trim99model <- lm(TARGET_WINS ~ ., data=moneyball99trim)
summary(trim99model)
trim99model <- lm(TARGET_WINS ~ ., data=moneyball99trim[c(2,3,4,5,6,7,8,9,12,16,17,21,22,23,25,27)])
summary(trim99model)
vif(trim99model)

par(mfrow=c(2,2))
plot(trim99model)
par(mfrow=c(1,1))

### Stepwise model selection
### Model 9
stepwiselogmodel <- lm(TARGET_WINS ~ ., data=moneyballlog)
stepwiselog <- stepAIC(stepwiselogmodel, direction = "both")
summary(stepwiselog)
vif(stepwiselog)

par(mfrow=c(2,2))
plot(stepwiselog)
par(mfrow=c(1,1))

### Model 10
stepwisestdmodel <- lm(TARGET_WINS ~ ., data=moneyballstandard)
stepwisestd <- stepAIC(stepwisestdmodel, direction = "both")
summary(stepwisestd)
vif(stepwisestd)

par(mfrow=c(2,2))
plot(stepwisestd)
par(mfrow=c(1,1))

### Model 11
stepwise95model <- lm(TARGET_WINS ~ ., data=moneyball95trim[-c(15)]) #remove pitching SO due to high VIF
stepwise95 <- stepAIC(stepwise95model, direction = "both")
summary(stepwise95)
vif(stepwise95)

par(mfrow=c(2,2))
plot(stepwise95)
par(mfrow=c(1,1))

### Model 12
stepwise99model <- lm(TARGET_WINS ~ ., data=moneyball99trim)
stepwise99 <- stepAIC(stepwise99model, direction = "both")
summary(stepwise99)
vif(stepwise99)

par(mfrow=c(2,2))
plot(stepwise99)
par(mfrow=c(1,1))

### R-Squared, AIC, BIC, MAE, MSE
modelrsquared <- c(summary(fullmodel)$r.squared,
                   summary(meanmodel)$r.squared,
                   summary(medianmodel)$r.squared,
                   summary(micemodel)$r.squared,
                   summary(standardmodel)$r.squared,
                   summary(logmodel)$r.squared,
                   summary(trim95model)$r.squared,
                   summary(trim99model)$r.squared,
                   summary(stepwiselog)$r.squared,
                   summary(stepwisestd)$r.squared,
                   summary(stepwise95)$r.squared,
                   summary(stepwise99)$r.squared
)
modelaic <- c(AIC(fullmodel),
              AIC(meanmodel),
              AIC(medianmodel),
              AIC(micemodel),
              AIC(standardmodel),
              AIC(logmodel),
              AIC(trim95model),
              AIC(trim99model),
              AIC(stepwiselog),
              AIC(stepwisestd),
              AIC(stepwise95),
              AIC(stepwise99)
)
modelbic <- c(BIC(fullmodel),
              BIC(meanmodel),
              BIC(medianmodel),
              BIC(micemodel),
              BIC(standardmodel),
              BIC(logmodel),
              BIC(trim95model),
              BIC(trim99model),
              BIC(stepwiselog),
              BIC(stepwisestd),
              BIC(stepwise95),
              BIC(stepwise99)
)
modelmae <- c(mean(abs(summary(fullmodel)$residuals)),
              mean(abs(summary(meanmodel)$residuals)),
              mean(abs(summary(medianmodel)$residuals)),
              mean(abs(summary(micemodel)$residuals)),
              mean(abs(summary(standardmodel)$residuals)),
              mean(abs(summary(logmodel)$residuals)),
              mean(abs(summary(trim95model)$residuals)),
              mean(abs(summary(trim99model)$residuals)),
              mean(abs(summary(stepwiselog)$residuals)),
              mean(abs(summary(stepwisestd)$residuals)),
              mean(abs(summary(stepwise95)$residuals)),
              mean(abs(summary(stepwise99)$residuals))
)
modelmse <- c(mean(summary(fullmodel)$residuals^2),
              mean(summary(meanmodel)$residuals^2),
              mean(summary(medianmodel)$residuals^2),
              mean(summary(micemodel)$residuals^2),
              mean(summary(standardmodel)$residuals^2),
              mean(summary(logmodel)$residuals^2),
              mean(summary(trim95model)$residuals^2),
              mean(summary(trim99model)$residuals^2),
              mean(summary(stepwiselog)$residuals^2),
              mean(summary(stepwisestd)$residuals^2),
              mean(summary(stepwise95)$residuals^2),
              mean(summary(stepwise99)$residuals^2)
)
modelmetrics <- data.frame(modelrsquared, modelaic, modelbic, modelmae, modelmse)
modelmetrics # models 1, 9, 6, and 11 are best

### Limit data
moneyball2 <- subset(moneyball, TARGET_WINS >= 21 & TARGET_WINS <= 120)
moneyball2 <- subset(moneyball2, TEAM_PITCHING_H < 2000)
moneyballlog2 <- subset(moneyballlog, TARGET_WINS >= 21 & TARGET_WINS <= 120)
moneyballlog2 <- subset(moneyballlog2, TEAM_PITCHING_H < 7.6)
moneyball99trim2 <- subset(moneyball99trim, TARGET_WINS >= 21 & TARGET_WINS <= 120)
moneyball99trim2 <- subset(moneyball99trim2, TEAM_PITCHING_H < 2000)
moneyball95trim2 <- subset(moneyball95trim, TARGET_WINS >= 21 & TARGET_WINS <= 120)
moneyball95trim2 <- subset(moneyball95trim2, TEAM_PITCHING_H < 2000)

### Rerun best models
### Model 1.2
fullmodel2 <- lm(TARGET_WINS ~ ., data=moneyball2)
summary(fullmodel2)
fullmodel2 <- lm(TARGET_WINS ~ ., data=moneyball2[-c(11,21)])
summary(fullmodel2)
fullmodel2 <- lm(TARGET_WINS ~ ., data=moneyball2[c(2,4,5,6,8,10,11,14,16,17)])
summary(fullmodel2)
vif(fullmodel2)

par(mfrow=c(2,2))
plot(fullmodel2)
par(mfrow=c(1,1))

### Model 6.2
logmodel2 <- lm(TARGET_WINS ~ ., data=moneyballlog2)
summary(logmodel2)
logmodel2 <- lm(TARGET_WINS ~ ., data=moneyballlog2[c(2,3,4,5,6,7,8,9,10,16,17,19,21,22,23,24,25,27)])
summary(logmodel2)
vif(logmodel2)

par(mfrow=c(2,2))
plot(logmodel2)
par(mfrow=c(1,1))

### Model 8.2
trim99model2 <- lm(TARGET_WINS ~ ., data=moneyball99trim2)
summary(trim99model2)
trim99model2 <- lm(TARGET_WINS ~ ., data=moneyball99trim2[c(2,4,5,9,10,12,13,14,15,16,17,21,22,23,25,27)])
summary(trim99model2)
vif(trim99model2)

par(mfrow=c(2,2))
plot(trim99model2)
par(mfrow=c(1,1))

### Model 9.2
stepwiselogmodel2 <- lm(TARGET_WINS ~ ., data=moneyballlog2[-c(7,8)]) #remove batting SO and BB due to high VIF
stepwiselog2 <- stepAIC(stepwiselogmodel2, direction = "both")
summary(stepwiselog2)
vif(stepwiselog2)

par(mfrow=c(2,2))
plot(stepwiselog2)
par(mfrow=c(1,1))

### Model 11.2
stepwise95model2 <- lm(TARGET_WINS ~ ., data=moneyball95trim2[-c(3,6,7,8,13)]) #remove batting HR and SO due to high VIF
stepwise952 <- stepAIC(stepwise95model2, direction = "both")
summary(stepwise952)
vif(stepwise952)

par(mfrow=c(2,2))
plot(stepwise952)
par(mfrow=c(1,1))

##################################################
### Model selection

### R-Squared, AIC, BIC, MAE, MSE
modelrsquared <- c(summary(fullmodel2)$r.squared,
                   summary(logmodel2)$r.squared,
                   summary(trim99model2)$r.squared,
                   summary(stepwiselog2)$r.squared,
                   summary(stepwise952)$r.squared
)
modelaic <- c(AIC(fullmodel2),
              AIC(logmodel2),
              AIC(trim99model2),
              AIC(stepwiselog2),
              AIC(stepwise952)
)
modelbic <- c(BIC(fullmodel2),
              BIC(logmodel2),
              BIC(trim99model2),
              BIC(stepwiselog2),
              BIC(stepwise952)
)
modelmae <- c(mean(abs(summary(fullmodel2)$residuals)),
              mean(abs(summary(logmodel2)$residuals)),
              mean(abs(summary(trim99model2)$residuals)),
              mean(abs(summary(stepwiselog2)$residuals)),
              mean(abs(summary(stepwise952)$residuals))
)
modelmse <- c(mean(summary(fullmodel2)$residuals^2),
              mean(summary(logmodel2)$residuals^2),
              mean(summary(trim99model2)$residuals^2),
              mean(summary(stepwiselog2)$residuals^2),
              mean(summary(stepwise952)$residuals^2)
)
modelmetrics <- data.frame(modelrsquared, modelaic, modelbic, modelmae, modelmse)
modelmetrics # models 1, 6, and 9 look good

### Outliers and Leverage Points
summary(influentialobs <- influence.measures(logmodel2))

plot(dffits(logmodel2))
dffitslog <- dffits(logmodel2)
moneyballfinal <- cbind(moneyballlog2,dffitslog)

moneyballfinal$absdf <- abs(moneyballfinal$dffitslog)
moneyballfinal <- moneyballfinal[which(moneyballfinal$absdf < 0.004244763),] #severely limits the nubmer of observations  
# 2*(sqrt(p+1)/(n-p-1)) = 2*(sqrt(18)/1999)

outlierscooks <- cooks.distance(logmodel2)
plot(outlierscooks)
abline(h = 12*mean(outlierscooks, na.rm=T), col="red") #typical cutoff is 4 times cooks distance

moneyballfinal <- cbind(moneyballlog2, outlierscooks)
moneyballfinal <- moneyballfinal[which(moneyballfinal$outlierscooks < 12*mean(outlierscooks)),]

### Final model
### Model 6.3
finalmodel <- lm(TARGET_WINS ~ ., data=moneyballfinal[c(2,3,4,5,6,7,8,9,10,16,17,19,21,22,23,24,25,27)])
summary(finalmodel)
vif(finalmodel)

par(mfrow=c(2,2))
plot(finalmodel)
par(mfrow=c(1,1))

### FaKE OUT - One More Model
### Outliers and Leverage Points
summary(influentialobs <- influence.measures(stepwiselog2))

plot(dffits(stepwiselog2))
dffitslog <- dffits(stepwiselog2)
moneyballfinal <- cbind(moneyballlog2,dffitslog)

moneyballfinal$absdf <- abs(moneyballfinal$dffitslog)
moneyballfinal <- moneyballfinal[which(moneyballfinal$absdf < 0.004244763),] #severely limits the nubmer of observations  
# 2*(sqrt(p+1)/(n-p-1)) = 2*(sqrt(18)/1999)

outlierscooks <- cooks.distance(stepwiselog2)
plot(outlierscooks)
abline(h = 12*mean(outlierscooks, na.rm=T), col="red") #typical cutoff is 4 times cooks distance

moneyballfinal <- cbind(moneyballlog2, outlierscooks)
moneyballfinal <- moneyballfinal[which(moneyballfinal$outlierscooks < 12*mean(outlierscooks)),]

### Final model
### Model 9.2
finalmodel2start <- lm(TARGET_WINS ~ ., data=moneyballlog2[-c(7,8)]) #remove batting SO and BB due to high VIF
finalmodel2 <- stepAIC(finalmodel2start, direction = "both")
summary(finalmodel2)
vif(finalmodel2)

par(mfrow=c(2,2))
plot(finalmodel2)
par(mfrow=c(1,1))