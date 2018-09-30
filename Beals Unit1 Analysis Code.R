##################################################
### Unit 1 Assignment – Moneyball
### MSDS 411-DL-56
### Brandi Beals

##################################################
### Install packages
library(corrplot)

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

Singles <- moneyball$TEAM_BATTING_H-moneyball$TEAM_BATTING_2B-moneyball$TEAM_BATTING_3B-moneyball$TEAM_BATTING_HR

par(mfrow=c(2,2), mai=c(0.5,0.5,0.5,0.2))
par(fig=c(0,0.5,0.25,1))
hist(moneyball$TEAM_BATTING_H, main="Total Hits", breaks=30, col="seagreen")
par(fig=c(0.5,1,0.25,1), new=TRUE)
hist(Singles, main="Singles", breaks=30, col="seagreen")
par(fig=c(0,0.5,0,0.3), new=TRUE)
boxplot(moneyball$TEAM_BATTING_H, horizontal=TRUE, width=1, col="seagreen")
par(fig=c(0.5,1,0,0.3), new=TRUE)
boxplot(Singles, horizontal=TRUE, width=1, col="seagreen")
par(mfrow=c(1,1))

### Correlation matrix
corrplot(cor(moneyball[3:17]))

### 

##################################################
### Preparation and transformations

### Imputation

### 

##################################################
### Model creation

##################################################
### Model selection

##################################################
### 

##################################################
### 

##################################################
### 