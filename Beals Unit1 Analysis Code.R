##################################################
### Unit 1 Assignment – Moneyball
### MSDS 411-DL-56
### Brandi Beals

##################################################
### Install packages

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

### Baserunning

### Pitching

### Batting


##################################################
### Preparation and transformations

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