# Analysis for CyberHap Study 1 (run by GM, Nov-Dec 2015)
# Author: Oliver Schneider oschneid@cs.ubc.ca

require(ggplot2)
require(Exact)
require(MASS)
require(car)
library(lsmeans)
library(lme4)
library(pbkrtest)


study1.data <- read.csv("CyberhapData-151216-GM.csv")

#Data cleanup - set types to discrete factors
study1.data <- study1.data[study1.data$Training==FALSE,]
study1.data$Spring.Pair <- factor(study1.data$Spring.Pair)
study1.data$PID <- factor(study1.data$PID)
study1.data$Condition.Number <- factor(study1.data$Condition.Number)
study1.data$Difficulty..0.19. <- as.numeric(study1.data$Difficulty..0.19.)
study1.data$Time.on.Task..sec. <- as.numeric(study1.data$Time.on.Task..sec.)





#
#
# CHI SQ/Fisher's Exact Test analysis of SCORE ratings
#
#
chisq.test(table(study1.data$Spring.Pair, study1.data$Score..0.1.))
fisher.test(table(study1.data$Spring.Pair, study1.data$Score..0.1.))
#looks like spring pair may not be independent on score

#chisq.test(table(study1.data$PID, study1.data$Score..0.1.))
fisher.test(table(study1.data$PID, study1.data$Score..0.1.))
#no reason to expect PID is connected to score

chisq.test(table(study1.data$Condition, study1.data$Score..0.1.))
fisher.test(table(study1.data$Condition, study1.data$Score..0.1.))
#Condition has no effect


#Logistic Regression analysis of score
study1.model <- glm(
  Score..0.1.~PID*Condition*Spring.Pair,
  #Score..0.1.~Condition+Spring.Pair+1, 
  #Score..0.1.~Spring.Pair, 
  data = study1.data,
  family=binomial())
Anova(study1.model, type=3)
summary(study1.model)
drop1(study1.model)
#cdplot(Score..0.1.~Condition, data=study1.data)
exp(cbind(coef(study1.model), confint(study1.model)))  

study1.model <- glm(
  Score..0.1.~Condition+Spring.Pair,
  data = study1.data,
  family=binomial())
#LS Means analysis
study1.score.lsm <- summary(lsmeans(study1.model, ~Spring.Pair));
#study1.score.lsm$lower.CL <- exp(study1.score.lsm$lower.CL)
study1.score.lsm$asymp.LCL <- exp(study1.score.lsm$asymp.LCL)
#study1.score.lsm$upper.CL <- exp(study1.score.lsm$upper.CL)
study1.score.lsm$asymp.UCL <- exp(study1.score.lsm$asymp.UCL)
study1.score.lsm$lsmean <- exp(study1.score.lsm$lsmean)
# study1.score.lsm$PID <- factor(study1.score.lsm$Condition)
study1.score.lsm$Spring.Pair <- factor(study1.score.lsm$Spring.Pair)

study1.score.lsm$lsm_percent <- study1.score.lsm$lsmean / (study1.score.lsm$lsmean+1)
study1.score.lsm$asymp.LCL_percent <- study1.score.lsm$asymp.LCL / (study1.score.lsm$asymp.LCL+1)
study1.score.lsm$asymp.UCL_percent <- study1.score.lsm$asymp.UCL / (study1.score.lsm$asymp.UCL+1)


p <- ggplot(study1.score.lsm, aes(y=lsm_percent, x=Spring.Pair))
p <- p + geom_pointrange(aes(ymin=asymp.LCL_percent, ymax=asymp.UCL_percent)); #geom_linerange geom_pointrange
p


#FROM statmethods.org
fit <- study1.model
summary(fit) # display results
confint(fit) # 95% CI for the coefficients
exp(coef(fit)) # exponentiated coefficients
exp(confint(fit)) # 95% CI for exponentiated coefficients
summary(predict(fit, type="response")) # predicted values
residuals(fit, type="deviance") # residuals




#
#
# Plotting Scores
#
#

attach(study1.data)
study1.data.aggregatescore <- aggregate(Score..0.1., by=list(PID, Condition, Condition.Number, Spring.Pair), FUN=mean, data=study1.data)
detach(study1.data)
names(study1.data.aggregatescore) <- c("PID", "Condition", "Condition.Number", "Spring.Pair", "Score")

qplot(PID,
      Score,
      data=study1.data.aggregatescore,
      facets=Spring.Pair~Condition,
      #color=Spring.Pair
      )

p <- ggplot(study1.data.aggregatescore, aes(x=Spring.Pair, y=Score))
p <- p + facet_grid(Condition~.)
p <- p + geom_boxplot()
p

p <- ggplot(study1.data.aggregatescore, aes(x=Spring.Pair, y=Score))
p <- p + facet_grid(Condition~.)
p <- p + geom_boxplot()
p

qplot(PID,
      Score,
      data=study1.data.aggregatescore,
      facets=Spring.Pair~Condition,
      #color=Spring.Pair
)

#NEXT UP: Use GGPLOT to produce box plots in each facet


#
# Check between dependent variables 
#

study1.score.time.mdl <- glm(Score..0.1.~Time.on.Task..sec., data=study1.data, family="binomial")
summary(study1.score.time.mdl)


study1.score.time.mdl <- glm(Score..0.1.~PID+Time.on.Task..sec.+Difficulty..0.19., data=study1.data, family=binomial)
summary(study1.score.time.mdl)


study1.score.time.mdl <- lm(Time.on.Task..sec.~Score..0.1.+Difficulty..0.19., data=study1.data)
summary(study1.score.time.mdl)


study1.score.time.mdl <- lm(Difficulty..0.19.~Score..0.1.+Time.on.Task..sec., data=study1.data)
summary(study1.score.time.mdl)

