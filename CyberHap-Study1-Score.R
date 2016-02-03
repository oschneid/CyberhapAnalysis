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


#Logistic Regression analysis of score

study1.model <- glm(
  Score..0.1.~Condition*Spring.Pair,
  data = study1.data,
  family=binomial())
summary(study1.model)
#LS Means analysis
study1.score.lsm <- lsmeans(study1.model, ~Spring.Pair)
pairs(study1.score.lsm)
study1.score.lsm <- summary(study1.score.lsm);
#study1.score.lsm$lower.CL <- exp(study1.score.lsm$lower.CL)
study1.score.lsm$asymp.LCL <- exp(study1.score.lsm$asymp.LCL)
#study1.score.lsm$upper.CL <- exp(study1.score.lsm$upper.CL)
study1.score.lsm$asymp.UCL <- exp(study1.score.lsm$asymp.UCL)
study1.score.lsm$lsmean <- exp(study1.score.lsm$lsmean)
# study1.score.lsm$PID <- factor(study1.score.lsm$Condition)
study1.score.lsm$Spring.Pair <- factor(study1.score.lsm$Spring.Pair)

study1.score.lsm$lsm_percent <- study1.score.lsm$lsmean / (study1.score.lsm$lsmean+1) * 100
study1.score.lsm$asymp.LCL_percent <- study1.score.lsm$asymp.LCL / (study1.score.lsm$asymp.LCL+1) * 100
study1.score.lsm$asymp.UCL_percent <- study1.score.lsm$asymp.UCL / (study1.score.lsm$asymp.UCL+1) * 100

study1.score.lsm

p <- ggplot(study1.score.lsm, aes(y=lsm_percent, x=Spring.Pair))
p <- p + geom_pointrange(aes(ymin=asymp.LCL_percent, ymax=asymp.UCL_percent),  size=1.5); #geom_linerange geom_pointrange
p <- p + labs(y="Score (%)", title="Cyberhap Study 1 Scores 95% Confidence Intervals by Spring Pair", x="Spring Pair");
p <- p + expand_limits(y=c(60, 100))
p <- p + geom_text(aes(label=strtrim(lsm_percent, 5), y=lsm_percent, x=as.numeric(Spring.Pair)+0.2))
p <- p + geom_text(aes(label=strtrim(asymp.LCL_percent, 5), y=asymp.LCL_percent, x=as.numeric(Spring.Pair)+0.2))
p <- p + geom_text(aes(label=strtrim(asymp.UCL_percent, 5), y=asymp.UCL_percent, x=as.numeric(Spring.Pair)+0.2))
p <- p + geom_line(data=data.frame(x=c(1,2), y=c(99.5, 99.5), alpha=0.75, size=1), mapping=aes(x=x, y=y), alpha = 0.5) + annotate("text", x=1.5,y=99.5,label="*", size=12, alpha=0.75)
p

