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
study1.data$Time.on.Task..sec. <- as.numeric(study1.data$Time.on.Task..sec.)
study1.data$SpringIndex <- factor(study1.data$SpringIndex)

#
#
#
# Time on Task
#
#
#

#build linear model using fully crossed factors (PID/Spring.Pair/Condition)
study1.time.model.interaction <- lm(Time.on.Task..sec.~PID*Spring.Pair*Condition, data=study1.data)

#ANOVA assumptions (PASSED!)

#shapiro-wilk test is significant (W 0.97936, p=1.079e-05)
shapiro.test(study1.time.model.interaction$residuals)
#QQNorm plot isn't too bad
qqnorm(study1.time.model.interaction$residuals)
# Levene's test passes (F(83, 336)=1.0203, p = 0.4401)
leveneTest(study1.time.model.interaction)

#ANOVA

#use Anova, not anova - uses type III SS so order of model doesn't matter
#anova(study1.difficulty.model.interaction)
Anova(study1.time.model.interaction, type=3)


#PLOTS

p <- ggplot(study1.data, aes(x=PID, y=Time.on.Task..sec.))
#p <- p + facet_grid(PID~.)
p <- p + geom_boxplot()
p <- p + labs(y="Time on Task (s)", title="Cyberhap Study 1 Time Box Plots by PID", x="PID");
p


#plot CIs using lsmeans for PID/Spring.Pair interaction
study1.time.lsm.interaction <- lsmeans(study1.time.model.interaction, ~PID)
pairs(study1.time.lsm.interaction)
study1.time.lsm.interaction <- summary(study1.time.lsm.interaction)
study1.time.lsm.interaction$PID <- factor(study1.time.lsm.interaction$PID)

p <- ggplot(study1.time.lsm.interaction, aes(y=lsmean, x=PID))
p <- p + geom_pointrange(aes(ymin=lower.CL, ymax=upper.CL)); #geom_linerange geom_pointrange
# p <- p + facet_grid(.~PID)
# p <- p + theme_bw();
p <- p + labs(y="Time on Task (s)", title="Cyberhap Study 1 Time 95% Confidence Intervals by PID", x="PID");
p



summary(study1.data$Time.on.Task..sec.)
