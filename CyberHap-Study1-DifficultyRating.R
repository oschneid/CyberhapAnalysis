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


#
#
#
# Difficulty
#
#
#

#build linear model using fully crossed factors (PID/Spring.Pair/Condition)
study1.difficulty.model.interaction <- lm(Difficulty..0.19.~PID*Spring.Pair*Condition, data=study1.data)

#ANOVA assumptions (PASSED!)

#shapiro-wilk test not significant, so no need to assume non-normality
#W=0.99324 p=0.05638
shapiro.test(study1.difficulty.model.interaction$residuals)
qqnorm(study1.difficulty.model.interaction$residuals)
# Levene's test passes (F(83, 336)=0.6369, p = 0.9928)
leveneTest(study1.difficulty.model.interaction)

#ANOVA

#use Anova, not anova - uses type III SS so order of model doesn't matter
#anova(study1.difficulty.model.interaction)
Anova(study1.difficulty.model.interaction, type=3)


#PLOTS

p <- ggplot(study1.data, aes(x=Spring.Pair, y=Difficulty..0.19.))
p <- p + facet_grid(.~PID)
p <- p + geom_boxplot()
p <- p + labs(y="Difficulty Rating", title="Cyberhap Study 1 Difficulty Rating Box Plots by PID", x="Spring Pair");
p


#plot CIs using lsmeans for PID/Spring.Pair interaction
study1.difficulty.lsm.interaction <- summary(lsmeans(study1.difficulty.model.interaction, ~PID*Spring.Pair))
study1.difficulty.lsm.interaction$PID <- factor(study1.difficulty.lsm.interaction$PID)
study1.difficulty.lsm.interaction$Spring.Pair <- factor(study1.difficulty.lsm.interaction$Spring.Pair)

p <- ggplot(study1.difficulty.lsm.interaction, aes(y=lsmean, x=Spring.Pair))
p <- p + geom_pointrange(aes(ymin=lower.CL, ymax=upper.CL)); #geom_linerange geom_pointrange
p <- p + facet_grid(.~PID)
# p <- p + theme_bw();
p <- p + labs(y="Difficulty Rating", title="Cyberhap Study 1 Difficulty Rating 95% Confidence Intervals by PID", x="Spring Pair");
p
# p <- p + theme(legend.position="top");
# p <- p + scale_colour_manual(values=c("grey88", "darkgreen", "chartreuse3", "indianred2"), name="Equivalence Level");


