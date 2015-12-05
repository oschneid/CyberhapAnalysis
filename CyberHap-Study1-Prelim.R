# Analysis for CyberHap Study 1 (run by GM, Nov-Dec 2015)
# Author: Oliver Schneider oschneid@cs.ubc.ca

require(ggplot2)
require(Exact)
require(MASS)
require(car)


study1.data <- read.csv("CyberhapData-Oliver.csv")

#Data cleanup - set types to discrete factors
study1.data <- study1.data[study1.data$Training==FALSE,]
study1.data$Spring.Pair <- factor(study1.data$Spring.Pair)
study1.data$PID <- factor(study1.data$PID)
study1.data$Condition.Number <- factor(study1.data$Condition.Number)





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
  Score..0.1.~Condition*Spring.Pair*PID, 
  #Score..0.1.~Condition+Spring.Pair+1, 
  #Score..0.1.~Spring.Pair, 
  data = study1.data,
  family=binomial())
Anova(study1.model, type=3)
summary(study1.model)
drop1(study1.model)
#cdplot(Score..0.1.~Condition, data=study1.data)
exp(cbind(coef(study1.model), confint(study1.model)))  


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

#NEXT UP: Use GGPLOT to produce box plots in each facet

#
#
#
# Difficulty
#
#
#

study1.difficulty.model <- lm(Difficulty..0.19.~Spring.Pair+Condition+Condition.Number+PID, data=study1.data)
anova(study1.difficulty.model)
Anova(study1.difficulty.model, type=3)

confint(study1.difficulty.model)

#Following two graphs indicate that P1, P5, and P6 found different spring pair have an effect on difficulty
#Other participants don't
qplot(Difficulty..0.19.,  data=study1.data, geom="density",
      facets=PID~.,
      color=Spring.Pair)
qplot(Difficulty..0.19.,  data=study1.data, geom="density",
      facets=PID~.,
      color=Condition.Number)

qplot(Difficulty..0.19.,  data=study1.data, geom="density",
      facets=PID~Condition,
      color=Spring.Pair)
#PID*SpringPair is significant for difficulty

study1.difficulty.model.interaction <- lm(Difficulty..0.19.-9.5~PID*Spring.Pair*Condition, data=study1.data)
anova(study1.difficulty.model.interaction)
Anova(study1.difficulty.model.interaction, type=3)
confint(study1.difficulty.model.interaction)

