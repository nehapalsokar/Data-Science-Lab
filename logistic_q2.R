new <- read.csv("MentalHealthCleanedDataset.csv", header = TRUE, stringsAsFactors = TRUE)

new<-subset(new, new$discuss.supervisor=='Maybe'| new$discuss.supervisor=='Yes' | new$discuss.supervisor=='No' )
new$discuss.supervisor <-factor(new$discuss.supervisor)
levels(new$discuss.supervisor)


#logistic regression
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)

new$mental.health.comfort.supervisor2 <- relevel(new$discuss.supervisor, ref = "Yes")
test <- multinom(mental.health.comfort.supervisor2 ~ mental.health.coverage + mental.health.options, data = new)
summary(test)

#levels(data$mental.health.comfort.supervisor2)


#1. Model execution output shows some iteration history and includes the final negative log-likelihood 1231.900910.This value is multiplied by two as shown in the model summary as the Residual Deviance. 

z <- summary(test)$coefficients/summary(test)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1))*2
p
  
exp(coef(test))
