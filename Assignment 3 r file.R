#Assignment 3-NehaPalsokar
#3
pnorm(2.0880281587410409562002335146688)


2*(1-pnorm(2.0880281587410409562002335146688))#0.03679529

pnorm(2.088,lower.tail=FALSE)+pnorm(-2.088,lower.tail=TRUE)#0.03679529
#4

install.packages("UsingR")
install.packages("dplyr")
library(UsingR)
height <- get("father.son")
height

#4a
plot(father.son$sheight)
hist(father.son$sheight)
cor(father.son)
hist(father.son$fheight)

#4b
lm(formula = sheight ~ fheight, data = father.son)
#Sons Height = 33.8866 + 0.5141 x Father's Height

#4c
mod<-lm(formula = sheight ~ fheight, data = father.son)
summary(mod)
confint(mod,level=0.95)

#4d
plot(father.son$fheight,father.son$sheight)
mod<-lm(formula = sheight ~ fheight, data = father.son)
abline(mod)

#4e
resid(mod)
plot(resid(mod))
hist(resid(mod))
names(father.son)
names(resid(mod))

ggplot(mod, aes(x=residuals(mod))) + geom_density() + labs(x='Residuals',y='Density',title='Density Plot for Residuals')

#4f
mod1<-plot(father.son)
new.df <- data.frame(fheight=c(50,55,70,75,90))
predict(mod,newdata=new.df)

#5
install.packages("openintro")
library(openintro)
data(gifted)
names(gifted)

#5a
#score and father's IQ
mod2 <- lm(score ~ fatheriq, data = gifted)
summary(mod2)
plot(mod2)


#score and mother's IQ
mod3 <- lm(score ~ motheriq, data = gifted)
summary(mod3)


#5b
confint(mod2,level=0.95)
confint(mod3,level=0.95)

View(gifted)
mean(gifted$motheriq)
mean(gifted$fatheriq)
mean(gifted$score)
