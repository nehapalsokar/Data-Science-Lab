#Lab 3- NehaPalsokar

#Load data
load("BostonData.Rdat")

#0
plot(boston)
#distance to work and NO concentration have a decreasing relationship, the presence of NO is reducing as you go away from the city
#Home value increases as distance to work increases

#1
mod1 <- lm(home.value ~ NO.concentration, data = boston)
summary(mod1)
#Home value reduces by -33.9 as one unit of NO increases

mod2 <- lm(home.value ~ distance.to.work, data = boston)
summary(mod2)
#Home value increases by 1.09 as distance to work increases

mod3 <- lm(home.value ~ student.teacher.ratio, data = boston)
summary(mod3)
#Home value decreases by -2.157 as student teacher ratio increases as maybe it is not a good school district,students are more and teachers are less

#2
#Higher adjusted R value indicates a better fit, as the student ratio has highest Adjusted R-squared we can say it explains the data the best

#3

mod.full <- lm(home.value ~ distance.to.work + NO.concentration +
                 student.teacher.ratio, data = boston)
summary(mod.full)
#After adjusting for other factors, we notice that as distance to work increases by a unit, the home value reduces by -1.28 , earlier the value was increasing when we did not factor in other things

#4 
#Since the Adjusted R-squared value is higher in multivariate model, it is a better fit for the data compared to single linear regression values

#5
predict(mod.full, newdata=data.frame("distance.to.work" = 3,
                                     "NO.concentration" = 0.35, "student.teacher.ratio" = 10),
        interval="prediction")
#Prediction takes into account the uncertainty and accomodates any new data values, it gives a median home value of 49.57499 and upper and lower bounds of 63.92261 and 35.22737
#It increases certainty in the confidence interval

#extra
plot(fitted(mod1),resid(mod1))
