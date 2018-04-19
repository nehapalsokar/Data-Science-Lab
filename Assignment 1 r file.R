#Load the Felix Hernandez dataset in R
Felix<-read.csv("FelixHernandez2015.csv")
head(Felix)

#Number of wins this year=18

#Method1
sum(Felix[,"W"] == 1)
#Method2
table<-table(Felix[,"W"])

table

#mean of number of strikeouts-6.16129
mean(Felix[,"SO"])

#median of number of strikeouts-6
median(Felix[,"SO"])

#mode of number of strikeouts-5
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Mode(Felix[,"SO"])



#Innings pitched and strikeouts
plot(Felix[,"IP"], Felix[,"SO"], col="red")

#Innings pitched and walks
plot(Felix[,"IP"], Felix[,"BB"], col="blue")


#Correlation coefficients:  
cor(Felix[,"IP"], Felix[,"SO"])

cor(Felix[,"IP"], Felix[,"BB"])


#mean and variance of walks by month  
by(Felix[,"BB"], Felix[,"Month"], mean)

plot(by(Felix[,"BB"], Felix[,"Month"], mean), type="l")

by(Felix[,"BB"], Felix[,"Month"], var)

plot(by(Felix[,"BB"], Felix[,"Month"], var))


#Felix wins more at home
table1<-table(Felix[,"away"],Felix[,"W"])

table1 
  
#Load Randy Johnson data
Randy<-read.csv("RandyJohnson1995.csv")  
#mean for Randy Johnson
mean(Randy[,"SO"])
by(Randy[,"SO"], Randy[,"Month"], mean)

#mean for Felix
by(Felix[,"SO"], Felix[,"Month"], mean)
plot(by(Felix[,"SO"], Felix[,"Month"], mean))

#change limits for Randy to match Felix--compare both means:
plot(by(Randy[,"SO"], Randy[,"Month"], mean), ylim=c(5,13),col='blue',type="l",xlab="Mean of StrikeOuts", ylab="Month")

points(by(Felix[,"SO"], Felix[,"Month"], mean), col='red',type="l")


#standard normal distribution curve

curve(dnorm, from = -5, to=5)
abline(v=0.7142, col="blue")
abline(v=0.5215, col="red")
text(0.7142857+1, 0.3, "Verbal: 0.71",col="blue") 
text(0.5215124-1.5, 0.1, "Quantitative: 0.52", col="red") 

#percentile scores 
pnorm(0.714) 
1-pnorm(0.714) 

pnorm(0.5215)
1-pnorm(0.5215)


  