#Lab 1- Neha Palsokar
getwd()

#Load the seatbelts data
Seatbelts<-read.csv("seatbelts.csv")
head(Seatbelts)

#1. Data Cleaning 
#verify class is data frame
class(Seatbelts)

# Get some information about a data frame:
dim(Seatbelts)

# data frame attributes including column names
colnames((Seatbelts))

# how many cases are in the observed data?--> 192 cases
nrow(Seatbelts)

# what variables are observed for each month?--> year.month

# Get some information about the variables in the data:
summary(Seatbelts)


------------------
#2. Computing Averages
#mean deaths
mean(Seatbelts[,"DriversKilled"])

mean((Seatbelts[Seatbelts[,"year"]>=1969 & Seatbelts[,"year"]<1970,"DriversKilled"]))

#mean of driverskilled by year
by(Seatbelts[,"DriversKilled"], Seatbelts[,"year"], mean)

#Average number of fatalities in 1970?
mean((Seatbelts[Seatbelts[,"year"]==1970,"DriversKilled"]))

#Average number of fatalities in 1978?
mean((Seatbelts[Seatbelts[,"year"]==1978,"DriversKilled"]))

#What was the average number of rear seat fatalities in 1972?
mean((Seatbelts[Seatbelts[,"year"]==1972,"rear"]))

#What was the average number of rear seat fatalities in 1980?
mean((Seatbelts[Seatbelts[,"year"]==1980,"rear"]))

-------------
#3. Exploring Relationships I
plot(Seatbelts[,"kms"], Seatbelts[,"drivers"])

#Driverskilled and petrol price
plot(Seatbelts[,"PetrolPrice"], Seatbelts[,"DriversKilled"])

#Driverskilled and km
plot(Seatbelts[,"kms"], Seatbelts[,"DriversKilled"], col="red")
#Decreasing trend-Drivers killed reduces as kms increase

#Vankilled and year
plot(Seatbelts[,"year.month"], Seatbelts[,"VanKilled"], col="red", type="l")
#The number of deaths have decreased per year

#What hypotheses might you make after seeing these relationships?

------------
  
#4.Exploring Relationships II

#Examine the mean fatalities before and after the implementation of the law
plot(Seatbelts[,"law"], Seatbelts[,"DriversKilled"], col="red")
#Drivers killed after law was implemented were less, therefore fatalities were reduced


#response to the seatbelt law
plot(Seatbelts[,"year.month"], Seatbelts[,"DriversKilled"], col="red", type="l")
abline(v=1983, col="blue") #Line drawn for year in which seatbelt law was implemented
#After the seatbelt law was implemnted in 1983, we can see number of deaths have reduced.

--------
#5. Extra Credit Question
#A comparison of the front seat vs back seat passengers killed shows that number of 
#deaths of front seat passengers reduced after implementing seat belt law in 1983, whereas
#it did not reduce for back seat passangers as it was not mandatory to wear a belt while sitting in the rear. 

#Front seat passengers killed
plot(Seatbelts[,"year.month"], Seatbelts[,"front"], col="red", type="l")
abline(v=1983, col="blue")

#Rear seat passengers killed
plot(Seatbelts[,"year.month"], Seatbelts[,"rear"], col="red", type="l")
abline(v=1983, col="blue")

