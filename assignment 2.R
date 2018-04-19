install.packages("nycflights13")
library(nycflights13) # load library
data(flights)

View(flights)

#a) (2 pt) How many flights were there to and from NYC in 2013?
#To NYC: 1 flight
length(which(flights$dest=="EWR"|flights$dest=="JFK"|flights$dest=="LGA"))
#Departing from NYC:336776 flights
nrow(flights)

#b) (2 pt) How many flights were there from NYC airports to Seattle (SEA) in 2013?--3923
sum(flights[,"dest"] == "SEA")

#c) (2 pt) How many airlines fly from NYC to Seattle?--5 airlines

seafl<-subset(flights, flights$dest == "SEA") #subset flights from NYC to SEA
View(seafl)
unique(seafl$carrier)
length(unique(seafl$carrier))

#d) (2 pt) What is the average arrival delay for flights from NYC to Seattle? 

mean(seafl$arr_delay) #returns NA
mean(seafl$arr_delay, na.rm = TRUE) #returns -1.099099

#2.a) (4 pt) What is the mean arrival delay time? What is the median arrival delay time?
mean(flights$arr_delay, na.rm = TRUE)
median(flights$arr_delay, na.rm = TRUE)
#mean =6.895377 median = -5

#2.b) (2 pt) What does a negative arrival delay mean?
# it means the flight has arrived before time

#c) (4 pt) Plot a histogram of arrival delay times. Does the answers you obtained in (a) consistent with the shape of the delay time distribution?
hist(flights$arr_delay)
# yes the histogram is concentrated below zero, as it is a negative delay.

#d) (4 pt) Is there seasonality in departure delays? 
by(flights$dep_delay, flights$month, function(x) mean(x, na.rm=T))
plot(by(flights$dep_delay, flights$month, function(x) mean(x, na.rm=T)),type='l')
#Departure delay is the maximum in summer(months 6 and 7) and the least in months 9-11, as summer and christmas are peak vacation seasons

#3.EDA
#a) (4 pt) Plot a histogram of the total air flight time with 100 breaks.

hist(flights$air_time, breaks = 100)
#there are two peaks depending on flight distance, most flights travel short distances, below 200 min approximately

#b) (4 pt) What time of day do flights most commonly depart? Why might there be two most popular times of day to depart?
hist(flights$sched_dep_time, breaks = 24)
hist(flights$dep_time, breaks = 24)
#the popular times are 8 am and between 3-6pm, the flight rates at these hours may be relatively cheaper, also people travel for work in the mornings and maybe return in the afternoons

#c) (4 pt) Plot a box plot of departure delays and hour of departure. What pattern do you see? What is an explanation for this?

boxplot(dep_delay ~ hour, data = flights)

#4. (10 pt) Develop one research question you can address using the nycflights2013 dataset.

#Arrival Delays vs Departure Delays for flights from NYC in 2013
plot(flights$arr_delay,flights$dep_delay)

#	Arrival delay vs distance
plot(flights$distance, flights$arr_delay)
