#Lab 2 by Neha Palsokar
#Load the data
ratings<-read.csv("ratings.csv")
movies<-read.csv("movie.titles.csv")

head(ratings,n=15)

#a) what are the dimensions of the data? Ie, how many ratings? How many raters?

nrow(ratings)
#no of ratings=100004

head(movies)

#How many raters?--671 raters
length(unique(ratings[,"userId"]))

#b) what are the mean, median, and standard deviation of the ratings?

#mean=3.543608
mean(ratings[,"rating"])

#median=4
median(ratings[,"rating"])

#Standard deviation=1.058064
sd(ratings[,"rating"])


#c) plot a histogram of the ratings. What patterns do you see? Do people seem to prefer round numbers?
hist(ratings[,"rating"])
# Frequency is lower for numbers that are not round numbers, histogram shows that people prefer round numbers

#2. Link the two datasets using movieId
temp <- merge(ratings, movies, by="movieId")


#3. Exploring Relationships I
#a) Plot the relationship between ratings and year. Try a scatter plot and a box plot. What do you see?

plot(ratings[,"rating"], ratings[,"year"])

boxplot(ratings[,"rating"], ratings[,"year"])

#on temp data:

plot(temp$rating, temp$year)

boxplot(temp$rating, temp$year)
#it is not very clear

#b) It is messy, try coarse-graining the data using cut() on the years.

plot(cut(as.numeric(temp$year),breaks = 2000:2014))

plot(cut(ratings$year,breaks = 2000:2014))

plot( by(ratings$rating, ratings$year,mean))
barplot( by(ratings$rating, ratings$year,mean))

#4. Exploring Relationships II:
#a) Do the ratings vary by genre? Create a 'comedy' column and draw a box plot of ratings for comedy versus others:

ratings$comedy <- rep(F, nrow=ratings)

ratings$comedy[grep("comedy",ratings$genre, ignore.case = T)] <- T

boxplot(ratings$comedy,ratings$comedy)
boxplot(ratings[,"comedy"] == TRUE,ratings$rating)
boxplot(ratings[,"comedy"] == FALSE,ratings$rating)


#b) Run a t-test to see if the differences in ratings for comedy versus non-comedy
t.test(ratings$rating[ratings$comedy],ratings$rating[!ratings$comedy])

#what's the "best" movie in the dataset? Come up with a metric to assess quality and find the top 10 movies.


#PART 2

#exponential distribution
N <- 1000 # number of exponential draws
n.samp <- 30 # number of sums to take
M <- matrix(NA, nrow=N, ncol=n.samp) # create an empty matrix to fill with samples
for(j in 1:n.samp) M[,j] <- rexp(N) #generate the samples
hist(rowSums(M), freq = F) # plot a histogram of the sums across rows of our matrix M

curve(dnorm(x, mean(rowSums(M)), sd(rowSums(M))), min(rowSums(M)),
      max(rowSums(M)), add=T, col="red", lwd=2)

#Uniform distribution
N <- 1000 # number of exponential draws
n.samp <- 30 # number of sums to take
M <- matrix(NA, nrow=N, ncol=n.samp) # create an empty matrix to fill with samples
for(j in 1:n.samp) M[,j] <- runif(N) #generate the samples
hist(rowSums(M), freq = F) # plot a histogram of the sums across rows of our matrix M

curve(dnorm(x, mean(rowSums(M)), sd(rowSums(M))), min(rowSums(M)),
      max(rowSums(M)), add=T, col="red", lwd=2)

#Poisson distribution

N <- 1000 # number of exponential draws
n.samp <- 30 # number of sums to take
M <- matrix(NA, nrow=N, ncol=n.samp) # create an empty matrix to fill with samples
for(j in 1:n.samp) M[,j] <- rpois(N,0.5) #generate the samples
hist(rowSums(M), freq = F) # plot a histogram of the sums across rows of our matrix M

curve(dnorm(x, mean(rowSums(M)), sd(rowSums(M))), min(rowSums(M)),
      max(rowSums(M)), add=T, col="red", lwd=2)

#What pattern has emerged?
#Normal distribution has emerged

#Further explorations
#What happens when you change the number of samples from the distribution? What happens when you change the number of sums taken?

#exponential distribution
N <- 950 # number of exponential draws
n.samp <- 30 # number of sums to take
M <- matrix(NA, nrow=N, ncol=n.samp) # create an empty matrix to fill with samples
for(j in 1:n.samp) M[,j] <- rexp(N) #generate the samples
hist(rowSums(M), freq = F) # plot a histogram of the sums across rows of our matrix M

curve(dnorm(x, mean(rowSums(M)), sd(rowSums(M))), min(rowSums(M)),
      max(rowSums(M)), add=T, col="red", lwd=2)