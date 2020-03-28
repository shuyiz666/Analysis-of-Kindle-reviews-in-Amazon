# Preparing the data
kindle_reviews<-read.csv("kindle_reviews.csv",header = TRUE, skipNul = TRUE)
head(kindle_reviews)
sapply(kindle_reviews,class)

# Analyzing the data
length(unique(kindle_reviews$asin))
unique(kindle_reviews$overall)

# one categorical variable
mostrating <- max(table(kindle_reviews$asin)) # the number of most reviews
leastrating <- min(table(kindle_reviews$asin)) # the number of least reviews
table(kindle_reviews$asin)[table(kindle_reviews$asin) == mostrating] # the product which has most rating
length(table(kindle_reviews$asin)[table(kindle_reviews$asin) == leastrating]) # the number of products which has least rating
mean(table(kindle_reviews$asin)) # the average number of comments for each product
median(table(kindle_reviews$asin)) # the average number of comments for each product
a <- table(table(kindle_reviews$asin)) # the number of products in each number of comments
plot(names(a),as.numeric(a), xlab = 'the number of comments for each product', ylab = 'the number of products for each comments number', main = 'comments number frequency')

# one numerical variable
ratingdistr <- table(kindle_reviews$overall)
slice.labels <- names(ratingdistr)
slice.percents <- round(ratingdistr/sum(ratingdistr)*100)
slice.labels <- paste(slice.labels, slice.percents, sep = ',')
slice.labels <- paste(slice.labels, "%", sep = '')
pie(ratingdistr, labels = slice.labels, col = c('#8B1A1A','#CD5B45','#FFB90F','#6495ED','#9BCD9B'),main = 'percentage of rating')

# set of two or more variables
# B006GWO5WK’s overall rating changes in different time
newdata <- kindle_reviews[kindle_reviews$asin == 'B006GWO5WK',]
b <- c(newdata$unixReviewTime)
class(b)=c('POSIXt','POSIXct')
plot(b, newdata$overall,pch=16,cex=0.5, xlab = 'review time', ylab = 'overall rating', main = 'the overall rating in different time')

# Implementation of any feature(s) - tibble
# the relationship of number of reviews and average rating for each product
library(tidyverse)
ProductAvgRating <- kindle_reviews %>% group_by(productID = asin)  %>% summarise(count = n(), AvgRating = mean(overall))
plot(ProductAvgRating$count, ProductAvgRating$AvgRating, xlab = 'Number of Reviews', ylab = 'Average Rating', main = 'The Relationship of The Number of Reviews and Average Rating for each Product', cex = 0.4)

# Pick one variable with numerical data and examine the distribution of the data. 
hist(ProductAvgRating$AvgRating,xlab = 'average rating',main = 'average rating of each product distribution')

# Draw various random samples of the data and show the applicability of the Central Limit Theorem for this variable. 
par(mfrow = c(2,2))
samples <- 1000
sample.size <- c(10,20,30,40)
xbar <- numeric(samples)
for(size in sample.size){
  for(i in 1:samples){
    xbar[i] <- mean(sample(ProductAvgRating$AvgRating, size = size, replace = TRUE))
  }
  hist(xbar, prob = TRUE, main = paste('Sample Size = ', size))
  cat('Sample Size = ', size, 'Mean = ', mean(xbar), 'SD = ', sd(xbar), '\n')
}

# Show how various sampling methods can be used on your data. What are your conclusions if these samples are used instead of the whole dataset. 
# Simple Random Sampling
library(sampling)
data <- ProductAvgRating$AvgRating
s <- srswor(100,length(data))
sample <- data[s!=0]
hist(sample)
# Systematic Sampling:
N <- length(data)
n <- 100
k <- floor(N/n)
r <- sample(k,1)
s <- seq(r, by = k, length = n)
sample <- data[s]
hist(sample)
 




