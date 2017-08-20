#Assignment #1
#Jiwon Jeon

#required packages for this assigment
library(lsr)  #provides the statistical measure for Problem 1(c)
library(e1071)  #provides the statistical measures for Problem 1(e)
library(plyr)  #provides the statistical data for Problem 3
library(datasets)  #provides datasets for Problem 4

#Problem 1(a)
x = c(3,12,6,-5,0,8,15,1,-10,7)  #a vector x with 10 numbers
x

#Problem 1(b)
y = seq(min(x), max(x), length = 10)  #a vector y with 10 elements 
                                      #between minimum x and maximum x
y

#Problem 1(c)
sum(x)  #sum of x
mean(x)  #mean of x
sd(x)  #standard deviation of x
var(x)  #variance of x
aad(x)  #mean absolute deviation of x (uses package 'lsr')
mad(x)  #median absolute deviation of x
quantile(x)  #quartile of x
quantile(x, probs = seq(0,1,0.2))  #quintile of x

sum(y)  #sum of y
mean(y)  #mean of y
sd(y)  #standard deviation of y
var(y)  #variance of y
aad(y)  #mean absolute deviation of y (uses package 'lsr')
mad(y)  #median absolute deviation of y
quantile(y)  #quartile of y
quantile(y, probs = seq(0,1,0.2))  #quintile of y

#Problem 1(d)
z = sample(x, 7, replace = TRUE)  #a vector z with 7 random numbers
                                  #from x with replacement
z

#Problem 1(e): uses package 'e1071'
skewness(x)  #skewness of x
kurtosis(x)  #kurtosis of x

#Problem 1(f)
t.test(x,y)  #statistical test between the vectors x and y 
             #Mean of x is 3.7 while mean of y is 2.5. 
             #The difference in these two means is not significant compared to 
             #the range of x and y. For individual statistical test, 
             #`t.test(x)` and `t.test(y)` can be used, respectively.

#Problem 1(g)
sort(x)  #sorts the vector x in ascending order
t.test(x,sort(x))  #t-test for x and sort(x)

#Problem 1(h)
x<0  #a logical vector to identify negative numbers in x

#Problem 1(i):
x = x[x>=0]  #removes the negative numbers from x
x

#Problem 2(a)
college = read.csv("college.csv", header = TRUE)  #reads the data file "College.csv"
college = data.frame(college)  #loads the data as data frame

#Problem 2(b)
rownames(college) = college[,1]  #displays the row.names with the name
                                 #in the first column
View(college)  #views the data
college = college[,-1]  #removes the generated column for row.names
View(college)

#Problem 2(c).i:
summary(college)  #produces a numerical summary

#Problem 2(c).ii 
?pairs  #help for the pairs()
pairs(college[,1:10])  #produces a scatterplot matrix of the first ten columns

#Problem 2(c).iii
plot(college$Private, college$Outstate, xlab = "Private", ylab = "Outstate", main = "Outstate vs. Private")  #boxplots of Outstate vs. Private

#Problem 2(c).iv
#creates 777 replicated value of "No", and names the vector as Elite
Elite = rep("No", nrow(college))
#creates a logical vector to see if the proportion of students from
#the top 10% of their high school classes exceeds 50%.
#If the condition is TRUE, replaces "No" to "Yes"
Elite [college$Top10perc > 50] = "Yes"
#converts a vector of Elite into a factor to recognize "Yes" or "No"
#in column of data frame
Elite = as.factor(Elite)
#finishes creating a new qualitative variable, Elite, by combining
#the data frame college and Elite
college = data.frame(college, Elite)

#Problem 2(c).v
summary(college)  #There are 78 elite universities out of 777 universities

#Problem 2(c).vi
plot(college$Elite, college$Outstate, xlab = "Elite", ylab = "Outstate", main = "Outstate vs. Elite")  #boxplots of Outstate vs. Elite

#Problem 2(c).vii
par(mfrow=c(2,2))  #divides the print window into four regions
hist(college$Apps)  #histogram for number of applications received
hist(college$Accept)  #histogram for number of applicants accepted
hist(college$Enroll)  #histogram for number of new students enrolled
hist(college$Expend)  #histogram for instructional expenditure/student
##or
attach(college)  #attaches data to the R search path to access simply with their names
par(mfrow=c(2,2))
hist(Apps)
hist(Accept)
hist(Enroll)
hist(Expend)

#Problem 3(a): uses package 'plyr'
baseball = data.frame(baseball)
?baseball

#Problem 3(b)
baseball$sf[baseball$year < 1954] = 0  #sets sacrifice flies (sf) to 0 before 1954
#or
#for(i in 1:nrow(baseball)){
#  if(baseball$year[i] < 1954){
#    baseball$sf[i] = 0
#  }
#}

baseball$hbp[is.na(baseball$hbp)] = 0  #sets missings in hit by pitch (hbp) to 0
baseball = baseball[baseball$ab >= 50,]  #excludes all player records with
                                         #fewer than 50 at bats(ab)

#Problem 3(c)
#calculates on base percentage in the variable obp
baseball$obp = (baseball$h + baseball$bb + baseball$hbp)/(baseball$ab + baseball$bb + baseball$hbp + baseball$sf)
#or
attach(baseball)
baseball$obp = (h+bb+hbp)/(ab+bb+hbp+sf)

#Problem 3(d)
#sorts the data in descending order
baseball_order = baseball[order(-baseball$obp),]
#prints year, id (name), and obp for top five records
baseball = print(baseball_order[1:5, c("year", "id", "obp")])
#or
#baseball = print(baseball_order[1:5, c(1,2,23)])

#Problem 4(a): uses package 'datasets'
quakes = data.frame(quakes)

#Problem 4(b)
#dev.off()  #resets par(mfrow=c(2,2)) to default print window
plot(quakes$depth, quakes$mag, xlab = "depth", ylab = "magnitude", main = "magnitude vs. depth")  #scatter plot of magnitude vs. depth

#Problem 4(c)
#computes the average earthquake depth for each magnitude level
quakeAvgDepth = aggregate(x = quakes$depth, by = list(quakes$mag), FUN = "mean")

#Problem 4(d)
names(quakeAvgDepth)[1] = "magnitude"
names(quakeAvgDepth)[2] = "average depth"
##or
colnames(quakeAvgDepth) = c("magnitude", "average depth")
quakeAvgDepth

#Problem 4(e)
#scatter plot of magnitude vs. average depth
plot(quakeAvgDepth$`average depth`, quakeAvgDepth$magnitude, xlab = "average depth", ylab = "magnitude", main = "magnitude vs. average depth")

#Problem 4(f)
#From the plot of magnitude vs. average depth, it can be said that the magnitude and average depth 
#of earthquake has fairly inverse linear relationship. 
#However, it is difficult to find this tendency from the plot of magnitude vs. depth.
