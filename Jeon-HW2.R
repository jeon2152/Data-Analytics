#Assignment #2
#Name: Jiwon Jeon
#ISE 5103 Intelligent Data Analytics
#Date: 09/18/2016

#required packages for this assignment
library(DescTools)  #provides concordance and discordance in Problem 1
library(reshape2)   #provides data reshaping in Problem 2(a)
library(ggplot2)    #provides graphics needed in Problem 2(b)
library(robustbase) #provides adjbox command in Problem 3(f)
library(outliers)   #provides grubbs test in Problem 3(f)
library(fitdistrplus)  #provides graphics to evaluate the distribution of data in Problem 3(h)
library(Amelia)     #provides the data freetrade in Problem 4
library(mice)       #provides commands to explore "missingness" in Problem 4(a)
library(HSAUR2)     #provides the data heptathlon in Problem 5(b)
library(devtools)   #provides the package github in Problem 5(b)
install_github("ggbiplot","vqv")
library(ggbiplot)


#Problem 1
x = c(3,4,2,1,7,6,5)  #a vector x with 7 elements
y = c(4,3,7,6,5,2,1)  #a vector y with 7 elements
tableXY = table(x,y)
ConDisPairs(tableXY)[c("C","D")]

#Problem 2(a)
a = rnorm(500)  #a vector a with 500 normally distributed random numbers
b = rnorm(500)  #a vector b with 500 normally distributed random numbers
c = rnorm(500)  #a vector c with 500 normally distributed random numbers
d = rnorm(500)  #a vector d with 500 normally distributed random numbers
df = data.frame(a,b,c,d)  #a data frame df with 500 rows and 4 variables
head_df = head(df)

df2 = melt(df, variable.name = "groupVar", value.name = "value")  #a data frame df2 by reforming the data df from a "wide" format to a "long" format
head_df2 = head(df2)

#Problem 2(b)
##density plot by qqplot
ggplot(df2, aes(x=value, fill=groupVar)) +                   #identify data & variable
  geom_density(alpha=0.25) +                                 #set geometry and transparency    
  labs(x = "groupVar",                                       #x-axis label
       title = "Densities for the Values of each Variable")  #title

##density plot by qplot
qplot(value, data = df2,                                   #identify data & variable
      geom = "density",                                    #set the "geometry"
      main = "Densities for the Values of each Variable",  #title
      xlab = "groupVar",                                   #x-axis label
      fill=groupVar,                                       #fill color
      alpha=I(0.25))                                       #set fill transparency

#Problem 3(b)
sharkData = read.csv("ISE 5103 GSAF.csv", header = TRUE)  #load the original shark attack data
GSAFdata = sharkData[-(1:4069),]                          #clean the shark attack data

#Problem 3(c)
GSAFdata$DateR = as.Date(GSAFdata$Date, "%d-%b-%y")   #create a new variable which converts the character date type into R date type

for (i in 1:length(GSAFdata$DateR))                   #clean the new date column
  if(GSAFdata$DateR[i] > as.Date("2016-01-01") && !is.na(GSAFdata$DateR[i]))
    GSAFdata$DateR[i] = as.Date(GSAFdata$Date, "%d-%b-%Y")

head_GSAFdata = head(GSAFdata)

#Problem 3(d)
mean(is.na(GSAFdata$DateR))  #ratio of missing new date field

#Problem 3(e)
GSAFdata = subset(GSAFdata, !GSAFdata$DateR==is.na(GSAFdata$DateR))  #delete rows with missing new date

#Problem 3(f).i
daysBetween = as.vector(diff(GSAFdata$DateR))  #create a vector daysBetween
daysBetween = c(NA,daysBetween)                #add a missing value as the first element of daysBetween
GSAFdata$daysBetween = daysBetween             #add the vector daysBetween in GSAFdata
daysBetween

#Problem 3(f).ii
boxplot(GSAFdata$daysBetween,                  #boxplot of daysBetween
        main = "Boxplot for daysBetween",      #main plot title
        xlab = "sharkAttack",                  #x-axis label
        ylab = "daysBetween")                  #y-axis label

adjbox(GSAFdata$daysBetween,                   #boxplot of daysBetween
       main = "Adjusted Boxplot for daysBetween",       #main plot title
       xlab = "sharkAttack",                   #x-axis label
       ylab = "daysBetween")                   #y-axis label

#Problem 3(f).iii
grubbs.test(GSAFdata$daysBetween)  #grubb's test for daysBetween

#generalized ESD test -- followed instruction in generalizedESD.R

# helper function
# Compute the critical value for ESD Test
esd.critical <- function(alpha, n, i) {
  p = 1 - alpha/(2*(n-i+1))
  t = qt(p,(n-i-1))
  return(t*(n-i) / sqrt((n-i-1+t**2)*(n-i+1)))
}

#main function
removeoutliers = function(y,k=5,alpha=0.05) {
  
  if (k<1 || k >= length(y))
    stop ("the number of suspected outliers, k, must be in [1,n-1]")
  
  ## Define values and vectors.
  y2 = y
  n = length(y)
  toremove = 0
  tval<-NULL
  ris<-NULL
  
  ## Compute test statistic until r values have been removed from the sample.
  for (i in 1:k){
    if(sd(y2)==0) break
    ares = abs(y2 - mean(y2))/sd(y2)
    Ri = max(ares)
    y2 = y2[ares!=Ri]
    
    tval<-c(tval,esd.critical(alpha,n,i))
    ris<-c(ris,Ri)
    ## Compute critical value.
    if(Ri>esd.critical(alpha,n,i))
      toremove = i
  }
  
  # Values to keep
  if(toremove>0){
    outlierLevel = sort(abs(y-mean(y)),decreasing=TRUE)[toremove]
    o = y[abs(y-mean(y)) >= outlierLevel]
    y = y[abs(y-mean(y)) < outlierLevel]
  }
  
  RVAL <- list(numOutliers=toremove,outliers=o,cleandata=y,critical=tval,teststat=ris)
  return (RVAL) 
}

#clean GSAFdata for use (remove NA)
GSAFdata2 = GSAFdata[-1,]
#run generalized ESD
removeoutliers(GSAFdata2$daysBetween, 10, 0.05)

#Problem 3(g)
sample = rexp(1500)                    #generate sample exponential distribution
qqplot(sample, GSAFdata$daysBetween,  #qqplot to evalute the time between shart attack
       main = "qqplot for exponential distribution",
       xlab = "sample data",
       ylab = "time between shark attacks") 
qqline(GSAFdata$daysBetween)          #add qqline

#Problem 3(h)
GSAFdata = GSAFdata[GSAFdata$daysBetween>0,]
GSAFdata = GSAFdata[-1,]
fitData = fitdist(GSAFdata$daysBetween, "exp")   #transform data to be a 'fitdist' object using exponential distribution
fitData2 = fitdist(GSAFdata$daysBetween, "pois") #transform data to be a 'fitdist' object using poisson distribution

cdfcomp((list(fitData)), legendtext = "exponential")
denscomp((list(fitData)), legendtext = "exponential")
qqcomp((list(fitData)), legendtext = "exponential")
gofstat((list(fitData, fitData2)), fitnames = c("exponential","pois"))

#Problem 4(a)
data(freetrade)         #load the data freetrade
md.pairs(freetrade)     #display number of observations per variable pair
md.pattern(freetrade)   #display missing-data patterns

#Problem 4(b)
tariff_country = freetrade[,c(2,3)]                             #data frame with country and tariff
tariff_country[is.na(tariff_country$tariff),]["tariff"] = 0     #assign 0 to missing values
tariff_country$tariff = ifelse(tariff_country$tariff>=1,1,0)    #assign 1 to the rest of values
table_tc = table(tariff_country$tariff,tariff_country$country)  #table of tariff and country with missing and nonmissing values
chisq.test(table_tc)  #chiSquare test
removeNepal = tariff_country[!tariff_country$country == "Nepal",]  #remove Nepal from dataset
table_nepal = table(removeNepal$tariff,removeNepal$country)
chisq.test(table_nepal)
removePhilippines = tariff_country[!tariff_country$country == "Philippines",]  #remove Philippines from dataset
table_Philippines = table(removePhilippines$tariff,removePhilippines$country)
chisq.test(table_Philippines)

#Problem 5(a).i
corMat = as.matrix(cor(mtcars))   #correlation matrix of all the attributes of mtcars
corMat

#Problem 5(a).ii
eigen(corMat, symmetric = TRUE)   #eigenvalues and eigenvectors of corMat

#Problem 5(a).iii
pca = prcomp(mtcars, scale. = TRUE)     #principal components of mtcars 
pca

#Problem 5(a).iv
PCs = as.matrix(pca$rotation)   #principal components of mtcars
PC1 = PCs[,1]                   #principal component 1
PC2 = PCs[,2]                   #principal component 2
innerProduct = PC1 %*% PC2      #inner product (dot product) of PC1 and PC2
innerProduct

#Problem 5(b).i
par(mfrow = c(2,2))
apply(heptathlon[,1:8],2,hist)  #quick inspection of heptathlon histograms
dev.off()                       #cancel par(mfrow = c(2,2))

#Problem 5(b).ii
grubbs.test(heptathlon$hurdles)  #outlier for hurdles: Launa (PNG)
grubbs.test(heptathlon$highjump) #outlier for highjump: Launa (PNG)
grubbs.test(heptathlon$shot)     #outlier for shot: Hui-Ing (TAI)
grubbs.test(heptathlon$run200m)  #outlier for run200m: Joyner-Kersee (USA)
grubbs.test(heptathlon$longjump) #outlier for longjump: Launa (PNG)
grubbs.test(heptathlon$javelin)  #outlier for javelin: Scheider (SWI)
grubbs.test(heptathlon$run800m)  #outlier for run800m: Launa (PNG)

heptathlon = heptathlon[-25,]    #remove the outlier; Launa (PNG)

#Problem 5(b).iii
heptathlon$hurdles = max(heptathlon$hurdles) - heptathlon$hurdles  #transform hurdles to make large values to be good
heptathlon$run200m = max(heptathlon$run200m) - heptathlon$run200m  #transform run200m to make large values to be good
heptathlon$run800m = max(heptathlon$run800m) - heptathlon$run800m  #transform run800m to make large values to be good

#Problem 5(b).iv
Hpca = prcomp(heptathlon[,1:7], scale. = TRUE)  #principal component analysis on the 7 events
Hpca
summary(Hpca)

#Problem 5(b).v
ggbiplot(Hpca, obs.scale = 1, var.scale = 1,   #ggbiplot to visualize PC1 and PC2
         circle = TRUE, varname.size = 4)

ggbiplot(Hpca, obs.scale = 1, var.scale = 1,
         varname.size = 4, labels.size = 2.5, 
         circle = TRUE, labels = rownames(heptathlon))

#Problem 5(b).vi
Hpca$x[,1]                               #projections onto PC1
plot(Hpca$x[,1], heptathlon$score,       #plot of the heptathlon score vs. projections on PC1
     main = "Heptathlon score vs. PC1",  #main title
     xlab = "projections onto PC1",      #x-axis label
     ylab = "heptathlon score")          #y-axis label
as.vector(Hpca$x[,1])

#Problem 5(c).i
digit1 = read.csv("train.1", header = FALSE)   #load the data digit 1
digit6 = read.csv("train.6", header = FALSE)   #load the data digit 6
digit7 = read.csv("train.7", header = FALSE)   #load the data digit 7

pca1 = prcomp(digit1)  #PCA analysis on digit 1 without scaling 
pca6 = prcomp(digit6)  #PCA analysis on digit 6 without scaling
pca7 = prcomp(digit7)  #PCA analysis on digit 7 without scaling

head(pca1$rotation[,1:5])  #rotation matrix of PCA digit 1
head(pca6$rotation[,1:5])  #rotation matrix of PCA digit 6
head(pca6$rotation[,1:5])  #rotation matrix of PCA digit 7

head(pca1$x[,1:5])  #projections of digit 1 on PC's
head(pca6$x[,1:5])  #projections of digit 6 on PC's
head(pca7$x[,1:5])  #projections of digit 7 on PC's

summary(pca1)  #proportion of variance of each PC for digit 1
summary(pca6)  #proportion of variance of each PC for digit 6
summary(pca7)  #proportion of variance of each PC for digit 7

screeplot(pca1, npcs = 30, type = "lines", main = "Screeplot for Digit 1")  #screeplot for PCA digit 1
screeplot(pca6, npcs = 30, type = "lines", main = "Screeplot for Digit 6")  #screeplot for PCA digit 6
screeplot(pca7, npcs = 30, type = "lines", main = "Screeplot for Digit 7")  #screeplot for PCA digit 7

#Problem 6(b)
titanic = read.csv("titanic.csv", header = TRUE)
attach(titanic)

nrow(titanic)    #number of rows
ncol(titanic)    #number of variables
plot(titanic)    #plot of variables
md.pairs(titanic)     #missingness in data per variable
md.pattern(titanic)   #missingness in data total

titanic = titanic[!is.na(titanic$Age),]  #clean data by removing missing values in Age
titanic_survival = titanic[titanic$Survived == 1,]  #clean data for survivals

boxplot(titanic_survival$Age,                 #boxplot of Survival Age
        main = "Boxplot for Survival Age",  
        xlab = "Survival",              
        ylab = "Age") 

grubbs.test(titanic_survival$Age)             #find the strongest outlier

ggplot(titanic_survival, aes(x=Age, fill=Survived)) +  #density plot for the Survival's age               
  geom_density(alpha=0.25) +                                     
  labs(x = "age",                                       
       title = "Survival by Age")   

