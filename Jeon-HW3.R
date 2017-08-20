#Assignment #3
#Name: Jiwon Jeon
#DSA/ISE 5103 Intelligent Data Analytics
#Date: 10/07/2016

#required packages for this assignment
library(mlbench)     #provides the dataset Glass for Problem 1
library(ggplot2)     #provides the command ggplot for Problem 1
library(reshape2)    #provides the command melt for Problem 1
library(e1071)       #provides the command skewness for Problem 1
library(car)         #provides the command symbox for Problem 1
library(EnvStats)    #provides the command boxcox for Problem 1
library(devtools)    #provides the package github in Problem 1
install_github("ggbiplot","vqv")
library(ggbiplot)
library(MASS)        #provides the command lda for Problem 1
library(gridExtra)   #provides the command grid.arrange for Problem 1
library(Amelia)      #provides the dataset freetrade for Problem 2
library(mice)        #provides packages for 'multivariate imputation by chained equations' in Problem 2
library(stats)       #provides the command fft for Problem 3


#Problem 1(a)
data(Glass)           #load the dataset Glass
Glass2 = melt(Glass)  #combine the nine predictors into one column 

#histogram -------------------------------------------------

ggplot(Glass2, aes(x=value, fill=variable)) +       #set data and aesthetics
  geom_histogram() +                                #histogram of nine predictors
  facet_wrap(~variable, scales = "free_x") +        #display all attributes in one page
  labs(x = "Element",          
       y = "Value",            
       title = "Histograms for Elements of Glass")

#RI shows slightly positive skewness
#Na shows fairly normal distribution
#Mg shows fairly symetric data but concentrated on bth ends
#Al shows slightly positive skewneww but normally distributed
#Si shows slightly negative skewness
#K shows very positive skewness
#Ca shows slightly positive skewness
#Ba shows very positive skewness
#Fe shows very positive skewness

#calculate skewness of each predictor
sk1 = skewness(Glass$RI)
sk2 = skewness(Glass$Na)
sk3 = skewness(Glass$Mg)
sk4 = skewness(Glass$Al)
sk5 = skewness(Glass$Si)
sk6 = skewness(Glass$K)
sk7 = skewness(Glass$Ca)
sk8 = skewness(Glass$Ba)
sk9 = skewness(Glass$Fe)

#show the results of skewness as a data frame
sk = data.frame(c(sk1,sk2,sk3,sk4,sk5,sk6,sk7,sk8,sk9), 
                row.names = c("RI","Na","Mg","Al","Si","K","Ca","Ba","Fe"))
colnames(sk) = "skewness"
sk = t(sk)

#scatter plots ---------------------------------------------

plot(Glass)                                                     #multiple scatter plots

ggplot(data=Glass2, aes(x=variable,y=value)) +                  #set data and aesthetics
  geom_point(aes(fill=variable),                                #add points (fill color based on "variable", i.e. predictors)
             colour=NA,                                         #outline set to none-color
             pch=21,                                            #shape = 21, a filled circle
             size=2) +                                          #size = 2
  theme_bw() +                                                  #using black and white background theme
  labs(x = "Predictors",                                        #x-axis label
       y = "Value",                                             #y-axis label
       title = "Scatter plots for Predictors of Glass") +       #main plot title   
  theme(legend.position = "right")                              #legend on the right side of the screen

#box plots (detecting outliers) ----------------------------

ggplot(Glass2, aes(x = variable, y = value, fill=variable)) +   #set data and aesthetics for boxplots using 'ggplot'   
  geom_boxplot() +                                              #boxplot of nine predictors in one graph
  labs(x = "Predictors",                                        #x-axis label
       title = "Boxplots for Predictors of Glass")              #main plot title

boxplot(data = Glass2, value ~ variable,                        #set data for boxplots of nine predictors using 'boxplot'; or boxplot(Glass[,-10])
        main = "Boxplots for Predictors of Glass",              #main plot title
        xlab = "Predictors",                                    #x-axis label   
        ylab = "Value")                                         #y-axis label

ggplot(Glass2, aes(x = variable, y = value, fill = variable)) + 
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_x") +                    #display individual boxplots of predictors in one page
  labs(x = "Predictors",          
       y = "Value",            
       title = "Boxplots for Predictors of Glass")


ggplot(Glass, aes(x=Na, y=Na, fill=I("blue"))) +                #set data and aesthetics for individual boxplots using 'ggplot'
  geom_boxplot(alpha=0.2) +
  labs(x = "Na",          
       y = "Value",            
       title = "Boxplot for Na of Glass")

boxplot(Glass$Na, col = "bisque",                               #set data and color for individual boxplots using 'boxplot'
        main = "Boxplot for Na of Glass", 
        xlab = "Na",   
        ylab = "Value")

#Detecting and printing outliers ---------------------------

Glass3 = Glass[,-10]                                #remove 'Type' from the data Glass

outlier = data.frame(boxplot(Glass3)$out)           #detect outliers(below or above +-1.5IQR) of each predictor
group = data.frame(boxplot(Glass3)$group)           #detect the predictors(groups) corresponding to the outliers
table = cbind(group, outlier)                       #combine and tabulize outliers of each predictor
colnames(table) = c("predictors", "outliers")       #assign column names of table
as.character(table$predictors)                      #convert the value of predictors to character-type

#Assign predictor names to the corresponding group numbers
table$predictors[table$predictors == "1"] = "RI"
table$predictors[table$predictors == "2"] = "Na"
table$predictors[table$predictors == "3"] = "Mg"
table$predictors[table$predictors == "4"] = "Al"
table$predictors[table$predictors == "5"] = "Si"
table$predictors[table$predictors == "6"] = "K"
table$predictors[table$predictors == "7"] = "Ca"
table$predictors[table$predictors == "8"] = "Ba"
table$predictors[table$predictors == "9"] = "Fe"

head(table)

#obtain the number of outliers of each predictor
RI = length(boxplot.stats(Glass$RI)$out)
Na = length(boxplot.stats(Glass$Na)$out)
Mg = length(boxplot.stats(Glass$Mg)$out)
Al = length(boxplot.stats(Glass$Al)$out)
Si = length(boxplot.stats(Glass$Si)$out)
K = length(boxplot.stats(Glass$K)$out)
Ca = length(boxplot.stats(Glass$Ca)$out)
Ba = length(boxplot.stats(Glass$Ba)$out)
Fe = length(boxplot.stats(Glass$Fe)$out)

nOutlier = cbind(RI,Na,Mg,Al,Si,K,Ca,Ba,Fe)          #display the number of outliers of each predictor


#Problem 1(b)
#Three attributes are selected for a skew transformation: K, Ba, Ca

#symbox for the three attributes ---------------------------

par(mfrow = c(3,1))          #setup graphics device to make three plots on the screen 

symbox(Glass$K + 0.001, data=Glass, powers=c(3,2,1,0.5,0,-0.5,-1,-2,-3), main = "K")    #produces parallel boxplots of K for different values of lambda
symbox(Glass$Ba + 0.001, data=Glass, powers=c(3,2,1,0.5,0,-0.5,-1,-2,-3), main = "Ba")  #produces parallel boxplots of Ba for different values of lambda
symbox(Glass$Ca, data=Glass, powers=c(3,2,1,0.5,0,-0.5,-1,-2,-3), main = "Ca")          #produces parallel boxplots of Ca for different values of lambda

par(mfrow = c(1,1))          #reset graphics device to the default 1 plot

#histogram after symbox command ----------------------------

par(mfrow = c(2,3))          #setup graphics device to make two plots on the screen

hist(Glass$K)                #histogram of the Glass$K variable
hist(Glass$Ba)               #histogram of the Glass$Ba variable
hist(Glass$Ca)               #histogram of the Glass$Ca variable

hist(log(Glass$K))           #histogram of the log transformed Glass$K variable
hist(log(Glass$Ba))          #histogram of the log transformed Glass$Ba variable
hist(log(Glass$Ca))          #histogram of the log transformed Glass$Ca variable

#additional....
hist((Glass$Ca)^(0.5))       #histogram of the transformed Glass$Ca variable with lambda of 0.5
hist((Glass$Ca)^(-0.5))      #histogram of the transformed Glass$Ca variable with lambda of -0.5

skewness(log(Glass$Ca))      #skewness of the log transformed Glass$Ca variable
skewness((Glass$Ca)^(0.5))   #skewness of the transformed Glass$Ca variable with lambda of 0.5
skewness((Glass$Ca)^(-0.5))  #skewness of the transformed Glass$Ca variable with lambda of -0.5; the least value of skewness (most normality)

#boxcox for the three attributes ---------------------------

boxcox(Glass$K + 0.001, optimize = TRUE, lambda=c(-3,3))    #lambda=0.374133
boxcox(Glass$Ba + 0.001, optimize = TRUE, lambda=c(-3,3))   #lambda=0.1563629
boxcox(Glass$Ca, optimize = TRUE, lambda=c(-3,3))           #lambda=-0.8593591

#histogram after boxcox command ----------------------------

hist(Glass$K)                                               #histogram of the Glass$K variable
hist(Glass$Ba)                                              #histogram of the Glass$Ba variable
hist(Glass$Ca)                                              #histogram of the Glass$Ca variable

hist((Glass$K**0.374133-1)/0.374133,                        #histogram of the transformed Glass$K variable with lambda=0.374133 
     main = "Transformed Glass$K",
     xlab = "Transformed Glass$K")
hist((Glass$Ba**0.1563629-1)/0.1563629,                     #histogram of the transformed Glass$K variable with lambda=0.1563629
     main = "Transformed Glass$Ba",
     xlab = "Transformed Glass$Ba")
hist((Glass$Ca**(-0.8593591)-1)/(-0.8593591),               #histogram of the transformed Glass$K variable with lambda=-0.8593591
     main = "Transformed Glass$Ca",
     xlab = "Transformed Glass$Ca")

par(mfrow = c(1,1))                                         #reset graphics device to the default 1 plot


#Problem 1(c)
pcaGlass = prcomp(Glass3, scale. = TRUE)                            #PCA of Glass without Type (no class information)
pcaGlass                                                            #accesses rotation (the components of the PCA object)
summary(pcaGlass)                                                   #provides proportion of variance of each PC   

ggbiplot(pcaGlass, obs.scale = 1, var.scale = 1,                    #visualizes the distribution of data onto PC1 and PC2 panel
         groups = Glass$Type, varname.size = 4, labels.size = 2.5,  #categorizes the data into Types
         ellipse = TRUE, circle = TRUE)


#Problem 1(d)
ldaGlass = lda(Type ~ ., scale. = TRUE, data = Glass)               #LDA of Glass with Type (with class information)
ldaGlass
summary(ldaGlass)

#lda vs. pca -----------------------------------------------

prop.pcaGlass = pcaGlass$sdev^2/sum(pcaGlass$sdev^2)
prop.ldaGlass = ldaGlass$svd^2/sum(ldaGlass$svd^2)

pldaGlass = predict(object = ldaGlass, newdata = Glass)

dataLDAPCA = data.frame(type = Glass[,"Type"],
                     pca = pcaGlass$x, lda = pldaGlass$x)

plotlda = ggplot(dataLDAPCA) + geom_point(aes(lda.LD1, lda.LD2, colour = type, shape = type), size = 2.5) + 
  labs(x = paste("LD1 (", percent(prop.ldaGlass[1]), ")", sep=""),
       y = paste("LD2 (", percent(prop.ldaGlass[2]), ")", sep=""))

plotpca <- ggplot(dataLDAPCA) + geom_point(aes(pca.PC1, pca.PC2, colour = type, shape = type), size = 2.5) +
  labs(x = paste("PC1 (", percent(prop.pcaGlass[1]), ")", sep=""),
       y = paste("PC2 (", percent(prop.pcaGlass[2]), ")", sep=""))

grid.arrange(plotlda, plotpca)


#Problem 2(a)
data(freetrade)                                #load the dataset freetrade
ftList.del = data.frame(na.omit(freetrade))    #listwise deletion (delete the record if any value missing) 

lmList.del = lm(data=ftList.del,                                                         #analysis(linear regression) to predict the value of tariff
                tariff ~ year+country+polity+pop+gdp.pc+intresmi+signed+fiveop+usheg)    #using listwise deletion
lmList.del

summary(lmList.del)                                  #assesses the coefficients of each term and their statistics
coeffList.del = summary(lmList.del)$coefficients     #prints the coefficient information
coeffList.del

plot(lmList.del)                               #we can see the fitness of the regression model by looking at residuals


#Problem 2(b)
ftMean.imp = freetrade                         #copy the freetrade data to construct imputation by mean
for(i in 3:10){                                                               #for column 3 to 10 in the ftMean.imp data,
  ftMean.imp[is.na(ftMean.imp[,i]),i] = mean(ftMean.imp[,i], na.rm = TRUE)    #do imputation by mean
}

lmMean.imp = lm(data=ftMean.imp,                                                         #analysis(linear regression) to predict the value of tariff
                tariff ~ year+country+polity+pop+gdp.pc+intresmi+signed+fiveop+usheg)    #using mean imputation
lmMean.imp

summary(lmMean.imp)                                  #assesses the coefficients of each term and their statistics
coeffMean.imp = summary(lmMean.imp)$coefficients     #prints the coefficient information
coeffMean.imp

plot(lmMean.imp)                               #we can see the fitness of the regression model by looking at residuals


#Problem 2(c)
#Multiple imputation (MI) with mice using 'sample' method
ftMICE.imp = freetrade                         #copy the freetrade data to construct MICE 

#perform the replication and imputation steps of MI using mice command ----------------------------

ftMICE.imp = mice(freetrade, m = 5, maxit = 10, method = "sample")    #creat m = 5 (default) data sets (replication) and 
                                                                      #impute missing values (imputation) with 10 iterations

ftMICE.imp$chainMean                                                  #check how the means and variances of 
ftMICE.imp$chainVar                                                   #the imputed values are converging

plot(ftMICE.imp)                                                      #plot the means and variance

#perform the analysis step of MI using the "with" command to perform a regression analysis --------

lmMICE.imp = with(ftMICE.imp, lm(tariff ~ year+country+polity+pop+gdp.pc+intresmi+signed+fiveop+usheg))
lmMICE.imp
summary(lmMICE.imp)

#perform the recombination step of MI using the "pool" command ------------------------------------

estMICE.imp = pool(lmMICE.imp)
estMICE.imp

summary(estMICE.imp)                 #assesses the coefficients of each term and their statistics
coeffMICE.imp = estMICE.imp$qbar     #prints the coefficient information
coeffMICE.imp


#Problem 2(d)
#Single imputations (SI) for each variable
ftSI.imp = freetrade                             #copy the freetrade data to construct Single imputations using MICE

ftSI.imp = mice(freetrade, m = 5, maxit = 10,    #perform mice with randomly chosen methods for each variable
                method = c("sample","rf","sample","sample","rf","rf","cart","cart","sample","rf"))

lmSI.imp = with(data = ftSI.imp,                 #apply linear regression analysis
                lm(tariff ~ year+country+polity+pop+gdp.pc+intresmi+signed+fiveop+usheg))
lmSI.imp
summary(lmSI.imp)

estSI.imp = pool(lmSI.imp)
estSI.imp

summary(estSI.imp)                   #assesses the coefficients of each term and their statistics
coeffSI.imp = estSI.imp$qbar         #prints the coefficient information
coeffSI.imp

#Problem 2(e)
c_a = data.frame(coeffList.del)      #the regression coefficients using Listwise Deletion
c_b = data.frame(coeffMean.imp)      #the regression coefficients using Mean Imputation
c_c = data.frame(coeffMICE.imp)      #the regression coefficients using Multiple Imputation by Predictive Mean Matching
c_d = data.frame(coeffSI.imp)        #the regression coefficients using Single Imputation by Predictive Mean Matching


#Problem 3
truckData = read.csv("bridgeSensor.csv", header = TRUE)    #load the bridgeSensor data set

plot(truckData$Time, truckData$Sensor1, type = "l", xlab = "Time", ylab = "Sensor1")
plot(truckData$Time, truckData$Sensor2, type = "l", xlab = "Time", ylab = "Sensor2")

#convert the time domain into the frequency domain -----------------------

#helper function to plot the frequency spectrum 
plot.frequency.spectrum = function(X.k, xlimits=c(0,length(X.k)/2)) {
  plot.data = cbind(0:(length(X.k)-1), Mod(X.k))
  
  plot(plot.data, t="h", lwd=2, main="", 
       xlab="Frequency (Hz)", ylab="Strength", 
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
}

#apply the fast fourier transform
fft1 = fft(truckData$Sensor1)
fft2 = fft(truckData$Sensor2)
fftAll = cbind(fft1, fft2)

#plot the frequency spectrum 
plot1 = plot.frequency.spectrum(fft1)
plot2 = plot.frequency.spectrum(fft2)

#Problem 3(b)

#construct the features using fft data from both sensors ------------

features.fft = function(x, sample.rate = 1){                            #user-defined function to create features
  x = x/length(x)     #to normalize the data
  
  distance.center = function(y)signif(Mod(y),3)                         #using the maximum(mode) value of fft 
  angle = function(y)signif(180*Arg(y)/pi,3)                            #and/or using angle calculated from the values of fft
  
  features = data.frame(cycle = 0:(length(x)-1),                        #four features are created incl. cycle,
                        freq = 0:(length(x)-1)*sample.rate/length(x),   #frequency (speed),
                        strength = sapply(x, distance.center),          #strength (size),
                        delay = sapply(x, angle))                       #and delay (starting point)
  features
}

features.fft1 = features.fft(fft1)                                      #prints the created features from fft of sensor 1
features.fft2 = features.fft(fft2)                                      #prints the created features from fft of sensor 2

head(features.fft1)
head(features.fft2)

