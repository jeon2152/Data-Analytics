
rm(list=ls())        #Just removing everything first
library(Amelia)      #provides the dataset freetrade
library(mice)        #provides packages for 'multivariate imputation by chained equations'

data("freetrade")                                   #Get the data
my_data=freetrade                                   #Copy the data to my data
my_rescaled_data = freetrade                        #Create a rescaled data template for later use
rescale_col_index=matrix(c(3,5,6,7,9,10),nrow=1)    #Specify The column index that needs to be rescaled
rescaled_info = matrix(0,2,ncol(my_data))           #Create a matrix that can restore the rescale information

#-----------------------------------------------------------------------------------------
#Performing data rescale

for (column_index in (rescale_col_index)){          #Loop through all columns that required rescale
  column_data = my_data[,column_index]              #Copy that column of data
  column_data_complete = column_data[complete.cases(column_data)]     #get rid of NAN first in order to calculate max/min
  maximum=max(column_data_complete)                          #get the max of the column
  minimum=min(column_data_complete)                          #get the min of the column
  my_rescaled_data[,column_index]=(column_data - minimum)/(maximum-minimum)   #rescaled the column from 0 to 1
  rescaled_info[1,column_index] = maximum           #Store the max/min number to the rescale matrix for later mapping back
  rescaled_info[2,column_index] = minimum
}


#----------------------------------------------------------------------------------------

freetradeMICE.imp = mice(my_rescaled_data)     #Multiple imputation with mice, you need to further specify other details as well. 
                                               #After getting the complete data, map back to original scale by the following equation:
                                              # Original = rescaled *(max-min)+min      the max/min data are stored in rescaled_info matrix
                                        

