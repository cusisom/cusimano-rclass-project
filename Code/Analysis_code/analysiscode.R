setwd("C:/Users/danny/Documents/git/cusimano-rclass-project/Code/Processing_code")



###############################
# processing script
#
## ---- packages --------
#load needed packages. 
require(dplyr) #for data processing/cleaning
require(tidyr) #for data processing/cleaning
require(skimr) #for nice visualization of data 

## ---- loaddata --------
#path to data

data_location <- "../../Data/Processed_data/penguins.csv"
data_path <- "../../Data/Processed_data/"

d3 <- read.csv(data_location, check.names=FALSE)