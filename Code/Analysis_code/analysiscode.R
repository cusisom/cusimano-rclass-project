

###############################
# Penguin analysis script
#
# This script loads the processed, cleaned data, 
# does a simple analysis and saves the results
# to the results folder
###############################

## ---- setup -----
#load needed packages. make sure they are installed.
require(ggplot2) #for plotting
require(magrittr) #for piping
require(knitr) #for formatting output
require(dplyr) 
require(report)
require(patchwork) #for grouping plots together
require(GGally)


#path to data and results 
data_path <- "../../Data/Processed_data/"
results_path <- "../../Results/"
figures_path <- "../../Results/Figures/"
stats_path <- "../../Results/Statistics/"
PC_path <- "../../Results/Statistics/Princomp/"


## ---- functions ----
# function to paste path to output filenames

addpath <- function( filename, path=data_path ) {
    location <- paste( path, filename, sep="")
	return( location )
}

## ---- loaddata ----
# load data. 
dat <- readRDS( addpath("penguins.rds", data_path) )

## ---- Break --------

########################################################
#                                                      #
################ Data Summary ##########################
#                                                      #
########################################################

source("Code_sections/Data_Summary.r")



## ---- Break --------

############################################################
#                                                          #
#          Distribution of Body Mass by Species            #
#                                                          #
############################################################

source("Code_Sections/Body_Mass_Analysis.r")

#################################################################
#                                                               #
#                    Sexual Dimorphism                          #
#                                                               #
#################################################################


source("Code_Sections/Sexual_Dimorphism.R")

###########################################################
#                                                         #
#               Adelie and Chinstrap Distinction          #
#                                                         #
###########################################################

source("Code_Sections/Analysis_of_Variance.r")



