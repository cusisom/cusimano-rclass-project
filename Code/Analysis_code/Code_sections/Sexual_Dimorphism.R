

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
dimorphism_path <- "../../Results/Figures/Dimorphism/"
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

#################################################################
#                                                               #
#                    Sexual Dimorphism                          #
#                                                               #
#################################################################


#Run statistical analysis for sexual dimorphism

## ---- Adelie_T-test1 --------

#Subset the Species
d2 <- dat[dat$Species=="Adelie", ]

#Remove the NA values

d2 <- d2[ !is.na(d2$"Sex"), ]

#report data grouping by Sex and Body Mass

Adelie_report <- report_sample(d2, group_by = "Sex", select = "Body Mass")

print(Adelie_report)


## ---- SaveTable --------

saveRDS(Adelie_report, file = addpath("Adelie_Dimorphism_Report.rds", stats_path))

## ---- comment1 --------

# filter the population into individual vectors

## ---- Adelie_T-test2 --------

female <- d2 %>%
 filter(Sex == "FEMALE")
male <- d2 %>%
 filter(Sex == "MALE")

# Run a two sample t-test after filtering the data

t.test.Ad <- t.test(female$'Body Mass', male$'Body Mass') 

print(t.test.Ad)

## ---- SaveTable2 --------

saveRDS(t.test.Ad, file = addpath("Adelie_Dimorphism_ttest.rds", stats_path))

## ---- Gentoo_T-test1 --------

#Subset the Species and remove NA values 

d3 <- dat[dat$Species=="Gentoo", ]
d3 <- d3[ !is.na(d3$"Sex"), ]

#Same process as above

Gentoo_report <- report_sample(d3, group_by = "Sex", select = "Body Mass")

print(Gentoo_report)

## ---- SaveTable3 --------

saveRDS(Gentoo_report, file = addpath("Gentoo_Dimorphism_Report.rds", stats_path))

## ---- Gentoo_T-test2 --------

# Filter the data
female <- d3 %>%
 filter(Sex == "FEMALE")
male <- d3 %>%
 filter(Sex == "MALE")

# Run the analysis

t.test.Gt <- t.test(female$'Body Mass', male$'Body Mass') 

print(t.test.Gt)

## ---- SaveTable4 --------

saveRDS(t.test.Gt, file = addpath("Gentoo_Dimorphism_ttest.rds", stats_path))

## ---- Chinstrap_T-test1 --------

#Subset the Species

d4 <- dat[dat$Species=="Chinstrap", ]

#There is no need to remove NA values for this species
#Check by:

d4$Sex

#Report on the data

Chinstrap_report <- report_sample(d4, group_by = "Sex", select = "Body Mass")

print(Chinstrap_report)

## ---- SaveTable5 --------

saveRDS(Chinstrap_report, file = addpath("Chinstrap_Dimorphism_Report.rds", stats_path))


## ---- Chinstrap_T-test2 --------

#Filter the data

female <- d4 %>%
 filter(Sex == "FEMALE")
male <- d4 %>%
 filter(Sex == "MALE")

# Run the analysis

t.test.Cs <- t.test(female$'Body Mass', male$'Body Mass') 

print(t.test.Cs)

## ---- SaveTable6 --------

saveRDS(t.test.Cs, file = addpath("Chinstrap_Dimorphism_ttest.rds", stats_path))