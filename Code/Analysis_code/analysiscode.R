

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

#path to data and results 
data_path <- "../../Data/Processed_data/"
results_path <- "../../Results/"
figures_path <- "../../Results/Figures/"
dimorphism_path <- "../../Results/Figures/Dimorphism/"


## ---- functions ----
# function to paste path to output filenames

addpath <- function( filename, path=data_path ) {
    location <- paste( path, filename, sep="")
	return( location )
}

## ---- loaddata ----
# load data. 
dat <- readRDS( addpath("penguins.rds", data_path) )


## ---- summarize ----
# create summary table of the data using skimr to use in paper
# variables, sample size, mean, standard error

sk <- skimr::skim(dat)  # save skim object
sk <- as.data.frame(sk) # save as data.frame
head(sk)  # see the variable names

nrows <- dim(dat)[1] # total number of rows
sk$N <- nrows - sk$n_missing  # sample size of each variable

## ---- summary.table ----
# select only the variable, N, mean, sd, and category counts

sk.table <- sk[c("skim_variable", "N", "numeric.mean", "numeric.sd", "factor.top_counts")]
names(sk.table) <- c("Variable", "N", "Mean", "SE", "Counts") # rename SD as SE
sk.table$SE <- sk.table$SE/sqrt(sk.table$N) # calculate SE

options(knitr.kable.NA = "")
knitr::kable(sk.table, digits=2)


# save summary table
saveRDS(sk.table, file = addpath("summary_table.rds", results_path))


## ---- header ----
######################################
# Data fitting/statistical analysis
######################################

############################
#### First model fit
#
#
# Distribution of Body Mass by Species
#
#
# The following plot shows the distribution of Body Mass across Penguin Species
#
# I Begin with a density plot to show Species distribution of Body Mass
## ---- Dimorphism1 --------
p <- dat %>%
  ggplot( aes(x=dat$'Body Mass', col=Species)) +
  geom_density(linewidth=0.75) +
  labs(
  title="Body Mass by Species",
  x='Body mass (mm)', y='Count',
  color='Species'
 ) +
 scale_color_brewer(palette="Dark2")
p

## ---- saveImage1 --------

#Save Image

ggsave(filename=addpath("Penguins_Body_Mass_~Species.png", dimorphism_path), plot=p)

#I wanted a clearer illustration of the Body Mass in each species. Created a Violin Plot that separated the species along the x-axis and inserted a jitter to show the data points within. 

## ---- Dimorphism2 --------

p <- dat %>%
  ggplot( aes(x=Species, y=dat$'Body Mass', col=Species)) +
  geom_violin(trim=FALSE) +
  geom_jitter() +
  labs(
  title="Body Mass by Species",
  x='Body mass (mm)', y='Count',
  color='Species'
 ) +
 scale_color_brewer(palette="Dark2")

p

## ---- saveImage2 --------
#Save Image 

ggsave(filename=addpath("Penguins_Body_Mass_~Species_violin.png", dimorphism_path), plot=p)

# I like this plot. It illustrates that Adelie and Gentoo penguins have a wider range in Body Mass than do Chinstrap Penguins.
#My question is whether these wider ranges in Body Mass reflect stronger Sexual Size Dimorphism in Adelie and Gentoo Penguins than in Chinstrap Penguins. 

#Create a plot that differentiates sample by sex and species

## ---- Dimorphism3 --------
p <- dat %>%
ggplot(aes(x=Sex, y=dat$'Body Mass', fill=Species)) +
geom_violin(trim=FALSE) +
labs(
title="Body Mass by Species and Sex",
x='Sex', y='Body Mass (g)',
color='Species'
) +
scale_fill_brewer(palette="Dark2")
p

## ---- subsetting1 --------
#The NA values add an extra variable that complicates the image. Subset out NA's.
d1 <- dat[ !is.na(dat$"Sex"), ]

#Redo same plot with subset data

## ---- Dimorphism4 ---------

p <- d1 %>%
ggplot(aes(x=Sex, y=d1$'Body Mass', fill=Species)) +
geom_violin(trim=FALSE) +
labs(
title="Body Mass by Species and Sex",
x='Sex', y='Body Mass (g)',
color='Species'
) +
scale_fill_brewer(palette="Dark2")
p

# Plot shows clear dimorphism with Gentoo Penguins. It also illustrates stronger dimorphism in Adelie than Chinstrap populations but it is more subtle. 

## ---- SaveImage3 --------
#Save Image
ggsave(filename=addpath("Penguins_Body_Mass_Dimorphism_violin.png", dimorphism_path), plot=p)
#

#I need to represent this Sexual Size Dimorphism statistically.
#This can be done by menas of a Welch's Two Sample T-test
# I run the test for each species


# For the Adelie Penguins I first subset the species and remove NA values

## ---- Adelie T-test1 --------

#Subset the Species
d1 <- dat[dat$Species=="Adelie", ]
#Remove the NA values
d1 <- d1[ !is.na(d1$"Sex"), ]

## ---- comment1 --------

#To test the means of each population (Male and Female) I need to filter the population into individual vectors

## ---- mass_species_aov ----
# fit linear model using mass as outcome, species as predictor

lm.fit.s <- lm(`Body Mass` ~ Species, dat)  
anova.table.s <- anova(lm.fit.s)

# print to screen the anova table
print(anova.table.s)

# save anova table to file in Results folder  
saveRDS(anova.table.s, file = addpath("mass_species_anova.rds", results_path))