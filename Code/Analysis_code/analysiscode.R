

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
ggplot(aes(x=Species, y=d1$'Body Mass', fill=Sex)) +
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

ggsave(filename=addpath("Penguins_Body_Mass_Dimorphism_violin.png", dimorphism_path), plot=p)



#
#I need to represent this Sexual Size Dimorphism statistically.
#This can be done by menas of a Welch's Two Sample T-test
# I run the test for each species


# For the Adelie Penguins I first subset the species and remove NA values

## ---- Adelie_T-test1 --------

#Subset the Species
d2 <- dat[dat$Species=="Adelie", ]
#Remove the NA values
d2 <- d2[ !is.na(d2$"Sex"), ]

#I want to get a quick glimpse at the difference in Body Mass between Male and Female
#I found that the report package is nice for this


Adelie_report <- report_sample(d2, group_by = "Sex", select = "Body Mass")

print(Adelie_report)

saveRDS(Adelie_report, file = addpath("Adelie_Dimorphism_Report.rds", results_path))

## ---- comment1 --------


#To test the means of each population (Male and Female) I need to filter the population into individual vectors

## ---- Adelie_T-test2 --------

female <- d2 %>%
 filter(Sex == "FEMALE")
male <- d2 %>%
 filter(Sex == "MALE")

# Then I can run the analysis

t.test.Ad <- t.test(female$'Body Mass', male$'Body Mass') 

print(t.test.Ad)
## ---- Save_table2 --------

saveRDS(t.test.Ad, file = addpath("Adelie_Dimorphism_ttest.rds", results_path))

## ---- Gentoo_T-test1 --------

#Subset the Species and remove NA values 

d3 <- dat[dat$Species=="Gentoo", ]
d3 <- d3[ !is.na(d3$"Sex"), ]


Gentoo_report <- report_sample(d3, group_by = "Sex", select = "Body Mass")

print(Gentoo_report)

saveRDS(Gentoo_report, file = addpath("Gentoo_Dimorphism_Report.rds", results_path))

## ---- comment1 --------


#To test the means of each population (Male and Female) I need to filter the population into individual vectors

## ---- Gentoo_T-test2 --------

female <- d3 %>%
 filter(Sex == "FEMALE")
male <- d3 %>%
 filter(Sex == "MALE")

# Then I can run the analysis

t.test.Gt <- t.test(female$'Body Mass', male$'Body Mass') 

print(t.test.Gt)
## ---- Save_table2 --------

saveRDS(t.test.Gt, file = addpath("Gentoo_Dimorphism_ttest.rds", results_path))

## ---- Chinstrap_T-test1 --------

#Subset the Species

d4 <- dat[dat$Species=="Chinstrap", ]

#There is no need to remove NA values for this species
#Check by:

d4$Sex

#I want to get a quick glimpse at the difference in Body Mass between Male and Female
#I found that the report package is nice for this


Chinstrap_report <- report_sample(d4, group_by = "Sex", select = "Body Mass")

print(Chinstrap_report)

saveRDS(Chinstrap_report, file = addpath("Chinstrap_Dimorphism_Report.rds", results_path))

## ---- comment1 --------


#To test the means of each population (Male and Female) I need to filter the population into individual vectors

## ---- Chinstrap_T-test2 --------

female <- d4 %>%
 filter(Sex == "FEMALE")
male <- d4 %>%
 filter(Sex == "MALE")

# Then I can run the analysis

t.test.Cs <- t.test(female$'Body Mass', male$'Body Mass') 

print(t.test.Cs)
## ---- Save_table2 --------

saveRDS(t.test.Cs, file = addpath("Chinstrap_Dimorphism_ttest.rds", results_path))

###############################################
#                                             #
#Question 2 Adelie and Chinstrap Distinction  #
#                                             #
###############################################

# Start by removing Gentoo Penguins from your analysis

## ---- nogentoo --------

#Subset out Gentoo

b1 <- dat[!dat$Species == "Gentoo", ] 

#Drop unused levels from analysis (otherwise Gentoo continues to appear, just with no data)

b1 <- droplevels(b1)

#Run a summary to get a quick look at the data means for each species

 b1 %>%
select(-starts_with("Delta")) %>%
group_by(Species) %>%
report(exclude = "b1$Gentoo") %>%
summary()

#Before I star plotting each variable, I wanted to get a broad graphical look at the interspecific variation in the variables I hadn't yet tested. 

#This task was accomplished by installing the GGally package 

## ---- Ad~Cs1 ---------

require(GGally)

# Using GGally, I established that I wanted to create bivariate plots for all of the variables ending with "th". These include Culmen Length, Culmen Depth, and Flipper Length. 

b1 %>%
  select(Species, ends_with("th")) %>% 
  GGally::ggpairs(aes(color = Species)) +
  scale_color_brewer(palette="Dark2") +
  scale_fill_brewer(palette="Dark2")

