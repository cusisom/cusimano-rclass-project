

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
#We closed out our first project by creating bivariate plots for all of the continuous variables in our dataset.

#Each of the morhpological data was specifically plotted against body mass to develop an idea of how these features correlated with overall size.

#This series of plots is designed to summarize that data. 

## ---- summary_plots --------

x <- dat |> ggplot(aes(dat$'Body Mass', dat$'Culmen Depth', color=Species, fill=Species)) +
 geom_point() +
 geom_smooth(method=lm, se = FALSE) +
labs(
 subtitle="Body Mass by Culmen Depth",
 x='Body mass (mm)', y='Culmen Depth (mm)',
 ) +
  scale_color_brewer(palette="Dark2")

x1 <- dat |> ggplot(aes(dat$'Body Mass', dat$'Culmen Length', color=Species, fill=Species)) +
 geom_point() +
 geom_smooth(method=lm, se = FALSE) +
labs(
 subtitle="Body Mass by Culmen Length",
 x='Body mass (mm)', y='Culmen Length (mm)',
 ) +
  scale_color_brewer(palette="Dark2")

x2 <- dat |> ggplot(aes(dat$'Body Mass', dat$'Flipper Length', color=Species, fill=Species)) +
 geom_point() +
 geom_smooth(method=lm, se = FALSE) +
labs(
 subtitle="Body Mass by Flipper Length",
 x='Body mass (mm)', y='Flipper Length (mm)',
 ) +
  scale_color_brewer(palette="Dark2")

x3 <- ggplot(data = dat, aes(x = dat$'Body Mass')) +
geom_histogram( aes(fill=Species), alpha=.3) +
  labs(
  subtitle="Body Mass",
  x='Body Mass (g)', y='Count',
  color='Species'
 ) 

require(patchwork)

cor_plots <- x + x1 + x2 + x3 + plot_layout(ncol=2, guides = "collect")

cor_plots

ggsave(filename=addpath("cor_plots.png", dimorphism_path), plot= cor_plots)

# The following plot shows the distribution of Body Mass across Penguin Species. 
#
# I Begin with a density plot to show Species distribution of Body Mass.
## ---- Dimorphism1 --------


p2 <- dat %>%
  ggplot( aes(x=Species, y=dat$'Body Mass', col=Species)) +
  geom_violin(trim=FALSE) +
  geom_jitter() +
  labs(
  title="Body Mass by Species",
  x='Species', y='Body Mass (g)',
  color='Species'
 ) +
 scale_color_brewer(palette="Dark2")

p2
ggsave(filename=addpath("penguins_MassV.png", dimorphism_path), plot=p2)

## ---- saveImage2 --------
#Save Image 

ggsave(filename=addpath("Penguins_Body_Mass_~Species_violin.png", dimorphism_path), plot=p2)

# I like this plot. It illustrates that Adelie and Gentoo penguins have a wider range in Body Mass than do Chinstrap Penguins.
#My question is whether these wider ranges in Body Mass reflect stronger Sexual Size Dimorphism in Adelie and Gentoo Penguins than in Chinstrap Penguins. 

#Create a plot that differentiates sample by sex and species

## ---- Dimorphism3 --------
p3 <- dat %>%
ggplot(aes(x=Sex, y=dat$'Body Mass', fill=Species)) +
geom_violin(trim=FALSE) +
labs(
title="Body Mass by Species and Sex",
x='Species', y='Body Mass (g)',
color='Species'
) +
scale_fill_brewer(palette="Dark2")
p3

## ---- subsetting1 --------
#The NA values add an extra variable that complicates the image. Subset out NA's.
d1 <- dat[ !is.na(dat$"Sex"), ]

#Redo same plot with subset data

## ---- Dimorphism4 ---------

p4 <- d1 %>%
ggplot(aes(x=Species, y=d1$'Body Mass', fill=Sex)) +
geom_violin(trim=FALSE) +
labs(
title="Body Mass by Species and Sex",
x='Sex', y='Body Mass (g)',
color='Species'
) +
scale_fill_brewer(palette="Dark2")
p4

# Plot shows clear dimorphism with Gentoo Penguins. It also illustrates stronger dimorphism in Adelie than Chinstrap populations but it is more subtle. 

## ---- SaveImage3 --------

ggsave(filename=addpath("Dimorphism_violin.png", dimorphism_path), plot=p4)

#
#I need to represent this Sexual Size Dimorphism statistically.
#This can be done by means of a Welch's Two Sample T-test
# I run the test for each species
# For the Adelie Penguins I first subset the species and remove NA values

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

#To test the means of each population (Male and Female) I need to filter the population into individual vectors

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

###############################################
#                                             #
#Question 3 Adelie and Chinstrap Distinction  #
#                                             #
###############################################

# Start by removing Gentoo Penguins from your analysis

## ---- nogentoo --------

#Subset out Gentoo

b1 <- dat[!dat$Species == "Gentoo", ] 

#Drop unused levels from analysis (otherwise Gentoo continues to appear, just with no data)

b1 <- droplevels(b1)

#Run a summary to get a quick look at the data means for each species

c1 <- b1 %>%
select(-starts_with("Delta")) %>%
group_by(Species) %>%
report(exclude = "b1$Gentoo") %>%
summary()
c1

## ---- SaveTable6 --------

saveRDS(c1, file = addpath("Ad_Cs_summary_data.rds", stats_path))

## ---- Ad~Cs1 ---------

require(GGally)

# Select the factor and continuous variables to plot. 

c2 <- b1 %>%
  select(Species, ends_with("th")) %>% 
  GGally::ggpairs(aes(color = Species)) +
  scale_color_brewer(palette="Dark2") +
  scale_fill_brewer(palette="Dark2")
c2
  
## ---- SaveImage7 --------

ggsave(filename=addpath("GGally.png", dimorphism_path), plot=c2)
  
## ----Ad~Cs2 --------

#Remove Body Mass by selecting for other variables only.
#Run Principal Components analysis with princomp function. 

b2 <- b1 %>%
select("Culmen Length", "Culmen Depth", "Flipper Length") 
b2.pc <- princomp(b2, scores=T)
summary(b2.pc)

pcsum <- summary(b2.pc)

print(pcsum)

## ---- SaveTable7 --------

saveRDS(pcsum, file = addpath("pcsum.rds", PC_path))

## ---- Ad~Cs3 ---------

#Shows pretty clearly that the most variation is found in pc1 (~77%) with a fairly steep drop with pc2 (~22%) and negligable scores for pc3.

loadings(b2.pc)

pcload <- loadings(b2.pc)
print(pcload)

## ---- SaveTable8 --------

saveRDS(pcload, file = addpath("pcload.rds", PC_path))

## ---- Ad~Cs4 ---------
#PC1 illustrates that most of the variation with the sample is found in Flipper Length. Second to that is the distribution of Culmen Length Values. Importantly, it does not seem from this that Culmen Depth factors into the variation within this population. 

#Because Culmen Depth is a negligable variable in this analysis, I focus on pc1 and pc2.
#The plot shows variation and when demarcating the species, shows distinct groupings.  

pc1 <- b2.pc$scores[,1]
pc2 <- b2.pc$scores[,2]
pc3 <- b2.pc$scores[,3]
plot(pc2 ~ pc1, col=b1$Species, cex=2, pch=16)

##----SaveImage8 --------

ANOVA

