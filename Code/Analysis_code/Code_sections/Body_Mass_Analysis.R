

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

############################################################
#                                                          #
#          Distribution of Body Mass by Species            #
#                                                          #
############################################################

## ---- summary_plots --------

# Generate bivariate plot of Culmen Depth~Body Mass
# Use geom_smooth to add in the correlation line
#Subtitle instead of title for easier small font

x <- dat |> ggplot(aes(dat$'Body Mass', dat$'Culmen Depth', color=Species, fill=Species)) +
 geom_point() +
 geom_smooth(method=lm, se = FALSE) +
labs(
 subtitle="Body Mass by Culmen Depth",
 x='Body mass (mm)', y='Culmen Depth (mm)',
 ) +
  scale_color_brewer(palette="Dark2")

# Generate bivariate plot of Culmen Length~Body Mass

x1 <- dat |> ggplot(aes(dat$'Body Mass', dat$'Culmen Length', color=Species, fill=Species)) +
 geom_point() +
 geom_smooth(method=lm, se = FALSE) +
labs(
 subtitle="Body Mass by Culmen Length",
 x='Body mass (mm)', y='Culmen Length (mm)',
 ) +
  scale_color_brewer(palette="Dark2")

# Generate bivariate plot of Flipper Length~Body Mass

x2 <- dat |> ggplot(aes(dat$'Body Mass', dat$'Flipper Length', color=Species, fill=Species)) +
 geom_point() +
 geom_smooth(method=lm, se = FALSE) +
labs(
 subtitle="Body Mass by Flipper Length",
 x='Body mass (mm)', y='Flipper Length (mm)',
 ) +
  scale_color_brewer(palette="Dark2")

# Generate histogram showing the distribution of Body Mass for each species

x3 <- ggplot(data = dat, aes(x = dat$'Body Mass')) +
geom_histogram( aes(fill=Species), alpha=.3) +
  labs(
  subtitle="Body Mass",
  x='Body Mass (g)', y='Count',
  color='Species'
 ) 

#Show these plots in a single grid.
#Allows them to be considered together.
#Need specific package to create this layout

require(patchwork)

#With patchwork I can use the plus sign (+) to combine the plots in a grid
#Specify the number of column you want
#Use "collect" to gather the common key for the plots and display it independantly instead of with each. 
cor_plots <- x + x1 + x2 + x3 + plot_layout(ncol=2, guides = "collect")

cor_plots

## ---- SaveImage --------

ggsave(filename=addpath("cor_plots.png", dimorphism_path), plot= cor_plots)

#Narrow in on Body Mass distribution 
#Violin plot shows mirrored bell curves and visually accentuates the data range
#Add geom_jitter to illustrate how individual data points align

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

## ---- SaveImage1 --------

#I'm choosing to keep the code for saving images and tables hidden. Not sure if this is okay for the assignment but I think it makes the quarto file cleaner. 

ggsave(filename=addpath("penguins_MassV.png", dimorphism_path), plot=p2)

## ---- saveImage2 --------

ggsave(filename=addpath("Penguins_Body_Mass_~Species_violin.png", dimorphism_path), plot=p2)

#
#Create a plot that differentiates sample by sex and species
# Basic Violin plot 
#First attempt include NA values and added another dimension to plot
#Be sure to remove NA values when plotting data like this

## ---- subsetting1 --------
#The NA values add an extra variable that complicates the image. Subset out NA's.
d1 <- dat[ !is.na(dat$"Sex"), ]

#Plot the data without NA values to see distribution.

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

## ---- SaveImage3 --------

ggsave(filename=addpath("Dimorphism_violin.png", dimorphism_path), plot=p4)