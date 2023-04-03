

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

## ---- Break --------

########################################################
#                                                      #
################ Data Summary ##########################
#                                                      #
########################################################



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

## ---- Break --------

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

ggsave(filename=addpath("cor_plots.png", figures_path), plot= cor_plots)

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

ggsave(filename=addpath("penguins_MassV.png", figures_path), plot=p2)

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

ggsave(filename=addpath("Dimorphism_violin.png", figures_path), plot=p4)

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

###########################################################
#                                                         #
#               Adelie and Chinstrap Distinction          #
#                                                         #
###########################################################

# Start by removing Gentoo Penguins from analysis

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

ggsave(filename=addpath("GGally.png", figures_path), plot=c2)
  
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

#Illustrate PC Loadings

loadings(b2.pc)

pcload <- loadings(b2.pc)
print(pcload)

## ---- SaveTable8 --------

saveRDS(pcload, file = addpath("pcload.rds", PC_path))

## ---- Ad~Cs4 ---------
 

#Because Culmen Depth is a negligable variable in this analysis, I focus on pc1 and pc2.
#Plot PC1~PC2

pc1 <- b2.pc$scores[,1]
pc2 <- b2.pc$scores[,2]
pc3 <- b2.pc$scores[,3]
plot(pc2 ~ pc1, col=b1$Species, cex=2, pch=16)


#ANOVA

#Illustrate variance in Culmen Length
#Generate dataset mean value and specific species means

## ---- anovaCL1 --------
#Scale by individual data points by assigning IDs to each.
b1 <- cbind(b1, id = 1:length(b1$Species))

#Establish group mean

yhat <- mean(b1$'Culmen Length')  


## ---- anovaCL2 --------

#This is supposed to differentiate the species means
#CAN'T GET TO WORK

spmeans  <- b1 %>% group_by(Species) %>% 
        summarise(
          sl = mean(b1$'Culmen Length'),
          n = length(id),
          minid = min(id),
          maxid = max(id)
        )

spmeans

#Species means are only representing the entire sample. Can't get it to differentiate.
#Proceed anyways
#Merge the means into the original dataset

## ---- anovaCL3 --------

l <- merge(b1, spmeans)

#Plot data with means
z <- b1 %>% ggplot(aes( x = b1$id, y = b1$'Culmen Length', group=Species)) 
s <- z + geom_point( size=2) + 
  geom_hline( aes(yintercept = mean(b1$'Culmen Length')) ) + 
  geom_segment( data=l, aes( x = l$id, y = l$'Culmen Length', xend = id, yend = sl), color="red", lty = 3)

s  
## ---- SaveImage8 --------

ggsave(filename=addpath("Cul_Len_Var1.png", figures_path), plot=s)

## ---- anovaCL4 --------
#Highlight the species differences in variance from mean

r <- z + geom_point(size=2) +
geom_segment(data=spmeans, aes(x = minid, y = sl, xend = maxid, yend = sl, group=Species)) +
geom_segment( data=l, aes( x = id, y = l$"Culmen Length", xend = id, yend = sl, color=Species), lty = 3) 
r

## ---- SaveImage9 --------

ggsave(filename=addpath("Cul_Len_Var.png", figures_path), plot=r)

## ---- anovaCL5 --------

#Run ANOVA to establish statistical difference
lm.fit.c <- lm(b1$'Culmen Length' ~ b1$Species, data=b1)
anova.table.c <- anova(lm.fit.c)
rownames(anova.table.c) = c("Species", "Residuals")
print(anova.table.c)

## ---- SaveTable9 --------
saveRDS(anova.table.c, file = addpath("anova.table.c", PC_path))

## ---- anovaFL1 --------

#Using the data IDs from above. Didn't use the yhat mean. No need to bring it back. 

#Establish species mean differences
#CAN'T GET TO WORK
spmeans2 <- b1 %>% group_by(b1$Species) %>%
  summarise(
  sl = mean(b1$"Flipper Length"),
  n = length(id),
  minid = min(id),
  maxid = max(id),
  )
spmeans2

#Species means are only representing the entire sample. Can't get it to differentiate.
#Proceed anyways

## ---- anovaFL2 --------

#Merge the means into the original dataset
y <- merge(b1, spmeans2)

## ---- anovaFL3 --------

#Plot data with means.

o <- b1 %>% ggplot(aes( x = b1$id, y = b1$'Flipper Length')) 
t <- o + geom_point( size=2) + 
  geom_hline( aes(yintercept = mean(b1$'Flipper Length')) ) + 
  geom_segment( data=y, aes( x = id, y = y$'Flipper Length', xend = id, yend = sl), color="red", lty = 3)

t  

## ---- SaveImage10 --------

ggsave(filename=addpath("Flipper_Var1.png", figures_path), plot=t)

## ---- anovaFL4 --------

#Highligh the species differences in variance from mean

g <- o + geom_point(size=2) +
geom_segment(data=spmeans2, aes(x = minid, y = sl, xend = maxid, yend = sl)) +
geom_segment( data=y, aes( x = id, y = y$"Flipper Length", xend = id, yend = sl, color=Species), lty = 3) +
labs(
title = "Flipper Variation",
x= "ID", y = "Flipper Length")

g

## ---- SaveImage11 --------

ggsave(filename=addpath("Flipper_Var.png", figures_path), plot=g)

## ---- anovaFL5 --------
#Run anova to establish statistical significance

lm.fit.f <- lm(b1$'Flipper Length' ~ b1$Species, data=d1)
anova.table.f <- anova(lm.fit.f)
rownames(anova.table.f) = c("Species", "Residuals")
print(anova.table.f)


## ---- SaveTable10 --------
saveRDS(anova.table.f, file = addpath("anova.table.f", PC_path))


