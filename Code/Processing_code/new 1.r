
setwd("C:/Users/danny/Documents/git/cusimano-rclass-project/Code/Processing_code")

require(dplyr) #for data processing/cleaning
require(tidyr) #for data processing/cleaning
require(skimr) #for nice visualization of data 

data_location <- "../../Data/Raw_data/penguins_raw_dirty.csv"
data_path <- "../../Data/Raw_data/"

rawdata <- read.csv(data_location, check.names=FALSE)
d1 <- rawdata

## ----cleandata1------

#first task is to fix all typos in Species names
#this will get us to the appropriate amount of unique names under the Species #category (d1$Species)

#I'm doing this in three steps
#The first two steps are group sub functions
#I did this twice instead of once so that I could check my work along the way
#This also reduced the amount of code I had in each line
#This made it easier to read

d1$Species <- gsub("ngufn", "nguin", gsub("eKie", "elie", gsub("gTin", "guin", d1$Species)))


d1$Species <- gsub("Peog", "peng", gsub("Peng", "peng", gsub("Vent", "Gent", gsub("eOg", "eog", d1$Species))))

#My third step was a single sub event
#For some reason, subing a space for the "M" would not work as a part of my group sub.

d1$Species <- sub("AdelieMp", "Adelie p", d1$Species)

#The next task was to shorten the species name to just the common name
#Thus, I wanted to remove each of the binomials as well as the designation as "penguins"
#Again, sub would not work for this task
#So I used three operations of grep

ii <- grep("(Pygoscelis adeliae)", d1$Species)
d1$Species[ii] <- "Adelie"

ii <- grep("(Pygoscelis papua)", d1$Species)
d1$Species[ii] <- "Gentoo"

ii <- grep("(Pygoscelis antarctica)", d1$Species)
d1$Species[ii] <- "Chinstrap"
unique(d1$Species)

## ----cleandata2-----

#We start working with the data here
#Start by viewing a summary

skim(d1)

#There are a couple of things that stand out about Culmen Length
#The histogram preview shows a peculiar pattern
#Further, this variable is listed under character, not numeric
#This becomes an issue if I try to plot the data
#Before doing this, however, I'll take a look at the raw values for Culmen Length
#Let's first make the data easier to work with 
cl <- d1$`Culmen Length (mm)`
cl

#The first thing I notice here is that there is a "missing" value
#Since R does not read "missing" as an absent value, this is the first thig that I need to change
cl[ cl == "missing" ] <- NA
#Check it again
cl
#With that edit out of the way, I can now change these values to "numeric"
cl <- as.numeric(cl)
#Now funnel these changes back into their original designation
d1$`Culmen Length (mm)` <- cl

#Finally, I can start working with the data 
#I can plot a histogram for some insight into the problem, but a bivariate plot with Body Mass is more explicit
hist(d1$'Culmen Length (mm)')

plot(d1$'Body Mass (g)', d1$'Culmen Length (mm)')
#There are clear outliers that should be edited
#We are told to assume that these reflect a typo (misplaced decimal)
#The issue seems to be the three points registering lengths over 400mm
#I first need to isolate these points
#To be conservative, I will identify any values for Culmen Length over 300mm

d2 <- d1 
cl[ cl > 300 ] 
#By default, R also shows me the 'NA' values
#I can be more specific in my indexing
cl[ !is.na(cl) & cl>300 ]
#These three values are missing a decimal point and skewing the data
#I solve this by dividing the values by ten
cl[ !is.na(cl) & cl>300 ] <- cl[ !is.na(cl) & cl>300 ]/10
#I can check to see if this worked by either examining the full dataset (not entirely practical), or by searching for values over 300mm again
cl
cl[ !is.na(cl) & cl>300 ]
#Beautiful. Funnel these edits back into the original designation
d2$`Culmen Length (mm)` <- cl

#Let's check the plot 

plot(d2$`Body Mass (g)`, d2$`Culmen Length (mm)`)

#Looks good
#Kinda...It seems that I appropriately corrected Culmen Length but there are three distinct outliers in Body Mass
#Given that these values seem to hover around zero, I can assume that they reflect a typo.
#Lets check them out
d2$'Body Mass (g)'
#Looks like there are a few in here that range less than 100g
#I can pull them out to see how many
#First task is to make Body Mass easier to work with
d3 <- d2
mm <- d3$'Body Mass (g)'
mm[ mm < 100 ]
#This showed three values under 100g as well as 2 NA's
#I can simply convert the low values to NA and then dispose of them
mm[ mm < 100 ] <- NA
#If I want to see which values are identified as NA, I can ask are by using 'which'
nas <- which( is.na(mm) )
nas
#It looks like there are 5 NA's, Let's delete them
d3 <- d3[ -nas, ]
d3$'Body Mass (g)'
#No more NA's in Body Mass
#I can check both the Histogram and the Bivariate Plot to ensure the edits solve the anomolies in the data
hist(d3$'Body Mass (g)')
plot(d3$'Body Mass (g)', d3$'Culmen Length (mm)')
#Looks good
d3$Species <- as.factor(d3$Species)
d3$Sex <- as.factor(d3$Sex)
d3$Island <- as.factor(d3$Island)  
skimr::skim(d3)