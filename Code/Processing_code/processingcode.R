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

data_location <- "../../Data/Raw_data/penguins_raw_dirty.csv"
data_path <- "../../Data/Raw_data/"

#load data.
# I am using check.names=F because these names have spaces and parentheses
# and I want to preserve the original names.

rawdata <- read.csv(data_location, check.names=FALSE)


## ---- exploredata --------

skim(rawdata)
dictionary <- read.csv(paste(data_path, "datadictionary.csv", sep=""))
print(dictionary)



## ---- cleandata1 --------

unique(rawdata$Species)

## ---- cleandata1.2 --------
d1 <- rawdata
d1$Species <- gsub("ngufn", "nguin", gsub("eKie", "elie", gsub("gTin", "guin", d1$Species)))


d1$Species <- gsub("Peog", "peng", gsub("Peng", "peng", gsub("Vent", "Gent", gsub("eOg", "eog", d1$Species))))

#My third step was a single sub event
#For some reason, subing a space for the "M" would not work as a part of my group sub.

d1$Species <- sub("AdelieMp", "Adelie p", d1$Species)
unique(d1$Species)

## ---- comment1 --------

#The next task was to shorten the species name to just the common name
#Thus, I wanted to remove each of the binomials as well as the designation as "penguins"
#Again, sub would not work for this task
#So I used three operations of grep

## ---- cleandata1.3 --------
ii <- grep("(Pygoscelis adeliae)", d1$Species)
d1$Species[ii] <- "Adelie"

ii <- grep("(Pygoscelis papua)", d1$Species)
d1$Species[ii] <- "Gentoo"

ii <- grep("(Pygoscelis antarctica)", d1$Species)
d1$Species[ii] <- "Chinstrap"
unique(d1$Species)

#I checked this again by repeating the skimr operation
skim(d1)

## ---- dropcolumns --------

names(d1)[1:4] <- c("study", "sample", "Species", "Region")
names(d1)[5:10] <- c("Island", "stage", "ID", "Clutch", "Date", "Culmen Length")
names(d1)[11:17] <- c("Culmen Depth", "Flipper Length", "Body Mass", "Sex", "Delta 15 N", "Delta 13 C", "Comments")

#Here I decided to remove information (columns) that were not useful. 
d1 <- subset(d1, select = -c(study, ID, sample, stage, Date, Comments, Region, Clutch))

d1$Species <- as.factor(d1$Species)
d1$Sex <- as.factor(d1$Sex)
d1$Island <- as.factor(d1$Island)  
skim(d1)

## ---- comment2 --------

#With the Species names properly edited, it is time to turn to the data.
#

# There is an entry for `Culmen Length` which says "missing" instead of a number or NA. 
# Should we delete this record (and all of the variables)?
# This "missing" entry also turned all culmen length entries into characters instead of numeric.
# That conversion to character also means that our summary function isn't very meaningful.
# So let's fix that first.

## ---- cleandata2 --------

cl <- d1$`Culmen Length`
#I'm just showing the head of Culmen Length here to save space. Since the error shows up in first row, the head is all that is needed. 
head(cl)

## ---- cleandata2.2 --------

cl[ cl == "missing" ] <- NA  
cl <- as.numeric(cl)  
d1$`Culmen Length` <- cl

## ---- cleandata2.3 --------
skim(d1)
hist(d1$`Culmen Length`)
plot(d1$`Body Mass`, d1$`Culmen Length`)

## ---- comment3 --------
#There are two noticable issues with the bivariate plot. When examining both plots, #it is apparent that there are three major outliers in Culmen Length. Additionally, #the bivariate plot shows Body Mass outliers as well. I tackled the issues with #Culmen Length first. In handling the problematic data, I am told that the outliers #represent a missed decimal point. Thus, my goal is to remedy the data points #instead of removing them. 

## ---- cleandata3 --------
d2 <- d1 
#I needed to exclude NAs while indexing 
#if I was going to fix the numeric values.
cl[ !is.na(cl) & cl>300 ]
#Since the typo is a missed decimal point, 
#I simply divided the values by 10. 
cl[ !is.na(cl) & cl>300 ] <- cl[ !is.na(cl) & cl>300 ]/10  

d2$`Culmen Length` <- cl


#culmen length values seem ok now
hist(d2$`Culmen Length`)

plot(d2$`Body Mass`, d2$`Culmen Length`)


## ---- comment3 --------
#Body Mass is the next task. Plotting this variable with both a histogram should reveal the outliers. 

## ---- cleandata4.1 --------
hist(d2$`Body Mass`)

## ---- comment4 --------
#Replace all values under 100g with NA and then remove the NAs. 
## ---- cleandata4.2 --------
d3 <- d2
mm <- d3$`Body Mass`

mm[ mm < 100 ] <- NA
#In order to remove the NAs, 
#I first needed to identify their positions. 
nas <- which( is.na(mm) )

d3 <- d3[ -nas, ] 

hist(d3$`Body Mass`)

plot(d3$`Body Mass`, d3$`Culmen Length`)



## ---- Culmendepth --------

hist(d3$'Culmen Depth')

## ---- Culmendepth1 --------

plot(d3$'Body Mass', d3$'Culmen Depth')

## ---- comment6 --------
#This is an interesting distribution
#It doesn't look like an error but I want to check why the data looks this way

## ---- Culmendepth2 --------
#Cumen Depth by Body Mass
require(ggplot2)

p <- d3 |> ggplot() +
  geom_point(aes(x= d3$'Body Mass', y = d3$'Culmen Depth', col=Species))
p

## ---- comment7 --------

#Though not surprising, it is also interesting to see that this distribution correlates with species diversity on different islands. 

## ---- Culmendepth3 --------

p <- d3 |> ggplot() +
  geom_point(aes(x= d3$'Body Mass', y = d3$'Culmen Depth', col=Island))
p

## ---- comment8 --------
#This makes sense. The data groupings reflect different species.
#There does not seem to be any reason for cleaning. 

# Make bivariate plots for any remaining continous data to ensure there are no further
# errors. It is a good check on the distribution of the data as well. 


## ---- Flipperlength --------

#Check distribution
hist(d3$'Flipper Length')
plot(d3$'Body Mass', d3$'Flipper Length')

## ---- comment6 --------
#This data shows a pretty normal scaling of Lipper Length with Body Mass. There is no need to investigate any further. That said, the histogram does show an interesting dip in the median range. This again can be explained by mapping the species distribution for these values. 

## ---- Flipperlength2 --------

p <- d3 |> ggplot() +
  geom_point(aes(x= d3$'Body Mass', y = d3$'Flipper Length', col=Species))
p

## ---- Nitrogen --------

hist(d3$'Delta 15 N')
plot(d3$'Body Mass', d3$'Delta 15 N')

## ---- Nitrogen2 --------

plot(d3$'Species', d3$'Delta 15 N')

## ---- Carbon --------

hist(d3$'Delta 13 C')
plot(d3$'Body Mass', d3$'Delta 13 C')


## ---- Carbon2 --------

p <- d3 |> ggplot() +
  geom_point(aes(x= d3$'Body Mass', y = d3$'Delta 13 C', col=Species))
p

## ---- Final --------
#Final review of data and edited data dictionary

processeddata <- d3
skim(processeddata)
dictionary <- read.csv(paste(data_path, "datadictionary2.0.csv", sep=""))
print(dictionary)




