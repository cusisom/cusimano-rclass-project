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
#By examining the unique objects in Species, it is clear that their are typos to fix
#I funnel rawdata into an easier to operate name 
d1 <- rawdata

#gsub allows me to substitute out the typos for correct spelling in groups
#sub would allow me to do this one-by-one
#I opt for gsub to shorten the length of the code
#I do this in two steps
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
#So I used three operations of grep allowing me to index and alter each species name

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

#I change the names of each variable by using their position in the names vector
#I did this in groups by setting smaller ranges (1-4, 5-10, and 11-17)

names(d1)[1:4] <- c("study", "sample", "Species", "Region")
names(d1)[5:10] <- c("Island", "stage", "ID", "Clutch", "Date", "Culmen Length")
names(d1)[11:17] <- c("Culmen Depth", "Flipper Length", "Body Mass", "Sex", "Delta 15 N", "Delta 13 C", "Comments")

#Here I decided to remove information (columns) that were not useful.
#I do this by subsetting the columns I don't want with the (-) sign. 
d1 <- subset(d1, select = -c(study, ID, sample, stage, Date, Comments, Region, Clutch))

#Here I set up my categorical variables as factors. 

d1$Species <- as.factor(d1$Species)
d1$Sex <- as.factor(d1$Sex)
d1$Island <- as.factor(d1$Island)  
skim(d1)

## ---- comment2 --------

#With the Species names properly edited, it is time to turn to the data.
#

# There is an entry for `Culmen Length` which says "missing" instead of a number or NA. 

## ---- cleandata2 --------
#I funnel Culmen Length into an easier name to operate

cl <- d1$`Culmen Length`
#I'm just showing the head of Culmen Length here to save space. Since the error shows up in first row, the head is all that is needed. 
head(cl)

## ---- cleandata2.2 --------

#I subset the "missing" value and change to NA
cl[ cl == "missing" ] <- NA  
#Since NA is not read as a character string I can now set Culmen Length as a numeric variable
cl <- as.numeric(cl)  
d1$`Culmen Length` <- cl

## ---- cleandata2.3 --------
skim(d1)
hist(d1$`Culmen Length`)
plot(d1$`Body Mass`, d1$`Culmen Length`)

## ---- comment3 --------
#There are two noticable issues with the bivariate plot. When examining both plots, #it is apparent that there are three major outliers in Culmen Length. Additionally, the bivariate plot shows Body Mass outliers as well. I tackled the issues with #Culmen Length first. In handling the problematic data, I am told that the outliers represent a missed decimal point. Thus, my goal is to remedy the data points instead of removing them. 

## ---- cleandata3 --------
d2 <- d1 
#I need to exclude NAs while indexing 
#if I am going to fix the numeric values.
#excluding the NAs is done with !is.na
#the outlier data is over 300 and so I include these values in the subset #(cl>300)
cl[ !is.na(cl) & cl>300 ]
#Since the typo is a missed decimal point, 
#I simply divided the values by 10. 
cl[ !is.na(cl) & cl>300 ] <- cl[ !is.na(cl) & cl>300 ]/10  
#This tells R to replace all numeric values over 300 with those values divided by 10
#Funnel this edit back into the original data
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
#Then subset the data without the NAs and funnel back into the original data
d3 <- d3[ -nas, ] 

hist(d3$`Body Mass`)

plot(d3$`Body Mass`, d3$`Culmen Length`)



## ---- Culmendepth --------
#RUnning through the rest of the variables
hist(d3$'Culmen Depth')

## ---- Culmendepth1 --------

plot(d3$'Body Mass', d3$'Culmen Depth')

## ---- comment6 --------
#This is an interesting distribution
#It doesn't look like an error but I want to check why the data looks this way

## ---- Culmendepth2 --------
#Culmen Depth by Body Mass
#I use ggplot because I know how to use this to highlight the species
require(ggplot2)

p <- d3 |> ggplot() +
  geom_point(aes(x= d3$'Body Mass', y = d3$'Culmen Depth', col=Species))
p


## ---- comment8 --------
#This makes sense. The data groupings reflect different species.
#There does not seem to be any reason for cleaning. 

## ---- Flipperlength --------

#Check distribution
hist(d3$'Flipper Length')
plot(d3$'Body Mass', d3$'Flipper Length')

## ---- comment6 --------
#This data shows a pretty normal scaling of Flipper Length with Body Mass. There is no need to investigate any further. That said, the histogram does show an interesting dip in the median range. This again can be explained by mapping the species distribution for these values. 

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


## ---- Final --------
#Final review of data and edited data dictionary

processeddata <- d3
skim(processeddata)
dictionary <- read.csv(paste("../../Data/Processed_data/datadictionary2.0.csv", sep="")) 
print(dictionary)

save_data_location_csv <- "../../Data/Processed_data/datadictionary2.0.csv"
write.csv(dictionary, file = save_data_location_csv, row.names=FALSE)

save_data_location <- "../../Data/Processed_data/penguins.rds"
saveRDS(processeddata, file = save_data_location)

save_data_location_csv <- "../../Data/Processed_data/penguins.csv"
write.csv(processeddata, file = save_data_location_csv, row.names=FALSE)
