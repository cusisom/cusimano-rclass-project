

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

## ---- anovaCL4 --------
#Highlight the species differences in variance from mean

r <- z + geom_point(size=2) +
geom_segment(data=spmeans, aes(x = minid, y = sl, xend = maxid, yend = sl, group=Species)) +
geom_segment( data=l, aes( x = id, y = l$"Culmen Length", xend = id, yend = sl, color=Species), lty = 3) 
r

## ---- SaveImage8 --------

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

## ---- anovaFL4 --------

#Highligh the species differences in variance from mean

g <- o + geom_point(size=2) +
geom_segment(data=spmeans2, aes(x = minid, y = sl, xend = maxid, yend = sl)) +
geom_segment( data=y, aes( x = id, y = y$"Flipper Length", xend = id, yend = sl, color=Species), lty = 3) +
labs(
title = "Flipper Variation",
x= "ID", y = "Flipper Length")

g

## ---- SaveImage9 --------

ggsave(filename=addpath("Flipper_Var.png", figures_path), plot=g)

## ---- anovaFL5 --------
#Run anova to establish statistical significance

lm.fit.f <- lm(b1$'Flipper Length' ~ b1$Species, data=b1)
anova.table.f <- anova(lm.fit.f)
rownames(anova.table.f) = c("Species", "Residuals")
print(anova.table.f)


## ---- SaveTable10 --------
saveRDS(anova.table.f, file = addpath("anova.table.f", PC_path))