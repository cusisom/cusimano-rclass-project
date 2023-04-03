#Subset out the Gentoo Penguin data to take a closer look at the comparison of Adelie and Chinstrap species

d5 <- d3[d3$Species!="Gentoo",]

p <- d5 |> ggplot() +
geom_point(aes(d5$'Body Mass', d5$'Flipper Length', color=Species, fill=Species)) +
labs(
 title="Body Mass by Flipper Length",
 subtitle="Adelie and Chinstrap Penguins",
 x="Body Mass (mm)", y="Flipper Length (mm)",
 color='Species'
 ) +
 scale_color_brewer(palette="Dark2")
 p
 

par(mfrow=c(1,2))
with(d5, boxplot(d5$"Body Mass"))
with(d5, plot(d5$"Body Mass" ~ Species))
  
dat <- cbind(d5, id = 1:length(d5$Species))
yhat <- mean(d5$"Body Mass")
p <- dat %>% ggplot(aes( x = id, y = d5$"Body Mass", group=Species))
p
q <- p + geom_point( size=2) +
 geom_hline( aes(yintercept = mean(d5$"Body Mass")) )+
 geom_segment( data=dat, aes( x=id, y= d5$"Body Mass", xend=id, yend=yhat), color="red", lty=3)
q

spmeans <- dat %>% group_by(Species) %>%
summarise(
sl = mean(d5$"Body Mass"),
n = length(d5$"Body Mass"),
minid = min(id),
maxid = max(id)
)
spmeans
# A tibble: 2 × 5
  Species      sl     n minid maxid
  <fct>     <dbl> <int> <int> <int>
1 Adelie    3711.   219     1   151
2 Chinstrap 3711.   219   152   219

#This seems wrong. Do Adelie and Chinstrap really have the exact same means for Body Mass?
#I have to try and check this by analyzing each species on its own. 

ii <- d3$Species=="Adelie"
d5 <- d3[d3$Species=="Adelie",]

dat <- cbind(d5, id = 1:length(d5$Species))
yhat <- mean(d5$"Body Mass")
p <- dat %>% ggplot( aes(x=id, y=d5$"Body Mass"))
p 
q <- p + geom_point( size=2) +
geom_hline( aes(yintercept = mean(d5$"Body Mass")) )+
 geom_segment( data=dat, aes( x=id, y= d5$"Body Mass", xend=id, yend=yhat), color="red", lty=3)
q
spmeans <- dat %>% group_by(Species) %>%
summarise(
sl = mean(d5$"Body Mass"),
n = length(d5$"Body Mass"),
minid = min(id),
maxid = max(id)
)
spmeans
# A tibble: 1 × 5
  Species    sl     n minid maxid
  <fct>   <dbl> <int> <int> <int>
1 Adelie  3701.   151     1   151

d5 <- d3[d3$Species=="Chinstrap",]

dat <- cbind(d5, id = 1:length(d5$Species))
yhat <- mean(d5$"Body Mass")
p <- dat %>% ggplot( aes(x=id, y=d5$"Body Mass"))
p 
q <- p + geom_point( size=2) +
geom_hline( aes(yintercept = mean(d5$"Body Mass")) )+
 geom_segment( data=dat, aes( x=id, y= d5$"Body Mass", xend=id, yend=yhat), color="red", lty=3)
q
spmeans <- dat %>% group_by(Species) %>%
summarise(
sl = mean(d5$"Body Mass"),
n = length(d5$"Body Mass"),
minid = min(id),
maxid = max(id)
)
spmeans
# A tibble: 1 × 5
  Species      sl     n minid maxid
  <fct>     <dbl> <int> <int> <int>
1 Chinstrap 3733.    68     1    68