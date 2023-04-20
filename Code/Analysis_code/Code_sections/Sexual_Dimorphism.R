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

readttest <- function( x = t.test.Ad ){
  tval <- x$statistic
  pval <- x$p.value
  fmean <- x$estimate[1]
  mmean <- x$estimate[2]
  return(data.frame("Females"= fmean, "Males"= mmean, "T-value" = tval, "P-value"=pval))
}
Ad.table <- readttest()
print(Ad.table)


## ---- SaveTable2 --------

saveRDS(Ad.table, file = addpath("Adelie_Dimorphism_ttest.rds", stats_path))

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
readttest(t.test.Gt)

Gt.table <- readttest(t.test.Gt)
print(Gt.table)


## ---- SaveTable4 --------

saveRDS(Gt.table, file = addpath("Gentoo_Dimorphism_ttest.rds", stats_path))

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
readttest(t.test.Cs)

Cs.table <- readttest(t.test.Cs)
print(Cs.table)


## ---- SaveTable6 --------

saveRDS(Cs.table, file = addpath("Chinstrap_Dimorphism_ttest.rds", stats_path))