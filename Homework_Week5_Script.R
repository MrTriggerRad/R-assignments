#Homework Week 5

 #Import Libraries
library(ggplot2)
library(gridExtra)
library(dplyr)

#Milex (SIPRI)
milex<-read.table("milex_1949_2021.txt", header = TRUE, sep = "\t", dec=".")
milex<-milex%>%select(-country_name)

#GDP and population
gdp<-read.table("gdp_2015_2020.txt", header = TRUE, sep = "\t", dec=".")
population<-read.table("population_2015_2020.txt", header = TRUE, sep = "\t", dec=".",na="..")

#Find find how many valid entries milex_usd exist for 1967

sum(!is.na(milex$milex_usd_1967))

#Merge Tables
gdp_population<-merge(gdp,population,by=c("country_iso3_code","country_name"),all=TRUE)
milex2<-merge(milex,gdp_population,by.x="iso3",by.y="country_iso3_code",all=TRUE)

#Q1b
milex2$gdppc_2020<-milex2$gdp_2020/milex2$population_2020
milex2_filter<-milex2%>%filter(milex2$gdppc_2020>10000)
sum(!is.na(milex2_filter$gdppc_2020))

#Q2 set up variables for high income
#lmao, fuck efficiency. Nestled vector if-statements are for losers that get laid.
milex2$high_income_2020<-NA
milex2$high_income_2020[milex2$gdppc_2020>12535]<-1
milex2$high_income_2020[milex2$gdppc_2020<12535]<-0

#Filter out NAs for gdpcc_2020 through high income. A fucking idiot wrote this question
milex2_high_filter2<-milex2%>%filter(!is.na(milex2$high_income_2020))


#Ok, now do shit on milex_usd_1967
mean(milex2_high_filter2$milex_usd_1967,na.rm=TRUE)
median(milex2_high_filter2$milex_usd_1967,na.rm=TRUE)
sd(milex2_high_filter2$milex_usd_1967,na.rm=TRUE)

#This is incredibly fucking confusing and the reason why you are probably losing points. What he ACTUALLY fucking wants from you is to calculate the descriptive stats for year x with regards to high/low income in 2020
milex2_hf_2020<-milex2%>%filter(milex2$high_income_2020 == 1) #sets filter to only high income states
milex2_lf_2020<-milex2%>%filter(milex2$high_income_2020 == 0) #sets filter to only low income states

#2020 milex mean for high income
mean(milex2_hf_2020$milex_usd_1967,na.rm=TRUE)

#2020 milex mean for low income
mean(milex2_lf_2020$milex_usd_1967,na.rm=TRUE)

#sd hf
sd(milex2_hf_2020$milex_usd_1967,na.rm=TRUE)

#sd_lf
sd(milex2_lf_2020$milex_usd_1967,na.rm=TRUE)

