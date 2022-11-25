

####################
install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)
library(ggplot2)


######
####################
#Working directory setup
getwd()
dir="C:/yourpath/yourworkingdirectory"
#change this to your path
#note the forward, rather than backward slashes
setwd(dir)





####################
#Load the dataset - .txt format
#note the explicit specification of header, columns separator, and decimal separator
gdp<-read.table("gdp_2015_2020.txt", header = TRUE, sep = "\t", dec=".")
gdp<-gdp%>%select(-country_name,-gdp_2015)

population<-read.table("population_2015_2020.txt", header = TRUE, sep = "\t", dec=".",na="..")
population<-population%>%select(-country_name,-population_2015)


vdem<-read.table("vdem_2020.txt", header = TRUE, sep = "\t", dec=".")
#https://www.v-dem.net/data/the-v-dem-dataset/country-year-v-dem-core/

###################
#Merge the datasets and create new variables
dat1<-merge(gdp,population,by.x="country_iso3_code",by.y="country_iso3_code",all=F)
dat2<-merge(dat1,vdem,by.x="country_iso3_code",by.y="iso3",all=F)


##################
#keep only complete cases (no NAs/missing values)
dat<-dat2 %>% filter(complete.cases(dat2))


###################
#Create new variables

#continuous variable
dat$gdppc_2020<-dat$gdp_2020/dat$population_2020

#categorical variable
dat$high_income_2020<-NA
dat$high_income_2020[dat$gdppc_2020>12535]<-1
dat$high_income_2020[dat$gdppc_2020<=12535]<-0
#income classification: https://blogs.worldbank.org/opendata/new-world-bank-country-classifications-income-level-2020-2021







####################
#Create a bar chart: count high-income vs. other states
#https://ggplot2.tidyverse.org/reference/geom_bar.html
#alternative without ggplot2 at http://www.statmethods.net/graphs/bar.html

counts <- table(dat$high_income_2020)
barplot(counts, xlab="High income")

ggplot(data=dat, aes(x=high_income_2020))+geom_bar()
sum(dat$high_income_2020)

ggplot(data=dat, aes(x=high_income_2020))+
  geom_bar(fill="orange")+
  theme_minimal()

#bar chart (column chart) in Excel
#https://support.microsoft.com/en-us/office/present-your-data-in-a-column-chart-d89050ba-e6b6-47de-b090-e9ab353c4c00



####################
#Create a histogram
#https://ggplot2.tidyverse.org/reference/geom_histogram.html
#for non-ggplot2 solution, see http://www.statmethods.net/graphs/density.html

hist(dat$population_2020)
hist(log10(dat$population_2020), col="red", main='Histogram of population (log 10)')

ggplot(data=dat, aes(x=population_2020)) +
  geom_histogram(fill="blue")

ggplot(data=dat, aes(x=libdem_2020)) +
  geom_histogram(fill="darkblue",binwidth = 0.05)




###################
#Create a box-plot

boxplot(dat$libdem_2020, main="Liberal democracy", col="gold") 
boxplot(dat$libdem_2020~dat$high_income_2020,main="Liberal democracy separately for high-income and low/middle-income") 

#https://ggplot2.tidyverse.org/reference/geom_boxplot.html
ggplot(data=dat, aes(y=libdem_2020))+
  geom_boxplot()+
  theme_dark()


ggplot(dat, aes(x=factor(high_income_2020), y=libdem_2020))+
  geom_boxplot(fill="blue")+
  ylim(0,1)+
  theme_light()




###################
#Scatter plot

plot(log10(dat$gdppc_2020), dat$libdem)
text(log10(dat$gdppc_2020), dat$libdem,dat$country_iso3_code)


ggplot(data=dat, aes(x=log10(gdppc_2020), y=libdem_2020))+
  geom_point()

ggplot(data=dat, aes(x=log10(gdppc_2020), y=libdem_2020,label=country_iso3_code))+
  geom_text()+
  xlab("GDPpc (log10)")+
  ylab("Liberal democracy")+
  theme_bw()

ggplot(data=dat, aes(x=log10(gdppc_2020), y=libdem_2020))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("GDPpc (log10)")+
  ylab("Liberal democracy")+
  theme_bw()


ggplot(data=dat, aes(x=log10(gdppc_2020), y=libdem_2020))+
  geom_point()+
  geom_smooth(method=loess)+
  xlab("GDPpc (log10)")+
  ylab("Liberal democracy")+
  theme_bw()
#locally estimated scatterplot smoothing


###################
#Bubble plot

ggplot(data=dat, aes(x=log10(gdppc_2020), y=libdem_2020))+
  geom_point(aes(size=population_2020))+
  xlab("GDP (log10)")+
  ylab("Liberal democracy")+
  theme_bw()




