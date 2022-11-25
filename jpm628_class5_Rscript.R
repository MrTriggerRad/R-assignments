

####################
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("gridExtra")


library(ggplot2)
library(dplyr)
library(gridExtra)



######
####################
#Working directory setup
getwd()
dir=""
#change this to your path
#note the forward, rather than backward slashes
setwd(dir)


###################
#Load the datasets - .txt format

#Milex (SIPRI)
milex<-read.table("milex_1949_2021.txt", header = TRUE, sep = "\t", dec=".")
milex<-milex%>%select(-country_name)

#GDP and population
gdp<-read.table("gdp_2015_2020.txt", header = TRUE, sep = "\t", dec=".")
population<-read.table("population_2015_2020.txt", header = TRUE, sep = "\t", dec=".",na="..")

#Merge
#gdp_population<-merge(gdp,population,by=c("country_iso3_code"),all=T)
gdp_population<-merge(gdp,population,by=c("country_iso3_code","country_name"),all=T)

milex2<-merge(milex,gdp_population,by.x="iso3",by.y="country_iso3_code",all=T)

#New variables
milex2$gdppc_2020<-milex2$gdp_2020/milex2$population_2020
milex2$high_income_2020<-NA
milex2$high_income_2020[milex2$gdppc_2020>12535]<-1
milex2$high_income_2020[milex2$gdppc_2020<=12535]<-0

#Write/export the merged data file
names(milex2)
milex2_export<-milex2%>%select(iso3,milex_usd_2021:high_income_2020)
write.table(milex2_export,"milex_merge.txt",row.names=F,sep="\t",na="")



###################
#Filtering observations

#Keeping non-missing on variable milex_usd_2021
milex2s<-milex2%>%filter(!is.na(milex_usd_2021))
sum(!is.na(milex2$milex_usd_2021))

#base R subset function
milex2s<-subset(milex2, !is.na(milex_usd_2021))


#Retaining only some variables
milex2_vars<-milex2%>%select(iso3,milex_usd_2021)


#Filtering with GDP per capita
milex2s2<-milex2%>%filter(gdppc_2020>20000)%>%select(iso3,milex_usd_2021,gdppc_2020)
#milex2s2<-subset(milex2, gdppc_2020>20000)


#Combine filtering with selection of variables
milex2s3<-milex2%>%filter(!is.na(milex_usd_2021) & gdppc_2020>20000)%>%select(iso3,milex_usd_2021,gdppc_2020)



###################
#Calculating descriptives
mean(milex2$milex_usd_2021)
mean(milex2$milex_usd_2021,na.rm=T)
median(milex2$milex_usd_2021,na.rm=T)

sd(milex2$milex_usd_2021,na.rm=T)
var(milex2$milex_usd_2021,na.rm=T)


#alternative using dplyr
milex2%>%
  summarise(milex_usd_2021_mean=mean(milex_usd_2021,na.rm=T),
            milex_usd_2021_sd=sd(milex_usd_2021,na.rm=T))



###################
#Calculating descriptives by group
milex2_desc_bygroup<-milex2%>%
  filter(!is.na(high_income_2020))%>%
  group_by(high_income_2020)%>%
  summarise(milex_usd_2021_mean=mean(milex_usd_2021,na.rm=T),
            milex_usd_2021_sd=sd(milex_usd_2021,na.rm=T))



###################
#Plotting the variables' distribution and exporting graphs
gdppc_hist<-ggplot(data=milex2, aes(x=gdppc_2020)) +
  geom_histogram(fill="darkblue")+
  theme_bw(base_size = 20)

jpeg("gdppc.jpg",quality=100,width=1000, height=600)
gdppc_hist
dev.off()


milex2$gdppc_2020_log10<-log10(milex2$gdppc_2020)
gdppc_log10_hist<-ggplot(data=milex2, aes(x=gdppc_2020_log10)) +
  geom_histogram(fill="darkblue")+
  theme_bw(base_size = 20)

jpeg("gdppc_gdppclog10.jpg",quality=100,width=1800, height=600)
grid.arrange(gdppc_hist,gdppc_log10_hist,nrow=1)
dev.off()
