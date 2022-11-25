#############################
#install.packages("tidyverse")
library(tidyverse) #includes dplyr
library(ggplot2)



##############################
##############################
#working directory setup, data import

dir=""
setwd(dir)

gd<-read.table("gov_eff_libdem_1996_2021.txt",sep="\t",header=T)

table(gd$libdem)
table(gd$year)


#data filter
gds<-gd%>%filter(year==1996)


#######################
#One-sample t-test

t.test(gds$v2x_libdem,mu=0.5)

t1<-t.test(gds$v2x_libdem,mu=0.5)
t1$estimate
t1$p.value
abs(t1$statistic)


#######################
#Independent samples t-test
t.test(gds$gov_eff~gds$libdem)

t2<-t.test(gds$gov_eff~gds$libdem)

t2$estimate
t2$estimate[[1]]
t2$p.value
abs(t2$statistic)


#visualization
ggplot(gds,aes(x=gov_eff, fill=factor(libdem))) +
  geom_density(alpha=0.25)+
  ggtitle(paste0("Government effectiveness in liberal democracies and beyond in ",gds$year))+
  theme_classic()





