

##########################################
#setup


library(dplyr)
library(ggplot2)
#install.packages("readxl")
library(readxl)
#install.packages("plotrix")
library(plotrix)


##########################################
#data import and selection (filter)
#rm(list = ls(all.names = TRUE))#for cleaning the space

dir=""
setwd(dir)


dwvs86<-read.table("wvs7_data_q86_nato_confidence.txt",sep="\t", header=T)
dwvs86xlsx<-read_excel("wvs7_data_q86_nato_confidence.xlsx")


names(dwvs86)
ds<-dwvs86%>%filter(iso3=="ARG")


############
#calculation of descriptives and SE
#scale: 4 lowest confidence,1 highest confidence
table(ds$q86)

#sample statistics
mean_q86<-mean(ds$q86)
sd_q86<-sd(ds$q86)

#Can we get a sense of how much out mean (central tendency) estimate would differ with a different sample?
#Yes, we calculate the SE to see how much space for movement of the true population mean, around our estimate, there is

#SE manually
se_q86<-sd(ds$q86)/sqrt(length(ds$q86))

#write own function
stderror<-function(x){sd(x)/sqrt(length(x))}
se_q86<-stderror(ds$q86)

#being lazy, using the fact that someone else already programmed the thing for us
se_q86<-std.error(ds$q86)


############
#get confidence intervals (CIs)
#https://onlinestatbook.com/stat_sim/sampling_dist/index.html

#plot normal distribution to understand where the shares of cases under normal distribution come from
#fixing random choice of numbers for replicability
set.seed(628)

#creating a dataset with a random variable
x<-rnorm(n=1000000)
dnorm<-data.frame(x)

hist(dnorm$x)
ggplot(dnorm, aes(x = x)) + 
  geom_histogram(colour = 1, fill = "white",binwidth = 0.5)+
  scale_x_continuous(breaks = seq(-5, 5, 0.5), lim = c(-5, 5))+
  geom_vline(xintercept=0,color="orange")+
  geom_vline(xintercept=-1.96,color="red")+
  geom_vline(xintercept=1.96,color="red")#+
  #geom_vline(xintercept=-2.58,color="blue")+
  #geom_vline(xintercept=2.58,color="blue")

dnorm_beyond_1_96_sd<-dnorm%>%filter(x>=1.96 | x<=-1.96)
nrow(dnorm_beyond_1_96_sd) #should be approx 50 000
dnorm_within_1_96_sd<-dnorm%>%filter(x<1.96 & x>-1.96)
nrow(dnorm_within_1_96_sd) # should be approx. 950 000
dnorm_beyond_2_58_sd<-dnorm%>%filter(x>=2.58 | x<=-2.58)
nrow(dnorm_beyond_2_58_sd) #should be approx. 10 000

#so we know the shares of cases under normal distribution
#yes, 95% of cases falls within +/- 1.96 SDs from the mean
#analogously, 95% of times taking a sample will result in mean falling within +/- 1.96 SD from the mean
#so calculate CI for 95% of sample means simply as mean +/- 1.96 SD
lowerci_q86<-mean(ds$q86)-1.96*stderror(ds$q86)
upperci_q86<-mean(ds$q86)+1.96*stderror(ds$q86)


#calculate CI for 99% of cases (i.e. +/- 2.58 SD from the mean)
lowerci_q86_99<-mean(ds$q86)-2.58*stderror(ds$q86)
upperci_q86_99<-mean(ds$q86)+2.58*stderror(ds$q86)

#print the key result
mean_q86
lowerci_q86
upperci_q86


#different SE formula with binary variables (values only 0 and 1)
#create a dummy (binary) variable for confidence in NATO
ds$nato_conf<-ifelse(ds$q86<=2,1,0)
table(ds$nato_conf,ds$q86)

mean(ds$nato_conf)
length(ds$nato_conf)

#manually
p<-mean(ds$nato_conf)
n<-length(ds$nato_conf)
sqrt((p*(1-p))/n)


#or define a function
stderror_prop<-function(x){sqrt((mean(x)*(1-mean(x))/length(x)))}
stderror_prop(ds$nato_conf)




######################
#Adjust scale for better interpretation
#change from 4 (lowest) -> 1 (highest) into 0 (lowest) -> 1 (highest)
#returning back to the entire dataset, as we will want to plot more countries

dwvs86$q86r<-((-1*dwvs86$q86)+4)/3
table(dwvs86$q86r)
table(dwvs86$q86,dwvs86$q86r)




######################
#Bar plots with CIs

table(dwvs86$iso3)
#ds<-dwvs86%>%filter(iso3=="CYP")
ds<-dwvs86%>%filter(iso3 %in% c("DEU","NLD","RUS"))


dss<-ds %>%
  group_by(iso3)%>%
  summarise(mean_q86r=mean(q86r),
            sd_q86r=sd(q86r),
            se_q86r=sd(q86r)/sqrt(n()))
dss$iso3<-factor(dss$iso3)

#bar plot with whiskers showing the SEs
ggplot(dss) +
  geom_bar( aes(x=iso3, y=mean_q86r), stat="identity", fill="skyblue", alpha=0.6)+
  geom_errorbar( aes(x=iso3, ymin=mean_q86r-se_q86r, ymax=mean_q86r+se_q86r), width=0.6, colour="orange", alpha=0.9,size=1.3)



#bar plot with whiskers showing the 95% CIs
ggplot(dss) +
  geom_bar( aes(x=iso3, y=mean_q86r), stat="identity", fill="skyblue", alpha=0.6)+
  geom_errorbar( aes(x=iso3, ymin=mean_q86r-1.96*se_q86r, ymax=mean_q86r+1.96*se_q86r), width=0.6, colour="orange", alpha=0.9,size=1.3)+
  #ylim(0,1)+
  theme_bw()

#more on graphing such bar plots with error bars
#https://r-graph-gallery.com/4-barplot-with-error-bar.html


