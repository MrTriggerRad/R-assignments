install.packages("stringr")
library(dplyr)
library(stringr)

td<-read.table("google_news_february2022_sample_en.txt",sep="\t", header=T, quote=c("'",'"'))
tds<-td%>%filter(audience_iso3="AUS")
