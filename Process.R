#Processing data

library(ggplot2)
library(dplyr)
library(gridExtra)
table1<-read.table("Processed_Data.csv", header = TRUE, dec = ".", sep =",")
talbe1<-table1%>%select(-timeset)
