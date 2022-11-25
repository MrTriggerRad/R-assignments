

##########################################
#setup

#install.packages("stringr")

library(dplyr)
library(stringr)



##########################################
#data import and selection (filter)

dir=""
setwd(dir)

td<-read.table("google_news_february2022_sample_en.txt",sep="\t", header=T, quote=c("'","'"))


tds<-td%>%filter(audience_iso3=="AUS")
#tds<-td
#tds<-td%>%filter(audience_iso3 %in% c("GBR","USA"))


##########################################
#Preparation of text

#lower case all text
tds$ft_lower<-str_to_lower(tds$ft)

#optionally, clean the text by replacing unwanted characters, here e.g. ;
sum(str_count(tds$ft_lower,";"))
tds$ft_lower<-str_replace_all(tds$ft_lower,";","")
sum(str_count(tds$ft_lower,";"))

#but be careful with some characters, because of regular expressions (REGEX)
str_count(tds$ft_lower,"\\.") #simply using a . will not work, . is a special character for regular expressions, needs to be escaped with \\.


#how to extract parts of a string
tds$iso3<-substr(tds$articleid,17,19)
table(tds$iso3)
tds$iso3<-str_to_upper(tds$iso3)


##########################################
#analysis

#count of occurrences of a word
tds$ukraine<-str_count(tds$ft_lower,"ukraine")
sum(tds$ukraine)


#binary (yes/no) count for occurrences of a word
tds$ukraine_bin<-str_detect(tds$ft_lower, "ukraine")
sum(tds$ukraine_bin)


#see what happens if we introduce an error by replacing the string with an incorrect one
tds$ft_lower_error<-str_replace_all(tds$ft_lower,"ukraine","ukraaine")

#count again, but this time nothing will come for ukraine, only for ukraaine
tds$ukraine2<-str_count(tds$ft_lower_error,"ukraine")
sum(tds$ukraine2)
tds$ukraine3<-str_count(tds$ft_lower_error,"ukraaine")
sum(tds$ukraine3)


##########################################
#Optionally, explore further


#more functions in stringr
#https://stringr.tidyverse.org/articles/stringr.html

#use REGEX
#https://stringr.tidyverse.org/articles/regular-expressions.html 
#possible to use regular expressions (REGEX)
#way more flexible, takes care of alternative spellings, word boundaries, etc., but also complex
#not necessarily always exactly the same results as with simple string search, depends
tds$ukraine4<-str_count(tds$ft,"[Uu]kraine")
sum(tds$ukraine4)
tds$ukraine5<-str_count(tds$ft,"[Uu]krain.") #includes e.g. ukrainian
sum(tds$ukraine5)


textstoanalyze<-c("we are the champions, or aren't we",
                  "we paid a fare")

#count occurences of "are"
str_count(textstoanalyze,"are")
str_count(textstoanalyze,"\\bare\\b")

#exact locations of all occurences
str_locate_all(textstoanalyze,"are")
str_locate_all(textstoanalyze,"\\bare\\b")

#other libraries/packages
#sentimentr for sentiment analysis
#quanteda for much more complex analysis, removal of stopwards, wordclouds,scaling of texts
#many others...

