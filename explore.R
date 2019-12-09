library(xml2)
library(rvest)
library(stringr)
library(jsonlite)

setwd('/home/kitlim/wqd7001/ShinyTest/GroupProject/')
profiles <- read.csv('data/data.csv', header=TRUE, sep = ",")
print(profiles)
