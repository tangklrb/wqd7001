library(xml2)
library(jpeg)
library(rvest)
library(stringr)
library(jsonlite)

#setwd('/home/kitlim/wqd7001/ShinyTest/GroupProject/')
setwd('C:\\Users\\82178\\Desktop\\KitLim Personal PenDrive\\Master in Data Science\\WQD7001 - Priciples of DataScience\\GroupProject\\wqd7001')
profiles <- read.csv('data/profiles.csv', header=TRUE, sep = ",")
#print(profiles)

# scrap fighter profile image from sherdog.com
download_picture <- function(photo) {
  prefix <- "https://www.sherdog.com"
  url <- paste0(prefix, photo)
  print(url)
  content <- read_html(url)
  image <- html_nodes(content, ".profile_image") %>% html_attr("src")
  class(photo)
  tryCatch(
    download.file(url=paste0(prefix, image), destfile=paste0("www", photo), method="libcurl", quiet=TRUE, mode="w", cacheOK=T),
    error = function(e) print(paste(photo, ' - Not found'))
  )
  Sys.sleep(1)
}

#lapply(profiles$url, download_picture)


# find dimension of downloaded profile image
find_dimension <- function(photo) {
  photo <- paste0("C:\\Users\\82178\\Desktop\\KitLim Personal PenDrive\\Master in Data Science\\WQD7001 - Priciples of DataScience\\GroupProject\\wqd7001\\www\\fighter\\", photo)
  print(photo)
  img <- readJPEG(photo) 
  print(dim(img))
}

files <- list.files(path="www/fighter", pattern="*", full.names=F, recursive=FALSE)
#lapply(files, find_dimension)
