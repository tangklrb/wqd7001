library(xml2)
library(rvest)
library(stringr)
library(jsonlite)

setwd('/home/kitlim/wqd7001/ShinyTest/GroupProject/')
profiles <- read.csv('data/profiles.csv', header=TRUE, sep = ",")
#print(profiles)


download_picture <- function(photo) {
  prefix <- "https://www.sherdog.com"
  url <- paste0(prefix, photo)
  print(url)
  content <- read_html(url)
  image <- html_nodes(content, ".profile_image") %>% html_attr("src")
  class(photo)
  tryCatch(
    download.file(url=paste0(prefix, image), destfile=paste0(".", photo), method="libcurl", quiet=TRUE, mode="w", cacheOK=T),
    error = function(e) print(paste(photo, ' - Not found'))
  )
  Sys.sleep(1)
}

lapply(profiles$url, download_picture)