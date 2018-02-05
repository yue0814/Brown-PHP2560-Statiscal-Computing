# get_data.R
# Draw down the IMDB 5000 Movie Dataset From Kaggle

# Write a function to download this data
pacman::p_load(RCurl, foreign, downloader, survey, srvyr, ggplot2, dplyr)
source_url("http://raw.githubusercontent.com/ajdamico/asdfree/master/Download%20Cache/download%20cache.R", prompt=F, echo=F)
setwd("/Users/PY/GoogleDrive/Fall2016/PHP2560/Project8")
list <- list(NULL)

tf <- tempfile(); td <- tempdir()
xpt <- paste("https://kaggle2.blob.core.windows.net/datasets/138/287/imdb-5000-movie-dataset.zip?sv=2012-02-12&se=2016-11-19T19%3A57%3A07Z&sr=b&sp=r&sig=XWazoPea%2Fix3%2FTplL1FuYhh1YjcxhuMZFPuRJHiBy88%3D")
download_cached(xpt, tf, mode='wb')
local.fn <- unzip(tf, exdir = td)

#Write a function to store this data into an rda format. 
movie_dat <- read.csv(local.fn[[1]], stringsAsFactors = FALSE, header = T)
name <- paste("movie_data.rda")
save(movie_dat, file = paste(name))



# Write this data to MonetDBLite
library(MonetDBLite)
library(DBI)
dbfolder <- "/Users/PY/R/DB"
con <- dbConnect(MonetDBLite(), dbfolder)
dbWriteTable(con, "movie_data", movie_dat)

#dbDisconnect(con, shutdown = TRUE)