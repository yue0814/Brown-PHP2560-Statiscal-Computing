## STEP1. Download and save the data

#pre-set before download

install.packages('pacman')
pacman::p_load(RCurl, foreign, downloader, survey, srvyr, ggplot2, dplyr)

source_url("https://raw.githubusercontent.com/ajdamico/asdfree/master/Download%20Cache/download%20cache.R", prompt=F, echo=F)

setwd("/Volumes/Home:lzhang25/Documents/project 8")
list <- list(NULL)

#download the data from internet
tf <- tempfile(); td <- tempdir()
xpt <- paste("https://kaggle2.blob.core.windows.net/datasets/138/287/imdb-5000-movie-dataset.zip?sv=2012-02-12&se=2016-11-05T18%3A43%3A21Z&sr=b&sp=r&sig=c46Bpb0oWCRXsRJ6suX%2BYBRWEbovag84sJM7B8PIqBU%3D")
download_cached(xpt, tf, mode='wb')
local.fn <- unzip(tf, exdir = td)

#store the data
movie_dat <- read.csv(local.fn[[1]])
name <- paste("movie_data.rda")
save(movie_dat, file = paste(name))

# write the data to MonetDBLite
library(DBI)
dbdir <- '/Users/ludanzhang/Documents/brown/statistical computing I/DBLite'
con <- dbConnect(MonetDBLite::MonetDBLite(), dbdir)
dbWriteTable(con, "movie_dat", movie_dat, overwrite = TRUE)

##STEP2: Clean the data

# Remove all the data of TV series
query <- "SELECT * FROM movie_dat where content_rating NOT LIKE 'TV%' "
movie_data <- dbGetQuery(con,query)

# trim whitespace
library(stringr)
movie_data$movie_title <- str_trim(movie_data$movie_title)
dbWriteTable(con,"movie_data",movie_data,overwrite = TRUE)

#Delete the repeat movies
movie_data <- movie_data[-which(duplicated(movie_data$movie_imdb_link, fromLast = TRUE)),]

#Complete the missing data
#scrap new data from internet
library(stringr)
library(rvest)
library(RCurl)
library(XML)
url <- read_html("http://www.the-numbers.com/movie/budgets/all") 
table  <- html_table(html_nodes(url, "table"), fill = TRUE)
table <- data.frame(table)
table <- table[seq(1,dim(table)[1],2),]

#get the new data into the right form
table$Production.Budget <- as.numeric(gsub(",","",str_sub(table$Production.Budget,2,-1)))
table$Worldwide.Gross <- as.numeric(gsub(",","",str_sub(table$Worldwide.Gross,2,-1)))
table$year <- as.numeric(str_sub(table$Release.Date, -4, -1))
table = table[,-1]

#merge the new data into the former data
for (x in table$Movie) {
        if (x %in% movie_data$movie_title) {
                movie_data$gross[which(movie_data$movie_title == x)] <- table$Worldwide.Gross[min(which(table$Movie == x))]
                movie_data$budget[which(movie_data$movie_title == x)] <- table$Production.Budget[min(which(table$Movie == x))]}
}




#set 0 as NA
for (x in names(movie_data)) {
        if (length(which(movie_data[[x]] == 0)) != 0) {
        movie_data[[x]][which(movie_data[[x]] == 0)] <- NA
        }
}

# save latest data to the MonetDBLite
dbWriteTable(con, "movie_data_new",movie_data,overwrite = TRUE)

## STEP3: Rearrange data for further use
#rearrange by genre
movie_data_new <- movie_data
movie_genre <- movie_data_new[1,]
temp_genre <- data.frame()


for (i in 1:dim(movie_data[1])) {
        genre <- data.frame(as.character(unlist(str_split(movie_data$genres[i],"\\|"))))
        n <- dim(genre)[1]
        
        temp_genre <- data.frame(matrix(rep(movie_data_new[i,],n),nrow = n,byrow = TRUE))
        names(temp_genre) <- names(movie_data)
        temp_genre$genres[1:n] <- as.character(genre[,1])
        movie_genre <- rbind(movie_genre,temp_genre)
}

for (i in 1:28) {
        movie_genre[,i] <- unlist(movie_genre[,i])
}

movie_genre <- movie_genre[-1,]
dbWriteTable(con, "movie_genre", movie_genre,overwrite = TRUE)




