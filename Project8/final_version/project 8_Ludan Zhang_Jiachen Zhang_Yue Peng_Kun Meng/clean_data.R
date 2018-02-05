## STEP1. Download and save the data

#pre-set before download
library(stringr)
library(rvest)
library(RCurl)
library(XML)
install.packages('pacman')
pacman::p_load(RCurl, foreign, downloader, survey, srvyr, ggplot2, dplyr)

source_url("https://raw.githubusercontent.com/ajdamico/asdfree/master/Download%20Cache/download%20cache.R", prompt=F, echo=F)

setwd("/Users/ludanzhang/Documents/brown/statistical computing I/project 8_Ludan Zhang_Jiachen Zhang_Yue Peng_Kun Meng")
list <- list(NULL)

#download the data from internet
tf <- tempfile(); td <- tempdir()
xpt <- paste("https://kaggle2.blob.core.windows.net/datasets/138/287/imdb-5000-movie-dataset.zip?sv=2015-12-11&sr=b&sig=Bb9Xnax70SbNFQsV%2BJqJhjajCWeSdPTuoAG4g9No5xo%3D&se=2016-12-02T20%3A16%3A00Z&sp=r")
download_cached(xpt, tf, mode='wb')
local.fn <- unzip(tf, exdir = td)

######NOTE:The link for the data may change. If you can't get the data from the link, please use:
######local.fn <- unzip("imdb-5000-movie-dataset.zip")



#store the data
movie_dat <- read.csv(local.fn[[1]])
name <- paste("movie_data.rda")
save(movie_dat, file = paste(name))

# write the data to MonetDBLite
library(DBI)
dbdir <- '/Users/ludanzhang/Documents/brown/statistical computing I/project 8_Ludan Zhang_Jiachen Zhang_Yue Peng_Kun Meng/DBLite'
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

#Complete the missing informaion
#scrap new information from internet

url <- read_html("http://www.the-numbers.com/movie/budgets/all") 
table  <- html_table(html_nodes(url, "table"), fill = TRUE)
table <- data.frame(table)
table <- table[seq(1,dim(table)[1],2),]

#put the new data into the right form
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

# save latest data to the MonetDBLite
dbWriteTable(con, "movie_data",movie_data,overwrite = TRUE)
save(movie_data, file = "/Users/ludanzhang/Documents/brown/statistical computing I/project 8/results/movie.rda")

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

#Seperate keyplot words
kwords <- movie_data$plot_keywords
kwords <- gsub("\\|", ",", kwords)

kword_list <- strsplit(kwords, ",")

get_k1 <- function(i){
        kword_list[[i]][1]
}
get_k2 <- function(i){
        kword_list[[i]][2]
}
get_k3 <- function(i){
        kword_list[[i]][3]
}

get_k4 <- function(i){
        kword_list[[i]][4]
}

get_k5 <- function(i){
        kword_list[[i]][5]
}

x <- 1:length(kword_list)
kword1 <- sapply(x, get_k1)
kword2 <- sapply(x, get_k2)
kword3 <- sapply(x, get_k3)
kword4 <- sapply(x, get_k4)
kword5 <- sapply(x, get_k5)
movie_keywords <- movie_data
movie_keywords$keyword1 <- kword1
movie_keywords$keyword2 <- kword2
movie_keywords$keyword3 <- kword3
movie_keywords$keyword4 <- kword4
movie_keywords$keyword5 <- kword5
movie_keywords <- subset(movie_keywords, select = -plot_keywords)

dbWriteTable(con, "movie_keywords",movie_keywords,overwrite = TRUE)

