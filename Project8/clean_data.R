# clean_data.R


#### load dataset from MonetDB   ####
setwd("/Users/PY/GoogleDrive/Fall2016/PHP2560/Project8")
library(MonetDBLite)
library(DBI)
library(dplyr)
dbfolder <- "/Users/PY/R/DB"
con <- dbConnect(MonetDBLite(), dbfolder)
dat <- dbGetQuery(con, "SELECT * FROM movie_data;")

# web scraping
load("/Users/PY/Downloads/table.rda")

#remove the TV show
which_TV <- grep("TV+", dat$content_rating)
dat <- dat[-which_TV,]
  
#change the duplicated movie name by year
dat <- dat[!duplicated(dat$movie_title),]
# change blank into NA
dat[which(dat == "", arr.ind = T)] <- NA

# count the number of NA
na_count <- as.matrix(colSums(is.na(dat)))
colnames(na_count) <- "Count"
na_count


#### clean the data  ####
str(dat)
library(stringr)
# trim whitespace
dat$movie_title <- str_trim(dat$movie_title)
table$Movie <- str_trim(table$Movie)

# change table into numeric type
table$Production.Budget <- as.numeric(gsub("\\D", "", table$Production.Budget))
table$Worldwide.Gross <- as.numeric(gsub("\\D", "", table$Worldwide.Gross))

# deal with the NA in budget
name_na_bud <- dat$movie_title[which(is.na(dat$budget))]
name_na_bud <- name_na_bud[which(name_na_bud %in% table$Movie)]
repl_bud <- table[table$Movie %in% name_na_bud, 3:4]
dup_mov <- unique(repl_bud[duplicated(repl_bud$Movie),1])
# remove the rows where new table cannot tell us the exact budget

repl_bud <- repl_bud[-which(repl_bud$Movie %in% dup_mov),]
name_na_bud <- name_na_bud[which(!name_na_bud %in% dup_mov)]

dat <- dat[-which(dat$movie_title %in% dup_mov),]

dat$budget[which(dat$movie_title %in% name_na_bud)] <- repl_bud$Production.Budget


# deal with the NA in gross

name_na_gro <- dat$movie_title[which(is.na(dat$gross))]
name_na_gro <- name_na_gro[which(name_na_gro %in% table$Movie)]
repl_gro <- table[table$Movie %in% name_na_gro, c(3,6)]
dup_mov_gro <- unique(repl_gro[duplicated(repl_gro$Movie),1])
# remove the rows where new table cannot tell us the exact gross

repl_gro <- repl_gro[-which(repl_gro$Movie %in% dup_mov_gro),]
name_na_gro <- name_na_gro[which(!name_na_gro %in% dup_mov_gro)]

dat <- dat[-which(dat$movie_title %in% dup_mov_gro),]

dat$gross[which(dat$movie_title %in% name_na_gro)] <- repl_gro$Worldwide.Gross

# change 0 to NA in gross column
dat[which(dat$gross ==0), 9] <- NA



# sort by title year
dat <- dat %>%
  arrange(title_year)

# put the movie title into the first column
dat <- cbind(dat$movie_title,subset(dat, select = -movie_title))
names(dat)[1] <- "movie_title"
# separate genres for each movie
genres <- dat$genres
genres <- gsub("\\|", ",", genres)

gen_list <- strsplit(genres, ",")

get_gen1 <- function(i){
  gen_list[[i]][1]
}
get_gen2 <- function(i){
  gen_list[[i]][2]
}
get_gen3 <- function(i){
  gen_list[[i]][3]
}

get_gen4 <- function(i){
  gen_list[[i]][4]
}

get_gen5 <- function(i){
  gen_list[[i]][5]
}

x <- 1:length(gen_list)
genres1 <- sapply(x, get_gen1)
genres2 <- sapply(x, get_gen2)
genres3 <- sapply(x, get_gen3)
genres4 <- sapply(x, get_gen4)
genres5 <- sapply(x, get_gen5)
dat$genres1 <- genres1
dat$genres2 <- genres2
dat$genres3 <- genres3
dat$genres4 <- genres4
dat$genres5 <- genres5
dat <- subset(dat, select = -genres)


# separate keyword for each movie
kwords <- dat$plot_keywords
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
dat$keyword1 <- kword1
dat$keyword2 <- kword2
dat$keyword3 <- kword3
dat$keyword4 <- kword4
dat$keyword5 <- kword5
dat <- subset(dat, select = -plot_keywords)

save(dat, file = "MOVIE.rda")





# Deal with the NA values 
### Web Crawler
library(stringr)
library(rvest)
library(RCurl)
library(XML)
# budget
scrapbudget <- function(x) {
  session <- html_session("https://www.google.com/")
  form <- html_form(session)[[1]]
  input <- paste(x, '', "movie budget")
  filled_form <- set_values(form, q = input)
  
  session1 <- submit_form(session, filled_form)
  
  info <- session1 %>%
    html_nodes("div#center_col div") %>%
    html_text()
  info <- unlist(strsplit(info[2],' '))[1]
  
  budget_pattern <- paste("[0-9]+")
  m <- regexpr(budget_pattern, info)
  budget <- as.numeric(regmatches(info, m))
  if (length(budget) != 0) {
    budget <- budget*1000000
  } else {budget = 0}
  return(budget)
}


movie_name <- as.data.frame(dat[is.na(dat$budget), 1])
movie_name <- lapply(movie_name, str_trim)
budget_whole <- lapply(movie_name[[1]], scrapbudget)
dat[is.na(dat$budget), 21] <- unlist(budget_whole)

# gross
scrapgross <- function(x) {
  session <- html_session("https://www.google.com/")
  form <- html_form(session)[[1]]
  input <- paste(x,'',"box office")
  filled_form <- set_values(form, q = input)
  
  session1 <- submit_form(session,filled_form)
  
  info <- session1 %>%
    html_nodes("div#center_col div") %>%
    html_text()
  info <- info[2]
  
  
  gross_pattern <- paste("[0-9]+\\.?[0-9]")
  m <- regexpr(gross_pattern, info)
  gross <- as.numeric(regmatches(info, m))
  if (length(gross) != 0) {
    gross <- gross*1000000
  } else {gross = 0}
  return(gross)
}  

movie_name <- as.data.frame(dat[is.na(dat$gross), 1])
movie_name <- lapply(movie_name, str_trim)
gross_whole <- lapply(movie_name[[1]],scrapgross)
dat[is.na(dat$gross), 10] <- unlist(gross_whole)





