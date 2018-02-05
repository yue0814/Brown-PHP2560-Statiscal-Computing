#make table of the genre data
library(DBI)
dbdir <- '/Users/ludanzhang/Documents/brown/statistical computing I/project 8_Ludan Zhang_Jiachen Zhang_Yue Peng_Kun Meng/DBLite'
con <- dbConnect(MonetDBLite::MonetDBLite(), dbdir)

setwd("/Users/ludanzhang/Documents/brown/statistical computing I/project 8_Ludan Zhang_Jiachen Zhang_Yue Peng_Kun Meng/results")

query <- "SELECT * FROM genre_table"
genre_table <- dbGetQuery(con,query)
genre_table[,18:19] <- genre_table[,18:19]/(10**7)
save(genre_table,file = "genre_table.rda")
knitr::kable(genre_table) 

#make table of the whole table
knitr::kable(summary(movie_data)) 