
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(DBI)
library(dplyr)
dbdir <- '/Users/ludanzhang/Documents/brown/statistical computing I/project 8_Ludan Zhang_Jiachen Zhang_Yue Peng_Kun Meng/DBLite'
con <- dbConnect(MonetDBLite::MonetDBLite(), dbdir)

setwd("/Users/ludanzhang/Documents/brown/statistical computing I/project 8_Ludan Zhang_Jiachen Zhang_Yue Peng_Kun Meng/results")

### Explore the number of movie: How number of movie have been growing 

#set 0 as NA
movie_data_new <- movie_data
for (x in names(movie_data_new)) {
        if (length(which(movie_data_new[[x]] == 0)) != 0) {
                movie_data_new[[x]][which(movie_data_new[[x]] == 0)] <- NA
        }
}
dbWriteTable(con, "movie_data_new",movie_data_new,overwrite = TRUE)


query <- "SELECT COUNT(*) AS num, title_year FROM movie_data_new WHERE title_year < 2016 GROUP BY title_year"
data <- dbGetQuery(con, query)

num_year <- ggplot(data, aes(x= title_year, y = num)) + geom_line(color = "slategray2",size = 1.5) + geom_smooth(formula = y ~ exp(x/10), color = "indianred2",se = FALSE) + xlab("Year") + ylab("Number of Movies") + ggtitle("The Growth of Number of Movies") + theme(plot.title = element_text(hjust = 0.5))

# save this graph
ggsave(file = "number of movie every year.png",plot = num_year, width = 9,height = 7)

### Rank the movies, directors and actors: top 20 movies with gross, budget, profit and lossess

query <- "SELECT * FROM movie_data_new ORDER BY gross DESC"
movie_data_new <- dbGetQuery(con, query)
data <- movie_data_new[20:1,]

data$movie_title <- factor(paste(data$movie_title,paste0("(",as.character(data$title_year),")")), levels = paste(data$movie_title,paste0("(",as.character(data$title_year),")")))
most_gross <- ggplot(data, aes(x = movie_title, y = gross/(10**9), fill = gross)) + layer(geom = "bar", stat = "identity", position = "identity") + coord_flip() + xlab("Movie") +  theme(legend.position="none") + scale_y_continuous(name = "Worldwide Box Office \n (Billion Dollars)", limits = c(0, 3), breaks = seq(0,3,0.5)) + ggtitle("Top 20 Movie with Largest Box Office")

query <- "SELECT * FROM movie_data_new ORDER BY budget DESC"
movie_data_new <- dbGetQuery(con, query)
data <- movie_data_new[22:3,]

data$movie_title <- factor(paste(data$movie_title,paste0("(",as.character(data$title_year),")")), levels = paste(data$movie_title,paste0("(",as.character(data$title_year),")")))
most_budget <- ggplot(data, aes(x = movie_title, y = budget/(10**9), fill = budget)) + layer(geom = "bar", stat = "identity", position = "identity") + coord_flip() + xlab("Movie") +  theme(legend.position="none") + scale_y_continuous(name = "Production Budget \n (Billion Dollars)", limits = c(0, 0.5), breaks = seq(0,0.5,0.1)) + ggtitle("Top 20 Movie with Largest Budget")

query <- "SELECT * FROM movie_data_new ORDER BY gross - budget DESC "
movie_data_new <- dbGetQuery(con, query)
data <- movie_data_new[20:1,]

data$movie_title <- factor(paste(data$movie_title,paste0("(",as.character(data$title_year),")")), levels = paste(data$movie_title,paste0("(",as.character(data$title_year),")")))
most_profit <- ggplot(data, aes(x = movie_title, y = (gross - budget)/(10**9), fill = (gross-budget))) + layer(geom = "bar", stat = "identity", position = "identity") + coord_flip() + xlab("Movie") +  theme(legend.position="none") + scale_y_continuous(name = "Profit \n (Billion Dollars)", limits = c(0, 2.5), breaks = seq(0,2.5,0.5)) + ggtitle("Top 20 Movie with Biggest Profit")

query <- "SELECT * FROM movie_data_new WHERE gross - budget IS NOT NULL ORDER BY budget - gross DESC "
movie_data_new <- dbGetQuery(con, query)
data <- movie_data_new[22:3,]

data$movie_title <- factor(paste(data$movie_title,paste0("(",as.character(data$title_year),")")), levels = paste(data$movie_title,paste0("(",as.character(data$title_year),")")))

most_losses <- ggplot(data, aes(x = movie_title, y = (budget - gross)/(10**9), fill = (budget - gross))) + layer(geom = "bar", stat = "identity", position = "identity") + coord_flip() + xlab("Movie") +  theme(legend.position="none") + scale_y_continuous(name = "Losses \n (Billion Dollars)", limits = c(0, 0.3), breaks = seq(0,0.3,0.1)) + ggtitle("Top 20 Movie with Biggest Losses")

top_movies <- grid.arrange(most_gross,most_budget,most_profit,most_losses, ncol=2)

#save this plot
ggsave(file = "top movies.png",plot = top_movies, width = 16,height = 9)

### Rank the movies, directors and actors:Most valuable directors and actors

query <- "SELECT director_name, AVG(imdb_score) AS score, SUM(gross-budget) AS profit FROM movie_data_new WHERE gross IS NOT NULL AND budget IS NOT NULL GROUP BY director_name ORDER BY score DESC"
data1 <- dbGetQuery(con,query)
data1 <- data1[30:1,]

query <- "SELECT director_name, AVG(imdb_score) AS score, SUM(gross-budget) AS profit FROM movie_data_new GROUP BY director_name ORDER BY profit DESC"
data2 <- dbGetQuery(con,query)
data2 <- data2[30:1,]

data1$class <- rep("Highest Score",30)
data2$class <- rep("Highest Profit",30)

data <- rbind(data1,data2)

data$label <- rep("",60)
data$class[data$score > 7 & data$profit > 0.5e+9] <- "Highest Profit & Score"
data$label[data$score > 7.2 & data$profit > 0.5e+9] <- data$director_name[data$score > 7.2 & data$profit > 0.5e+9] 
data <- data[!duplicated(data$director_name),]


director_value <- ggplot(data, aes(x = score, y = profit/(10**9), color = class)) + 
        geom_point(size = 2) + scale_color_manual(values = c("gold2","indianred2","skyblue3")) + 
        theme(legend.position = "top") + 
        scale_x_continuous(name = "IMDB Score", limits = c(6.5,9.7), breaks = seq(7,9.5,0.5)) + 
        scale_y_continuous(name = "Total Profit (Billion Dollars)", limits = c(-0.1,7.5), breaks = seq(0,7.5,0.5)) + 
        geom_text_repel(aes(label = label),colour = "gray30", force = 1) + 
        ggtitle("Most Valuable Directors") + theme(plot.title = element_text(hjust = 0.5))

query <- "SELECT actor_1_name AS name, AVG(imdb_score) AS score, SUM(gross -budget) AS profit FROM movie_data_new WHERE gross IS NOT NULL AND budget IS NOT NULL GROUP BY actor_1_name"
actor_1 <- dbGetQuery(con, query)
query <- "SELECT actor_2_name AS name, AVG(imdb_score) AS score, SUM(gross -budget) AS profit FROM movie_data_new WHERE gross IS NOT NULL AND budget IS NOT NULL GROUP BY actor_2_name"
actor_2 <- dbGetQuery(con, query)
query <- "SELECT actor_3_name AS name, AVG(imdb_score) AS score, SUM(gross -budget) AS profit FROM movie_data_new WHERE gross IS NOT NULL AND budget IS NOT NULL GROUP BY actor_3_name"
actor_3 <- dbGetQuery(con, query)

actor <- rbind(actor_1,actor_2,actor_3)
dbWriteTable(con,"actor",actor,overwrite = TRUE)

query <- "SELECT name, AVG(score) AS score, SUM(profit) AS profit FROM actor GROUP BY name ORDER BY score DESC"
data1 <- dbGetQuery(con,query) 
data1 <- data1[30:1,]

query <- "SELECT name, AVG(score) AS score, SUM(profit) AS profit FROM actor GROUP BY name ORDER BY profit DESC"
data2 <- dbGetQuery(con,query) 
data2 <- data2[30:1,]

data1$class <- rep("Highest Score",30)
data2$class <- rep("Highest Profit",30)

data <- rbind(data1,data2)

data$label <- rep("",60)
data$class[data$score > 7 & data$profit > 0.5e+9] <- "Highest Profit & Score"
data$label[data$score > 7 & data$profit > 0.5e+9] <- data$name[data$score > 7 & data$profit > 0.5e+9] 
data <- data[!duplicated(data$name),]

actor_value <- ggplot(data, aes(x = score, y = profit/(10**9), color = class)) + 
        geom_point(size = 2) + scale_color_manual(values = c("gold2","indianred2","skyblue3")) + 
        theme(legend.position = "top") + 
        scale_x_continuous(name = "IMDB Score", limits = c(6,9), breaks = seq(6,9,0.5)) + 
        scale_y_continuous(name = "Total Profit (Billion Dollars)", limits = c(-0.05,7), breaks = seq(0,7,0.5)) + 
        geom_text_repel(aes(label = label),colour = "gray30", force = 1) + 
        ggtitle("Most Valuable Actors") + theme(plot.title = element_text(hjust = 0.5))

director_actor <- grid.arrange(director_value,actor_value,ncol = 2)

#save this graph
ggsave(file = "Most Valuable Directors and Actors.png",plot = director_actor, width = 16,height = 7)

### Explore the movie genres: The weight of different genres among all films
query <- "SELECT COUNT(*) AS count ,genres FROM movie_genre GROUP BY genres"
data <- dbGetQuery(con, query)
data$percentage = round(data$count/sum(data$count)*100,digits = 2)

genre_table <- data[order(data$genres),][,c(2,1,3)]
genres<-data$genres
percentage<-data$percentage
genre_label<-paste(genres," ",percentage,"%")
genre_label[22:26] <- ""
genre_label[6] <- ""
pie(percentage,labels=genre_label,col=rainbow(length(genres)),radius = 1,main="Pie Chart of Genres Weights")

ggsave("weight of genres.png",pie(percentage,labels=genre_label,col=rainbow(length(genres)),radius = 1,main="Pie Chart of Genres Weights")
,width = 10)
### Explore the movie genres: what movie genre earns the most box office and profit

query <- "SELECT genres, gross, gross-budget AS profit FROM movie_genre ORDER BY genres"
data <- dbGetQuery(con,query)
data1 <- data.frame(matrix(1:length(unique(data$genres)),ncol = 1))
for (i in 1:length(unique(data$genres))) {
        x <- unique(data$genres)[i]
        data1$genres[i] <- x
        data1$gross_mean[i] <- mean(data$gross[which(data$genres == x)],na.rm = TRUE)
        data1$gross_se[i] <- sd(data$gross[which(data$genres == x)],na.rm = TRUE)/sqrt(length(which(data$genres == x)))
        data1$profit_mean[i] <- mean(data$profit[which(data$genres == x)],na.rm = TRUE)
        data1$profit_se <- sd(data$profit[which(data$genres == x)],na.rm = TRUE)/sqrt(length(which(data$genres == x)))
}
data <- data1[,2:6]
data <- data[order(data$gross_mean),]
genre_table <- cbind(genre_table,data[order(data$genres),][,c(2,4)])
data$genres <- factor(data$genres,level = data$genres)
limits <- aes(ymax = (gross_mean + gross_se)/(10**9), ymin = (gross_mean - gross_se)/(10**9))

genre_gross <- ggplot(data, aes(x = genres, y = gross_mean/(10**9), fill = genres)) + 
        geom_bar(stat = "identity") +scale_fill_hue(h=c(90, 20)) + 
        geom_errorbar(limits, colour = "black", alpha = 0.5,width = 0.3,size = 1) + 
        coord_flip() + theme(legend.position="none") + xlab("Genres") + 
        scale_y_continuous(name = "Average Gross (Billion Dollars)", limits = c(0, 0.3), breaks = seq(0,0.3,0.05)) + 
        ggtitle("Average Gross Gained by Movie of Difference Genres") + theme(plot.title = element_text(hjust = 0.5))


data <- data[order(data$profit_mean),]
data$genre <- factor(data$genres,level = data$genres)
limits <- aes(ymax = (profit_mean + profit_se)/(10**9), ymin = (profit_mean - profit_se)/(10**9))

genre_profit <- ggplot(data, aes(x = genre, y = profit_mean/(10**9))) + 
        geom_bar(aes(fill = genre),stat = "identity") + 
        geom_errorbar(limits, colour = "black", alpha = 0.5,width = 0.3,size = 1) + 
        coord_flip() + scale_fill_hue(h=c(90, 20))+ theme(legend.position="none") + 
        xlab("Genres") + scale_y_continuous(name = "Average Profit (Billion Dollars)", limits = c(0, 0.2), breaks = seq(0,0.2,0.05)) + 
        ggtitle("Average Profit Gained by Movie of Difference Genres") + theme(plot.title = element_text(hjust = 0.5))

genre_ranking <- grid.arrange(genre_gross,genre_profit,ncol = 2)

#save this graph
ggsave(file = "Genres with Highest Gross and Profit.png",plot = genre_ranking, width = 16,height = 7)

### Explore the movie genres: How gross change regards to the genres
query <- "SELECT gross,genres,title_year from movie_genre WHERE title_year > 2000 ORDER BY title_year"
data <- dbGetQuery(con, query)
genre_change<-data%>%group_by(genres,title_year)%>%summarise(total_gross=sum(gross,na.rm=TRUE)/10^9)
genre_change<-data.frame(genre_change)
genre_change[,2]<-as.character(genre_change[,2])
genre_mean <- genre_change[1:23,]
genre_mean$genres <- unique(genre_change$genres)
genre_mean$title_year <- "Mean gross \n of 15 Years"
for (x in genre_mean$genres) {
        genre_mean$total_gross[genre_mean$genres == x] <-mean(genre_change$total_gross[genre_change$genres %in% x], rm.na = TRUE)
}
genre_change<-rbind(genre_change,genre_mean)
gross_genres<-ggplot(genre_change,aes(title_year,genres,fill=total_gross))+
        geom_raster()+scale_fill_gradient(low="lightgoldenrod2",high="Blue")+
        labs(fill="Total Gross\nfor Each Genre(Billion)")+xlab("Year")+ylab("Genres")+
        ggtitle("Heatmap of Genres' Gross Change") + theme(panel.background = element_blank()) + theme(plot.title = element_text(hjust = 0.5))


#save this graph
ggsave(file = "heat map of genre.png",plot = gross_genres, width = 10, height = 7)


##make a table to summarize important genre information
data <- genre_change[1:340,]
data$mean_gross <- data[,3]*(10**9)
for (x in data$title_year) {
        year <- data[data$title_year == x,]
        genre_table[[x]] <- rep(0,26)
        for (y in data$genres) {
                genre_table[[x]][genre_table$genres == y] <- ifelse(length(year[year$genres == y,3]) == 0 || is.nan(year[year$genres == y,3]),  0, year[year$genres == y,3]) }
}
genre_table <- genre_table[c(1,2,3,6:21,4,5)]
names(genre_table) <- c("Genres","Number of Movies","Weights in All Genres","Mean Gross in 2001",
                        "Mean Gross in 2002","Mean Gross in 2003","Mean Gross in 2004",
                        "Mean Gross in 2005","Mean Gross in 2006","Mean Gross in 2007",
                        "Mean Gross in 2008","Mean Gross in 2009","Mean Gross in 2010",
                        "Mean Gross in 2011","Mean Gross in 2012","Mean Gross in 2013",
                        "Mean Gross in 2014","Mean Gross in 2015","Mean Gross in 2016",
                        "Mean Gross", "Mean Profit"
)
rownames(genre_table) <- NULL

dbWriteTable(con,"genre_table",genre_table,overwrite = TRUE)

##explore the key plot words
library(wordcloud)
query <- "SELECT * FROM movie_keywords"
movie_kword <- dbGetQuery(con,query)
x1 <- movie_kword %>%
        select(keyword1) %>%
        group_by(keyword1) %>%
        summarise(Freq = n())

x2 <-movie_kword %>%
        select(keyword2) %>%
        group_by(keyword2) %>%
        summarise(Freq = n())
x3 <- movie_kword %>%
        select(keyword3) %>%
        group_by(keyword3) %>%
        summarise(Freq = n())
x4 <- movie_kword %>%
        select(keyword4) %>%
        group_by(keyword4) %>%
        summarise(Freq = n())
x5 <- movie_kword %>%
        select(keyword5) %>%
        group_by(keyword5) %>%
        summarise(Freq = n())
names_key <-c(x1[,1], x2[,1], x3[,1], x4[,1], x5[,1])
kw_ana <- unique(unlist(names_key))
kw_ana <- kw_ana[-which(is.na(kw_ana))]
kw_count <- rep(0, length(kw_ana))
for (i in 1:length(kw_ana)){
        if ( kw_ana[i] %in% as.matrix(x1[,1])){
                kw_count[i] <- kw_count[i] + as.numeric(x1[which(as.matrix(x1[,1])==kw_ana[i]), 2])
        }
        if ( kw_ana[i] %in% as.matrix(x2[,1])){
                kw_count[i] <- kw_count[i] + as.numeric(x2[which(as.matrix(x2[,1])==kw_ana[i]), 2])
        }
        if ( kw_ana[i] %in% as.matrix(x3[,1])){
                kw_count[i] <- kw_count[i] + as.numeric(x3[which(as.matrix(x3[,1])==kw_ana[i]), 2])
        }
        if ( kw_ana[i] %in% as.matrix(x4[,1])){
                kw_count[i] <- kw_count[i] + as.numeric(x4[which(as.matrix(x4[,1])==kw_ana[i]), 2])
        }
        if ( kw_ana[i] %in% as.matrix(x5[,1])){
                kw_count[i] <- kw_count[i] + as.numeric(x5[which(as.matrix(x5[,1])==kw_ana[i]), 2])
        }
}

colors <- c("red2", "gold2", "skyblue2")
wordcloud(kw_ana, kw_count, scale = c(4, 0.3), colors = colors,
          max.words = 100, random.order = F)


ggsave("keyplot wordcloud.png",wordcloud(kw_ana, kw_count, scale = c(4, 0.3), colors = colors,
                       max.words = 100, random.order = F,fixed.asp = TRUE))

