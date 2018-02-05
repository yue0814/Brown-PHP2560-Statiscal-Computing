# make_graphs.R
library(dplyr)
library(ggplot2)

# Top 10 directors with most movie
directors <- as.data.frame(table(dat$director_name)) 
names(directors)[1] <- "Name"
directors <- directors %>%
  arrange(desc(Freq))

ggplot(directors[1:10, ], aes(x = reorder(factor(Name), Freq), y = Freq, alpha = Freq)) +
  geom_bar(stat = "identity", fill = "dark blue") +
  labs(x = "Directors", y = "Number of Movies") +
  ggtitle("Top 10 directors with most movies") + 
  coord_flip() + 
  theme_minimal()+
  theme_bw()+
  theme(text = element_text(color = "gray20"),
        legend.position = c("top"),
        axis.line.y = element_blank())

#######################################
##Explore the data

##Produce quality graphics that allow you to begin to answer your questions.

# Explore the number of movie: How number of movie have been growing 
query <- "SELECT COUNT(*) AS num, title_year FROM movie_data_new WHERE title_year < 2016 GROUP BY title_year"
data <- dbGetQuery(con, query)

num_year <- ggplot(data, aes(x= title_year, y = num)) + geom_line(color = "slategray2",size = 1.5) + geom_smooth(formula = y ~ exp(x/10), color = "indianred2",se = FALSE) + scale_x_dicrete(name = "Year", limits=c(1910,2020), breaks = 1920:10:2010) + ylab("Number of Movies") + ggtitle("The Growth of Number of Movies")

# Rank the movies, directors and actors: top 20 movies with gross, budget, profit and lossess

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

grid.arrange(most_gross,most_budget,most_profit,most_losses, ncol=2)
```


# # Rank the movies, directors and actors:Most valuable directors and actors

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

library(ggrepel)
director_value <- ggplot(data, aes(x = score, y = profit/(10**9), color = class)) + geom_point(size = 2) + scale_color_manual(values = c("gold2","indianred2","skyblue3")) + theme(legend.position = "top") + scale_x_continuous(name = "IMDB Score", limits = c(6.5,9.7), breaks = seq(7,9.5,0.5)) + scale_y_continuous(name = "Total Profit (Billion Dollars)", limits = c(-0.1,7.5), breaks = seq(0,7.5,0.5)) + geom_text_repel(aes(label = label),colour = "gray30", force = 1) + ggtitle("Most Valuable Directors")

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

actor_value <- ggplot(data, aes(x = score, y = profit/(10**9), color = class)) + geom_point(size = 2) + scale_color_manual(values = c("gold2","indianred2","skyblue3")) + theme(legend.position = "top") + scale_x_continuous(name = "IMDB Score", limits = c(6,9), breaks = seq(6,9,0.5)) + scale_y_continuous(name = "Total Profit (Billion Dollars)", limits = c(-0.05,7), breaks = seq(0,7,0.5)) + geom_text_repel(aes(label = label),colour = "gray30", force = 1) + ggtitle("Most Valuable Actors")

grid.arrange(director_value,actor_value,ncol = 2)


# Explore the movie genres: The weight of different genres among all films
query <- "SELECT COUNT(*) AS count ,genres FROM movie_genre GROUP BY genres"
data <- dbGetQuery(con, query)
data$percentage = round(data$count/sum(data$count)*100,digits = 2)

genres<-data$genres
percentage<-data$percentage
genre_label<-paste(genres," ",percentage,"%")
genre_weight <- pie(percentage,labels=genre_label,col=rainbow(length(genres)),main="Pie Chart of Genres Weights")


# Explore the movie genres: what movie genre earns the most box office and profit

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
data$genres <- factor(data$genres,level = data$genres)
limits <- aes(ymax = (gross_mean + gross_se)/(10**9), ymin = (gross_mean - gross_se)/(10**9))

genre_gross <- ggplot(data, aes(x = genres, y = gross_mean/(10**9), fill = genres)) + geom_bar(stat = "identity") +scale_fill_hue(h=c(90, 20)) + geom_errorbar(limits, colour = "black", alpha = 0.5,width = 0.3,size = 1) + coord_flip() + theme(legend.position="none") + xlab("Genres") + scale_y_continuous(name = "Average Gross (Billion Dollars)", limits = c(0, 0.3), breaks = seq(0,0.3,0.05)) + ggtitle("Average Gross Gained by Movie of Difference Genres")


data <- data[order(data$profit_mean),]
data$genre <- factor(data$genres,level = data$genres)
limits <- aes(ymax = (profit_mean + profit_se)/(10**9), ymin = (profit_mean - profit_se)/(10**9))

genre_profit <- ggplot(data, aes(x = genre, y = profit_mean/(10**9))) + geom_bar(aes(fill = genre),stat = "identity") + geom_errorbar(limits, colour = "black", alpha = 0.5,width = 0.3,size = 1) + coord_flip() + scale_fill_hue(h=c(90, 20))+ theme(legend.position="none") + xlab("Genres") + scale_y_continuous(name = "Average Profit (Billion Dollars)", limits = c(0, 0.2), breaks = seq(0,0.2,0.05)) + ggtitle("Average Profit Gained by Movie of Difference Genres")

grid.arrange(genre_gross,genre_profit,ncol = 2)
```

# Explore the movie genres: How gross change regards to the genres
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
gross_genres<-ggplot(genre_change,aes(title_year,genres,fill=total_gross))+geom_raster()+scale_fill_gradient(low="lightgoldenrod2",high="Blue")+labs(fill="Total Gross\nfor Each Genre(Billion)")+xlab("Year")+ylab("Genres")+ggtitle("Heatmap of Genres' Gross Change") + theme(panel.background = element_blank())




