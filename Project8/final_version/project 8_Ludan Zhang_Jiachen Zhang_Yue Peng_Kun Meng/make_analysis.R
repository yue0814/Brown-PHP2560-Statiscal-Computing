# make_analysis.R

#load("MOVIE.rda")
library(dplyr)
dbdir <- '/Users/ludanzhang/Documents/brown/statistical computing I/project 8_Ludan Zhang_Jiachen Zhang_Yue Peng_Kun Meng/DBLite'
con <- dbConnect(MonetDBLite::MonetDBLite(), dbdir)
query <- "SELECT * FROM movie_data"
dat <- dbGetQuery(con, query)
### regression analysis
x <- dat %>%
  select(aspect_ratio, cast_total_facebook_likes,facenumber_in_poster,
         num_critic_for_reviews, duration, movie_facebook_likes, 
         director_facebook_likes, num_voted_users, actor_1_facebook_likes, 
         actor_2_facebook_likes, actor_3_facebook_likes, budget, gross, imdb_score)
x <- na.omit(x)
var_name <- c("aspect_ratio", "cast_total_facebook_likes", "facenumber_in_poster",
              "num_critic_for_reviews", "duration", "movie_facebook_likes",
              "director_facebook_likes", "num_voted_users", "actor_1_facebook_likes", 
              "actor_2_facebook_likes", "actor_3_facebook_likes", "budget", "gross","imdb_score")
pval <- rep(0, (length(var_name)-1))

for (i in 1:(length(var_name)-1)){
  pval[i] <- summary(lm(paste0("imdb_score~", var_name[i]), data = x))$coefficients[8]
}

sig_var <- var_name[which(pval < 0.001)]  # significant variable

sum_lm <- list(0,0,0,0,0,0,0,0,0,0,0,0,0)
for (i in 1:(length(var_name)-1)){
  sum_lm[[i]] <- summary(lm(paste0("imdb_score~", var_name[i]), data = x))$coefficients
}

for (i in 1:length(sum_lm)){
  print(knitr::kable(as.data.frame(sum_lm[[i]])))
}

# take the significant variables to use them in the following analysis
x <- x %>%
  arrange(imdb_score)
X <- subset(x, select = sig_var)

# Classification by distance analysis
# Both using Euclidean Distance
distance_analysis1 <- function(training1, training2, test = NULL){
  
  dist2 <- function(test){
    mu2 <- colMeans(scale(training2))
    sqrt(rowSums((scale(test)-mu2)^2))
  }
  
  dist1 <- function(test){
    mu1 <- colMeans(scale(training1))
    sqrt(rowSums((scale(test)-mu1)^2))
  }
  
  if (is.null(test) == TRUE) test <- rbind(training1, training2) 
  
  nx <- nrow(test)
  belong <- matrix(rep(0, nx), nrow = 1, byrow = T)
  mu1 <- colMeans(training1)
  S1 <- var(training1)
  w <- dist2(test) - dist1(test)
  for (i in 1:nx){
    if (w[i] > 0){
      belong[i] <- 1 # good movie
    }else{belong[i] <- 2}
  }
  return(belong)
}

# One using Euclidean distance, the other using Mahalanobis distance
distance_analysis <- function(training1, training2, test = NULL){
  
  dist2 <- function(test){
    mu2 <- colMeans(scale(training2))
    sqrt(rowSums((scale(test)-mu2)^2))
  }
  
  if (is.null(test) == TRUE) test <- rbind(training1, training2) 
  
  nx <- nrow(test)
  belong <- matrix(rep(0, nx), nrow = 1, byrow = T)
  mu1 <- colMeans(training1)
  S1 <- var(training1)
  w <- dist2(test) - mahalanobis(test, mu1, S1)
  for (i in 1:nx){
    if (w[i] > 0){
      belong[i] <- 1 # good movie
    }else{belong[i] <- 2}
  }
  return(belong)
}




# pull out the trainning set
# Let half of movies with imdb_score less than 8.0 as trainning set
Y <- as.matrix(X[1:median(which(x$imdb_score<8.0)), ]) 
# Let the movies with imdb_score larger than 8.0 as another trainning set
Z <- as.matrix(X[(max(which(x$imdb_score<8.0))+1):(dim(X)[1]), ])  
# Let the rest of movies with imdb_score less than 8.0 as testing set
test <- as.matrix(X[(median(which(x$imdb_score<8.0))+1):max(which(x$imdb_score<8.0)), ])

# classification by distance analysis
belong <- distance_analysis(Y, Z, test)
belong1 <- distance_analysis1(Y, Z, test)
# the probability of distinguishing not so good moive successfully by each distance analysis 
paste0("When using both euclidean distance, the successful classification probability is ",
       round(sum(belong==2)/nrow(test), digits = 4))
paste0("When using euclidean distance and mahalanobis distance,
       the successful classification probability is ",
       round(sum(belong1==2)/nrow(test), digits = 4))


# Regression analysis for log(year)
y <- as.matrix(table(dat$title_year))
x <-1:length(y)
z <- data.frame(x,y) 
knitr::kable(summary(lm(log(y)~x,z))[[4]])
# strongly significant and R-squared = 92.6%

# 需要嘉琛加注释
data <- dat
nums<-sapply(data, is.numeric)
data=data[nums]
data[data==0]<-NA
data=data[,c(-9,-15)]
data=na.omit(data)

data_clean<-data%>%filter(duration<=200&director_facebook_likes<=20000&actor_3_facebook_likes<=2500&actor_1_facebook_likes<=50000&gross<=3e+08&cast_total_facebook_likes<=80000&num_user_for_reviews<=1500&budget<=2.0e+08&title_year>1980&actor_2_facebook_likes<=5000)

ata_graph1<- data_clean%>%
        filter(title_year>1985)%>%
        select(title_year,num_critic_for_reviews)%>%
        group_by(title_year)%>%summarise(average_critic=mean(num_critic_for_reviews))

data_graph2<-data_clean%>%
        filter(title_year>1989&title_year<=2013)%>%
        select(title_year,num_critic_for_reviews)%>%
        group_by(title_year)%>%summarise(average_critic=mean(num_critic_for_reviews))

lm_data1<-data_graph2%>%filter(title_year>1994)
log_critic<-unlist(log(lm_data1[,2]))
year_critic<-unlist(lm_data1[,1])
knitr::kable(summary(lm(log_critic~year_critic))[[4]])

data_graph3<-data_clean%>%filter(title_year>1985)%>%
        select(title_year,movie_facebook_likes)%>%
        group_by(title_year)%>%summarise(average_like=mean(movie_facebook_likes))

lm_data2<-data_graph3%>%filter(title_year>=2000)
log_fblike<-unlist(log(lm_data2[,2]))
year_fblike<-unlist(lm_data2[,1])
knitr::kable(summary(lm(log_fblike~year_fblike))[[4]])
