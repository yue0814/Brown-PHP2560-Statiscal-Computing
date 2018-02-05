# make_analysis.R

load("MOVIE.rda")
library(dplyr)

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
Z <- as.matrix(X[(max(which(x$imdb_score<8.0))+1):4171, ])  
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



#### Keyword and Genre analysis
movie_kword <- dat %>%
  select(keyword1, keyword2, keyword3, keyword4, keyword5) 
movie_gen <- dat %>%
  select(genres1, genres2, genres3, genres4, genres5)

# Genres
x1 <- movie_gen %>%
  select(genres1) %>%
  group_by(genres1) %>%
  summarise(Freq = n())

x2 <-movie_gen %>%
            select(genres2) %>%
            group_by(genres2) %>%
            summarise(Freq = n())
x3 <- movie_gen %>%
            select(genres3) %>%
            group_by(genres3) %>%
            summarise(Freq = n())
x4 <- movie_gen %>%
            select(genres4) %>%
            group_by(genres4) %>%
            summarise(Freq = n())
x5 <- movie_gen %>%
            select(genres5) %>%
            group_by(genres5) %>%
            summarise(Freq = n())
names_gen <-c(x1[,1], x2[,1], x3[,1], x4[,1], x5[,1])
gen_ana <- unique(unlist(names_gen))
gen_ana <- gen_ana[-27]
gen_count <- rep(0, length(gen_ana))
for (i in 1:length(gen_ana)){
  if ( gen_ana[i] %in% as.matrix(x1[,1])){
    gen_count[i] <- gen_count[i] + as.numeric(x1[which(as.matrix(x1[,1])==gen_ana[i]), 2])
  }
  if ( gen_ana[i] %in% as.matrix(x2[,1])){
    gen_count[i] <- gen_count[i] + as.numeric(x2[which(as.matrix(x2[,1])==gen_ana[i]), 2])
  }
  if ( gen_ana[i] %in% as.matrix(x3[,1])){
    gen_count[i] <- gen_count[i] + as.numeric(x3[which(as.matrix(x3[,1])==gen_ana[i]), 2])
  }
  if ( gen_ana[i] %in% as.matrix(x4[,1])){
    gen_count[i] <- gen_count[i] + as.numeric(x4[which(as.matrix(x4[,1])==gen_ana[i]), 2])
  }
  if ( gen_ana[i] %in% as.matrix(x5[,1])){
    gen_count[i] <- gen_count[i] + as.numeric(x5[which(as.matrix(x5[,1])==gen_ana[i]), 2])
  }
}

library(wordcloud)
colors <- c("red2", "gold2", "skyblue2")
wordcloud(gen_ana, gen_count, scale = c(4, 0.6), colors = colors, random.order = F)
        
# Keywords
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

wordcloud(kw_ana, kw_count, scale = c(4, 0.3), colors = colors,
          max.words = 100, random.order = F)

