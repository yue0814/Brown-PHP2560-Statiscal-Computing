# Large Scale Exponential Distribution
# 1.a
big.exp.draws.1 <- rexp(1100000)       # lambda = 1
mean(big.exp.draws.1)                  # Mean = 1/1=1
sd(big.exp.draws.1)                    # Stand deviation = sqrt(1/(lambda)^2) = 1

# 1.b
hist(big.exp.draws.1)
fexp <- function(x){
	return(1 - exp(-1 * x))
}
x <- seq(-10, 10, 20/10000)
par(new = TRUE)
plot(fexp(x))

# 1.c

# 1.d
big.exp.draws.1.mat <- matrix(big.exp.draws.1, nrow = 1100, ncol = 1000)

# 1.e
mean(big.exp.draws.1.mat[,371])

# 1.f
col_mean <- apply(big.exp.draws.1.mat, 2, mean)
col_mean
hist(col_mean)

# 1.g
big.exp.draws.1.square <- big.exp.draws.1 ^ 2
mean(big.exp.draws.1.square)


# 2. Characters, Strings and Data Frames
# 1
setwd('/Users/PY/R/PHP2560')
text <- readLines('earthquakes_09_16.html')

# 2
length(text)

# 3.a
text[7]

# 3.b
text[5:6]

# 4
earthquake_data <- text[9:length(text)]
datalist <- strsplit(earthquake_data, ',')
lat <- datalist[[1]][2]
long <- datalist[[1]][3]

for (i in 2:length(datalist)){
	lat <- c(lat,datalist[[i]][2])
	long <- c(long,datalist[[i]][3])
}

lat.long <- data.frame(latitude = as.vector(lat), longtitude = as.vector(long), 
	stringsAsFactors = FALSE)

# 5
date1 <- strsplit(datalist[[1]][1], ' ')[[1]][1]
for (i in 2:length(datalist)) {
	date1 <- c(date1,strsplit(datalist[[i]][1], ' ')[[1]][1])
}

date <- data.frame(date = as.vector(date1), stringsAsFactors = FALSE)

# 6
mag <- datalist[[1]][5]
for (i in 2:length(datalist)) {
	mag <- c(mag, datalist[[i]][5])
}
magnitude <- data.frame( magnitude = as.vector(mag), stringsAsFactors = FALSE)

mags <- regmatches(x = , m = )

mags2 <- rep(NA, length(magnitude[,1]))

for (i in 1:length(magnitude) ){
    l <- length(magnitude[[i]])
	mags2[i] <- magnitude[[i]][l]
}

# 7
id <- datalist[[1]][12]
for (i in 2:length(datalist)) {
	id <- c(id, datalist[[i]][12])
}

ids <- data.frame(ids = as.vector(id), stringsAsFactors = FALSE)

# 8

# 9.a
time1 <- strsplit(datalist[[1]][1], ' ')[[1]][2]
for (i in 2:length(datalist)) {
	time1 <- c(time1,strsplit(datalist[[i]][1], ' ')[[1]][2])
}
time <- as.vector(time1)

# 9.b
time_coll <- strsplit(time, ':')
time_coll1 <- unlist(time_coll)
time_each <- NA
for (i in 1:length(datalist)) {
	time2 <- paste(time_coll1[(3*i-2):(3*i)], collapse = '')
	time_each <- c(time_each,time2)
}
time_each <- time_each[2:6606]

# 9.c
date_coll <- strsplit(date[,1], '/')
date_coll1 <- unlist(date_coll)
date_each <- NA
for (i in 1:length(datalist)) {
	date2 <- paste(date_coll1[(3*i-2):(3*i)], collapse = '')
	date_each <- c(date_each,date2)
}
date_each <- date_each[2:6606]

# 9.d
Datetime <- paste(date_each, time_each, sep = '')
row.names(lat.long) <- Datetime

# 9.f


