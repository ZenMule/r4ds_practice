library(tidyverse)

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
df
median(df$a)
median(df$b)
median(df$c)
median(df$d)

#where to save the output:
output <- vector("double", ncol(df)) 

#for loop:
for (i in seq_along(df)) { 
  output[[i]] <- median(df[[i]]) #the thing(s) to do
}
#the result:
round(output, 4)

seq_along(df)
df[1]
df[[1]]
df["a"]

mtcars
colmean <- vector("list", ncol(mtcars))
names(colmean) <- names(mtcars)
for (i in seq_along(mtcars)) {
  colmean[[i]] <- mean(mtcars[[i]]) %>%
    round(4) 
}
as_tibble(colmean) %>%
  gather(mpg:carb, key = columns, value = mean) %>%
  as.numeric()

library(nycflights13)
colname <- vector("list", ncol(flights))
names(colname) <- names(flights)
for (i in seq_along(flights)) {
  colname[[i]] <- typeof(flights[[i]])
}
as_tibble(colname) %>%
  gather(year:time_hour, key = columns, value = data_type) %>%
  group_by(data_type) %>%
  count()

iris
unique_iris <- vector("list", ncol(iris))
names(unique_iris) <- names(iris)
for (i in seq_along(iris)) {
  unique_iris[[i]] <- n_distinct(iris[[i]])
}
as_tibble(unique_iris) %>%
  gather(Sepal.Length:Species, key = columns, value = distinct_vals) 

normals <- vector("list", 4)
norm_mean <- c(-10, 0, 10, 100)
for (i in seq_along(norm_mean)) {
  normals[[i]] <- rnorm(10, norm_mean[[i]])
}
normals

out <- ""
for (x in letters) {
  out <- stringr::str_c(out, x)
}
str_c(letters, collapse = "")

x <- sample(100)
sd <- 0
for (i in seq_along(x)) {
  sd <- sd + (x[i] - mean(x)) ^ 2
}
sd <- sqrt(sd / (length(x) - 1))
sd
sd(x)

x <- runif(100)
out <- vector("numeric", length(x))
out[1] <- x[1]
for (i in 2:length(x)) {
  out[i] <- out[i - 1] + x[i]
}
identical(cumsum(x), out)

rescale01 <- function(x) {
  rng <- range(x, na.tm = T)
  (x - rng[1]) / (rng[2] - rng[1])
}
for (i in seq_along(df)) {
  df[[i]] <- rescale01(df[[i]])
}
df

means <- c(0, 1, 2)
out <- vector("list", length(means))
for (i in seq_along(means)) {
  n <- sample(100, 1)
  out[[i]] <- rnorm(n, means[[i]])
}
str(out)


