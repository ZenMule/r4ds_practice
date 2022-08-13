library(tidyverse)
library(MASS)
library(lubridate)
library(hms)
library(assertthat)

x <- c(1:10, Inf)
rescale01 <- function(x) {
  rng <- range(x, na.rm = T, finite = T)
  (x- rng[1]) / (rng[2]- rng[1])
}
rescale01(x)

prop_na <- function(x) {
  mean(is.na(x))
}

propx <- function(x) {
  x / sum(x, na.rm = T)
}

sdmn <- function(x) {
  sd(x, na.rm = T) / mean(x, na.rm = T) 
}

x <- c(1, 2, 3, 4, 5, 6, 7)
prop_na(x)
propx(x)
sdmn(x)

std_err <- function(x) {
  sqrt(var(x)/length(x))
}

std_err(x, na.rm = T)

variance <- function(x) {
  n <- length(x)
  m <- mean(x)
  (1/(n - 1)) * sum((x - m)^2)
}
variance(x)
var(x)

neg <- "un"
x <- "ungood"
substr("ungood", 1, nchar("un")) == prefix

prefixed <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
}
prefixed(x, neg)

x <- 1
y <- c(1, 2)
z <- c(1:10)
remove_last <- function(x) {
  if (length(x) <= 1) 
    return (NULL)
  else 
    x[-length(x)]
}
remove_last(z)

?rep
x <- (1:3)
y <- (1:4)
f3 <- function(x, y) {
  rep(y, length.out = length(x))
}
f3(x, y)

if (condition) {
} else {
}

near(2, (sqrt(2))^2)

?ifelse

now()

greet <- function(hr) {
  hr = hour(now())
  if (hr < 12 && hr > 5) {
    "Good Morning!"
  } else if (hr < 17) {
    "Good Afternoon!"
  } else if (hr < 22) {
    "Good Evening"
  } else if (hr >= 22) {
    "Good Night!"
  } else {
    "..."
  }
}
greet()

summer_greet <- function(x) {
  #First check if the object is a Date
  if (is.date(x)) { #if the object is not a Date
    m = month(x)
    if (m > 5 && m < 9) {
      "Have a good summer!!!" 
    } 
    else {
      "Go write your paper!!!"
    }
  } 
  else { #if the object is a Date
    m = ymd(x) %>% month()
    if (m > 5 && m < 9) {
    "Have a good summer!!!" 
      } 
    else {
    "Go write your paper!!!"
    }
  }
}

summer_greet(today())
summer_greet("20180605")

fizzbuzz <- function(x) {
  if (x %% 3 == 0 && x %% 5 != 0) {
    "fizz"
  } else if (x %% 5 == 0 && x %% 3 != 0 ) {
    "buzz"
  } else if (x %% 3 == 0 && x %% 5 == 0) {
    "fizzbuzz"
  } else {
    x
  }
}

?cut
