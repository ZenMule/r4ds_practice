library(purrr)
library(pryr)

typeof(letters)
typeof(rnorm(100))
typeof(1:10)

x <- list("a", "b", 1:10)
typeof(x)
length(x)
length(letters)

c <- c(T, T, F, NA)
typeof(c)

typeof(1)
typeof(1L)

near((sqrt(2))^2, 2)
x <- c(-1, 0, 1) / 0
is.finite(x)
is.infinite(x)
is.na(x)
is.nan(x)

x <- "This is a reasonably long string."
object_size(x)

dbl_to_integ <- function(x) {
  if (typeof(x) == "double")
    as.integer(x)
}
typeof(dbl_to_integ(2))
typeof(2)

#Coerse to convert: logical(), as.integer(), as.double(), as.character()
?sample
x <- sample(20, 100, replace = T)
sum(x > 10)

#is_*
#"*": 
#is_logical()
#is_integer()
#is_double()
#is_numric()
#is_character()
#is_atomic()
#is_list()
#is_vector()

#subset vectors
x <- c("one", "two", "three", "four", "five")
x[c(3,2,5)]
identical(x[-c(3,2,5)], x[c(-3,-2,-5)])

#subset a vector
x <- c(10, 3, NA, 5, 8, 1, NA)
x[!is.na(x)]
x[x %% 2 == 0 & !is.na(x)]
x[x %% 2 == 0 | !is.na(x)]

#subset a tibble
t <- tibble(a = c(1, 2, 3), b = c(4, 5, 6), c = c(7:9))
filter(t, a == 1)
t[, 1]
t[1, ]
t[1,2]
t[1:2, ]
t[2:3, 2:3]

x <- c(a = 1, b = 5)
x[c("a", "b")]

NaN
NA

identical(1:5, c(1:5))
?is.vector

?setNames
?set_names

last_val <- function(x) {
  if (is.vector(x) && length(x) > 1) {
    x[length(x)] 
    } else {
    "Impossible!"
    }
  }

x <- (1:10)
y <- 1
last_val(x)
last_val(y)

even_num <- function(x) {
  if (n %% 2 == 0) {
    x[n]
  }
}
even_num(x)

even_ind <- function(x) {
  if (length(x)) {
    x[seq_along(x) %% 2 == 0]
  }
}
even_ind(x)
even_ind(letters)

all_but_last <- function(x) {
  if (is.vector(x) && length(x) > 1) {
    x[-length(x)] 
  } else {
    "Impossible!"
  }
}
all_but_last(letters)
all_but_last(x)

evennum_nona <- function(x) {
  if (is.numeric(x)) {
    x[x %% 2 == 0]
    } else {
    "Not a vector"
  }
}
evennum_nona(x)
evennum_nona(letters)

x <- list(1, 2, 3)
str(x)
x_named <- list(a = 1, b = 2, c = 3)
x_named
str(x_named)

y = list("a", 1L, 1.5, T)
str(y)

z = list(list(1,2), list(3,4))
str(z)

a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))
str(a)
str(a[1:2])
str(a[4])
a[["a"]]
a["a"]
?str

x <- factor(c("ab", "cd", "ef"), levels = c("ab", "cd", "ef"))
attributes(x)

hms::hms(3600)
