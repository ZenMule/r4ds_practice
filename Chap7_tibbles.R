library(tidyverse)

#coerce into tibbles
as_tibble(iris)

#create a tibble
tibble(
  x = 1:5, 
  y = 1,
  z = x^2 + y
)

tb <- tibble(
  `:)` = "smile",
  ` ` = "space",
  `2000` = "number"
)
tb

#tribble()
tribble(
  ~x, ~y, ~z,
  #----------
  "a", 2, 3.6,
  "b", 1, 8.5
)

tibble(
  a = lubridate::now() + runif(1e3) * 86400,
  b = lubridate::today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, replace = T)
)

#print more
#display all columns with width = Inf
nycflights13::flights %>%
  print(n = 10, width = Inf)
#if more than 20 rows, display only 10 rows, 
options(tibbble.print_max = 20, tibble.print_min = 10)

nycflights13::flights %>% View

#Subsetting
(df <- tibble(
  x = runif(5),
  y = rnorm(5)
)
)

df$x  
df[["x"]]

df %>% .$x
df %>% .[["x"]]

class(as.data.frame(tb))

#exercises p.123
mtcars
as_tibble(mtcars)

df
df1 <- data.frame(abc = 1, xyz = "a")
df1$x
df$x
df1[, "xyz"]
df1[, c("abc", "xyz")]

var <- "mpg"
df[[mpg]]

annoying <- tibble(
  `1` = 1:10,
  `2` = `1`*2 + rnorm(length(`1`))
)
annoying
annoying$`1`

ggplot(annoying, aes(x = `1`, y = `2`)) +
  geom_point()

annoying <- annoying %>%
  mutate(`3` = `2`/`1`)
annoying

annoying %>%
  rename(one = `1`, two = `2`, three = `3`)

1:3
enframe(1:3)
enframe(c(a = 5, b = 7))
tribble(
  ~a, ~b, ~c,
  #---------
  5, 7, 9,
  11, 13, 15
)
