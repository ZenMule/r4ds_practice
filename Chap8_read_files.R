library(tidyverse)

#read a file
read.csv("diamonds.csv")
as_tibble(read.csv("diamonds.csv"))

read_csv("a, b, c
         1, 2, 3
         4, 5, 6")

read_csv("The first line of meatadata
         The second line of meatadata
         x, y, z
         1, 2, 3", skip = 2)

read_csv("# comment
  x, y, z
  1, 2, 3", comment = "#")

read_csv("1, 2, 3\n4, 5, 6", col_names = FALSE)           
read_csv("1, 2, 3\n4, 5, 6", col_names = c("x", "y", "z"))

read_csv("a, b, c\n1, 2, .", na = ".")

#parsing a vector
parse_integer(c("1", "231", ".", "456"), na = ".")
x <- parse_integer(c("123", "345", "abc", "123.45"))
problems(x)

#parsing a number
parse_double(1.23)
parse_double("1,23", locale = locale(decimal_mark = ","))

parse_number("$100")
parse_number("20%")
parse_number("It costs $123.45")

parse_number("$123,456,789")
parse_number("123.456.789", locale = locale(grouping_mark = "."))
parse_number("123'456'789", locale = locale(grouping_mark = "'"))

#strings
charToRaw("Hadley")

x1 <- "El Ni\xf1o was particularly bad this year"
x2 <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"
parse_character(x1, locale = locale(encoding = "Latin1"))
parse_character(x2, locale = locale(encoding = "Shift-JIS"))

x3 <- "你高兴就好"
charToRaw(x3)
#[1] e4 bd a0 e9 ab 98 e5 85 b4 e5 b0 b1 e5 a5 bd

guess_encoding(charToRaw(x3))
guess_encoding(charToRaw(x1))
guess_encoding(charToRaw(x2))

#factors
fruit <- c("apple", "banana")
parse_factor(c("apple", "banana", "bananana"), levels = fruit)

#dates, date-time, and times
parse_datetime("2010-10-01T2010")
parse_datetime("20101010")

parse_date("2010-10-01")

library(hms)
parse_time("01:10 am")
parse_time("20:10:01")

parse_date("01/02/15", "%m/%d/%y")
parse_date("01/02/15", "%d/%m/%y")
parse_date("01/02/15", "%y/%m/%d")

parse_date("1 janvier 2015", "%d %B %Y", locale = locale("fr"))

#parse a file
guess_parser("2010-10-01")
guess_parser("15:01")
guess_parser(c("TRUE", "FALSE"))
guess_parser(c("1", "5", "9"))
guess_parser(c("12,352,561"))
str(parse_guess("2010-10-10"))

challenge <- read_csv(readr_example("challenge.csv"))
problems(challenge)

challenge <- read_csv(readr_example("challenge.csv"),
                      col_types = cols(
                        x = col_integer(), 
                        y = col_character()
                        )
                      )
tail(challenge)

challenge <- read_csv(readr_example("challenge.csv"),
                      col_types = cols(
                        x = col_double(),
                        y = col_date()
                      )
)
tail(challenge)                      

#always supplying col_types is highly recommended.

challenge2 <- read_csv(
  readr_example("challenge.csv"), 
                guess_max = 1001
                )
challenge2

challenge2 <- read_csv(readr_example("challenge.csv"),
                       col_types = cols(.default = col_character()))
head(challenge2)

df <- tribble(
  ~x, ~y,
  #-----
  "1", "1.21",
  "2", "2.32",
  "3", "4.56"
)
df
type_convert(df)

#writing to a file
write_csv(challenge, "challenge.csv")
challenge
read_csv("challenge.csv")

#write_csv() will lose the type information
#instead, use write_rds() and read_rds
write_rds(challenge, "challenge.csv")
read_rds("challenge.csv")

library(feather)
write_feather(challenge, "challenge.csv")
read_feather("challenge.csv")
