library(tidyverse)
library(stringr)
library(stringi)
library(htmlwidgets)

string1 <- "this is a string"
string2 <- 'To put a "quote" inside a string, use single quotes'

x <- "t\u0361sa\u0303 m\u02B2a\u0361\u028A"
x

y = c("one", "two", "three")

str_length(c("a", "R for data science", NA))
str_length(y)

#str_c() concatenate strings
str_c("x", "y", "z")
str_c("x", "y", "z", sep = ", ")
str_c(y, collapse = "|")

x <- c("abc", NA)
str_c("|-", x, "-|")
str_c("|-", str_replace_na(x), "-|")
str_c("prefix-", c("a", "b", "c"), c("d", "e", "f"), "-suffix")
#[1] "prefix-ad-suffix" "prefix-be-suffix" "prefix-cf-suffix"

name <- "Miao"
time_of_day <- "Morning"
birthday <- F
str_c("Good ", time_of_day, " ", name, 
      if (birthday) ", and HAPPY BIRTHDAY", "!")

str_c(c("a", "b", "c"), collapse = ", ")

#access subpart of strings
x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)
str_sub(x, -3, -1)
str_sub("a", 1, 5)
str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1))
x

#find patterns in strings
x <- c("apple", "banana", "pear")
str_view_all(x, "an")
str_view(x, "an")

str_view(x, ".a.")
dot <- "\\."
writeLines(dot)

str_view(c("abc", "a.c", "bef"), "a\\.c")

x <- "a\\b"
writeLines(x)
str_view(x, "\\\\")

x <- c("apple", "banana", "pear")
str_view(x, "^a")
str_view(x, "a$")
#if you begin with power, you end with money

x <- c("apple pie", "apple", "apple cake")
str_view(x, "apple")
str_view(x, "^apple$")

x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "C{2}")
str_view(x, "C{2,}")
str_view(x, "C{2,3}")
str_view(x, "C{2,3}?")
str_view(x, "C[LX]+")
str_view(x, "C[LX]+?")

fruit
str_view(fruit, "(..)\\1", match = T)

#tools
#detect match
words
str_detect(words, "e")
sum(str_detect(words, "^t"))
mean(str_detect(words, "[aeiou]$"))

no_vowels_1 <- !str_detect(words, "[aeiou]")
no_vowels_2 <- str_detect(words, "^[^aeiou]+$")
identical(no_vowels_1, no_vowels_2)

#select elements that match a pattern
words[str_detect(words, "x$")]
str_subset(words, "x$")

df <- tibble(word = words, i = seq_along(word))
df %>% 
  filter(str_detect(words, "x$"))
x <- c("apple", "banana", "pear")
str_count(x, "a")
mean(str_count(words, "[aeiou]"))
df_VC <- df %>%
  mutate(vowels = str_count(word, "[aeiou]"),
         consonants = str_count(word, "[^aeiou]")) %>%
  filter(consonants > 0) %>%
  mutate(ratio_VtoC = vowels/consonants)
summarise(df_VC, mean_ratio = mean(ratio_VtoC))

#extract matches
sentences #needs stringr package
length(sentences)
head(sentences)

colors <- c("red", "orange", "yellow", "green", "blue", "purple")
(color_match <- str_c(colors, collapse = "|"))

has_color <- str_subset(sentences, color_match)
matches <- str_extract(has_color, color_match)
head(matches)
count(group_by(as_tibble(matches), value)) %>%
  ggplot(aes(x = value, y = n)) +
  geom_bar(stat = "identity")

more <- sentences[str_count(sentences, color_match) > 1]
more
str_view(more, color_match)
str_view_all(more, color_match)
str_extract(more, color_match)
str_extract_all(more, color_match)
str_extract_all(more, color_match, simplify = T)

as_tibble(str_extract(sentences, "^[^\\s]+")) %>%
  count(value) %>%
  rename(word = value, count = n) %>%
  mutate(logcount = log(count)) %>%
  arrange(desc(count)) %>%
  mutate(rank_n1 = 1/row_number()) %>%
  ggplot(aes(x = rank_n1, y = logcount), stat = "identity") +
    geom_point(alpha = 1/3)
  
sentences %>%
  str_subset("[^\\s]*ing[^a-z]") %>%
  str_extract_all("[^\\s]*ing[^a-z]", simplify = T) %>%
  str_extract("[a-zA-Z]+") %>%
  as_tibble() %>%
  ggplot(aes(x = value)) +
    geom_bar() + 
    coord_flip()

#grouped matches
noun <- "(a|the)\\s([^\\s]+)"
has_noun <- sentences %>%
  str_subset(noun) 
has_noun %>%
  str_extract(noun) %>%
  str_extract(" [^ ]+") %>%
  str_extract("[a-zA-Z]+")
has_noun %>%
  str_match(noun)

number <- "(one|two|three) ([^ ]+)"
afterNumber <- sentences %>%
  str_subset(number)
afterNumber %>%
  str_extract(number) %>%
  str_extract(" [^ ]+") %>%
  str_extract("[a-zA-Z]+")
afterNumber %>%
  str_extract("[^\\.]+") %>%
  str_match(number)

contractions <- "([a-zA-Z]+)\\'([a-zA-Z]+)"
contra <- sentences %>%
  str_subset(contractions)
contra %>%
  str_extract(contractions) %>%
  str_match(contractions)

#replacing matches
x <- c("apple", "pear", "banana")
str_replace_all(x, "[aeiou]", "V") %>%
  str_replace_all("[^aeiouV]", "C")

#splitting
sentences %>%
  head(5) %>%
  str_split(" ") %>%
  unlist() %>%
  as_tibble() %>%
  count(value)

"a|b|c|d" %>%
  str_split("\\|") %>%
  .[[1]]

sentences %>%
  head(5) %>%
  str_split(" ", simplify = T)

fields <- c("Name: Miao", "Country: CHN", "Age: 29")
fields %>%
  str_split(": ", n = 2, simplify = T)

str_view(fruit, "nana")
bananas <- c("banana", "Banana", "BANANA")
str_view(bananas, regex("banana", ignore_case = T))
