library(tidyverse)

table1
table2
table3
table4a
table4b

#1. put each dataset in a tibble
#2. put each variable in a column

#Each variable must have its own column
#Each observation must have its own row
#Each value must have its own cell

table1 %>%
  mutate(rate = cases / population * 10000)

table1 %>%
  count(year, wt = cases)

ggplot(table1, aes(year, cases)) +
  geom_line(aes(group = country), color = "grey50") +
  geom_point(aes(color = country))

#Spreading and Gathering
#problems frequently faced:
  #One variable spread across multiple columns
  #One observation scattered across multiple rows

table4a
#  A tibble: 3 x 3
#country     `1999` `2000`
#* <chr>        <int>  <int>
#  1 Afghanistan    745   2666
#2 Brazil       37737  80488
#3 China       212258 213766

table4a <- table4a %>%
  gather(`1999`, `2000`, key = "year", value = "cases")
#3 parameters:
  #1. The set of columns that represent values, not variables.
  #2. The name of the variable whose values form the column names, the "key"
  #3. The name of the variable whose values are spread over the cells, the "value"

table4b <- table4b %>%
  gather(`1999`,`2000`, key = "year", value = "population")

left_join(table4a, table4b)

#spread() used when an observation is scattered across multiple rows.
#The column that contains variable names, the "key" column.
#The column that contains values forms multiple variables, the "value" column.
table2
table2 %>%
  spread(key = type, value = count)

#exercises p.156
stocks <- tibble(
  year = c(2015, 2015, 2016, 2016),
  half = c(1, 2, 1, 2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks %>% 
  spread(year, return) %>%
  gather("year", "return", `2015`, `2016`)

table4a %>%
  spread(key = "year", value = "cases") %>%
  gather(`1999`,`2000`, key = year, value = cases)

preg <- tribble(
  ~pregnant, ~male, ~female,
  #------------------------
  "yes", NA, 10,
  "no", 20, 12
)
preg %>%
  gather(male, female, key = gender, value = cases) %>%
  mutate(pregnant = pregnant == "yes")

#Separating and Pull
#separate
table3 %>%
  separate(rate, into = c("cases", "population"), sep = "/", convert = T)
table3 %>%
  separate(year, into = c("century", "year"), sep = 2)
#unite
table5 %>%
  unite(new, century, year, sep = "")

#missing values:
  #explicit and implicit
stocks <- tibble(
  year = c("2015", "2015", "2015", "2015", "2016", "2016","2016"),
  qtr = c("1", "2", "3", "4", "2", "3", "4"),
  return = c(1.88, 0.59, 0.35, NA, 0.92, 0.17, 2.66))
stocks %>%
  spread(key = year, value = return)
stocks %>%
  spread(key = year, value = return) %>%
  gather(`2015`, `2016`, key = year, value = return, na.rm = T)

stocks %>%
  complete(year, qtr)

treatment <- tribble(
  ~person, ~treatment, ~response,
  #-----------------------------
  "Derrick", 1, 7,
  NA, 2, 10,
  NA, 3, 9,
  "Katherine", 1, 4
)
#fill() fill all the missing value with the last unmissing value
fill(treatment, person)

#case study
who
#gather all the columns from new_sp_m014 to newrel_f65
who1 <- gather(who, 
       new_sp_m014:newrel_f65, 
       key = "key",
       value = "cases",
       na.rm = T
       )
who1
who1 %>%
  count(key)

who2 <- who1 %>%
  mutate(key = stringr::str_replace(key, "newrel", "new_rel"))
who2

who3 <- separate(who2, key, c("new", "type", "sexage"), sep = "_")
who3

who4 <- who3 %>% select(-iso2, -iso3, -new)
who4

who5 <- who4 %>% separate(sexage, into = c("sex", "age"), sep = 1)
who5

who_tidy <- who %>%
  gather(new_sp_m014:newrel_f65, key = "key", value = "cases", na.rm = T) %>%
  mutate(key = stringr::str_replace(key, "newrel", "new_rel")) %>%
  separate(key, c("new", "type", "sexage"), sep = "_") %>%
  select(-iso2, -iso3, -new) %>%
  separate(sexage, c("sex", "age"), sep = 1)

#exercise
who_tidy %>%
  group_by(country) %>%
  mutate(cases_per_country = sum(cases)) %>%
  filter(cases_per_country >= 1000000) %>%
  count(country, year, sex, wt = cases) %>%
  rename(cases = n) %>%
  filter(year > 1994) %>%
  ggplot(aes(x = year, y = cases, color = sex)) +
    geom_line() +
    facet_wrap(. ~ country)

