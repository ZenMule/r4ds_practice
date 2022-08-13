library(nycflights13)
library(tidyverse)

flights

#filter
jan1 <- filter(flights,month == 1, day == 1)
(dec12 <- filter(flights, month == 12, day == 12))

sqrt(2)^2 == 2
near(sqrt(2)^2,2)

#filter with logical operators
filter(flights, month == 11 | month == 12)
non_dec <- filter(flights, month %in% c(11,12))

filter(flights, dep_delay <= 120 | arr_delay <= 120)
filter(flights, !(dep_delay > 120 & dep_delay > 120))

#filter missing values
df <- tibble(x = c(1, NA, 3))
filter(df, x>1)
filter(df, is.na(x)|x>1)

#exercise. p.49
View(filter(flights, arr_delay >= 120))
View(filter(flights, dest == "IAH" | dest == "HOU"))
View(filter(flights, carrier == "UA" | carrier == "AA" | carrier == "DL"))
View(filter(flights, month == 7 | month == 8 | month == 9))
View(filter(flights, arr_delay > 120, dep_delay < 0))
View(filter(flights, dep_delay >= 60, arr_delay <= 30))
View(filter(flights, dep_time > 0, dep_time <= 600))

View(filter(flights, between(month, 7, 9)))
View(filter(flights, between(dep_time, 0, 600)))

View(filter(flights, is.na(dep_time)))

#arrange
arrange(flights, year, month, day)
arrange(flights, desc(arr_delay))

#arrange by descending
arrange(flights, desc(is.na(dep_time)))
arrange(flights, desc(dep_delay))
arrange(flights, year, month, day, dep_time)

#select (columns)
select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))

select(flights, starts_with("arr"))
select(flights, ends_with("delay"))
select(flights, contains("time"))
select(flights, matches("(.)\\1"))

#rename a variable
rename(flights, arrdelay = arr_delay)

#put selected variables in front of any other variables
select(flights, time_hour, air_time, everything())

#select exercises
select(flights, dep_time : arr_delay)
select(flights, ends_with("delay"), ends_with("time"), -contains("sched"), -contains("air"))
select(flights, starts_with("dep"), starts_with("arr"))

select(flights, dep_time)

vars <- c("year", "month", "day", "dep_delay", "arr_delay")

#mutate(), add new variables
flights_sml <- select(flights, year:day, ends_with("delay"), distance, air_time)
View(flights_sml)
mutate(flights_sml, 
  gain = arr_delay - dep_delay,
  speed = distance / air_time * 60
  )

mutate(flights_sml, 
  gain = arr_delay - dep_delay, 
  hours = air_time/60, 
  gain_per_hour = gain / hours
  )

transmute(flights, 
  gain = arr_delay - dep_delay,
  hours = air_time / 60,
  gain_per_hour = gain / hours
  )

#exercises mutate
transmute(flights,
  dep_time0 = ((dep_time %/% 100)*60 + dep_time %% 100),
  sched_dep_time0 = ((sched_dep_time %/% 100)*60 + sched_dep_time %% 100),
)

select(flights, air_time)
transmute(flights,
  air_time00 = arr_time - dep_time)
  
transmute(flights,
  dep_time0 = ((dep_time %/% 100)*60 + dep_time %% 100),
  arr_time0 = ((arr_time %/% 100)*60 + arr_time %% 100),
  arr_time0 = arr_time0 - dep_time0)

arrange(flights, -min_rank(flights$dep_delay))

1:3 + 1:10

#summarize()
summarize(flights, delay = mean(dep_delay, na.rm = T))

by_day <- group_by(flights, year, month, day)
summarize(by_day, delay = mean(dep_delay, na.rm = T))

by_dest <- group_by(flights, dest)
delay <- summarize(by_dest,
  n = n(),
  dist = mean(distance, na.rm= T),
  delay = mean(arr_delay, na.rm = T)
)
delay <- filter(delay, n>20, dest != "HNL")

ggplot(delay, aes(x=dist, y=delay)) +
  geom_smooth(se = F) +
  geom_point(aes(size=n), alpha = 1/3)
  
#pipe %>%
delays <- flights %>%
  group_by(dest) %>%
  summarize(
    n = n(),
    dist = mean(distance, na.rm = T),
    delay = mean(arr_delay, na.rm = T)
  ) %>%
  filter(n > 20, dest != "HNL")
  
ggplot(delays, aes(x=dist, y=delay)) +
  geom_smooth(se = F) +
  geom_point(aes(size=n), alpha = 1/3)

#missing values
(not_cancelled <- flights %>%
  filter(!(is.na(dep_delay) | is.na(arr_delay))) %>%
  group_by(year, month, day) %>%
  summarize(mean = mean(dep_delay))
)

#counts, n(), sum(!is.na(x))
not_cancelled <- flights %>%
  filter(!(is.na(dep_delay) | is.na(arr_delay)))

delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarize(delay = mean(arr_delay))
ggplot(data = delays, aes(x = delay)) +
  geom_freqpoly(binwidth = 10)
  
delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarize(delay = mean(arr_delay, na.rm = T), n=n())
ggplot(data = delays, aes(x = n, y = delay)) +
  geom_point(alpha = 1/10)
  
#filter out the groups with the smallest numbers of observations
delays %>%
  filter(n > 25) %>%
  ggplot(aes(x=delay, y=n)) +
    geom_point(alpha = 1/10, color = "orange",position="jitter")

#batting data 
library(Lahman)
batting <- as_tibble(Lahman::Batting)
batters <- batting %>%
  group_by(playerID) %>%
  summarize(
    ba = sum(H, na.rm = T) / sum(AB, na.rm = T),
    ab = sum(AB, na.rm = T)
  )
batters %>%
  filter(ab > 100) %>%
  ggplot(aes(x = ab, y = ba)) +
    geom_point(alpha = 1/10) +
    geom_smooth(se = F)
  
#Summary functions
not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(avg_delay = mean(arr_delay[arr_delay > 0]))
  
not_cancelled %>%
  group_by(dest) %>%
  summarize(distance_sd = sd(distance)) %>%
  arrange(desc(distance_sd))
  
not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(
    first = min(dep_time),
    last = max(dep_time)
  )
  
not_cancelled %>%
  group_by(year, month, day) %>%
  mutate(r = min_rank(desc(dep_time))) %>%
  filter(r %in% range(r))

not_cancelled %>%
  group_by(dest) %>%
  summarize(carriers = n_distinct(carrier)) %>%
  arrange(desc(carriers))
  
not_cancelled %>%
  count(dest) %>%
  arrange(desc(n))

not_cancelled %>%
  count(tailnum, wt = distance) %>%
  arrange(desc(n))

not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(n_early = sum(dep_time < 600))

#mean(x) gives the proportion
not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(delay_1h = mean(arr_delay >= 60))
  
#grouping by multiple variables. When you group by multiple variables, each summary peels off one level of the grouping.
daily <- group_by(flights, year, month, day)
(perday <- summarize(daily, flights = n()))
(permonth <- summarize(perday, flights = sum(flights)))
(peryear <- summarize(permonth, flights = sum(flights)))

#ungrouping
daily %>%
  ungroup() %>%
  summarize(flights = n())

#exercises p.72-73
View(flights)

not_cancelled <- filter(flights, !is.na(dep_delay), !is.na(arr_delay))
late_early_15 <- not_cancelled %>% 
  group_by(flight) %>% 
  transmute(
    p_15_early = mean(arr_delay < -15),
    p_15_late = mean(arr_delay > 15),
    p_on_time = mean(arr_delay == 0))
View(filter(late_early_15, p_15_early > 0.5))
View(filter(late_early_15, p_15_late > 0.5))

late_10min <- not_cancelled %>%
  group_by(flight) %>%
  transmute(
    late_10min_prop = mean(arr_delay >= 10))
View(filter(late_10min, late_10min_prop == 1))

late_early_30 <- not_cancelled %>%
  group_by(flight) %>%
  transmute(
    late_30 = mean(arr_delay >= 30),
    early_30 = mean(arr_delay <= -30))
View(filter(late_early_30, late_30 > 0.5))
View(filter(late_early_30, early_30 > 0.5))

ontime_late2h <- not_cancelled %>%
  group_by(flight) %>%
  transmute(
    on_time = mean(abs(arr_delay) <= 10),
    late_2h = mean(arr_delay >= 120)
  )
filter(ontime_late2h, on_time >= 0.99)
filter(ontime_late2h, late_2h <= 0.01)

not_cancelled %>%
  group_by(dest) %>%
  summarize(n = n())
not_cancelled %>%
  group_by(dest) %>%
  tally()
  
not_cancelled %>%
  group_by(tailnum) %>%
  summarize(n = sum(distance)) %>%
  arrange(desc(n))

delay_cancel <- flights %>%
  group_by(year, month, day) %>%
  summarise(
    avg_delay = mean(dep_delay, na.rm = T),
    sum_cancel = sum(is.na(dep_time) | is.na(arr_delay))
  )
ggplot(data = delay_cancel, aes(y = sum_cancel, x = avg_delay)) +
  geom_point(alpha = 1/10) +
  geom_smooth(se = F)
ggplot(data = delay_cancel, aes(x = sum_cancel)) +
  geom_freqpoly(binwidth = 10)

worst_carrier <- flights %>%
  group_by(carrier) %>%
  summarize(
    nflights = n(),
    prop_delay = mean(dep_delay >= 15, na.rm = T),
    prop_cancel = mean(is.na(dep_time))
  )
ggplot(data=worst_carrier, aes(x = prop_delay, y = prop_cancel)) +
  geom_text(aes(label = carrier, color = carrier, size = nflights))

bad_carrier_airport <- flights %>%
  group_by(carrier, dest) %>%
  summarize(
    nflights = n(),
    avg_arr_delay = mean(arr_delay, na.rm = T),
    prop_cancel = mean(is.na(arr_time))
  ) %>%
  filter(nflights >= 10) %>%
  arrange(desc(avg_arr_delay))

#Grouped mutates
flights_sml %>%
  group_by(year, month, day)
  filter(rank(desc(arr_delay)) < 10)

(popular_dests <- flights %>%
  group_by(dest) %>%
  filter(n() > 365)
)

popular_dests %>%
  filter(arr_delay > 0) %>%
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>%
  select(year:day, dest, arr_delay, prop_delay)
  
#exercises p.75-76