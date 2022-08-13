library(tidyverse)
library(lubridate)
library(nycflights13)

today()
now()

#from strings
ymd("2017-01-31")
mdy("January 31st, 2017")
dmy("31-Jan-2017")

ymd(20170405)
ymd_hms("20170131 201159")

ymd(20170131, tz = "UTC")

flights %>%
  select(year,month,day,hour,minute) %>%
  mutate(departure = make_datetime(year, month, day, hour, minute))

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year,month,day,time %/% 100, time %% 100)
}

flights_dt <- flights %>%
  filter(!is.na(dep_time), !is.na(arr_time)) %>%
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sche_dep_time = make_datetime_100(
      year, month, day, sched_arr_time
    )
  ) %>%
  select(origin, dest, ends_with("delay"), ends_with("time"))
flights_dt %>%
  ggplot(aes(dep_time)) +
    geom_freqpoly(binwidth = 86400) #86400 = 1day

flights_dt %>%
  filter(dep_time < ymd(20130102)) %>%
  ggplot(aes(dep_time)) +
    geom_freqpoly(binwidth = 600) #600 = 10 minute

as_datetime(today())
as_date(now())

datetime <- ymd_hms("2018-09-06 01:01:20")
year(datetime)
month(datetime)
mday(datetime)
yday(datetime)
wday(datetime)

month(datetime, label = T)
wday(datetime, label = T, abbr = F)

flights_dt %>%
  mutate(wday = wday(dep_time, label = T)) %>%
  ggplot(aes(wday)) +
    geom_bar()

flights_dt %>%
  mutate(minute = minute(dep_time)) %>%
  group_by(minute) %>%
  summarize(avg_delay = mean(arr_delay, na.rm = T), n = n()) %>%
  ggplot(aes(minute, avg_delay)) +
    geom_line()

sched_dep <- flights_dt %>%
  mutate(minute = minute(sched_dep_time)) %>%
  group_by(minute) %>%
  summarize(avg_delay = mean(arr_delay, na.rm = T), n = n())
ggplot(sched_dep, aes(minute, avg_delay)) + geom_line()

#rounding
flights_dt %>%
  count(week = floor_date(dep_time, "week")) %>%
  ggplot(aes(week,n)) +
    geom_line()


