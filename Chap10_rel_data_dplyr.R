library(tidyverse)
library(nycflights13)

#combine many tables of data to answer questions
#mutate join, filtering join, set operations

airlines
airports
planes
weather

#the variables used to connect each pair of tables are called 'keys'.
#primary key and foreign key.

planes %>%
  count(tailnum) %>%
  filter(n>1)
weather %>%
  count(year, month, day, hour, origin) %>%
  filter(n>1)

flights2 <- flights %>%
  select(year:day, hour, origin, dest, tailnum, carrier)
flights2 %>%
  select(-origin, -dest) %>%
  left_join(airlines, by = "carrier")
flights2 %>%
  select(-origin, -dest) %>%
  mutate(name = airlines$name[match(carrier, airlines$carrier)])

#inner join: matches paris of observations whenever their keys are equal. 
#Unmatched rows are not included.
x <- tribble(
  ~key, ~val_x,
  #-----------,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key, ~val_x,
  #-----------,
  1, "y1",
  2, "y2",
  4, "y3"
)

x %>%
  inner_join(y, by = "key")
# A tibble: 2 x 3
#    key val_x.x val_x.y
#  <dbl> <chr>   <chr>  
#1     1 x1      y1     
#2     2 x2      y2   

#outer_join(): keeps observations that appear in both tables.
#left_join(): keeps all in x
#right_join(): keeps all in y
#full_join(): keeps all in both

x %>%
  left_join(y, by = "key")
x %>%
  right_join(y, by = "key")
x %>%
  full_join(y, by = "key")

#duplicate keys
#one table has duplicate keys
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  1, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2"
)
x %>%
  left_join(y, by = "key")

#both tables have duplicate keys
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  3, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  2, "y3",
  3, "y4"
)
x %>%
  left_join(y, by = "key")

flights2 %>%
  left_join(weather)
flights2 %>%
  left_join(planes, by = "tailnum")
flights2 %>%
  left_join(airports, by = c("dest" = "faa"))
flights2 %>%
  left_join(airports, c("origin" = "faa"))

#exercises
library(maps)

avg_delay <- flights %>% 
  group_by(dest) %>%
  filter(arr_delay > 0) %>%
  summarise(avg_delay = round(mean(arr_delay, na.rm = T), 2)) %>%
  inner_join(airports, c("dest" = "faa")) 
avg_delay %>%
  ggplot(aes(lon, lat, alpha = avg_delay, size = avg_delay)) +
    borders("state") +
    geom_point(color = "#003366") +
    coord_quickmap()

loc <- select(airports, faa, lon, lat)
flights_loc <- flights %>%
  left_join(loc, by = c("dest" = "faa")) %>%
  rename(dest_lon = lon, dest_lat = lat) %>%
  left_join(loc, by = c("origin" = "faa")) %>%
  rename(origin_lon = lon, origin_lat = lat)

plane_age <- planes %>%
  mutate(plane_age = 2013 - year) %>%
  select(tailnum, plane_age)
plane_delay <- flights %>%
  group_by(tailnum) %>%
  summarise(avg_delay = mean(dep_delay, na.rm = T))
plane_delay %>%
  inner_join(plane_age, by = "tailnum") %>%
  ggplot(aes(x = plane_age, y = avg_delay, na.rm = T)) +
    geom_jitter(alpha = 1/10) 

flights_weather <- flights %>%
  inner_join(weather, by = c("origin" = "origin",
                             "year" = "year",
                             "month" = "month",
                             "day" = "day",
                             "hour" = "hour"))
flights_weather %>%
  group_by(precip) %>%
  summarise(avg_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = precip, y = avg_delay)) +
  geom_smooth() + geom_point()

flights_20130613 <- flights %>%
  filter(year == 2013 , month == 6 , day == 13) 
ggplot(flights_20130613, aes(x = dep_delay)) +
    geom_histogram()
ggplot(flights_20130613, aes(x = arr_delay)) +
  geom_histogram()

#filtering joins
#semi_join() keeps all observations in x that have a match in y.
#anti_join() drops all ovservations in x that have a match in y.
top_dest <- flights %>%
  count(dest, sort = T) %>%
  head(10)
flights %>%
  filter(dest %in% top_dest$dest)
flights %>%
  semi_join(top_dest)

flights %>%
  anti_join(planes, by = "tailnum") %>%
  count(tailnum, sort = T)
