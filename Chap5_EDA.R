library(tidyverse)

#visualizing distribution
ggplot(data = diamonds) +
  geom_bar(aes(x = cut))

diamonds %>%
  count(cut)
diamonds %>%
  group_by(cut) %>%
  summarise(n = n())
diamonds %>%
  group_by(cut) %>%
  summarize(n = length(cut))

ggplot(data = diamonds) +
  geom_histogram(aes(x=carat), binwidth = 0.5)

diamonds %>%
  count(cut_width(carat, 0.5))

smaller <- diamonds %>%
  filter(carat < 3) 
ggplot(data = smaller, aes(x = carat)) +
  geom_histogram(binwidth = 0.1)
ggplot(data = smaller, aes(x = carat, color = cut)) +
  geom_freqpoly(binwidth = 0.1)

#typical values
ggplot(data = smaller, aes(x = carat)) +
  geom_histogram(binwidth = 0.01)
#In general, clusters of similar values suggest that subgroups exist in your data.

ggplot(data = faithful, aes(x = eruptions)) +
  geom_histogram(binwidth = 0.25)
ggplot(diamonds) +
  geom_histogram(aes(x = y), binwidth = 0.5)
ggplot(diamonds) +
  geom_histogram(aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0,50))

unusual <- diamonds %>%
  filter(y <3 | y > 20) %>%
  arrange(y)
unusual

#exercises
ggplot(data = diamonds, aes(x=x)) +
  geom_histogram(binwidth = 0.5)
ggplot(data = diamonds, aes(x=x)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))
diamonds %>%
  count(cut_width(x, 0.5))

ggplot(data = diamonds, aes(x=z)) +
  geom_histogram(binwidth = 0.5)
ggplot(data = diamonds, aes(x=z)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))
diamonds %>%
  count(cut_width(z, 0.5))

ggplot(data = diamonds) +
  geom_histogram(aes(x=price), binwidth = 1000)
ggplot(data = diamonds) +
  geom_histogram(aes(x=price), binwidth = 10) +
  coord_cartesian(xlim = c(1000, 2000))

diamonds %>%
  count(carat = 0.99)
count(diamonds, carat = 1)

#Missing values
diamonds2 <- diamonds %>%
  filter(between(y, 3, 20))

diamonds2 <- diamonds %>%
  mutate(y = ifelse(y < 3 | y >20, NA, y))
ggplot(data = diamonds2, aes(x = x, y = y)) +
  geom_point(aes(color = cut), alpha = 1/10, na.rm = T)

library(nycflights13)
flights_cancelled <- flights %>%
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  )
ggplot(data = flights_cancelled, aes(sched_dep_time)) +
  geom_freqpoly(
    aes(color = cancelled), 
    binwidth = 1/4
  )

#covariation
#density, the count standardized so that the area under each frequency polygon is one
ggplot(data = diamonds, aes(x = price, y = ..density..)) +
  geom_freqpoly(aes(color = cut), binwidth = 500)

#boxplot
ggplot(data = diamonds, aes(x = cut, y = price, color = cut)) +
  geom_boxplot()
#reroder()
ggplot(data = mpg) +
  geom_boxplot(
    aes(
      x = reorder(class, hwy, FUN = median),
      y = hwy
    )
  ) +
  coord_flip()

#exercises p.99
flights %>%
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min/60
  ) %>%
  ggplot(aes(x = sched_dep_time, y = ..density.., color = cancelled)) +
    geom_freqpoly(binwidth = 1/4)

ggplot(diamonds2) +
  geom_boxplot(aes(
    x = reorder(cut, price, FUN = median), 
    y = price)
  )
ggplot(diamonds2) +
  geom_boxplot(aes(
    x = reorder(color, price, FUN = median), 
    y = price)
  )
ggplot(diamonds2) +
  geom_boxplot(aes(
    x = reorder(clarity, price, FUN = median),
    y = price
  ))
ggplot(diamonds2) +
  geom_boxplot(aes(x = carat, y = price, group = cut_width(carat, 0.1)))

ggplot(diamonds2) +
  geom_boxploth(aes(y = cut, x = price))
ggplot(diamonds2) +
  geom_boxploth(aes(price, carat, group = cut_width(carat, 0.1)))

install.packages("lvplot")
library(lvplot)
ggplot(diamonds, aes(x = cut, y = price)) +
  geom_lv()
ggplot(diamonds, aes(x = cut, y = price)) +
  geom_violin()
ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 50) +
  facet_wrap(. ~ cut)
ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 50) +
  facet_grid(clarity~ cut)
ggplot(diamonds, aes(x = price, y = ..density..)) +
  geom_freqpoly(aes(color = cut))
  
install.packages("ggbeeswarm")
library(ggbeeswarm)

#if small set of data, geom_jitter() and geom_quasirandom() is helpful to visualize overlapping
ggplot(mpg, aes(class, hwy)) + geom_quasirandom()
ggplot(mpg, aes(class, hwy)) + geom_jitter()
ggplot(mpg, aes(class, hwy)) + geom_point()

ggplot(mpg, aes(class, hwy)) + geom_boxplot()
ggplot(mpg, aes(class, hwy)) + geom_violin()
ggplot(mpg, aes(class, hwy)) + geom_lv()

#Two categorical variables
ggplot(data = diamonds) +
  geom_count(
    aes(x = cut, y = color))
diamonds %>% count(color, cut)
diamonds %>%
  count(color, cut) %>%
  ggplot(aes(x = color, y = cut)) +
  geom_tile(aes(fill = n))

#exercises p.101
diamonds %>%
  count(color, cut) %>%
  group_by(color) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(color, cut)) +
    geom_tile(aes(fill = prop), color = "white")

diamonds %>%
  count(cut, color) %>%
  group_by(cut) %>%
  mutate(prop = n/sum(n)) %>%
  ggplot(aes(cut, color)) +
    geom_tile(aes(fill = prop), color = "white")

flights %>%
  group_by(month, dest) %>%
  summarise(avg_arr_delay = mean(arr_delay, na.rm = T)) %>%
  ggplot(aes(x = factor(month), y = dest)) +
    geom_tile(aes(fill = avg_arr_delay))



#two continuous variables
ggplot(smaller) +
  geom_point(aes(x = carat, y = price), alpha = 1/100)

ggplot(smaller) +
  geom_bin2d(aes(x = carat, y = price))

ggplot(smaller) +
  geom_hex(aes(x = carat, y = price))

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_boxplot(aes(group = cut_width(carat, 0.1)))

#cut_width() as a groupo_by() for continuous variable

#cut_number() displays approximately the same number of point in each bin.
ggplot(smaller, aes(x = carat, y = price)) +
  geom_boxplot(aes(group = cut_number(carat, 20)))

#exercises p.104-105
#use geom_freqpoly()
ggplot(data = diamonds,
       mapping = aes(x = carat, y = ..density.., color = cut_width(price, 5000))) +
  geom_freqpoly(binwidth = 0.1)

#not to display groups on the axis
ggplot(diamonds, aes(x = price, y = carat)) +
  geom_boxplot(aes(group = cut_number(price, 10))) +
ggplot(diamonds, aes(x = price, y = carat)) +
  geom_boxplot(aes(group = cut_width(price, 500)))

#display groups on the axis
ggplot(diamonds, aes(x = cut_number(price, 10), y = carat)) +
  geom_boxplot() +
  coord_flip()
ggplot(diamonds, aes(x = cut_width(price, 2000), y = carat)) +
  geom_boxplot() +
  coord_flip()

ggplot(diamonds, aes(x = carat, y = price)) +
  geom_boxplot(aes(group = cut_width(carat, 0.1))) +
  facet_wrap(. ~ cut)
ggplot(diamonds, aes(x = carat, y = ..density..)) +
  geom_freqpoly(aes(color = cut_number(price, 10))) +
  facet_wrap(. ~ cut)
ggplot(diamonds, aes(x = cut_number(carat, 5), y = price)) +
  geom_boxplot(aes(fill = cut, color = cut))
ggplot(diamonds, aes(x = cut, y = price, color = cut_number(carat, 5))) +
  geom_boxplot(aes(fill = cut_number(carat, 5)))

ggplot(diamonds, aes(x = x, y = y)) +
  geom_boxplot(aes(group = cut_width(x, 0.5))) +
  coord_cartesian(xlim = c(4,11), ylim = c(4,11))
ggplot(diamonds, aes(x = x, y = y)) +
  geom_boxplot(aes(group = cut_width(x, 0.5))) +
  xlim(4,11) +
  ylim(4,11)

ggplot(diamonds, aes(x = x, y = y)) +
  geom_point() +
  coord_cartesian(xlim = c(4,11), ylim = c(4,11))
ggplot(diamonds, aes(x = x, y = y)) +
  geom_point() +
  xlim(4,11) +
  ylim(4,11)

#Patterns and models
library(modelr)
mod <- lm(log(price) ~ log(carat), diamonds)
diamonds2 <- diamonds %>%
  add_residuals(mod) %>%
  mutate(resid = exp(resid))
ggplot(diamonds2) +
  geom_point(aes(x = carat, y = resid))
ggplot(diamonds2) +
  geom_boxplot(aes(x=cut, y=resid))

#the difference between coord_cartesian() between xlim() and ylim()
ggplot(diamonds) +
  geom_histogram(mapping = aes(x = price)) +
  coord_cartesian(xlim = c(100, 5000), ylim = c(0, 3000))

ggplot(diamonds) +
  geom_histogram(mapping = aes(x = price)) +
  xlim(100, 5000) + ylim(0, 3000)
