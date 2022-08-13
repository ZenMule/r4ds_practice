library(tidyverse)
library(modelr)
library(ggbeeswarm)
options(na.action = na.warn)

library(nycflights13)
library(lubridate)

ggplot(diamonds, aes(cut, price)) + geom_boxplot()
ggplot(diamonds, aes(color, price)) + geom_boxplot()
ggplot(diamonds, aes(clarity, price)) + geom_boxplot()

ggplot(diamonds, aes(carat, price)) +
  geom_hex(bins = 50)

# focus on diamonds smaller than 2.5 carat
# log-transform the carat and price
diamonds2 <- diamonds %>%
  filter(carat <= 2.5) %>%
  mutate(lprice = log2(price), lcarat = log2(carat))

ggplot(diamonds2, aes(lcarat, lprice)) +
  geom_hex(bins = 50)
mod_diamonds <- lm(lprice ~ lcarat, data = diamonds2)
summary(mod_diamonds)

# back transform the predicted data
grid <- diamonds2 %>%
  data_grid(carat = seq_range(carat, 20)) %>%
  mutate(lcarat = log2(carat)) %>%
  add_predictions(mod_diamonds, "lprice") %>%
  mutate(price = 2^lprice)

ggplot(diamonds2, aes(carat, price)) +
  geom_hex(bins = 50) +
  geom_line(data = grid, color = "red", size = 1)

diamonds2 <- diamonds2 %>%
  add_residuals(mod_diamonds, "lresid")
ggplot(diamonds2, aes(lcarat, lresid)) +
  geom_hex(bins = 50) +
  geom_ref_line(h = 0, size = 0.5, colour = "red")

ggplot(diamonds2, aes(cut, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(color, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(clarity, lresid)) + geom_boxplot()

mod_diamonds2 <- lm(lprice ~ lcarat + color + cut + clarity, data = diamonds2)
grid <- diamonds2 %>%
  data_grid(cut, .model = mod_diamonds2) %>%
  add_predictions(mod_diamonds2)
grid
ggplot(grid, aes(cut, pred)) +
  geom_point(color = "red")

diamonds2 <- diamonds2 %>%
  add_residuals(mod_diamonds2, "lresid2")
ggplot(diamonds2, aes(lcarat, lresid2)) +
  geom_hex(bins = 50)

# flights
daily <- flights %>%
  mutate(date = make_date(year, month, day)) %>%
  group_by(date) %>%
  summarize(n = n())
daily

ggplot(daily, aes(date, n)) +
  geom_line()

daily <- daily %>%
  mutate(wday = wday(date, label = T))
daily

ggplot(daily, aes(wday, n)) +
  geom_boxplot()
mod_wday <- lm(n ~ wday, data = daily)
grid <- daily %>%
  data_grid(wday) %>%
  add_predictions(mod_wday, "n")
ggplot(daily, aes(wday, n)) +
  geom_quasirandom(size = 0.7) +
  geom_point(data = grid, color = "red", size = 4)

daily <- daily %>%
  add_residuals(mod_wday)
ggplot(daily, aes(date, resid)) +
  geom_ref_line(h = 0, colour = "red", size = 1) +
  geom_line()
ggplot(daily, aes(date, resid, color = wday)) +
  geom_ref_line(h=0, colour="red", size=1) +
  geom_line()
daily %>%
  ggplot(aes(date, resid)) +
  geom_ref_line(h = 0, colour = "red", size = 0.8) +
  geom_line(color = "grey50") +
  geom_smooth(se = F, span = 0.20)

daily %>%
  filter(wday == "Sat") %>%
  ggplot(aes(date, n)) +
  geom_point() +
  geom_line() +
  scale_x_date(
    NULL,
    date_breaks = "1 month",
    date_labels = "%b"
  )

term <- function(date) {
  cut(date,
      breaks = ymd(20130101, 20130605, 20130825, 20140101),
      labels = c("spring", "summer", "fall")
      )
}

daily <- daily %>%
  mutate(term = term(date))

wday <- daily %>%
  filter(wday == "Sat")
  
ggplot(wday, aes(date, n, color = term)) +
  geom_point(alpha = 1/3) +
  geom_line() +
  scale_x_date(
    NULL,
    date_breaks = "1 month",
    date_labels = "%b"
  )

daily %>%
  ggplot(aes(wday, n, color = term)) +
  geom_boxplot()

mod1 <- lm(n ~ wday, data = daily)
mod2 <- lm(n ~ wday * term, data = daily)

daily %>%
  gather_residuals(without_term = mod1, with_term = mod2) %>%
  ggplot(aes(date, resid, color = model)) +
  geom_line(alpha = 3/4)

grid <- daily %>%
  data_grid(wday, term) %>%
  add_predictions(mod2, "n")
ggplot(daily, aes(wday, n)) +
  geom_boxplot(aes(color = term)) +
  geom_point(data = grid, color = "red") +
  facet_wrap(~ term)

mod3 <- MASS::rlm(n ~wday * term, data = daily)
daily %>%
  gather_residuals(without_term = mod1, with_term = mod2, rlm = mod3) %>%
  ggplot(aes(date, resid)) + 
  geom_hline(yintercept = 0, size = 1, color = "red") +
  geom_line(alpha = 0.8) +
  facet_wrap(~model)

library(splines)
mod <- MASS::rlm(n ~ wday * ns(date, 5), data = daily)
daily %>%
  data_grid(wday, date = seq_range(date, n=13)) %>%
  add_predictions(mod) %>%
  ggplot(aes(date, pred, color = wday)) +
  geom_line() +
  geom_point() + 
  theme_calc()

daily %>%
  top_n(3, resid)
