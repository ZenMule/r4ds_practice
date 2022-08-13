library(tidyverse)
library(modelr)
library(purrr)

options(na.action = na.warn)

ggplot(sim1, aes(x,y)) + geom_point()

#randomly generate some models to test
models <- tibble(a1 = runif(250, -20, 40), a2 = runif(250, -5, 5))

#plot all the models generated
ggplot(sim1, aes(x, y )) +
  geom_abline(
    aes(slope = a2, intercept = a1),
    data = models, alpha = 1/4) +
  geom_point()

#create predicted values
model1 <- function(mod, data) {
  data$x * mod[2] + mod[1]
}
model1(c(7, 1.5), sim1)

#measure the distance of the actual value and predicted value
measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}
measure_distance(c(7, 1.5), sim1)

#calculate the distance of each observation in each model
sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}

#add the distance to the models
models <- models %>%
  mutate(dist = map2_dbl(a1, a2, sim1_dist))
models

ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, color = -dist),
    data = filter(models, rank(dist) <= 10)
  )

ggplot(models, aes(a1, a2)) +
  geom_point(
    data = filter(models, rank(dist) <= 10),
    size = 4, color = "red"
  ) +
  geom_point(aes(color = -dist))

grid <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) %>%
  mutate(dist = map2_dbl(a1, a2, sim1_dist))

grid %>%
  ggplot(aes(a1, a2)) +
  geom_point(data = filter(grid, rank(dist) <= 10),
             size = 4, color = "red") +
  geom_point(aes(color = -dist))

ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(aes(intercept = a1, slope = a2, color = -dist), 
              data= filter(grid, rank(dist) <= 10))

best <- optim(c(0,0), measure_distance, data = sim1)
best$par
ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, coloer = "grey30") +
  geom_abline(intercept + bst$par[1], slope = best$par[2])

#modeling use lm()
sim1_mod <- lm(y ~ x, sim1)
coef(sim1_mod)
sim1_mod

sim1a <- tibble(x = rep(1:10, each = 3),
                y = x * 1.5 + 6 + rt(length(x), df = 2))
sim1a_mod <- lm(y ~ x, sim1a)
summary(sim1a_mod)
coef(sim1a_mod)

#generate the predicted values
grid <- sim1 %>%
  data_grid(x)
grid %>%
  add_predictions(sim1_mod)
add_predictions(sim1, sim1_mod)

sim_mz <- tibble(x = rep(c(1:2), 5), y = rep(c(3:7), 2), z = c(5:14))
sim_mz_mod <- lm(z ~ x*y, sim_mz)
summary(sim_mz_mod)
coef(sim_mz_mod)
add_predictions(sim_mz, sim_mz_mod)
data_grid(sim1, x, y) %>%
  add_predictions(sim1_mod)

grid <- sim1 %>%
  data_grid(x)
grid <- grid %>%
  add_predictions(sim1_mod)
grid

ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +
  geom_line(
    aes(y = pred),
    data = grid,
    color = "red",
    size = 1
  )

#redisuals
sim1_resid <- sim1 %>%
  add_residuals(sim1_mod)
sim1_resid

#understand the spread of residulas
ggplot(sim1_resid, aes(resid)) +
  geom_freqpoly(binwidth = 0.5)

ggplot(sim1_resid, aes(x, resid)) +
  geom_ref_line(h = 0, colour = "blue", size = 1/2) + #add a reference line in te plot
  geom_point()

#use loess() to model
sim1_mod2 <- loess(y ~ x, sim1)
summary(sim1_mod2)
grid <- sim1 %>%
  data_grid(x)
grid <- grid %>%
  add_predictions(sim1_mod2)

ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +
  geom_line(data = grid, aes(y = pred), color = "red", size = 3/2)

ggplot(sim1, aes(x, y)) +
  geom_point() +
  geom_smooth()

?gather_predictions #list the perdicted values from different models in different variables
?spread_predictions #list the predicted values from different models in the same variable but different value

?geom_ref_line

df <- tribble(
  ~y, ~x1, ~x2,
  4, 2, 5,
  5, 1, 6
)
model_matrix(df)

sim2
sim2_mod <- lm(y ~ x, data = sim2)
grid <- sim2 %>%
  data_grid(x) %>%
  add_predictions(sim2_mod)
grid
ggplot(sim2, aes(x)) +
  geom_boxplot(aes(y = y), width = (1-0.618)) +
  geom_point(data = grid, aes(y = pred), color = "red", size = 4)

sim3
ggplot(sim3, aes(x1, y)) +
  geom_point(aes(color = x2)) +
  geom_smooth(aes(color = x2))

mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)

grid <- sim3 %>%
  data_grid(x1, x2) %>%
  gather_predictions(mod1, mod2)
grid

ggplot(sim3, aes(x1, y, color = x2)) +
  geom_point() +
  geom_line(data = grid, aes(y = pred)) +
  facet_wrap(~ model)

sim3 <- sim3 %>%
  gather_residuals(mod1, mod2)
sim3
ggplot(sim3, aes(x1, resid, color = x2)) +
  geom_point() +
  geom_ref_line(h = 0) +
  facet_grid(model ~ x2)
ggplot(sim3, aes(resid, y = ..density.., colour = x2)) +
  geom_freqpoly(binwidth = 0.5) +
  facet_grid(model ~ x2)

sim4
mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)
grid <- sim4 %>%
  data_grid(
    x1 = seq_range(x1, 5),
    x2 = seq_range(x2, 5)
  ) %>%
  gather_predictions(mod1, mod2)
grid

ggplot(grid, aes(x1, x2)) +
  geom_tile(aes(fill = pred)) +
  facet_wrap(~ model)

ggplot(grid, aes(x1, pred, color = x2, group = x2)) +
  geom_line() +
  facet_wrap(~ model) +
  theme_bw()
