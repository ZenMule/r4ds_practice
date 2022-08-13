library(modelr)
library(tidyverse)
install.packages("gapminder")
library(gapminder)

gapminder %>%
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3)
