library(tidyverse)
library(ggbeeswarm)
library(Hmisc)

a <- sample(100, 80)
a
b<- sample(20:120, 80)
b

var.test(a, b)
t.test(a, b, var.equal = T)

var2 <- rep(c("0.25","0.4","0.7","0.95"), each = 20)
var3 <- rep(c("h","i","j","k"), each = 20)

test <- as_tibble(cbind(a,b,var2)) %>%
  gather(a, b, key = var1, value = value) %>%
  mutate(value = as.numeric(value), var1 = as.factor(var1), var2 = as.numeric(var2))

test2 <- as_tibble(cbind(a,b,var3)) %>%
  gather(a, b, key = var1, value = value) %>%
  mutate(value = as.numeric(value), var1 = as.factor(var1), var3 = as.factor(var3))

test_summary <- test %>%
  group_by(var1, var2) %>%
  summarize(
    n = n(),
    mean_v = mean(value),
    sd = sd(value), 
    se = sd/sqrt(n))

test_summary2 <- test2 %>%
  group_by(var1, var3) %>%
  summarize(
    n = n(),
    mean_v = mean(value),
    sd = sd(value), 
    se = sd/sqrt(n))

t.test(value ~ var1, data = test, var.equal = T)

ggplot(test_summary2, aes(x=var3, y=mean_v, group = var1)) +
  geom_point(aes(shape = var1), size = 2.3) +
  geom_errorbar(aes(ymin = mean_v-se, ymax = mean_v+se, color = var1), width = .1) +
  geom_line(aes(linetype = var1))

source("summarySE.R")

tg <- as_tibble(ToothGrowth)
head(tg)
tgc <- summarySE(tg, measurevar = "len", groupvars = c("supp", "dose"))


# Standard error of the mean
ggplot(tgc, aes(x=dose, y=len, colour=supp)) + 
  geom_errorbar(aes(ymin=len-se, ymax=len+se), width=.1) +
  geom_line() +
  geom_point()


# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.05) # move them .05 to the left and right

ggplot(tgc, aes(x=dose, y=len, colour=supp)) + 
  geom_errorbar(aes(ymin=len-se, ymax=len+se), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd)


# Use 95% confidence interval instead of SEM
ggplot(tgc, aes(x=dose, y=len, colour=supp)) + 
  geom_errorbar(aes(ymin=len-ci, ymax=len+ci), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd)

# Black error bars - notice the mapping of 'group=supp' -- without it, the error
# bars won't be dodged!
ggplot(tgc, aes(x=dose, y=len, colour=supp, group=supp)) + 
  geom_errorbar(aes(ymin=len-ci, ymax=len+ci), colour="black", width=.05, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3)

