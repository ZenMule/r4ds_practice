library(tidyverse)
library(forcats)
library(readr)

month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
x1 <- c("Dec", "Apr", "Jan", "Mar")
y1 <- factor(x1, levels = month_levels)

x2 <- c("Dec", "Apr", "Jam", "Mar")
y2 <- parse_factor(x2, levels = month_levels)

gss_cat
gss_cat %>%
  count(race)
ggplot(gss_cat, aes(race)) +
  geom_bar() +
  scale_x_discrete(drop = F)
gss_cat %>%
  count(relig)

#modify order, fct_reorder()
relig <- gss_cat %>%
  group_by(relig) %>%
  summarize(
    age = mean(age, na.rm = T),
    tvhours = mean(tvhours, na.rm = T),
    n = n()
  )

relig %>%
  ggplot(aes(fct_reorder(relig, tvhours), tvhours)) + geom_point() + coord_flip()

relig %>%
  mutate(relig = fct_reorder(relig, tvhours)) %>%
  ggplot(aes(tvhours, relig)) +
  geom_point()

rincome <- gss_cat %>%
  group_by(rincome) %>%
  summarize(
    age = mean(age, na.rm = T),
    tvhours = mean(tvhours, na.rm = T),
    n = n()
  ) 
ggplot(rincome, aes(age, fct_reorder(rincome, age))) +
  geom_point()
ggplot(rincome, aes(age, fct_relevel(rincome, "Not applicable"))) +
  geom_point()

by_age <- gss_cat %>%
  filter(!is.na(age)) %>%
  group_by(age, marital) %>%
  count() %>%
ggplot(by_age, aes(age, n/sum(n), color = marital)) + geom_line(na.rm = T)
ggplot(by_age, aes(age, n/sum(n), color = fct_reorder2(marital, age, n/sum(n)))) +
  geom_line() +
  labs(color = "marital")

gss_cat %>%
  mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(marital)) +
    geom_bar()

#fct_recode()
gss_cat %>% count(partyid)
gss_cat %>% 
  mutate(partyid = fct_recode(
    partyid,
    "Republican, strong" = "Strong rep",
    "Republican, weak" = "Weak rep",
    "Independent, near rep" = "Ind, near rep",
    "Independent, near dem" = "Ind, near dem",
    "Democrat, weak" = "Weak dem",
    "Democrat, strong" = "Strong dem"
  )) %>%
  count(partyid)

#recode a lot of factors
gss_cat %>%
  mutate(partyid = fct_collapse(partyid, 
                                Other = c("No answer", "Don't know", "Other party"),
                                Republican = c("Strong republican", "Not str republican"),
                                Independent = c("Ind,near rep", "Independent", "Ind,near dem"),
                                Democratic = c("Not str democrat", "Strong democrat")
                                )) %>%
  count(partyid) %>%
  ggplot(aes(x = partyid, y = n/sum(n))) +
    geom_bar(stat = "identity")

#lump together small groups
gss_cat %>%
  mutate(relig = fct_lump(relig)) %>%
  count(relig)
gss_cat %>%
  mutate(relig = fct_lump(relig, n = 10)) %>%
  count(relig, sort = T) %>%
  print(n = Inf)
