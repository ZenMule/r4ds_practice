library(tidyverse)

ggplot(mpg, aes(displ, hwy)) + 
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(
    title = paste("Fuel effeciency generally decreases with engine size"),
    subtitle = paste("Two seaters (sports cars) are an exception because of their light weight"),
    caption = "Data from fueleconomy.gov",
    x = "Engine displacement (L)",
    y = "Highway fuel economy (mpg)",
    colour = "Car type"
    )

df <- tibble(
  x = runif(10),
  y = runif(10)
)
ggplot(df, aes(x, y)) +
  geom_point() +
  labs(
    x = quote(sum(x[i] ^2, i ==1, n)),
    y = quote(alpha + beta + frac(delta, theta))
  )
?plotmath

best_in_class <- mpg %>%
  group_by(class) %>%
  filter(row_number(desc(hwy)) == 1)

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_text(
    aes(label = model), 
    data = best_in_class,
    nudge_y = 2,
    alpha = 0.5)

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_point(size = 3, shape = 1, data = best_in_class) +
  geom_label_repel(aes(label = model), data = best_in_class)

class_avg <- mpg %>%
  group_by(class) %>%
  summarize(
    displ = median(displ),
    hwy = median(hwy)
  )
ggplot(mpg, aes(displ, hwy, color = class)) +
  geom_label_repel(aes(label = class), data = class_avg, size = 6, label.size = 0, segment.color = NA) +
  geom_point() +
  theme(legend.position = "none")

label <- mpg %>%
  summarize(
    displ = max(displ), 
    hwy = max(hwy),
    label = paste("Increasing engine size is \n related to decreasing fuel economy.")
    )
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(
    aes(label = label),
    data = label,
    vjust = "top",
    hjust = "right"
  )

label <- tibble(
  displ = Inf,
  hwy = Inf,
  label = paste("Increasing engine size is \nrelated to decreasing fuel economy.")
)
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(
    aes(label = label),
    data = label, 
    vjust = "top",
    hjust = "right"
  ) +
  geom_hline(yintercept = median(mpg$hwy)) +
  geom_vline(xintercept = median(mpg$displ)) +
  geom_rect(aes(xmin = 5.6, xmax = 7.1, ymin = 22.5, ymax = 26.5), fill = NA, color = "black")

range_2seaters <- mpg %>%
  filter(class == "2seater") %>%
  summarise(
    max_hwy = max(hwy),
    min_hwy = min(hwy),
    max_displ = max(displ),
    min_displ = min(displ)
  )

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_y_continuous(breaks = seq(15, 40, by = 5))

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme(legend.position = "bottom") +
  guides(
    color = guide_legend(
      nrow = 1,
      override.aes = list(size = 4)
    )
  )

ggplot(diamonds, aes(carat, price)) +
  geom_bin2d() +
  scale_x_log10() +
  scale_y_log10()
