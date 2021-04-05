library(tidyverse)
library(ggtext)

x_range <- seq(-8*pi, 8*pi, length.out = 500)
y_sine <- sin(x_range)
y_cosine <- cos(x_range)
df <- tibble(x_range, y_sine, y_cosine)

p <- ggplot(df, aes(x_range)) +
  geom_path(aes(y = y_sine), color = "#f07167", size = 1.1, alpha = 0.8) +
  geom_path(aes(y = y_cosine), color = "#fed9b7", size = 1.1, alpha = 0.8) +
  scale_y_continuous(limits = c(-1.5, 1.5)) +
  labs(title = "<span style='color: #f07167'>Sine</span> and its slope <span style='color: #fed9b7'>Cosine</span>",
       caption = "#30DayChartChallenge | Day 5: Slope | Plot: Kaustav Sen") +
  theme_void(base_size = 14) +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "#FFFFEC", color = "#FFFFEC"),
    plot.title = element_markdown(family = "Teko", face = "bold", hjust = 0.5, size = rel(1.5)),
    plot.caption = element_text(family = "Teko", hjust = 0.5, size = rel(0.7), color = "grey60"),
    plot.margin = margin(10, 0, 10, 0)
  )

ragg::agg_png(here::here("day_05", "day_05.png"), width = 9, height = 3, units = "in", res = 320)
print(p)
dev.off()