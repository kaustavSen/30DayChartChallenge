library(tidyverse)
library(ggforce)
library(ggfx)

body <- tribble(
  ~x, ~y,
   0.5, 3,
   0, 3.1,
  -0.5, 3,
  -0.55, 2.8,
  -0.55, 2.7,
  -0.75, 1,
  -0.8, 0.5,
  -0.75, 0,
   0.75, 0,
   0.75, 1,
   0.76, 0.5,
   0.64, 2.5,
   0.58, 2.8,
   0.56, 2.9,
   0.55, 2.95,
   0.5, 3
)

eyes <- tribble(
  ~x,    ~y, ~shape,
  -0.25, 2.5, "X",
   0.25, 2.5, "O"
)

nose <- tribble(
  ~x, ~y,
  -0.1, 2.2,
  0.1, 2.2,
  0, 2,
  -0.1, 2.2
)

mouth <- tribble(
  ~x, ~y,
  -0.2, 1.8,
  -0.1, 1.95,
   0  , 1.9,
   0.1, 1.9,
   0.2, 2.0
)

whiskers <- tribble(
  ~x, ~xend, ~y, ~yend, ~group,
  -0.5, -1, 2, 2, 1,
  -0.5, -1, 2.1, 2.3, 2,
  -0.5, -1, 1.9, 1.7, 3,
   0.5,  1, 2, 2, 1,
   0.5,  1, 2.1, 2.3, 2,
   0.5,  1, 1.9, 1.7, 3,
)

tails <- tribble(
  ~x,     ~y, ~group, ~color,
  -1.5,   0.3,      1, "grey70", 
  -1,     0.5,      1, "grey70",
  -0.9,   0.1,      1, "grey70",
  -0.75,  0.3,      1, "grey70",
   1.5,   0.5,      2, "black",
   1,     0.3,      2, "black",
   0.9,   0.1,      2, "black",
   0.64,  0.3,      2, "black"
)

ears <- tribble(
  ~x, ~y, group,
  
)

p <- 
  ggplot() +
  geom_bspline_closed(data = body, aes(x = x, y = y), color = NA, fill = "white") +
  as_reference(
    geom_polygon(aes(x = c(-1, 0, 0, -1), y = c(0, 0, 3.2, 3.2)), fill = "grey70"),
    id = "bg"
  ) +
  with_blend(
    geom_bspline_closed(data = body, aes(x = x, y = y), color = NA, fill = "grey70"),
    bg_layer = "bg",
    blend_type = "in"
  ) +
  geom_bspline(data = body, aes(x = x, y = y)) +
  geom_point(data = eyes, aes(x = x, y = y, shape = shape), size = 12) +
  geom_segment(aes(x = 0, xend = 0, y = 2.2, yend = 1.92)) +
  geom_polygon(data = nose, aes(x = x, y = y), fill = "#f8edeb", color = NA) +
  geom_bspline(data = mouth, aes(x = x, y = y)) +
  geom_segment(data = whiskers, aes(x = x, xend = xend, y = y, yend = yend, group = group)) +
  geom_bspline(data = tails, aes(x = x, y = y, group = group, color = color), size = 1.5) +
  scale_shape_identity() +
  scale_color_identity() +
  labs(
    title = "Schrödinger's cat",
    subtitle = "Is it dead or alive?",
    caption = "#30DayChartChallenge | Day 6: Experimental | Plot: Kaustav Sen"
  ) +
  geom_point() +
  theme_void(base_size = 14) +
  theme(
    plot.margin = margin(t = 15, b = 10),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(family = "Impact", size = rel(2.5), hjust = 0.5),
    plot.subtitle = element_text(family = "Roboto Condensed", size = rel(1.5), hjust = 0.5, color = "grey40", face = "bold"),
    plot.caption = element_text(family = "Roboto Condensed", size = rel(0.8), hjust = 0.5, color = "grey40"),
    
  )

ragg::agg_png(here::here("day_06", "day_06.png"), width = 11, height = 8, units = "in", res = 320)
print(p)
dev.off()
