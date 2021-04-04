library(tidyverse)
library(systemfonts)

register_variant(
  name = "Playfair Custom",
  family = "Playfair Display",
  features = font_feature(ligatures = "discretionary", kern = 1)
)

df <- tribble(
  ~year, ~book_author, ~quote,
  1859, "A tale of two cities, Charles Dickens", "It was the best of times, it was the worst of times, it was the age of wisdom, it was the age of foolishness",
  1936, "Gone with the Wind, Margaret Mitchell", "Frankly, my dear, I don't give a damn",
  1949, "1984, George Orwell", "Freedom is the freedom to say that two plus two makes four",
  1969, "The Godfather, Mario Puzo", "I’m gonna make him an offer he can’t refuse",
  2007, "A thousand splendid suns, Khaled Hosseini", "Of all the hardships a person had to face, none was more punishing than the simple act of waiting"
)

label_segments <- tibble(
  label = glue::glue("*{df$quote}*<br /><span style='font-family: Roboto Condensed; color: grey; font-size: 8px'>**{df$book_author}**</span>"),
  x = df$year,
  xend = x + 6 * rep(c(1,-1), length.out = 5),
  y = 1,
  yend = 1 * rep(c(0.7, 1.3), length.out = 5),
  x2 = xend + 10 *  rep(c(1,-1), length.out = 5),
  hjust = rep(c(0.06, 0.4), length.out = 5),
  vjust = rep(c(0.98, 0.07), length.out = 5)
)

p <- ggplot(df, aes(x = year, y = 1)) +
  geom_segment(data = label_segments,
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "grey70", size = 0.75) +
  geom_segment(data = label_segments,
               aes(x = xend, y = yend, xend = x2, yend = yend),
               color = "grey70", size = 0.75) +
  geom_line(color = "#1d3557", size = 2) +
  geom_point(shape = 21, size = 6, stroke = 1.5, fill = "#a8dadc", color = "#457b9d") +
  geom_text(aes(label = year), nudge_y = 0.15*rep(c(1,-1), length.out = 5), family = "Limelight", size = 4) +
  ggtext::geom_textbox(data = label_segments,
                       aes(x = xend, y = yend, label = label, hjust = hjust, vjust = vjust),
                       width = unit(40, "mm"), box.color = NA, fill = NA, 
                       size = 2.8, family = "Playfair Custom", lineheight = 1.1) +
  scale_y_continuous(limits = c(0, 2)) +
  labs(
    title = "Quotable quotes from some best-selling books over the years",
    caption = "Data: From memory + Googling | Plot: Kaustav Sen"
  ) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(family = "Cormorant Upright", face = "bold", size = rel(2.5), hjust = 0.5),
    plot.caption = element_text(family = "Roboto Condensed", size = rel(0.9), hjust = 0.5, color = "grey60"),
    plot.margin = margin(t = 10, b = 10, l = 30, r = 80)
  )

ragg::agg_png(here::here("day_03", "day_03.png"), 
              width = 12, height = 4, units ="in", res = 320)
print(p)
dev.off()
