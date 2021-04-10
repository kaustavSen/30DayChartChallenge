library(tidyverse)
library(scico)
library(ggfx)
library(patchwork)

set.seed(200)

# Beta --------------------------------------------------------------------

df_beta <- tibble(
  x = rbeta(8000, shape1 = 2, shape2 = 5),
) %>% 
  mutate(group = rep(1:800, each = 10))

df_beta_norm <- 
  df_beta %>% 
  group_by(group) %>% 
  summarise(
    x_mean = mean(x),
  )

p_1 <- 
  ggplot(filter(df_beta, group <= 300), aes(x, fill = group)) +
  as_reference(geom_text(aes(x = 0.1, y = 0.3, label = "BETA"), 
                         hjust = 0, size = 45, family = "Impact", color = "grey60"), 
               id = "text") +
  with_blend(
    ggdist::geom_dots(color = NA, show.legend = FALSE),
    bg = "text",
    blend_type = "out"
  ) +
  scale_fill_scico(palette = "lajolla") +
  theme_void()

p_2 <- 
  ggplot(df_beta_norm, aes(x_mean, fill = group)) +
  ggdist::geom_dots(color = NA, show.legend = FALSE) +
  scale_fill_scico(palette = "lajolla") +
  theme_void()

# Uniform -----------------------------------------------------------------

df_unif <- tibble(
  x = runif(8000),
  y = runif(8000)
) %>% 
  arrange(x, y) %>% 
  mutate(group = rep(1:800, each = 10))

df_unif_norm <- 
  df_unif %>% 
  group_by(group) %>% 
  summarise(
    x_mean = mean(x),
    y_mean = mean(y)
  ) %>% 
  pivot_longer(-group)


p_3 <- 
  ggplot(df_unif, aes(x, y, fill = group)) +
  as_reference(geom_text(aes(x = 0.1, y = 0.3, label = "UNIFORM"), 
                         hjust = 0, size = 45, family = "Impact", color = "grey60"), 
               id = "text") +
  with_blend(
    geom_point(shape = 21, color = "white", stroke = 0.5, size = 2.5, show.legend = FALSE),
    bg = "text",
    blend_type = "out"
  ) +
  scale_fill_scico(palette = "lajolla") +
  theme_void()

p_4 <- 
  ggplot(df_unif_norm, aes(value, fill = group)) +
  ggdist::geom_dots(color = NA, show.legend = FALSE) +
  scale_fill_scico(palette = "lajolla") +
  theme_void()

# Expoential --------------------------------------------------------------

df_expo <- tibble(
  x = rexp(8000, rate = 1),
) %>% 
  mutate(group = rep(1:800, each = 10))

df_expo_norm <- 
  df_expo %>% 
  group_by(group) %>% 
  summarise(
    x_mean = mean(x),
  )

p_5 <- 
  ggplot(filter(df_expo, group <= 200), aes(x, fill = group)) +
  as_reference(geom_text(aes(x = 0.1, y = 0.3, label = "EXPONENTIAL"), 
                         hjust = 0, size = 45, family = "Impact", color = "grey80"), 
               id = "text") +
  with_blend(
    ggdist::geom_dots(color = NA, show.legend = FALSE),
    bg = "text",
    blend_type = "xor"
  ) +
  scale_fill_scico(palette = "lajolla") +
  theme_void()

p_6 <- 
  ggplot(df_expo_norm, aes(x_mean, fill = group)) +
  ggdist::geom_dots(color = NA, show.legend = FALSE) +
  scale_fill_scico(palette = "lajolla") +
  theme_void()

# Combine Plots -----------------------------------------------------------

ragg::agg_png(here::here("day_09", "day_09.png"), 
              width = 24, height = 18, units = "in", res = 320)
print(
  (p_1 + p_2) / (p_3 + p_4) / (p_5 + p_6)  +
    plot_annotation(
      title = "Give me a distribution, and I'll average it out to be normal\n— The Central Limit Theorem —",
      caption = "#30DayChartChallenge | Day 6: Experimental | Plot: Kaustav Sen"
    ) &
    theme(
      plot.margin = margin(15, 15, 15, 15),
      plot.background = element_rect(fill = "grey85", color = "grey85"),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_text(family = "Abril Fatface", size = 45, hjust = 0.5, color = "grey40"),
      plot.caption = element_text(family = "Roboto Condensed", size = 18, hjust = 0.5, color = "grey70")
    )
)
dev.off()