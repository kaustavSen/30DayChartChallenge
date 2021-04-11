library(tidyverse)
library(ggforce)

set.seed(123)

# Formula for generating fibonacci numbers: 
# https://stackoverflow.com/questions/62090178/use-mapv-reduce-to-create-fibonacci-sequence-in-r

fibo <- 
  accumulate(
  .init = c(0L, 1L),
  rep(0, 18),
  ~c(.x, sum(.x))[2:3]
) %>% 
  map_int(~pluck(.x, 1)) %>% 
  .[-1:-10]

unif_wall <- 
  tibble(fibo_number = 1:9) %>% 
  mutate(sample = list(runif(1000, min = 0, max = 5))) %>% 
  unnest(sample)

p <- 
  tibble(fibo) %>% 
  rowwise() %>% 
  mutate(sample = list(rexp(fibo, 2))) %>% 
  unnest(sample) %>% 
  mutate(
    fibo_number = as.numeric(as.factor(fibo)),
    fibo_label = case_when(
      fibo_number == 1 ~ "f",
      fibo_number == 2 ~ "i",
      fibo_number == 3 ~ "b",
      fibo_number == 4 ~ "o",
      fibo_number == 5 ~ "n",
      fibo_number == 6 ~ "a",
      fibo_number == 7 ~ "c",
      fibo_number == 8 ~ "c",
      TRUE ~ "i",
    )
  ) %>%
  group_by(fibo_number) %>% 
  mutate(avg = mean(sample)) %>% 
  ggplot(aes(x = fibo_number, y = sample, fill = as.factor(fibo))) +
  geom_jitter(data = unif_wall, aes(x = fibo_number, y = sample),
              shape = 22, color = "white", fill = "grey90", stroke = 1, 
              size = 5, alpha = 0.3, show.legend = FALSE) +
  geom_jitter(shape = 22, color = "white", stroke = 1, 
              size = 5, alpha = 0.6, show.legend = FALSE) +
  geom_text(aes(x = fibo_number, y = avg, label = fibo_label, 
                color = after_scale(colorspace::darken(fill, 0.4))), 
            size = 20, family = "Bauhaus 93") +
  ggsci::scale_fill_uchicago() +
  labs(
    caption = "#30DayChartChallenge | Day 10: Abstract | Plot: Kaustav Sen"
  ) +
  theme_void() +
  theme(
    plot.caption.position = "plot",
    plot.caption = element_text(family = "Roboto Condensed", size = 12, face = "bold", 
                                color = "grey40", hjust = 0.5, margin = margin(b = 10))
  )


ragg::agg_png(here::here("day_10", "day_10.png"), 
              height = 10, width = 6, units = "in", res = 320)
print(p)
dev.off()
