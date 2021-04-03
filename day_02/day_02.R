library(tidyverse)
library(patchwork)
library(here)

responses <- read_csv(here("day_02", "data", "responses.csv")) %>% janitor::clean_names()
genres <- c("musical", "latino", "hiphop_rap", "pop")

music_preferences <- 
  responses %>% 
  filter(!is.na(gender)) %>% 
  select(slow_songs_or_fast_songs:opera, gender) %>% 
  mutate(student_id = row_number()) %>% 
  pivot_longer(cols = slow_songs_or_fast_songs:opera, names_to = "song_type", values_to = "response") %>% 
  filter(response %in% c(1, 5)) %>% 
  count(gender, song_type, response) %>% 
  group_by(gender, song_type) %>% 
  mutate(prop = round((n / sum(n))*10, 0)) %>% 
  ungroup() 

df <- 
  music_preferences %>% 
  filter(response == 5) %>% 
  select(gender, song_type, prop, response) %>% 
  pivot_wider(names_from = gender, values_from = prop) %>% 
  filter(song_type %in% genres) %>% 
  rowwise() %>% 
  mutate(
    song_type = factor(song_type, levels = c("hiphop_rap", "pop", "latino", "musical")),
    female_total = str_flatten(rep("k", 10)),
    female_like = str_flatten(rep("k", female)),
    male_total = str_flatten(rep("A", 10)),
    male_like = str_flatten(rep("A", male))
  )

labels_female <- 
  tribble(
    ~song_type, ~label,
    "pop", "9 out of 10 women like <span style='color: #ff6b6b'>**Pop**</span>",
    "musical", "6 out of 10 women like <span style='color: #1a535c'>**Musical**</span>",
    "latino", "6 out of 10 women like <span style='color: #4ecdc4'>**Latino**</span>",
    "hiphop_rap", "3 out of 10 women like <span style='color: #ffe66d'>**Hip Hop**</span>"
  )

labels_male <- 
  tribble(
    ~song_type, ~label,
    "pop", "6 out of 10 men like <span style='color: #ff6b6b'>**Pop**</span>",
    "musical", "1 out of 10 men like <span style='color: #1a535c'>**Musical**</span>",
    "latino", "2 out of 10 men like <span style='color: #4ecdc4'>**Latino**</span>",
    "hiphop_rap", "6 out of 10 men like <span style='color: #ffe66d'>**Hip Hop**</span>"
  )

p_female <- 
  ggplot(df, aes(x = 1, y = song_type)) +
  geom_text(aes(label = female_total), family = "WeePeople", size = 25, color = "grey90", hjust = 0) +
  geom_text(aes(label = female_like, color = song_type), 
            family = "WeePeople", size = 25, show.legend = FALSE, hjust = 0) +
  ggtext::geom_richtext(data = labels_female,
            aes(x = 1.02, label = label), nudge_y = -0.4, hjust = 0, size = 6, family = "Roboto Condensed", label.color = NA) +
  scale_x_continuous(limits = c(1, 2)) +
  scale_color_manual(values = c("musical" = "#1a535c", "latino" = "#4ecdc4", "pop" = "#ff6b6b", "hiphop_rap" = "#ffe66d")) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_void() 

p_male <- 
  ggplot(df, aes(x = 1, y = song_type)) +
  geom_text(aes(label = male_total), family = "WeePeople", size = 25, color = "grey90", hjust = 0) +
  geom_text(aes(label = male_like, color = song_type), 
            family = "WeePeople", size = 25, show.legend = FALSE, hjust = 0) +
  ggtext::geom_richtext(data = labels_male,
            aes(x = 1.02, label = label), nudge_y = -0.4, hjust = 0, size = 6, family = "Roboto Condensed", label.color = NA) +
  scale_x_continuous(limits = c(1, 2)) +
  scale_color_manual(values = c("musical" = "#1a535c", "latino" = "#4ecdc4", "pop" = "#ff6b6b", "hiphop_rap" = "#ffe66d")) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_void() 

p <- p_female + p_male +
  plot_annotation(
    title = "What are your musical preferences?",
    subtitle = "Based on a 2013 survey of students of the Statistics class at FSEV UK",
    caption = "Data: Kaggle | Plot: Kaustav Sen"
  ) &
  theme(
    plot.margin = margin(20, 20, 10, 20),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(family = "Playfair Display", size = 21, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(family = "Roboto Condensed", color = "grey30", size = 14, hjust = 0.5, margin = margin(b = 30)),
    plot.caption = element_text(family = "Roboto Condensed", size = 12, hjust = 0.5, margin = margin(t = 15))
  )

ragg::agg_png(here("day_02", "day_02.png"), width = 14, height = 8, units = "in", res = 320)
print(p)
dev.off()
