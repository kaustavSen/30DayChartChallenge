library(tidyverse)
library(readxl)
library(lubridate)
library(colorspace)
library(patchwork)
library(ggforce)
library(cowplot)
library(here)

files_full <- list.files(path = here("day_11", "data"), full.names = TRUE)
files <- list.files(path = here("day_11", "data"), full.names = FALSE)
file_dates <- str_remove_all(files, pattern = "[a-zA-Z _\\:/]") %>% dmy()

read_excel_df <- function(file_path, file_date) {
  df <- read_excel(
    path = file_path,
    sheet = 1,
    range = "B166:D167",
    col_names = FALSE
  )
  
  if (nrow(df) == 0) {
    df <- read_excel(
      path = file_path,
      sheet = 3,
      range = "B166:D167",
      col_names = FALSE
    )
  }
  
  colnames(df) <- c("Prem_Type", "FY2020", "FY2021")
  
  df %>% mutate(date = file_date)
  
}

prem_data_df <- pmap_dfr(tibble(file_path = files_full, file_date = file_dates), read_excel_df)

prem_data_df <- 
  prem_data_df %>% 
  mutate(
    month = month(date, label = TRUE),
    # Assign a weight of 10% to single premiums
    FY2020 = if_else(Prem_Type == "Individual Single Premium", 0.1 * FY2020, FY2020),
    FY2021 = if_else(Prem_Type == "Individual Single Premium", 0.1 * FY2021, FY2021)
  ) %>% 
  group_by(month) %>% 
  summarise(
    monthly_prem_fy2020 = sum(FY2020),
    monthly_prem_fy2021 = sum(FY2021),
    .groups = "drop"
  ) %>% 
  mutate(
    avg_monthly_prem_fy2020 = mean(monthly_prem_fy2020),
    avg_monthly_prem_fy2021 = mean(monthly_prem_fy2021)
  )

# Plot FY2020 -------------------------------------------------------------

last_2020 <- prem_data_df$monthly_prem_fy2020[[12]]

df_2020 <- as_tibble(
  spline(x = c(0, as.numeric(prem_data_df$month)),
         y = c(last_2020, prem_data_df$monthly_prem_fy2020),
         n = 5000)
)

p <- df_2020 %>% 
  mutate(avg_y = prem_data_df$avg_monthly_prem_fy2020[1]) %>% 
  ggplot() +
  geom_hline(data = tibble(yintercept = c(2000, 4000, 6000)),
             aes(yintercept = yintercept), 
             size = 0.4, color = "grey90") +
  geom_label(
    data = tibble(x = 7, y = c(2000, 4000, 6000), label = c(20, 40, 60)),
    aes(x = x, y = y, label = label), size = 3.5, color = "grey80",
    family = "Roboto Condensed", label.size = 0
  ) +
  geom_ribbon(aes(x = x, ymin = avg_y, ymax = y, fill = y > avg_y, 
                  color = after_scale(darken(fill, 0.2))), size = 0.5, 
              show.legend = FALSE, alpha = 0.8) +
  geom_text(
    data = tibble(x = seq(0, 9, 3), y = 4200, label = c("Dec", "Mar", "Jun", "Sep")),
    aes(x = x, y = y, label = label), 
    family = "Roboto Condensed", fontface = "bold", size = 5, 
    color = "grey20", alpha = 0.5
  ) +
  geom_hline(aes(yintercept = avg_y), size = 1.5, color = "#e9c46a") +
  scale_y_continuous(limits = c(800, 7000)) +
  scale_fill_manual(values = c("#f4a261", "#2a9d8f")) +
  coord_polar() +
  theme_void()


# Plot FY2021 -------------------------------------------------------------

last_2021 <- prem_data_df$monthly_prem_fy2021[[12]]

df_2021 <- as_tibble(
  spline(x = c(0, as.numeric(prem_data_df$month)),
         y = c(last_2021, prem_data_df$monthly_prem_fy2021),
         n = 5000)
)

p2 <- 
  df_2021 %>% 
  mutate(avg_y = prem_data_df$avg_monthly_prem_fy2021[1]) %>% 
  ggplot() +
  geom_hline(data = tibble(yintercept = c(2000, 4000, 6000)),
             aes(yintercept = yintercept), 
             size = 0.4, color = "grey90") +
  geom_ribbon(aes(x = x, ymin = avg_y, ymax = y, fill = y > avg_y,
                  color = after_scale(darken(fill, 0.2))),
              size = 0.5, show.legend = FALSE, alpha = 0.8) +
  geom_text(
    data = tibble(x = seq(0, 9, 3), y = 4200, label = c("Dec", "Mar", "Jun", "Sep")),
    aes(x = x, y = y, label = label), 
    family = "Roboto Condensed", fontface = "bold", size = 5, 
    color = "grey20", alpha = 0.5
  ) +
  geom_mark_circle(
    aes(x = 10, y = avg_y, label = "Monthly\nAverage"),
    expand = unit(0.1, "mm"),
    con.cap = unit(0, "mm"),
    con.colour = "#e9c46a",
    con.type = "elbow",
    con.size = 0.7,
    label.lineheight = 0.75,
    label.fontsize = 11,
    label.buffer = unit(15, "mm"),
    label.colour = "#e9c46a",
    label.family = "Roboto Condensed"
  ) +
  geom_text(
    aes(x = 1.5, y = 4950, 
        label = str_wrap("January to March produce the highest sales for insurers but the pandemic seems to have subdued this.", width = 30)),
    lineheight = 1,
    color = "#2a9d8f",
    hjust = 0,
    size = 3.5,
    family = "Roboto Condensed"
  ) +
  geom_label(
    data = tibble(x = 7, y = c(2000, 4000, 6000), label = c(20, 40, 60)),
    aes(x = x, y = y, label = label), size = 3.5, color = "grey80",
    family = "Roboto Condensed", label.size = 0
  ) +
  geom_hline(aes(yintercept = avg_y), size = 1.5, color = "#e9c46a") +
  scale_y_continuous(limits = c(800, 7000)) +
  scale_fill_manual(values = c("#f4a261", "#2a9d8f")) +
  coord_polar() +
  theme_void()

p1_final <- ggdraw() +
  draw_plot(p, scale = 1.5) +
  draw_text("FY2020", x = 0.51, y = 0.98, family = "Impact",
            size = 45, color = "#a8dadc") +
  draw_text("Pre Pandemic", x = 0.51, y = 0.92, family = "Roboto Condensed",
            size = 15, color = "grey80")

p2_final <- ggdraw() +
  draw_plot(p2, scale = 1.5) + 
  draw_text("FY2021", x = 0.51, y = 0.98, family = "Impact",
            size = 45, color = "#a8dadc") +
  draw_text("During Pandemic", x = 0.51, y = 0.92, family = "Roboto Condensed",
            size = 15, color = "grey80")


ragg::agg_png(here("day_11", "day_11.png"), height = 8, width = 12, 
              units = "in", res = 320)
print(
  p2_final + p1_final +
    plot_annotation(
      title = "How has the pandemic impacted the Indian Life Insurance industry?",
      subtitle = "New Business premium collections by Private Insurers (in INR Billions)",
      "#30DayChartChallenge | Day 11: Circular | Data: IRDAI | Plot: Kaustav Sen"
    ) &
    theme(
      plot.margin = margin(15, 25, 15, 25),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_text(family = "Roboto Condensed", size = rel(2.5), 
                                hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(family = "Roboto Condensed", size = rel(1.5), 
                                hjust = 0.5, face = "bold", color = "grey70"),
      plot.caption = element_text(family = "Roboto Condensed", size = rel(1), 
                                hjust = 0.5, face = "bold", color = "grey70")
    )
)
dev.off()
