library(tidyverse)
library(here)

new_line <- function(line, angle, reduce = 1) {
  x0 <- line[1]
  y0 <- line[2]
  x1 <- line[3]
  y1 <- line[4]
  
  dx <- unname(x1 - x0)
  dy <- unname(y1 - y0)
  l <- sqrt(dx^2 + dy^2)
  
  theta <- atan(dy/dx) * 180 / pi
  rad <- (angle + theta) * pi / 180
  
  coeff <- sign(theta)*sign(dy)
  if(coeff == 0) coeff <- -1
  
  x2 <- x0 + coeff*l*cos(rad)*reduce + dx
  y2 <- y0 + coeff*l*sin(rad)*reduce + dy
  
  return(c(x1, y1, x2, y2))
}

iterate <- function(object, ifun, ...) {
  lines_list <- vector("list",0)
  for(i in 1:nrow(object)) {
    old_line <- matrix(object[i,], nrow=1)
    new_line <- ifun(old_line, ...)
    lines_list[[length(lines_list)+1]] <- new_line
  }
  
  do.call(rbind, lines_list)
}

koch <- function(line0) {
  
  # new triangle (starting at right)
  line1 <- new_line(line0, angle=180, reduce=1/3)
  line2 <- new_line(line1, angle=-60, reduce=1)
  line3 <- new_line(line2, angle=120, reduce=1)
  line4 <- new_line(line3, angle=-60, reduce=1)
  
  # reorder lines (to start at left)
  line1 <- line1[c(3,4,1,2)]
  line2 <- line2[c(3,4,1,2)]
  line3 <- line3[c(3,4,1,2)]
  line4 <- line4[c(3,4,1,2)]
  
  matrix(c(line4,line3,line2,line1), byrow=T, ncol=4)
  
}

A <- c(0,1e-9)
B <- c(3,5)
C <- c(6,0)
fractal <- matrix(c(A,B,B,C,C,A), nrow=3, byrow=T)

for(i in 1:4) fractal <- iterate(fractal, ifun=koch)

df <- tibble::as_tibble(fractal, .names_repair = "unique")
names(df) <- c("x", "y", "xend", "yend")
df <- df %>% 
  mutate(group = row_number())

p <- ggplot(df) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend, 
                   group = group, color = group), 
               show.legend = F, size = 0.3) +
  annotate("text", x = 3, y = 2, label = "Koch Snowflake", family = "Playfair Display", size = 20, fontface = "bold.italic", color = "white") +
  ggtext::geom_richtext(aes(x = 3, y = 1.5, label = "A fractal with **<span style='color: #C1A53A'>infinite perimeter</span>** but **<span style='color: #79D5DA'>finite area</span>**"),
                        family = "Playfair Display", size = 7, fontface = "italic", color = "white", label.color = NA, fill = NA) +
  # annotate("text", x = 3, y = 1.5, label = "A fractal with an infinite perimeter but a finite area", family = "Playfair Display", size = 7, fontface = "italic", color = "white") +
  scico::scale_color_scico(palette = "roma") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey20", color = NA)
  )

ragg::agg_png(here("day_04", "day_04.png"), width = 12, height = 12, units = "in", res = 320)
print(p)
dev.off()