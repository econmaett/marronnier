## load packages ---
library(tidyverse)

## load data ----
# Source: https://www.swissinfo.ch/eng/swiss-oddities/spring-begins-in-geneva-when-the-horse-chestnut-tree-blossoms/88756963
maronnier <- readr::read_csv(file = "maronnier.csv") |> 
  select(year, date) |> 
  mutate(date = lubridate::dmy(paste0(stringr::str_sub(date, 1, 6), year))) |> 
  mutate(first_of_jan = as.Date(paste0(year, "-01-01"))) |> 
  mutate(doy = as.numeric(date - first_of_jan)) |> 
  select(-first_of_jan)

# Fix year for the extrarodinarily summer of 2003
maronnier[maronnier$date == as.Date("2002-12-29"), ]$year <- 2003
maronnier[maronnier$date == as.Date("2002-12-29"), ]$doy <- maronnier[maronnier$date == as.Date("2002-12-29"), ]$doy - 365

write_csv(x = maronnier, file = "maronnier_clean.csv")

## plot ----
maronnier_caption <- "https://ge.ch/grandconseil/secretariat/marronnier"

p <- ggplot(data = maronnier, mapping = aes(x = year, y = doy)) +
  geom_hline(yintercept = 0, linewidth = 0.5, colour = "darkgray") +
  geom_smooth(method = "loess", formula = "y ~ x", se = FALSE, colour = "#bc6c25") +
  geom_smooth(method = "loess", formula = "y ~ x", se = TRUE, colour = "#dda15e") +
  geom_line(linewidth = 0.5, colour = "#606c38") +
  geom_point(size = 0.75, colour = "#283618") +
  scale_x_continuous(breaks = seq(1800, 2025, 25), limits = c(1800, 2025)) +
  scale_y_continuous(breaks = seq(-20, 120, 20), limits = c(-20, 120)) +
  labs(
    title = "Spring begins in Geneva when the official horse chestnut tree says so",
    subtitle = "Evolution of the dates of the appearance of the first official horse chestnut leaves of the year (1818-2024)",
    x = NULL, y = NULL,
    caption = str_wrap(maronnier_caption)
  ) +
  theme_minimal(base_size = 6)

print(p)

ggsave(filename = "maronnier.png", plot = p, path = ".", width = 12, height = 6, units = "cm", bg = "white")

