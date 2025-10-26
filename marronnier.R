## load packages ---
library(tidyverse)

## load data ----
# Source: https://www.swissinfo.ch/eng/swiss-oddities/spring-begins-in-geneva-when-the-horse-chestnut-tree-blossoms/88756963
marronnier <- readr::read_csv(file = "marronnier.csv") |> 
  select(year, date) |> 
  mutate(date = lubridate::dmy(paste0(stringr::str_sub(date, 1, 6), year))) |> 
  mutate(first_of_jan = as.Date(paste0(year, "-01-01"))) |> 
  mutate(doy = as.numeric(date - first_of_jan)) |> 
  select(-first_of_jan)

# Fix year for the extraordinarily hot summer of 2003
marronnier[marronnier$date == as.Date("2002-12-29"), ]$year <- 2003
marronnier[marronnier$date == as.Date("2002-12-29"), ]$doy <- marronnier[marronnier$date == as.Date("2002-12-29"), ]$doy - 365

write_csv(x = marronnier, file = "marronnier_clean.csv")

## plot ----
marronnier_caption <- "https://ge.ch/grandconseil/secretariat/marronnier"

p <- ggplot(data = marronnier, mapping = aes(x = year, y = doy)) +
  geom_hline(yintercept = 0, linewidth = 0.5, colour = "darkgray") +
  geom_smooth(method = "loess", formula = "y ~ x", se = TRUE, colour = "#954535", fill = "#954535", alpha = 0.1, span = 0.1) +
  geom_line(linewidth = 0.5, colour = "gray", alpha = 0.1) +
  geom_point(size = 0.75) +
  scale_y_continuous(breaks = seq(-10, 110, 20), limits = c(-10, 110)) +
  scale_x_continuous(breaks = seq(1800, 2024, 25)) +
  labs(
    title = "Spring begins in Geneva when the official horse chestnut tree says so",
    subtitle = "Evolution of the dates of the appearance of the first official horse chestnut leaves of the year (1818-2024)",
    x = NULL, y = NULL,
    caption = str_wrap(marronnier_caption)
  ) +
  theme_minimal(base_size = 8) +
  theme(panel.grid.minor = element_blank())

print(p)

ggsave(filename = "marronnier.png", plot = p, path = ".", width = 18, height = 9, units = "cm", bg = "white")

## combine datasets ----
### Geneva horse chestnut tree ----
geneva_chestnut <- readr::read_csv("https://raw.githubusercontent.com/econmaett/marronnier/refs/heads/main/marronnier_clean.csv") |> 
  dplyr::mutate(date = as.Date(date), location = "Geneva")

### Liestal cherry blossom ----
liestal_cherry <- readr::read_csv("https://raw.githubusercontent.com/GMU-CherryBlossomCompetition/peak-bloom-prediction/refs/heads/main/data/liestal.csv") |> 
  dplyr::select(year, date = bloom_date, doy = bloom_doy) |> 
  dplyr::mutate(date = as.Date(date), location = "Liestal")

### Kyoto cherry blossom ----
kyoto_cherry <- readr::read_csv("https://raw.githubusercontent.com/GMU-CherryBlossomCompetition/peak-bloom-prediction/refs/heads/main/data/kyoto.csv") |> 
  dplyr::select(year, date = bloom_date, doy = bloom_doy) |> 
  dplyr::mutate(date = as.Date(date), location = "Kyoto")

### combine datasets ----
phenology <- dplyr::bind_rows(geneva_chestnut, liestal_cherry, kyoto_cherry)

## plot ----
phenology_caption <- "Vitasse, Y., Baumgarten, F., Zohner, C.M. et al. The great acceleration of plant phenological shifts. Nat. Clim. Chang. 12, 300–302 (2022). https://doi.org/10.1038/s41558-022-01283-y"

p <- ggplot(data = phenology |> filter(year >= 1900), mapping = aes(x = year, y = doy)) +
  geom_smooth(method = "loess", formula = "y ~ x", se = TRUE, span = 0.1, alpha = 0.1) +
  geom_line(linewidth = 0.5, alpha = 0.1) +
  geom_point(size = 1) +
  facet_grid(rows = vars(location), scale = "free_y") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1900, 2025, 25), limits = c(1900, 2025)) +
  labs(
    title = "The onset of phenological plant response to climate warming",
    subtitle = NULL,
    x = NULL, y = NULL,
    caption = str_wrap(phenology_caption)
  )

print(p)

ggsave(filename = "phenology.png", plot = p, path = ".", width = 14, height = 18, units = "cm", bg = "white")
