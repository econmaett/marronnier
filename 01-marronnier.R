# 01-marronnier.R ----

## load packages ---
library(tidyverse)
library(slider)

## load maronnier ----
# Source: https://www.swissinfo.ch/eng/swiss-oddities/spring-begins-in-geneva-when-the-horse-chestnut-tree-blossoms/88756963
marronnier <- readr::read_csv(file = "raw_marronnier.csv") |> 
  select(year, date) |> 
  mutate(date = lubridate::dmy(paste0(stringr::str_sub(date, 1, 6), year))) |> 
  mutate(first_of_jan = as.Date(paste0(year, "-01-01"))) |> 
  mutate(doy = as.numeric(date - first_of_jan)) |> 
  select(-first_of_jan)

# Fix year for the extraordinarily hot summer of 2003
marronnier[marronnier$date == as.Date("2002-12-29"), ]$year <- 2003
marronnier[marronnier$date == as.Date("2002-12-29"), ]$doy <- marronnier[marronnier$date == as.Date("2002-12-29"), ]$doy - 365

write_csv(x = marronnier, file = "clean_marronnier.csv")

## plot ----
marronnier_caption <- "Source: https://ge.ch/grandconseil/secretariat/marronnier"

p1 <- ggplot(data = marronnier, mapping = aes(x = year, y = doy)) +
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

print(p1)

ggsave(filename = "fig_marronnier.png", plot = p1, path = ".", width = 18, height = 9, units = "cm", bg = "white")

## load all five series ----
### Northern Switzerland, Flowering, cherry tree----
northern_switzerland <- readr::read_csv("https://raw.githubusercontent.com/GMU-CherryBlossomCompetition/peak-bloom-prediction/refs/heads/main/data/liestal.csv") |> 
  select(year, date = bloom_date, doy = bloom_doy) |> 
  mutate(
    date = as.Date(date), 
    description_short = "Flowering, cherry tree, Northern Switzerland", 
    description_long = "Flowering dates of the wild cherry tree (Prunus avium L.) in Liestal, Switzerland (1894–2024)"
  )

### Eastern China, Flowering, three woody species ----
eastern_china <- readxl::read_excel(path = "raw_marsham-combes-shrub-index-china.xlsx") |> 
  rename_with(.fn = tolower, .cols = everything()) |> 
  filter(country == "China") |> 
  rename(location = country) |> 
  mutate(
    date = as.Date(paste0(year, "-01-01")) + days(round(doy)), 
    description_short = "Flowering, three woody species, Eastern China", 
    description_long = "Spring flowering index derived from three woody species (flowering of Amygdalus davidiana (Carrière) Franch., Cercis chinensis Bunge and Paeonia suffruticosa Andrews) in China (1834–2020)"
  ) |> 
  select(year, date, doy, location, description_short, description_long)

### Western Switzerland, Budburst, Horse chestnut ----
western_switzerland <- readr::read_csv("https://raw.githubusercontent.com/econmaett/marronnier/refs/heads/main/clean_marronnier.csv") |> 
  mutate(
    date = as.Date(date), 
    description_short = "Budburst, Horse chestnut, Western Switzerland", 
    description_long = "Budburst dates of horse chestnut (Aesculus hippocastanum) in Geneva, Switzerland (1808–2024)"
  )

### Southern England, Leaf out, pedunculate oak ----
southern_england <- readxl::read_excel(path = "raw_marsham-combes-shrub-index-china.xlsx") |> 
  rename_with(.fn = tolower, .cols = everything()) |> 
  filter(country == "UK", group == "Tree", species == "Quercus robur") |> 
  rename(location = country) |> 
  mutate(
    date = as.Date(paste0(year, "-01-01")) + days(round(doy)), 
    description_short = "Leaf out, pedunculate oak, Southern UK", 
    description_long = "Budburst dates of pedunculate oak (Quercus robur L.) in SE UK from the Marsham series (1736–1958) and J. Combes series (1950–2020)"
  ) |> 
  select(year, date, doy, location, description_short, description_long)

### Japan, Flowering, cherry tree ----
japan <- readr::read_csv("https://raw.githubusercontent.com/GMU-CherryBlossomCompetition/peak-bloom-prediction/refs/heads/main/data/kyoto.csv") |> 
  select(year, date = bloom_date, doy = bloom_doy) |> 
  mutate(
    date = as.Date(date), 
    description_short = "Flowering, cherry tree, Japan", 
    description_long = "Blooming of the yamazakura cherry tree (Prunus jamasakura Lindl.) in Kyoto in Japan (812–2024)"
  )

### combine datasets ----
phenology_levels <- c(
  "Flowering, cherry tree, Northern Switzerland",
  "Flowering, three woody species, Eastern China",
  "Budburst, Horse chestnut, Western Switzerland",
  "Leaf out, pedunculate oak, Southern UK",
  "Flowering, cherry tree, Japan"
)

phenology <- bind_rows(northern_switzerland, eastern_china, western_switzerland, southern_england, japan) |> 
  mutate(description_short = factor(description_short, levels = phenology_levels)) |> 
  group_by(description_short) |> 
  arrange(year) |> 
  mutate(ma = slider::slide_dbl(.x = doy, .f = ~mean(.x, na.rm = FALSE), .before = 9, .step = 1L, .complete = TRUE)) |> 
  ungroup()


## plot ----
phenology_caption <- "Vitasse, Y., Baumgarten, F., Zohner, C.M. et al. The great acceleration of plant phenological shifts. Nat. Clim. Chang. 12, 300–302 (2022). https://doi.org/10.1038/s41558-022-01283-y"

df_annotate <- data.frame(
  year = rep(1980, 5),
  doy = rep(105, 5),
  description_short = factor(phenology_levels),
  label = c(NA,"The great\nacceleration", rep(NA, 3))
)

df_annotate

p2 <- ggplot(data = phenology, mapping = aes(x = year)) +
  geom_rect(mapping = aes(xmin = 1950, xmax = 1985, ymin = -Inf, ymax = Inf), fill = "#ffef9f", alpha = 0.01, inherit.aes = FALSE) +
  geom_rect(mapping = aes(xmin = 1985, xmax = 2025, ymin = -Inf, ymax = Inf), fill = "#ffef9f", alpha = 0.02, inherit.aes = FALSE) +
  geom_line(mapping = aes(y = doy), linewidth = 0.5, alpha = 0.5, colour = "darkgray") +
  geom_point(mapping = aes(y = doy), size = 0.5) +
  geom_line(mapping = aes(y = ma, colour = description_short), linewidth = 0.75) +
  scale_x_continuous(breaks = seq(1500, 2000, 50), limits = c(1500, NA), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "Trees’ bloom and budbreak earlier and earlier as climate change accelerates",
    subtitle = NULL,
    x = NULL, y = "Spring phenology (day of the year)",
    caption = str_wrap(phenology_caption)
  ) +
  facet_wrap(~description_short, scales = "free_y", nrow = 5) +
  scale_color_manual(
    values = c("#036666", "#00bbf9", "#9a48d0", "#f17300", "#a4243b")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.placement = "outside",
    strip.text = element_text(hjust = 0),
    legend.position = "none",
    title = element_text(vjust = 0),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0, "lines")
  ) +
  geom_text(data = df_annotate, mapping = aes(x = year, y = doy, label = label), inherit.aes = TRUE, size = 3, vjust = 0)

print(p2)

ggsave(filename = "fig_phenology.png", plot = p2, path = ".", width = 22, height = 20, units = "cm", bg = "white")

