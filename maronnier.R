library(tidyverse)

maronnier <- readr::read_csv(file = "maronnier.csv") |> 
  select(year, date) |> 
  mutate(date = lubridate::dmy(paste0(stringr::str_sub(date, 1, 6), year))) |> 
  mutate(first_of_jan = as.Date(paste0(year, "-01-01"))) |> 
  mutate(doy = as.numeric(date - first_of_jan)) |> 
  select(-first_of_jan)

maronnier[maronnier$date == as.Date("2002-12-29"), ]$year <- 2003
maronnier[maronnier$date == as.Date("2002-12-29"), ]$doy <- maronnier[maronnier$date == as.Date("2002-12-29"), ]$doy - 365

write_csv(x = maronnier, file = "maronnier_clean.csv")
