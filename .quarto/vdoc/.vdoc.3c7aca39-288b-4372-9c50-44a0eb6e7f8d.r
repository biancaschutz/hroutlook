#
#
#
#
#
#
library(tidyverse)
library(here)
source(here("consts.R"))

example_data <- data.frame(r = rep(c("Cat 1", "Cat 2", "Cat 3", "Cat 4", "Cat 5", "Cat 6", "Cat 7"), each = 3), v = c(8, 8, 5, 14, 10, 12, 3, 5, 2, 8, 7, 6, 10, 11, 11, 5, 6, 4, 8, 5, 6), y = rep(2023:2025, times  = 7))

example_data |>
ggplot(aes(x = factor(y), y = v)) + 
geom_line(aes(color = r, group = r)) +
scale_color_manual("Categories", values = palettes$cats) + 
scale_y_continuous(NULL) +
scale_x_discrete(NULL, expand = c(0, 0)) +
theme_minimal_grid() + 
theme(legend.position= "top")

example_regions <- data.frame(r = rep(c("Africa", "Asia", "Americas", "Europe", "Oceania"), each = 3), v = c(25.55, 25.96, 25.84, 20.24, 20.74, 21.19, 34.89, 34.79, 35.32, 32.35, 32.86, 32.91, 19.53, 19.37, 20.10), y = rep(2023:2025, times  = 5))

example_regions |>
ggplot(aes(x = factor(y), y = v)) + 
geom_line(aes(color = r, group = r)) +
scale_color_manual("Regions", values = palettes$regions) + 
scale_y_continuous(NULL) +
scale_x_discrete(NULL, expand = c(0, 0)) +
theme_minimal_grid() + 
theme(legend.position= "top")

example_disability <- read.csv(here("data/raw/expofdisc2026.csv"), skip = 0) |>
select(!starts_with("X")) |>
filter(GeoAreaName == "Peru", Disability.status != "_T", Grounds.of.discrimination == "ALL")

example_disability |> 
ggplot(aes(x = ))
#
#
#
