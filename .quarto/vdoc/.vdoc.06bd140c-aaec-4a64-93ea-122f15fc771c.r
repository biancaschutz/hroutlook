#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| echo: false
library(magrittr)
library(showtext)
library(hrbrthemes)
library(waffle)
library(tidyverse)
library(cowplot)
library(rvest)
library(ggbeeswarm)
library(scales)
library(httr)
library(jsonlite)
library(ggrepel)
library(readxl)
library(ggmosaic)
library(ggarrow)
library(here)
library(ggtext)
source(here("consts.R"))

dir.create(here("drafts/rightToDevelopment/datasets"), recursive = TRUE, showWarnings = FALSE)
#
#
#
url <- "https://sdmx.oecd.org/public/rest/data/OECD.WISE.RSB,DSD_SDG@DF_SDG_G_17,2.0/..17_2..._T._T._T._T._T.?startPeriod=2020&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"

sdg1721 <- read.csv(url)
#
#
#
#
#| fig-height: 10
#| fig-width: 7
#| out-width: 100%
codes <- c("17_2_1_DC_ODA_TOTGGE", "17_2_1_DC_ODA_LDCG")

sdg1721 |>
    filter(SDG_SERIES == codes[1], TIME_PERIOD == 2024) |>
    ggplot(aes(y = fct_reorder(Reference.area, OBS_VALUE), x = OBS_VALUE)) +
    geom_col(fill = "#006fb7") +
    geom_text(aes(label = scales::percent(OBS_VALUE, scale = 1, accuracy = .01), 
    x = OBS_VALUE + .01), 
    hjust = 0) +
    scale_x_continuous(NULL, expand = c(0, 0), labels = NULL, breaks = NULL) +
    scale_y_discrete(NULL) +
    theme_minimal_vgrid() +
    theme(plot.margin = margin(r = 75), axis.ticks.x = element_blank()) +
    coord_cartesian(clip = "off")
#
#
#
sdg1721 |>
    filter(SDG_SERIES == codes[1]) |>
    group_by(TIME_PERIOD) |>
    summarize(m = mean(OBS_VALUE)) |>
    ggplot(aes(x = TIME_PERIOD, y = m)) +
    geom_line(color = "#006fb7") +
    scale_x_continuous(NULL, expand = c(0, 0)) +
    scale_y_continuous("average ODA on grant equivalent basis (% GNI)") + 
    theme_minimal_grid() +
    theme(plot.margin = margin(r = 20))
#
#
#
url2 <- "https://sdmx.oecd.org/public/rest/data/OECD.DCD.FSD,DSD_DAC2@DF_DAC2A,/DAC.LDC+ALLR.206.USD.Q?startPeriod=2020&dimensionAtObservation=AllDimensions&format=csvfilewithlabels"

disburse <- read.csv(url2)

dac <- disburse |>
    arrange(Recipient, TIME_PERIOD) |>
    group_by(Recipient) |>
    mutate(
        lag = lag(OBS_VALUE),
        pct_change = (OBS_VALUE - lag) / lag
    )
#
#
#
dac |>
    ggplot(aes(x = TIME_PERIOD, y = OBS_VALUE)) +
    geom_line(aes(color = Recipient)) +
    scale_color_ghro("misc") +
    scale_x_continuous(NULL) +
    scale_y_continuous("Millions of US dollars, 2024", labels = scales::comma_format()) +
    theme_minimal_grid()
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
gini <- read.csv(here("data/raw/gini.csv"), nrow = 265)

gini2 <- gini |>
    pivot_longer(starts_with("X")) |>
    mutate(
        name = as.integer(str_sub(name, 2, 6)),
        value = replace(value, value == "..", NA)
    ) |>
    drop_na() |>
    group_by(Country.Name) |>
    filter(name == max(name))

bands <- data.frame(
    level = factor(c("Low", "Moderately low", "Moderately high", "High", "Very high"), levels = c("Low", "Moderately low", "Moderately high", "High", "Very high")),
    min = c(0, 25, 30, 40, 45),
    max = c(25, 30, 40, 45, Inf)
)

pal <- dichromat::colorschemes$DarkRedtoBlue.12[c(2, 4, 8, 10, 11)]
names(pal) <- bands$level

g1 <- gini2 |>
    filter(name > 2015) |>
    group_by(name) |>
    summarize(
        m = mean(as.numeric(value)),
        n_countries = n()
    ) |>
    ggplot(aes(x = name, y = m)) +
    geom_rect(data = bands, aes(ymin = min, ymax = max, fill = level), xmax = Inf, xmin = -Inf, alpha = .5, inherit.aes = FALSE) +
    scale_fill_manual(values = pal) +
    geom_line() +
    scale_x_continuous(NULL, expand = c(0, 0), breaks = seq(2016, 2025, by = 2)) +
    scale_y_continuous("Gini index", limits = c(0, 55), expand = c(0, 0)) +
    theme_minimal_grid() +
    coord_cartesian(clip = "off") +
    theme(plot.margin = margin(r = 10, t = 5), legend.position = "none") +
    ggtitle("Non-weighted gini,\nnot filled")
#
#
#
#
#
#
#
#
#
#| fig-width: 10
#| fig-height: 6
library(patchwork)
pop <- read.csv(here("data/raw/unpop2000.csv")) |> select(Iso3, Time, Value)

test <- gini2 |>
    ungroup() |>
    mutate(name = as.integer(name)) |>
    complete(nesting(Country.Name, Country.Code, Series.Name, Series.Code), name = 2000:2025) |>
    arrange(Country.Name, name) |>
    group_by(Country.Name) |>
    fill(value, .direction = "down") |>
    ungroup() |>
    left_join(pop, by = c("Country.Code" = "Iso3", "name" = "Time"))

weighted_gini_by_year <- test %>%
    mutate(value = as.numeric(value)) %>%
    filter(!is.na(value)) %>%
    group_by(name) %>%
    summarise(
        weighted_gini = sum(value * Value) / sum(Value),
        n_countries   = n()
    )

weighted_gini_by_year

g2 <- weighted_gini_by_year |>
    filter(name > 2015) |>
    ggplot(aes(x = name, y = weighted_gini)) +
    geom_rect(data = bands, aes(ymin = min, ymax = max, fill = level), xmax = Inf, xmin = -Inf, alpha = .5, inherit.aes = FALSE) +
    scale_fill_manual(values = pal) +
    geom_line() +
    scale_x_continuous(NULL, expand = c(0, 0), breaks = seq(2016, 2025, by = 2)) +
    scale_y_continuous("Gini index", limits = c(0, 55), expand = c(0, 0)) +
    theme_minimal_grid() +
    coord_cartesian(clip = "off") +
    theme(plot.margin = margin(r = 10, t = 5), legend.position = "none") +
    ggtitle("Weighted gini,\nfilled from 2020 on")

g3 <- test %>%
    mutate(value = as.numeric(value)) %>%
    filter(name > 2015) |>
    filter(!is.na(value)) %>%
    group_by(name) %>%
    summarise(
        m = mean(value),
        n_countries = n()
    ) |>
    ggplot(aes(x = name, y = m)) +
    geom_rect(data = bands, aes(ymin = min, ymax = max, fill = level), xmax = Inf, xmin = -Inf, alpha = .5, inherit.aes = FALSE) +
    scale_fill_manual(values = pal) +
    geom_line() +
    scale_x_continuous(NULL, expand = c(0, 0), breaks = seq(2016, 2025, by = 2)) +
    scale_y_continuous("Gini index", limits = c(0, 55), expand = c(0, 0)) +
    theme_minimal_grid() +
    coord_cartesian(clip = "off") +
    theme(plot.margin = margin(r = 10, t = 5), legend.position = "none") +
    ggtitle("Non-weighted gini,\nfilled from 2020 on")

g1 + g2 + g3
#
#
#
test2 <- gini2 |>
    ungroup() |>
    mutate(name = as.integer(name)) |>
    complete(nesting(Country.Name, Country.Code, Series.Name, Series.Code), name = 2000:2025) |>
    arrange(Country.Name, name) |>
    group_by(Country.Name) |>
    fill(value, .direction = "down") |>
    ungroup() |>
    mutate(high_ineq = ifelse(as.numeric(value) >= 40, 1, 0))

test2 |> distinct(Country.Name, value) |> View()
#
#
#
#
#
sdg_gini <- readxl::read_excel(here("data/raw/SI_DST_FISP.xlsx")) |>
    select(-SeriesCode, -Goal, -Target, -Indicator, -SeriesDescription) |>
    pivot_longer(cols = `2007`:`2025`)

sdg_gini_avg <- sdg_gini |>
    group_by(name, `Fiscal intervention stage`) |>
    drop_na(value) |>
    filter(value != NaN, !name %in% c(2007, 2025)) |>
    summarize(
        m = mean(as.numeric(value)),
        n = n()
    )

sdg_gini_avg |>
    filter(`Fiscal intervention stage` != "POSTFIS_CON_INC") |>
    ggplot(aes(x = as.integer(name), y = m)) +
    geom_rect(data = bands, aes(ymin = min, ymax = max, fill = level), xmax = Inf, xmin = -Inf, alpha = .5, inherit.aes = FALSE) +
    scale_fill_manual(values = pal) +
    geom_line(aes(color = `Fiscal intervention stage`)) +
    scale_x_continuous(NULL, expand = c(0, 0), breaks = seq(2016, 2025, by = 2)) +
    scale_y_continuous("Gini index", limits = c(0, 55), expand = c(0, 0)) +
    theme_minimal_grid() +
    coord_cartesian(clip = "off") +
    theme(plot.margin = margin(r = 10, t = 5), legend.position = "none") +
    ggtitle("Weighted gini,\nfilled from 2020 on")

sdg_gini_avg |>
    select(name, `Fiscal intervention stage`, n) |>
    pivot_wider(names_from = `Fiscal intervention stage`, values_from = n)
#
#
#
sdg_gini |>
drop_na(value) |>
filter(`Fiscal intervention stage` != "POST")
ggplot(aes(x = name, y = as.numeric(value))) + 
geom_boxplot() +
facet_wrap(~`Fiscal intervention stage`)
#
#
#
