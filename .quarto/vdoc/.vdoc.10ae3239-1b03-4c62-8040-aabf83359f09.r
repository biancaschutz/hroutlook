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
library(here)

source(here("consts.R"))

dir.create(here("drafts/rightToLife/datasets"), recursive = TRUE, showWarnings = FALSE)
path <- here("drafts/rightToLife/datasets/")
raw <- here("data/raw/")

## countries and regions
countries <- getHRDx("country")

regions <- getHRDx("region")

level3 <- regions |>
    filter(level == 3) |>
    select(id, parentId) |>
    rename(level2 = parentId, level3 = id)
level2 <- regions |>
    filter(level == 2) |>
    select(id, parentId) |>
    rename(level1 = parentId, level2 = id)


regions_by_level <- level3 |>
    full_join(level2, by = "level2")

countries_region <- countries |> left_join(regions |> select(id, level), by = c("regionId" = "id"))

countries_level2 <- countries_region |>
    filter(level == 2) |>
    left_join(regions_by_level, by = c("regionId" = "level2")) |>
    rename(level2 = regionId)
countries_level3 <- countries_region |>
    filter(level == 3) |>
    left_join(regions_by_level, by = c("regionId" = "level3")) |>
    rename(level3 = regionId)

countries <- rbind(countries_level2, countries_level3)

#
#
#
#
hrds <- read_excel(here(raw, "hrd_killings_updated2025.xlsx")) |>
filter(Sex == "BOTHSEX") |>
select(-Goal, -Target, -Indicator, -SeriesCode, -Sex, -Units, -`Reporting Type`) |> 
pivot_longer(cols = `2015`:`2025`, names_to = "year") |>
mutate(value = as.numeric(value))

hrds %>%
filter(GeoAreaCode == 1) |>
    ggplot(aes(x = year, y = value)) +
    geom_col(fill = orange) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(x = "Year", y = "Number of human rights defenders killed annually") +
    theme_minimal_hgrid()
#
#
#
hrds_ts <- hrds |> 
filter(GeoAreaCode != 1)

pal <- palettes$cats2
names(pal) <- hrds_ts |> filter(GeoAreaName != "Oceania") |> distinct(GeoAreaName) |> pull(GeoAreaName)

labels <- hrds_ts |> filter(year == 2025, GeoAreaName != "Oceania")

hrds_ts |>
filter(GeoAreaName != "Oceania") |>
ggplot(aes(x = as.integer(year), y = value, color = GeoAreaName, group = GeoAreaName)) + 
geom_line() + 
geom_text(data = labels, aes(label = str_wrap(GeoAreaName, 20)), hjust = 0) +
scale_color_manual(values = pal, guide = "none") + 
scale_x_continuous(NULL, breaks = seq(2015, 2025, by = 2)) + 
scale_y_continuous("Number of human rights defenders killed") + 
theme_minimal_grid() +
theme(plot.margin = margin(r = 125)) +
coord_cartesian(clip = "off")
#
#
#
## stacked area instead? 
hrds_pct <- hrds_ts |> 
left_join(hrds |> filter(GeoAreaCode == 1, !is.na(value)) |> select(value, year) |> rename(tot = value), by = "year") |>
mutate(pct = ifelse(is.na(value), 0, value)/tot) 

labels <- hrds_pct |> 
  filter(year == 2025, GeoAreaName != "Oceania") |> 
  arrange(desc(GeoAreaName)) |> 
  mutate(
    ymax = cumsum(pct),
    ymin = lag(ymax, default = 0),
    y = (ymin + ymax) / 2
  ) |> 
  select(-SeriesDescription)

hrds_pct |>
ggplot(aes(x = as.integer(year), y = pct, fill = GeoAreaName, group = GeoAreaName)) + 
geom_area() + 
geom_text(data = labels, aes(y = y, label = paste0( str_wrap(GeoAreaName, 20),  "\n", ), color = GeoAreaName), hjust = 0, lineheight = 1) +
scale_fill_manual(values = c(pal, "Oceania" = "#808080"), guide = "none") + 
scale_color_manual(values = c(pal, "Oceania" = "#808080"), guide = "none") + 
scale_x_continuous(NULL, breaks = seq(2015, 2025, by = 2), expand = c(0, 0)) + 
scale_y_continuous("Percent of human rights defenders killed", labels = scales::percent_format(), expand = c(0, 0)) + 
theme_minimal_grid() +
theme(plot.margin = margin(r = 125)) +
coord_cartesian(clip = "off")
#
#
#
#| fig-height: 7
cols <- c("Category", "x", "Central Asia and Southern Asia", "Eastern Asia and South-eastern Asia", "Latin America and the Caribbean", "Northern America and Europe", "Sub-Saharan Africa", "Western Asia and Northern Africa", "World", "Min", "Max", "start", "end")

wide <- read_csv(I("Women HRDs,1,12%,19%,16%,19%,7%,11%,14%,7%,19%,7%,12%
Environment & land HRDs,2,12%,25%,52%,6%,19%,1%,31%,1%,52%,1%,51%
Indigenous & minority HRDs,3,18%,14%,45%,13%,18%,13%,29%,13%,45%,13%,32%
Youth HRDs,4,8%,6%,10%,31%,10%,13%,10%,6%,31%,6%,25%"),
    col_names = cols
) %>%
    select(-x, -World, -Min, -Max, -start, -end) %>%
    mutate(across(!Category, ~ as.numeric(str_extract(., "[0-9]*"))))

hrds_category_export <- wide %>%
    pivot_longer(-Category) %>%
    mutate(
        Category = factor(Category),
        cat = as.numeric(Category)
    )

hrds_category_export %>%
    write.csv(paste0(path, "/hr_defenders_regional_category.csv"), row.names = FALSE)

hrds_category_export %>%
    ggplot(aes(x = str_wrap(as.character(Category), 20), y = value / 100, color = name)) +
    geom_beeswarm(method = "center", cex = 2, size = 3) +
    scale_color_manual(values = palettes$cats) +
    labs(x = "Category", y = "Percentage of total killed or disappeared", color = "Region") +
    scale_y_continuous(
        expand = c(0, 0),
        limits = c(0, .55),
        labels = label_percent()
    ) +
    theme_minimal_hgrid() +
    theme(legend.position = "bottom") +
    guides(color = guide_legend(nrow = 3, byrow = TRUE))
#
#
#
#
#
crd_files <- list.files(paste0(raw, "/crd"))

l <- list()

for (f in crd_files) {
    l[[str_remove(f, ".rds")]] <- readRDS(paste0(raw, "/crd/", f))
}

crds_total_export <- l$series_total_w

crds_total_export %>% write.csv(paste0(path, "/global_crds_annual.csv"), row.names = FALSE)

crds_total_export %>%
    ggplot(aes(x = as.factor(year), y = deaths_OHCHR)) +
    geom_line(color = orange, group = 1) +
    scale_y_continuous(
        limits = c(0, max(crds_total_export$deaths_OHCHR)),
        expand = c(0, 0),
        labels = label_comma()
    ) +
    labs(x = "Year", y = "Number of global conflict-related deaths") +
    theme_minimal_grid()
#
#
#
#
#
unpop2026 <- read.csv(here(raw, "unpop2000.csv")) |> filter(Time == 2026) 

lifeexp <- read.csv(here(raw, "unpd_lifeexp.csv")) |> 
filter(Time == 2026) |> 
inner_join(countries |> select(iso3, level1, level2, level3), by = c("Iso3" = "iso3")) |> 
left_join(regions |> select(id, name), by = c("level1" = "id")) |> 
select(Value, name, Iso3) |> 
left_join(unpop2026 |> select(Iso3, Value) |> rename(pop = Value), by = "Iso3") 

lifeexp_avgs <- lifeexp |>
group_by(name) |>
 summarize(
    region_lifeexp = sum(Value * pop, na.rm = TRUE) / sum(pop, na.rm = TRUE)
  ) |> 
  ungroup()


    ggplot() +
     geom_segment(data = lifeexp_avgs, aes(x = region_lifeexp, xend = region_lifeexp, y = as.numeric(as.factor(name)) - .4, yend = as.numeric(as.factor(name)) + .4), color = "black", alpha = .7) + 
     geom_text(data = lifeexp_avgs, aes(x = region_lifeexp - .1, y = as.numeric(as.factor(name)) + .35, label = paste0("Average: ", round(region_lifeexp, 1), " years")), hjust = 1, size = 4) + 
    geom_beeswarm(data = lifeexp, aes(x = Value, y = as.numeric(as.factor(name)), color = name), cex = 1.5) +
    scale_color_ghro("region") +
    guides(color = "none") +
    scale_x_continuous(expand = expansion(add = 1)) +
    scale_y_continuous(breaks = 1:5, labels = c("Africa", "Americas", "Asia", "Europe", "Oceania")) + 
    theme_minimal_vgrid() +
    labs(color = "Region", y = NULL, x = "Life expectancy at birth")
#
#
#
#
#
body <- list(
    limit = 1000,
    where = list(
        datetime = c("2010-01-01", "2024-01-01"),
        sexCode = "_T"
    )
)

response <- POST(
    url = "https://hrdx-api-staging.un.org/vcIhrPsrc/search",
    body = toJSON(body, auto_unbox = TRUE),
    content_type_json()
)

homicide_per_100k <- fromJSON(content(response, "text"), flatten = TRUE)$results |> filter(!is.na(regionId))

body <- list(
    limit = 1000
)

response <- POST(
    url = "https://hrdx-api-staging.un.org/region/search",
    body = toJSON(body, auto_unbox = TRUE),
    content_type_json()
)


regions <- fromJSON(content(response, "text"), flatten = TRUE)$results %>%
    select(id, name, level) %>%
    filter(level %in% 0:1)

homicide_per_100k_export <- homicide_per_100k %>%
    left_join(regions, by = c("regionId" = "id")) %>%
    filter(!is.na(name)) %>%
    mutate(
        year = year(datetime),
        highlight = ifelse(name == "World", "Y", "N")
    ) %>%
    select(name, year, value, highlight)

homicide_per_100k_export %>%
    pivot_wider(names_from = year, values_from = value) %>%
    mutate(incdec = ifelse(`2024` - `2010` > 0, "increased", "decreased")) %>%
    write.csv(paste0(path, "/homicide_100k_range.csv"), row.names = FALSE)

label_2010 <- homicide_per_100k_export %>%
    filter(year == 2010) %>%
    mutate(label_x = 2009)

label_2023 <- homicide_per_100k_export %>%
    filter(year == 2024) %>%
    mutate(label_x = 2025)

label_data <- data.frame(x = c(2010, 2024), label = c(2010, 2024), y = 0)

ggplot(homicide_per_100k_export, aes(x = year, y = round(value, 1), group = name)) +
    geom_line(aes(color = highlight, alpha = highlight)) +
    scale_alpha_manual(values = c("Y" = 1, "N" = .9)) +
    geom_text_repel(
        data = label_2010,
        aes(x = label_x, y = value, label = paste0(name, "\n", value), color = highlight),
        direction = "y",
        hjust = 1,
        segment.color = NA,
        xlim = c(-Inf, 2009.75),
        lineheight = 0.8,
        size = 3
    ) +
    geom_text_repel(
        data = label_2023,
        aes(x = label_x, y = value, label = paste0(name, "\n", value), color = highlight),
        direction = "y",
        hjust = 0,
        segment.color = NA,
        xlim = c(2023.25, Inf),
        lineheight = 0.8,
        size = 3
    ) +
    geom_segment(aes(x = 2010, xend = 2010, y = .5, yend = 16), inherit.aes = FALSE) +
    geom_segment(aes(x = 2024, xend = 2024, y = .5, yend = 16), inherit.aes = FALSE) +
    geom_point(aes(color = highlight)) +
    scale_color_manual(values = palettes$highlight) +
    scale_x_continuous(expand = expansion(mult = c(0.3, 0.3))) +
    geom_text(data = label_data, aes(x = x, y = y, label = label), inherit.aes = FALSE) +
    theme_void() +
    guides(color = "none", label = "none", alpha = "none")
#
#
#
homicide_per_100k_export |> filter(year == 2024, name != "World") |>
ggplot(aes(x = fct_reorder(name, value), y = value)) + 
geom_col(aes(fill = name)) + 
scale_fill_ghro("region") + 
guides(fill = "none") +
    scale_y_continuous("homicide rate per 100,000 population", expand = c(0, 0)) +
    scale_x_discrete(NULL) +
    theme_minimal_hgrid()
#
#
#
#| fig-height: 5
body <- list(
    limit = 1000,
    where = list(
        sexCode = "_T",
        datetime = "2024-01-01"
    )
)

response <- POST(
    url = "https://hrdx-api-staging.un.org/vcIhrPsrcn/search",
    body = toJSON(body, auto_unbox = TRUE),
    content_type_json()
)

homicide_n_export <- fromJSON(content(response, "text"), flatten = TRUE)$results %>%
    mutate(
        year = year(datetime)
    ) %>%
    left_join(countries, by = c("countryId" = "id")) %>%
    group_by(level1) %>%
    summarize(total = sum(value), .groups = "drop") %>%
    mutate(
        value = total / sum(total) * 100,
        n = n()
    ) |> 
    left_join(regions |> select(id, name) |> rename(region_name = name), by = c("level1" = "id"))

homicide_n_export %>%
    write.csv(paste0(path, "/homicide_n_2024.csv"), row.names = FALSE)

homicide_n_export %>%
    arrange(total) %>%
    mutate(
        csum = rev(cumsum(rev(value))),
        pos = value / 2 + lead(csum, 1),
        pos = if_else(is.na(pos), value / 2, pos),
        pos = ifelse(region_name == "Oceania", 103, pos)
    ) %>%
    ggplot(aes(x = 1, y = value, fill = fct_inorder(region_name))) +
    geom_col(width = .75, color = "white", linewidth = .5) +
    scale_fill_manual(values = palettes$regions, guide = "none") +
    geom_text(
        aes(y = pos, x = 1.4, label = paste0(region_name, "\n", scales::comma(total), " homicides")),
        size = 4, show.legend = FALSE, hjust = 0, lineheight = .8
    ) +
    scale_y_continuous("Proportion of global homicides, 2024", labels = scales::label_percent(scale = 1), expand = c(0, 0), limits = c(0, 108)) +
    scale_x_continuous(NULL, expand = c(0, 0), limits = c(.5, 2), breaks = NULL) +
    theme_minimal_hgrid() +
    theme(panel.grid = element_blank())
#
#
#
pop_vs_homicides <- read.csv(here(paste0(raw, "/region_pops.csv"))) |> 
select(Location, Value) |> 
inner_join(homicide_n_export, by = c("Location" = "region_name")) |> 
mutate(pop_pct = Value/sum(Value)*100) |> 
select(value, pop_pct, Location) |> 
mutate(hom_rt = value) |> 
pivot_longer(c("pop_pct", "value")) |> 
mutate(name = case_when(name == "pop_pct" ~ "% of world population", 
.default = "% of homicides"), is_america = ifelse(Location == "Americas", "Y", "N"))

pop_vs_homicides |> 
ggplot(aes(x = fct_reorder(Location, hom_rt), y = value, fill = is_america)) + 
geom_col(position = "dodge") + 
facet_wrap(~name) +
scale_fill_manual(values = c("Y" = palettes$regions[["Americas"]], "N" = "#cccccc"), guide = "none") +
scale_y_continuous(NULL, labels = scales::percent_format(scale = 1), expand = c(0, 0)) +
scale_x_discrete(2024) +
theme_minimal_hgrid() + 
panel_border()
#
#
#
#| fig-height: 5
body <- list(
    limit = 1000,
    where = list(
        datetime = c("2024-01-01"),
        sexCode = c("M", "F")
    )
)

response <- POST(
    url = "https://hrdx-api-staging.un.org/vcIhrPsrc/search",
    body = toJSON(body, auto_unbox = TRUE),
    content_type_json()
)

homicides_sex <- fromJSON(content(response, "text"), flatten = TRUE)$results

homicides_sex_export <- homicides_sex %>%
    mutate(
        year = year(datetime)
    ) %>%
    filter(year == 2024, !is.na(countryId)) %>%
    left_join(countries, by = c("countryId" = "id")) %>%
    left_join(regions |> rename(region_name = name), by = c("level1" = "id")) |>
    select(name, region_name, value, year, sexCode) %>%
    pivot_wider(names_from = sexCode, values_from = value)

homicides_sex_export %>%
    write.csv(paste0(path, "/homicides_sex.csv"), row.names = FALSE)

homicides_sex_export %>%
    ggplot(aes(x = M, y = F)) +
    geom_point(aes(color = region_name)) +
    scale_color_manual(values = palettes$region) +
    theme_minimal_grid(12) +
    labs(x = "Intentional homicides of men, per 100,000 population", y = "Intentional homicides of women, per 100,000 population") +
    scale_x_continuous(expand = c(.02, 0)) +
    scale_y_continuous(expand = c(.02, 0))
#
#
#
#
#
#| fig-height: 6
# obtained here: https://data.unodc.org/datareport/hom-victim
homicides_unodc <- read_excel(paste0(raw, "/unodc_homicide.xlsx"), skip = 2)

homicides_ipv_export <- homicides_unodc %>%
    filter(
        Dimension == "by relationship to perpetrator",
        `Unit of measurement` == "Rate per 100,000 population",
        Sex %in% c("Male", "Female"),
        Year == 2022,
        Category %in% c(
            "Perpetrator unknown to the victim",
            "Intimate partner or family member: Intimate partner",
            "Intimate partner or family member: Family member"
        )
    ) %>%
    group_by(Sex, Category) %>%
    summarize(value = mean(VALUE)) %>%
    pivot_wider(names_from = Sex, values_from = value) %>%
    mutate(
        diff = Female - Male,
        sign = ifelse(diff > 0, "F", "M"),
        Category = str_to_lower(str_remove(Category, "Intimate partner or family member: |Perpetrator "))
    )

homicides_ipv_export %>%
    write.csv(paste0(path, "/homicides_ipv.csv"), row.names = FALSE)

homicides_ipv_export %>%
filter(Category != "family member") |>
    ggplot(aes(y = Category, x = diff, fill = sign)) +
    geom_col() +
    scale_fill_manual(values = palettes$gender, guide = "none") +
    labs(y = "Relationship to victim", x = "Difference in rate of intentional homicide, Female - Male") +
    geom_vline(xintercept = 0) +
    theme_minimal_hgrid()
#
#
#
#
#
#
#
#
#
body <- list(
    limit = 1000,
    where = list("regionId" = c(19, 2, 9, 142, 150, 1))
)

response <- POST(
    url = "https://hrdx-api-staging.un.org/shStaMort/search",
    body = toJSON(body, auto_unbox = TRUE),
    content_type_json()
)

maternal_mort_export <- fromJSON(content(response, "text"), flatten = TRUE)$results %>%
    inner_join(regions, by = c("regionId" = "id")) %>%
    mutate(year = year(datetime)) %>%
    select(year, name, value)

maternal_mort_export %>%
filter(name != "World") %>%
    ggplot(aes(x = year, y = value, color = name)) +
    geom_line() +
    scale_color_manual(values = palettes$regions) +
    theme_minimal_grid() +
    labs(y = "Number of maternal deaths per 100,000 live births", x = "Year", color = "Region") +
    geom_vline(xintercept = 2021, linetype = "dashed") +
    geom_text(x = 2016, y = 650, label = "COVID-19 pandemic", color = "black")
#
#
#
#
#
body <- list(
    limit = 1000,
    where = list(
        "datetime" = "2024-01-01",
        "sexCode" = "_T"
    )
)

response <- POST(
    url = "https://hrdx-api-staging.un.org/shDynMort/search",
    body = toJSON(body, auto_unbox = TRUE),
    content_type_json()
)

under5_mort_export <- fromJSON(content(response, "text"), flatten = TRUE)$results %>%
    filter(!is.na(regionId)) %>%
    left_join(regions, by = c("regionId" = "id")) %>%
    mutate(parentId = ifelse(level == 1, regionId, parentId)) %>%
    filter(name != "Australia and New Zealand") %>%
    select(-name, -level) %>%
    left_join(regions, by = c("parentId" = "id")) %>%
    group_by(name) %>%
    summarize(mean_regional = mean(value))

under5_mort_export %>%
    write.csv(paste0(path, "/under5-mortalities2024.csv"), row.names = FALSE)

under5_mort_export %>%
    ggplot(aes(x = fct_reorder(name, -mean_regional), y = mean_regional)) +
    geom_col(fill = orange) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(x = NULL, y = "probability of a child dying before\nreaching age 5, per 1,000 live births") +
    theme_minimal_hgrid()
#
#
#
#
#
#
#
op2 <- read_excel(paste0(raw, "/op2.xls"), skip = 1)

pal <- c("Ratified" = "#faa31b", "Not ratified" = "#cccccc")


# op2_export <- op2 %>%
# mutate(rat = ifelse(!is.na(`Date of Ratification/Accession`), T, F),
#        country_cleaned = case_when(Country == "Netherlands" ~ "Netherlands (Kingdom of the)",
#                                    Country == "Côte d'Ivoire" ~ "Côte d’Ivoire",
#                                    Country == "Czech Republic" ~ "Czechia",
#                                   .default = Country)
#       ) %>%
#     left_join(unregions, by = c("country_cleaned" = "Country or Area")) %>%
#     filter(!is.na(`M49 Code`)) %>%
# group_by(`Region Name`) %>%
# summarize(Ratified = sum(rat)/n(),
#           `Not ratified` = 1 - Ratified) %>%
# pivot_longer(cols = c(Ratified, `Not ratified`))

# op2_export %>%
# write.csv(paste0(path, "/op2_pct.csv"), row.names = FALSE)

# op2_export %>%
#   ggplot(aes(x = fct_reorder(`Region Name`, value * (name == "Ratified")), y = value)) +
#   geom_col(aes(fill = name)) +
# scale_fill_manual(values = pal) +
# labs(fill = "Ratified OP2", y = "% of countries in region", x = NULL) +
# scale_y_continuous(expand = c(0, 0)) +
# theme_minimal_hgrid()

# OR

op2_export2 <- op2 %>%
    mutate(
        rat = ifelse(!is.na(`Date of Ratification/Accession`), T, F),
        country_cleaned = case_when(Country == "Netherlands" ~ "Netherlands (Kingdom of the)",
            Country == "Côte d'Ivoire" ~ "Côte d'Ivoire",
            Country == "Czech Republic" ~ "Czechia",
            .default = Country
        )
    ) %>%
 left_join(countries, by = c("country_cleaned" = "name")) %>%
    left_join(regions |> rename(region_name = name), by = c("level1" = "id")) |>
    group_by(region_name) %>%
    summarize(
        Ratified = sum(rat),
        `Not ratified` = sum(!rat)
    ) %>%
    pivot_longer(cols = c(Ratified, `Not ratified`)) %>%
    group_by(region_name) %>%
    mutate(pct_rat = value[name == "Ratified"] / sum(value)) %>%
    ungroup() %>%
    mutate(
        region = fct_reorder(region_name, pct_rat),
        name = fct_rev(name)
    )

op2_export2 %>%
    write.csv(paste0(path, "/op2_mosaic.csv"), row.names = FALSE)

op2_export2 %>%
    ggplot() +
    geom_mosaic(aes(x = product(name, region_name), fill = name, weight = value)) +
    scale_fill_manual(values = pal, guide = "none") +
    theme_void() +
    theme(axis.text = element_text()) +
    coord_flip() +
    scale_x_productlist(expand = c(0, 0)) +
    scale_y_productlist(expand = c(0, 0))
#
#
#
