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
library(tidyverse)
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
library(ISOcodes)
source(here("consts.R"))

dir.create(here("drafts/rightToParticipate/datasets"), recursive = TRUE, showWarnings = FALSE)
path <- here("drafts/rightToParticipate/datasets/")
raw <- here("data/raw/")
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
#| include: false
#| echo: false
# getting all the data from HRDx

getHRDx <- function(code, params = list(), offline = FALSE) {
    if (offline) {
        raw_path <- here("data/raw/rightToParticipation")
        read.csv(paste0(raw_path, "/", code, ".csv"))
    }
    else {
    url <- paste0("https://hrdx-api-staging.un.org/", gsub("_", "", code), "/search")
    print(url)
    params$limit <- 1000

    response <- POST(
        url = url,
        body = toJSON(params, auto_unbox = TRUE),
        content_type_json()
    )

    pages <- fromJSON(content(response, "text"), flatten = TRUE)$pagination$pages

    print(pages)

    df_list <- list()
    for (p in 1:pages) {
        params$page <- p
        response <- POST(
            url = url,
            body = toJSON(params, auto_unbox = TRUE),
            content_type_json()
        )

        res <- fromJSON(content(response, "text"), flatten = TRUE)$results

        df_list[[p]] <- res
    }

    df_list |> rbind_pages()
    }
}

right_codes <- c(
    "IU_COR_BRIB", "SG_GEN_PARL", "SG_GEN_LOCGELS",
    "SG_DMK_PARLCC_LC", "SG_DMK_PARLCC_UC", "SG_DMK_PARLCC_JC",
    "SG_DMK_JDC", "SG_DMK_JDC_HGR", "SG_DMK_JDC_LWR", "SG_DMK_JDC_CNS",
    "SG_DMK_PSRVC", "SG_DMK_PARLYR_LC", "SG_DMK_PARLYR_UC",
    "SG_DMK_PARLMP_LC", "SG_DMK_PARLMP_UC", "SG_DMK_PARLSP_UC",
    "SG_DMK_PARLSP_LC"
)

right_raw_data <- list()
for (d in right_codes) {
    right_raw_data[[d]] <- getHRDx(d, offline = TRUE)
}

page <- read_html("https://unstats.un.org/unsd/methodology/m49/overview/")
t <- html_element(page, "table")
unregions <- html_table(t, header = TRUE) |>
    select(`Region Name`, `M49 Code`, `Sub-region Name`, `ISO-alpha3 Code`, `Country or Area`) |>
    rename(m49 = `M49 Code`, region_map = `Region Name`, subregion_map = `Sub-region Name`, country_name = `Country or Area`)
#
#
#
#
#
#| include: false
#| echo: false
bribes <- right_raw_data[["IU_COR_BRIB"]] |>
mutate(year = year(datetime)) |>
    left_join(unregions, by = c("countryId" = "m49"))

dim(bribes)
colnames(bribes) # disaggs available are sexCode

length(unique(bribes$countryId)) # we have data for 141 countries

unique(bribes$regionId) # and no regions

unique(year(bribes$datetime)) # data for 2010-2024

table(bribes$sexCode) # 50 obs where we have gender disagg
#
#
#
#| out-width: 100%
#| fig-width: 7
#| fig-height: 9
bribes |>
    filter(year > 2018, sexCode == "_T") |>
    ggplot(aes(x = year, y = value)) +
    geom_point(aes(color = region_map), size = 5, alpha = .5) +
    scale_color_ghro("region") + 
    scale_y_continuous("Prevalence of bribery (%)", expand = expansion(add = 1), labels = scales::percent_format(scale = 1)) +
    scale_x_continuous("Year", breaks = 2018:2024) +
    geom_line(data = bribes |>
        filter(sexCode == "_T", year > 2018) |>
        group_by(year) |>
        summarize(avg = mean(value)), aes(y = avg)) +
    annotate("text", x = 2024, y = (bribes |>
        filter(sexCode == "_T", year > 2018) |>
        group_by(year) |>
        summarize(avg = mean(value)) |> tail(1) |> pull(avg))[1] + 0.5, label = "Global average", hjust = 0) +
    theme_minimal_hgrid() +
    theme(legend.position = "top", axis.line.x = element_blank(), axis.ticks.x = element_blank(), plot.margin = margin(r = 100)) +
    labs(subtitle = "Each point represents a country") + 
    coord_cartesian(clip = "off")
#
#
#
#
bribes |>
    filter(sexCode == "_T") |>
    group_by(year) |>
    count(countryId) |>
    ggplot(aes(x = year, y = n)) + 
    geom_col(fill = "#006fb7") +
    scale_y_continuous("number of countries", expand = c(0, 0)) + 
    scale_x_continuous(NULL, breaks = seq(2005, 2020, by = 5)) + 
    theme_minimal_hgrid() + 
    labs(subtitle = "Number of countries reporting the total rate of bribery")
#
#
#
bribes |>
filter(sexCode == "_T") |>
group_by(year) |>
summarize(n = n(), 
avg = mean(value)) |>
ggplot(aes(x = year, y = avg)) + 
geom_line(color = "#006fb7") + 
scale_x_continuous(NULL) + 
scale_y_continuous("Prevalence of bribery (%)", expand = c(0, 0)) +
theme_minimal_grid()
#
#
#
#
#
#
#
#
#| include: false
#| echo: false
#| 
SG_DMK_JDC <- right_raw_data[["SG_DMK_JDC"]]
SG_DMK_JDC$court <- "All"

SG_DMK_JDC_HGR <- right_raw_data[["SG_DMK_JDC_HGR"]]
SG_DMK_JDC_HGR$court <- "Higher"

SG_DMK_JDC_LWR <- right_raw_data[["SG_DMK_JDC_LWR"]]
SG_DMK_JDC_LWR$court <- "Lower"

SG_DMK_JDC_CNS <- right_raw_data[["SG_DMK_JDC_CNS"]]
SG_DMK_JDC_CNS$court <- "Constitutional"

judiciary <- rbind(SG_DMK_JDC, SG_DMK_JDC_HGR, SG_DMK_JDC_LWR, SG_DMK_JDC_CNS) |>
mutate(year = year(datetime)) |>
left_join(unregions, by = c("countryId" = "m49"))

colnames(judiciary) # we have disaggs of court type, occupation, disability, sex, and age

disaggs <- c("court", "populationGroupCode", "occupationCode", "personsWithDisabilityCode", "sexCode", "ageCode")

for (d in disaggs) {
    print(table(judiciary[[d]]))
}
#
#
#
make_parity_labels <- function(y_min, y_max, drop) {
breaks <- seq(round(y_min * 5) / 5 + .2, round(y_max * 5) / 5 - .2, by = .2)    
    labels <- data.frame(
        y = breaks,
        angle = ifelse(breaks == 1, 90, 0),
        label = ifelse(breaks == 1, "Equal\nrepresentation", as.character(breaks))
    )
    
    labels <- labels |>
    mutate(label = ifelse(y %in% drop, "", label)) |>
        add_row(y = y_min + .1, angle = 90, label = "Under-\nrepresented") |>
                add_row(y = y_max - .1, angle = 90, label = "Over-\nrepresented")

    labels
}

parity_plot <- function(data, x_col, y_col, color_col,
                        palette,
                        facet = NULL, 
                        y_padding_bottom = 0.1,
                        y_padding_top = 0.1, 
                        drop = c()) {
    
    axis_x <- min(data[[x_col]]) - .5
    axis_end_x <- max(data[[x_col]]) + .5
    
    y_min <- max(min(data[[y_col]]) - y_padding_bottom, 0)
    y_max <- max(max(data[[y_col]]) + y_padding_top, 1.35)
    
    y_labels <- make_parity_labels(y_min, y_max, drop = drop)
    
    x_limits <- c(axis_x - .5, axis_end_x + .5)
    x_breaks <- seq(axis_x + .5, axis_end_x - .5)
    label_x <- axis_x - .5
    
    p <- ggplot(data, aes(x = .data[[x_col]], y = .data[[y_col]])) +
        geom_segment(
            data = y_labels |> filter(angle != 90),
            aes(y = y), x = axis_x, xend = axis_end_x,
            color = "grey85", linewidth = .5
        ) +
        geom_segment(x = axis_x, xend = axis_end_x, y = 1, color = "gray30", linetype = "dashed") +
        geom_line(aes(group = .data[[x_col]]), color = "black") +
        geom_point(aes(color = .data[[color_col]]), size = 5) +
        scale_color_ghro(palette) +
        annotate("text", x = axis_end_x + .5, y = 1.02, label = "Parity",
                 hjust = 1.1, vjust = 0, color = "gray30", size = 3.5) +
        scale_y_continuous(NULL, limits = c(y_min, y_max), breaks = NULL) +
        scale_x_continuous(NULL, limits = x_limits, expand = c(0, 0), breaks = x_breaks) +
        annotate_arrow(
            y = c(-Inf, Inf), x = axis_x,
            linewidth = 1,
            arrow_head = ggarrow::arrow_head_wings(),
            arrow_fins = ggarrow::arrow_head_wings(),
            length_head = unit(3, "mm"),
            length_fins = unit(3, "mm")
        ) +
        geom_text(
            data = y_labels,
            aes(x = label_x, y = y, label = label, angle = angle),
            vjust = 0, size = 12/.pt
        ) +
        coord_cartesian(clip = "off") +
        theme_minimal_grid() +
        theme(
            axis.line = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_text(margin = margin(r = 10)),
            plot.margin = margin(l = 50, r = 40),
            panel.grid = element_blank()
        )

    if (!is.null(facet)) {
        p <- p + facet_wrap(~.data[[facet]])
    }

    p
}

#
#
#
#| fig-height: 7
#| fig-width: 8
by_sex <- judiciary |>
    filter(sexCode != "_T", 
    populationGroupCode == "_T", 
    occupationCode == "OC2612", 
    ageCode == "_T", 
    personsWithDisabilityCode == "_T", 
    year > 2014, 
    court == "All") |>
    group_by(year, sexCode) |>
    summarize(
        m = mean(value),
        n = n()
    )

by_sex |>
    parity_plot("year", "m", "sexCode", "gender", y_padding_top = .2, y_padding_bottom = .15, drop = c(1.6)) + 
    theme(legend.position = "top")
#
#
#
#
#
#
#
#| fig-height: 7
#| fig-width: 8

by_disability <- judiciary |> filter(personsWithDisabilityCode != "_T", occupationCode == "OC2612", court == "All") |>
group_by(year, personsWithDisabilityCode) |>
summarize(m = mean(value))

by_disability |>
parity_plot("year", "m", "personsWithDisabilityCode", "disability", y_padding_top = .25, y_padding_bottom = .3, drop = c(1.2, .2))
#
#
#
#
#
by_sex_court <- judiciary |>
    filter(sexCode != "_T", 
    populationGroupCode == "_T", 
    occupationCode == "OC2612", 
    ageCode == "_T", 
    personsWithDisabilityCode == "_T", 
    year > 2014, 
    court != "All") |>
    group_by(year, sexCode, court) |>
    summarize(
        m = mean(value),
        n = n()
    )

    by_sex_court |>
    ggplot(aes(x = year, y = m)) + 
    geom_line(aes(color = sexCode)) + 
    geom_hline(yintercept = 1, color = "gray30", linetype = "dashed") +
    facet_wrap(~court, ncol = 1) +
    scale_color_ghro("gender") + 
    scale_x_continuous(NULL, breaks = seq(2015, 2025, by = 2)) +
    theme_minimal_grid() + 
    labs(y = NULL)
#
#
#
#
#

by_occupation_sex <- judiciary |>
    filter(sexCode != "_T", 
    populationGroupCode == "_T", 
    ageCode == "_T", 
    personsWithDisabilityCode == "_T", 
    year == 2025, 
    court != "All") |>
    group_by(year, sexCode, occupationCode, court) |>
    summarize(
        m = mean(value),
        n = n()
    )

    labels <- c(OC26 = "Registrars", OC2612 = "Judges")

by_occupation_sex |>
ggplot(aes(x = occupationCode, y = m)) + 
    geom_hline(yintercept = 1, color = "gray30", linetype = "dashed") +

geom_line(aes(group = occupationCode)) +
geom_point(aes(color = sexCode), size = 5) + 
scale_x_discrete("Occupation", labels = labels) +
scale_color_ghro("gender") + 
facet_wrap(~court) +
labs(y = NULL) +
theme_minimal_hgrid()
#
#
#
#
#| include: false
#| echo: false
chairs <- right_raw_data[["SG_DMK_PARLCC_LC"]] |>
    mutate(year = year(datetime))

dim(chairs)
colnames(chairs) # disaggs available are sexCode, parlCommitteesCode, ageCode

length(unique(chairs$countryId)) # we have data for 141 countries

table(chairs$regionId) # and regions

# regions <- getHRDx("region") |> mutate(regionId = id)

regions <- data.frame(regionId = c(1, 2, 9, 19, 142, 150), name = c("World", "Africa", "Oceania", "Americas", "Asia", "Europe"))

unique(year(chairs$datetime)) # data for 2021-2026

table(chairs$sexCode)

committee_flags <- c(
    isHR = "HR|HUM",
    isDEF = "DEF",
    isFAFF = "FAFF|FOR",
    isFIN = "FIN",
    isYTH = "YTH|YOUTH",
    isGEQU = "GEN|GEQU"
)

committee_mapping <- data.frame(committeeType = paste0("is", c("HR", "DEF", "FAFF", "FIN", "YTH", "GEQU")), 
label = c("Human Rights", "Defense", "Foreign Affairs", "Finance", "Youth", "Gender Equality"))

chair_regions <- chairs |>
    filter(!is.na(regionId)) |>
    inner_join(regions, by = "regionId")

chair_regions <- chair_regions |>
    bind_cols(
        imap_dfc(committee_flags, ~ str_detect(chair_regions$parlCommitteesCode, .x))
    ) |>
    pivot_longer(starts_with("is"), values_to = ".drop", names_to = "committeeType") |>
    left_join(committee_mapping) |>
    filter(.drop)
#
#
#
region_gender_chairs <- chair_regions |>
filter(sexCode %in% c("M", "F"), year == 2026) |> 
pivot_wider(names_from = ageCode, values_from = value) |>
  mutate(total = rowSums(across(Y_GE41:Y_GE46), na.rm = TRUE)) |>
  select(id, label, parlCommitteesCode, sexCode, year, name, total) |>
    group_by(year, name, label, sexCode) |>
    summarize(chairs_gender = sum(total)) |>
    ungroup() |>
    pivot_wider(names_from = sexCode, values_from = chairs_gender) |>
    mutate(total = F + M, 
    F2 = F/total, 
    M2 = M/total)

region_gender_chairs |>
ggplot(aes(x = name, y = label, fill = F2)) +
geom_tile(color = "white") + 
scale_fill_gradient2("% women", mid = "white", low = palettes$gender[["M"]], high = palettes$gender[["F"]], labels = scales::percent_format(), na.value = "grey80", midpoint = .5, limits = c(0, 1)) +
scale_x_discrete("Region", expand = c(0, 0)) +
scale_y_discrete("Committee type", expand = c(0, 0)) + 
theme_minimal_hgrid() + 
labs(title = "Proportion of permanent committee chairs\nheld by women", 
subtitle = str_wrap("The darker the green, the more male-dominated the committee is. The darker the purple, the more female-dominated the committee is. Lighter colours are closer to gender parity (50%).", 60)) + 
theme(legend.position = "top", legend.key.width = unit(1.5, "cm"), legend.title = element_text(margin = margin(r = 15)))
#
#
#
#
#
region_age_chairs <- chair_regions |>
    filter(year == 2026, ageCode %in% c("Y_GE46", "Y0T45")) |>
    mutate(isYoung = case_when(str_detect(ageCode, "Y0") ~ "Young",
                               .default = "Old")) |>
    group_by(label, name, year, isYoung) |>      
    summarise(total_by_type = sum(value), .groups = "drop") |>
    pivot_wider(names_from = isYoung, values_from = total_by_type) |>
    select(label, name, year, Old, Young) |>
    mutate(total = rowSums(across(Old:Young), na.rm = TRUE), 
    pct= ifelse(!is.na(Young), Young/total, 0)) |>
    select(-Old, -Young, -total) |>
    pivot_wider(names_from = name, values_from = pct) |>
    pivot_longer(cols = Africa:Oceania)

pal <- palettes$age(2)

region_age_chairs |>
ggplot(aes(x = name, y = label, fill = value)) + 
geom_tile(color = "white") + 
scale_fill_gradient("% youth", low = pal[1], high = pal[2], labels = scales::percent_format(), na.value = "grey80", limits = c(0, 1)) +
scale_x_discrete("Region", expand = c(0, 0)) +
scale_y_discrete("Committee type", expand = c(0, 0)) + 
theme_minimal_hgrid() + 
labs(title = "Proportion of permanent committee chairs\nheld by young members", 
subtitle = str_wrap("The darker the yellow, the older the committee is. The darker the purple, the more young members there are. Lighter colours are closer to parity (50%).", 60)) + 
theme(legend.position = "top", legend.key.width = unit(1.5, "cm"), legend.title = element_text(margin = margin(r = 15)))
#
#
#
#
#
#
#
#| include: false
#| echo: false
pubser <- right_raw_data[["SG_DMK_PSRVC"]] |>
mutate(year = year(datetime))

dim(pubser)
colnames(pubser) # disaggs available are sexCode, pwdCode, ageCode, occupationCode, ageCode

length(unique(pubser$countryId)) # we have data for 159 countries

table(pubser$regionId) # and regions - but only 4 obs? use country data instead

unique(sort(year(pubser$datetime))) # data for 2010-2025

 pubser %>% group_by(year) %>% count() # not a lot pre-2015, exclude that 

table(pubser$sexCode)

# pubser %>% filter(sexCode != "_T") %>% distinct(occupationCode) %>% left_join(getHRDx("occupation"), by = c("occupationCode" = "code")) %>% select(occupationCode, name)
#
#
#
pubser_women <- pubser |>
filter(sexCode != "_T", occupationCode != "TOTAL_PSP", year > 2014) |>
left_join(unregions, by = c("countryId" = "m49")) |>
group_by(year, sexCode, occupationCode) |>
summarize(m = mean(value)) |>
mutate(highlights = case_when(occupationCode %in% c("GENERAL", "SENIORMAN") ~ occupationCode, 
.default = "Other"))

pubser_women |> 
filter(highlights != "Other") |>
ggplot(aes(x = year, y = m, color = occupationCode)) + 
    geom_hline(yintercept = 1, color = "gray30", linetype = "dashed") +

geom_line() + 
facet_wrap(~sexCode, labeller = as_labeller(c(`F` = "Female", `M` = "Male"))) + 
scale_color_manual(NULL, values = c("SENIORMAN" = palettes$cats[5], "GENERAL" = palettes$cats[1], "Other" = "#cccccc"), labels = scale_x_ghro_consts$occupation$labs) + 
scale_y_continuous(NULL, expand = c(0, 0), labels = function(x) {
    ifelse(
      x == 1,
      "Over-represented ↑\n\nEqual representation = 1\n\nUnder-represented ↓",
      x
    )
  }
) +
scale_x_continuous(NULL, breaks = seq(2015, 2025, by = 2)) +
theme_minimal_grid() +
theme(legend.position = "top", legend.direction="vertical") + 
labs(y = NULL)
#
#
#
#
#
#
#
#
#
#| include: false
#| echo: false
parl_w <- rbind(right_raw_data[['SG_DMK_PARLMP_LC']] |> mutate(level = "Lower or Unicameral Chamber"), right_raw_data[['SG_DMK_PARLMP_LC']] |> mutate(level = "Upper Chamber")) |>
mutate(year = year(datetime))

dim(parl_w)
colnames(parl_w) # no disaggs available

length(unique(parl_w$countryId)) # we have data for 191 countries

table(parl_w$regionId) # and regions

unique(year(parl_w$datetime)) # data for 2021-2026
#
#
#
parl_w |> 
filter(regionId == 1, level == "Lower or Unicameral Chamber") |>
ggplot(aes(x = year, y = value)) + 
geom_line(color = palettes$gender[["F"]]) +
scale_y_continuous(NULL, limits = c(0, 1.25), expand = c(0, 0), labels = function(x) {
    ifelse(
      x == 1,
      "Over-represented ↑\n\nEqual representation = 1\n\nUnder-represented ↓",
      x
    )
  }
) +
geom_hline(yintercept = 1, color = "gray30", linetype = "dashed") + 
theme_minimal_grid() 
#
#
#
#
#
#| include: false
#| echo: false
parl_y <- rbind(right_raw_data[['SG_DMK_PARLYR_LC']] |> mutate(level = "Lower or Unicameral Chamber"), right_raw_data[['SG_DMK_PARLYR_UC']] |> mutate(level = "Upper Chamber")) |>
mutate(year = year(datetime))

dim(parl_y)
colnames(parl_y) # no disaggs available

length(unique(parl_y$countryId)) # we have data for 174 countries

table(parl_y$regionId) # and regions

unique(year(parl_y$datetime)) # data for 2021-2026
#
#
#
parl_y |> 
filter(regionId == 1, level == "Lower or Unicameral Chamber") |>
ggplot(aes(x = year, y = value)) + 
geom_line(color = palettes$age(2)[2]) +
scale_y_continuous(NULL, limits = c(0, 1.25), expand = c(0, 0), labels = function(x) {
    ifelse(
      x == 1,
      "Over-represented ↑\n\nEqual representation = 1\n\nUnder-represented ↓",
      x
    )
  }
) + 
geom_hline(yintercept = 1, color = "gray30", linetype = "dashed") + 
theme_minimal_grid() 
#
#
#
#
#
#| include: false
#| echo: false
speak <- rbind(right_raw_data[['SG_DMK_PARLSP_LC']] |> mutate(level = "Lower or Unicameral Chamber"), right_raw_data[['SG_DMK_PARLSP_UC']] |> mutate(level = "Upper Chamber")) |>
mutate(year = year(datetime))

dim(speak)
colnames(speak) # sex and age are available disaggs

length(unique(speak$countryId)) # we have data for 194 countries

table(speak$regionId) # and regions

unique(year(speak$datetime)) # data for 2021-2026

speak |> filter(regionId == 1)
#
#
#
speak_sex <- speak |> 
filter(regionId == 1, sexCode %in% c("F", "M"), ageCode == "_T", level == "Lower or Unicameral Chamber") |> 
group_by(year) |> 
mutate(total_speakers = sum(value), 
pct = value/total_speakers) |>
select(sexCode, level, total_speakers, pct, year)

speak_sex |> 
ggplot(aes(x = year, y = pct)) + 
geom_hline(yintercept = .5, color = "gray30", linetype = "dashed") + 
geom_line(aes(color = sexCode)) + 
theme_minimal_grid() + 
scale_y_continuous("% of speakers", limits = c(0, 1), expand = c(0,0), labels = function(x) {
    ifelse(
      x == .5,
      "Parity",
      scales::percent(x)
    )
  }
) + 
scale_color_ghro("gender")
#
#
#
#
#
#
#| include: false
#| echo: false
wseats <- right_raw_data[["SG_GEN_PARL"]]  |>
mutate(year = year(datetime))

length(unique(speak$countryId)) # we have data for 194 countries

table(speak$regionId) # and regions

d <- UN_M.49_Regions |> mutate(Code = as.numeric(Code)) |> select(Code, Name)

test <- wseats |> distinct(year, regionId) |> drop_na() |> left_join(d, by = c("regionId" = "Code")) |> group_by(Name, regionId, Children) |> count()

m49_2 <- c(15, 202, 419, 21, 150, 9, 1, 142, 2, 19)

wseats_regions <- wseats |> 
filter(regionId %in% m49_2) |>
inner_join(d, by = c("regionId" = "Code"))
#
#
#
world_ref <- wseats_regions |> 
filter(year == max(year), regionId == 1) |>
pull(value) |> head(1)

wseats_regions |>
filter(year == max(year), regionId != 1, regionId %in% regions$regionId) |>
ggplot(aes(x = value, y = fct_reorder(Name, value))) + 
geom_col(fill = palettes$gender[["F"]]) + 
geom_vline(xintercept = world_ref, color = "gray30", linetype = "dashed") + 
geom_vline(xintercept = 50, color = "#006fb7") + 
annotate("text", x = world_ref + 1, y = 1, label = "Global average", hjust = 0) +
theme_minimal_vgrid() + 
scale_x_continuous(
    limits = c(0, 55), expand = c(0, 0),
    breaks = c(0, 10, 20, 30, 40, 50),
    labels = function(x) {
        ifelse(x == 50, "", scales::percent(x, scale = 1))
    }
) +
annotate("text", x = 50, y = 0.4, label = "Parity", 
         color = "#006fb7", hjust = 0.5, vjust = 1, fontface = "bold") + 
         coord_cartesian(clip = "off") + 
labs(x = "% seats held by women", y = NULL)
#
#
#

wseats_regions |>
filter(!regionId %in% c(2, 19)) |> 
mutate(ref_region = case_when(str_detect(Name, "Africa") ~ "Africa", 
str_detect(Name, "America") ~ "Americas", 
.default = Name)) |> 
group_by(year, ref_region) |>
summarize(value = mean(value)) |> 
ungroup() |>
mutate(is_world = ifelse(ref_region == "World", "T", "F")) |>
ggplot(aes(x = year, y = value)) + 
geom_line(aes(color = ref_region, linetype = is_world)) +
scale_color_ghro("region") + 
scale_linetype_manual(values = c("T" = "dashed" = "guide = "none")
#
#
#
#

#
#
#
