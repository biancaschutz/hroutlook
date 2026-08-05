library(here)
library(ISOcodes)
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

# get data from HRDx

getHRDx <- function(code, params = list(), offline = FALSE, raw = NULL) {
    if (offline) {
        read.csv(paste0(raw, "/", code, ".csv"))
    } else {
        url <- paste0("https://hrdx-api-staging.un.org/", gsub("_", "", code), "/search")
        params$limit <- 1000

        response <- POST(
            url = url,
            body = toJSON(params, auto_unbox = TRUE),
            content_type_json()
        )

        pages <- fromJSON(content(response, "text"), flatten = TRUE)$pagination$pages

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

### REGIONAL MAPPING AND COUNTRY INFORMATION ###
countries <- getHRDx("country", offline = TRUE, raw = here("data/raw/")) |> 
mutate(id = as.integer(id), across(starts_with("level"), as.integer))

regions <- getHRDx("region", offline = TRUE, raw = here("data/raw/")) |>
mutate(id = as.integer(id))

### STYLES ###
palettes <- list(
    highlight = c(
        "Y" = "#006fb7",
        "N" = "#cccccc"
    ),
    regions = sort(c(
        "Africa" = "#f78e1e",
        "Asia" = "#81ac31",
        "Americas" = "#16b5c5",
        "Europe" = "#b95380",
        "Oceania" = "#8a69a1", 
        "Other*" = "grey40"
    )),
    cats = c(
        "#009e73",
        "#56b4e9",
        "#e69f00",
        "#cc79a7",
        "#d55e00",
        "#f0e442",
        "#0072b2"
    ),
    cats2 = c(
        "#f84c1e", 
        "#E7B800", 
        "#00AFBB", 
        "#8B4769", 
        "#1D457F", 
        "#94b54e"
    ), 
    gender = c("F" = "#990099", "M" = "#009900", "P" = "#006fb7"),
    disability = c("PD" = "#7cc3e1", "PWD" = "#eede77"),
    age = colorRampPalette(c("#f7d2ca", "#e5664a")),
    occupation = colorRampPalette(c("#e5d8ec", "#7d52ad")),
    education = colorRampPalette(c("#ddeefc", "#0e6fc0")),
    urbanrural = c("RURAL" = "#bedb71", "URBAN" = "#7e9e9d"), 
    wealth = colorRampPalette(c("#f7ecd6", "#825516"))
)

orange <- "#faa31b"

occ <- read.csv(here("data/raw/occupation_key.csv"))

edu <- read.csv(here("data/raw/education_key.csv"))

occ_labs <- occ$name
names(occ_labs) <- occ$code

edu_labs <- edu$name
names(edu_labs) <- edu$code

scale_x_ghro_consts <- list(
    "gender" = list(
        "vals" = palettes$gender,
        "labs" = c("F" = "Female", "M" = "Male", "P" = "Parity")
    ),
    "disability" = list(
        "vals" = palettes$disability,
        "labs" = c("PD" = "Persons with disability", "PWD" = "Persons without disability")
    ),
    "urbanrural" = list(
        "vals" = palettes$urbanrural,
        "labs" = c("URBAN" = "Urban", "RURAL" = "Rural")
    ),
    "region" = list("vals" = palettes$regions, "labs" = waiver()), 
    "highlight" = list("vals" = palettes$highlight), 
    "misc" = list("vals" = palettes$cats)
)

scale_color_ghro <- function(palette, label = NULL) {
    if (is.null(palette) || !palette %in% names(scale_x_ghro_consts)) {
        stop(paste0("Provide a valid palette name: ", paste0(names(scale_x_ghro_consts), collapse = ", ")))
    }

    pal_labs <- scale_x_ghro_consts[[palette]][["labs"]]

    scale_color_manual(
        label,
        values = scale_x_ghro_consts[[palette]][["vals"]],
        labels = if (!is.null(pal_labs)) pal_labs else waiver()
    )
}
scale_fill_ghro <- function(palette, label = NULL) {
    if (is.null(palette) || !palette %in% names(scale_x_ghro_consts)) {
        stop(paste0("Provide a valid palette name: ", paste0(names(scale_x_ghro_consts), collapse = ", ")))
    }

    pal_labs <- scale_x_ghro_consts[[palette]][["labs"]]

    scale_fill_manual(
        label,
        values = scale_x_ghro_consts[[palette]][["vals"]],
        labels = if (!is.null(pal_labs)) pal_labs else waiver()
    )
}

### DATA EXPLORATION ### 
eda <- function(df, detailed = FALSE) {
    if (!all(c("datetime", "regionId", "countryId") %in% colnames(df))) {
        stop("Invalid HRDx data structure")
    }
    disaggs <- df |>
        select(
            ends_with("Code") & 
            !starts_with(c("datetimeFormat", "unit", "reportingAggregation", 
            "observationStatus", "natureEstimate", "dataset"))) |>
        colnames()

    if (length(disaggs) == 0) {
        cat("No disaggregations available.\n")
    }

    else {
        for (disagg in disaggs) {
            message(paste0(
                "Data is disaggregated by ", gsub("Code", "", disagg),
                "."
            ))
            if (detailed) {
                message(" The following groups are available with n observations:")
            labels <- getHRDx(gsub("Code", "", disagg))
            counts <- df |>
                count(.data[[disagg]]) |>
                left_join(labels, by = join_by(!!disagg == code)) |>
                select(name, n)

            colnames(counts) <- c(str_to_title(gsub("Code", "", disagg)), "Observations")
            print(counts)
            cat("\n")
            }
        }
    }

    data_regions <- df |>
        filter(!is.na(regionId)) |>
        left_join(regions |> mutate(id = as.character(id)), by = c("regionId" = "id")) |>
        count(name)
    
    if (nrow(data_regions) == 0) {
        cat("There are no region aggregated observations in this dataset.")
    }
    else { 
    cat(paste0("There are ", length(unique(data_regions$name)), " regions in the dataset."))
    
    if (detailed) {
    print(data_regions)

    }

    if ("World" %in% data_regions$name) {
        cat("\nGlobally-aggregated data is available.")
    }
    }
            cat("\n")

    data_country <- df |>
        filter(!is.na(countryId)) |>
        left_join(countries |> mutate(id = as.character(id)), by = c("countryId" = "id")) |>
        count(name)
    
    if (nrow(data_country) == 0) {
        cat("There are country-level observations in this dataset.")
    }
    else { 
    cat(paste0("There are ", length(unique(data_country$name)), " countries in the dataset."))
    
    if (detailed) {
    print(data_country)

    }    }
                cat("\n")

    years <- df |>
        mutate(year = year(datetime)) |>
        count(year)
    
    most_recent <- max(years$year)
    
    cat(paste0("The most recent data is from ", most_recent, " with ", years |> filter(year == most_recent) |> pull(n), " observations.\n"))
    if (detailed) {
        print(years)
    }
}

### TRANSFORMATION HELPER FUNCTIONS ###

symmetrize_ratio <- function(ratio) {
  ifelse(ratio <= 1, ratio, 2 - 1/ratio)
}

populations <- read.csv(here("data/raw/unpop2000.csv")) |> 
select(Iso3, Time, Value) |>
 rename(pop = Value) |> 
 left_join(countries |> select(iso3, level1, id), by = c("Iso3" = "iso3")) |> 
 left_join(regions |> select(id, name), by = c("level1" = "id")) |>
 mutate(name = ifelse(Iso3 == "TWN", "Asia", name)) |>
 select(id, name, pop, Iso3, Time) |>
 rename(region = name)

popAverage <- function(df, value_col = "value", iso3_col = "iso3", year_col = "year", other_groupings = c()) {
  df2 <- df |> 
    filter(!is.na(.data[["countryId"]])) |>
    left_join(populations, by = setNames(c("Iso3", "Time"), c(iso3_col, year_col))) |>
    select(all_of(c(value_col, iso3_col, year_col, "pop", "region", other_groupings)))
  
  df2 |> 
    group_by(across(all_of(c(year_col, other_groupings, "region")))) |>
    mutate(total_pop = sum(pop)) |>
    ungroup() |>
    mutate(wt = pop/total_pop, 
    wi = wt * .data[[value_col]]) |>
        group_by(across(all_of(c(year_col, other_groupings, "region")))) |>
    summarize(wt_avg = sum(wi))
}

ghro_world <- function(hrdxCode, xaxis = NULL, yaxis = NULL, scales_function = waiver()) {
 tryCatch(
     # this is the chunk of code we want to run
     {
df <- getHRDx(hrdxCode, params = list(where = list(regionId = c(1), datetime = paste0("01-01-", 2010:2026))))
     },
     error = function(msg) {
message(paste("Use a valid HRDx dataset code."))
            return(NA)     }
 )
     
     disaggs <- df |>
         select(
             ends_with("Code") &
                 !starts_with(c(
                     "datetimeFormat", "unit", "reportingAggregation",
                     "observationStatus", "natureEstimate", "dataset"
                 ))
         ) |>
         colnames()
     for (d in disaggs) {
         df <- df |> filter(.data[[d]] %in% c("_T"))
     }

     range <- c(min(df$value), max(df$value))

     if ((max(df$value) - min(df$value)) < 10) {
        limits <- c(min(df$value) - 5, max(df$value) + 5)
     } else {
        limits <- NULL
     }
     
     df |>
         ggplot(aes(x = year(datetime), y = value)) +
         geom_line(color = "#006fb7") +
         scale_x_continuous(xaxis) +
         scale_y_continuous(yaxis, labels = scales_function, limits = limits) +
             theme_minimal_grid()
}