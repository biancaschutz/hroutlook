## consts
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

symmetrize_ratio <- function(ratio) {
  ifelse(ratio <= 1, ratio, 2 - 1/ratio)
}

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
        "Oceania" = "#8a69a1"
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
        message("No disaggregations available.\n")
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
        left_join(regionISO, by = c("regionId" = "Code")) |>
        count(Name)
    
    if (nrow(data_regions) == 0) {
        message("There are no region aggregated observations in this dataset.")
    }
    else { 
    message(paste0("There are ", length(unique(data_regions$Name)), " regions in the dataset."))
    
    if (detailed) {
    print(data_regions)

    }

    if ("World" %in% data_regions$Name) {
        message("\nGlobally-aggregated data is available.")
    }
    }
            cat("\n")

    data_country <- df |>
        filter(!is.na(countryId)) |>
        left_join(countryISO, by = c("countryId" = "Code")) |>
        count(Name)
    
    if (nrow(data_country) == 0) {
        message("There are country-level observations in this dataset.")
    }
    else { 
    message(paste0("There are ", length(unique(data_country$Name)), " countries in the dataset."))
    
    if (detailed) {
    print(data_country)

    }    }
                cat("\n")

    years <- df |>
        mutate(year = year(datetime)) |>
        count(year)
    
    most_recent <- max(years$year)
    
    message(paste0("The most recent data is from ", most_recent, " with ", years |> filter(year == most_recent) |> pull(n), " observations.\n"))
    if (detailed) {
        print(years)
    }
}