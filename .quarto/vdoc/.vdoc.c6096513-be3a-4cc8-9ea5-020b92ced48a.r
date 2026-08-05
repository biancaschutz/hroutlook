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
library(waffle)
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
library(here)
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
# getting all the data from HRDx

getHRDx <- function(code, params = NULL) {
    url <- paste0("https://hrdx-api-staging.un.org/", gsub("_", "", code), "/search")
    print(url)
    body <- if (!is.null(params)) {
        params
    } else {
        list(limit = 1000)
    }

    response <- POST(
        url = url,
        body = toJSON(body, auto_unbox = TRUE),
        content_type_json()
    )

    pages <- fromJSON(content(response, "text"), flatten = TRUE)$pagination$pages

    print(pages)

    df_list <- list()
    for (p in 1:pages) {
        response <- POST(
            url = url,
            body = toJSON(body, auto_unbox = TRUE),
            content_type_json()
        )

        res <- fromJSON(content(response, "text"), flatten = TRUE)$results

        df_list[[p]] <- res
    }

    df_list %>% rbind_pages()
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
    right_raw_data[[d]] <- getHRDx(d)
}

page <- read_html("https://unstats.un.org/unsd/methodology/m49/overview/")
t <- html_element(page, "table")
unregions <- html_table(t, header = TRUE) %>%
    select(`Region Name`, `M49 Code`, `Sub-region Name`, `ISO-alpha3 Code`, `Country or Area`) %>%
    rename(m49 = `M49 Code`, region_map = `Region Name`, subregion_map = `Sub-region Name`, country_name = `Country or Area`) %>%
    mutate(m49 = as.character(m49))
#
#
#
#
#
#| include: false
bribes <- right_raw_data[["IU_COR_BRIB"]] %>%
mutate(year = year(datetime)) %>%
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
bribe_region_avg <- bribes %>%
filter(sexCode == "_T") %>%
    group_by(year, region_map) %>%
    summarize(n = n(), 
    avg = mean(value), 
    countries = list(country_name))

bribes %>%
filter(year > 2020) %>%
ggplot(aes(x = factor(year), y = value)) + 
geom_point(aes(color = region_map), size = 5) + 
scale_color_manual(NULL, values = palettes$regions) + 
scale_y_continuous(expand = c(0, 0), labels = scales::percent_format(scale = 1)) +
scale_x_discrete(NULL)
theme_minimal_hgrid() 
#
#
#
