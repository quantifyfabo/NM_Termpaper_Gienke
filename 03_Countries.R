# By Fabian G.

# This script identifies the country referenced in Tagesschau disaster-related
# news articles. It uses a dictionary-based approach with country names and common
# aliases to automatically detect countries in the English text and assign ISO3country codes.
# Articles that cannot be classified automatically are exported for manual country labeling and re-imported.
# Automatic and manual labels are then combined into a final country variable.

# Based on the final ISO3 country codes, a region variable is added. The script
# then aggregates Tagesschau disaster articles to the country level and merges
# the resulting data with the country-level EM-DAT dataset.
# The output is a merged country-level dataset combining real disaster exposure and media coverage.
# Controll variables on GDP, Population and VDEM are added

library(dplyr)
library(stringr)
library(countrycode)
library(geosphere)
library(writexl)

# Base country list (ISO3 + English names)
countries <- countrycode::codelist %>%
  select(country.name.en, iso3c) %>%
  distinct() %>%
  filter(!is.na(iso3c)) %>%
  rename(
    iso = iso3c,
    country = country.name.en
  )

# different spellings of countries
aliases <- tibble::tibble(
  iso = c("USA","USA","USA","USA",
          "GBR","GBR",
          "RUS",
          "KOR"),
  alias = c("United States","US","U\\.S\\.","America",
            "United Kingdom","Britain",
            "Russia",
            "South Korea")
)

# build regex pattern to identify different spelling (ed, s) of country names
countries_patterns <- countries %>%
  mutate(
    pattern = paste0("\\b", country, "(s)?\\b")
  ) %>%
  select(iso, pattern)

aliases_patterns <- aliases %>%
  mutate(
    pattern = paste0("\\b", alias, "\\b")
  ) %>%
  select(iso, pattern)

country_patterns <- bind_rows(countries_patterns, aliases_patterns)


# create new variables in TS_Disaster (empty)
TS_Disaster <- TS_Disaster %>%
  mutate(
    country_auto = NA_character_,
    country_manual = NA_character_,
    country_final = NA_character_
  )



# Automatic Country Detection in TS_Disaster english text (first country mentioned only) --> output ISO3
for (i in seq_len(nrow(TS_Disaster))) {
  
  text <- TS_Disaster$ts_text_en[i]
  
  if (is.na(text) || text == "") next
  
  found_iso <- character(0)
  
  for (j in seq_len(nrow(country_patterns))) {
    if (grepl(
      country_patterns$pattern[j],
      text,
      ignore.case = TRUE,
      perl = TRUE
    )) {
      found_iso <- c(found_iso, country_patterns$iso[j])
    }
  }
  
  found_iso <- unique(found_iso)
  
  if (length(found_iso) > 0) {
    TS_Disaster$country_auto[i] <- found_iso[1]  # erstes Land
  }
}

# Check results
table(is.na(TS_Disaster$country_auto))
sort(table(TS_Disaster$country_auto), decreasing = TRUE)[1:15]
TS_Disaster %>%
  filter(is.na(country_auto)) %>%
  slice_sample(n = 10) %>%
  pull(ts_text_en)

write_xlsx(TS_Disaster, "TS_ManualLabel.xlsx")




# MANUAL LABELLING OF COUNTRIES OF UNCLASSIFIDED TEXTS
# Load labeled data

TS_Disaster_Country <- read_excel('/Users/fabiangi/Documents/Goethe Uni/Semester 3/VP Naturkatastrophen/Paper_Project/R_Project_Termpaper/TS_ManualLabel.xlsx')

# remove cases that are maunualy declared as not relevant
TS_Disaster_Country <- TS_Disaster_Country %>%
  filter(
    !(is.na(country_auto) & is.na(country_manual))
  )

# create ISO from manual country labels
TS_Disaster_Country <- TS_Disaster_Country %>%
  mutate(
    country_manual_iso = countrycode(
      sourcevar = country_manual,
      origin = "country.name",
      destination = "iso3c",
      warn = FALSE
    )
  )
TS_Disaster_Country <- TS_Disaster_Country %>%
  mutate(
    country_manual = ifelse(
      !is.na(country_manual_iso),
      country_manual_iso,
      country_manual
    )
  ) %>%
  select(-country_manual_iso)

# create variable country_final (either country_auto or country_manual is labeled at this point)
TS_Disaster_Country <- TS_Disaster_Country %>%
  mutate(
    country_final = ifelse(
      !is.na(country_auto),
      country_auto,
      country_manual
    )
  )

# Add region variable based on country ISO
TS_Disaster_Country <- TS_Disaster_Country %>%
  mutate(
    region_iso = countrycode(
      sourcevar = country_final,
      origin = "iso3c",
      destination = "region",
      warn = FALSE
    ),
    region_final = ifelse(
      country_final == "MYT",
      "Sub-Saharan Africa",
      ifelse(!is.na(region_iso), region_iso, country_final)
    )
  ) %>%
  select(-region_iso)





# Next Part: Aggregating TS Articles on Country Level
# Keep only cases with valid ISO as final country (Regions removed because they dont match emdata)
ts_country <- TS_Disaster_Country %>%
  filter(nchar(country_final) == 3) %>%
  group_by(iso = country_final) %>%
  summarise(
    ts_articles = n(),
    .groups = "drop"
  )

# merge ts and emdata
analysis_country <- ts_country %>%
  full_join(emdat_country, by = "iso")

# make na=0 for emdata cases not covered in ts
analysis_country <- analysis_country %>%
  mutate(
    ts_articles = ifelse(is.na(ts_articles), 0, ts_articles)
  )

# add region absed on iso
analysis_country <- analysis_country %>%
  mutate(
    region = countrycode(
      sourcevar = iso,
      origin = "iso3c",
      destination = "region",
      warn = FALSE
    )
  )



# Add Data for Controll variables

# GDP by World Bank 2023
gdp_country <- read.csv(
  '/Users/fabiangi/Documents/Goethe Uni/Semester 3/VP Naturkatastrophen/Paper_Project/Datasets/GDP_Worldbank.csv',
  skip = 4
)
gdp_country <- gdp_country %>%
  select(
    iso = Country.Code,
    gdp_2023 = X2023
  ) %>%
  mutate(
    gdp_2023 = gdp_2023 / 1e6
  )

# Population by World Bank 2023
population_country <- read.csv('/Users/fabiangi/Documents/Goethe Uni/Semester 3/VP Naturkatastrophen/Paper_Project/Datasets/Population_Worldbank.csv',
                               skip = 4)
population_country <- population_country %>%
  select(
    iso = Country.Code,
    population_2023 = X2023
  ) %>%
  mutate(
    population_2023 = population_2023 / 1e6
  )

# Varieties of Democracy V-DEM Score (0-1)
vdem <- read.csv('/Users/fabiangi/Documents/Goethe Uni/Semester 3/VP Naturkatastrophen/Paper_Project/Datasets/V-Dem-CY-Core-v15.csv')

vdem_country <- vdem %>%
  filter(year == 2023) %>%
  select(
    iso = country_text_id,
    democracy_vdem = v2x_libdem
  )

# Merge control variables to the country dataset
analysis_country <- analysis_country %>%
  left_join(gdp_country, by = "iso") %>%
  left_join(population_country, by = "iso") %>%
  left_join(vdem_country, by = "iso")

#add gdp per capita
analysis_country$gdp_pc_2023 <- 
  analysis_country$gdp_2023 / analysis_country$population_2023







# Add distance from germany as new variable
# Load libraries
library(countrycode)
library(geosphere)
library(maps) 

# use map library
world_data <- map_data("world")
country_coords <- aggregate(cbind(long, lat) ~ region, data = world_data, FUN = mean)

# Add ISO Codes to Coordinates
country_coords$iso <- countrycode(country_coords$region, origin = "country.name", destination = "iso3c")

# Add coordinates to analysis_country dataset
analysis_country <- merge(analysis_country, country_coords[, c("iso", "lat", "long")], by = "iso", all.x = TRUE)

# Define Berlin coordinates 
berlin_coord <- c(13.4050, 52.5200) 

# Calculate distance in km
analysis_country$dist_to_germany <- distHaversine(
  cbind(analysis_country$long, analysis_country$lat), 
  berlin_coord
) / 1000

# Manual Fix for HongKong and Macau (as not covered by Maps library)
china_dist <- analysis_country$dist_to_germany[analysis_country$iso == "CHN"]
analysis_country$dist_to_germany[analysis_country$iso %in% c("HKG", "MAC")] <- china_dist

# Delete Coordinates variable
analysis_country$lat <- NULL
analysis_country$long <- NULL

# Check Results
head(analysis_country[, c("iso", "dist_to_germany")])



# safe resulting dataset
write.csv(analysis_country, "analysis_country_final.csv", row.names = FALSE)
