library(dplyr)
library(stringr)
library(countrycode)

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

write.csv(
  TS_Disaster,
  "TS_Disaster_CountryCoding.xlsx",
  row.names = FALSE
)
