library(countrycode)
library(countrycode)
library(dplyr)

# Country List with ISO to identify countries within TS Snippets
countries <- countrycode::codelist %>%
  select(country.name.en, iso3c) %>%
  distinct() %>%
  filter(!is.na(iso3c))


countries <- countries %>%
  rename(
    iso = iso3c,
    country_name = country.name.en
  )


aliases <- data.frame(
  iso = c(
    "USA","USA","USA","USA",
    "GBR","GBR",
    "RUS",
    "KOR"
  ),
  alias = c(
    "United States",
    "US",
    "U\\.S\\.",
    "America",
    "United Kingdom",
    "Britain",
    "Russia",
    "South Korea"
  ),
  stringsAsFactors = FALSE
)

countries_patterns <- countries %>%
  mutate(
    pattern = paste0(
      "\\b",
      country_name,
      "(s)?",
      "\\b"
    )
  ) %>%
  select(iso, pattern)

aliases_patterns <- aliases %>%
  mutate(
    pattern = paste0("\\b", alias, "\\b")
  ) %>%
  select(iso, pattern)

country_patterns <- rbind(countries_patterns, aliases_patterns)

head(country_patterns)


# Use Iso Finder
ts_iso_found <- data.frame(
  snippet_id = integer(),
  iso = character(),
  stringsAsFactors = FALSE
)

n_snippets <- nrow(ts_data_clean)

for (i in seq_len(n_snippets)) {
  
  text <- ts_data_clean$ts_text_en[i]
  
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
    ts_iso_found <- rbind(
      ts_iso_found,
      data.frame(
        snippet_id = i,
        iso = found_iso,
        stringsAsFactors = FALSE
      )
    )
  }
  
# Progression Bar
  if (i %% 1000 == 0) {
    cat("Processed", i, "of", n_snippets, "snippets\n")
  }
}
# Check top countries (result)
sort(table(ts_iso_found$iso), decreasing = TRUE)[1:15]
nrow(ts_iso_found)


# Match Results with TS 
ts_iso_wide <- ts_iso_found %>%
  group_by(snippet_id) %>%
  mutate(n = row_number()) %>%
  filter(n <= 5) %>%          # max. 5 Länder pro Snippet
  pivot_wider(
    names_from = n,
    values_from = iso,
    names_prefix = "country_"
  )


ts_data_clean <- ts_data_clean %>%
  mutate(snippet_id = row_number()) %>%
  left_join(ts_iso_wide, by = "snippet_id")

head(ts_iso_wide)







# Matching TS ISO Results with emdat ISO
emdat_events$coverage_auto_n <- 0L

for (i in seq_len(nrow(emdat_events))) {
  
  event <- emdat_events[i, ]
  
  matches <- ts_data_clean %>%
    filter(
      date >= event$start_date,
      date <= event$window_end,
      country_1 == event$iso |
        country_2 == event$iso |
        country_3 == event$iso |
        country_4 == event$iso |
        country_5 == event$iso
    )
  
  emdat_events$coverage_auto_n[i] <- nrow(matches)
}

emdat_events$covered_auto_yn <- emdat_events$coverage_auto_n > 0


# Check results
summary(emdat_events$coverage_auto_n)
quantile(emdat_events$coverage_auto_n, probs = c(.9, .95, .99))
table(emdat_events$covered_auto_yn)
