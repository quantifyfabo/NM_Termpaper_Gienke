# By Fabian G.
## Purpose of this file
# This file identifies disaster-related news articles in the Tagesschau foreign news dataset. The goal is to separate articles about natural disasters from all other foreign news.
# A dictionary-based approach is used. A list of disaster-related terms is created based on EM-DAT disaster types. Regular expressions are used to capture common linguistic variations.
# The output consists of two datasets: disaster-related and non-disaster articles.

library(stringr)
library(quanteda)

# identify relevant words for disaster_terms with help of emdat
unique(emdat_clean$`Disaster Type`)
unique(emdat_clean$`Disaster Subgroup`)
unique(emdat_clean$`Disaster Subtype`)


# All relevant words to filter for articles related to natural hazards (selection)
disaster_terms <- c(
  # earthquakes & geophysical
  "earthquake", "aftershock", "ground movement", "rockfall", "mass movement",
  "landslide", "mudslide", "avalanche",
  "volcanic eruption", "volcano", "volcanic activity", "lava flow",
  "ash fall", "pyroclastic flow", "lahar",
  "tsunami",
  
  # hydrological
  "flood", "flooding", "flash flood", "riverine flood",
  "storm surge", "glacial lake outburst flood",
  
  # meteorological
  "storm", "severe storm", "storm system",
  "tropical cyclone", "cyclone", "hurricane", "typhoon",
  "extra-tropical storm",
  "thunderstorm", "lightning",
  "tornado", "hail",
  "blizzard", "winter storm", "severe winter conditions",
  "sand storm", "dust storm",
  
  # climatological
  "drought",
  "heatwave", "heat wave", "extreme heat",
  "cold wave",
  
  # fires
  "wildfire", "forest fire", "bushfire",
  
  # biological
  "epidemic", "pandemic",
  "infectious disease", "viral disease", "bacterial disease",
  
  # technological / industrial (optional, EM-DAT consistency)
  "gas leak",
  "oil spill",
  "poisoning"
)


# Pattern: Allow for different endings to include all relevant versions of disaster_terms (s|es|ed|ing)
make_flex_pattern <- function(term) {
  term_escaped <- stringr::str_replace_all(term, " ", "\\\\s*")
  paste0("\\b", term_escaped, "(s|es|ed|ing)?\\b")
}


# Apply the Pattern to the disaster_terms
disaster_patterns <- vapply(
  disaster_terms,
  make_flex_pattern,
  character(1)
)


# start the dictionary based approach using quanteda
# tokenize TS Data (english text)
corp <- corpus(ts_data_clean, text_field = "ts_text_en")
toks <- tokens(
  corp,
  remove_punct   = TRUE,
  remove_numbers = TRUE
) |>
  tokens_tolower()

dfm_ts <- dfm(toks)


# use quanteda to combine dictionary with DFM of TS Data and filter by disaster_patterns 
dict_disaster <- dictionary(list(
  disaster = disaster_patterns
))

dfm_hits <- dfm_lookup(
  dfm_ts,
  dict = dict_disaster,
  valuetype = "regex"
)

# create new variable disaster_hits (number) und is_disaster (True, False)
ts_data_clean$disaster_hits <- as.numeric(dfm_hits[, "disaster"])
ts_data_clean$is_disaster  <- ts_data_clean$disaster_hits > 0

# Result (split )
TS_Disaster   <- ts_data_clean |> dplyr::filter(is_disaster)
TS_NoDisaster <- ts_data_clean |> dplyr::filter(!is_disaster)



