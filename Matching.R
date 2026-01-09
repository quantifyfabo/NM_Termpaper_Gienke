library(dplyr)
library(tidyr)
library(purrr)

ts_data_clean <- ts_data_clean %>%
  mutate(date = as.Date(date))

# iso_list absichern (keine NULL / leeren / NA-Listen)
ts_data_clean <- ts_data_clean %>%
  mutate(
    iso3_list = map(
      iso3_list,
      ~ if (is.null(.x) || length(.x) == 0 || all(is.na(.x))) character(0) else .x
    )
  )

emdat_events <- emdat_events %>%
  mutate(
    start_date = as.Date(start_date),
    window_end = as.Date(window_end)
  )

ts_long <- ts_data_clean %>%
  mutate(snippet_id = row_number()) %>%
  unnest(iso3_list) %>%
  rename(iso = iso3_list)

coverage_counts <- emdat_events %>%
  select(event_id, iso, start_date, window_end) %>%
  left_join(ts_long, by = "iso") %>%
  filter(
    date >= start_date,
    date <= window_end
  ) %>%
  count(event_id, name = "coverage_auto_n")

emdat_events <- emdat_events %>%
  left_join(coverage_counts, by = "event_id")

# falls es keine Matches gab → Spalte existiert noch nicht
if (!"coverage_auto_n" %in% names(emdat_events)) {
  emdat_events$coverage_auto_n <- 0L
}

emdat_events <- emdat_events %>%
  mutate(
    coverage_auto_n = replace_na(coverage_auto_n, 0L),
    covered_auto_yn = coverage_auto_n > 0
  )


summary(emdat_events$coverage_auto_n)
quantile(emdat_events$coverage_auto_n, probs = c(.9, .95, .99), na.rm = TRUE)
table(emdat_events$covered_auto_yn)
