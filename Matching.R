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
