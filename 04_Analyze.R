library(tidyverse)
library(readxl)
library(writexl)
library(sjPlot)
library(modelsummary)
library(gt)
library(MASS)
library(car)
library(gt)


# load final dataset (Final_Dataset_CountryAnalysis.csv)
analysis_country <- read.csv('/Users/fabiangi/Documents/Goethe Uni/Semester 3/VP Naturkatastrophen/Paper_Project/R_Project_Termpaper/NM_Termpaper_Gienke/analysis_country_final.csv')


# check for correlations individually
analysis_country %>%
  select(ts_articles, n_events, total_deaths, total_injured, total_affected,
         gdp_2023, population_2023, democracy_vdem, gdp_pc_2023, dist_to_germany) %>%
  cor(use = "complete.obs") %>%
  .["ts_articles",]


# create analysis_country_robust by removing all cases with NA in an non-disaster-variable
analysis_country_robust <- analysis_country %>%
  filter(
    !is.na(gdp_2023),
    !is.na(population_2023),
    !is.na(democracy_vdem),
    !is.na(gdp_pc_2023),
    !is.na(dist_to_germany)
  )


# Linear Regression including all variables
lm_full <- lm(
  log(ts_articles + 1) ~
    log(n_events + 1) +
    log(total_deaths + 1) +
    log(total_injured + 1) +
    log(total_affected + 1) +
    log(gdp_2023) +
    log(population_2023) +
    log(dist_to_germany) +
    democracy_vdem,
  data = analysis_country_robust
)
summary(lm_full)


# check for Multicollinearity
vif_values <- vif(lm_full)
vif_values



# compare regions (descriptive)
region_table <- analysis_country %>%
  group_by(region) %>%
  summarise(
    ts_articles = sum(ts_articles, na.rm = TRUE),
    n_events = sum(n_events, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    article_share = ts_articles / sum(ts_articles),
    event_share = n_events / sum(n_events),
    coverage_diff = (article_share - event_share) * 100
  ) %>%
  select(region, ts_articles, n_events, coverage_diff) %>%
  arrange(desc(coverage_diff))

region_table %>%
  gt() %>%
  cols_label(
    region = "Region",
    ts_articles = "TS Articles",
    n_events = "Disaster Events",
    coverage_diff = "Coverage Difference (%)"
  ) %>%
  fmt_number(columns = coverage_diff, decimals = 1) %>%
  grand_summary_rows(
    columns = c(ts_articles, n_events),
    fns = list("Total (n)" = ~sum(.))
  )
