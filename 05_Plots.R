library(tidyverse)
library(readxl)
library(writexl)
library(sjPlot)
library(modelsummary)
library(gt)
library(MASS)
library(car)
library(gt)
library(corrplot)

# load final dataset (Final_Dataset_CountryAnalysis.csv)
analysis_country <- read.csv('/Users/fabiangi/Documents/Goethe Uni/Semester 3/VP Naturkatastrophen/Paper_Project/R_Project_Termpaper/NM_Termpaper_Gienke/analysis_country_final.csv')

# -- Plot R1
# show individual correlations

# correlation matrix
vars <- analysis_country %>%
  select(ts_articles, n_events, total_deaths, total_injured, total_affected,
         gdp_2023, population_2023, democracy_vdem, gdp_pc_2023, dist_to_germany)

cor_matrix <- cor(vars, use = "complete.obs")

corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200),
  tl.col = "black",
  tl.cex = 0.9,
  addCoef.col = "black",
  number.cex = 0.7
)
# --

# Plot full regression
# LM
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

# Plot as Coefficient etimate plot R2
plot_model(
  lm_full,
  type = "est",
  show.values = TRUE,
  value.offset = 0.3,
  title = "Determinants of Tagesschau Disaster Coverage",
  axis.title = c("Effect on Tagesschau coverage (log scale)", "")
) +
  geom_hline(
    yintercept = 0,
    color = "black",
    linewidth = 0.5,
    alpha = 0.25
  ) +
  scale_color_discrete(guide = "none") +
  scale_fill_discrete(guide = "none") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10))
  )

# Plot as Tabel R3
tab_model(
  lm_full,
  show.se = TRUE,
  show.ci = FALSE,
  show.r2 = TRUE,
  show.obs = TRUE,
  p.style = "numeric",
  show.p = TRUE,
  digits = 3,
  dv.labels = "Tagesschau coverage (log TS articles)",
  pred.labels = c(
    "Intercept",
    "Disaster Events (log)",
    "Total Deaths (log)",
    "Total Injured (log)",
    "Total Affected (log)",
    "GDP (log)",
    "Population (log)",
    "Distance to Germany (log)",
    "Democracy (V-Dem)"
  )
)


# Plot Region Scatter
ggplot(
  analysis_country,
  aes(
    x = log(n_events + 1),
    y = log(ts_articles + 1),
    color = region
  )
) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    x = "Log natural disaster events (EM-DAT)",
    y = "Log Tagesschau disaster articles",
    color = "Region"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )


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
  # Hier die Änderung:
  dplyr::select(region, ts_articles, n_events, coverage_diff) %>% 
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


# regions as diagramm
total_articles <- sum(region_table$ts_articles)
total_events <- sum(region_table$n_events)

ggplot(region_table, aes(x = reorder(region, coverage_diff), y = coverage_diff, fill = coverage_diff > 0)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.7) +
  coord_flip() +
  # fix x scale
  scale_y_continuous(limits = c(-25, 25), breaks = seq(-25, 25, 5)) +
  # add color
  scale_fill_manual(values = c("TRUE" = "#2c7bb6", "FALSE" = "#d7191c")) +
  theme_minimal() +
  labs(
    title = "TS Media Coverage Bias by Region (based on analysis_country) ",
    x = "",
    y = "Coverage Difference (Percentage Points)",
    caption = "Calculated as: (Share of ts_articles per region) - (Share of n_events per region)"
  ) +
  # add percent labels
  geom_text(aes(label = paste0(ifelse(coverage_diff > 0, "+", ""), round(coverage_diff, 1), "%")), 
            hjust = ifelse(region_table$coverage_diff > 0, -0.2, 1.2), 
            size = 3.5, fontface = "bold") +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", alpha = 0.3)



# apply same for iso (countries) --> Table to check percentage difference in coverage/events on country lvl


# Berechnung auf ISO lvl
country_bias <- analysis_country %>%
  filter(!is.na(iso)) %>% 
  group_by(iso) %>%
  summarise(
    ts_articles = sum(ts_articles, na.rm = TRUE),
    n_events = sum(n_events, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    article_share = (ts_articles / sum(ts_articles)) * 100,
    event_share = (n_events / sum(n_events)) * 100,
    coverage_diff = article_share - event_share
  ) %>%
  arrange(desc(coverage_diff))

top_bottom_countries <- bind_rows(
  head(country_bias, 10),
  tail(country_bias, 10)
)
top_bottom_countries %>%
  gt() %>%
  tab_header(
    title = "Country-Level Media Coverage Bias (ISO)",
    subtitle = "Top 10 and Bottom 10 Countries by Coverage Difference"
  ) %>%
  cols_label(
    iso = "ISO Code",
    ts_articles = "Articles (n)",
    n_events = "Events (n)",
    article_share = "Article Share (%)",
    event_share = "Event Share (%)",
    coverage_diff = "Diff (pp)"
  ) %>%
  fmt_number(columns = c(article_share, event_share, coverage_diff), decimals = 2) %>%
  tab_style(
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(rows = coverage_diff > 0)
  ) %>%
  tab_style(
    style = cell_fill(color = "salmon", alpha = 0.5),
    locations = cells_body(rows = coverage_diff < 0)
  )





# Plot VIF for the base LM model:
vif_values <- vif(lm_full)
vif_values

vif_df <- data.frame(
  Variable = names(vif_values),
  VIF = as.numeric(vif_values)
) %>%
  mutate(Variable = gsub("log\\(|\\+ 1\\)", "", Variable)) # Säubert die Namen (entfernt log und +1)
# table
vif_table <- vif_df %>%
  gt() %>%
  tab_header(title = "Multicollinearity Diagnostic") %>%
  cols_label(Variable = "Independent Variable", VIF = "VIF Score") %>%
  fmt_number(columns = VIF, decimals = 2)

vif_table





# plot nb results regression table (calculated in 04_analyze.R)
tab_model(nb_full, 
          show.ci = FALSE,
          show.se = TRUE,
          p.style = "numeric_stars", 
          dv.labels = "Negative Binomial Model (Articles)",
          string.pred = "Predictors",
          string.est = "Estimates (Log-Odds)")
