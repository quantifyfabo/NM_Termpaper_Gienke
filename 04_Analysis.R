library(dplyr)
library(writexl)
library(stringr)
library(tidyverse)
library(readxl)
library(ggplot2)
library(sjPlot)
library(modelsummary)
library(gt)


#check for correlation
analysis_corr <- analysis_country %>%
  filter(n_events > 0) # minimum of 1 case in emdata

# Pearson corr with n_events and total deaths
cor(
  analysis_corr$ts_articles,
  analysis_corr$n_events,
  method = "pearson"
)

cor(
  analysis_corr$ts_articles,
  analysis_corr$total_deaths,
  method = "pearson"
)

# Spearmann correlation because data is unequal
cor(
  analysis_corr$ts_articles,
  analysis_corr$n_events,
  method = "spearman"
)

cor(
  analysis_corr$ts_articles,
  analysis_corr$total_deaths,
  method = "spearman"
)

# Log Log Regression for n_events
model_events <- lm(
  log(ts_articles + 1) ~ log(n_events + 1),
  data = analysis_country
)

summary(model_events)

# log log regression for deaths
model_deaths <- lm(
  log(ts_articles + 1) ~ log(total_deaths + 1),
  data = analysis_country
)

summary(model_deaths)

# log log combined
model_both <- lm(
  log(ts_articles + 1) ~ log(n_events + 1) + log(total_deaths + 1),
  data = analysis_countries
)

summary(model_both)



# Model including control variables
# create dataset that includes only cases where gpd, population and vdem != na
# This excludes 20 cases (12%) of the dataset
analysis_country_robust <- analysis_country %>%
  filter(
    !is.na(gdp_2023),
    !is.na(population_2023),
    !is.na(democracy_vdem)
  )

robust_model <- lm(
  log(ts_articles + 1) ~ 
    log(n_events + 1) +
    log(gdp_2023) +
    log(population_2023) +
    democracy_vdem,
  data = analysis_country_robust
)

summary(robust_model)

# Visual Upgradet Regressiontable for controls
tab_model(
  robust_model,
  show.se = TRUE,
  show.ci = FALSE,
  show.r2 = TRUE,
  show.icc = FALSE,
  show.obs = TRUE,
  dv.labels = "Tagesschau coverage (log TS articles)",
  pred.labels = c(
    "Intercept",
    "Disaster Events (log)",
    "GDP (log, million USD)",
    "Population (log, million)",
    "Democracy (V-Dem)"
  ),
  string.pred = "Predictors",
  string.est = "Estimate",
  string.se = "Std. Error",
  string.p = "p-value",
  p.style = "numeric",
  digits = 2
)



# plot regression with controls
plot_model(
  robust_model,
  type = "est",
  show.values = TRUE,
  value.offset = 0.3,
  title = "Determinants of natural disaster TS coverage",
  axis.title = c("Effect on Tagesschau coverage (log scale)", "")
) +
  # dezente Null-Linie im Hintergrund
  geom_hline(
    yintercept = 0,
    color = "black",
    linewidth = 0.5,
    alpha = 0.25
  ) +
  # einheitliche Farbe für Punkte & CI
  scale_color_discrete(guide = "none") +
  scale_fill_discrete(guide = "none") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10))
  )






# visualize relationshop between ts_articles and n_events

ggplot(
  analysis_country,
  aes(
    x = n_events,
    y = ts_articles,
    color = region
  )
) +
  geom_point(
    alpha = 0.7,
    size = 2
  ) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    x = "Number of natural disaster events (EM-DAT)",
    y = "Number of Tagesschau disaster articles",
    color = "Region"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )


# same scatter but with total deaths
ggplot(
  analysis_country,
  aes(
    x = total_deaths,
    y = ts_articles,
    color = region
  )
) +
  geom_point(
    alpha = 0.7,
    size = 2
  ) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    x = "Total deaths from natural disasters (EM-DAT)",
    y = "Number of Tagesschau disaster articles",
    color = "Region"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )


