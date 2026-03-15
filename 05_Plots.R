library(tidyverse)
library(readxl)
library(writexl)
library(sjPlot)
library(modelsummary)
library(gt)
library(MASS)
library(car)

# load final dataset (Final_Dataset_CountryAnalysis.csv)
analysis_country <- read.csv('/Users/fabiangi/Documents/Goethe Uni/Semester 3/VP Naturkatastrophen/Paper_Project/R_Project_Termpaper/NM_Termpaper_Gienke/analysis_country_final.csv')


# -- Plot R1
# show individual correlations
library(corrplot)

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
