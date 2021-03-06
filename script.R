library(tidyverse)

source("fetch_free_school_meals.R")
source("fetch_pupils.R")
source("fetch_children_income_deprivation.R")
source("extract_children_in_relative_poverty.R")

constituencies <- read_csv("data/constituencies.csv") %>%
  left_join(crp_by_constituency, by='ConstituencyCodeW07') %>%
  left_join(fsm_by_constituency, by='ConstituencyCode') %>%
  left_join(pupils_by_constituency, by='ConstituencyCode') %>%
  left_join(incdep_by_constituency, by='ConstituencyCode') %>%
  mutate(
    PropEligibleForFreeSchoolMeals5to15 = 
    CountEligibleForFreeSchoolMeals5to15.Total/CountAllPupils5to15.Total
  )

constituencies %>% write_csv("outputs/stats_by_constituency.csv")

library(sf)
library(biscale)
library(ggplot2)
library(cowplot)

constituency_boundaries <- st_read("data/constituency_boundaries.geojson") %>%
  rename(ConstituencyCode = nawc16cd) %>%
  left_join(
    constituencies,
    by='ConstituencyCode'
  )

fsm_vs_relpov_pal = "DkViolet"

fsm_vs_relpov_map <- ggplot() + 
  geom_sf(constituency_boundaries  %>% 
      bi_class(
        x=PropChildrenUnder16InRelativePovertyAfterHousingCosts, 
        y=PropEligibleForFreeSchoolMeals5to15), 
    mapping = aes(fill=bi_class), color = "white", size = 0.1, show.legend = FALSE) + 
  bi_scale_fill(pal = fsm_vs_relpov_pal, dim = 3) +
  theme_void()

fsm_vs_relpov_legend <- bi_legend(
    pal = fsm_vs_relpov_pal,
    dim = 3,
    xlab = "",#"\n\nHigher % of children \naged 0 to 15 \nin income deprivation",
    ylab = "",#"Higher % of children \naged 5 to 15 eligible for \nfree school meals",
    size = 12
  )

fsm_vs_relpov_legend_plot <- ggdraw() +
  draw_plot(fsm_vs_relpov_legend, 0.05, 0.05, 0.9, 0.9) + 
  draw_text("Red areas may be particularly \nbadly served by current policy.",
    0.85, 0.38, hjust=0, size=6.5, colour = '#ad394e'
    ) +
  # x-axis
  draw_text("Higher % of children \naged 0 to 15\nin relative poverty\nafter housing costs",
    0.58, -0.08, hjust=0.5, vjust=0, size=7
  ) +
  # y-axis
  draw_text("Higher % of children \naged 5 to 15 eligible for \nfree school meals",
    0, 0.5, hjust=0.5, vjust=0.5, size=7, angle=90
  )

fsm_vs_relpov_plot <- ggdraw() +
  draw_text(
    "Eligibility for Free School Meals in 2020 vs\nEstimated Relative Poverty After Housing Costs"
  , 0.5, 0.98, size=14, hjust=0.5, vjust=1, family="Bahnschrift") +

  draw_plot(fsm_vs_relpov_map, 0.005, 0, 0.95, 0.95) +
  draw_plot(fsm_vs_relpov_legend_plot, 0.05, .38, 0.28, 0.28) + 
  draw_text(
    "‘Relative poverty’ is estimated using DWP/HMRC income statistics
combined with local housing costs information.
Source: endchildpoverty.org.uk, CRSP at Loughborough University
"
  , 0.05, 0.05, size=6.5, hjust=0 , colour="#777777")

save_plot("outputs/fsm_vs_relpov.png", fsm_vs_relpov_plot, base_width=6.5, base_height=7.1)

fsm_vs_incdep_pal = "DkViolet"

fsm_vs_incdep_map <- ggplot() + 
  geom_sf(constituency_boundaries  %>% 
      bi_class(
        x=PropIncDepUnder16, 
        y=PropEligibleForFreeSchoolMeals5to15), 
    mapping = aes(fill=bi_class), color = "white", size = 0.1, show.legend = FALSE) + 
  bi_scale_fill(pal = fsm_vs_relpov_pal, dim = 3) +
  theme_void()

fsm_vs_incdep_legend <- bi_legend(
    pal = fsm_vs_relpov_pal,
    dim = 3,
    xlab = "",#"\n\nHigher % of children \naged 0 to 15 \nin income deprivation",
    ylab = "",#"Higher % of children \naged 5 to 15 eligible for \nfree school meals",
    size = 12
  )

fsm_vs_incdep_legend_plot <- ggdraw() +
  draw_plot(fsm_vs_incdep_legend, 0.05, 0.05, 0.9, 0.9) + 
  draw_text("Red areas may be particularly \nbadly served by current policy.",
    0.85, 0.38, hjust=0, size=6.5, colour = '#ad394e'
    ) +
  # x-axis
  draw_text("Higher % of children \naged 0 to 15 \nin income deprivation",
    0.58, -0.05, hjust=0.5, vjust=0, size=7
  ) +
  # y-axis
  draw_text("Higher % of children \naged 5 to 15 eligible for \nfree school meals",
    0, 0.5, hjust=0.5, vjust=0.5, size=7, angle=90
  )

fsm_vs_incdep_plot <- ggdraw() +
  draw_text(
    "Eligibility for Free School Meals in 2020 vs\nWIMD 2019 Income Deprivation indicator"
  , 0.5, 0.98, size=14, hjust=0.5, vjust=1, family="Bahnschrift") +

  draw_plot(fsm_vs_incdep_map, 0.005, 0, 0.95, 0.95) +
  draw_plot(fsm_vs_incdep_legend_plot, 0.05, 0.38, 0.28, 0.28) + 
  draw_text(
    "‘Income deprivation’ is a WIMD 2019 indicator based on
income-related benefit claimants, Tax Credit recipients,
supported asylum seekers, and people on Universal Credit.
Source: statswales.gov.wales"
  , 0.05, 0.06, size=6.5, hjust=0, colour="#777777")

save_plot("outputs/fsm_vs_incdep.png", fsm_vs_incdep_plot, base_width=6.5, base_height=7.1)

