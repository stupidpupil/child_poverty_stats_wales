library(tidyverse)

source("fetch_free_school_meals.R")
source("fetch_pupils.R")
source("extract_children_in_relative_poverty.R")

constituencies <- read_csv("data/constituencies.csv") %>%
  left_join(crp_by_constituency, by='ConstituencyCodeW07') %>%
  left_join(fsm_by_constituency, by='ConstituencyCode') %>%
  left_join(pupils_by_constituency, by='ConstituencyCode') %>%
  mutate(
    PropEligibleForFreeSchoolMeals5to15 = 
    CountEligibleForFreeSchoolMeals5to15.Total/CountAllPupils5to15.Total
  )

library(sf)
library(biscale)
library(ggplot2)
library(cowplot)

constituency_boundaries <- st_read("data/constituency_boundaries.geojson") %>%
  rename(ConstituencyCode = nawc16cd) %>%
  left_join(
    constituencies %>% 
      bi_class(
        x=PropChildrenUnder16InRelativePovertyAfterHousingCosts, 
        y=PropEligibleForFreeSchoolMeals5to15)
    , 
    by='ConstituencyCode'
  )

fsm_vs_relpov_pal = "DkViolet"

fsm_vs_relpov_map <- ggplot() + 
  geom_sf(constituency_boundaries, 
    mapping = aes(fill=bi_class), color = "white", size = 0.1, show.legend = FALSE) + 
  bi_scale_fill(pal = fsm_vs_relpov_pal, dim = 3) +
  theme_void()

fsm_vs_relpov_legend <- bi_legend(
    pal = fsm_vs_relpov_pal,
    dim = 3,
    xlab = "Higher % of children \nin relative poverty\nafter housing costs",
    ylab = "Higher % of children \neligible for \nfree school meals",
    size = 7
  )

fsm_vs_relpov_plot <- ggdraw() +
  draw_plot(fsm_vs_relpov_map, 0, 0, 1, 1) +
  draw_plot(fsm_vs_relpov_legend, 0.05, .38, 0.28, 0.28)