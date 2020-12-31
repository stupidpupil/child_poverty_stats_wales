library(tidyverse)

source("fetch_free_school_meals.R")
source("extract_children_in_relative_poverty.R")

constituencies <- read_csv("data/constituencies.csv") %>%
  left_join(crp_by_constituency, by='ConstituencyCodeW07') %>%
  left_join(fsm_by_constituency, by='ConstituencyCode')