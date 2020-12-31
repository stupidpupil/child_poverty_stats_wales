library(tidyverse)
library(assertthat)

crp_by_constituency <- readxl::read_xlsx(
  "data/Local-child-poverty-estimates-ahc-october-2020_revised.xlsx", 
  sheet = "Parliamentary Constituency",
  range = "A3:H652",
  col_types = c("skip","skip","text","skip","skip","skip","skip","numeric"),
  col_names = c("ConstituencyCodeW07", "ChildrenUnder16InRelativePovertyAfterHousingCosts")
  ) %>%
  filter(ConstituencyCodeW07 %>% str_detect("^W07")) %>%
  mutate(ChildrenUnder16InRelativePovertyAfterHousingCosts = round(ChildrenUnder16InRelativePovertyAfterHousingCosts))


assert_that(
  crp_by_constituency %>% pull(ConstituencyCodeW07) %>% n_distinct
  == 40
)

assert_that(
  crp_by_constituency %>% pull(ChildrenUnder16InRelativePovertyAfterHousingCosts) %>% sum
  %in% 150000:158000 # Country & Region sheet says 157601, but the constituency one doesn't match
)