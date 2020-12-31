library(statswalesr)
library(tidyverse)
library(assertthat)

incdep_by_constituency <- statswales_get_dataset("WIMD1924") %>%
  filter(
    AgeGroup_Code %in% c('0015'),
    Area_AltCode1 %>% str_detect("^W09")
    ) %>%
  select(
    ConstituencyCode = Area_AltCode1,
    PropIncDepUnder16 = Data) %>%
  mutate(
    PropIncDepUnder16 = as.numeric(PropIncDepUnder16)/100
  )



#
# Some very rough checks
#

assert_that(
  incdep_by_constituency %>% pull(ConstituencyCode) %>% n_distinct
  == 40
)