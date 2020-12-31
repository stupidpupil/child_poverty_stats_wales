library(statswalesr)
library(tidyverse)
library(assertthat)

fsm_by_school_and_constituency <- statswales_get_dataset("SCHS0288")

fsm_by_constituency_and_school_type <- by_school_and_constituency %>%
  filter(
    Category_ItemName_ENG == "Eligible for free school meals"
  ) %>%
  select(
    Constituency_AltCode1,
    Sector_ItemName_ENG,
    Data) %>%
  group_by(
    ConstituencyCode = Constituency_AltCode1,
    SchoolType =Sector_ItemName_ENG,
  ) %>%
  filter(
    Data >= 0
  ) %>%
  summarise(
    CountEligibleForFreeSchoolMeals = sum(Data)
  )

#
# Some very rough checks
#

assert_that(
  fsm_by_constituency_and_school_type %>% pull(CountEligibleForFreeSchoolMeals) %>% sum
  %in% 85000:86000 # StatsWales gives 85,731
)

assert_that(
  fsm_by_constituency_and_school_type %>% pull(ConstituencyCode) %>% str_detect("^W09") %>% all
)

assert_that(
  fsm_by_constituency_and_school_type %>% pull(ConstituencyCode) %>% n_distinct
  == 40
)

assert_that(
  fsm_by_constituency_and_school_type %>% pull(SchoolType) %>% n_distinct
  == 5
)

#
# Pivot wider
#

fsm_by_constituency <- fsm_by_constituency_and_school_type %>%
  mutate(
    SchoolType = paste0("CountEligibleForFreeSchoolMeals.", SchoolType %>% str_to_title() %>% str_replace_all("\\s+", ""))
  ) %>%
  pivot_wider(
    names_from = SchoolType,
    values_from = CountEligibleForFreeSchoolMeals
  ) %>%
  replace_na(list(
    CountEligibleForFreeSchoolMeals.PrimarySchools = 0,
    CountEligibleForFreeSchoolMeals.SecondarySchools = 0,
    CountEligibleForFreeSchoolMeals.SpecialSchools = 0,
    CountEligibleForFreeSchoolMeals.MiddleSchools = 0,
    CountEligibleForFreeSchoolMeals.NurserySchools = 0
  )) %>%
  mutate(
    CountEligibleForFreeSchoolMeals.Total = sum(
      CountEligibleForFreeSchoolMeals.PrimarySchools,
      CountEligibleForFreeSchoolMeals.SecondarySchools,
      CountEligibleForFreeSchoolMeals.SpecialSchools,
      CountEligibleForFreeSchoolMeals.MiddleSchools,
      CountEligibleForFreeSchoolMeals.NurserySchools
    )
  )













