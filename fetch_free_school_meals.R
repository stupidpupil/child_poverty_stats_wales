library(statswalesr)
library(tidyverse)
library(assertthat)

fsm_by_school_and_constituency <- statswales_get_dataset("SCHS0289")

# SCHS0288 is all pupils, SCHS0289 is 5-15s

fsm_by_constituency_and_school_type <- fsm_by_school_and_constituency %>%
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
    CountEligibleForFreeSchoolMeals5to15 = sum(Data)
  )

#
# Some very rough checks
#

assert_that(
  fsm_by_constituency_and_school_type %>% pull(CountEligibleForFreeSchoolMeals5to15) %>% sum
  %in% 74000:76000
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
  == 4 #No nurseries
)

#
# Pivot wider
#

fsm_by_constituency <- fsm_by_constituency_and_school_type %>%
  mutate(
    SchoolType = paste0("CountEligibleForFreeSchoolMeals5to15.", SchoolType %>% str_to_title() %>% str_replace_all("\\s+", ""))
  ) %>%
  pivot_wider(
    names_from = SchoolType,
    values_from = CountEligibleForFreeSchoolMeals5to15
  ) %>%
  replace_na(list(
    CountEligibleForFreeSchoolMeals5to15.PrimarySchools = 0,
    CountEligibleForFreeSchoolMeals5to15.SecondarySchools = 0,
    CountEligibleForFreeSchoolMeals5to15.SpecialSchools = 0,
    CountEligibleForFreeSchoolMeals5to15.MiddleSchools = 0
  )) %>%
  mutate(
    CountEligibleForFreeSchoolMeals5to15.Total = sum(
      CountEligibleForFreeSchoolMeals5to15.PrimarySchools,
      CountEligibleForFreeSchoolMeals5to15.SecondarySchools,
      CountEligibleForFreeSchoolMeals5to15.SpecialSchools,
      CountEligibleForFreeSchoolMeals5to15.MiddleSchools
    )
  )













