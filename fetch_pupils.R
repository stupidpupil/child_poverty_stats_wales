library(statswalesr)
library(tidyverse)
library(assertthat)

pupils_by_school_and_constituency <- statswales_get_dataset("SCHS0289")

pupils_by_constituency_and_school_type <- pupils_by_school_and_constituency %>%
  filter(
    Category_ItemName_ENG == "All pupils"
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
    CountAllPupils5to15 = sum(Data)
  )

#
# Some very rough checks
#

assert_that(
  pupils_by_constituency_and_school_type %>% pull(CountAllPupils5to15) %>% sum
  %in% 376000:378000
)

assert_that(
  pupils_by_constituency_and_school_type %>% pull(ConstituencyCode) %>% str_detect("^W09") %>% all
)

assert_that(
  pupils_by_constituency_and_school_type %>% pull(ConstituencyCode) %>% n_distinct
  == 40
)

assert_that(
  pupils_by_constituency_and_school_type %>% pull(SchoolType) %>% n_distinct
  == 4 #No nurseries
)

pupils_by_constituency <- pupils_by_constituency_and_school_type %>%
  mutate(
    SchoolType = paste0("CountAllPupils5to15.", SchoolType %>% str_to_title() %>% str_replace_all("\\s+", ""))
  ) %>%
  pivot_wider(
    names_from = SchoolType,
    values_from = CountAllPupils5to15
  ) %>%
  replace_na(list(
    CountAllPupils5to15.PrimarySchools = 0,
    CountAllPupils5to15.SecondarySchools = 0,
    CountAllPupils5to15.SpecialSchools = 0,
    CountAllPupils5to15.MiddleSchools = 0
  )) %>%
  mutate(
    CountAllPupils5to15.Total = sum(
      CountAllPupils5to15.PrimarySchools,
      CountAllPupils5to15.SecondarySchools,
      CountAllPupils5to15.SpecialSchools,
      CountAllPupils5to15.MiddleSchools
    )
  )



