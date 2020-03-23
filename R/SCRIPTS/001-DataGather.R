# This file might be out of date due to changes in the CensusAPI and TidyCensus R packages.

pr_20102017 <- get_estimates(geography = "state", 
                               product = "characteristics", 
                               breakdown = c("AGEGROUP", "SEX"),
                               state = 72,
                               time_series = TRUE) %>%
  filter(DATE >=3,
         AGEGROUP<=18) %>%
  mutate(YEAR = DATE + 2007,
         STATE = GEOID,
         POPULATION = value) %>% # converting DATE format (1-10) to the actual year.
  dplyr::select(STATE, NAME, POPULATION, YEAR, AGEGROUP, SEX)

# In the vintage 2000-2010 data,
# Date == 1 refers to April 1 2000
# Date == 2 refers to July 1 2000
# Date == 12 refers to April 1 2010
# AGEGROUPS are 0 = TOTAL, 1-18 = 0-4...85+
pr_20002010 <- getCensus(name = "pep/int_charagegroups",
          vars = c("STATE", "POP", "DATE", "AGEGROUP", "SEX"), 
          # region = "state:*", 
          region = "state:72",
          key = "0206e3f2924a424be8722887fd0a49cea6308a7e",
          vintage = 2000) %>%
  mutate(DATE = as.numeric(DATE),
         AGEGROUP = as.numeric(AGEGROUP),
         POPULATION = as.numeric(POP),
         YEAR = DATE + 1997,
         NAME = "Puerto Rico") %>%
  filter(between(DATE, 3 , 11),
         AGEGROUP <=18) %>%
  dplyr::select(STATE, NAME, POPULATION, YEAR, AGEGROUP, SEX)

dat <- rbind(pr_20002010, pr_20102017)

agesex <- filter(dat, AGEGROUP >0) %>%
  mutate(RACE = 0,
         COUNTY = "000") %>%
  dplyr::select(everything(),AGE = AGEGROUP )

write_csv(agesex, "R/DATA-RAW/pr2000_2017.csv")  