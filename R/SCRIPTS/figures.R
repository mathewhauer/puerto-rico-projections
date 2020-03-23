set.seed(100)

# source('R/SCRIPTS/000-Libraries.R')      # loading in the libraries

K05_pop <- read_csv("R/DATA-RAW/pr2000_2017.csv") %>%
  dplyr::select(A = POPULATION, everything()) %>%
  mutate(B = A,
         C = A) %>%
  filter(!SEX == 0)



pr_projections2017 <- read_csv("R/PROJECTIONS/PR2017_2100.csv") %>%
  dplyr::select(-COUNTYRACE, -Var1, -TYPE, -GEOID) %>%
  mutate(NAME = "Puerto Rico")

pr_projections2015 <- read_csv("R/PROJECTIONS/PR2015_2100.csv") %>%
  dplyr::select(-COUNTYRACE, -Var1, -TYPE, -GEOID) %>%
  mutate(NAME = "Puerto Rico")

pr_projections20152 <- read_csv("R/PROJECTIONS/PR2015_2100plus.csv") %>%
  dplyr::select(-COUNTYRACE, -Var1, -TYPE, -GEOID) %>%
  mutate(NAME = "Puerto Rico")

combined2017 <- rbind(K05_pop, pr_projections2017) %>%
  mutate(LaunchYear = "2017")

combined2015 <- rbind(filter(K05_pop, YEAR <= 2015), pr_projections2015) %>%
  mutate(LaunchYear = "2015")

combined20152 <- rbind(filter(K05_pop, YEAR <= 2015), pr_projections20152) %>%
  mutate(LaunchYear = "2015_TFR")


combined <- rbind(combined2015, combined2017) %>%
  rbind(., combined20152) %>%
  # filter(LaunchYear != "2015_TFR") 
  # %>%
  mutate(LaunchYear = case_when(
    LaunchYear == "2015_TFR" ~ "2015-TFR+",
    TRUE ~ as.character(LaunchYear)
  ))
# 

combined2 <- combined %>%
  dplyr::select(STATE:SEX, LaunchYear, Mean = A, Lower80 = B, Upper80 = C) %>%
  filter(LaunchYear != "2015")
# 
# write_csv(combined2, "R/PROJECTIONS/projections_combined.csv")

totals <- combined %>%
  group_by(YEAR, LaunchYear) %>%
  dplyr::summarise(A = sum(A),
                   B = sum(B),
                   C = sum(C))

agegroups <- combined %>%
  mutate(agegroup = case_when(
    AGE <= 3 ~ "Young, (0-14)",
    AGE >= 14 ~ "Old, (65+)",
    TRUE ~ "Working Age, (15-64)"
  ))  %>%
  group_by(YEAR, LaunchYear,agegroup) %>%
  dplyr::summarise(A = sum(A),
                   B = sum(B),
                   C = sum(C))

# Making Figures
prprojected_total <- ggplot(data = totals, aes(x = YEAR, y = A, color = LaunchYear, fill = LaunchYear, linetype = LaunchYear)) +
  geom_ribbon(aes(ymin = B, ymax=C), alpha = 0.2, colour = NA) +
  geom_line(size = 1.5) +
  # ylim(0, max(totals$C)*1.1) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(2000, 2048),
                     breaks = c(2000,2010, 2020, 2030, 2040, 2047)) +
  scale_y_continuous(limits = c(0, max(totals$C)*1.1),
                     labels = scales::comma,
                     expand = c(0,0)) +
  theme_bw() +
  labs(x = "Year",
       y = "Population",
       title = "Projected Population Total for Puerto Rico, 2017-2047",
       caption = "Uncertainty is the 80th percentile prediction interval")

ggsave(file = "R/FIGURES/prprojected_total-TFR.png", plot = prprojected_total)

prprojected_ages <- 
  ggplot(data = filter(agegroups, LaunchYear %in% c("2015", "2017")), aes(x = YEAR, y = A, color = agegroup, fill = agegroup, linetype = LaunchYear)) +
  geom_ribbon(aes(ymin = B, ymax=C), alpha = 0.2, color = NA) +
  geom_line(size =1.5) +
  # ylim(0, max(totals$C)*1.1) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(2000, 2048),
                     breaks = c(2000,2010, 2020, 2030, 2040, 2047)) +
  scale_y_continuous(limits = c(0, max(agegroups$C)*1.1),
                     labels = scales::comma,
                     expand = c(0,0)) +
  theme_bw() +
  labs(x = "Year",
       y = "Population",
       title = "Projected Population for select Age Groups for Puerto Rico, 2017-2047",
       caption = "Uncertainty is the 80th percentile prediction interval")

ggsave(file = "R/FIGURES/prprojected_ages2.png", plot = prprojected_ages)
