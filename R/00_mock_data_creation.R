####################################################
###############       Mock data    #################
####################################################

library(tidyverse)
library(lubridate)
library(here)

source(here("functions", "theme_pepe.R"))


# Site data ---------------------------------------------------------------

sites <- tibble(site_id = paste("site_", c("a", "b", "c", "d", "e", "f", "g", "h", "i"), sep = ""),
                altitude = round(rnorm(9, 450, 20)),
                latitude = round(rnorm(9, 49.904955, 0.005), digits = 6),
                longitude = round(rnorm(9, -94.605126, 0.5), digits = 6),
                treatment = rep(c("Low", "Medium", "High"), each = 3)) 

sites %>% 
  write_csv(path = here("data", "sites.csv"))


# Sample logbook ----------------------------------------------------------

sample_logbook <- tibble(sample_id = paste("TR_", (seq(1, 108) %>% str_pad(., 3, side = "left", pad = "0")), sep = ""),
                         site_id = paste("site_", rep(c("a", "b", "c", "d", "e", "f", "g", "h", "i"), 12), sep = ""),
                         day = round(rnorm(108, 15, 5)),
                         month = rep(seq(1,12), each = 9), 
                         year = rep(2018, 108),
                         hh = round(rnorm(108, 12, 2)),
                         mm = round(rnorm(108, 30, 5)),
                         collected_by = sample(c("JLR", "JS", "TB"), 108, replace = TRUE))

sample_logbook %>% 
  write_csv(path = here("data", "sample_logbook.csv"))

# Laboratory data ---------------------------------------------------------

lab_data <- tibble(sample_id = paste("TR_", (seq(1, 108) %>% str_pad(., 3, side = "left", pad = "0")), sep = ""),
                   submission_date = paste(27, rep(seq(1,12), each = 9), 2018, sep = "/"),
                   analysis_date_time = paste(paste(rep(paste(30, c(3,6, 9, 12), sep = "/"), each = 27), 2018, sep = "/"), rep("15:30"), sep = " "),
                   compound_1 = rnorm(108, rep(c(9, 8, 9, 12, 17, 20, 25, 27, 18, 17, 14, 10), each = 9), 0.6),
                   compound_2 = rnorm(108, c(rep(c(9, 8, 9, 12, 17, 20, 25, 27, 18, 17, 14, 10), each = 3),
                                             15,
                                             rep(c(32, 30, 28, 25, 22, 20, 16, 13, 14, 15, 24, 28), each = 3)), 0.6),
                   compound_3 =  rnorm(108, rep(c(19, 18, 19, 22, 27, 30, 35, 37, 28, 27, 24, 20), each = 9), 0.4),
                   compound_4 = rnorm(108, 23.7, 3),
                   compound_5 = rnorm(108, rep(c(32, 30, 28, 25, 22, 20, 16, 13, 14, 15, 24, 28), each = 9), 0.8)) %>% 
  pivot_longer(cols = -c(sample_id, submission_date, analysis_date_time), 
               names_to = "compound", 
               values_to = "concentration") %>% 
  mutate(submission_date = dmy(submission_date)) %>% 
  left_join(sample_logbook) %>% 
  left_join(sites) %>% 
  group_by(treatment, compound) %>% 
  mutate(concentration = case_when(
    treatment == "High" ~ concentration * rnorm(1, 0.2, 0.1),
    treatment == "Medium" ~ concentration * rnorm(1, 0.5, 0.1),
    TRUE ~ concentration
  )) 
  
  
# quick check of how it looks

lab_data %>% 
  ggplot() +
  facet_grid(rows = vars(compound), cols = vars(treatment)) +
  geom_point(aes(x = submission_date, y = concentration, colour = compound)) +
  geom_smooth(aes(x = submission_date, y = concentration, colour = compound)) +
  theme_pepe()

# save each analytical batch separately


lab_data %>% 
  ungroup() %>% 
  select(sample_id, submission_date, analysis_date_time, compound, concentration) %>% 
  pivot_wider(names_from = compound, values_from = concentration) %>%
  mutate(batch = case_when(
    analysis_date_time == "30/3/2018 15:30" ~ "batch_1",
    analysis_date_time == "30/6/2018 15:30" ~ "batch_2",
    analysis_date_time == "30/9/2018 15:30" ~ "batch_3",
    analysis_date_time == "30/12/2018 15:30" ~ "batch_4"
  )) %>%
  group_by(batch) %>% 
  nest() %>% 
  pwalk(~write_csv(x = .y, path = paste0("data/", .x, ".csv") ) )




