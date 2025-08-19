## Cleaning my environment
rm(list = ls())

# Attach packages
library(tidyverse)
library(palmerpenguins)
library(lubridate)

# Data wrangling refresher

# Only include penguins at Biscoe and Dream islands
# Remove the year and sex variables
# Add a new column called body_mass_kg, with penguin mass # converted from grams to kilograms
# Rename the island variable to location


ex1 <- penguins %>% 
  filter(island %in% c("Biscoe", "Dream")) %>% 
  select(-year, -sex) %>% 
  mutate("body_mass_kg" = body_mass_g / 1000) %>% 
  rename(location = island)


# Limit to only Adelie penguins
#Remove any observations where flipper_length_mm is NA (hint: !is.na())
#Group the data by sex
# Find the mean (mean()), standard deviation (sd) and sample size (n) of flipper_length_mm for male and female Adelie penguins, returned in a nice summary table

exp2 <- penguins %>% 
  filter(species == "Adelie") %>% 
  filter(!is.na(flipper_length_mm),
         !is.na(sex)) %>%  # remove rows that are not NA
  group_by(sex) %>% 
  summarise(mean = mean(flipper_length_mm),
            standard_dev = sd(flipper_length_mm),
            sample_size = n())

data.frame(
  stringsAsFactors = FALSE,
          location = c("beach", "lagoon", "bluff", "oaks"),
    full_site_name = c("Goleta Beach","UCSB Lagoon",
                       "Ellwood Mesa","Fremont Campground"),
      jurisdiction = c("SB City", "UCSB", "SB City", "USFS")
)
sites <- data.frame(
                        stringsAsFactors = FALSE,
                             check.names = FALSE,
  `location	full_site_name	jurisdiction` = c("beach\tGoleta Beach\tSB City",
                                             "lagoon\tUCSB Lagoon\tUCSB","bluff\tEllwood Mesa\tSB City",
                                             "oaks\tFremont Campground\tUSFS",
                                             "Ā")
)


animals <- data.table::data.table(
    location = c("lagoon", "bluff", "creek", "oaks", "bluff"),
     species = c("bobcat", "coyote", "fox", "squirrel", "bobcat"),
    maturity = c("adult", "juvenile", "adult", "juvenile", "adult")
)


sites <- data.table::data.table(
        location = c("beach", "lagoon", "bluff", "oaks"),
  full_site_name = c("Goleta Beach","UCSB Lagoon",
                     "Ellwood Mesa","Fremont Campground"),
    jurisdiction = c("SB City", "UCSB", "SB City", "USFS")
)

#Practice with a full join
# Keeps all rows and adds all columns

full_join(animals, sites)


# left_join()
left_join(animals, sites)

# right_join()

right_join(animals, sites)

# inner_join()
inner_join(animals, sites)

#Filtering joins
semi_join_example <- semi_join(animals, sites)


semi_join_example

anti_join_example <- anti_join(animals, sites)
