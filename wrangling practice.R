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


anti_join_example <- anti_join(animals, sites)


# Practice with lubridate
my_date <- "03-13-1998"
lubridate::mdy(my_date)

# New format for date
my_date <- "08-jun-1974"
lubridate::dmy(my_date)

# Another format
my_date <- "19160518"
lubridate::ymd(my_date)

# What happens if we give a date that doesn't make sense

lubridate::mdy("1942-08-31")
lubridate::dmy("09/12/84")

# working with date=times

time <- "2020-08-12 11:18"
time <- ymd_hm (time, tz = "America/Los_Angeles")

# Convert to PDT
with_tz(time, "America/Los_Angeles")

# Extract info from dates
week(time)
year(time)
day(time)

# Practice lubridate within a dataframe

urchin_counts <- tribble(
  ~date, ~species, ~size_mm,
  "10/3/2020", "purple", 55,
  "10/4/2020", "red", 48,
  "11/17/2020", "red", 67
)

urchin_counts %>% 
  mutate(date = lubridate::mdy(date)) %>%
  mutate(year = year(date),
month = month(date),
day = day(date))


day_1 <- lubridate::ymd("2020-01-06")
day_2 <- lubridate::ymd("2020-05-18")
day_3 <- lubridate::ymd("2020-05-19")

# Create a time interval
time_interval <- interval(day_1, day_2)

# Check the length in weeks
time_length(time_interval, "week")
time_length(time_interval, "year")

# Practice with stringr

# str_detect() to detect string patterns
# Return TRUE/FALSE sepending on whether the patter is detected

my_string <- "Teddy loves eating salmon and socks."

# Does the pattern "love" exist within the string?
my_string %>% str_detect("love")

# Does the pattern "pup" exist within the string?
my_string %>% str_detect("pup")

my_string <- c("burrito", "fish taco", "Taco salad")

# Does the vector element contain the pattern "fish"?
my_string %>% str_detect("fish")

# powerful when combined with dplyr function

skywalkers <- starwars %>% 
  filter(str_detect(name, "Skywalker"))

skywalkers

firewalkers <-starwars %>% 
  mutate(name = str_replace(name, patter = "Sky", replacement = "Fire"))

firewalkers


# Cleaning up white space
feedback <- c("  I ate   some nachos", "wednesday   morning  ")

# remove the leading, trailing, and duplicate spaces
str_squish(feedback)

# remove just the leading and trailing spaces
str_trim(feedback)

# convert cases
str_to_lower(feedback)
str_to_upper(feedback)
str_to_sentence(feedback)
str_to_title(feedback)


# Count the number of matches on a string

str_count(feedback, pattern = "nachos")
