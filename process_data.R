library(tidyverse)
library(fs)
library(readxl)
library(stringr)

### In this section, clean up the actual results data
# Read in the data
raw <- read_csv("mt_2_results.csv")

# Get rid of the senate and governor races
df <- raw %>%
  filter(district != "sen", district != "gov")

# Make a new variable for race
df <- df %>%
  filter(! district == "AL") %>% 
  mutate(race = paste0(state, "-", district)) %>% 
  select(-state, -district)

# Calculate vote percentages
df <- df %>% 
  mutate(total = dem_votes + rep_votes + other_votes) %>% 
  mutate(d_percent = dem_votes/total,
         r_percent = rep_votes/total,
         o_percent = other_votes/total) %>% 
  select(-dem_votes, -rep_votes, -other_votes, -total)


### In this section, clean up the poll data
# Download the file
download.file(url = "https://goo.gl/ZRCBda", destfile = "master.zip", quiet = TRUE, mode = "wb")
unzip("master.zip")
file_delete(c("master.zip", "2018-live-poll-results-master"))

# Make a list of the files
file_names <- dir_ls("2018-live-poll-results-master/data/")
# Read the file names in a big tibble
x <- map_dfr(file_names, read_csv, .id = "source")

# Save the Upshot data for just House races in wave 3 and select only variables of interest
upshot <- x %>% 
  filter(! str_detect(source, "sen"), ! str_detect(source, "gov")) %>% 
  filter(str_detect(source, "-3.")) %>% 
  select(source, response, educ, gender, file_race, age_combined, partyid, region, turnout_score, final_weight, likely)

# Make a new variable for the race name
upshot <- upshot %>% 
  mutate(race = str_extract(source, pattern = "[a-z][a-z]\\d\\d")) %>% 
  mutate(race = paste0(toupper(str_extract(race, pattern = "[a-z][a-z]")), "-", str_extract(race, pattern = "\\d\\d")))

### VOTES
# Find vote percentages
upshot_votes <- upshot %>% 
  group_by(race, response) %>% 
  tally(wt = final_weight)

upshot_votes <- upshot_votes %>% 
  group_by(race) %>% 
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  group_by(race, response) %>% 
  mutate(percent = n / total)

# Drop responses that aren't D, R, or U, and other unimportant variables
upshot_votes <- upshot_votes %>% 
  filter(response %in% c("Dem", "Rep", "Und")) %>% 
  select(-n, -total)

# Spread with new variables to match 
upshot_votes_final <- upshot_votes %>% 
  group_by(race, response) %>% 
  spread(response, percent) %>% 
  mutate(d_percent = Dem,
         r_percent = Rep,
         o_percent = Und) %>% 
  select(-Dem, -Rep, -Und)

# Join the Upshot poll data to the actual data, .a for actual and .p for poll
all <- df %>% 
  inner_join(upshot_votes_final, by = "race", suffix = c(".a", ".p"))

### GENDER
# Find gender percentages
upshot_gender <- upshot %>% 
  filter(! gender == "[DO NOT READ] Don't know/Refused") %>% 
  group_by(race, gender) %>% 
  tally(wt = final_weight)

upshot_gender <- upshot_gender %>% 
  group_by(race) %>% 
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  group_by(race, gender) %>% 
  mutate(percent = n / total) %>% 
  select(-total, -n)

# Spread with new variables to match 
upshot_gender_final <- upshot_gender %>% 
  group_by(race, gender) %>% 
  spread(gender, percent)

# Join the Upshot poll data to the actual data, .a for actual and .p for poll
all <- all %>% 
  inner_join(upshot_gender_final, by = "race", suffix = c(".a", ".p"))

### DO THE SAME FOR educ, race, likely, age
