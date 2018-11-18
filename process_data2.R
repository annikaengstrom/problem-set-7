library(tidyverse)
library(fs)
library(readxl)
library(stringr)
library(DataExplorer)
library(janitor)
library(lme4)
library(moderndive)

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

# Make a list of the files
file_names <- dir_ls("2018-live-poll-results-master/data/")
# Read the file names in a big tibble
x <- map_dfr(file_names, read_csv, .id = "source")

# Delete the files
file_delete(c("master.zip", "2018-live-poll-results-master"))

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

## EDUC
# Find educ percentages
upshot_educ <- upshot %>% 
  filter(! educ == "[DO NOT READ] Refused") %>% 
  filter(! educ == "[DO NOT READ] Don't know/Refused") %>% 
  group_by(race, educ) %>% 
  tally(wt = final_weight)

upshot_educ <- upshot_educ %>% 
  group_by(race) %>% 
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  group_by(race, educ) %>% 
  mutate(percent = n / total) %>% 
  select(-total, -n)

# Spread with new variables to match 
upshot_educ_final <- upshot_educ %>% 
  group_by(race, educ) %>% 
  spread(educ, percent)

# Convert NAs to 0s
upshot_educ_final[is.na(upshot_educ_final)] <- 0

# Join the Upshot poll data to the actual data, .a for actual and .p for poll
all <- all %>% 
  inner_join(upshot_educ_final, by = "race", suffix = c(".a", ".p"))

### FILE_RACE
# Find file_race percentages
upshot_race <- upshot %>% 
  filter(! file_race == "Unknown") %>% 
  group_by(race, file_race) %>% 
  tally(wt = final_weight)

upshot_race <- upshot_race %>% 
  group_by(race) %>% 
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  group_by(race, file_race) %>% 
  mutate(percent = n / total) %>% 
  select(-total, -n)

# Spread with new variables to match 
upshot_race_final <- upshot_race %>% 
  group_by(race, file_race) %>% 
  spread(file_race, percent)

# Convert NAs to 0s
upshot_race_final[is.na(upshot_educ_final)] <- 0

# Join the Upshot poll data to the actual data, .a for actual and .p for poll
all <- all %>% 
  inner_join(upshot_race_final, by = "race", suffix = c(".a", ".p"))

### LIKELY
# Find likely percentages
upshot_likely <- upshot %>% 
  filter(! likely == "[DO NOT READ] Don't know/Refused") %>% 
  group_by(race, likely) %>% 
  tally(wt = final_weight)

upshot_likely <- upshot_likely %>% 
  group_by(race) %>% 
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  group_by(race, likely) %>% 
  mutate(percent = n / total) %>% 
  select(-total, -n)

# Spread with new variables to match 
upshot_likely_final <- upshot_likely %>% 
  group_by(race, likely) %>% 
  spread(likely, percent)

# Convert NAs to 0s
upshot_likely_final[is.na(upshot_likely_final)] <- 0

# Join the Upshot poll data to the actual data, .a for actual and .p for poll
all <- all %>% 
  inner_join(upshot_likely_final, by = "race", suffix = c(".a", ".p"))

### AGE_COMBINED
# Find age_combined percentages
upshot_age <- upshot %>% 
  filter(! age_combined == "[DO NOT READ] Don't know/Refused") %>% 
  group_by(race, age_combined) %>% 
  tally(wt = final_weight)

upshot_age <- upshot_age %>% 
  group_by(race) %>% 
  mutate(total = sum(n)) %>% 
  ungroup() %>% 
  group_by(race, age_combined) %>% 
  mutate(percent = n / total) %>% 
  select(-total, -n)

# Spread with new variables to match 
upshot_age_final <- upshot_age %>% 
  group_by(race, age_combined) %>% 
  spread(age_combined, percent)

# Convert NAs to 0s
upshot_age_final[is.na(upshot_age_final)] <- 0

# Join the Upshot poll data to the actual data, .a for actual and .p for poll
all <- all %>% 
  inner_join(upshot_age_final, by = "race", suffix = c(".a", ".p"))


### Work with all of the data

# Fix any of the NAs to 0
all[is.na(all)] <- 0

# Get the variables of interest, and drop the undecided races
df2 <- all %>%
  filter(! win_party == "UNDECIDED") %>% 
  select(-o_percent.a, -o_percent.p) %>% 
  
# Make new variables that represent the polling accuracy
  mutate(win_party.a = win_party,
         win_party.p = case_when(
           d_percent.p > r_percent.p ~ "D",
           r_percent.p > d_percent.p ~ "R")) %>%
  mutate(error = abs(r_percent.a - r_percent.p) + abs(d_percent.a - d_percent.p)) %>% 
  mutate(Prediction = case_when(
    win_party.a == win_party.p ~ "Correct",
    win_party.a != win_party.p ~ "Incorrect")) %>% 
  select(-win_name) %>% 

# Add together the education variables
  mutate(lesscollege = `Grade school` + `High school` + `Some college or trade school`) %>% 
  select(win_party, race, d_percent.a, r_percent.a, d_percent.p, r_percent.p,
         Female, White, Hispanic, Black, `65 and older`, lesscollege, Prediction)

# Read in the demographic data and join to df2
demog <- read_csv("demog.csv")
final <- demog %>%
  inner_join(df2, by = "race") %>% 
  
# Find the absolute error amounts
  mutate(
    white_error = abs(white_pct - White),
    black_error = abs(black_pct - Black),
    hispanic_error = abs(hispanic_pct - Hispanic),
    age_error = abs(age65andolder_pct - `65 and older`),
    gender_error = abs(female_pct - Female),
    edu_error = abs(lesscollege_pct - lesscollege),
    d_error = abs(d_percent.a - d_percent.p),
    r_error = abs(r_percent.a - r_percent.p)) %>% 
  select(race, Prediction, d_error, r_error, white_error, black_error, hispanic_error,
         age_error, gender_error, edu_error, total_population)

# Write the data where R and the app can read it
write_rds(final, "upshot/data2.rds")
write_rds(final, "data2.rds")
