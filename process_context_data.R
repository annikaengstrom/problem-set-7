library(tidyverse)
library(fs)
library(readxl)
library(stringr)
library(DataExplorer)
library(janitor)
library(lme4)
library(moderndive)
library(numform)

## CONTEXT DATA
# Download the raw data
download.file(url = "https://raw.githubusercontent.com/MEDSL/2018-elections/master/election-context-2018.csv", destfile = "election-context-2018.csv", quiet = TRUE, mode = "wb")

# Read in the raw data and select only variables of interest
df <- read_csv("election-context-2018.csv") %>% 
  select(state, county, fips, total_population, white_pct, black_pct, hispanic_pct, female_pct, age65andolder_pct, lesscollege_pct) %>% 

# Manipulate the fips variable 
  mutate(state_code = case_when(
    nchar(fips) == 4 ~ str_extract(fips, "\\d"),
    nchar(fips) > 4 ~ str_extract(fips, "\\d\\d")
  )) %>% 
  mutate(county_code = str_extract(fips, "\\d\\d\\d$")) %>% 
  mutate(county_code = as.double(f_num(county_code))) %>% 

# Make a grouping variable
  mutate(state_co = paste0(state_code, "-", county_code)) %>% 
  select(-state_code, -county_code)

# Delete the file
file_delete("election-context-2018.csv")

## DISTRICT DATA
# Read in the congressional district by county data
download.file(url = "https://www2.census.gov/geo/relfiles/cdsld16/natl/natl_cocd_delim.txt", destfile = "districts.txt")

# Read in the raw txt
dist <- read_delim("districts.txt", delim = ",", escape_backslash = TRUE, skip = 1) %>% 
  clean_names() %>% 
  mutate(state_code = as.double(state), county_code = as.double(county), district = as.double(congressional_district)) %>% 
  select(-congressional_district, -state, -county) %>% 

# Make a grouping variable
  mutate(state_co = paste0(state_code, "-", county_code)) %>% 
  select(-state_code, -county_code)


## JOINED DATA
# Get state name abbreviations
download.file("http://www.fonz.net/blog/wp-content/uploads/2008/04/states.csv", "states.csv")
states <- read_csv("states.csv") %>% 
  clean_names()

# Join the data
all <- df %>% 
  left_join(dist, by = "state_co") %>% 

# Fix the state names
  left_join(states, by = "state") %>% 
  mutate(state = abbreviation) %>% 
  select(-county, -fips, -state_co, -abbreviation) %>% 

# Make voting district names
  mutate(race = case_when(
    nchar(district) == 1 ~ paste0(state, "-", "0", district),
    TRUE ~ paste0(state, "-", district))) %>% 
  select(-state, -district) %>% 

# Recode the percentage variables
  mutate(white_pct = white_pct / 100,
         black_pct = black_pct / 100,
         hispanic_pct = hispanic_pct / 100,
         female_pct = female_pct / 100,
         age65andolder_pct = age65andolder_pct / 100,
         lesscollege_pct = lesscollege_pct / 100,) %>% 
  
## Get weighted percentages for each voting district for each variable
  group_by(race) %>% 
  mutate(dist_pop = sum(total_population)) %>% 
# white pct
  mutate(white_wpct = total_population * white_pct / dist_pop) %>% 
  mutate(white_pct = sum(white_wpct)) %>%
  select(-white_wpct) %>% 
# black pct
  mutate(black_wpct = total_population * black_pct / dist_pop) %>% 
  mutate(black_pct = sum(black_wpct)) %>%
  select(-black_wpct) %>% 
# hispanic pct
  mutate(hispanic_wpct = total_population * hispanic_pct / dist_pop) %>% 
  mutate(hispanic_pct = sum(hispanic_wpct)) %>%
  select(-hispanic_wpct) %>% 
# female pct
  mutate(female_wpct = total_population * female_pct / dist_pop) %>% 
  mutate(female_pct = sum(female_wpct)) %>%
  select(-female_wpct) %>%
# age65 pct
  mutate(age65andolder_wpct = total_population * age65andolder_pct / dist_pop) %>% 
  mutate(age65andolder_pct = sum(age65andolder_wpct)) %>%
  select(-age65andolder_wpct) %>%
# lesscollege pct
  mutate(lesscollege_wpct = total_population * lesscollege_pct / dist_pop) %>% 
  mutate(lesscollege_pct = sum(lesscollege_wpct)) %>%
  select(-lesscollege_wpct)

## Make a final demographics df for export
# Save only unique lists
demog <- tibble(race = unique(all$race)) %>% 
  inner_join(all, by = "race")

demog <- demog[!duplicated(demog$race), ]

# Save the file
write_csv(demog, "demog.csv")

# Delete the downloaded files
file_delete(c("election-context-2018.csv", "districts.txt", "states.csv"))
