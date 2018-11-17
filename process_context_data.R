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
  select(-state, -district)
  

