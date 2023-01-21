# Loading in Required Pacakges
library(here)
library(rvest)
library(janitor)
library(tidyverse)

# Reading in URL
url <- "https://en.wikipedia.org/wiki/2018_United_States_House_of_Representatives_election_ratings"

# Pulling datatable from url
data_22 <- url %>%
  read_html() %>%
  html_nodes('.wikitable') %>%
  html_table(fill = T) %>% 
  .[[1]] %>% 
  janitor::clean_names() %>% 
  mutate(year = 2018)

# Setting column names
names(data_22) <- c("district", "cpvi", "incumbent", "last_result", "cook", "IE",
                    "sabatos_crystal_ball", "real_clear", "538", "daily kos", "winner", "year")

# Cleaning the data
data_full <- data_22 %>% 
  filter(!district %in% c("District", "Overall")) %>% 
  mutate(last_result = case_when(
    incumbent == "New seat" ~ "",
    TRUE ~ last_result
  )) %>% 
  select(year, district, incumbent, last_result, cpvi, cook, IE, `daily kos`,
         sabatos_crystal_ball, real_clear,
         `538`) %>% 
  # Removing special characters
  mutate(district = str_replace_all(district, "[^[:alnum:]]", " "),
         district = str_replace_all(district, "at large", "AL")) %>% 
  # Separating state and district
  separate(district, into = c("state", "district"), sep = " (?=[^ ]+$)", extra = "merge") %>% 
  mutate(district = case_when(
    district == "at large" ~ "AL",
    TRUE ~ district
  )) %>% 
  # Removing (flip) from data
  mutate(across(everything(), ~str_remove_all(., "\\s\\(flip\\)"))) %>% 
  # Re-coding the other expert predictions
  mutate(across(c(cook, sabatos_crystal_ball), ~case_when(
    . %in% c("Solid D", "Safe D") ~ 1,
    . == "Likely D" ~ 2,
    . == "Lean D" ~ 3,
    . == "Tossup" ~ 4,
    . == "Lean R" ~ 5,
    . == "Likely R" ~ 6,
    . %in% c("Solid R", "Safe R") ~ 7,
  )))

# Saving csv
write_csv(data_full, str_c("expert_rating_", Sys.Date(), ".csv"))
  

