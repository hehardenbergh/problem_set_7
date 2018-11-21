# load libraries
library(tidyverse)
library(fs)
library(stringr)

# read in JS data
raw_JS <- read_csv("mt_2_results.csv")
# tidy the JS data
tidy_data_JS <- raw_JS %>% 
  filter(!(district %in% c("sen", "gov"))) %>% 
  mutate(key = paste(state, district, sep = "-"),
         actual_dem_advantage = round(100 * ((dem_votes - rep_votes)/(dem_votes + rep_votes + other_votes)), 1))

# download upshot data from internt
download.file(url = "https://goo.gl/ZRCBda",
              destfile = "master.zip",
              quiet = TRUE,
              mode = "wb")
# unzip upshot data
unzip("master.zip")
# remove zip file
file_delete("master.zip")

# create vector of file names from Upshot
file_names <- dir_ls("2018-live-poll-results-master/data/")
# load all files into one tibble
raw_upshot <- map_dfr(file_names, read_csv, .id = "source")

# create tidy upshot data
tidy_upshot <- raw_upshot %>% 
  # filter out senate and governor races and polls from anything but wave 3
  filter(!(str_sub(source, 53L, 55L) %in% c("sen", "gov")), 
         str_sub(source, 56L, 56L) == "3") %>% 
  # create variables for state, district, and key
  mutate(state = toupper(str_sub(source, 51L, 52L)),
         district = str_sub(source, 53L, 54L),
         key = paste(state, district, sep = "-")) %>% 
  # select important variables for final output
  select(key, district, state, response, final_weight) %>% 
  # group data by key and response
  group_by(key, response) %>% 
  # count the data with final_weight as weight
  tally(wt = final_weight) %>% 
  # spread the data by response
  spread(key = "response", value = "n", fill = 0) %>% 
  # create republican advantage variable
  mutate(poll_dem_advantage = round(100 * ((Dem - Rep) / (`3` + `4` + `5` + `6` + Rep + Dem + Und)), 1))

# create x axis variables
x_axis_upshot <- raw_upshot %>% 
  # filter out senate and governor races and polls from anything but wave 3
  filter(!(str_sub(source, 53L, 55L) %in% c("sen", "gov")), 
         str_sub(source, 56L, 56L) == "3") %>% 
  # create variables for state, district, and key
  mutate(state = toupper(str_sub(source, 51L, 52L)),
         district = str_sub(source, 53L, 54L),
         key = paste(state, district, sep = "-")) %>% 
  # select important variables for final output
  select(key, response, educ4, ager, file_race, gender, likely, phone_type, final_weight) %>%
  # group data by key
  group_by(key) %>% 
  # create importnt variables
  summarize(pct_female = round((sum(gender == "Female")/n()) * 100, 1),
            pct_white = round((sum(file_race == "White")/n()) * 100, 1),
            pct_likely = round((sum(likely %in% c("Already voted", "Almost certain", "Very likely"))/n()) * 100, 1),
            pct_college = round((sum(educ4 %in% c("4-year College Grad.", "Postgraduate Degree"))/n()) * 100, 1), 
            pct_und = round((sum(response == "Und")/n()) * 100, 1),
            pct_mil = round((sum(ager == "18 to 34")/n()) * 100, 1))

# create dataframe for shiny app
final_shiny_data <- tidy_upshot %>% 
  # left join all three datasets
  left_join(x_axis_upshot, by = "key") %>% 
  left_join(tidy_data_JS, by = "key") %>% 
  # select important variables
  select(key, state, district, poll_dem_advantage, actual_dem_advantage, pct_female, pct_white, pct_likely, pct_college, pct_und, pct_mil)

# create dataframe of state names and abbreviations of those with data
state_test <- data.frame(state_abb = state.abb, state_name = state.name)

# use semi_join to filter for states thar are in our dataset
shiny_states <- state_test %>%
  semi_join(final_shiny_data, by = c( "state_abb" = "state")) %>%
  # spread to use state names in shiny selectInput
  spread(state_name, state_abb)

# create rds file 
write_rds(final_shiny_data, "final_shiny_data.rds")

# Create rds file of states/state names 
write_rds(shiny_states, "shiny_states.rds")

