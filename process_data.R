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
         actual_dem_advantage = (dem_votes - rep_votes)/(dem_votes + rep_votes + other_votes))

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
  mutate(poll_dem_advantage = (Dem - Rep) / (`3` + `4` + `5` + `6` + Rep + Dem + Und))

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
  summarize(pct_female = sum(gender == "Female")/n(),
            pct_white = sum(file_race == "White")/n(),
            pct_likely = sum(likely %in% c("Already voted", "Almost certain", "Very likely"))/n(),
            pct_college = sum(educ4 %in% c("4-year College Grad.", "Postgraduate Degree"))/n(), 
            pct_und = sum(response == "Und")/n(),
            pct_mil = sum(ager == "18 to 34")/n())

# create dataframe for shiny app
final_shiny_data <- tidy_upshot %>% 
  # left join all three datasets
  left_join(x_axis_upshot, by = "key") %>% 
  left_join(tidy_data_JS, by = "key") %>% 
  # select important variables
  select(key, state, district, poll_dem_advantage, actual_dem_advantage, pct_female, pct_white, pct_likely, pct_college, pct_und, pct_mil)

# create rds file
write_rds(final_shiny_data, "final_shiny_data.rds")

