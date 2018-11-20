# load libraries
library(tidyverse)
library(fs)
library(stringr)

# read in JS data
raw_JS <- read_csv("mt_2_results.csv")
# tidy the JS data
tidy_data_JS <- raw_JS %>% 
  filter(!(district %in% c("sen", "gov"))) %>% 
  mutate(key = paste(state, district, sep = "-"))
# create rds file
write_rds(tidy_data_JS, "tidy_data_JS.rds")

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

tidy_upshot <- raw_upshot %>% 
  filter(!(str_sub(source, 53L, 55L) %in% c("sen", "gov")), 
         str_sub(source, 56L, 56L) == "3") %>% 
  mutate(state = toupper(str_sub(source, 51L, 52L)),
         district = str_sub(source, 53L, 54L),
         key = paste(state, district, sep = "-"))
# create rds file
write_rds(tidy_upshot, "tidy_upshot.rds")
