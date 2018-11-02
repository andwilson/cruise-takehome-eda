## clean workspace
rm(list=ls(all=TRUE))

#################
## load libraries

## data manipulation libraries
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(forcats)
#library(purrr)
#library(car)
#library(scales)

## i/o libraries 
library(readxl)
#library(readr)
#library(knitr)
#library(officer)
#library(rvg)

## visualization libraries
library(ggplot2)
library(ggthemes)
library(scales)
library(GGally)
#library(gridExtra)
#library(lattice)
#library(treemapify)
#library(waterfall)
#library(vcd)

## calculation libraries
#library(FinCal)
#library(zoo)
#library(caTools)

## ML libraries

## Special libraries
library(reticulate)


#######################
## settings and globals

## disable scientific notation
options(scipen=999)

## set cruise colors
cruise_colors <- c("#fc553c", "#553d65")


##############
## import data

## set up data directories, files names, and paths
root_dir <- "/Users/andrewwilson/Documents-backup/Projects/cruise-take-home/"
data_file_name <- "final_analytics_takehome.xlsx"
data_path <- str_c(root_dir, "data/", data_file_name)

## import data from Excel
df_raw <- read_excel(data_path)


####################
## 'tidy' input data
# pull in 'tidy,' or normalized data has the following properties:
# Each variable forms a column
# Each observation forms a row
# Each type of observational unit forms a table

## initial data cleaning
df <- df_raw %>% 
  # change characters to factors
  mutate_if(is.character, as.factor) %>%
  mutate(user_id = as.factor(user_id)) %>%
  mutate(num_riders = as.factor(num_riders)) %>%
  mutate(num_near_misses = as.factor(num_near_misses)) %>%
  mutate(rating = as.factor(rating)) %>%
  # calculate ride time in minutes
  mutate(ride_mins = as.numeric(end_time - start_time)/60) %>%
  # eliminate data points that have negative ride time
  filter(ride_mins>0) %>%
  # add categorical labels for long, short, cheap, and expensive rides
  mutate(time_cat = ifelse(ride_mins < 45, "short", "long")) %>%
  mutate(price_cat = ifelse(price < 4, "cheap", "expensive"))


############################
## exploratory data analysis

df_times <- df %>% 
  filter(car_id == "hulk") %>%
  filter(start_time < ymd_hms("2018-10-02 6:00:00")) %>%
  select(start_time, end_time, ride_mins, num_riders) %>%
  mutate(ride_id = row_number()) %>%
  gather("startORend", "time", 1:2) %>%
  arrange(ride_id)

ggplot(df_times, aes(time, ride_id, color=startORend)) + geom_point() + scale_fill_brewer(palette="Set1")



