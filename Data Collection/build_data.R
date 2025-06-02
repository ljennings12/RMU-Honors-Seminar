## Liam Jennings
## Honors Seminar: Disney


# Getting Started ---------------------------------------------------------

## libraries
library(tidyverse)


## read in ride duration information
entities <- read_csv("entities.csv")

## read in metadata
metadata <- read_csv("metadata.csv")


## glimpse metadata
glimpse(metadata)



# Clean Data --------------------------------------------------------------

## set folder path
folder_path <- "C:/Users/madde/Documents/RMU/Spring 2025/HNRS 3900 Honors Seminar/Final Project/Attraction Wait Times"

## list all CSV files
files <- list.files(
  # path
  path = folder_path,
  # select all CSVs
  pattern = "*.csv",
  # full names
  full.names = TRUE
)

## combine into one dataframe
wait_times <- files %>%
  # use filenames as ride names
  set_names(
    nm = basename(.) %>% tools::file_path_sans_ext()
  ) %>%
  # apply to each csv to return dataframes
  map_dfr(
    ~ read_csv(.x),
    .id = "ride_name"
  )

## glimpse
glimpse(wait_times)


## clean the dataset
wait_times_clean <- {
  wait_times |> 
    # mutate
    mutate(
      # convert to date object
      date = mdy(date),
      
      # give ride name its real name
      code = case_when(
        ride_name == "7_dwarfs_train"          ~ "MK141",
        ride_name == "alien_saucers"           ~ "HS104",
        ride_name == "dinosaur"                ~ "AK18",
        ride_name == "expedition_everest"      ~ "AK11",
        ride_name == "flight_of_passage"       ~ "AK86",
        ride_name == "kilimanjaro_safaris"     ~ "AK07",
        ride_name == "navi_river"              ~ "AK85",
        ride_name == "pirates_of_caribbean"    ~ "MK16",
        ride_name == "rock_n_rollercoaster"    ~ "HS12",
        ride_name == "slinky_dog"              ~ "HS103",       
        ride_name == "soarin"                  ~ "EP09",
        ride_name == "spaceship_earth"         ~ "EP02",
        ride_name == "splash_mountain"         ~ "MK04",
        ride_name == "toy_story_mania"         ~ "HS20"
      )
    ) |> 
    # join with entities
    left_join(
      entities
    ) |> 
    # select
    select(
      code,
      ride_name = name,
      datetime,
      date,
      posted_wait = SPOSTMIN,
      actual_wait = SACTMIN,
      ride_duration = duration,
      average_wait_per_hundred
    ) 
}


## add metadata
wait_times_full <- {
  ## join with metadata
  wait_times_clean |> 
    # join with metadata
    left_join(
      metadata |> mutate(DATE = mdy(DATE)),
      by = c("date" = "DATE")
    )
}
