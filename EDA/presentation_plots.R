## Liam Jennings
## Honors Seminar: Disney


# Getting Started ---------------------------------------------------------

## libraries
library(tidyverse)
library(gt)
library(gtExtras)

## create theme
disney_theme <- function(){
  # theme
  theme(
    # adjust plot title
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    # adjust plot subtitle
    plot.subtitle = element_text(size = 16, face = "bold", hjust = 0.5),
    # adjust plot caption
    plot.caption = element_text(size = 10),
    # adjust x axis title
    axis.title.x = element_text(size = 14, face = "bold", hjust = 0.5),
    # adjust y axis title
    axis.title.y = element_text(size = 14, face = "bold", hjust = 0.5),
    # adjust x axis text
    axis.text.x = element_text(size = 12, hjust = 0.5),
    # adjust y axis text
    axis.text.y = element_text(size = 12, hjust = 0.5),
    # adjust legend position
    legend.position = "bottom",
    # adjust legend title text
    legend.title = element_text(size = 14, face = "bold", hjust = 0.5),
    # adjust legend text
    legend.text = element_text(size = 12, hjust = 0.5),
    # adjust the strip text size
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    # adjust the strip text background color
    strip.background = element_rect(fill = "#013369", color = "black", linewidth = 1),
  )
}



# Data Table --------------------------------------------------------------

wait_times_full |> 
  # mutate
  mutate(
    # convert day of week to character
    weekday = case_when(
      DAYOFWEEK == 1 ~ "Monday",
      DAYOFWEEK == 2 ~ "Tuesday",
      DAYOFWEEK == 3 ~ "Wednesday",
      DAYOFWEEK == 4 ~ "Thursday",
      DAYOFWEEK == 5 ~ "Friday", 
      DAYOFWEEK == 6 ~ "Saturday",
      DAYOFWEEK == 7 ~ "Sunday"
    )
  ) |> 
  select(
    ride_name,
    datetime,
    posted_wait,
    actual_wait,
    ride_duration,
    weekday,
    SEASON,
    WDWMEANTEMP,
    inSession
  ) |> 
  # drop NAs
  drop_na(
    weekday, 
    SEASON
  ) |> 
  # randomly sample 10 observations
  slice(
    c(
      13744,
      913889,
      2551108,
      23347,
      102887,
      108,
      802817,
      784598,
      1156600,
      317757
    )
  ) |> 
  # gt table
  gt() |> 
  # align columns
  cols_align(
    align = "center"
  ) |> 
  # label columns
  cols_label(
    ride_name = "Ride Name",
    datetime = "Date",
    posted_wait = "Posted Wait",
    actual_wait = "Actual Wait",
    ride_duration = "Ride Duration",
    weekday = "Day of Week",
    SEASON = "Season",
    WDWMEANTEMP = "Mean Temperature",
    inSession = "% of Schools in Session"
  ) |> 
  # format numerical columns
  fmt_number(
    columns = c(
      WDWMEANTEMP
    ),
    decimals = 1
  ) |> 
  # # add color
  # data_color(
  #   # columns
  #   columns = c(
  #     avg_total_yards_home_team,
  #     net_ppg,
  #     net_offensive_yards_ma4
  #   ),
  #   # scale
  #   fn = scales::col_numeric(
  #     palette = c("dodgerblue4", "white", "goldenrod"), 
  #     domain = NULL
  #   )
  # ) |> 
  # # this needs to be in reverse since it is a defensive statistic (lower = better)
  # data_color(
  #   # columns
  #   columns = c(
  #     avg_total_yards_allowed_ma4_away_team
  #   ),
  #   # scale
  #   fn = scales::col_numeric(
  #     palette = c("goldenrod", "white", "dodgerblue4"), 
  #     domain = NULL
  #   )
  # ) |> 
  # title and subtitle
  tab_header(
    title = md("**Walt Disney World Wait Times Data**"),
    subtitle = md("*Data: TouringPlans.com*")
  ) |> 
  # footnote
  tab_footnote(
    footnote = md("*From 2015-2021*")
  ) |> 
  # theme
  gtExtras::gt_theme_nytimes()
