## Liam Jennings
## Honors Seminar: Disney


# Getting Started ---------------------------------------------------------

## libraries
library(tidyverse)
library(gt)
library(gtExtras)


## set theme
theme_set(theme_bw())

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



# General Wait Times Throughout the Day -----------------------------------

## general wait times
wait_times_full |> 
  # mutate
  mutate(
    # hour of day
    hour_of_day = hour(datetime),
    
    # year as factor
    YEAR = factor(YEAR)
  ) |> 
  # filter
  filter(
    # only use wait times that make sense
    posted_wait >= 0 & posted_wait <= 400 |
      actual_wait >= 0 & actual_wait <= 400,
    
    # pre-COVID
    YEAR %in% c(2015:2019),
    
    # only use primary open hours
    hour_of_day >= 8 & hour_of_day <= 21,
  ) |> 
  # group by
  group_by(
    hour_of_day,
    YEAR
  ) |> 
  # average wait time
  summarize(
    # average posted wait
    mean_actual_wait = mean(actual_wait, na.rm = TRUE)
  ) |> 
  ggplot(
    aes(
      # x axis
      hour_of_day,
      # y axis
      mean_actual_wait,
      # color
      color = YEAR
    )
  ) +
  # geom line
  geom_line() +
  # points
  geom_point(
    alpha = 0.6,
    size = 2
  ) +
  # average posted wait time
  geom_hline(
    yintercept = 27.22,
    # dashed line
    linetype = "dashed",
    # color
    color = "black",
    # line width
    linewidth = 1.5
  ) +
  # scale x axis
  scale_x_continuous(
    breaks = seq(8, 21, 1),
    labels = c(
      8:12,
      1:9
    )
  ) +
  # scale colors
  scale_color_manual(
    values = c("#3136c6", "#ad41df", "#F3CC64", "#1a9bcb", "#B12228")
  ) +
  # labels
  labs(
    x = "Hour of the Day",
    y = "Average Actual Wait Times",
    color = "Year",
    title = "Disney Wait Times by Hour of Day"
  ) +
  # custom theme
  disney_theme()



# General EDA -------------------------------------------------------------

## mean wait times of each ride
wait_times_full |> 
  # mutate
  mutate(
    # hour of day
    hour_of_day = hour(datetime)
  ) |> 
  filter(
    # only use times that make sense
    posted_wait >= 0 |
    actual_wait >= 0 & actual_wait <= 400,
    
    hour_of_day >= 8 & hour_of_day <= 21
  ) |> 
  # group by ride
  group_by(ride_name) |> 
  summarize(
    # mean posted wait time
    mean_posted_wait = mean(posted_wait, na.rm = TRUE),
    
    # sd posted wait time
    sd_posted_wait = sd(posted_wait, na.rm = TRUE),
    
    # mean actual wait time
    mean_actual_wait = mean(actual_wait, na.rm = TRUE),
    
    # sd actual wait time
    sd_actual_wait = sd(actual_wait, na.rm = TRUE)
  ) |> 
  mutate(
    # difference between actual
    difference = mean_posted_wait - mean_actual_wait
  ) |> 
  # order wait time
  arrange(-difference) |> 
  # gt table
  gt() |> 
  # align columns
  cols_align(
    align = "center"
  ) |> 
  # label columns
  cols_label(
    ride_name = "Ride Name",
    mean_posted_wait = "Avg Posted Wait Time",
    sd_posted_wait = "SD of Posted Wait Time",
    mean_actual_wait = "Avg Actual Wait Time",
    sd_actual_wait = "SD of Actual Wait Time",
    difference = "Difference of Posted and Actual Wait Time"
  ) |> 
  # format numerical columns
  fmt_number(
    columns = c(
      mean_posted_wait,
      sd_posted_wait,
      mean_actual_wait,
      sd_actual_wait,
      difference
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
  # this needs to be in reverse since it is a defensive statistic (lower = better)
  data_color(
    # columns
    columns = c(
      mean_posted_wait,
      mean_actual_wait,
      difference
    ),
    # scale
    fn = scales::col_numeric(
      palette = c("goldenrod", "white", "dodgerblue4"),
      domain = NULL
    )
  ) |>
  # title and subtitle
  tab_header(
    title = md("**Walt Disney World Overestimates Wait Times**"),
    subtitle = md("*Data: TouringPlans.com*")
  ) |> 
  # footnote
  tab_footnote(
    footnote = md("*SD = Standard Deviation<br>
                  Gold = 'Good'<br>
                  Blue = 'Bad'*")
  ) |> 
  # theme
  gtExtras::gt_theme_nytimes()



# One Ride Mean Wait Time by Year -----------------------------------------

## wait time over the day for one ride
wait_times_full |> 
  # mutate
  mutate(
    # hour of day
    hour_of_day = hour(datetime)
  ) |> 
  # Avatar Flight of Passage
  filter(
    ride_name == "Seven Dwarfs Mine Train",
    # only have positive wait times
    posted_wait >= 0 & posted_wait <= 400 |
      actual_wait >= 0 & actual_wait <= 400,
    
    hour_of_day >= 8 & hour_of_day <= 21,
    
    YEAR %in% C(2015:2019)
  ) |>
  # group by
  group_by(date) |> 
  summarize(
    # mean wait time
    mean_actual_wait = mean(actual_wait, na.rm = TRUE)
  ) |> 
  # mutate
  mutate(
    year = factor(year(date)),
    
    month_day = format(date, "%m-%d")
  ) |> 
  ggplot(
    aes(
      # x axis
      date,
      # y axis
      mean_actual_wait,
      # group
      group = year,
      # color
      color = year
    ) 
  ) +
  # line plot
  geom_line() +
  # scatterplot
  geom_point(
    alpha = 0.7
  ) +
  # scale x axis
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  # scale colors
  scale_color_manual(
    values = c("#3136c6", "#ad41df", "#F3CC64", "#1a9bcb", "#B12228")
  ) +
  # labels
  labs(
    x = "Date",
    y = "Average Actual Wait (min)",
    color = "Year",
    title = "Wait Times for Seven Dwarfs Mine Train"
  ) +
  # custom theme
  disney_theme()




# Season ------------------------------------------------------------------


## season
wait_times_full |> 
  # mutate
  mutate(
    # hour of day
    hour_of_day = hour(datetime)
  ) |> 
  # filter
  filter(
    # only use wait times that make sense
    posted_wait >= 0 & posted_wait <= 400 |
      actual_wait >= 0 & actual_wait <= 400,
    
    # only use primary open hours
    hour_of_day >= 8 & hour_of_day <= 21,
    
    # remove NA season
    !(is.na(SEASON))
  ) |> 
  # group by
  group_by(
    SEASON,
  ) |> 
  # average wait time
  summarize(
    # average posted wait
    mean_actual_wait = mean(actual_wait, na.rm = TRUE),
  ) |> 
  ggplot(
    aes(
      # x axis
      mean_actual_wait,
      # y axis
      fct_reorder(SEASON, mean_actual_wait),
      # color
      fill = SEASON
    )
  ) +
  # boxplot
  geom_col() +
  # labels
  labs(
    x = "Actual Wait Times",
    y = "Season",
    title = "Disney Wait Times for Each Season"
  ) +
  # custom theme
  disney_theme() +
  # remove legend
  theme(
    legend.position = "none"
  )



# only have a couple seasons on there
wait_times_full |>
  # mutate
  mutate(
    # hour of day
    hour_of_day = hour(datetime)
  ) |>
  # filter
  filter(
    # only use wait times that make sense
    posted_wait >= 0 & posted_wait <= 400 |
      actual_wait >= 0 & actual_wait <= 400,

    # only use primary open hours
    hour_of_day >= 8 & hour_of_day <= 21,

    # remove NA season
    !(is.na(SEASON))
  ) |>
  # group by
  group_by(
    SEASON,
  ) |>
  # average wait time
  summarize(
    # average posted wait
    mean_actual_wait = mean(actual_wait, na.rm = TRUE),
  ) |>
  # filter
  filter(
    mean_actual_wait > 32.1 | mean_actual_wait <= 25
  ) |>
  ggplot(
    aes(
      # x axis
      mean_actual_wait,
      # y axis
      fct_reorder(SEASON, mean_actual_wait),
      # color
      fill = SEASON
    )
  ) +
  # bar plot
  geom_col() +
  # scale x axis
  scale_x_continuous(
    breaks = seq(0, 35, 5)
  ) +
  # scale fill
  scale_fill_manual(
    values = c("#BB2528", "#0F1647", "#ffcc33", "#E66C2C", "#1a9bcb")
  ) +
  # labels
  labs(
    x = "Avg Actual Wait Times",
    y = "Season",
    title = "Disney Wait Times for Each Season",
    caption = "Top 3 and Bottom 2 Seasons"
  ) +
  # custom theme
  disney_theme() +
  # remove legend
  theme(
    legend.position = "none"
  )



# Day of Week -------------------------------------------------------------

## season
wait_times_full |> 
  # mutate
  mutate(
    # hour of day
    hour_of_day = hour(datetime),
    
    # convert day of week to character
    weekday = factor(
      case_when(
        DAYOFWEEK == 1 ~ "Monday",
        DAYOFWEEK == 2 ~ "Tuesday",
        DAYOFWEEK == 3 ~ "Wednesday",
        DAYOFWEEK == 4 ~ "Thursday",
        DAYOFWEEK == 5 ~ "Friday", 
        DAYOFWEEK == 6 ~ "Saturday",
        DAYOFWEEK == 7 ~ "Sunday"
      ),
      levels = c(
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
        "Sunday"
      )
    )
  ) |> 
  # filter
  filter(
    # only use wait times that make sense
    posted_wait >= 0 & posted_wait <= 400 |
      actual_wait >= 0 & actual_wait <= 400,
    
    # pre-COVID
    YEAR %in% c(2015:2019),
    
    # only use primary open hours
    hour_of_day >= 8 & hour_of_day <= 21,
    
    # remove NA season
    !(is.na(DAYOFWEEK))
  ) |> 
  # group by
  group_by(
    weekday,
  ) |> 
  # average wait time
  summarize(
    # average posted wait
    mean_actual_wait = mean(actual_wait, na.rm = TRUE),
    
    # sd of posted wait
    sd_actual_wait = sd(actual_wait, na.rm = TRUE),
    
    .groups = "drop"
  ) |> 
  # mutate
  mutate(
    # standard error
    se = sd_actual_wait / sqrt(n())
  ) |> 
  ggplot(
    aes(
      # x axis
      mean_actual_wait,
      # y axis
      fct_rev(weekday),
      # color
      fill = weekday
    )
  ) +
  # bar plot
  geom_col() +
  # standard error bars
  geom_errorbar(
    aes(
      xmin = mean_actual_wait - se, 
      xmax = mean_actual_wait + se
    ), 
    width = 0.2
  ) +
  # line for average
  geom_vline(
    xintercept = 52.44286,
    # dashed line
    linetype = "dashed",
    # color
    color = "black",
    # line width
    linewidth = 1.5
  ) +
  # scale colors
  scale_fill_manual(
    values = c("#3136c6", "#ad41df", "#C4CED4", "#1a9bcb", "#B12228", "#F3CC64", "#12194A")
  ) +
  # scale x axis
  scale_x_continuous(
    breaks = seq(0, 70, 10)
  ) +
  # labels
  labs(
    x = "Average Posted Wait Times",
    y = "Day of Week",
    title = "Disney Wait Times for Day of Week"
  ) +
  # custom theme
  disney_theme() +
  # remove legend
  theme(
    legend.position = "none"
  )


## day of week without standard error bars
wait_times_full |> 
  # mutate
  mutate(
    # hour of day
    hour_of_day = hour(datetime),
    
    # convert day of week to character
    weekday = factor(
      case_when(
        DAYOFWEEK == 1 ~ "Monday",
        DAYOFWEEK == 2 ~ "Tuesday",
        DAYOFWEEK == 3 ~ "Wednesday",
        DAYOFWEEK == 4 ~ "Thursday",
        DAYOFWEEK == 5 ~ "Friday", 
        DAYOFWEEK == 6 ~ "Saturday",
        DAYOFWEEK == 7 ~ "Sunday"
      ),
      levels = c(
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
        "Sunday"
      )
    )
  ) |> 
  # filter
  filter(
    # only use wait times that make sense
    posted_wait >= 0 & posted_wait <= 400 |
      actual_wait >= 0 & actual_wait <= 400,
    
    # pre-COVID
    YEAR %in% c(2015:2019),
    
    # only use primary open hours
    hour_of_day >= 8 & hour_of_day <= 21,
    
    # remove NA season
    !(is.na(DAYOFWEEK))
  ) |> 
  # group by
  group_by(
    weekday,
  ) |> 
  # average wait time
  summarize(
    # average posted wait
    mean_actual_wait = mean(actual_wait, na.rm = TRUE),
    
    # sd of posted wait
    sd_actual_wait = sd(actual_wait, na.rm = TRUE),
    
    .groups = "drop"
  ) |> 
  # mutate
  mutate(
    # standard error
    se = sd_actual_wait / sqrt(n())
  ) |> 
  ggplot(
    aes(
      # x axis
      mean_actual_wait,
      # y axis
      fct_rev(weekday),
      # color
      fill = weekday
    )
  ) +
  # bar plot
  geom_col() +
  # # standard error bars
  # geom_errorbar(
  #   aes(
  #     xmin = mean_posted_wait - se, 
  #     xmax = mean_posted_wait + se
  #   ), 
  #   width = 0.2
  # ) +
  # line for average
  geom_vline(
    xintercept = 27.82857,
    # dashed line
    linetype = "dashed",
    # color
    color = "black",
    # line width
    linewidth = 1.5
  ) +
  # scale colors
  scale_fill_manual(
    values = c("#3136c6", "#ad41df", "#C4CED4", "#1a9bcb", "#B12228", "#F3CC64", "#12194A")
  ) +
  # scale x axis
  scale_x_continuous(
    breaks = seq(0, 70, 10)
  ) +
  # labels
  labs(
    x = "Average Actual Wait Times",
    y = "Day of Week",
    title = "Day of the Week is Not Significant"
  ) +
  # custom theme
  disney_theme() +
  # remove legend
  theme(
    legend.position = "none"
  )




# Temperature ------------------------------------------------------------------

## temperature
wait_times_full |> 
  # mutate
  mutate(
    # hour of day
    hour_of_day = hour(datetime),
    
    # convert day of week to character
    weekday = factor(
      case_when(
        DAYOFWEEK == 1 ~ "Monday",
        DAYOFWEEK == 2 ~ "Tuesday",
        DAYOFWEEK == 3 ~ "Wednesday",
        DAYOFWEEK == 4 ~ "Thursday",
        DAYOFWEEK == 5 ~ "Friday", 
        DAYOFWEEK == 6 ~ "Saturday",
        DAYOFWEEK == 7 ~ "Sunday"
      ),
      levels = c(
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
        "Sunday"
      )
    )
  ) |> 
  # filter
  filter(
    # only use wait times that make sense
    posted_wait >= 0 & posted_wait <= 400 |
      actual_wait >= 0 & actual_wait <= 400,
    
    # pre-COVID
    YEAR %in% c(2015:2019),
    
    # only use primary open hours
    hour_of_day >= 8 & hour_of_day <= 21,
  ) |> 
  # group by
  group_by(
    date
  ) |> 
  # average wait time
  summarize(
    # average posted wait
    mean_actual_wait = mean(actual_wait, na.rm = TRUE),
    
    # sd of posted wait
    sd_actual_wait = sd(actual_wait, na.rm = TRUE),
    
    # average temperature
    mean_temp = mean(WDWMEANTEMP, na.rm = TRUE),
    
    .groups = "drop"
  ) |> 
  # mutate
  mutate(
    # standard error
    se = sd_actual_wait / sqrt(n())
  ) |> 
  # plot
  ggplot(
    aes(
      # x axis
      mean_temp,
      # y axis
      mean_actual_wait
    )
  ) +
  # points
  geom_point(
    alpha = 0.6,
    size = 2,
    color = "#1a9bcb"
  ) +
  # LOES curve
  geom_smooth(
    # method
    method = "loess",
    # color
    color = "goldenrod"
  ) +
  # labels
  labs(
    x = "Mean Temperature (in Fahrenheit)",
    y = "Average Actual Wait Times",
    color = "Year",
    title = "Temperature Does Not Impact Wait Times"
  ) +
  # custom theme
  disney_theme()




# School in Session -------------------------------------------------------

## % of schools in session
wait_times_full |> 
  # mutate
  mutate(
    # hour of day
    hour_of_day = hour(datetime),
    
    # year as factor
    YEAR = factor(YEAR),
    
    # insession as numeric
    inSession = parse_number(inSession)
  ) |> 
  # filter
  filter(
    # only use wait times that make sense
    posted_wait >= 0 & posted_wait <= 400 |
      actual_wait >= 0 & actual_wait <= 400,
    
    # pre-COVID
    YEAR %in% c(2015:2019),
    
    # only use primary open hours
    hour_of_day >= 8 & hour_of_day <= 21,
  ) |> 
  # group by
  group_by(
    date
  ) |> 
  # average wait time
  summarize(
    # average posted wait
    mean_actual_wait = mean(actual_wait, na.rm = TRUE),
    
    # average temperature
    mean_inSession = mean(inSession, na.rm = TRUE)
  ) |> 
  # mutate
  mutate(
    YEAR = factor(year(date))
  ) |> 
  ggplot(
    aes(
      # x axis
      mean_inSession,
      # y axis
      mean_actual_wait
    )
  ) +
  # points
  geom_point(
    alpha = 0.6,
    size = 2,
    color = "#1a9bcb"
  ) +
  # LOES curve
  geom_smooth(
    # method
    method = "loess",
    # color
    color = "goldenrod"
  ) +
  # labels
  labs(
    x = "% of Schools in Session",
    y = "Average Actual Wait Times",
    color = "Year",
    title = "More Schools in Session Leads to Short Wait Times"
  ) +
  # custom theme
  disney_theme()



