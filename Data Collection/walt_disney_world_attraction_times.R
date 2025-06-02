## Liam Jennings
## Honors Seminar: Disney


# Getting Started ---------------------------------------------------------

## libraries
library(tidyverse)
library(ggdendro)

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


## read in ride duration information
entities <- read_csv("entities.csv")

## read in metadata
metadata <- read_csv("metadata.csv")


## glimpse metadata
glimpse(metadata)


# Get Data ----------------------------------------------------------------

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



# EDA ---------------------------------------------------------------------

wait_times_clean |> 
  # remove times when the ride was closed
  filter(
    posted_wait != -999
  ) |> 
  # group by ride
  group_by(ride_name) |> 
  summarize(
    # mean posted wait time
    mean_posted_wait = mean(posted_wait, na.rm = TRUE),
    # mean actual wait time
    mean_actual_wait = mean(actual_wait, na.rm = TRUE)
  ) |> 
  # order wait time
  arrange(-mean_actual_wait)


## rides closed
ride_closed <- wait_times_clean |> 
  filter(
    actual_wait == -999
  )

## which rides were close the longest
ride_closed |> 
  count(ride_name) |> 
  arrange(-n)


## wait time over the day for one ride
wait_times_clean |> 
  # 7 Dwarfs Train
  filter(
    ride_name == "Seven Dwarfs Mine Train",
    # only have positive wait times
    posted_wait >= 0
  ) |> 
  ggplot(
    aes(
      # x axis
      datetime,
      # y axis
      posted_wait
    ) 
  ) +
  # line plot
  geom_line(
    color = "#3a69a9"
  ) +
  # labels
  labs(
    x = "Date and Time",
    y = "Posted Wait (min)",
    title = "Wait Times for 7 Dwarfs Mine Train"
  ) +
  # custom theme
  disney_theme()



# k-means Clustering Rides by Wait Time Patterns ----------------------------------

## set seed
set.seed(8029)

## prepare the data
ride_daily_waits <- {
  wait_times_clean |> 
    # only use posted wait times that make sense
    filter(
      posted_wait >= 0 & posted_wait <= 400
    ) |> 
    # extract one hour of the data
    mutate(
      hour_of_day = hour(datetime)
    ) |> 
    # group by ride and hour
    group_by(
      ride_name,
      hour_of_day
    ) |> 
    # find average wait time
    summarize(
      mean_posted_wait = mean(posted_wait, na.rm = TRUE)
    ) |> 
    # ungroup
    ungroup()
}


## transform data into a matrix (wide format)
ride_matrix <- ride_daily_waits |> 
  spread(
    key = hour_of_day,
    value = mean_posted_wait,
    fill = NA
  )


## scale data
ride_matrix_scaled <- scale(
  # remove name column before scaling
  ride_matrix |> select(-1)
)

## replace NA values with 0
ride_matrix_scaled[is.na(ride_matrix_scaled)] <- 0


## k-means clustering
ride_times_kmeans <- ride_matrix_scaled |> 
  kmeans(
    # number of clusters
    centers = 3, 
    # algorith,
    algorithm = "Lloyd", 
    # how many random sets should be chosen
    nstart = 1
  )


## add cluster back to ride matrix
ride_matrix_clusters <- tibble(
  ride_matrix,
  cluster = ride_times_kmeans$cluster
)


## join with mean posted wait time
ride_daily_waits |> 
  left_join(
    # data
    ride_matrix_clusters |> select(ride_name, cluster)
  ) |> 
  # mutate
  mutate(
    # cluster as a factor
    cluster = factor(cluster)
  ) |> 
  ggplot(
    aes(
      # x axis
      hour_of_day,
      # y axis
      mean_posted_wait,
      # color
      color = cluster
    )
  ) +
  # line plot
  geom_line(
    # line width
    linewidth = 1.15
  ) +
  # points
  geom_point() +
  # facet wrap
  facet_wrap(
    # by ride name
    ~ ride_name
  ) +
  # scale color
  scale_color_manual(
    values = c("#3136c6", "#ad41df", "#C4CED4", "black")
  ) +
  # labels
  labs(
    x = "Hour of Day",
    y = "Average Wait Time (min)",
    color = "Cluster",
    title = "Clustered Ride Wait Time Patterns"
  ) +
  # custome theme
  disney_theme()


## ride names with clusters
ride_cluster <- ride_matrix_clusters |> 
  group_by(ride_name) |> 
  summarize(cluster = first(cluster))



# Hierarchical Clustering -------------------------------------------------

## convert to distance and only keep 8am through 10 pm
rides_hierachical_clustering <- ride_matrix_scaled[, 9:23] |> 
  dist() |> 
  hclust(method = "complete")


## dendrogram
rides_hierachical_clustering |> 
  # dendrogram
  ggdendrogram(
    labels = FALSE,
    leaf_labels = FALSE,
    theme_dendro = FALSE
  ) +
  # labels
  labs(
    y = "Dissimilarity between clusters"
  ) +
  # custom theme
  disney_theme() +
  # remove x axis text
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
  )


## dendrogram
rides_hierachical_clustering |> 
  # dendrogram
  ggdendrogram(
    labels = FALSE,
    leaf_labels = FALSE,
    theme_dendro = FALSE
  ) +
  # labels
  labs(
    y = "Dissimilarity between clusters"
  ) +
  # custom theme
  disney_theme() +
  # remove x axis text
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
  )


## cutting dendrogram
rides_hierachical_clustering |> 
  # dendrogram
  ggdendrogram(
    labels = FALSE,
    leaf_labels = FALSE,
    theme_dendro = FALSE
  ) +
  # cut tree
  geom_hline(
    yintercept = 5, 
    linetype = "dashed", 
    color = "firebrick"
  ) +
  # labels
  labs(
    y = "Dissimilarity between clusters"
  ) +
  # custom theme
  disney_theme() +
  # remove x axis text
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
  )


## example of cutting tree
cutree(
  rides_hierachical_clustering,
  h = 5
)


## add cluster back to ride matrix
ride_matrix_hierachical_clusters <- tibble(
  ride_matrix,
  cluster = cutree(
    rides_hierachical_clustering,
    h = 5
  )
)


## ride names with clusters
ride_hierarchical_cluster <- ride_matrix_hierachical_clusters |> 
  group_by(ride_name) |> 
  summarize(cluster = first(cluster))



## join with mean posted wait time
ride_daily_waits |> 
  left_join(
    # data
    ride_hierarchical_cluster |> select(ride_name, cluster)
  ) |> 
  # mutate
  mutate(
    # cluster as a factor
    cluster = factor(cluster)
  ) |> 
  # filter
  filter(
    hour_of_day >= 8 & hour_of_day <= 23
  ) |> 
  ggplot(
    aes(
      # x axis
      hour_of_day,
      # y axis
      mean_posted_wait,
      # color
      color = cluster
    )
  ) +
  # line plot
  geom_line(
    # line width
    linewidth = 1.15
  ) +
  # points
  geom_point() +
  # facet wrap
  facet_wrap(
    # by ride name
    ~ ride_name
  ) +
  # scale color
  scale_color_manual(
    values = c("#3136c6", "#ad41df", "#C4CED4", "black")
  ) +
  # labels
  labs(
    x = "Hour of Day",
    y = "Average Wait Time (min)",
    color = "Cluster",
    title = "Clustered Ride Wait Time Patterns"
  ) +
  # custome theme
  disney_theme()



## cluster table
ride_daily_waits |> 
  left_join(
    # data
    ride_hierarchical_cluster |> select(ride_name, cluster)
  ) |> 
  # mutate
  mutate(
    # cluster as a factor
    cluster = factor(cluster)
  ) |> 
  # filter
  filter(
    hour_of_day >= 8 & hour_of_day <= 23
  ) |> 
  # group by
  group_by(cluster) |> 
  # summarize
  summarize(
    avg_posted_wait = mean(mean_posted_wait, na.rm = TRUE),
    sd_posted_wait = sd(mean_posted_wait, na.rm = TRUE),
    number_of_rides = n_distinct(ride_name)
  )



# Ride Efficiency Rankings ------------------------------------------------

## calculate ride efficiency
wait_times_clean |> 
  # only use wait times that make sense
  filter(
    posted_wait >= 0 & posted_wait <= 400 |
    actual_wait >= 0 & actual_wait <= 400
  ) |> 
  mutate(
    # efficiency
    posted_wait_efficiency = posted_wait / ride_duration,
    actual_wait_efficiency = actual_wait / ride_duration
  ) |> 
  # group by 
  group_by(ride_name) |> 
  # summarize
  summarize(
    # average ride efficiency
    mean_posted_wait_efficiency = mean(posted_wait_efficiency, na.rm = TRUE),
    mean_actual_wait_efficiency = mean(actual_wait_efficiency, na.rm = TRUE)
  ) |> 
  # arrange by mean efficiency
  arrange(-mean_posted_wait_efficiency)


## higher efficiency: long waits, short ride duration


## use mean posted wait efficiency
## calculate ride efficiency
ride_efficiency <- wait_times_clean |> 
  # only use wait times that make sense
  filter(
    posted_wait >= 0 & posted_wait <= 400
  ) |> 
  mutate(
    # efficiency
    efficiency = posted_wait / ride_duration
  ) |> 
  # group by 
  group_by(ride_name) |> 
  # summarize
  summarize(
    # average ride efficiency
    mean_efficiency = mean(efficiency, na.rm = TRUE),
    
    # average posted wait
    mean_posted_wait = mean(posted_wait, na.rm = TRUE),
    
    # duration
    ride_duration = first(ride_duration)
  ) |> 
  # arrange by mean efficiency
  arrange(-mean_efficiency)


## plot ride efficiency
ride_efficiency |> 
  ggplot(
    aes(
      # x axis
      mean_efficiency,
      # y axis
      reorder(ride_name, mean_efficiency)
    )
  ) +
  # geom column
  geom_col(
    # color
    color = "black",
    # fill 
    fill = "dodgerblue4"
  ) +
  # labels
  labs(
    x = "Average Wait Time / Ride Duration (Efficiency)",
    y = "Ride Name",
    title = "Ride Efficiency Rankings"
  ) +
  # custom theme
  disney_theme()



## plot ride efficiency with clusters
ride_efficiency |> 
  # left join with clusters
  left_join(
    ride_cluster
  ) |> 
  # have cluster as a factor
  mutate(
    cluster = factor(cluster)
  ) |> 
  # plot
  ggplot(
    aes(
      # x axis
      mean_efficiency,
      # y axis
      reorder(ride_name, mean_efficiency),
      # color
      fill = cluster
    )
  ) +
  # geom column
  geom_col(
    # color
    color = "black"
  ) +
  # scale color
  scale_fill_manual(
    values = c("#3136c6", "#ad41df", "#C4CED4")
  ) +
  # labels
  labs(
    x = "Average Wait Time / Ride Duration (Efficiency)",
    y = "Ride Name",
    fill = "Cluster",
    title = "Ride Efficiency Rankings"
  ) +
  # custom theme
  disney_theme()




## plot ride efficiency with clusters
ride_efficiency |> 
  # left join with clusters
  left_join(
    ride_hierarchical_cluster
  ) |> 
  # have cluster as a factor
  mutate(
    cluster = factor(cluster)
  ) |> 
  # plot
  ggplot(
    aes(
      # x axis
      mean_efficiency,
      # y axis
      reorder(ride_name, mean_efficiency),
      # color
      fill = cluster
    )
  ) +
  # geom column
  geom_col(
    # color
    color = "black"
  ) +
  # scale color
  scale_fill_manual(
    values = c("#3136c6", "#ad41df", "#C4CED4")
  ) +
  # labels
  labs(
    x = "Average Wait Time / Ride Duration (Efficiency)",
    y = "Ride Name",
    fill = "Cluster",
    title = "Ride Efficiency Rankings"
  ) +
  # custom theme
  disney_theme()





# Ride Efficiency with Primary Hours --------------------------------------


## calculate ride efficiency
wait_times_clean |> 
  # mutate
  mutate(
    # hour of day
    hour_of_day = hour(datetime)
  ) |> 
  filter(
    # only use wait times that make sense
    posted_wait >= 0 & posted_wait <= 400 |
    actual_wait >= 0 & actual_wait <= 400,
    
    # only use primary open hours
    hour_of_day >= 8 & hour_of_day <= 23
  ) |> 
  mutate(
    # efficiency
    posted_wait_efficiency = posted_wait / ride_duration,
    actual_wait_efficiency = actual_wait / ride_duration
  ) |> 
  # group by 
  group_by(ride_name) |> 
  # summarize
  summarize(
    # average ride efficiency
    mean_posted_wait_efficiency = mean(posted_wait_efficiency, na.rm = TRUE),
    mean_actual_wait_efficiency = mean(actual_wait_efficiency, na.rm = TRUE)
  ) |> 
  # calculate difference between the two
  mutate(
    difference = mean_posted_wait_efficiency - mean_actual_wait_efficiency
  ) |> 
  # arrange by mean efficiency
  arrange(-mean_posted_wait_efficiency)



## calculate ride efficiency
wait_times_clean |> 
  # mutate
  mutate(
    # hour of day
    hour_of_day = hour(datetime)
  ) |> 
  filter(
    # only use wait times that make sense
    posted_wait >= 0 & posted_wait <= 400 |
      actual_wait >= 0 & actual_wait <= 400,
    
    # only use primary open hours
    hour_of_day >= 8 & hour_of_day <= 23
  ) |> 
  mutate(
    # efficiency
    posted_wait_efficiency = posted_wait / ride_duration,
    actual_wait_efficiency = actual_wait / ride_duration
  ) |> 
  # group by 
  group_by(ride_name) |> 
  # summarize
  summarize(
    # average ride efficiency
    mean_posted_wait_efficiency = mean(posted_wait_efficiency, na.rm = TRUE),
    mean_actual_wait_efficiency = mean(actual_wait_efficiency, na.rm = TRUE)
  ) |> 
  # left join with clusters
  left_join(
    ride_hierarchical_cluster
  ) |> 
  # calculate difference between the two
  mutate(
    difference = mean_actual_wait_efficiency - mean_posted_wait_efficiency,
    
    cluster = factor(cluster)
  ) |> 
  # arrange by mean efficiency
  arrange(-mean_posted_wait_efficiency) |> 
  # plot
  ggplot(
    aes(
      # x axis
      difference,
      # y axis
      reorder(ride_name, difference),
      # color
      fill = cluster
    )
  ) +
  # geom column
  geom_col(
    # color
    color = "black"
  ) +
  # scale color
  scale_fill_manual(
    values = c("#3136c6", "#ad41df", "#C4CED4")
  ) +
  # labels
  labs(
    x = "Difference Between Actual and Posted Wait Times",
    y = "Ride Name",
    fill = "Cluster",
    title = "Walt Disney Overestimates All Wait Times"
  ) +
  # custom theme
  disney_theme()


## use mean posted wait efficiency
## calculate ride efficiency
ride_efficiency <- wait_times_clean |> 
  mutate(
    # efficiency
    efficiency = posted_wait / ride_duration,
    
    # hour of day
    hour_of_day = hour(datetime)
  ) |> 
  # filter
  filter(
    # only use wait times that make sense
    posted_wait >= 0 & posted_wait <= 400,
    
    # only use primary open hours
    hour_of_day >= 8 & hour_of_day <= 23
  ) |> 
  # group by 
  group_by(ride_name) |> 
  # summarize
  summarize(
    # average ride efficiency
    mean_efficiency = mean(efficiency, na.rm = TRUE),
    
    # average posted wait
    mean_posted_wait = mean(posted_wait, na.rm = TRUE),
    
    # duration
    ride_duration = first(ride_duration)
  ) |> 
  # arrange by mean efficiency
  arrange(-mean_efficiency)




# plot ride efficiency with heirarchical clusters
ride_efficiency |> 
  # left join with clusters
  left_join(
    ride_hierarchical_cluster
  ) |> 
  # have cluster as a factor
  mutate(
    cluster = factor(cluster)
  ) |> 
  # plot
  ggplot(
    aes(
      # x axis
      mean_efficiency,
      # y axis
      reorder(ride_name, mean_efficiency),
      # color
      fill = cluster
    )
  ) +
  # geom column
  geom_col(
    # color
    color = "black"
  ) +
  # scale color
  scale_fill_manual(
    values = c("#3136c6", "#ad41df", "#C4CED4")
  ) +
  # labels
  labs(
    x = "Average Wait Time / Ride Duration (Efficiency)",
    y = "Ride Name",
    fill = "Cluster",
    title = "Ride Efficiency Rankings"
  ) +
  # custom theme
  disney_theme()




# Look into Metadata ------------------------------------------------------

wait_times_full <- {
  ## join with metadata
  wait_times_clean |> 
    # join with metadata
    left_join(
      metadata |> mutate(DATE = mdy(DATE)),
      by = c("date" = "DATE")
    )
}




# Season ------------------------------------------------------------------


## weather season
wait_times_full |> 
  # mutate
  mutate(
    # hour of day
    hour_of_day = hour(datetime)
  ) |> 
  # filter
  filter(
    SEASON %in% c("SPRING", "SUMMER BREAK", "FALL", "WINTER"),
    
    # only use wait times that make sense
    posted_wait >= 0 & posted_wait <= 400 |
    actual_wait >= 0 & actual_wait <= 400,
    
    # only use primary open hours
    hour_of_day >= 8 & hour_of_day <= 23
  ) |> 
  ggplot(
    aes(
      # x axis
      posted_wait,
      # y axis
      SEASON,
      # color
      color = SEASON
    )
  ) +
  # boxplot
  geom_boxplot() +
  # labels
  labs(
    x = "Posted Wait Times",
    y = "Season",
    title = "Disney Wait Times for Season"
  ) +
  # custom theme
  disney_theme()


## weather season
wait_times_full |> 
  # mutate
  mutate(
    # hour of day
    hour_of_day = hour(datetime)
  ) |> 
  # filter
  filter(
    SEASON %in% c("SPRING", "SUMMER BREAK", "FALL", "WINTER"),
    
    # only use wait times that make sense
    posted_wait >= 0 & posted_wait <= 400 |
    actual_wait >= 0 & actual_wait <= 400,
    
    # only use primary open hours
    hour_of_day >= 8 & hour_of_day <= 23
  ) |> 
  # group by
  group_by(
    SEASON,
  ) |> 
  # average wait time
  summarize(
    # average posted wait
    mean_posted_wait = mean(posted_wait, na.rm = TRUE),
  ) |> 
  ggplot(
    aes(
      # x axis
      mean_posted_wait,
      # y axis
      SEASON,
      # color
      fill = SEASON
    )
  ) +
  # boxplot
  geom_col() +
  # labels
  labs(
    x = "Posted Wait Times",
    y = "Season",
    title = "Disney Wait Times for Season"
  ) +
  # custom theme
  disney_theme()



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
    hour_of_day >= 8 & hour_of_day <= 23,
    
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
    mean_posted_wait = mean(posted_wait, na.rm = TRUE),
  ) |> 
  ggplot(
    aes(
      # x axis
      mean_posted_wait,
      # y axis
      fct_reorder(SEASON, mean_posted_wait),
      # color
      fill = SEASON
    )
  ) +
  # boxplot
  geom_col() +
  # labels
  labs(
    x = "Posted Wait Times",
    y = "Season",
    title = "Disney Wait Times for Season"
  ) +
  # custom theme
  disney_theme() +
  # remove legend
  theme(
    legend.position = "none"
  )


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
    hour_of_day >= 8 & hour_of_day <= 22,
    
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
    mean_posted_wait = mean(posted_wait, na.rm = TRUE),
  ) |> 
  # filter
  filter(
    mean_posted_wait >= 30 | mean_posted_wait <= 25
  ) |> 
  ggplot(
    aes(
      # x axis
      mean_posted_wait,
      # y axis
      fct_reorder(SEASON, mean_posted_wait),
      # color
      fill = SEASON
    )
  ) +
  # boxplot
  geom_col() +
  # labels
  labs(
    x = "Posted Wait Times",
    y = "Season",
    title = "Disney Wait Times for Season",
    caption = "Top 7 and Bottom 2 Seasons"
  ) +
  # custom theme
  disney_theme() +
  # remove legend
  theme(
    legend.position = "none"
  )




# Day of Week -------------------------------------------------------------

## weather season
wait_times_full |> 
  # mutate
  mutate(
    # hour of day
    hour_of_day = hour(datetime),
    
    # convert to factor
    DAYOFWEEK = factor(
      DAYOFWEEK
    )
  ) |> 
  # filter
  filter(
    # only use wait times that make sense
    posted_wait >= 0 & posted_wait <= 400 |
      actual_wait >= 0 & actual_wait <= 400,
    
    # only use primary open hours
    hour_of_day >= 8 & hour_of_day <= 22,
    
    # remove NA day of week
    !(is.na(DAYOFWEEK))
  ) |> 
  ggplot(
    aes(
      # x axis
      posted_wait,
      # y axis
      DAYOFWEEK,
      # color
      color = DAYOFWEEK
    )
  ) +
  # boxplot
  geom_boxplot() +
  # labels
  labs(
    x = "Posted Wait Times",
    y = "Day of Week",
    title = "Disney Wait Times for Day of Week"
  ) +
  # custom theme
  disney_theme()



## season
wait_times_full |> 
  # mutate
  mutate(
    # hour of day
    hour_of_day = hour(datetime),
    
    # convert to factor
    DAYOFWEEK = factor(
      DAYOFWEEK
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
    hour_of_day >= 8 & hour_of_day <= 22,
    
    # remove NA season
    !(is.na(DAYOFWEEK))
  ) |> 
  # group by
  group_by(
    DAYOFWEEK,
  ) |> 
  # average wait time
  summarize(
    # average posted wait
    mean_posted_wait = mean(posted_wait, na.rm = TRUE),
  ) |> 
  ggplot(
    aes(
      # x axis
      mean_posted_wait,
      # y axis
      fct_reorder(DAYOFWEEK, mean_posted_wait),
      # color
      fill = DAYOFWEEK
    )
  ) +
  # boxplot
  geom_col() +
  # labels
  labs(
    x = "Posted Wait Times",
    y = "Day of Week",
    title = "Disney Wait Times for Day of Week"
  ) +
  # custom theme
  disney_theme() +
  # remove legend
  theme(
    legend.position = "none"
  )




# Month -------------------------------------------------------------------

## season
wait_times_full |> 
  # mutate
  mutate(
    # hour of day
    hour_of_day = hour(datetime),
    
    # convert to factor
    MONTHOFYEAR = factor(
      MONTHOFYEAR
    )
  ) |> 
  # filter
  filter(
    # only use wait times that make sense
    posted_wait >= 0 & posted_wait <= 400 |
      actual_wait >= 0 & actual_wait <= 400,
    
    # only use primary open hours
    hour_of_day >= 8 & hour_of_day <= 22,
    
    # remove NA season
    !(is.na(MONTHOFYEAR))
  ) |> 
  # group by
  group_by(
    MONTHOFYEAR,
  ) |> 
  # average wait time
  summarize(
    # average posted wait
    mean_posted_wait = mean(posted_wait, na.rm = TRUE),
  ) |> 
  ggplot(
    aes(
      # x axis
      mean_posted_wait,
      # y axis
      fct_reorder(MONTHOFYEAR, mean_posted_wait),
      # color
      fill = MONTHOFYEAR
    )
  ) +
  # boxplot
  geom_col() +
  # labels
  labs(
    x = "Posted Wait Times",
    y = "Month",
    title = "Disney Wait Times for Month"
  ) +
  # custom theme
  disney_theme() +
  # remove legend
  theme(
    legend.position = "none"
  )



## season
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
    
    # only use primary open hours
    hour_of_day >= 8 & hour_of_day <= 22,
    
    # remove NA season
    !(is.na(MONTHOFYEAR))
  ) |> 
  # group by
  group_by(
    YEAR,
    MONTHOFYEAR
  ) |> 
  # average wait time
  summarize(
    # average posted wait
    mean_posted_wait = mean(posted_wait, na.rm = TRUE),
  ) |> 
  ggplot(
    aes(
      # x axis
      MONTHOFYEAR,
      # y axis
      mean_posted_wait,
      # color
      color = YEAR
    )
  ) +
  # line plot
  geom_line() +
  # points
  geom_point() +
  # scale x axis
  scale_x_continuous(
    breaks = seq(1, 12, 1)
  ) +
  # labels
  labs(
    x = "Month",
    y = "Posted Wait Times",
    color = "Year",
    title = "Disney Wait Times by Month"
  ) +
  # custom theme
  disney_theme()



# Mean Temperature --------------------------------------------------------

## season
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
    
    # only use primary open hours
    hour_of_day >= 8 & hour_of_day <= 22
  ) |> 
  # group by
  group_by(
    date
  ) |> 
  # average wait time
  summarize(
    # average posted wait
    mean_posted_wait = mean(posted_wait, na.rm = TRUE),
    
    # average temperature
    mean_temp = mean(WDWMEANTEMP, na.rm = TRUE)
  ) |> 
  # mutate
  mutate(
    YEAR = factor(year(date))
  ) |> 
  ggplot(
    aes(
      # x axis
      mean_temp,
      # y axis
      mean_posted_wait,
      # color
      color = YEAR
    )
  ) +
  # points
  geom_point(
    alpha = 0.5,
    size = 2
  ) +
  # labels
  labs(
    x = "Mean Temperature (in Fahrenheit)",
    y = "Posted Wait Times",
    color = "Year",
    title = "Disney Wait Times by Temperature"
  ) +
  # custom theme
  disney_theme()



# Schools in Session --------------------------------------------------------

## season
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
    
    # only use primary open hours
    hour_of_day >= 8 & hour_of_day <= 22
  ) |> 
  # group by
  group_by(
    date
  ) |> 
  # average wait time
  summarize(
    # average posted wait
    mean_posted_wait = mean(posted_wait, na.rm = TRUE),
    
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
      mean_posted_wait,
      # color
      color = YEAR
    )
  ) +
  # points
  geom_point(
    alpha = 0.5,
    size = 2
  ) +
  # labels
  labs(
    x = "% of Schools in Session",
    y = "Posted Wait Times",
    color = "Year",
    title = "Disney Wait Times by Temperature"
  ) +
  # custom theme
  disney_theme()



# General Wait Times Throughout the Day -----------------------------------

## season
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
    
    # only use primary open hours
    hour_of_day >= 8 & hour_of_day <= 22
  ) |> 
  # group by
  group_by(
    hour_of_day
  ) |> 
  # average wait time
  summarize(
    # average posted wait
    mean_posted_wait = mean(posted_wait, na.rm = TRUE)
  ) |> 
  # mutate
  mutate(
    YEAR = factor(year(date))
  ) |> 
  ggplot(
    aes(
      # x axis
      hour_of_day,
      # y axis
      mean_posted_wait,
      # color
      color = YEAR
    )
  ) +
  # geom line
  geom_line() +
  # points
  geom_point(
    alpha = 0.75,
    size = 2
  ) +
  # average sunset time
  geom_vline(
    xintercept = mean(wait_times_full |> group_by(date) |> summarize(mean_sunset_time = first(SUNSET_WDW)))
  ) +
  # labels
  labs(
    x = "% of Schools in Session",
    y = "Posted Wait Times",
    color = "Year",
    title = "Disney Wait Times by Temperature"
  ) +
  # custom theme
  disney_theme()
  

# Day of Week -------------------------------------------------------------

## season
wait_times_full |> 
  # mutate
  mutate(
    # hour of day
    hour_of_day = hour(datetime),
    
    # convert to factor
    weekday = factor(
      if_else(
        # condition
        DAYOFWEEK %in% c(1:4),
        # weekday
        "weekday",
        # weekend
        "weekend"
      )
    ),
    
    # convert to a factor
    YEAR = factor(YEAR)
  ) |> 
  # filter
  filter(
    # only use wait times that make sense
    posted_wait >= 0 & posted_wait <= 400 |
      actual_wait >= 0 & actual_wait <= 400,
    
    # only use primary open hours
    hour_of_day >= 8 & hour_of_day <= 22,
    
    # remove NA season
    !(is.na(weekday)),
    
    # remove NA season
    !(is.na(YEAR))
  ) |> 
  # group by
  group_by(
    YEAR,
    weekday
  ) |> 
  # average wait time
  summarize(
    # average posted wait
    mean_posted_wait = mean(posted_wait, na.rm = TRUE),
  ) |> 
  ggplot(
    aes(
      # x axis
      YEAR,
      # y axis
      mean_posted_wait,
      # color
      fill = weekday
    )
  ) +
  # boxplot
  geom_col() +
  # labels
  labs(
    x = "Year",
    y = "Posted Wait Time",
    title = "Disney Wait Times for Day of Week"
  ) +
  # facet wrap
  facet_wrap(
    ~ weekday
  ) +
  # custom theme
  disney_theme()
  
  
  
  

# One Ride Mean Wait Time by Year -----------------------------------------

## wait time over the day for one ride
wait_times_full |> 
  # mutate
  mutate(
    # hour of day
    hour_of_day = hour(datetime)
  ) |> 
  # 7 Dwarfs Train
  filter(
    ride_name == "Avatar Flight of Passage",
    # only have positive wait times
    posted_wait >= 0 & posted_wait <= 400,
    
    hour_of_day >= 8 & hour_of_day <= 22
  ) |>
  # group by
  group_by(date) |> 
  summarize(
    # mean wait time
    mean_posted_wait = mean(posted_wait, na.rm = TRUE)
  ) |> 
  # mutate
  mutate(
    year = factor(year(date)),
    
    month_day = format(date, "%m-%d")
  ) |> 
  ggplot(
    aes(
      # x axis
      month_day,
      # y axis
      mean_posted_wait,
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
  scale_x_discrete(
    breaks = paste0(sprintf("%02d", 1:12), "-01")
  ) +
  # labels
  labs(
    x = "Date",
    y = "Average Posted Wait (min)",
    color = "Year",
    title = "Wait Times for Avatar"
  ) +
  # custom theme
  disney_theme()
