## Liam Jennings
## Honors Seminar: Disney


# Getting Started ---------------------------------------------------------

## libraries
library(tidyverse)
library(ggdendro)



# Prepare Data ------------------------------------------------------------

## set seed
set.seed(8029)

## prepare the data
ride_daily_waits <- {
  wait_times_full |> 
    # extract one hour of the data
    mutate(
      hour_of_day = hour(datetime)
    ) |> 
    filter(
      # only use posted wait times that make sense
      actual_wait >= 0 & actual_wait <= 400,
      
      # prime hours
      hour_of_day >= 8 & hour_of_day <= 21,
      
      # pre COVID-19
      YEAR %in% c(2015:2019)
    ) |> 
    # group by ride and hour
    group_by(
      ride_name,
      hour_of_day
    ) |> 
    # find average wait time
    summarize(
      mean_actual_wait = mean(actual_wait, na.rm = TRUE)
    ) |> 
    # ungroup
    ungroup()
}


## transform data into a matrix (wide format)
ride_matrix <- ride_daily_waits |> 
  spread(
    key = hour_of_day,
    value = mean_actual_wait,
    fill = NA
  )


## scale data
ride_matrix_scaled <- scale(
  # remove name column before scaling
  ride_matrix |> select(-1)
)

## replace NA values with 0
ride_matrix_scaled[is.na(ride_matrix_scaled)] <- 0




# Hierarchical Clustering -------------------------------------------------

## convert to distance and only keep 8am through 9 pm
rides_hierachical_clustering <- ride_matrix_scaled |> 
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
    y = "Distance between clusters",
    title = "'Cut' at y = 5 Gives 3 Clusters"
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
  ggplot(
    aes(
      # x axis
      mean_actual_wait,
      # y axis
      fct_rev(cluster),
      # color
      fill = cluster
    )
  ) +
  # boxplot
  geom_boxplot() +
  # scale color
  scale_fill_manual(
    values = c("#3136c6", "#ad41df", "#1a9bcb")
  ) +
  # labels
  labs(
    x = "Average Wait Time (min)",
    y = "Cluster",
    color = "Cluster",
    title = "Clustered Ride Wait Times"
  ) +
  # custome theme
  disney_theme() +
  # remove legend
  theme(
    legend.position = "none"
  )


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
  ggplot(
    aes(
      # x axis
      hour_of_day,
      # y axis
      mean_actual_wait,
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
    avg_actual_wait = mean(mean_actual_wait, na.rm = TRUE),
    sd_actual_wait = sd(mean_actual_wait, na.rm = TRUE),
    number_of_rides = n_distinct(ride_name)
  )


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
    ride_name == "Spaceship Earth"
  ) |> 
  ggplot(
    aes(
      # x axis
      hour_of_day,
      # y axis
      mean_actual_wait,
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
  # scale color
  scale_color_manual(
    values = c("#3136c6", "#ad41df", "#C4CED4", "black")
  ) +
  # scale x axis
  scale_x_continuous(
    breaks = seq(8, 21, 1),
    labels = c(
      8:12,
      1:9
    )
  ) +
  # labels
  labs(
    x = "Hour of Day",
    y = "Average Wait Time (min)",
    color = "Cluster",
    title = "Spaceship Earth"
  ) +
  # custome theme
  disney_theme() +
  # remove legend
  theme(
    legend.position = "none"
  )


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
    ride_name == "Avatar Flight of Passage"
  ) |> 
  ggplot(
    aes(
      # x axis
      hour_of_day,
      # y axis
      mean_actual_wait,
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
  # scale color
  scale_color_manual(
    values = c("#ad41df", "#C4CED4", "black")
  ) +
  # scale x axis
  scale_x_continuous(
    breaks = seq(8, 21, 1),
    labels = c(
      8:12,
      1:9
    )
  ) +
  # labels
  labs(
    x = "Hour of Day",
    y = "Average Wait Time (min)",
    color = "Cluster",
    title = "Avatar Flight of Passage"
  ) +
  # custome theme
  disney_theme() +
  # remove legend
  theme(
    legend.position = "none"
  )


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
    ride_name == "Seven Dwarfs Mine Train"
  ) |> 
  ggplot(
    aes(
      # x axis
      hour_of_day,
      # y axis
      mean_actual_wait,
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
  # scale color
  scale_color_manual(
    values = c("#1a9bcb", "#C4CED4", "black")
  ) +
  # scale x axis
  scale_x_continuous(
    breaks = seq(8, 21, 1),
    labels = c(
      8:12,
      1:9
    )
  ) +
  # labels
  labs(
    x = "Hour of Day",
    y = "Average Wait Time (min)",
    color = "Cluster",
    title = "Seven Dwarfs Mine Train"
  ) +
  # custome theme
  disney_theme() +
  # remove legend
  theme(
    legend.position = "none"
  )


# Ride Efficiency Rankings ------------------------------------------------

## calculate ride efficiency
wait_times_full |> 
  # extract one hour of the data
  mutate(
    hour_of_day = hour(datetime)
  ) |> 
  # only use wait times that make sense
  filter(
    # only use posted wait times that make sense
    posted_wait >= 0 & posted_wait <= 400 |
      actual_wait >= 0 & actual_wait <= 400,
    
    # prime hours
    hour_of_day >= 8 & hour_of_day <= 21,
    
    # pre COVID-19
    YEAR %in% c(2015:2019)
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
ride_efficiency <- wait_times_full |> 
  # extract one hour of the data
  mutate(
    hour_of_day = hour(datetime)
  ) |> 
  # only use wait times that make sense
  filter(
    # only use posted wait times that make sense
    posted_wait >= 0 & posted_wait <= 400 |
      actual_wait >= 0 & actual_wait <= 400,
    
    # prime hours
    hour_of_day >= 8 & hour_of_day <= 21,
    
    # pre COVID-19
    YEAR %in% c(2015:2019)
  ) |> 
  mutate(
    # efficiency
    efficiency = actual_wait / ride_duration
  ) |> 
  # group by 
  group_by(ride_name) |> 
  # summarize
  summarize(
    # average ride efficiency
    mean_efficiency = mean(efficiency, na.rm = TRUE),
    
    # average posted wait
    mean_actual_wait = mean(actual_wait, na.rm = TRUE),
    
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
    values = c("#3136c6", "#ad41df", "#1a9bcb")
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
