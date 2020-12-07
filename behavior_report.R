# Creates a report of all behavioral measures
## maximilian.harkotte@gmail.com - December 2020

rm(list = ls()) # clear workspace
cat("\014") # clear console


# 0 - Load packages -------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(psych)
library(ggsignif)

# 1 - Source file ---------------------------------------------------------
dataPath <-
  "Y:/Max/OPTO_Control/Analyses/" # Make sure this path directs to the .csv files
setwd(dataPath)

# 2 - Read in data --------------------------------------------------------

hab <-
  read.csv2("100-MPV0120-Habituation_clean.csv", stringsAsFactors = FALSE)

# coerce all values of interest into numeric
for (i in 8:length(hab)) {
  hab[, i]  <- as.numeric(hab[, i])
}

hab$Animal <- as.factor(hab$Animal)
hab$Habituation <- as.factor(hab$Habituation)
hab$Entry <- as.factor(hab$Entry)

# Exclude animals from analysis
hab <-
  subset(hab, !(Animal %in% c())) # exclude animals from analysis

# 4 - Plotting ------------------------------------------------------------
# Add aestetics
limits = aes(ymax = mean + (se), ymin = mean - (se)) # (1.96*se) for confidence intervals
dodge = position_dodge(width = 0.8)


# Habituation -------------------------------------------------------------

## 1. Distance travelled

Cum_hab_dist <-
  subset(
    hab,
    select = c(
      "Animal",
      "Habituation",
      "Cum_dist_min_1",
      "Cum_dist_min_2",
      "Cum_dist_min_3",
      "Cum_dist_min_4",
      "Cum_dist_min_5",
      "Cum_dist_min_6",
      "Cum_dist_min_7",
      "Cum_dist_min_8",
      "Cum_dist_min_9",
      "Cum_dist_min_10"
    )
  )

Cum_hab_dist <-
  pivot_longer(
    Cum_hab_dist,
    cols = 3:12 ,
    names_to = "Minute",
    values_to = "dist"
  )

Cum_hab_dist$Minute <- as.factor(Cum_hab_dist$Minute)

Cum_hab_dist_summary = describeBy(
  Cum_hab_dist$dist,
  list(Cum_hab_dist$Habituation, Cum_hab_dist$Minute),
  mat = TRUE,
  digits = 2
)

Cum_hab_dist_plot <-
  ggplot(Cum_hab_dist_summary,
         aes(x = group2, y = mean, fill = group1)) +
  geom_bar(stat = 'identity',
           position = dodge,
           width = .8) +
  geom_errorbar(limits, position = dodge, width = 0.3) +
  theme_minimal() +
  scale_y_continuous(name = "Distance travelled [m]",
                     breaks = seq(0, 60, 5),
                     limits = c(0, 60)) +
  scale_x_discrete(
    name = "Minute",
    labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
    limits = c(
      "Cum_dist_min_1",
      "Cum_dist_min_2",
      "Cum_dist_min_3",
      "Cum_dist_min_4",
      "Cum_dist_min_5",
      "Cum_dist_min_6",
      "Cum_dist_min_7",
      "Cum_dist_min_8",
      "Cum_dist_min_9",
      "Cum_dist_min_10"
    )
  ) +
  scale_fill_grey(
    name = "Habituation",
    labels = c(
      paste0("1 (N=", mean(Cum_hab_dist_summary$n), ")"),
      paste0("2 (N=", mean(Cum_hab_dist_summary$n), ")"),
      paste0("3 (N=", mean(Cum_hab_dist_summary$n), ")")
    ),
    limits = c("Hab1", "Hab2", "Hab3")
  ) +
  ggtitle("Habituation | Distance travelled (cumulative)") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Bin_hab_dist <-
  subset(
    hab,
    select = c(
      "Animal",
      "Habituation",
      "Bin_dist_min_1",
      "Bin_dist_min_2",
      "Bin_dist_min_3",
      "Bin_dist_min_4",
      "Bin_dist_min_5",
      "Bin_dist_min_6",
      "Bin_dist_min_7",
      "Bin_dist_min_8",
      "Bin_dist_min_9",
      "Bin_dist_min_10"
    )
  )

Bin_hab_dist <-
  pivot_longer(
    Bin_hab_dist,
    cols = 3:12 ,
    names_to = "Minute",
    values_to = "dist"
  )

Bin_hab_dist$Minute <- as.factor(Bin_hab_dist$Minute)

Bin_hab_dist_summary = describeBy(
  Bin_hab_dist$dist,
  list(Bin_hab_dist$Habituation, Bin_hab_dist$Minute),
  mat = TRUE,
  digits = 2
)

Bin_hab_dist_plot <-
  ggplot(Bin_hab_dist_summary,
         aes(x = group2, y = mean, fill = group1)) +
  geom_bar(stat = 'identity',
           position = dodge,
           width = .8) +
  geom_errorbar(limits, position = dodge, width = 0.3) +
  theme_minimal() +
  scale_y_continuous(name = "Distance travelled [m]",
                     breaks = seq(0, 15, 3),
                     limits = c(0, 15)) +
  scale_x_discrete(
    name = "Minute",
    labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
    limits = c(
      "Bin_dist_min_1",
      "Bin_dist_min_2",
      "Bin_dist_min_3",
      "Bin_dist_min_4",
      "Bin_dist_min_5",
      "Bin_dist_min_6",
      "Bin_dist_min_7",
      "Bin_dist_min_8",
      "Bin_dist_min_9",
      "Bin_dist_min_10"
    )
  ) +
  scale_fill_grey(
    name = "Habituation",
    labels = c(
      paste0("1 (N=", mean(Bin_hab_dist_summary$n), ")"),
      paste0("2 (N=", mean(Bin_hab_dist_summary$n), ")"),
      paste0("3 (N=", mean(Bin_hab_dist_summary$n), ")")
    ),
    limits = c("Hab1" , "Hab2", "Hab3")
  ) +
  ggtitle("Habituation | Distance travelled (non-cumulative)") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Cum_hab_dist_plot
Bin_hab_dist_plot

## 2. Velocity/ Mean speed
Cum_hab_velo <-
  subset(
    hab,
    select = c(
      "Animal",
      "Habituation",
      "Cum_velo_min_1",
      "Cum_velo_min_2",
      "Cum_velo_min_3",
      "Cum_velo_min_4",
      "Cum_velo_min_5",
      "Cum_velo_min_6",
      "Cum_velo_min_7",
      "Cum_velo_min_8",
      "Cum_velo_min_9",
      "Cum_velo_min_10"
    )
  )

Cum_hab_velo <-
  pivot_longer(
    Cum_hab_velo,
    cols = 3:12 ,
    names_to = "Minute",
    values_to = "Speed"
  )

Cum_hab_veloMinute <- as.factor(Cum_hab_velo$Minute)

Cum_hab_velo_summary = describeBy(
  Cum_hab_velo$Speed,
  list(Cum_hab_velo$Habituation, Cum_hab_velo$Minute),
  mat = TRUE,
  digits = 2
)

Cum_hab_velo_plot <-
  ggplot(Cum_hab_velo_summary,
         aes(x = group2, y = mean, fill = group1)) +
  geom_bar(stat = 'identity',
           position = dodge,
           width = .8) +
  geom_errorbar(limits, position = dodge, width = 0.3) +
  theme_minimal() +
  scale_y_continuous(name = "Mean speed [m/s]",
                     breaks = seq(0, .3, .1),
                     limits = c(0, .3)) +
  scale_x_discrete(
    name = "Minute",
    labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
    limits = c(
      "Cum_velo_min_1",
      "Cum_velo_min_2",
      "Cum_velo_min_3",
      "Cum_velo_min_4",
      "Cum_velo_min_5",
      "Cum_velo_min_6",
      "Cum_velo_min_7",
      "Cum_velo_min_8",
      "Cum_velo_min_9",
      "Cum_velo_min_10"
    )
  ) +
  scale_fill_grey(
    name = "Habituation",
    labels = c(
      paste0("1 (N=", mean(Cum_hab_velo_summary$n), ")"),
      paste0("2 (N=", mean(Cum_hab_velo_summary$n), ")"),
      paste0("3 (N=", mean(Cum_hab_velo_summary$n), ")")
    ),
    limits = c("Hab1", "Hab2", "Hab3")
  ) +
  ggtitle("Habituation | Mean speed (cumulative)") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Bin_hab_velo <-
  subset(
    hab,
    select = c(
      "Animal",
      "Habituation",
      "Bin_velo_min_1",
      "Bin_velo_min_2",
      "Bin_velo_min_3",
      "Bin_velo_min_4",
      "Bin_velo_min_5",
      "Bin_velo_min_6",
      "Bin_velo_min_7",
      "Bin_velo_min_8",
      "Bin_velo_min_9",
      "Bin_velo_min_10"
    )
  )

Bin_hab_velo <-
  pivot_longer(
    Bin_hab_velo,
    cols = 3:12 ,
    names_to = "Minute",
    values_to = "Speed"
  )

Bin_hab_veloMinute <- as.factor(Bin_hab_velo$Minute)

Bin_hab_velo_summary = describeBy(
  Bin_hab_velo$Speed,
  list(Bin_hab_velo$Habituation, Bin_hab_velo$Minute),
  mat = TRUE,
  digits = 2
)

Bin_hab_velo_plot <-
  ggplot(Bin_hab_velo_summary,
         aes(x = group2, y = mean, fill = group1)) +
  geom_bar(stat = 'identity',
           position = dodge,
           width = .8) +
  geom_errorbar(limits, position = dodge, width = 0.3) +
  theme_minimal() +
  scale_y_continuous(name = "Mean speed [m/s]",
                     breaks = seq(0, .3, .1),
                     limits = c(0, .3)) +
  scale_x_discrete(
    name = "Minute",
    labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
    limits = c(
      "Bin_velo_min_1",
      "Bin_velo_min_2",
      "Bin_velo_min_3",
      "Bin_velo_min_4",
      "Bin_velo_min_5",
      "Bin_velo_min_6",
      "Bin_velo_min_7",
      "Bin_velo_min_8",
      "Bin_velo_min_9",
      "Bin_velo_min_10"
    )
  ) +
  scale_fill_grey(
    name = "Habituation",
    labels = c(
      paste0("1 (N=", mean(Bin_hab_velo_summary$n), ")"),
      paste0("2 (N=", mean(Bin_hab_velo_summary$n), ")"),
      paste0("3 (N=", mean(Bin_hab_velo_summary$n), ")")
    ),
    limits = c("Hab1", "Hab2", "Hab3")
  ) +
  ggtitle("Habituation | Mean speed (non-cumulative)") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Cum_hab_velo_plot
Bin_hab_velo_plot

## 3. Time spent rearing 
Cum_hab_rear <-
  subset(
    hab,
    select = c(
      "Animal",
      "Habituation",
      "Cum_rear_tim_1",
      "Cum_rear_tim_2",
      "Cum_rear_tim_3",
      "Cum_rear_tim_4",
      "Cum_rear_tim_5",
      "Cum_rear_tim_6",
      "Cum_rear_tim_7",
      "Cum_rear_tim_8",
      "Cum_rear_tim_9",
      "Cum_rear_tim_10"
    )
  )

Cum_hab_rear <-
  pivot_longer(
    Cum_hab_rear,
    cols = 3:12 ,
    names_to = "Minute",
    values_to = "Rear_Time"
  )

Cum_hab_rear$Minute <- as.factor(Cum_hab_rear$Minute)

Cum_hab_rear_summary = describeBy(
  Cum_hab_rear$Rear_Time,
  list(Cum_hab_rear$Habituation, Cum_hab_rear$Minute),
  mat = TRUE,
  digits = 2
)

Cum_hab_rear_plot <-
  ggplot(Cum_hab_rear_summary,
         aes(x = group2, y = mean, fill = group1)) +
  geom_bar(stat = 'identity',
           position = dodge,
           width = .8) +
  geom_errorbar(limits, position = dodge, width = 0.3) +
  theme_minimal() +
  scale_y_continuous(name = "Time spent rearing [s]",
                     breaks = seq(0, 180, 10),
                     limits = c(0, 180)) +
  scale_x_discrete(
    name = "Minute",
    labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
    limits = c(
      "Cum_rear_tim_1",
      "Cum_rear_tim_2",
      "Cum_rear_tim_3",
      "Cum_rear_tim_4",
      "Cum_rear_tim_5",
      "Cum_rear_tim_6",
      "Cum_rear_tim_7",
      "Cum_rear_tim_8",
      "Cum_rear_tim_9",
      "Cum_rear_tim_10"
    )
  ) +
  scale_fill_grey(
    name = "Habituation",
    labels = c(
      paste0("1 (N=", mean(Cum_hab_dist_summary$n), ")"),
      paste0("2 (N=", mean(Cum_hab_dist_summary$n), ")"),
      paste0("3 (N=", mean(Cum_hab_dist_summary$n), ")")
    ),
    limits = c("Hab1", "Hab2", "Hab3")
  ) +
  ggtitle("Habituation | Time spent rearing (cumulative)") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Bin_hab_rear <-
  subset(
    hab,
    select = c(
      "Animal",
      "Habituation",
      "Bin_rear_tim_1",
      "Bin_rear_tim_2",
      "Bin_rear_tim_3",
      "Bin_rear_tim_4",
      "Bin_rear_tim_5",
      "Bin_rear_tim_6",
      "Bin_rear_tim_7",
      "Bin_rear_tim_8",
      "Bin_rear_tim_9",
      "Bin_rear_tim_10"
    )
  )

Bin_hab_rear <-
  pivot_longer(
    Bin_hab_rear,
    cols = 3:12 ,
    names_to = "Minute",
    values_to = "Rear_Time"
  )

Bin_hab_rear$Minute <- as.factor(Bin_hab_rear$Minute)

Bin_hab_rear_summary = describeBy(
  Bin_hab_rear$Rear_Time,
  list(Bin_hab_rear$Habituation, Bin_hab_rear$Minute),
  mat = TRUE,
  digits = 2
)

Bin_hab_rear_plot <-
  ggplot(Bin_hab_rear_summary,
         aes(x = group2, y = mean, fill = group1)) +
  geom_bar(stat = 'identity',
           position = dodge,
           width = .8) +
  geom_errorbar(limits, position = dodge, width = 0.3) +
  theme_minimal() +
  scale_y_continuous(name = "Time spent rearing [s]",
                     breaks = seq(0, 30, 5),
                     limits = c(0, 30)) +
  scale_x_discrete(
    name = "Minute",
    labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
    limits = c(
      "Bin_rear_tim_1",
      "Bin_rear_tim_2",
      "Bin_rear_tim_3",
      "Bin_rear_tim_4",
      "Bin_rear_tim_5",
      "Bin_rear_tim_6",
      "Bin_rear_tim_7",
      "Bin_rear_tim_8",
      "Bin_rear_tim_9",
      "Bin_rear_tim_10"
    )
  ) +
  scale_fill_grey(
    name = "Habituation",
    labels = c(
      paste0("1 (N=", mean(Bin_hab_dist_summary$n), ")"),
      paste0("2 (N=", mean(Bin_hab_dist_summary$n), ")"),
      paste0("3 (N=", mean(Bin_hab_dist_summary$n), ")")
    ),
    limits = c("Hab1", "Hab2", "Hab3")
  ) +
  ggtitle("Habituation | Time spent rearing (non-cumulative)") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Cum_hab_rear_plot
Bin_hab_rear_plot

## 4. Time spent grooming 
Cum_hab_groo <-
  subset(
    hab,
    select = c(
      "Animal",
      "Habituation",
      "Cum_groo_tim_1",
      "Cum_groo_tim_2",
      "Cum_groo_tim_3",
      "Cum_groo_tim_4",
      "Cum_groo_tim_5",
      "Cum_groo_tim_6",
      "Cum_groo_tim_7",
      "Cum_groo_tim_8",
      "Cum_groo_tim_9",
      "Cum_groo_tim_10"
    )
  )

Cum_hab_groo <-
  pivot_longer(
    Cum_hab_groo,
    cols = 3:12 ,
    names_to = "Minute",
    values_to = "groo_Time"
  )

Cum_hab_groo$Minute <- as.factor(Cum_hab_groo$Minute)

Cum_hab_groo_summary = describeBy(
  Cum_hab_groo$groo_Time,
  list(Cum_hab_groo$Habituation, Cum_hab_groo$Minute),
  mat = TRUE,
  digits = 2
)

Cum_hab_groo_plot <-
  ggplot(Cum_hab_groo_summary,
         aes(x = group2, y = mean, fill = group1)) +
  geom_bar(stat = 'identity',
           position = dodge,
           width = .8) +
  geom_errorbar(limits, position = dodge, width = 0.3) +
  theme_minimal() +
  scale_y_continuous(name = "Time spent grooming [s]",
                     breaks = seq(0, 130, 10),
                     limits = c(0, 130)) +
  scale_x_discrete(
    name = "Minute",
    labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
    limits = c(
      "Cum_groo_tim_1",
      "Cum_groo_tim_2",
      "Cum_groo_tim_3",
      "Cum_groo_tim_4",
      "Cum_groo_tim_5",
      "Cum_groo_tim_6",
      "Cum_groo_tim_7",
      "Cum_groo_tim_8",
      "Cum_groo_tim_9",
      "Cum_groo_tim_10"
    )
  ) +
  scale_fill_grey(
    name = "Habituation",
    labels = c(
      paste0("1 (N=", mean(Cum_hab_dist_summary$n), ")"),
      paste0("2 (N=", mean(Cum_hab_dist_summary$n), ")"),
      paste0("3 (N=", mean(Cum_hab_dist_summary$n), ")")
    ),
    limits = c("Hab1", "Hab2", "Hab3")
  ) +
  ggtitle("Habituation | Time spent grooming (cumulative)") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Bin_hab_groo <-
  subset(
    hab,
    select = c(
      "Animal",
      "Habituation",
      "Bin_groo_tim_1",
      "Bin_groo_tim_2",
      "Bin_groo_tim_3",
      "Bin_groo_tim_4",
      "Bin_groo_tim_5",
      "Bin_groo_tim_6",
      "Bin_groo_tim_7",
      "Bin_groo_tim_8",
      "Bin_groo_tim_9",
      "Bin_groo_tim_10"
    )
  )

Bin_hab_groo <-
  pivot_longer(
    Bin_hab_groo,
    cols = 3:12 ,
    names_to = "Minute",
    values_to = "groo_Time"
  )

Bin_hab_groo$Minute <- as.factor(Bin_hab_groo$Minute)

Bin_hab_groo_summary = describeBy(
  Bin_hab_groo$groo_Time,
  list(Bin_hab_groo$Habituation, Bin_hab_groo$Minute),
  mat = TRUE,
  digits = 2
)

Bin_hab_groo_plot <-
  ggplot(Bin_hab_groo_summary,
         aes(x = group2, y = mean, fill = group1)) +
  geom_bar(stat = 'identity',
           position = dodge,
           width = .8) +
  geom_errorbar(limits, position = dodge, width = 0.3) +
  theme_minimal() +
  scale_y_continuous(name = "Time spent grooming [s]",
                     breaks = seq(0, 60, 5),
                     limits = c(0, 60)) +
  scale_x_discrete(
    name = "Minute",
    labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
    limits = c(
      "Bin_groo_tim_1",
      "Bin_groo_tim_2",
      "Bin_groo_tim_3",
      "Bin_groo_tim_4",
      "Bin_groo_tim_5",
      "Bin_groo_tim_6",
      "Bin_groo_tim_7",
      "Bin_groo_tim_8",
      "Bin_groo_tim_9",
      "Bin_groo_tim_10"
    )
  ) +
  scale_fill_grey(
    name = "Habituation",
    labels = c(
      paste0("1 (N=", mean(Bin_hab_dist_summary$n), ")"),
      paste0("2 (N=", mean(Bin_hab_dist_summary$n), ")"),
      paste0("3 (N=", mean(Bin_hab_dist_summary$n), ")")
    ),
    limits = c("Hab1", "Hab2", "Hab3")
  ) +
  ggtitle("Habituation | Time spent grooming (non-cumulative)") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Cum_hab_groo_plot
Bin_hab_groo_plot

## 5. Rearing event count 
Cum_hab_rear_count <-
  subset(
    hab,
    select = c(
      "Animal",
      "Habituation",
      "Cum_rear_cnt_1",
      "Cum_rear_cnt_2",
      "Cum_rear_cnt_3",
      "Cum_rear_cnt_4",
      "Cum_rear_cnt_5",
      "Cum_rear_cnt_6",
      "Cum_rear_cnt_7",
      "Cum_rear_cnt_8",
      "Cum_rear_cnt_9",
      "Cum_rear_cnt_10"
    )
  )

Cum_hab_rear_count <-
  pivot_longer(
    Cum_hab_rear_count,
    cols = 3:12 ,
    names_to = "Minute",
    values_to = "Rear_Count"
  )

Cum_hab_rear_count$Minute <- as.factor(Cum_hab_rear_count$Minute)

Cum_hab_rear_count_summary = describeBy(
  Cum_hab_rear_count$Rear_Count,
  list(Cum_hab_rear_count$Habituation, Cum_hab_rear_count$Minute),
  mat = TRUE,
  digits = 2
)

Cum_hab_rear_count_plot <-
  ggplot(Cum_hab_rear_count_summary,
         aes(x = group2, y = mean, fill = group1)) +
  geom_bar(stat = 'identity',
           position = dodge,
           width = .8) +
  geom_errorbar(limits, position = dodge, width = 0.3) +
  theme_minimal() +
  scale_y_continuous(name = "Rearing event count",
                     breaks = seq(0, 100, 10),
                     limits = c(0, 100)) +
  scale_x_discrete(
    name = "Minute",
    labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
    limits = c(
      "Cum_rear_cnt_1",
      "Cum_rear_cnt_2",
      "Cum_rear_cnt_3",
      "Cum_rear_cnt_4",
      "Cum_rear_cnt_5",
      "Cum_rear_cnt_6",
      "Cum_rear_cnt_7",
      "Cum_rear_cnt_8",
      "Cum_rear_cnt_9",
      "Cum_rear_cnt_10"
    )
  ) +
  scale_fill_grey(
    name = "Habituation",
    labels = c(
      paste0("1 (N=", mean(Cum_hab_dist_summary$n), ")"),
      paste0("2 (N=", mean(Cum_hab_dist_summary$n), ")"),
      paste0("3 (N=", mean(Cum_hab_dist_summary$n), ")")
    ),
    limits = c("Hab1", "Hab2", "Hab3")
  ) +
  ggtitle("Habituation | Rearing event count (cumulative)") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Bin_hab_rear_count <-
  subset(
    hab,
    select = c(
      "Animal",
      "Habituation",
      "Bin_rear_cnt_1",
      "Bin_rear_cnt_2",
      "Bin_rear_cnt_3",
      "Bin_rear_cnt_4",
      "Bin_rear_cnt_5",
      "Bin_rear_cnt_6",
      "Bin_rear_cnt_7",
      "Bin_rear_cnt_8",
      "Bin_rear_cnt_9",
      "Bin_rear_cnt_10"
    )
  )

Bin_hab_rear_count <-
  pivot_longer(
    Bin_hab_rear_count,
    cols = 3:12 ,
    names_to = "Minute",
    values_to = "Rear_Count"
  )

Bin_hab_rear_count$Minute <- as.factor(Bin_hab_rear_count$Minute)

Bin_hab_rear_count_summary = describeBy(
  Bin_hab_rear_count$Rear_Count,
  list(Bin_hab_rear_count$Habituation, Bin_hab_rear_count$Minute),
  mat = TRUE,
  digits = 2
)

Bin_hab_rear_count_plot <-
  ggplot(Bin_hab_rear_count_summary,
         aes(x = group2, y = mean, fill = group1)) +
  geom_bar(stat = 'identity',
           position = dodge,
           width = .8) +
  geom_errorbar(limits, position = dodge, width = 0.3) +
  theme_minimal() +
  scale_y_continuous(name = "Rearing event count",
                     breaks = seq(0, 250, 5),
                     limits = c(0, 25)) +
  scale_x_discrete(
    name = "Minute",
    labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
    limits = c(
      "Bin_rear_cnt_1",
      "Bin_rear_cnt_2",
      "Bin_rear_cnt_3",
      "Bin_rear_cnt_4",
      "Bin_rear_cnt_5",
      "Bin_rear_cnt_6",
      "Bin_rear_cnt_7",
      "Bin_rear_cnt_8",
      "Bin_rear_cnt_9",
      "Bin_rear_cnt_10"
    )
  ) +
  scale_fill_grey(
    name = "Habituation",
    labels = c(
      paste0("1 (N=", mean(Bin_hab_dist_summary$n), ")"),
      paste0("2 (N=", mean(Bin_hab_dist_summary$n), ")"),
      paste0("3 (N=", mean(Bin_hab_dist_summary$n), ")")
    ),
    limits = c("Hab1", "Hab2", "Hab3")
  ) +
  ggtitle("Habituation | Rearing event count (non-cumulative)") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Cum_hab_rear_count_plot
Bin_hab_rear_count_plot

# 6. Grooming onset
Groom_onset <- 
  subset(
    hab,
    select = c(
      "Animal",
      "Habituation",
      "Groo_latency"
    )
  )

Groom_onset_summary = describeBy(
  Groom_onset$Groo_latency,
  list(Groom_onset$Habituation),
  mat = TRUE,
  digits = 2
)

Groom_onset_plot <-
  ggplot(Groom_onset_summary,
         aes(x = group1, y = mean)) +
  geom_bar(stat = 'identity',
           position = dodge,
           width = .8) +
  geom_errorbar(limits, position = dodge, width = 0.3) +
  theme_minimal() +
  scale_y_continuous(name = "Grooming Onset",
                     breaks = seq(0, 280, 20),
                     limits = c(0, 280)) +
  scale_x_discrete(
    name = "Habituation",
    labels = c(
      paste0("1 (N=", mean(Bin_hab_dist_summary$n), ")"),
      paste0("2 (N=", mean(Bin_hab_dist_summary$n), ")"),
      paste0("3 (N=", mean(Bin_hab_dist_summary$n), ")")
    ),
    limits = c("Hab1", "Hab2", "Hab3")
  )+
  ggtitle("Habituation | Grooming onset") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Groom_onset_plot
