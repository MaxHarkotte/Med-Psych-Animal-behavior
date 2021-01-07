# Creates a descriptive report of all behavioral measures
## maximilian.harkotte@gmail.com

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

# Create directories to store figures
dir.create(paste0(dataPath, "Descriptives"))
dir.create(paste0(dataPath, "Descriptives/Plots"))
dir.create(paste0(dataPath, "Descriptives/Plots/Habituation"))
dir.create(paste0(dataPath, "Descriptives/Plots/Habituation/Cumulative"))
dir.create(paste0(dataPath, "Descriptives/Plots/Habituation/Non_cumulative"))
dir.create(paste0(dataPath, "Descriptives/Plots/Habituation/General"))
dir.create(paste0(dataPath, "Descriptives/Plots/Encoding"))
dir.create(paste0(dataPath, "Descriptives/Plots/Encoding/Cumulative"))
dir.create(paste0(dataPath, "Descriptives/Plots/Encoding/Non_cumulative"))
dir.create(paste0(dataPath, "Descriptives/Plots/Encoding/General"))
dir.create(paste0(dataPath, "Descriptives/Plots/Test"))
dir.create(paste0(dataPath, "Descriptives/Plots/Test/Cumulative"))
dir.create(paste0(dataPath, "Descriptives/Plots/Test/Non_cumulative"))
dir.create(paste0(dataPath, "Descriptives/Plots/Test/General"))
dir.create(paste0(dataPath, "Descriptives/Tables"))

# 2 - Read in data --------------------------------------------------------

hab <-
  read.csv2("100-MPV0120-Habituation_clean.csv", stringsAsFactors = TRUE)

enc <-
  read.csv2("100-MPV0120-Encoding_clean.csv", stringsAsFactors = TRUE)

test <-
  read.csv2("100-MPV0120-Test_clean.csv", stringsAsFactors = TRUE)


# Exclude animals from analysis
## Habituation
hab <-
  subset(hab,!(Animal %in% c())) # exclude animals from analysis

## Encoding
enc <-
  subset(enc,!(Animal %in% c())) # exclude animals from analysis

## Test
test <-
  subset(test,!(Animal %in% c())) # exclude animals from analysis

# 4 - Plotting ------------------------------------------------------------
# Add aestetics
limits = aes(ymax = mean + (se), ymin = mean - (se)) # (1.96*se) for confidence intervals
dodge = position_dodge(width = 0.8)

# Habituation -------------------------------------------------------------
# 01 - Distance traveled -------------------------------------------------

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

write.csv2(Cum_hab_dist,
           file.path(dataPath, "Descriptives/Tables/Hab_Cum_Dist.csv"),
           row.names = FALSE)

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

write.csv2(Bin_hab_dist,
           file.path(dataPath, "Descriptives/Tables/Hab_Bin_Dist.csv"),
           row.names = FALSE)

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

ggsave(
  Cum_hab_dist_plot,
  path = paste0(dataPath, "Descriptives/Plots/Habituation/Cumulative"),
  filename = "01-Distance.pdf"
)
ggsave(
  Bin_hab_dist_plot,
  path = paste0(dataPath, "Descriptives/Plots/Habituation/Non_cumulative"),
  filename = "01-Distance.pdf"
)



# 02 - Velocity/ Mean speed -----------------------------------------------

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


write.csv2(Cum_hab_velo,
           file.path(dataPath, "Descriptives/Tables/Hab_Cum_Mean_Speed.csv"),
           row.names = FALSE)

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


write.csv2(Bin_hab_velo,
           file.path(dataPath, "Descriptives/Tables/Hab_Bin_Mean_Speed.csv"),
           row.names = FALSE)


Bin_hab_velo <-
  pivot_longer(
    Bin_hab_velo,
    cols = 3:12 ,
    names_to = "Minute",
    values_to = "Speed"
  )

Bin_hab_velo$Minute <- as.factor(Bin_hab_velo$Minute)

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

ggsave(
  Cum_hab_velo_plot,
  path = paste0(dataPath, "Descriptives/Plots/Habituation/Cumulative"),
  filename = "02-Mean_Speed.pdf"
)
ggsave(
  Bin_hab_velo_plot,
  path = paste0(dataPath, "Descriptives/Plots/Habituation/Non_cumulative"),
  filename = "02-Mean_Speed.pdf"
)

# 03 - Time spent rearing  ------------------------------------------------
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

write.csv2(Cum_hab_rear,
           file.path(dataPath, "Descriptives/Tables/Hab_Cum_Rear_Time.csv"),
           row.names = FALSE)

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

write.csv2(Bin_hab_rear,
           file.path(dataPath, "Descriptives/Tables/Hab_Bin_Rear_Time.csv"),
           row.names = FALSE)

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

ggsave(
  Cum_hab_rear_plot,
  path = paste0(dataPath, "Descriptives/Plots/Habituation/Cumulative"),
  filename = "03-Rearing_Time.pdf"
)
ggsave(
  Bin_hab_rear_plot,
  path = paste0(dataPath, "Descriptives/Plots/Habituation/Non_cumulative"),
  filename = "03-Rearing_Time.pdf"
)


# 04 - Time spent grooming  -----------------------------------------------
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

write.csv2(Cum_hab_groo,
           file.path(dataPath, "Descriptives/Tables/Hab_Cum_Groom_Time.csv"),
           row.names = FALSE)

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

write.csv2(Bin_hab_groo,
           file.path(dataPath, "Descriptives/Tables/Hab_Bin_Groom_Time.csv"),
           row.names = FALSE)

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

ggsave(
  Cum_hab_groo_plot,
  path = paste0(dataPath, "Descriptives/Plots/Habituation/Cumulative"),
  filename = "04-Grooming_Time.pdf"
)
ggsave(
  Bin_hab_groo_plot,
  path = paste0(dataPath, "Descriptives/Plots/Habituation/Non_cumulative"),
  filename = "04-Grooming_Time.pdf"
)


# 05 - Rearing event count  -----------------------------------------------

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

write.csv2(Cum_hab_rear_count,
           file.path(dataPath, "Descriptives/Tables/Hab_Cum_Rear_Count.csv"),
           row.names = FALSE)

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

write.csv2(Bin_hab_rear_count,
           file.path(dataPath, "Descriptives/Tables/Hab_Bin_Rear_Count.csv"),
           row.names = FALSE)

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

ggsave(
  Cum_hab_rear_count_plot,
  path = paste0(dataPath, "Descriptives/Plots/Habituation/Cumulative"),
  filename = "05-Rearing_Count.pdf"
)
ggsave(
  Bin_hab_rear_count_plot,
  path = paste0(dataPath, "Descriptives/Plots/Habituation/Non_cumulative"),
  filename = "05-Rearing_Count.pdf"
)

# 06 - Grooming onset -----------------------------------------------------
Groom_onset <-
  subset(hab,
         select = c("Animal",
                    "Habituation",
                    "Groo_latency"))

write.csv2(Groom_onset,
           file.path(dataPath, "Descriptives/Tables/Hab_Groom_Onset.csv"),
           row.names = FALSE)

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
  ) +
  ggtitle("Habituation | Grooming onset") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Groom_onset_plot

ggsave(
  Groom_onset_plot,
  path = paste0(dataPath, "Descriptives/Plots/Habituation/General"),
  filename = "01-Grooming_Onset.pdf"
)

# Encoding ----------------------------------------------------------------
# 01 - Distance traveled -------------------------------------------------

Cum_enc_dist <-
  subset(
    enc,
    select = c(
      "Animal",
      "Task",
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

write.csv2(Cum_enc_dist,
           file.path(dataPath, "Descriptives/Tables/Enc_Cum_Dist.csv"),
           row.names = FALSE)

Cum_enc_dist <-
  pivot_longer(
    Cum_enc_dist,
    cols = 3:12 ,
    names_to = "Minute",
    values_to = "dist"
  )

Cum_enc_dist$Minute <- as.factor(Cum_enc_dist$Minute)

Cum_enc_dist_summary = describeBy(
  Cum_enc_dist$dist,
  list(Cum_enc_dist$Task, Cum_enc_dist$Minute),
  mat = TRUE,
  digits = 2
)

Cum_enc_dist_plot <-
  ggplot(Cum_enc_dist_summary,
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
    name = "Task",
    labels = c(paste0(
      "OPR (N=", mean(Cum_enc_dist_summary$n), ")"
    ),
    paste0(
      "NOR (N=", mean(Cum_enc_dist_summary$n), ")"
    )),
    limits = c("OPR", "NOR")
  ) +
  ggtitle("Encoding | Distance travelled (cumulative)") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Bin_enc_dist <-
  subset(
    enc,
    select = c(
      "Animal",
      "Task",
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

write.csv2(Bin_enc_dist,
           file.path(dataPath, "Descriptives/Tables/Enc_Bin_Dist.csv"),
           row.names = FALSE)

Bin_enc_dist <-
  pivot_longer(
    Bin_enc_dist,
    cols = 3:12 ,
    names_to = "Minute",
    values_to = "dist"
  )

Bin_enc_dist$Minute <- as.factor(Bin_enc_dist$Minute)

Bin_enc_dist_summary = describeBy(
  Bin_enc_dist$dist,
  list(Bin_enc_dist$Task, Bin_enc_dist$Minute),
  mat = TRUE,
  digits = 2
)

Bin_enc_dist_plot <-
  ggplot(Bin_enc_dist_summary,
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
    name = "Task",
    labels = c(paste0(
      "OPR (N=", mean(Bin_enc_dist_summary$n), ")"
    ),
    paste0(
      "NOR (N=", mean(Bin_enc_dist_summary$n), ")"
    )),
    limits = c("OPR", "NOR")
  ) +
  ggtitle("Encoding | Distance travelled (non-cumulative)") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)


Cum_enc_dist_plot
Bin_enc_dist_plot

ggsave(
  Cum_enc_dist_plot,
  path = paste0(dataPath, "Descriptives/Plots/Encoding/Cumulative"),
  filename = "01-Distance.pdf"
)
ggsave(
  Bin_enc_dist_plot,
  path = paste0(dataPath, "Descriptives/Plots/Encoding/Non_cumulative"),
  filename = "01-Distance.pdf"
)

# 02 - Velocity/ Mean speed -----------------------------------------------
Cum_enc_velo <-
  subset(
    enc,
    select = c(
      "Animal",
      "Task",
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

write.csv2(Cum_enc_velo,
           file.path(dataPath, "Descriptives/Tables/Enc_Cum_Mean_Speed.csv"),
           row.names = FALSE)

Cum_enc_velo <-
  pivot_longer(
    Cum_enc_velo,
    cols = 3:12 ,
    names_to = "Minute",
    values_to = "Speed"
  )

Cum_enc_veloMinute <- as.factor(Cum_enc_velo$Minute)

Cum_enc_velo_summary = describeBy(
  Cum_enc_velo$Speed,
  list(Cum_enc_velo$Task, Cum_enc_velo$Minute),
  mat = TRUE,
  digits = 2
)

Cum_enc_velo_plot <-
  ggplot(Cum_enc_velo_summary,
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
    name = "Task",
    labels = c(paste0(
      "OPR (N=", mean(Cum_enc_velo_summary$n), ")"
    ),
    paste0(
      "NOR (N=", mean(Cum_enc_velo_summary$n), ")"
    )),
    limits = c("OPR", "NOR")
  ) +
  ggtitle("Encoding | Mean speed (cumulative)") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Bin_enc_velo <-
  subset(
    enc,
    select = c(
      "Animal",
      "Task",
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

write.csv2(Bin_enc_velo,
           file.path(dataPath, "Descriptives/Tables/Enc_Bin_Mean_Speed.csv"),
           row.names = FALSE)

Bin_enc_velo <-
  pivot_longer(
    Bin_enc_velo,
    cols = 3:12 ,
    names_to = "Minute",
    values_to = "Speed"
  )

Bin_enc_velo$Minute <- as.factor(Bin_enc_velo$Minute)

Bin_enc_velo_summary = describeBy(
  Bin_enc_velo$Speed,
  list(Bin_enc_velo$Task, Bin_enc_velo$Minute),
  mat = TRUE,
  digits = 2
)

Bin_enc_velo_plot <-
  ggplot(Bin_enc_velo_summary,
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
    name = "Task",
    labels = c(paste0(
      "OPR (N=", mean(Bin_enc_velo_summary$n), ")"
    ),
    paste0(
      "NOR (N=", mean(Bin_enc_velo_summary$n), ")"
    )),
    limits = c("OPR", "NOR")
  ) +
  ggtitle("Encoding | Mean speed (non-cumulative)") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Cum_enc_velo_plot
Bin_enc_velo_plot

ggsave(
  Cum_enc_velo_plot,
  path = paste0(dataPath, "Descriptives/Plots/Encoding/Cumulative"),
  filename = "02-Mean_Speed.pdf"
)
ggsave(
  Bin_enc_velo_plot,
  path = paste0(dataPath, "Descriptives/Plots/Encoding/Non_cumulative"),
  filename = "02-Mean_Speed.pdf"
)

# 03 - Time spent rearing  ------------------------------------------------
Cum_enc_rear <-
  subset(
    enc,
    select = c(
      "Animal",
      "Task",
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

write.csv2(Cum_enc_rear,
           file.path(dataPath, "Descriptives/Tables/Enc_Cum_Rear_Time.csv"),
           row.names = FALSE)

Cum_enc_rear <-
  pivot_longer(
    Cum_enc_rear,
    cols = 3:12 ,
    names_to = "Minute",
    values_to = "Rear_Time"
  )

Cum_enc_rear$Minute <- as.factor(Cum_enc_rear$Minute)

Cum_enc_rear_summary = describeBy(
  Cum_enc_rear$Rear_Time,
  list(Cum_enc_rear$Task, Cum_enc_rear$Minute),
  mat = TRUE,
  digits = 2
)

Cum_enc_rear_plot <-
  ggplot(Cum_enc_rear_summary,
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
    name = "Task",
    labels = c(paste0(
      "OPR (N=", mean(Cum_enc_dist_summary$n), ")"
    ),
    paste0(
      "NOR (N=", mean(Cum_enc_dist_summary$n), ")"
    )),
    limits = c("OPR", "NOR")
  ) +
  ggtitle("Encoding | Time spent rearing (cumulative)") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Bin_enc_rear <-
  subset(
    enc,
    select = c(
      "Animal",
      "Task",
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

write.csv2(Bin_enc_rear,
           file.path(dataPath, "Descriptives/Tables/Enc_Bin_Rear_Time.csv"),
           row.names = FALSE)

Bin_enc_rear <-
  pivot_longer(
    Bin_enc_rear,
    cols = 3:12 ,
    names_to = "Minute",
    values_to = "Rear_Time"
  )

Bin_enc_rear$Minute <- as.factor(Bin_enc_rear$Minute)

Bin_enc_rear_summary = describeBy(
  Bin_enc_rear$Rear_Time,
  list(Bin_enc_rear$Task, Bin_enc_rear$Minute),
  mat = TRUE,
  digits = 2
)

Bin_enc_rear_plot <-
  ggplot(Bin_enc_rear_summary,
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
    name = "Task",
    labels = c(paste0(
      "OPR (N=", mean(Bin_enc_dist_summary$n), ")"
    ),
    paste0(
      "NOR (N=", mean(Bin_enc_dist_summary$n), ")"
    )),
    limits = c("OPR", "NOR")
  ) +
  ggtitle("Encding | Time spent rearing (non-cumulative)") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Cum_enc_rear_plot
Bin_enc_rear_plot

ggsave(
  Cum_enc_rear_plot,
  path = paste0(dataPath, "Descriptives/Plots/Encoding/Cumulative"),
  filename = "03-Rearing_Time.pdf"
)
ggsave(
  Bin_enc_rear_plot,
  path = paste0(dataPath, "Descriptives/Plots/Encoding/Non_cumulative"),
  filename = "03-Rearing_Time.pdf"
)

# 04 - Time spent grooming  -----------------------------------------------
Cum_enc_groo <-
  subset(
    enc,
    select = c(
      "Animal",
      "Task",
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

write.csv2(Cum_enc_groo,
           file.path(dataPath, "Descriptives/Tables/Enc_Cum_Groom_Time.csv"),
           row.names = FALSE)

Cum_enc_groo <-
  pivot_longer(
    Cum_enc_groo,
    cols = 3:12 ,
    names_to = "Minute",
    values_to = "groo_Time"
  )

Cum_enc_groo$Minute <- as.factor(Cum_enc_groo$Minute)

Cum_enc_groo_summary = describeBy(
  Cum_enc_groo$groo_Time,
  list(Cum_enc_groo$Task, Cum_enc_groo$Minute),
  mat = TRUE,
  digits = 2
)

Cum_enc_groo_plot <-
  ggplot(Cum_enc_groo_summary,
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
    name = "Task",
    labels = c(paste0(
      "OPR (N=", mean(Cum_enc_dist_summary$n), ")"
    ),
    paste0(
      "NOR (N=", mean(Cum_enc_dist_summary$n), ")"
    )),
    limits = c("OPR", "NOR")
  ) +
  ggtitle("Encoding | Time spent grooming (cumulative)") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Bin_enc_groo <-
  subset(
    enc,
    select = c(
      "Animal",
      "Task",
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

write.csv2(Bin_enc_groo,
           file.path(dataPath, "Descriptives/Tables/Enc_Bin_Groom_Time.csv"),
           row.names = FALSE)

Bin_enc_groo <-
  pivot_longer(
    Bin_enc_groo,
    cols = 3:12 ,
    names_to = "Minute",
    values_to = "groo_Time"
  )

Bin_enc_groo$Minute <- as.factor(Bin_enc_groo$Minute)

Bin_enc_groo_summary = describeBy(
  Bin_enc_groo$groo_Time,
  list(Bin_enc_groo$Task, Bin_enc_groo$Minute),
  mat = TRUE,
  digits = 2
)

Bin_enc_groo_plot <-
  ggplot(Bin_enc_groo_summary,
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
    name = "Task",
    labels = c(paste0(
      "OPR (N=", mean(Bin_enc_dist_summary$n), ")"
    ),
    paste0(
      "NOR (N=", mean(Bin_enc_dist_summary$n), ")"
    )),
    limits = c("OPR", "NOR")
  ) +
  ggtitle("Encoding | Time spent grooming (non-cumulative)") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Cum_enc_groo_plot
Bin_enc_groo_plot

ggsave(
  Cum_enc_groo_plot,
  path = paste0(dataPath, "Descriptives/Plots/Encoding/Cumulative"),
  filename = "04-Grooming_Time.pdf"
)
ggsave(
  Bin_enc_groo_plot,
  path = paste0(dataPath, "Descriptives/Plots/Encoding/Non_cumulative"),
  filename = "04-Grooming_Time.pdf"
)

# 05 - Rearing event count  -----------------------------------------------
Cum_enc_rear_count <-
  subset(
    enc,
    select = c(
      "Animal",
      "Task",
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

write.csv2(Cum_enc_rear_count,
           file.path(dataPath, "Descriptives/Tables/Enc_Cum_Rear_Count.csv"),
           row.names = FALSE)

Cum_enc_rear_count <-
  pivot_longer(
    Cum_enc_rear_count,
    cols = 3:12 ,
    names_to = "Minute",
    values_to = "Rear_Count"
  )

Cum_enc_rear_count$Minute <- as.factor(Cum_enc_rear_count$Minute)

Cum_enc_rear_count_summary = describeBy(
  Cum_enc_rear_count$Rear_Count,
  list(Cum_enc_rear_count$Task, Cum_enc_rear_count$Minute),
  mat = TRUE,
  digits = 2
)

Cum_enc_rear_count_plot <-
  ggplot(Cum_enc_rear_count_summary,
         aes(x = group2, y = mean, fill = group1)) +
  geom_bar(stat = 'identity',
           position = dodge,
           width = .8) +
  geom_errorbar(limits, position = dodge, width = 0.3) +
  theme_minimal() +
  scale_y_continuous(name = "Rearing event count",
                     breaks = seq(0, 120, 10),
                     limits = c(0, 120)) +
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
    name = "Task",
    labels = c(paste0(
      "OPR (N=", mean(Cum_enc_dist_summary$n), ")"
    ),
    paste0(
      "NOR (N=", mean(Cum_enc_dist_summary$n), ")"
    )),
    limits = c("OPR", "NOR")
  ) +
  ggtitle("Encoding | Rearing event count (cumulative)") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Bin_enc_rear_count <-
  subset(
    enc,
    select = c(
      "Animal",
      "Task",
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

write.csv2(Bin_enc_rear_count,
           file.path(dataPath, "Descriptives/Tables/Enc_Bin_Rear_Count.csv"),
           row.names = FALSE)

Bin_enc_rear_count <-
  pivot_longer(
    Bin_enc_rear_count,
    cols = 3:12 ,
    names_to = "Minute",
    values_to = "Rear_Count"
  )

Bin_enc_rear_count$Minute <- as.factor(Bin_enc_rear_count$Minute)

Bin_enc_rear_count_summary = describeBy(
  Bin_enc_rear_count$Rear_Count,
  list(Bin_enc_rear_count$Task, Bin_enc_rear_count$Minute),
  mat = TRUE,
  digits = 2
)

Bin_enc_rear_count_plot <-
  ggplot(Bin_enc_rear_count_summary,
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
    name = "Task",
    labels = c(paste0(
      "OPR (N=", mean(Bin_enc_dist_summary$n), ")"
    ),
    paste0(
      "NOR (N=", mean(Bin_enc_dist_summary$n), ")"
    )),
    limits = c("OPR", "NOR")
  ) +
  ggtitle("Encoding | Rearing event count (non-cumulative)") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Cum_enc_rear_count_plot
Bin_enc_rear_count_plot

ggsave(
  Cum_enc_rear_count_plot,
  path = paste0(dataPath, "Descriptives/Plots/Encoding/Cumulative"),
  filename = "05-Rearing_Count.pdf"
)
ggsave(
  Bin_enc_rear_count_plot,
  path = paste0(dataPath, "Descriptives/Plots/Encoding/Non_cumulative"),
  filename = "05-Rearing_Count.pdf"
)

# 06 - Grooming onset -----------------------------------------------------
Groom_onset <-
  subset(enc,
         select = c("Animal",
                    "Task",
                    "Groo_latency"))

write.csv2(Groom_onset,
           file.path(dataPath, "Descriptives/Tables/Enc_Groom_Onset.csv"),
           row.names = FALSE)

Groom_onset_summary = describeBy(
  Groom_onset$Groo_latency,
  list(Groom_onset$Task),
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
    labels = c(paste0(
      "OPR (N=", mean(Bin_hab_dist_summary$n), ")"
    ),
    paste0(
      "NOR (N=", mean(Bin_hab_dist_summary$n), ")"
    )),
    limits = c("OPR", "NOR")
  ) +
  ggtitle("Encoding | Grooming onset") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Groom_onset_plot

ggsave(
  Groom_onset_plot,
  path = paste0(dataPath, "Descriptives/Plots/Encoding/General"),
  filename = "01-Grooming_Onset.pdf"
)


# 07 - Total object exploration time --------------------------------------
Enc_total_exploration <-
  subset(enc,
         select = c("Animal",
                    "Task",
                    "Total_exp_time"))

write.csv2(Enc_total_exploration,
           file.path(dataPath, "Descriptives/Tables/Enc_Total_Obj_exploration.csv"),
           row.names = FALSE)

Enc_total_exploration_sum = describeBy(
  Enc_total_exploration$Total_exp_time,
  list(Enc_total_exploration$Task),
  mat = TRUE,
  digits = 2
)

Enc_total_exploratio_plot <-
  ggplot(Enc_total_exploration_sum,
         aes(x = group1, y = mean)) +
  geom_bar(stat = 'identity',
           position = dodge,
           width = .8) +
  geom_errorbar(limits, position = dodge, width = 0.3) +
  theme_minimal() +
  scale_y_continuous(name = "Total exploration time [s]",
                     breaks = seq(0, 10, 2),
                     limits = c(0, 10)) +
  scale_x_discrete(
    name = "Task",
    labels = c(
      paste0("OPR (N=", mean(Enc_total_exploration_sum$n), ")"),
      paste0("NOR (N=", mean(Enc_total_exploration_sum$n), ")")
    ),
    limits = c("OPR", "NOR")
  ) +
  ggtitle("Encoding | Total exploration time") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Enc_total_exploratio_plot

ggsave(
  Enc_total_exploratio_plot,
  path = paste0(dataPath, "Descriptives/Plots/Encoding/General"),
  filename = "02-Total_exploration_time.pdf"
)
# 08 - Exploration time per object ----------------------------------------
Enc_exp_per_object <-
  subset(enc,
         select = c("Animal",
                    "Task",
                    "Cum_FrRi_exp_10",
                    "Cum_BaLe_exp_10"))

write.csv2(Enc_exp_per_object,
           file.path(dataPath, "Descriptives/Tables/Enc_Exploration_per_object.csv"),
           row.names = FALSE)

# 09 - Exploration onset --------------------------------------------------





# Test --------------------------------------------------------------------
# 01 - Distance traveled -------------------------------------------------
Cum_test_dist <-
  subset(
    test,
    select = c(
      "Animal",
      "Task",
      "Cum_dist_min_1",
      "Cum_dist_min_2",
      "Cum_dist_min_3",
      "Cum_dist_min_4",
      "Cum_dist_min_5"
    )
  )

write.csv2(Cum_test_dist,
           file.path(dataPath, "Descriptives/Tables/Test_Cum_Dist.csv"),
           row.names = FALSE)

Cum_test_dist <-
  pivot_longer(
    Cum_test_dist,
    cols = 3:7 ,
    names_to = "Minute",
    values_to = "dist"
  )

Cum_test_dist$Minute <- as.factor(Cum_test_dist$Minute)

Cum_test_dist_summary = describeBy(
  Cum_test_dist$dist,
  list(Cum_test_dist$Task, Cum_test_dist$Minute),
  mat = TRUE,
  digits = 2
)

Cum_test_dist_plot <-
  ggplot(Cum_test_dist_summary,
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
    labels = c("1", "2", "3", "4", "5"),
    limits = c(
      "Cum_dist_min_1",
      "Cum_dist_min_2",
      "Cum_dist_min_3",
      "Cum_dist_min_4",
      "Cum_dist_min_5"
    )
  ) +
  scale_fill_grey(
    name = "Task",
    labels = c(paste0(
      "OPR (N=", mean(Cum_test_dist_summary$n), ")"
    ),
    paste0(
      "NOR (N=", mean(Cum_test_dist_summary$n), ")"
    )),
    limits = c("OPR", "NOR")
  ) +
  ggtitle("Test | Distance travelled (cumulative)") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Bin_test_dist <-
  subset(
    test,
    select = c(
      "Animal",
      "Task",
      "Bin_dist_min_1",
      "Bin_dist_min_2",
      "Bin_dist_min_3",
      "Bin_dist_min_4",
      "Bin_dist_min_5"
    )
  )

write.csv2(Bin_test_dist,
           file.path(dataPath, "Descriptives/Tables/Test_Bin_Dist.csv"),
           row.names = FALSE)

Bin_test_dist <-
  pivot_longer(
    Bin_test_dist,
    cols = 3:7 ,
    names_to = "Minute",
    values_to = "dist"
  )

Bin_test_dist$Minute <- as.factor(Bin_test_dist$Minute)

Bin_test_dist_summary = describeBy(
  Bin_test_dist$dist,
  list(Bin_test_dist$Task, Bin_test_dist$Minute),
  mat = TRUE,
  digits = 2
)

Bin_test_dist_plot <-
  ggplot(Bin_test_dist_summary,
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
    labels = c("1", "2", "3", "4", "5"),
    limits = c(
      "Bin_dist_min_1",
      "Bin_dist_min_2",
      "Bin_dist_min_3",
      "Bin_dist_min_4",
      "Bin_dist_min_5"
    )
  ) +
  scale_fill_grey(
    name = "Task",
    labels = c(paste0(
      "OPR (N=", mean(Bin_test_dist_summary$n), ")"
    ),
    paste0(
      "NOR (N=", mean(Bin_test_dist_summary$n), ")"
    )),
    limits = c("OPR", "NOR")
  ) +
  ggtitle("Test | Distance travelled (non-cumulative)") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)


Cum_test_dist_plot
Bin_test_dist_plot

ggsave(
  Cum_test_dist_plot,
  path = paste0(dataPath, "Descriptives/Plots/Test/Cumulative"),
  filename = "01-Distance.pdf"
)
ggsave(
  Bin_test_dist_plot,
  path = paste0(dataPath, "Descriptives/Plots/Test/Non_cumulative"),
  filename = "01-Distance.pdf"
)

# 02 - Velocity/ Mean speed -----------------------------------------------
Cum_test_velo <-
  subset(
    test,
    select = c(
      "Animal",
      "Task",
      "Cum_velo_min_1",
      "Cum_velo_min_2",
      "Cum_velo_min_3",
      "Cum_velo_min_4",
      "Cum_velo_min_5"
    )
  )

write.csv2(Cum_test_velo,
           file.path(dataPath, "Descriptives/Tables/Test_Cum_Mean_Speed.csv"),
           row.names = FALSE)

Cum_test_velo <-
  pivot_longer(
    Cum_test_velo,
    cols = 3:7 ,
    names_to = "Minute",
    values_to = "Speed"
  )

Cum_test_veloMinute <- as.factor(Cum_test_velo$Minute)

Cum_test_velo_summary = describeBy(
  Cum_test_velo$Speed,
  list(Cum_test_velo$Task, Cum_test_velo$Minute),
  mat = TRUE,
  digits = 2
)

Cum_test_velo_plot <-
  ggplot(Cum_test_velo_summary,
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
    labels = c("1", "2", "3", "4", "5"),
    limits = c(
      "Cum_velo_min_1",
      "Cum_velo_min_2",
      "Cum_velo_min_3",
      "Cum_velo_min_4",
      "Cum_velo_min_5"
    )
  ) +
  scale_fill_grey(
    name = "Task",
    labels = c(paste0(
      "OPR (N=", mean(Cum_test_velo_summary$n), ")"
    ),
    paste0(
      "NOR (N=", mean(Cum_test_velo_summary$n), ")"
    )),
    limits = c("OPR", "NOR")
  ) +
  ggtitle("Test | Mean speed (cumulative)") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Bin_test_velo <-
  subset(
    test,
    select = c(
      "Animal",
      "Task",
      "Bin_velo_min_1",
      "Bin_velo_min_2",
      "Bin_velo_min_3",
      "Bin_velo_min_4",
      "Bin_velo_min_5"
    )
  )

write.csv2(Bin_test_velo,
           file.path(dataPath, "Descriptives/Tables/Test_Bin_Mean_Speed.csv"),
           row.names = FALSE)

Bin_test_velo <-
  pivot_longer(
    Bin_test_velo,
    cols = 3:7,
    names_to = "Minute",
    values_to = "Speed"
  )

Bin_test_velo$Minute <- as.factor(Bin_test_velo$Minute)

Bin_test_velo_summary = describeBy(
  Bin_test_velo$Speed,
  list(Bin_test_velo$Task, Bin_test_velo$Minute),
  mat = TRUE,
  digits = 2
)

Bin_test_velo_plot <-
  ggplot(Bin_test_velo_summary,
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
    labels = c("1", "2", "3", "4", "5"),
    limits = c(
      "Bin_velo_min_1",
      "Bin_velo_min_2",
      "Bin_velo_min_3",
      "Bin_velo_min_4",
      "Bin_velo_min_5"
    )
  ) +
  scale_fill_grey(
    name = "Task",
    labels = c(paste0(
      "OPR (N=", mean(Bin_test_velo_summary$n), ")"
    ),
    paste0(
      "NOR (N=", mean(Bin_test_velo_summary$n), ")"
    )),
    limits = c("OPR", "NOR")
  ) +
  ggtitle("Test | Mean speed (non-cumulative)") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Cum_test_velo_plot
Bin_test_velo_plot

ggsave(
  Cum_test_velo_plot,
  path = paste0(dataPath, "Descriptives/Plots/Test/Cumulative"),
  filename = "02-Mean_Speed.pdf"
)
ggsave(
  Bin_test_velo_plot,
  path = paste0(dataPath, "Descriptives/Plots/Test/Non_cumulative"),
  filename = "02-Mean_Speed.pdf"
)

# 03 - Time spent rearing  ------------------------------------------------
Cum_test_rear <-
  subset(
    test,
    select = c(
      "Animal",
      "Task",
      "Cum_rear_tim_1",
      "Cum_rear_tim_2",
      "Cum_rear_tim_3",
      "Cum_rear_tim_4",
      "Cum_rear_tim_5"
    )
  )

write.csv2(Cum_test_rear,
           file.path(dataPath, "Descriptives/Tables/Test_Cum_Rear_Time.csv"),
           row.names = FALSE)

Cum_test_rear <-
  pivot_longer(
    Cum_test_rear,
    cols = 3:7,
    names_to = "Minute",
    values_to = "Rear_Time"
  )

Cum_test_rear$Minute <- as.factor(Cum_test_rear$Minute)

Cum_test_rear_summary = describeBy(
  Cum_test_rear$Rear_Time,
  list(Cum_test_rear$Task, Cum_test_rear$Minute),
  mat = TRUE,
  digits = 2
)

Cum_test_rear_plot <-
  ggplot(Cum_test_rear_summary,
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
    labels = c("1", "2", "3", "4", "5"),
    limits = c(
      "Cum_rear_tim_1",
      "Cum_rear_tim_2",
      "Cum_rear_tim_3",
      "Cum_rear_tim_4",
      "Cum_rear_tim_5"
    )
  ) +
  scale_fill_grey(
    name = "Task",
    labels = c(paste0(
      "OPR (N=", mean(Cum_test_dist_summary$n), ")"
    ),
    paste0(
      "NOR (N=", mean(Cum_test_dist_summary$n), ")"
    )),
    limits = c("OPR", "NOR")
  ) +
  ggtitle("Test | Time spent rearing (cumulative)") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Bin_test_rear <-
  subset(
    test,
    select = c(
      "Animal",
      "Task",
      "Bin_rear_tim_1",
      "Bin_rear_tim_2",
      "Bin_rear_tim_3",
      "Bin_rear_tim_4",
      "Bin_rear_tim_5"
    )
  )

write.csv2(Bin_test_rear,
           file.path(dataPath, "Descriptives/Tables/Test_Bin_Rear_Time.csv"),
           row.names = FALSE)

Bin_test_rear <-
  pivot_longer(
    Bin_test_rear,
    cols = 3:7,
    names_to = "Minute",
    values_to = "Rear_Time"
  )

Bin_test_rear$Minute <- as.factor(Bin_test_rear$Minute)

Bin_test_rear_summary = describeBy(
  Bin_test_rear$Rear_Time,
  list(Bin_test_rear$Task, Bin_test_rear$Minute),
  mat = TRUE,
  digits = 2
)

Bin_test_rear_plot <-
  ggplot(Bin_test_rear_summary,
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
    labels = c("1", "2", "3", "4", "5"),
    limits = c(
      "Bin_rear_tim_1",
      "Bin_rear_tim_2",
      "Bin_rear_tim_3",
      "Bin_rear_tim_4",
      "Bin_rear_tim_5"
    )
  ) +
  scale_fill_grey(
    name = "Task",
    labels = c(paste0(
      "OPR (N=", mean(Bin_test_dist_summary$n), ")"
    ),
    paste0(
      "NOR (N=", mean(Bin_test_dist_summary$n), ")"
    )),
    limits = c("OPR", "NOR")
  ) +
  ggtitle("testding | Time spent rearing (non-cumulative)") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Cum_test_rear_plot
Bin_test_rear_plot

ggsave(
  Cum_test_rear_plot,
  path = paste0(dataPath, "Descriptives/Plots/Test/Cumulative"),
  filename = "03-Rearing_Time.pdf"
)
ggsave(
  Bin_test_rear_plot,
  path = paste0(dataPath, "Descriptives/Plots/Test/Non_cumulative"),
  filename = "03-Rearing_Time.pdf"
)

# 04 - Time spent grooming  -----------------------------------------------
Cum_test_groo <-
  subset(
    test,
    select = c(
      "Animal",
      "Task",
      "Cum_groo_tim_1",
      "Cum_groo_tim_2",
      "Cum_groo_tim_3",
      "Cum_groo_tim_4",
      "Cum_groo_tim_5"
    )
  )

write.csv2(Cum_test_groo,
           file.path(dataPath, "Descriptives/Tables/Test_Cum_Groom_Time.csv"),
           row.names = FALSE)

Cum_test_groo <-
  pivot_longer(
    Cum_test_groo,
    cols = 3:7,
    names_to = "Minute",
    values_to = "groo_Time"
  )

Cum_test_groo$Minute <- as.factor(Cum_test_groo$Minute)

Cum_test_groo_summary = describeBy(
  Cum_test_groo$groo_Time,
  list(Cum_test_groo$Task, Cum_test_groo$Minute),
  mat = TRUE,
  digits = 2
)

Cum_test_groo_plot <-
  ggplot(Cum_test_groo_summary,
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
    labels = c("1", "2", "3", "4", "5"),
    limits = c(
      "Cum_groo_tim_1",
      "Cum_groo_tim_2",
      "Cum_groo_tim_3",
      "Cum_groo_tim_4",
      "Cum_groo_tim_5"
    )
  ) +
  scale_fill_grey(
    name = "Task",
    labels = c(paste0(
      "OPR (N=", mean(Cum_test_dist_summary$n), ")"
    ),
    paste0(
      "NOR (N=", mean(Cum_test_dist_summary$n), ")"
    )),
    limits = c("OPR", "NOR")
  ) +
  ggtitle("Test | Time spent grooming (cumulative)") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Bin_test_groo <-
  subset(
    test,
    select = c(
      "Animal",
      "Task",
      "Bin_groo_tim_1",
      "Bin_groo_tim_2",
      "Bin_groo_tim_3",
      "Bin_groo_tim_4",
      "Bin_groo_tim_5"
    )
  )

write.csv2(Bin_test_groo,
           file.path(dataPath, "Descriptives/Tables/Test_Bin_Groom_Time.csv"),
           row.names = FALSE)

Bin_test_groo <-
  pivot_longer(
    Bin_test_groo,
    cols = 3:7,
    names_to = "Minute",
    values_to = "groo_Time"
  )

Bin_test_groo$Minute <- as.factor(Bin_test_groo$Minute)

Bin_test_groo_summary = describeBy(
  Bin_test_groo$groo_Time,
  list(Bin_test_groo$Task, Bin_test_groo$Minute),
  mat = TRUE,
  digits = 2
)

Bin_test_groo_plot <-
  ggplot(Bin_test_groo_summary,
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
    labels = c("1", "2", "3", "4", "5"),
    limits = c(
      "Bin_groo_tim_1",
      "Bin_groo_tim_2",
      "Bin_groo_tim_3",
      "Bin_groo_tim_4",
      "Bin_groo_tim_5"
    )
  ) +
  scale_fill_grey(
    name = "Task",
    labels = c(paste0(
      "OPR (N=", mean(Bin_test_dist_summary$n), ")"
    ),
    paste0(
      "NOR (N=", mean(Bin_test_dist_summary$n), ")"
    )),
    limits = c("OPR", "NOR")
  ) +
  ggtitle("Test | Time spent grooming (non-cumulative)") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Cum_test_groo_plot
Bin_test_groo_plot

ggsave(
  Cum_test_groo_plot,
  path = paste0(dataPath, "Descriptives/Plots/Test/Cumulative"),
  filename = "04-Grooming_Time.pdf"
)
ggsave(
  Bin_test_groo_plot,
  path = paste0(dataPath, "Descriptives/Plots/Test/Non_cumulative"),
  filename = "04-Grooming_Time.pdf"
)

# 05 - Rearing event count  -----------------------------------------------
Cum_test_rear_count <-
  subset(
    test,
    select = c(
      "Animal",
      "Task",
      "Cum_rear_cnt_1",
      "Cum_rear_cnt_2",
      "Cum_rear_cnt_3",
      "Cum_rear_cnt_4",
      "Cum_rear_cnt_5"
    )
  )

write.csv2(Cum_test_rear_count,
           file.path(dataPath, "Descriptives/Tables/Test_Cum_Rear_Count.csv"),
           row.names = FALSE)

Cum_test_rear_count <-
  pivot_longer(
    Cum_test_rear_count,
    cols = 3:7,
    names_to = "Minute",
    values_to = "Rear_Count"
  )

Cum_test_rear_count$Minute <- as.factor(Cum_test_rear_count$Minute)

Cum_test_rear_count_summary = describeBy(
  Cum_test_rear_count$Rear_Count,
  list(Cum_test_rear_count$Task, Cum_test_rear_count$Minute),
  mat = TRUE,
  digits = 2
)

Cum_test_rear_count_plot <-
  ggplot(Cum_test_rear_count_summary,
         aes(x = group2, y = mean, fill = group1)) +
  geom_bar(stat = 'identity',
           position = dodge,
           width = .8) +
  geom_errorbar(limits, position = dodge, width = 0.3) +
  theme_minimal() +
  scale_y_continuous(name = "Rearing event count",
                     breaks = seq(0, 120, 10),
                     limits = c(0, 120)) +
  scale_x_discrete(
    name = "Minute",
    labels = c("1", "2", "3", "4", "5"),
    limits = c(
      "Cum_rear_cnt_1",
      "Cum_rear_cnt_2",
      "Cum_rear_cnt_3",
      "Cum_rear_cnt_4",
      "Cum_rear_cnt_5"
    )
  ) +
  scale_fill_grey(
    name = "Task",
    labels = c(paste0(
      "OPR (N=", mean(Cum_test_dist_summary$n), ")"
    ),
    paste0(
      "NOR (N=", mean(Cum_test_dist_summary$n), ")"
    )),
    limits = c("OPR", "NOR")
  ) +
  ggtitle("Test | Rearing event count (cumulative)") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Bin_test_rear_count <-
  subset(
    test,
    select = c(
      "Animal",
      "Task",
      "Bin_rear_cnt_1",
      "Bin_rear_cnt_2",
      "Bin_rear_cnt_3",
      "Bin_rear_cnt_4",
      "Bin_rear_cnt_5"
    )
  )

write.csv2(Bin_test_rear_count,
           file.path(dataPath, "Descriptives/Tables/Test_Bin_Rear_Count.csv"),
           row.names = FALSE)

Bin_test_rear_count <-
  pivot_longer(
    Bin_test_rear_count,
    cols = 3:7,
    names_to = "Minute",
    values_to = "Rear_Count"
  )

Bin_test_rear_count$Minute <- as.factor(Bin_test_rear_count$Minute)

Bin_test_rear_count_summary = describeBy(
  Bin_test_rear_count$Rear_Count,
  list(Bin_test_rear_count$Task, Bin_test_rear_count$Minute),
  mat = TRUE,
  digits = 2
)

Bin_test_rear_count_plot <-
  ggplot(Bin_test_rear_count_summary,
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
    labels = c("1", "2", "3", "4", "5"),
    limits = c(
      "Bin_rear_cnt_1",
      "Bin_rear_cnt_2",
      "Bin_rear_cnt_3",
      "Bin_rear_cnt_4",
      "Bin_rear_cnt_5"
    )
  ) +
  scale_fill_grey(
    name = "Task",
    labels = c(paste0(
      "OPR (N=", mean(Bin_test_dist_summary$n), ")"
    ),
    paste0(
      "NOR (N=", mean(Bin_test_dist_summary$n), ")"
    )),
    limits = c("OPR", "NOR")
  ) +
  ggtitle("Test | Rearing event count (non-cumulative)") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Cum_test_rear_count_plot
Bin_test_rear_count_plot

ggsave(
  Cum_test_rear_count_plot,
  path = paste0(dataPath, "Descriptives/Plots/Test/Cumulative"),
  filename = "05-Rearing_Count.pdf"
)
ggsave(
  Bin_test_rear_count_plot,
  path = paste0(dataPath, "Descriptives/Plots/Test/Non_cumulative"),
  filename = "05-Rearing_Count.pdf"
)

# 06 - Grooming onset -----------------------------------------------------
Groom_onset <-
  subset(test,
         select = c("Animal",
                    "Task",
                    "Groo_latency"))

write.csv2(Groom_onset,
           file.path(dataPath, "Descriptives/Tables/Test_Groom_onset.csv"),
           row.names = FALSE)

Groom_onset_summary = describeBy(
  Groom_onset$Groo_latency,
  list(Groom_onset$Task),
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
    labels = c(paste0(
      "OPR (N=", mean(Bin_hab_dist_summary$n), ")"
    ),
    paste0(
      "NOR (N=", mean(Bin_hab_dist_summary$n), ")"
    )),
    limits = c("OPR", "NOR")
  ) +
  ggtitle("Test | Grooming onset") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Groom_onset_plot

ggsave(
  Groom_onset_plot,
  path = paste0(dataPath, "Descriptives/Plots/Test/General"),
  filename = "01-Grooming_Onset.pdf"
)

# 07 - Total object exploration time --------------------------------------
Test_total_exploration <-
  subset(test,
         select = c("Animal",
                    "Task",
                    "Total_exp_time"))

write.csv2(Test_total_exploration,
           file.path(dataPath, "Descriptives/Tables/Test_Total_Obj_exploration.csv"),
           row.names = FALSE)

Test_total_exploration_sum = describeBy(
  Test_total_exploration$Total_exp_time,
  list(Test_total_exploration$Task),
  mat = TRUE,
  digits = 2
)

Test_total_exploratio_plot <-
  ggplot(Test_total_exploration_sum,
         aes(x = group1, y = mean)) +
  geom_bar(stat = 'identity',
           position = dodge,
           width = .8) +
  geom_errorbar(limits, position = dodge, width = 0.3) +
  theme_minimal() +
  scale_y_continuous(name = "Total exploration time [s]",
                     breaks = seq(0, 10, 2),
                     limits = c(0, 10)) +
  scale_x_discrete(
    name = "Task",
    labels = c(
      paste0("OPR (N=", mean(Test_total_exploration_sum$n), ")"),
      paste0("NOR (N=", mean(Test_total_exploration_sum$n), ")")
    ),
    limits = c("OPR", "NOR")
  ) +
  ggtitle("Test| Total exploration time") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Test_total_exploratio_plot

ggsave(
  Test_total_exploratio_plot,
  path = paste0(dataPath, "Descriptives/Plots/Test/General"),
  filename = "02-Total_exploration_time.pdf"
)
# 08 - Exploration onset --------------------------------------------------

# 09 - Discrimination ratio -----------------------------------------------
Cum_DR <-
  subset(
    test,
    select = c(
      "Animal",
      "Task",
      "Cum_DiRa_min_1",
      "Cum_DiRa_min_2",
      "Cum_DiRa_min_3",
      "Cum_DiRa_min_4",
      "Cum_DiRa_min_5"
    )
  )

write.csv2(Cum_DR,
           file.path(dataPath, "Descriptives/Tables/Test_Cum_DR.csv"),
           row.names = FALSE)

Cum_DR <-
  pivot_longer(Cum_DR,
               cols = 3:7 ,
               names_to = "Minute",
               values_to = "DR")
Cum_DR$Minute <- as.factor(Cum_DR$Minute)

Cum_DR_sum = describeBy(Cum_DR$DR,
                        list(Cum_DR$Task, Cum_DR$Minute),
                        mat = TRUE,
                        digits = 2)

DR_Cum_plot <-
  ggplot(data = Cum_DR_sum, aes(x = group2, y = mean, fill = group1)) +
  geom_bar(stat = 'identity',
           position = dodge,
           width = .8) +
  geom_errorbar(limits, position = dodge, width = 0.3) +
  geom_dotplot(
    data = Cum_DR,
    aes(x = Minute, y = DR, fill = Task),
    binaxis = 'y',
    stackdir = 'center',
    dotsize = .5
  ) +
  theme_classic() +
  scale_y_continuous(name = "DR",
                     breaks = seq(-1, 1, 0.1),
                     limits = c(-1, 1)) +
  scale_x_discrete(
    name = "Minute",
    labels = c("1", "2", "3", "4", "5"),
    limits = c(
      "Cum_DiRa_min_1",
      "Cum_DiRa_min_2",
      "Cum_DiRa_min_3",
      "Cum_DiRa_min_4",
      "Cum_DiRa_min_5"
    )
  ) +
  scale_fill_grey(
    name = "Task",
    labels = c(paste0("OPR (N=", mean(Cum_DR_sum$n), ")"),
               paste0("NOR (N=", mean(Cum_DR_sum$n), ")")),
    limits = c("OPR", "NOR")
  ) +
  ggtitle("Test | Discrimination ratio (cumulative)") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)


Cum_DR$AniTreat <-  paste(Cum_DR$Animal, Cum_DR$Task)

DR_Cum_lines <- ggplot(Cum_DR, aes(x = Minute, y = DR)) +
  geom_point(aes(group = AniTreat, color = AniTreat)) +
  geom_line(aes(group = AniTreat, color = AniTreat)) +
  xlab("Minute") +
  ylab("DR") +
  labs(color = "Animal / Condition") +
  theme_classic() +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

Bin_DR <-
  subset(
    test,
    select = c(
      "Animal",
      "Task",
      "Bin_DiRa_min_1",
      "Bin_DiRa_min_2",
      "Bin_DiRa_min_3",
      "Bin_DiRa_min_4",
      "Bin_DiRa_min_5"
    )
  )

write.csv2(Bin_DR,
           file.path(dataPath, "Descriptives/Tables/Test_Bin_DR.csv"),
           row.names = FALSE)

Bin_DR <-
  pivot_longer(Bin_DR,
               cols = 3:7 ,
               names_to = "Minute",
               values_to = "DR")
Bin_DR$Minute <- as.factor(Bin_DR$Minute)

Bin_DR_sum = describeBy(Bin_DR$DR,
                        list(Bin_DR$Task, Bin_DR$Minute),
                        mat = TRUE,
                        digits = 2)

DR_bin_plot <-
  ggplot(data = Bin_DR_sum, aes(x = group2, y = mean, fill = group1)) +
  geom_bar(stat = 'identity',
           position = dodge,
           width = .8) +
  geom_errorbar(limits, position = dodge, width = 0.3) +
  geom_dotplot(
    data = Bin_DR,
    aes(x = Minute, y = DR, fill = Task),
    binaxis = 'y',
    stackdir = 'center',
    dotsize = .5
  ) +
  theme_classic() +
  scale_y_continuous(name = "DR",
                     breaks = seq(-1, 1, 0.1),
                     limits = c(-1, 1)) +
  scale_x_discrete(
    name = "Minute",
    labels = c("1", "2", "3", "4", "5"),
    limits = c(
      "Bin_DiRa_min_1",
      "Bin_DiRa_min_2",
      "Bin_DiRa_min_3",
      "Bin_DiRa_min_4",
      "Bin_DiRa_min_5"
    )
  ) +
  scale_fill_grey(
    name = "Task",
    labels = c(paste0("OPR (N=", mean(Bin_DR_sum$n), ")"),
               paste0("NOR (N=", mean(Bin_DR_sum$n), ")")),
    limits = c("OPR", "NOR")
  ) +
  ggtitle("Test | Discrimination ratio (non cumulative)") +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)


Bin_DR$AniTreat <-  paste(Bin_DR$Animal, Bin_DR$Task)

DR_bin_lines <- ggplot(Bin_DR, aes(x = Minute, y = DR)) +
  geom_point(aes(group = AniTreat, color = AniTreat)) +
  geom_line(aes(group = AniTreat, color = AniTreat)) +
  xlab("Minute") +
  ylab("DR") +
  labs(color = "Animal / Condition") +
  theme_classic() +
  geom_hline(colour = "black",
             yintercept = 0,
             size = .1)

DR_Cum_plot
DR_Cum_lines
DR_bin_plot
DR_bin_lines


ggsave(DR_Cum_plot,
       path = paste0(dataPath, "Descriptives/Plots/Test/Cumulative"),
       filename = "06-DR_point.pdf")
ggsave(DR_Cum_lines,
       path = paste0(dataPath, "Descriptives/Plots/Test/Cumulative"),
       filename = "07-DR_spaghetti.pdf")

ggsave(DR_bin_plot,
       path = paste0(dataPath, "Descriptives/Plots/Test/Non_cumulative"),
       filename = "06-DR_point.pdf")
ggsave(DR_bin_lines,
       path = paste0(dataPath, "Descriptives/Plots/Test/Non_cumulative"),
       filename = "07-DR_spaghetti.pdf")
