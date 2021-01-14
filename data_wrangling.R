# Data wrangling
## This script takes the output from AnyMaze and curates the data matrix for all further analyses
## maximilian.harkotte@gmail.com - December 2020

rm(list = ls()) # clear workspace
cat("\014") # clear console


# 0 - Load packages -------------------------------------------------------
library(tidyverse)

# 1 - Source file ---------------------------------------------------------
dataPath <-
  "Y:/Max/OPTO_Control/Analyses/" # Make sure this path directs to the .csv files
setwd(dataPath)

# 2 - Read in data --------------------------------------------------------
hab <-
  read.csv2("100-MPV0120-Habituation.csv",
            header = TRUE,
            sep = ",")

hab_ref <- read.csv2("100-MPV0120-Habituation_reference.csv",
                     header = TRUE,
                     sep = ",")

enc <-
  read.csv2("100-MPV0120-Encoding.csv",
            header = TRUE,
            sep = ",")

enc_rear_groom <-   read.csv2("100-MPV0120-Encoding_Rear_Groom.csv",
                              header = TRUE,
                              sep = ",")

enc_ref <- read.csv2("100-MPV0120-Encoding_reference.csv",
                     header = TRUE,
                     sep = ",")

test <-
  read.csv2("100-MPV0120-Test.csv",
            header = TRUE,
            sep = ",")

test_rear_groom <-   read.csv2("100-MPV0120-Test_Rear_Groom.csv",
                               header = TRUE,
                               sep = ",")

test_ref <- read.csv2("100-MPV0120-Test_reference.csv",
                      header = TRUE,
                      sep = ",")


# 3 - Data Wrangling ------------------------------------------------------
## Rename Col Names of Habituation tables
hab_clean <- hab %>%
  rename(
    Cum_dist_min_1 = Cum_1..Distance,
    Cum_dist_min_2 = Cum_2..Distance,
    Cum_dist_min_3 = Cum_3..Distance,
    Cum_dist_min_4 = Cum_4..Distance,
    Cum_dist_min_5 = Cum_5..Distance,
    Cum_dist_min_6 = Cum_6..Distance,
    Cum_dist_min_7 = Cum_7..Distance,
    Cum_dist_min_8 = Cum_8..Distance,
    Cum_dist_min_9 = Cum_9..Distance,
    Cum_dist_min_10 = Cum_10..Distance,
    Bin_dist_min_1 = Bin_1..Distance,
    Bin_dist_min_2 = Bin_2..Distance,
    Bin_dist_min_3 = Bin_3..Distance,
    Bin_dist_min_4 = Bin_4..Distance,
    Bin_dist_min_5 = Bin_5..Distance,
    Bin_dist_min_6 = Bin_6..Distance,
    Bin_dist_min_7 = Bin_7..Distance,
    Bin_dist_min_8 = Bin_8..Distance,
    Bin_dist_min_9 = Bin_9..Distance,
    Bin_dist_min_10 = Bin_10..Distance,
    Cum_velo_min_1 = Cum_1..Mean.speed,
    Cum_velo_min_2 = Cum_2..Mean.speed,
    Cum_velo_min_3 = Cum_3..Mean.speed,
    Cum_velo_min_4 = Cum_4..Mean.speed,
    Cum_velo_min_5 = Cum_5..Mean.speed,
    Cum_velo_min_6 = Cum_6..Mean.speed,
    Cum_velo_min_7 = Cum_7..Mean.speed,
    Cum_velo_min_8 = Cum_8..Mean.speed,
    Cum_velo_min_9 = Cum_9..Mean.speed,
    Cum_velo_min_10 = Cum_10..Mean.speed,
    Bin_velo_min_1 = Bin_1..Mean.speed,
    Bin_velo_min_2 = Bin_2..Mean.speed,
    Bin_velo_min_3 = Bin_3..Mean.speed,
    Bin_velo_min_4 = Bin_4..Mean.speed,
    Bin_velo_min_5 = Bin_5..Mean.speed,
    Bin_velo_min_6 = Bin_6..Mean.speed,
    Bin_velo_min_7 = Bin_7..Mean.speed,
    Bin_velo_min_8 = Bin_8..Mean.speed,
    Bin_velo_min_9 = Bin_9..Mean.speed,
    Bin_velo_min_10 = Bin_10..Mean.speed,
    Cum_rear_cnt_1 = Cum_1..Rearing...number.of.presses,
    Cum_rear_cnt_2 = Cum_2..Rearing...number.of.presses,
    Cum_rear_cnt_3 = Cum_3..Rearing...number.of.presses,
    Cum_rear_cnt_4 = Cum_4..Rearing...number.of.presses,
    Cum_rear_cnt_5 = Cum_5..Rearing...number.of.presses,
    Cum_rear_cnt_6 = Cum_6..Rearing...number.of.presses,
    Cum_rear_cnt_7 = Cum_7..Rearing...number.of.presses,
    Cum_rear_cnt_8 = Cum_8..Rearing...number.of.presses,
    Cum_rear_cnt_9 = Cum_9..Rearing...number.of.presses,
    Cum_rear_cnt_10 = Cum_10..Rearing...number.of.presses,
    Bin_rear_cnt_1 = Bin_1..Rearing...number.of.presses,
    Bin_rear_cnt_2 = Bin_2..Rearing...number.of.presses,
    Bin_rear_cnt_3 = Bin_3..Rearing...number.of.presses,
    Bin_rear_cnt_4 = Bin_4..Rearing...number.of.presses,
    Bin_rear_cnt_5 = Bin_5..Rearing...number.of.presses,
    Bin_rear_cnt_6 = Bin_6..Rearing...number.of.presses,
    Bin_rear_cnt_7 = Bin_7..Rearing...number.of.presses,
    Bin_rear_cnt_8 = Bin_8..Rearing...number.of.presses,
    Bin_rear_cnt_9 = Bin_9..Rearing...number.of.presses,
    Bin_rear_cnt_10 = Bin_10..Rearing...number.of.presses,
    Cum_rear_tim_1 = Cum_1..Rearing...time.pressed,
    Cum_rear_tim_2 = Cum_2..Rearing...time.pressed,
    Cum_rear_tim_3 = Cum_3..Rearing...time.pressed,
    Cum_rear_tim_4 = Cum_4..Rearing...time.pressed,
    Cum_rear_tim_5 = Cum_5..Rearing...time.pressed,
    Cum_rear_tim_6 = Cum_6..Rearing...time.pressed,
    Cum_rear_tim_7 = Cum_7..Rearing...time.pressed,
    Cum_rear_tim_8 = Cum_8..Rearing...time.pressed,
    Cum_rear_tim_9 = Cum_9..Rearing...time.pressed,
    Cum_rear_tim_10 = Cum_10..Rearing...time.pressed,
    Bin_rear_tim_1 = Bin_1..Rearing...time.pressed,
    Bin_rear_tim_2 = Bin_2..Rearing...time.pressed,
    Bin_rear_tim_3 = Bin_3..Rearing...time.pressed,
    Bin_rear_tim_4 = Bin_4..Rearing...time.pressed,
    Bin_rear_tim_5 = Bin_5..Rearing...time.pressed,
    Bin_rear_tim_6 = Bin_6..Rearing...time.pressed,
    Bin_rear_tim_7 = Bin_7..Rearing...time.pressed,
    Bin_rear_tim_8 = Bin_8..Rearing...time.pressed,
    Bin_rear_tim_9 = Bin_9..Rearing...time.pressed,
    Bin_rear_tim_10 = Bin_10..Rearing...time.pressed,
    Rear_latency = Rearing...latency.1st.press,
    Cum_groo_cnt_1 = Cum_1..Grooming...number.of.presses,
    Cum_groo_cnt_2 = Cum_2..Grooming...number.of.presses,
    Cum_groo_cnt_3 = Cum_3..Grooming...number.of.presses,
    Cum_groo_cnt_4 = Cum_4..Grooming...number.of.presses,
    Cum_groo_cnt_5 = Cum_5..Grooming...number.of.presses,
    Cum_groo_cnt_6 = Cum_6..Grooming...number.of.presses,
    Cum_groo_cnt_7 = Cum_7..Grooming...number.of.presses,
    Cum_groo_cnt_8 = Cum_8..Grooming...number.of.presses,
    Cum_groo_cnt_9 = Cum_9..Grooming...number.of.presses,
    Cum_groo_cnt_10 = Cum_10..Grooming...number.of.presses,
    Bin_groo_cnt_1 = Bin_1..Grooming...number.of.presses,
    Bin_groo_cnt_2 = Bin_2..Grooming...number.of.presses,
    Bin_groo_cnt_3 = Bin_3..Grooming...number.of.presses,
    Bin_groo_cnt_4 = Bin_4..Grooming...number.of.presses,
    Bin_groo_cnt_5 = Bin_5..Grooming...number.of.presses,
    Bin_groo_cnt_6 = Bin_6..Grooming...number.of.presses,
    Bin_groo_cnt_7 = Bin_7..Grooming...number.of.presses,
    Bin_groo_cnt_8 = Bin_8..Grooming...number.of.presses,
    Bin_groo_cnt_9 = Bin_9..Grooming...number.of.presses,
    Bin_groo_cnt_10 = Bin_10..Grooming...number.of.presses,
    Cum_groo_tim_1 = Cum_1..Grooming...time.pressed,
    Cum_groo_tim_2 = Cum_2..Grooming...time.pressed,
    Cum_groo_tim_3 = Cum_3..Grooming...time.pressed,
    Cum_groo_tim_4 = Cum_4..Grooming...time.pressed,
    Cum_groo_tim_5 = Cum_5..Grooming...time.pressed,
    Cum_groo_tim_6 = Cum_6..Grooming...time.pressed,
    Cum_groo_tim_7 = Cum_7..Grooming...time.pressed,
    Cum_groo_tim_8 = Cum_8..Grooming...time.pressed,
    Cum_groo_tim_9 = Cum_9..Grooming...time.pressed,
    Cum_groo_tim_10 = Cum_10..Grooming...time.pressed,
    Bin_groo_tim_1 = Bin_1..Grooming...time.pressed,
    Bin_groo_tim_2 = Bin_2..Grooming...time.pressed,
    Bin_groo_tim_3 = Bin_3..Grooming...time.pressed,
    Bin_groo_tim_4 = Bin_4..Grooming...time.pressed,
    Bin_groo_tim_5 = Bin_5..Grooming...time.pressed,
    Bin_groo_tim_6 = Bin_6..Grooming...time.pressed,
    Bin_groo_tim_7 = Bin_7..Grooming...time.pressed,
    Bin_groo_tim_8 = Bin_8..Grooming...time.pressed,
    Bin_groo_tim_9 = Bin_9..Grooming...time.pressed,
    Bin_groo_tim_10 = Bin_10..Grooming...time.pressed,
    Groo_latency = Grooming...latency.1st.press,
    Cum_BaLe_tim_1 = Cum_1..BL...time,
    Cum_BaLe_tim_2 = Cum_2..BL...time,
    Cum_BaLe_tim_3 = Cum_3..BL...time,
    Cum_BaLe_tim_4 = Cum_4..BL...time,
    Cum_BaLe_tim_5 = Cum_5..BL...time,
    Cum_BaLe_tim_6 = Cum_6..BL...time,
    Cum_BaLe_tim_7 = Cum_7..BL...time,
    Cum_BaLe_tim_8 = Cum_8..BL...time,
    Cum_BaLe_tim_9 = Cum_9..BL...time,
    Cum_BaLe_tim_10 = Cum_10..BL...time,
    Bin_BaLe_tim_1 = Bin_1..BL...time,
    Bin_BaLe_tim_2 = Bin_2..BL...time,
    Bin_BaLe_tim_3 = Bin_3..BL...time,
    Bin_BaLe_tim_4 = Bin_4..BL...time,
    Bin_BaLe_tim_5 = Bin_5..BL...time,
    Bin_BaLe_tim_6 = Bin_6..BL...time,
    Bin_BaLe_tim_7 = Bin_7..BL...time,
    Bin_BaLe_tim_8 = Bin_8..BL...time,
    Bin_BaLe_tim_9 = Bin_9..BL...time,
    Bin_BaLe_tim_10 = Bin_10..BL...time,
    Cum_BaRi_tim_1 = Cum_1..BR...time,
    Cum_BaRi_tim_2 = Cum_2..BR...time,
    Cum_BaRi_tim_3 = Cum_3..BR...time,
    Cum_BaRi_tim_4 = Cum_4..BR...time,
    Cum_BaRi_tim_5 = Cum_5..BR...time,
    Cum_BaRi_tim_6 = Cum_6..BR...time,
    Cum_BaRi_tim_7 = Cum_7..BR...time,
    Cum_BaRi_tim_8 = Cum_8..BR...time,
    Cum_BaRi_tim_9 = Cum_9..BR...time,
    Cum_BaRi_tim_10 = Cum_10..BR...time,
    Bin_BaRi_tim_1 = Bin_1..BR...time,
    Bin_BaRi_tim_2 = Bin_2..BR...time,
    Bin_BaRi_tim_3 = Bin_3..BR...time,
    Bin_BaRi_tim_4 = Bin_4..BR...time,
    Bin_BaRi_tim_5 = Bin_5..BR...time,
    Bin_BaRi_tim_6 = Bin_6..BR...time,
    Bin_BaRi_tim_7 = Bin_7..BR...time,
    Bin_BaRi_tim_8 = Bin_8..BR...time,
    Bin_BaRi_tim_9 = Bin_9..BR...time,
    Bin_BaRi_tim_10 = Bin_10..BR...time,
    Cum_FrLe_tim_1 = Cum_1..FL...time,
    Cum_FrLe_tim_2 = Cum_2..FL...time,
    Cum_FrLe_tim_3 = Cum_3..FL...time,
    Cum_FrLe_tim_4 = Cum_4..FL...time,
    Cum_FrLe_tim_5 = Cum_5..FL...time,
    Cum_FrLe_tim_6 = Cum_6..FL...time,
    Cum_FrLe_tim_7 = Cum_7..FL...time,
    Cum_FrLe_tim_8 = Cum_8..FL...time,
    Cum_FrLe_tim_9 = Cum_9..FL...time,
    Cum_FrLe_tim_10 = Cum_10..FL...time,
    Bin_FrLe_tim_1 = Bin_1..FL...time,
    Bin_FrLe_tim_2 = Bin_2..FL...time,
    Bin_FrLe_tim_3 = Bin_3..FL...time,
    Bin_FrLe_tim_4 = Bin_4..FL...time,
    Bin_FrLe_tim_5 = Bin_5..FL...time,
    Bin_FrLe_tim_6 = Bin_6..FL...time,
    Bin_FrLe_tim_7 = Bin_7..FL...time,
    Bin_FrLe_tim_8 = Bin_8..FL...time,
    Bin_FrLe_tim_9 = Bin_9..FL...time,
    Bin_FrLe_tim_10 = Bin_10..FL...time,
    Cum_FrRi_tim_1 = Cum_1..FR...time,
    Cum_FrRi_tim_2 = Cum_2..FR...time,
    Cum_FrRi_tim_3 = Cum_3..FR...time,
    Cum_FrRi_tim_4 = Cum_4..FR...time,
    Cum_FrRi_tim_5 = Cum_5..FR...time,
    Cum_FrRi_tim_6 = Cum_6..FR...time,
    Cum_FrRi_tim_7 = Cum_7..FR...time,
    Cum_FrRi_tim_8 = Cum_8..FR...time,
    Cum_FrRi_tim_9 = Cum_9..FR...time,
    Cum_FrRi_tim_10 = Cum_10..FR...time,
    Bin_FrRi_tim_1 = Bin_1..FR...time,
    Bin_FrRi_tim_2 = Bin_2..FR...time,
    Bin_FrRi_tim_3 = Bin_3..FR...time,
    Bin_FrRi_tim_4 = Bin_4..FR...time,
    Bin_FrRi_tim_5 = Bin_5..FR...time,
    Bin_FrRi_tim_6 = Bin_6..FR...time,
    Bin_FrRi_tim_7 = Bin_7..FR...time,
    Bin_FrRi_tim_8 = Bin_8..FR...time,
    Bin_FrRi_tim_9 = Bin_9..FR...time,
    Bin_FrRi_tim_10 = Bin_10..FR...time,
    Cum_cent_tim_1 = Cum_1..Center...time,
    Cum_cent_tim_2 = Cum_2..Center...time,
    Cum_cent_tim_3 = Cum_3..Center...time,
    Cum_cent_tim_4 = Cum_4..Center...time,
    Cum_cent_tim_5 = Cum_5..Center...time,
    Cum_cent_tim_6 = Cum_6..Center...time,
    Cum_cent_tim_7 = Cum_7..Center...time,
    Cum_cent_tim_8 = Cum_8..Center...time,
    Cum_cent_tim_9 = Cum_9..Center...time,
    Cum_cent_tim_10 = Cum_10..Center...time,
    Bin_cent_tim_1 = Bin_1..Center...time,
    Bin_cent_tim_2 = Bin_2..Center...time,
    Bin_cent_tim_3 = Bin_3..Center...time,
    Bin_cent_tim_4 = Bin_4..Center...time,
    Bin_cent_tim_5 = Bin_5..Center...time,
    Bin_cent_tim_6 = Bin_6..Center...time,
    Bin_cent_tim_7 = Bin_7..Center...time,
    Bin_cent_tim_8 = Bin_8..Center...time,
    Bin_cent_tim_9 = Bin_9..Center...time,
    Bin_cent_tim_10 = Bin_10..Center...time,
    Cum_peri_tim_1 = Cum_1..Periphery...time,
    Cum_peri_tim_2 = Cum_2..Periphery...time,
    Cum_peri_tim_3 = Cum_3..Periphery...time,
    Cum_peri_tim_4 = Cum_4..Periphery...time,
    Cum_peri_tim_5 = Cum_5..Periphery...time,
    Cum_peri_tim_6 = Cum_6..Periphery...time,
    Cum_peri_tim_7 = Cum_7..Periphery...time,
    Cum_peri_tim_8 = Cum_8..Periphery...time,
    Cum_peri_tim_9 = Cum_9..Periphery...time,
    Cum_peri_tim_10 = Cum_10..Periphery...time,
    Bin_peri_tim_1 = Bin_1..Periphery...time,
    Bin_peri_tim_2 = Bin_2..Periphery...time,
    Bin_peri_tim_3 = Bin_3..Periphery...time,
    Bin_peri_tim_4 = Bin_4..Periphery...time,
    Bin_peri_tim_5 = Bin_5..Periphery...time,
    Bin_peri_tim_6 = Bin_6..Periphery...time,
    Bin_peri_tim_7 = Bin_7..Periphery...time,
    Bin_peri_tim_8 = Bin_8..Periphery...time,
    Bin_peri_tim_9 = Bin_9..Periphery...time,
    Bin_peri_tim_10 = Bin_10..Periphery...time,
  )

## Rename Col Names of Encoding tables
enc_clean <- enc %>%
  rename(
    Cum_dist_min_1 = Cum_1..Distance,
    Cum_dist_min_2 = Cum_2..Distance,
    Cum_dist_min_3 = Cum_3..Distance,
    Cum_dist_min_4 = Cum_4..Distance,
    Cum_dist_min_5 = Cum_5..Distance,
    Cum_dist_min_6 = Cum_6..Distance,
    Cum_dist_min_7 = Cum_7..Distance,
    Cum_dist_min_8 = Cum_8..Distance,
    Cum_dist_min_9 = Cum_9..Distance,
    Cum_dist_min_10 = Cum_10..Distance,
    Bin_dist_min_1 = Bin_1..Distance,
    Bin_dist_min_2 = Bin_2..Distance,
    Bin_dist_min_3 = Bin_3..Distance,
    Bin_dist_min_4 = Bin_4..Distance,
    Bin_dist_min_5 = Bin_5..Distance,
    Bin_dist_min_6 = Bin_6..Distance,
    Bin_dist_min_7 = Bin_7..Distance,
    Bin_dist_min_8 = Bin_8..Distance,
    Bin_dist_min_9 = Bin_9..Distance,
    Bin_dist_min_10 = Bin_10..Distance,
    Cum_velo_min_1 = Cum_1..Mean.speed,
    Cum_velo_min_2 = Cum_2..Mean.speed,
    Cum_velo_min_3 = Cum_3..Mean.speed,
    Cum_velo_min_4 = Cum_4..Mean.speed,
    Cum_velo_min_5 = Cum_5..Mean.speed,
    Cum_velo_min_6 = Cum_6..Mean.speed,
    Cum_velo_min_7 = Cum_7..Mean.speed,
    Cum_velo_min_8 = Cum_8..Mean.speed,
    Cum_velo_min_9 = Cum_9..Mean.speed,
    Cum_velo_min_10 = Cum_10..Mean.speed,
    Bin_velo_min_1 = Bin_1..Mean.speed,
    Bin_velo_min_2 = Bin_2..Mean.speed,
    Bin_velo_min_3 = Bin_3..Mean.speed,
    Bin_velo_min_4 = Bin_4..Mean.speed,
    Bin_velo_min_5 = Bin_5..Mean.speed,
    Bin_velo_min_6 = Bin_6..Mean.speed,
    Bin_velo_min_7 = Bin_7..Mean.speed,
    Bin_velo_min_8 = Bin_8..Mean.speed,
    Bin_velo_min_9 = Bin_9..Mean.speed,
    Bin_velo_min_10 = Bin_10..Mean.speed,
    Cum_BaLe_exp_1 = Cum_1..Leftback_encoding...time.pressed,
    Cum_BaLe_exp_2 = Cum_2..Leftback_encoding...time.pressed,
    Cum_BaLe_exp_3 = Cum_3..Leftback_encoding...time.pressed,
    Cum_BaLe_exp_4 = Cum_4..Leftback_encoding...time.pressed,
    Cum_BaLe_exp_5 = Cum_5..Leftback_encoding...time.pressed,
    Cum_BaLe_exp_6 = Cum_6..Leftback_encoding...time.pressed,
    Cum_BaLe_exp_7 = Cum_7..Leftback_encoding...time.pressed,
    Cum_BaLe_exp_8 = Cum_8..Leftback_encoding...time.pressed,
    Cum_BaLe_exp_9 = Cum_9..Leftback_encoding...time.pressed,
    Cum_BaLe_exp_10 = Cum_10..Leftback_encoding...time.pressed,
    Bin_BaLe_exp_1 = Bin_1..Leftback_encoding...time.pressed,
    Bin_BaLe_exp_2 = Bin_2..Leftback_encoding...time.pressed,
    Bin_BaLe_exp_3 = Bin_3..Leftback_encoding...time.pressed,
    Bin_BaLe_exp_4 = Bin_4..Leftback_encoding...time.pressed,
    Bin_BaLe_exp_5 = Bin_5..Leftback_encoding...time.pressed,
    Bin_BaLe_exp_6 = Bin_6..Leftback_encoding...time.pressed,
    Bin_BaLe_exp_7 = Bin_7..Leftback_encoding...time.pressed,
    Bin_BaLe_exp_8 = Bin_8..Leftback_encoding...time.pressed,
    Bin_BaLe_exp_9 = Bin_9..Leftback_encoding...time.pressed,
    Bin_BaLe_exp_10 = Bin_10..Leftback_encoding...time.pressed,
    BaLe_exp_Latency = Leftback_encoding...latency.1st.press,
    Cum_FrRi_exp_1 = Cum_1..Rightfront_encoding...time.pressed,
    Cum_FrRi_exp_2 = Cum_2..Rightfront_encoding...time.pressed,
    Cum_FrRi_exp_3 = Cum_3..Rightfront_encoding...time.pressed,
    Cum_FrRi_exp_4 = Cum_4..Rightfront_encoding...time.pressed,
    Cum_FrRi_exp_5 = Cum_5..Rightfront_encoding...time.pressed,
    Cum_FrRi_exp_6 = Cum_6..Rightfront_encoding...time.pressed,
    Cum_FrRi_exp_7 = Cum_7..Rightfront_encoding...time.pressed,
    Cum_FrRi_exp_8 = Cum_8..Rightfront_encoding...time.pressed,
    Cum_FrRi_exp_9 = Cum_9..Rightfront_encoding...time.pressed,
    Cum_FrRi_exp_10 = Cum_10..Rightfront_encoding...time.pressed,
    Bin_FrRi_exp_1 = Bin_1..Rightfront_encoding...time.pressed,
    Bin_FrRi_exp_2 = Bin_2..Rightfront_encoding...time.pressed,
    Bin_FrRi_exp_3 = Bin_3..Rightfront_encoding...time.pressed,
    Bin_FrRi_exp_4 = Bin_4..Rightfront_encoding...time.pressed,
    Bin_FrRi_exp_5 = Bin_5..Rightfront_encoding...time.pressed,
    Bin_FrRi_exp_6 = Bin_6..Rightfront_encoding...time.pressed,
    Bin_FrRi_exp_7 = Bin_7..Rightfront_encoding...time.pressed,
    Bin_FrRi_exp_8 = Bin_8..Rightfront_encoding...time.pressed,
    Bin_FrRi_exp_9 = Bin_9..Rightfront_encoding...time.pressed,
    Bin_FrRi_exp_10 = Bin_10..Rightfront_encoding...time.pressed,
    FrRi_exp_Latency = Rightfront_encoding...latency.1st.press,
    Cum_BaLe_tim_1 = Cum_1..BL...time,
    Cum_BaLe_tim_2 = Cum_2..BL...time,
    Cum_BaLe_tim_3 = Cum_3..BL...time,
    Cum_BaLe_tim_4 = Cum_4..BL...time,
    Cum_BaLe_tim_5 = Cum_5..BL...time,
    Cum_BaLe_tim_6 = Cum_6..BL...time,
    Cum_BaLe_tim_7 = Cum_7..BL...time,
    Cum_BaLe_tim_8 = Cum_8..BL...time,
    Cum_BaLe_tim_9 = Cum_9..BL...time,
    Cum_BaLe_tim_10 = Cum_10..BL...time,
    Bin_BaLe_tim_1 = Bin_1..BL...time,
    Bin_BaLe_tim_2 = Bin_2..BL...time,
    Bin_BaLe_tim_3 = Bin_3..BL...time,
    Bin_BaLe_tim_4 = Bin_4..BL...time,
    Bin_BaLe_tim_5 = Bin_5..BL...time,
    Bin_BaLe_tim_6 = Bin_6..BL...time,
    Bin_BaLe_tim_7 = Bin_7..BL...time,
    Bin_BaLe_tim_8 = Bin_8..BL...time,
    Bin_BaLe_tim_9 = Bin_9..BL...time,
    Bin_BaLe_tim_10 = Bin_10..BL...time,
    Cum_BaRi_tim_1 = Cum_1..BR...time,
    Cum_BaRi_tim_2 = Cum_2..BR...time,
    Cum_BaRi_tim_3 = Cum_3..BR...time,
    Cum_BaRi_tim_4 = Cum_4..BR...time,
    Cum_BaRi_tim_5 = Cum_5..BR...time,
    Cum_BaRi_tim_6 = Cum_6..BR...time,
    Cum_BaRi_tim_7 = Cum_7..BR...time,
    Cum_BaRi_tim_8 = Cum_8..BR...time,
    Cum_BaRi_tim_9 = Cum_9..BR...time,
    Cum_BaRi_tim_10 = Cum_10..BR...time,
    Bin_BaRi_tim_1 = Bin_1..BR...time,
    Bin_BaRi_tim_2 = Bin_2..BR...time,
    Bin_BaRi_tim_3 = Bin_3..BR...time,
    Bin_BaRi_tim_4 = Bin_4..BR...time,
    Bin_BaRi_tim_5 = Bin_5..BR...time,
    Bin_BaRi_tim_6 = Bin_6..BR...time,
    Bin_BaRi_tim_7 = Bin_7..BR...time,
    Bin_BaRi_tim_8 = Bin_8..BR...time,
    Bin_BaRi_tim_9 = Bin_9..BR...time,
    Bin_BaRi_tim_10 = Bin_10..BR...time,
    Cum_FrLe_tim_1 = Cum_1..FL...time,
    Cum_FrLe_tim_2 = Cum_2..FL...time,
    Cum_FrLe_tim_3 = Cum_3..FL...time,
    Cum_FrLe_tim_4 = Cum_4..FL...time,
    Cum_FrLe_tim_5 = Cum_5..FL...time,
    Cum_FrLe_tim_6 = Cum_6..FL...time,
    Cum_FrLe_tim_7 = Cum_7..FL...time,
    Cum_FrLe_tim_8 = Cum_8..FL...time,
    Cum_FrLe_tim_9 = Cum_9..FL...time,
    Cum_FrLe_tim_10 = Cum_10..FL...time,
    Bin_FrLe_tim_1 = Bin_1..FL...time,
    Bin_FrLe_tim_2 = Bin_2..FL...time,
    Bin_FrLe_tim_3 = Bin_3..FL...time,
    Bin_FrLe_tim_4 = Bin_4..FL...time,
    Bin_FrLe_tim_5 = Bin_5..FL...time,
    Bin_FrLe_tim_6 = Bin_6..FL...time,
    Bin_FrLe_tim_7 = Bin_7..FL...time,
    Bin_FrLe_tim_8 = Bin_8..FL...time,
    Bin_FrLe_tim_9 = Bin_9..FL...time,
    Bin_FrLe_tim_10 = Bin_10..FL...time,
    Cum_FrRi_tim_1 = Cum_1..FR...time,
    Cum_FrRi_tim_2 = Cum_2..FR...time,
    Cum_FrRi_tim_3 = Cum_3..FR...time,
    Cum_FrRi_tim_4 = Cum_4..FR...time,
    Cum_FrRi_tim_5 = Cum_5..FR...time,
    Cum_FrRi_tim_6 = Cum_6..FR...time,
    Cum_FrRi_tim_7 = Cum_7..FR...time,
    Cum_FrRi_tim_8 = Cum_8..FR...time,
    Cum_FrRi_tim_9 = Cum_9..FR...time,
    Cum_FrRi_tim_10 = Cum_10..FR...time,
    Bin_FrRi_tim_1 = Bin_1..FR...time,
    Bin_FrRi_tim_2 = Bin_2..FR...time,
    Bin_FrRi_tim_3 = Bin_3..FR...time,
    Bin_FrRi_tim_4 = Bin_4..FR...time,
    Bin_FrRi_tim_5 = Bin_5..FR...time,
    Bin_FrRi_tim_6 = Bin_6..FR...time,
    Bin_FrRi_tim_7 = Bin_7..FR...time,
    Bin_FrRi_tim_8 = Bin_8..FR...time,
    Bin_FrRi_tim_9 = Bin_9..FR...time,
    Bin_FrRi_tim_10 = Bin_10..FR...time,
    Cum_cent_tim_1 = Cum_1..Center...time,
    Cum_cent_tim_2 = Cum_2..Center...time,
    Cum_cent_tim_3 = Cum_3..Center...time,
    Cum_cent_tim_4 = Cum_4..Center...time,
    Cum_cent_tim_5 = Cum_5..Center...time,
    Cum_cent_tim_6 = Cum_6..Center...time,
    Cum_cent_tim_7 = Cum_7..Center...time,
    Cum_cent_tim_8 = Cum_8..Center...time,
    Cum_cent_tim_9 = Cum_9..Center...time,
    Cum_cent_tim_10 = Cum_10..Center...time,
    Bin_cent_tim_1 = Bin_1..Center...time,
    Bin_cent_tim_2 = Bin_2..Center...time,
    Bin_cent_tim_3 = Bin_3..Center...time,
    Bin_cent_tim_4 = Bin_4..Center...time,
    Bin_cent_tim_5 = Bin_5..Center...time,
    Bin_cent_tim_6 = Bin_6..Center...time,
    Bin_cent_tim_7 = Bin_7..Center...time,
    Bin_cent_tim_8 = Bin_8..Center...time,
    Bin_cent_tim_9 = Bin_9..Center...time,
    Bin_cent_tim_10 = Bin_10..Center...time,
    Cum_peri_tim_1 = Cum_1..Periphery...time,
    Cum_peri_tim_2 = Cum_2..Periphery...time,
    Cum_peri_tim_3 = Cum_3..Periphery...time,
    Cum_peri_tim_4 = Cum_4..Periphery...time,
    Cum_peri_tim_5 = Cum_5..Periphery...time,
    Cum_peri_tim_6 = Cum_6..Periphery...time,
    Cum_peri_tim_7 = Cum_7..Periphery...time,
    Cum_peri_tim_8 = Cum_8..Periphery...time,
    Cum_peri_tim_9 = Cum_9..Periphery...time,
    Cum_peri_tim_10 = Cum_10..Periphery...time,
    Bin_peri_tim_1 = Bin_1..Periphery...time,
    Bin_peri_tim_2 = Bin_2..Periphery...time,
    Bin_peri_tim_3 = Bin_3..Periphery...time,
    Bin_peri_tim_4 = Bin_4..Periphery...time,
    Bin_peri_tim_5 = Bin_5..Periphery...time,
    Bin_peri_tim_6 = Bin_6..Periphery...time,
    Bin_peri_tim_7 = Bin_7..Periphery...time,
    Bin_peri_tim_8 = Bin_8..Periphery...time,
    Bin_peri_tim_9 = Bin_9..Periphery...time,
    Bin_peri_tim_10 = Bin_10..Periphery...time,
  )

enc_rear_groom_clean <- enc_rear_groom %>%
  rename(
    Cum_rear_cnt_1 = Cum_1..Rearing...number.of.presses,
    Cum_rear_cnt_2 = Cum_2..Rearing...number.of.presses,
    Cum_rear_cnt_3 = Cum_3..Rearing...number.of.presses,
    Cum_rear_cnt_4 = Cum_4..Rearing...number.of.presses,
    Cum_rear_cnt_5 = Cum_5..Rearing...number.of.presses,
    Cum_rear_cnt_6 = Cum_6..Rearing...number.of.presses,
    Cum_rear_cnt_7 = Cum_7..Rearing...number.of.presses,
    Cum_rear_cnt_8 = Cum_8..Rearing...number.of.presses,
    Cum_rear_cnt_9 = Cum_9..Rearing...number.of.presses,
    Cum_rear_cnt_10 = Cum_10..Rearing...number.of.presses,
    Bin_rear_cnt_1 = Bin_1..Rearing...number.of.presses,
    Bin_rear_cnt_2 = Bin_2..Rearing...number.of.presses,
    Bin_rear_cnt_3 = Bin_3..Rearing...number.of.presses,
    Bin_rear_cnt_4 = Bin_4..Rearing...number.of.presses,
    Bin_rear_cnt_5 = Bin_5..Rearing...number.of.presses,
    Bin_rear_cnt_6 = Bin_6..Rearing...number.of.presses,
    Bin_rear_cnt_7 = Bin_7..Rearing...number.of.presses,
    Bin_rear_cnt_8 = Bin_8..Rearing...number.of.presses,
    Bin_rear_cnt_9 = Bin_9..Rearing...number.of.presses,
    Bin_rear_cnt_10 = Bin_10..Rearing...number.of.presses,
    Cum_rear_tim_1 = Cum_1..Rearing...time.pressed,
    Cum_rear_tim_2 = Cum_2..Rearing...time.pressed,
    Cum_rear_tim_3 = Cum_3..Rearing...time.pressed,
    Cum_rear_tim_4 = Cum_4..Rearing...time.pressed,
    Cum_rear_tim_5 = Cum_5..Rearing...time.pressed,
    Cum_rear_tim_6 = Cum_6..Rearing...time.pressed,
    Cum_rear_tim_7 = Cum_7..Rearing...time.pressed,
    Cum_rear_tim_8 = Cum_8..Rearing...time.pressed,
    Cum_rear_tim_9 = Cum_9..Rearing...time.pressed,
    Cum_rear_tim_10 = Cum_10..Rearing...time.pressed,
    Bin_rear_tim_1 = Bin_1..Rearing...time.pressed,
    Bin_rear_tim_2 = Bin_2..Rearing...time.pressed,
    Bin_rear_tim_3 = Bin_3..Rearing...time.pressed,
    Bin_rear_tim_4 = Bin_4..Rearing...time.pressed,
    Bin_rear_tim_5 = Bin_5..Rearing...time.pressed,
    Bin_rear_tim_6 = Bin_6..Rearing...time.pressed,
    Bin_rear_tim_7 = Bin_7..Rearing...time.pressed,
    Bin_rear_tim_8 = Bin_8..Rearing...time.pressed,
    Bin_rear_tim_9 = Bin_9..Rearing...time.pressed,
    Bin_rear_tim_10 = Bin_10..Rearing...time.pressed,
    Rear_latency = Rearing...latency.1st.press,
    Cum_groo_cnt_1 = Cum_1..Grooming...number.of.presses,
    Cum_groo_cnt_2 = Cum_2..Grooming...number.of.presses,
    Cum_groo_cnt_3 = Cum_3..Grooming...number.of.presses,
    Cum_groo_cnt_4 = Cum_4..Grooming...number.of.presses,
    Cum_groo_cnt_5 = Cum_5..Grooming...number.of.presses,
    Cum_groo_cnt_6 = Cum_6..Grooming...number.of.presses,
    Cum_groo_cnt_7 = Cum_7..Grooming...number.of.presses,
    Cum_groo_cnt_8 = Cum_8..Grooming...number.of.presses,
    Cum_groo_cnt_9 = Cum_9..Grooming...number.of.presses,
    Cum_groo_cnt_10 = Cum_10..Grooming...number.of.presses,
    Bin_groo_cnt_1 = Bin_1..Grooming...number.of.presses,
    Bin_groo_cnt_2 = Bin_2..Grooming...number.of.presses,
    Bin_groo_cnt_3 = Bin_3..Grooming...number.of.presses,
    Bin_groo_cnt_4 = Bin_4..Grooming...number.of.presses,
    Bin_groo_cnt_5 = Bin_5..Grooming...number.of.presses,
    Bin_groo_cnt_6 = Bin_6..Grooming...number.of.presses,
    Bin_groo_cnt_7 = Bin_7..Grooming...number.of.presses,
    Bin_groo_cnt_8 = Bin_8..Grooming...number.of.presses,
    Bin_groo_cnt_9 = Bin_9..Grooming...number.of.presses,
    Bin_groo_cnt_10 = Bin_10..Grooming...number.of.presses,
    Cum_groo_tim_1 = Cum_1..Grooming...time.pressed,
    Cum_groo_tim_2 = Cum_2..Grooming...time.pressed,
    Cum_groo_tim_3 = Cum_3..Grooming...time.pressed,
    Cum_groo_tim_4 = Cum_4..Grooming...time.pressed,
    Cum_groo_tim_5 = Cum_5..Grooming...time.pressed,
    Cum_groo_tim_6 = Cum_6..Grooming...time.pressed,
    Cum_groo_tim_7 = Cum_7..Grooming...time.pressed,
    Cum_groo_tim_8 = Cum_8..Grooming...time.pressed,
    Cum_groo_tim_9 = Cum_9..Grooming...time.pressed,
    Cum_groo_tim_10 = Cum_10..Grooming...time.pressed,
    Bin_groo_tim_1 = Bin_1..Grooming...time.pressed,
    Bin_groo_tim_2 = Bin_2..Grooming...time.pressed,
    Bin_groo_tim_3 = Bin_3..Grooming...time.pressed,
    Bin_groo_tim_4 = Bin_4..Grooming...time.pressed,
    Bin_groo_tim_5 = Bin_5..Grooming...time.pressed,
    Bin_groo_tim_6 = Bin_6..Grooming...time.pressed,
    Bin_groo_tim_7 = Bin_7..Grooming...time.pressed,
    Bin_groo_tim_8 = Bin_8..Grooming...time.pressed,
    Bin_groo_tim_9 = Bin_9..Grooming...time.pressed,
    Bin_groo_tim_10 = Bin_10..Grooming...time.pressed,
    Groo_latency = Grooming...latency.1st.press
  )


## Rename Col Names of Test tables
test_clean <- test %>%
  rename(
    Cum_dist_min_1 = Cum_1..Distance,
    Cum_dist_min_2 = Cum_2..Distance,
    Cum_dist_min_3 = Cum_3..Distance,
    Cum_dist_min_4 = Cum_4..Distance,
    Cum_dist_min_5 = Cum_5..Distance,
    Bin_dist_min_1 = Bin_1..Distance,
    Bin_dist_min_2 = Bin_2..Distance,
    Bin_dist_min_3 = Bin_3..Distance,
    Bin_dist_min_4 = Bin_4..Distance,
    Bin_dist_min_5 = Bin_5..Distance,
    Cum_velo_min_1 = Cum_1..Mean.speed,
    Cum_velo_min_2 = Cum_2..Mean.speed,
    Cum_velo_min_3 = Cum_3..Mean.speed,
    Cum_velo_min_4 = Cum_4..Mean.speed,
    Cum_velo_min_5 = Cum_5..Mean.speed,
    Bin_velo_min_1 = Bin_1..Mean.speed,
    Bin_velo_min_2 = Bin_2..Mean.speed,
    Bin_velo_min_3 = Bin_3..Mean.speed,
    Bin_velo_min_4 = Bin_4..Mean.speed,
    Bin_velo_min_5 = Bin_5..Mean.speed,
    Cum_BaLe_exp_1 = Cum_1..Leftback_exploration...time.pressed,
    Cum_BaLe_exp_2 = Cum_2..Leftback_exploration...time.pressed,
    Cum_BaLe_exp_3 = Cum_3..Leftback_exploration...time.pressed,
    Cum_BaLe_exp_4 = Cum_4..Leftback_exploration...time.pressed,
    Cum_BaLe_exp_5 = Cum_5..Leftback_exploration...time.pressed,
    Bin_BaLe_exp_1 = Bin_1..Leftback_exploration...time.pressed,
    Bin_BaLe_exp_2 = Bin_2..Leftback_exploration...time.pressed,
    Bin_BaLe_exp_3 = Bin_3..Leftback_exploration...time.pressed,
    Bin_BaLe_exp_4 = Bin_4..Leftback_exploration...time.pressed,
    Bin_BaLe_exp_5 = Bin_5..Leftback_exploration...time.pressed,
    BaLe_exp_Latency = Leftback_exploration...latency.1st.press,
    Cum_FrRi_exp_1 = Cum_1..Rightfront_exploration...time.pressed,
    Cum_FrRi_exp_2 = Cum_2..Rightfront_exploration...time.pressed,
    Cum_FrRi_exp_3 = Cum_3..Rightfront_exploration...time.pressed,
    Cum_FrRi_exp_4 = Cum_4..Rightfront_exploration...time.pressed,
    Cum_FrRi_exp_5 = Cum_5..Rightfront_exploration...time.pressed,
    Bin_FrRi_exp_1 = Bin_1..Rightfront_exploration...time.pressed,
    Bin_FrRi_exp_2 = Bin_2..Rightfront_exploration...time.pressed,
    Bin_FrRi_exp_3 = Bin_3..Rightfront_exploration...time.pressed,
    Bin_FrRi_exp_4 = Bin_4..Rightfront_exploration...time.pressed,
    Bin_FrRi_exp_5 = Bin_5..Rightfront_exploration...time.pressed,
    FrRi_exp_Latency = Rightfront_exploration...latency.1st.press,
    Cum_BaLe_tim_1 = Cum_1..BL...time,
    Cum_BaLe_tim_2 = Cum_2..BL...time,
    Cum_BaLe_tim_3 = Cum_3..BL...time,
    Cum_BaLe_tim_4 = Cum_4..BL...time,
    Cum_BaLe_tim_5 = Cum_5..BL...time,
    Bin_BaLe_tim_1 = Bin_1..BL...time,
    Bin_BaLe_tim_2 = Bin_2..BL...time,
    Bin_BaLe_tim_3 = Bin_3..BL...time,
    Bin_BaLe_tim_4 = Bin_4..BL...time,
    Bin_BaLe_tim_5 = Bin_5..BL...time,
    Cum_BaRi_tim_1 = Cum_1..BR...time,
    Cum_BaRi_tim_2 = Cum_2..BR...time,
    Cum_BaRi_tim_3 = Cum_3..BR...time,
    Cum_BaRi_tim_4 = Cum_4..BR...time,
    Cum_BaRi_tim_5 = Cum_5..BR...time,
    Bin_BaRi_tim_1 = Bin_1..BR...time,
    Bin_BaRi_tim_2 = Bin_2..BR...time,
    Bin_BaRi_tim_3 = Bin_3..BR...time,
    Bin_BaRi_tim_4 = Bin_4..BR...time,
    Bin_BaRi_tim_5 = Bin_5..BR...time,
    Cum_FrLe_tim_1 = Cum_1..FL...time,
    Cum_FrLe_tim_2 = Cum_2..FL...time,
    Cum_FrLe_tim_3 = Cum_3..FL...time,
    Cum_FrLe_tim_4 = Cum_4..FL...time,
    Cum_FrLe_tim_5 = Cum_5..FL...time,
    Bin_FrLe_tim_1 = Bin_1..FL...time,
    Bin_FrLe_tim_2 = Bin_2..FL...time,
    Bin_FrLe_tim_3 = Bin_3..FL...time,
    Bin_FrLe_tim_4 = Bin_4..FL...time,
    Bin_FrLe_tim_5 = Bin_5..FL...time,
    Cum_FrRi_tim_1 = Cum_1..FR...time,
    Cum_FrRi_tim_2 = Cum_2..FR...time,
    Cum_FrRi_tim_3 = Cum_3..FR...time,
    Cum_FrRi_tim_4 = Cum_4..FR...time,
    Cum_FrRi_tim_5 = Cum_5..FR...time,
    Bin_FrRi_tim_1 = Bin_1..FR...time,
    Bin_FrRi_tim_2 = Bin_2..FR...time,
    Bin_FrRi_tim_3 = Bin_3..FR...time,
    Bin_FrRi_tim_4 = Bin_4..FR...time,
    Bin_FrRi_tim_5 = Bin_5..FR...time,
    Cum_cent_tim_1 = Cum_1..Center...time,
    Cum_cent_tim_2 = Cum_2..Center...time,
    Cum_cent_tim_3 = Cum_3..Center...time,
    Cum_cent_tim_4 = Cum_4..Center...time,
    Cum_cent_tim_5 = Cum_5..Center...time,
    Bin_cent_tim_1 = Bin_1..Center...time,
    Bin_cent_tim_2 = Bin_2..Center...time,
    Bin_cent_tim_3 = Bin_3..Center...time,
    Bin_cent_tim_4 = Bin_4..Center...time,
    Bin_cent_tim_5 = Bin_5..Center...time,
    Cum_peri_tim_1 = Cum_1..Periphery...time,
    Cum_peri_tim_2 = Cum_2..Periphery...time,
    Cum_peri_tim_3 = Cum_3..Periphery...time,
    Cum_peri_tim_4 = Cum_4..Periphery...time,
    Cum_peri_tim_5 = Cum_5..Periphery...time,
    Bin_peri_tim_1 = Bin_1..Periphery...time,
    Bin_peri_tim_2 = Bin_2..Periphery...time,
    Bin_peri_tim_3 = Bin_3..Periphery...time,
    Bin_peri_tim_4 = Bin_4..Periphery...time,
    Bin_peri_tim_5 = Bin_5..Periphery...time,
  )

test_rear_groom_clean <- test_rear_groom %>%
  rename(
    Cum_rear_cnt_1 = Cum_1..Rearing...number.of.presses,
    Cum_rear_cnt_2 = Cum_2..Rearing...number.of.presses,
    Cum_rear_cnt_3 = Cum_3..Rearing...number.of.presses,
    Cum_rear_cnt_4 = Cum_4..Rearing...number.of.presses,
    Cum_rear_cnt_5 = Cum_5..Rearing...number.of.presses,
    Bin_rear_cnt_1 = Bin_1..Rearing...number.of.presses,
    Bin_rear_cnt_2 = Bin_2..Rearing...number.of.presses,
    Bin_rear_cnt_3 = Bin_3..Rearing...number.of.presses,
    Bin_rear_cnt_4 = Bin_4..Rearing...number.of.presses,
    Bin_rear_cnt_5 = Bin_5..Rearing...number.of.presses,
    Cum_rear_tim_1 = Cum_1..Rearing...time.pressed,
    Cum_rear_tim_2 = Cum_2..Rearing...time.pressed,
    Cum_rear_tim_3 = Cum_3..Rearing...time.pressed,
    Cum_rear_tim_4 = Cum_4..Rearing...time.pressed,
    Cum_rear_tim_5 = Cum_5..Rearing...time.pressed,
    Bin_rear_tim_1 = Bin_1..Rearing...time.pressed,
    Bin_rear_tim_2 = Bin_2..Rearing...time.pressed,
    Bin_rear_tim_3 = Bin_3..Rearing...time.pressed,
    Bin_rear_tim_4 = Bin_4..Rearing...time.pressed,
    Bin_rear_tim_5 = Bin_5..Rearing...time.pressed,
    Rear_latency = Rearing...latency.1st.press,
    Cum_groo_cnt_1 = Cum_1..Grooming...number.of.presses,
    Cum_groo_cnt_2 = Cum_2..Grooming...number.of.presses,
    Cum_groo_cnt_3 = Cum_3..Grooming...number.of.presses,
    Cum_groo_cnt_4 = Cum_4..Grooming...number.of.presses,
    Cum_groo_cnt_5 = Cum_5..Grooming...number.of.presses,
    Bin_groo_cnt_1 = Bin_1..Grooming...number.of.presses,
    Bin_groo_cnt_2 = Bin_2..Grooming...number.of.presses,
    Bin_groo_cnt_3 = Bin_3..Grooming...number.of.presses,
    Bin_groo_cnt_4 = Bin_4..Grooming...number.of.presses,
    Bin_groo_cnt_5 = Bin_5..Grooming...number.of.presses,
    Cum_groo_tim_1 = Cum_1..Grooming...time.pressed,
    Cum_groo_tim_2 = Cum_2..Grooming...time.pressed,
    Cum_groo_tim_3 = Cum_3..Grooming...time.pressed,
    Cum_groo_tim_4 = Cum_4..Grooming...time.pressed,
    Cum_groo_tim_5 = Cum_5..Grooming...time.pressed,
    Bin_groo_tim_1 = Bin_1..Grooming...time.pressed,
    Bin_groo_tim_2 = Bin_2..Grooming...time.pressed,
    Bin_groo_tim_3 = Bin_3..Grooming...time.pressed,
    Bin_groo_tim_4 = Bin_4..Grooming...time.pressed,
    Bin_groo_tim_5 = Bin_5..Grooming...time.pressed,
    Groo_latency = Grooming...latency.1st.press,
    Cum_BLre_cnt_1 = Cum_1..BL...rearing...number,
    Cum_BLre_cnt_2 = Cum_2..BL...rearing...number,
    Cum_BLre_cnt_3 = Cum_3..BL...rearing...number,
    Cum_BLre_cnt_4 = Cum_4..BL...rearing...number,
    Cum_BLre_cnt_5 = Cum_5..BL...rearing...number,
    Bin_BLre_cnt_1 = Bin_1..BL...rearing...number,
    Bin_BLre_cnt_2 = Bin_2..BL...rearing...number,
    Bin_BLre_cnt_3 = Bin_3..BL...rearing...number,
    Bin_BLre_cnt_4 = Bin_4..BL...rearing...number,
    Bin_BLre_cnt_5 = Bin_5..BL...rearing...number,
    Cum_BLre_tim_1 = Cum_1..BL...rearing...time.pressed,
    Cum_BLre_tim_2 = Cum_2..BL...rearing...time.pressed,
    Cum_BLre_tim_3 = Cum_3..BL...rearing...time.pressed,
    Cum_BLre_tim_4 = Cum_4..BL...rearing...time.pressed,
    Cum_BLre_tim_5 = Cum_5..BL...rearing...time.pressed,
    Bin_BLre_tim_1 = Bin_1..BL...rearing...time.pressed,
    Bin_BLre_tim_2 = Bin_2..BL...rearing...time.pressed,
    Bin_BLre_tim_3 = Bin_3..BL...rearing...time.pressed,
    Bin_BLre_tim_4 = Bin_4..BL...rearing...time.pressed,
    Bin_BLre_tim_5 = Bin_5..BL...rearing...time.pressed,
    Cum_BLgr_tim_1 = Cum_1..BL...grooming...time.pressed,
    Cum_BLgr_tim_2 = Cum_2..BL...grooming...time.pressed,
    Cum_BLgr_tim_3 = Cum_3..BL...grooming...time.pressed,
    Cum_BLgr_tim_4 = Cum_4..BL...grooming...time.pressed,
    Cum_BLgr_tim_5 = Cum_5..BL...grooming...time.pressed,
    Bin_BLgr_tim_1 = Bin_1..BL...grooming...time.pressed,
    Bin_BLgr_tim_2 = Bin_2..BL...grooming...time.pressed,
    Bin_BLgr_tim_3 = Bin_3..BL...grooming...time.pressed,
    Bin_BLgr_tim_4 = Bin_4..BL...grooming...time.pressed,
    Bin_BLgr_tim_5 = Bin_5..BL...grooming...time.pressed,
    Cum_BRre_cnt_1 = Cum_1..BR...rearing...number,
    Cum_BRre_cnt_2 = Cum_2..BR...rearing...number,
    Cum_BRre_cnt_3 = Cum_3..BR...rearing...number,
    Cum_BRre_cnt_4 = Cum_4..BR...rearing...number,
    Cum_BRre_cnt_5 = Cum_5..BR...rearing...number,
    Bin_BRre_cnt_1 = Bin_1..BR...rearing...number,
    Bin_BRre_cnt_2 = Bin_2..BR...rearing...number,
    Bin_BRre_cnt_3 = Bin_3..BR...rearing...number,
    Bin_BRre_cnt_4 = Bin_4..BR...rearing...number,
    Bin_BRre_cnt_5 = Bin_5..BR...rearing...number,
    Cum_BRre_tim_1 = Cum_1..BR...rearing...time.pressed,
    Cum_BRre_tim_2 = Cum_2..BR...rearing...time.pressed,
    Cum_BRre_tim_3 = Cum_3..BR...rearing...time.pressed,
    Cum_BRre_tim_4 = Cum_4..BR...rearing...time.pressed,
    Cum_BRre_tim_5 = Cum_5..BR...rearing...time.pressed,
    Bin_BRre_tim_1 = Bin_1..BR...rearing...time.pressed,
    Bin_BRre_tim_2 = Bin_2..BR...rearing...time.pressed,
    Bin_BRre_tim_3 = Bin_3..BR...rearing...time.pressed,
    Bin_BRre_tim_4 = Bin_4..BR...rearing...time.pressed,
    Bin_BRre_tim_5 = Bin_5..BR...rearing...time.pressed,
    Cum_BRgr_tim_1 = Cum_1..BR...grooming...time.pressed,
    Cum_BRgr_tim_2 = Cum_2..BR...grooming...time.pressed,
    Cum_BRgr_tim_3 = Cum_3..BR...grooming...time.pressed,
    Cum_BRgr_tim_4 = Cum_4..BR...grooming...time.pressed,
    Cum_BRgr_tim_5 = Cum_5..BR...grooming...time.pressed,
    Bin_BRgr_tim_1 = Bin_1..BR...grooming...time.pressed,
    Bin_BRgr_tim_2 = Bin_2..BR...grooming...time.pressed,
    Bin_BRgr_tim_3 = Bin_3..BR...grooming...time.pressed,
    Bin_BRgr_tim_4 = Bin_4..BR...grooming...time.pressed,
    Bin_BRgr_tim_5 = Bin_5..BR...grooming...time.pressed,
    Cum_FLre_cnt_1 = Cum_1..FL...rearing...number,
    Cum_FLre_cnt_2 = Cum_2..FL...rearing...number,
    Cum_FLre_cnt_3 = Cum_3..FL...rearing...number,
    Cum_FLre_cnt_4 = Cum_4..FL...rearing...number,
    Cum_FLre_cnt_5 = Cum_5..FL...rearing...number,
    Bin_FLre_cnt_1 = Bin_1..FL...rearing...number,
    Bin_FLre_cnt_2 = Bin_2..FL...rearing...number,
    Bin_FLre_cnt_3 = Bin_3..FL...rearing...number,
    Bin_FLre_cnt_4 = Bin_4..FL...rearing...number,
    Bin_FLre_cnt_5 = Bin_5..FL...rearing...number,
    Cum_FLre_tim_1 = Cum_1..FL...rearing...time.pressed,
    Cum_FLre_tim_2 = Cum_2..FL...rearing...time.pressed,
    Cum_FLre_tim_3 = Cum_3..FL...rearing...time.pressed,
    Cum_FLre_tim_4 = Cum_4..FL...rearing...time.pressed,
    Cum_FLre_tim_5 = Cum_5..FL...rearing...time.pressed,
    Bin_FLre_tim_1 = Bin_1..FL...rearing...time.pressed,
    Bin_FLre_tim_2 = Bin_2..FL...rearing...time.pressed,
    Bin_FLre_tim_3 = Bin_3..FL...rearing...time.pressed,
    Bin_FLre_tim_4 = Bin_4..FL...rearing...time.pressed,
    Bin_FLre_tim_5 = Bin_5..FL...rearing...time.pressed,
    Cum_FLgr_tim_1 = Cum_1..FL...grooming...time.pressed,
    Cum_FLgr_tim_2 = Cum_2..FL...grooming...time.pressed,
    Cum_FLgr_tim_3 = Cum_3..FL...grooming...time.pressed,
    Cum_FLgr_tim_4 = Cum_4..FL...grooming...time.pressed,
    Cum_FLgr_tim_5 = Cum_5..FL...grooming...time.pressed,
    Bin_FLgr_tim_1 = Bin_1..FL...grooming...time.pressed,
    Bin_FLgr_tim_2 = Bin_2..FL...grooming...time.pressed,
    Bin_FLgr_tim_3 = Bin_3..FL...grooming...time.pressed,
    Bin_FLgr_tim_4 = Bin_4..FL...grooming...time.pressed,
    Bin_FLgr_tim_5 = Bin_5..FL...grooming...time.pressed,
    Cum_FRre_cnt_1 = Cum_1..FR...rearing...number,
    Cum_FRre_cnt_2 = Cum_2..FR...rearing...number,
    Cum_FRre_cnt_3 = Cum_3..FR...rearing...number,
    Cum_FRre_cnt_4 = Cum_4..FR...rearing...number,
    Cum_FRre_cnt_5 = Cum_5..FR...rearing...number,
    Bin_FRre_cnt_1 = Bin_1..FR...rearing...number,
    Bin_FRre_cnt_2 = Bin_2..FR...rearing...number,
    Bin_FRre_cnt_3 = Bin_3..FR...rearing...number,
    Bin_FRre_cnt_4 = Bin_4..FR...rearing...number,
    Bin_FRre_cnt_5 = Bin_5..FR...rearing...number,
    Cum_FRre_tim_1 = Cum_1..FR...rearing...time.pressed,
    Cum_FRre_tim_2 = Cum_2..FR...rearing...time.pressed,
    Cum_FRre_tim_3 = Cum_3..FR...rearing...time.pressed,
    Cum_FRre_tim_4 = Cum_4..FR...rearing...time.pressed,
    Cum_FRre_tim_5 = Cum_5..FR...rearing...time.pressed,
    Bin_FRre_tim_1 = Bin_1..FR...rearing...time.pressed,
    Bin_FRre_tim_2 = Bin_2..FR...rearing...time.pressed,
    Bin_FRre_tim_3 = Bin_3..FR...rearing...time.pressed,
    Bin_FRre_tim_4 = Bin_4..FR...rearing...time.pressed,
    Bin_FRre_tim_5 = Bin_5..FR...rearing...time.pressed,
    Cum_FRgr_tim_1 = Cum_1..FR...grooming...time.pressed,
    Cum_FRgr_tim_2 = Cum_2..FR...grooming...time.pressed,
    Cum_FRgr_tim_3 = Cum_3..FR...grooming...time.pressed,
    Cum_FRgr_tim_4 = Cum_4..FR...grooming...time.pressed,
    Cum_FRgr_tim_5 = Cum_5..FR...grooming...time.pressed,
    Bin_FRgr_tim_1 = Bin_1..FR...grooming...time.pressed,
    Bin_FRgr_tim_2 = Bin_2..FR...grooming...time.pressed,
    Bin_FRgr_tim_3 = Bin_3..FR...grooming...time.pressed,
    Bin_FRgr_tim_4 = Bin_4..FR...grooming...time.pressed,
    Bin_FRgr_tim_5 = Bin_5..FR...grooming...time.pressed
  )

## Merge reference tables with data matrices
hab_clean <-
  merge(hab_ref, hab_clean, by.x = "AnyMaze_Test", by.y = "Test")

for (i in (length(hab_ref) + 1):(length(hab_clean))) {
  hab_clean[, i]  <- as.numeric(as.character(hab_clean[, i]))
}

enc_clean <-
  merge(enc_ref, enc_clean, by.x = "AnyMaze_Test", by.y = "Test")

enc_clean <-
  merge(enc_clean,
        enc_rear_groom_clean,
        by.x = "AnyMaze_Test",
        by.y = "Test")

for (i in (length(enc_ref) + 1):(length(enc_clean))) {
  enc_clean[, i]  <- as.numeric(as.character(enc_clean[, i]))
}

test_clean <-
  merge(test_ref, test_clean, by.x = "AnyMaze_Test", by.y = "Test")

test_clean <-
  merge(test_clean,
        test_rear_groom_clean,
        by.x = "AnyMaze_Test",
        by.y = "Test")

for (i in (length(test_ref) + 1):(length(test_clean))) {
  test_clean[, i]  <- as.numeric(as.character(test_clean[, i]))
}

## Total exploration time
enc_clean$Total_exp_time <-
  enc_clean$Cum_FrRi_exp_10 + enc_clean$Cum_BaLe_exp_10
test_clean$Total_exp_time <-
  test_clean$Cum_BaLe_exp_5 + test_clean$Cum_FrRi_exp_5

## Calculate discrimination ratios for test session
# Target (Trgt) position/object
test_clean$Bin_Trgt_exp_min_1 <- NA
test_clean$Bin_Trgt_exp_min_2 <- NA
test_clean$Bin_Trgt_exp_min_3 <- NA
test_clean$Bin_Trgt_exp_min_4 <- NA
test_clean$Bin_Trgt_exp_min_5 <- NA
test_clean$Cum_Trgt_exp_min_1 <- NA
test_clean$Cum_Trgt_exp_min_2 <- NA
test_clean$Cum_Trgt_exp_min_3 <- NA
test_clean$Cum_Trgt_exp_min_4 <- NA
test_clean$Cum_Trgt_exp_min_5 <- NA

# Control (Cntl) position/object
test_clean$Bin_Cntl_exp_min_1 <- NA
test_clean$Bin_Cntl_exp_min_2 <- NA
test_clean$Bin_Cntl_exp_min_3 <- NA
test_clean$Bin_Cntl_exp_min_4 <- NA
test_clean$Bin_Cntl_exp_min_5 <- NA
test_clean$Cum_Cntl_exp_min_1 <- NA
test_clean$Cum_Cntl_exp_min_2 <- NA
test_clean$Cum_Cntl_exp_min_3 <- NA
test_clean$Cum_Cntl_exp_min_4 <- NA
test_clean$Cum_Cntl_exp_min_5 <- NA

# Assign Cntrl and Trgt values
for (row in 1:(nrow(test_clean))) {
  if (test_clean$Task[row] == "OPR") {
    if (test_clean$Unfamiliar_Position[row] == "RF" |
        test_clean$Unfamiliar_Position[row] == "RB") {
      # If unfamiliar position was on right side of OFA
      test_clean$Bin_Trgt_exp_min_1[row] = test_clean$Bin_FrRi_exp_1[row]
      test_clean$Bin_Trgt_exp_min_2[row] = test_clean$Bin_FrRi_exp_2[row]
      test_clean$Bin_Trgt_exp_min_3[row] = test_clean$Bin_FrRi_exp_3[row]
      test_clean$Bin_Trgt_exp_min_4[row] = test_clean$Bin_FrRi_exp_4[row]
      test_clean$Bin_Trgt_exp_min_5[row] = test_clean$Bin_FrRi_exp_5[row]
      test_clean$Cum_Trgt_exp_min_1[row] = test_clean$Cum_FrRi_exp_1[row]
      test_clean$Cum_Trgt_exp_min_2[row] = test_clean$Cum_FrRi_exp_2[row]
      test_clean$Cum_Trgt_exp_min_3[row] = test_clean$Cum_FrRi_exp_3[row]
      test_clean$Cum_Trgt_exp_min_4[row] = test_clean$Cum_FrRi_exp_4[row]
      test_clean$Cum_Trgt_exp_min_5[row] = test_clean$Cum_FrRi_exp_5[row]
      
      test_clean$Bin_Cntl_exp_min_1[row] = test_clean$Bin_BaLe_exp_1[row]
      test_clean$Bin_Cntl_exp_min_2[row] = test_clean$Bin_BaLe_exp_2[row]
      test_clean$Bin_Cntl_exp_min_3[row] = test_clean$Bin_BaLe_exp_3[row]
      test_clean$Bin_Cntl_exp_min_4[row] = test_clean$Bin_BaLe_exp_4[row]
      test_clean$Bin_Cntl_exp_min_5[row] = test_clean$Bin_BaLe_exp_5[row]
      test_clean$Cum_Cntl_exp_min_1[row] = test_clean$Cum_BaLe_exp_1[row]
      test_clean$Cum_Cntl_exp_min_2[row] = test_clean$Cum_BaLe_exp_2[row]
      test_clean$Cum_Cntl_exp_min_3[row] = test_clean$Cum_BaLe_exp_3[row]
      test_clean$Cum_Cntl_exp_min_4[row] = test_clean$Cum_BaLe_exp_4[row]
      test_clean$Cum_Cntl_exp_min_5[row] = test_clean$Cum_BaLe_exp_5[row]
    } else{
      # if unfamiliar position was on left side of OFA
      test_clean$Bin_Trgt_exp_min_1[row] = test_clean$Bin_BaLe_exp_1[row]
      test_clean$Bin_Trgt_exp_min_2[row] = test_clean$Bin_BaLe_exp_2[row]
      test_clean$Bin_Trgt_exp_min_3[row] = test_clean$Bin_BaLe_exp_3[row]
      test_clean$Bin_Trgt_exp_min_4[row] = test_clean$Bin_BaLe_exp_4[row]
      test_clean$Bin_Trgt_exp_min_5[row] = test_clean$Bin_BaLe_exp_5[row]
      test_clean$Cum_Trgt_exp_min_1[row] = test_clean$Cum_BaLe_exp_1[row]
      test_clean$Cum_Trgt_exp_min_2[row] = test_clean$Cum_BaLe_exp_2[row]
      test_clean$Cum_Trgt_exp_min_3[row] = test_clean$Cum_BaLe_exp_3[row]
      test_clean$Cum_Trgt_exp_min_4[row] = test_clean$Cum_BaLe_exp_4[row]
      test_clean$Cum_Trgt_exp_min_5[row] = test_clean$Cum_BaLe_exp_5[row]
      
      test_clean$Bin_Cntl_exp_min_1[row] = test_clean$Bin_FrRi_exp_1[row]
      test_clean$Bin_Cntl_exp_min_2[row] = test_clean$Bin_FrRi_exp_2[row]
      test_clean$Bin_Cntl_exp_min_3[row] = test_clean$Bin_FrRi_exp_3[row]
      test_clean$Bin_Cntl_exp_min_4[row] = test_clean$Bin_FrRi_exp_4[row]
      test_clean$Bin_Cntl_exp_min_5[row] = test_clean$Bin_FrRi_exp_5[row]
      test_clean$Cum_Cntl_exp_min_1[row] = test_clean$Cum_FrRi_exp_1[row]
      test_clean$Cum_Cntl_exp_min_2[row] = test_clean$Cum_FrRi_exp_2[row]
      test_clean$Cum_Cntl_exp_min_3[row] = test_clean$Cum_FrRi_exp_3[row]
      test_clean$Cum_Cntl_exp_min_4[row] = test_clean$Cum_FrRi_exp_4[row]
      test_clean$Cum_Cntl_exp_min_5[row] = test_clean$Cum_FrRi_exp_5[row]
    }
  } else{
    if (test_clean$Task[row] == "NOR") {
      if (substring(test_clean$Unfamiliar_Position[row], 1, 1) == "R" &
          substring(test_clean$Familiar_Position[row], 1, 1) == "R") {
        # if both object were placed on the right side of OFA
        if (test_clean$Unfamiliar_Position[row] == "RB") {
          # unfamiliar object at the back
          test_clean$Bin_Trgt_exp_min_1[row] = test_clean$Bin_BaLe_exp_1[row]
          test_clean$Bin_Trgt_exp_min_2[row] = test_clean$Bin_BaLe_exp_2[row]
          test_clean$Bin_Trgt_exp_min_3[row] = test_clean$Bin_BaLe_exp_3[row]
          test_clean$Bin_Trgt_exp_min_4[row] = test_clean$Bin_BaLe_exp_4[row]
          test_clean$Bin_Trgt_exp_min_5[row] = test_clean$Bin_BaLe_exp_5[row]
          test_clean$Cum_Trgt_exp_min_1[row] = test_clean$Cum_BaLe_exp_1[row]
          test_clean$Cum_Trgt_exp_min_2[row] = test_clean$Cum_BaLe_exp_2[row]
          test_clean$Cum_Trgt_exp_min_3[row] = test_clean$Cum_BaLe_exp_3[row]
          test_clean$Cum_Trgt_exp_min_4[row] = test_clean$Cum_BaLe_exp_4[row]
          test_clean$Cum_Trgt_exp_min_5[row] = test_clean$Cum_BaLe_exp_5[row]
          
          test_clean$Bin_Cntl_exp_min_1[row] = test_clean$Bin_FrRi_exp_1[row]
          test_clean$Bin_Cntl_exp_min_2[row] = test_clean$Bin_FrRi_exp_2[row]
          test_clean$Bin_Cntl_exp_min_3[row] = test_clean$Bin_FrRi_exp_3[row]
          test_clean$Bin_Cntl_exp_min_4[row] = test_clean$Bin_FrRi_exp_4[row]
          test_clean$Bin_Cntl_exp_min_5[row] = test_clean$Bin_FrRi_exp_5[row]
          test_clean$Cum_Cntl_exp_min_1[row] = test_clean$Cum_FrRi_exp_1[row]
          test_clean$Cum_Cntl_exp_min_2[row] = test_clean$Cum_FrRi_exp_2[row]
          test_clean$Cum_Cntl_exp_min_3[row] = test_clean$Cum_FrRi_exp_3[row]
          test_clean$Cum_Cntl_exp_min_4[row] = test_clean$Cum_FrRi_exp_4[row]
          test_clean$Cum_Cntl_exp_min_5[row] = test_clean$Cum_FrRi_exp_5[row]
        } else{
          # unfamiliar object at the front
          test_clean$Bin_Trgt_exp_min_1[row] = test_clean$Bin_FrRi_exp_1[row]
          test_clean$Bin_Trgt_exp_min_2[row] = test_clean$Bin_FrRi_exp_2[row]
          test_clean$Bin_Trgt_exp_min_3[row] = test_clean$Bin_FrRi_exp_3[row]
          test_clean$Bin_Trgt_exp_min_4[row] = test_clean$Bin_FrRi_exp_4[row]
          test_clean$Bin_Trgt_exp_min_5[row] = test_clean$Bin_FrRi_exp_5[row]
          test_clean$Cum_Trgt_exp_min_1[row] = test_clean$Cum_FrRi_exp_1[row]
          test_clean$Cum_Trgt_exp_min_2[row] = test_clean$Cum_FrRi_exp_2[row]
          test_clean$Cum_Trgt_exp_min_3[row] = test_clean$Cum_FrRi_exp_3[row]
          test_clean$Cum_Trgt_exp_min_4[row] = test_clean$Cum_FrRi_exp_4[row]
          test_clean$Cum_Trgt_exp_min_5[row] = test_clean$Cum_FrRi_exp_5[row]
          
          test_clean$Bin_Cntl_exp_min_1[row] = test_clean$Bin_BaLe_exp_1[row]
          test_clean$Bin_Cntl_exp_min_2[row] = test_clean$Bin_BaLe_exp_2[row]
          test_clean$Bin_Cntl_exp_min_3[row] = test_clean$Bin_BaLe_exp_3[row]
          test_clean$Bin_Cntl_exp_min_4[row] = test_clean$Bin_BaLe_exp_4[row]
          test_clean$Bin_Cntl_exp_min_5[row] = test_clean$Bin_BaLe_exp_5[row]
          test_clean$Cum_Cntl_exp_min_1[row] = test_clean$Cum_BaLe_exp_1[row]
          test_clean$Cum_Cntl_exp_min_2[row] = test_clean$Cum_BaLe_exp_2[row]
          test_clean$Cum_Cntl_exp_min_3[row] = test_clean$Cum_BaLe_exp_3[row]
          test_clean$Cum_Cntl_exp_min_4[row] = test_clean$Cum_BaLe_exp_4[row]
          test_clean$Cum_Cntl_exp_min_5[row] = test_clean$Cum_BaLe_exp_5[row]
        }
      } else{
        if (substring(test_clean$Unfamiliar_Position[row], 1, 1) == "L" &
            substring(test_clean$Familiar_Position[row], 1, 1) == "L") {
          # if both object were placed on the left side of OFA
          if (test_clean$Unfamiliar_Position[row] == "LB") {
            # unfamiliar object at the back
            test_clean$Bin_Trgt_exp_min_1[row] = test_clean$Bin_BaLe_exp_1[row]
            test_clean$Bin_Trgt_exp_min_2[row] = test_clean$Bin_BaLe_exp_2[row]
            test_clean$Bin_Trgt_exp_min_3[row] = test_clean$Bin_BaLe_exp_3[row]
            test_clean$Bin_Trgt_exp_min_4[row] = test_clean$Bin_BaLe_exp_4[row]
            test_clean$Bin_Trgt_exp_min_5[row] = test_clean$Bin_BaLe_exp_5[row]
            test_clean$Cum_Trgt_exp_min_1[row] = test_clean$Cum_BaLe_exp_1[row]
            test_clean$Cum_Trgt_exp_min_2[row] = test_clean$Cum_BaLe_exp_2[row]
            test_clean$Cum_Trgt_exp_min_3[row] = test_clean$Cum_BaLe_exp_3[row]
            test_clean$Cum_Trgt_exp_min_4[row] = test_clean$Cum_BaLe_exp_4[row]
            test_clean$Cum_Trgt_exp_min_5[row] = test_clean$Cum_BaLe_exp_5[row]
            
            test_clean$Bin_Cntl_exp_min_1[row] = test_clean$Bin_FrRi_exp_1[row]
            test_clean$Bin_Cntl_exp_min_2[row] = test_clean$Bin_FrRi_exp_2[row]
            test_clean$Bin_Cntl_exp_min_3[row] = test_clean$Bin_FrRi_exp_3[row]
            test_clean$Bin_Cntl_exp_min_4[row] = test_clean$Bin_FrRi_exp_4[row]
            test_clean$Bin_Cntl_exp_min_5[row] = test_clean$Bin_FrRi_exp_5[row]
            test_clean$Cum_Cntl_exp_min_1[row] = test_clean$Cum_FrRi_exp_1[row]
            test_clean$Cum_Cntl_exp_min_2[row] = test_clean$Cum_FrRi_exp_2[row]
            test_clean$Cum_Cntl_exp_min_3[row] = test_clean$Cum_FrRi_exp_3[row]
            test_clean$Cum_Cntl_exp_min_4[row] = test_clean$Cum_FrRi_exp_4[row]
            test_clean$Cum_Cntl_exp_min_5[row] = test_clean$Cum_FrRi_exp_5[row]
          } else{
            # unfamiliar object at the front
            test_clean$Bin_Trgt_exp_min_1[row] = test_clean$Bin_FrRi_exp_1[row]
            test_clean$Bin_Trgt_exp_min_2[row] = test_clean$Bin_FrRi_exp_2[row]
            test_clean$Bin_Trgt_exp_min_3[row] = test_clean$Bin_FrRi_exp_3[row]
            test_clean$Bin_Trgt_exp_min_4[row] = test_clean$Bin_FrRi_exp_4[row]
            test_clean$Bin_Trgt_exp_min_5[row] = test_clean$Bin_FrRi_exp_5[row]
            test_clean$Cum_Trgt_exp_min_1[row] = test_clean$Cum_FrRi_exp_1[row]
            test_clean$Cum_Trgt_exp_min_2[row] = test_clean$Cum_FrRi_exp_2[row]
            test_clean$Cum_Trgt_exp_min_3[row] = test_clean$Cum_FrRi_exp_3[row]
            test_clean$Cum_Trgt_exp_min_4[row] = test_clean$Cum_FrRi_exp_4[row]
            test_clean$Cum_Trgt_exp_min_5[row] = test_clean$Cum_FrRi_exp_5[row]
            
            test_clean$Bin_Cntl_exp_min_1[row] = test_clean$Bin_BaLe_exp_1[row]
            test_clean$Bin_Cntl_exp_min_2[row] = test_clean$Bin_BaLe_exp_2[row]
            test_clean$Bin_Cntl_exp_min_3[row] = test_clean$Bin_BaLe_exp_3[row]
            test_clean$Bin_Cntl_exp_min_4[row] = test_clean$Bin_BaLe_exp_4[row]
            test_clean$Bin_Cntl_exp_min_5[row] = test_clean$Bin_BaLe_exp_5[row]
            test_clean$Cum_Cntl_exp_min_1[row] = test_clean$Cum_BaLe_exp_1[row]
            test_clean$Cum_Cntl_exp_min_2[row] = test_clean$Cum_BaLe_exp_2[row]
            test_clean$Cum_Cntl_exp_min_3[row] = test_clean$Cum_BaLe_exp_3[row]
            test_clean$Cum_Cntl_exp_min_4[row] = test_clean$Cum_BaLe_exp_4[row]
            test_clean$Cum_Cntl_exp_min_5[row] = test_clean$Cum_BaLe_exp_5[row]
          }
        } else{
          # if objects on both, left and right side
          if (substring(test_clean$Unfamiliar_Position[row], 1, 1) == "L") {
            # unfamiliar object on left side
            test_clean$Bin_Trgt_exp_min_1[row] = test_clean$Bin_BaLe_exp_1[row]
            test_clean$Bin_Trgt_exp_min_2[row] = test_clean$Bin_BaLe_exp_2[row]
            test_clean$Bin_Trgt_exp_min_3[row] = test_clean$Bin_BaLe_exp_3[row]
            test_clean$Bin_Trgt_exp_min_4[row] = test_clean$Bin_BaLe_exp_4[row]
            test_clean$Bin_Trgt_exp_min_5[row] = test_clean$Bin_BaLe_exp_5[row]
            test_clean$Cum_Trgt_exp_min_1[row] = test_clean$Cum_BaLe_exp_1[row]
            test_clean$Cum_Trgt_exp_min_2[row] = test_clean$Cum_BaLe_exp_2[row]
            test_clean$Cum_Trgt_exp_min_3[row] = test_clean$Cum_BaLe_exp_3[row]
            test_clean$Cum_Trgt_exp_min_4[row] = test_clean$Cum_BaLe_exp_4[row]
            test_clean$Cum_Trgt_exp_min_5[row] = test_clean$Cum_BaLe_exp_5[row]
            
            test_clean$Bin_Cntl_exp_min_1[row] = test_clean$Bin_FrRi_exp_1[row]
            test_clean$Bin_Cntl_exp_min_2[row] = test_clean$Bin_FrRi_exp_2[row]
            test_clean$Bin_Cntl_exp_min_3[row] = test_clean$Bin_FrRi_exp_3[row]
            test_clean$Bin_Cntl_exp_min_4[row] = test_clean$Bin_FrRi_exp_4[row]
            test_clean$Bin_Cntl_exp_min_5[row] = test_clean$Bin_FrRi_exp_5[row]
            test_clean$Cum_Cntl_exp_min_1[row] = test_clean$Cum_FrRi_exp_1[row]
            test_clean$Cum_Cntl_exp_min_2[row] = test_clean$Cum_FrRi_exp_2[row]
            test_clean$Cum_Cntl_exp_min_3[row] = test_clean$Cum_FrRi_exp_3[row]
            test_clean$Cum_Cntl_exp_min_4[row] = test_clean$Cum_FrRi_exp_4[row]
            test_clean$Cum_Cntl_exp_min_5[row] = test_clean$Cum_FrRi_exp_5[row]
          } else{
            # unfamiliar object on right side
            test_clean$Bin_Trgt_exp_min_1[row] = test_clean$Bin_FrRi_exp_1[row]
            test_clean$Bin_Trgt_exp_min_2[row] = test_clean$Bin_FrRi_exp_2[row]
            test_clean$Bin_Trgt_exp_min_3[row] = test_clean$Bin_FrRi_exp_3[row]
            test_clean$Bin_Trgt_exp_min_4[row] = test_clean$Bin_FrRi_exp_4[row]
            test_clean$Bin_Trgt_exp_min_5[row] = test_clean$Bin_FrRi_exp_5[row]
            test_clean$Cum_Trgt_exp_min_1[row] = test_clean$Cum_FrRi_exp_1[row]
            test_clean$Cum_Trgt_exp_min_2[row] = test_clean$Cum_FrRi_exp_2[row]
            test_clean$Cum_Trgt_exp_min_3[row] = test_clean$Cum_FrRi_exp_3[row]
            test_clean$Cum_Trgt_exp_min_4[row] = test_clean$Cum_FrRi_exp_4[row]
            test_clean$Cum_Trgt_exp_min_5[row] = test_clean$Cum_FrRi_exp_5[row]
            
            test_clean$Bin_Cntl_exp_min_1[row] = test_clean$Bin_BaLe_exp_1[row]
            test_clean$Bin_Cntl_exp_min_2[row] = test_clean$Bin_BaLe_exp_2[row]
            test_clean$Bin_Cntl_exp_min_3[row] = test_clean$Bin_BaLe_exp_3[row]
            test_clean$Bin_Cntl_exp_min_4[row] = test_clean$Bin_BaLe_exp_4[row]
            test_clean$Bin_Cntl_exp_min_5[row] = test_clean$Bin_BaLe_exp_5[row]
            test_clean$Cum_Cntl_exp_min_1[row] = test_clean$Cum_BaLe_exp_1[row]
            test_clean$Cum_Cntl_exp_min_2[row] = test_clean$Cum_BaLe_exp_2[row]
            test_clean$Cum_Cntl_exp_min_3[row] = test_clean$Cum_BaLe_exp_3[row]
            test_clean$Cum_Cntl_exp_min_4[row] = test_clean$Cum_BaLe_exp_4[row]
            test_clean$Cum_Cntl_exp_min_5[row] = test_clean$Cum_BaLe_exp_5[row]
          }
        }
      }
    }
  }
}

# Calculate DRs (time spent at target[novel] − time spent at control[familiar])/(time spent at target + time spent at control)
test_clean$Bin_DiRa_min_1 <-
  (test_clean$Bin_Trgt_exp_min_1 - test_clean$Bin_Cntl_exp_min_1) / (test_clean$Bin_Trgt_exp_min_1 + test_clean$Bin_Cntl_exp_min_1)
test_clean$Bin_DiRa_min_2 <-
  (test_clean$Bin_Trgt_exp_min_2 - test_clean$Bin_Cntl_exp_min_2) / (test_clean$Bin_Trgt_exp_min_2 + test_clean$Bin_Cntl_exp_min_2)
test_clean$Bin_DiRa_min_3 <-
  (test_clean$Bin_Trgt_exp_min_3 - test_clean$Bin_Cntl_exp_min_3) / (test_clean$Bin_Trgt_exp_min_3 + test_clean$Bin_Cntl_exp_min_3)
test_clean$Bin_DiRa_min_4 <-
  (test_clean$Bin_Trgt_exp_min_4 - test_clean$Bin_Cntl_exp_min_4) / (test_clean$Bin_Trgt_exp_min_4 + test_clean$Bin_Cntl_exp_min_4)
test_clean$Bin_DiRa_min_5 <-
  (test_clean$Bin_Trgt_exp_min_5 - test_clean$Bin_Cntl_exp_min_5) / (test_clean$Bin_Trgt_exp_min_5 + test_clean$Bin_Cntl_exp_min_5)

test_clean$Cum_DiRa_min_1 <-
  (test_clean$Cum_Trgt_exp_min_1 - test_clean$Cum_Cntl_exp_min_1) / (test_clean$Cum_Trgt_exp_min_1 + test_clean$Cum_Cntl_exp_min_1)
test_clean$Cum_DiRa_min_2 <-
  (test_clean$Cum_Trgt_exp_min_2 - test_clean$Cum_Cntl_exp_min_2) / (test_clean$Cum_Trgt_exp_min_2 + test_clean$Cum_Cntl_exp_min_2)
test_clean$Cum_DiRa_min_3 <-
  (test_clean$Cum_Trgt_exp_min_3 - test_clean$Cum_Cntl_exp_min_3) / (test_clean$Cum_Trgt_exp_min_3 + test_clean$Cum_Cntl_exp_min_3)
test_clean$Cum_DiRa_min_4 <-
  (test_clean$Cum_Trgt_exp_min_4 - test_clean$Cum_Cntl_exp_min_4) / (test_clean$Cum_Trgt_exp_min_4 + test_clean$Cum_Cntl_exp_min_4)
test_clean$Cum_DiRa_min_5 <-
  (test_clean$Cum_Trgt_exp_min_5 - test_clean$Cum_Cntl_exp_min_5) / (test_clean$Cum_Trgt_exp_min_5 + test_clean$Cum_Cntl_exp_min_5)

# If DR = NA (when no exploration was scored for time interval) insert DR = 0
test_clean$Bin_DiRa_min_1[is.na(test_clean$Bin_DiRa_min_1)] <- 0
test_clean$Bin_DiRa_min_2[is.na(test_clean$Bin_DiRa_min_2)] <- 0
test_clean$Bin_DiRa_min_3[is.na(test_clean$Bin_DiRa_min_3)] <- 0
test_clean$Bin_DiRa_min_4[is.na(test_clean$Bin_DiRa_min_4)] <- 0
test_clean$Bin_DiRa_min_5[is.na(test_clean$Bin_DiRa_min_5)] <- 0

test_clean$Cum_DiRa_min_1[is.na(test_clean$Cum_DiRa_min_1)] <- 0
test_clean$Cum_DiRa_min_2[is.na(test_clean$Cum_DiRa_min_2)] <- 0
test_clean$Cum_DiRa_min_3[is.na(test_clean$Cum_DiRa_min_3)] <- 0
test_clean$Cum_DiRa_min_4[is.na(test_clean$Cum_DiRa_min_4)] <- 0
test_clean$Cum_DiRa_min_5[is.na(test_clean$Cum_DiRa_min_5)] <- 0


# 4 - Save csv ------------------------------------------------------------
write.csv2(hab_clean,
           file.path(dataPath, "100-MPV0120-Habituation_clean.csv"),
           row.names = FALSE)

write.csv2(enc_clean,
           file.path(dataPath, "100-MPV0120-Encoding_clean.csv"),
           row.names = FALSE)

write.csv2(test_clean,
           file.path(dataPath, "100-MPV0120-Test_clean.csv"),
           row.names = FALSE)
