library(tidyverse)
library(arrow)
library(ggokabeito)
library(ggplot2)
library(dplyr)
library(lubridate)
library(gghighlight)
library(ggview)
# Load dataset
dataset <- read_parquet("renewables-dataset.parquet")

# Compute residual for given penetration
a_s <- 0.30 # solar matches (a_s x 100)% of the average yearly demand across EU
a_w <- 0.7 # wind matches (a_w x 100)% of the average yearly demand across EU

dataset_residual <- dataset %>%
  mutate(
    solar_scaled_MWh = a_s * solar_MWh,
    wind_scaled_MWh = a_w * wind_MWh,
    date = date(Time)) %>% 
  select(Time, date, ID, latitude, longitude, solar_scaled_MWh, wind_scaled_MWh, country)


daily_df <- dataset_residual %>%
  group_by(ID, date,latitude,longitude,country) %>%
  summarise(
    solar_scaled_MWh_daily = sum(solar_scaled_MWh),
    wind_scaled_MWh_daily = sum(wind_scaled_MWh),
    .groups="drop"
  )

daily_with_extreme<-daily_df %>% group_by(ID,latitude,longitude,country) %>% 
  mutate(
    solar_is_extreme = solar_scaled_MWh_daily <= quantile(solar_scaled_MWh_daily,0.1),
    wind_is_extreme = wind_scaled_MWh_daily <= quantile(wind_scaled_MWh_daily,0.1),
    both_extreme = solar_is_extreme & wind_is_extreme
  ) %>% ungroup()

daily_extremes <- daily_with_extreme %>% group_by(ID,latitude,longitude,country) %>%
  summarise(
    longest_extreme_run = max(rle(both_extreme)$lengths[rle(both_extreme)$values]),
    numbers_of_extremes_runs_over_three = sum(rle(both_extreme)$lengths[rle(both_extreme)$values]>=3),
    numbers_of_extremes_runs_over_two = sum(rle(both_extreme)$lengths[rle(both_extreme)$values]>=2),
    .groups="drop"
  )

ggplot(daily_extremes,aes(x=longitude,y=latitude,colour = longest_extreme_run,size=longest_extreme_run))+geom_point()+scale_color_viridis_b()

ggplot(daily_extremes,aes(x=longitude,y=latitude,colour = numbers_of_extremes_runs_over_three,size=numbers_of_extremes_runs_over_three))+geom_point()+scale_color_viridis_b()

ggplot(daily_extremes,aes(x=longitude,y=latitude,colour = numbers_of_extremes_runs_over_two,size=numbers_of_extremes_runs_over_two))+geom_point()+scale_color_viridis_b()





