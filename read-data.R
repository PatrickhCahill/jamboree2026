library(tidyverse)
library(arrow)
library(ggokabeito)
library(ggplot2)
library(dplyr)
library(lubridate)
library(gghighlight)
library(ggview)
library(gganimate)
# Load dataset
dataset <- read_parquet("dataset/renewables-dataset.parquet")

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

daily_with_low_extreme<-daily_df %>% group_by(ID,latitude,longitude,country) %>% 
  mutate(
    solar_is_extreme = (solar_scaled_MWh_daily <= quantile(solar_scaled_MWh_daily,0.1)),
    wind_is_extreme = (wind_scaled_MWh_daily <= quantile(wind_scaled_MWh_daily,0.1)),
    both_extreme = solar_is_extreme & wind_is_extreme
  ) %>% ungroup()

daily_with_low_extreme[] <- lapply(daily_with_low_extreme, function(x) {
  if (is.logical(x)) as.integer(x) else x
})

daily_with_low_extreme %>%
  pivot_wider(names_from = date, values_from = both_extreme)  %>% 
  select(-date)
%>%
  select(-date) %>%              # remove date column
  cor(use = "pairwise.complete.obs")




daily_with_low_extreme %>%
  group_by(ID) %>%
  summarise(
    r = cor(solar_is_extreme)
  )
  
daily_with_high_extreme<-daily_df %>% group_by(ID,latitude,longitude,country) %>% 
  mutate(
    solar_is_extreme = !(solar_scaled_MWh_daily <= quantile(solar_scaled_MWh_daily,0.9)),
    wind_is_extreme = !(wind_scaled_MWh_daily <= quantile(wind_scaled_MWh_daily,0.9)),
    both_extreme = solar_is_extreme & wind_is_extreme
  ) %>% ungroup()

daily_extremes <- daily_with_extreme %>% group_by(ID,latitude,longitude,country) %>%
  summarise(
    longest_extreme_run = max(rle(both_extreme)$lengths[rle(both_extreme)$values]),
    numbers_of_extremes_runs_over_three = sum(rle(both_extreme)$lengths[rle(both_extreme)$values]>=3),
    numbers_of_extremes_runs_over_two = sum(rle(both_extreme)$lengths[rle(both_extreme)$values]>=2),
    when_longest_run = date[cumsum(rle(both_extreme)$lengths)[(which(rle(both_extreme)$lengths==max(rle(both_extreme)$lengths[rle(both_extreme)$values]))[1]-1)]+1],
    number_of_low_gen_days = sum(both_extreme),
    extremes_non_isolated = sum((rle(both_extreme)$lengths[rle(both_extreme)$values])[(rle(both_extreme)$lengths[rle(both_extreme)$values]>=2)]),
    .groups="drop"
  )

at_specific_date<-filter(daily_df,date=="2014-12-04") 

couple_months<-filter(daily_df, (month(date) == 12 | month(date)==11) & year(date)==2014) %>% group_by(ID,latitude,longitude)

anim<-ggplot(couple_months,aes(x=longitude,y=latitude,group=ID,size=solar_scaled_MWh_daily))+geom_point()+transition_time(date)+labs(title = "Date: {frame_time}")+ theme(legend.position="none")
animate(anim,fps=5)

ggplot(couple_months,aes(x=longitude,y=latitude,group=ID,size=wind_scaled_MWh_daily))+geom_point()

ggplot(at_specific_date,aes(x=longitude,y=latitude,size=solar_scaled_MWh_daily))+geom_point()+
  scale_color_viridis_b()

ggplot(daily_extremes,aes(x=longitude,y=latitude,colour = longest_extreme_run,size=longest_extreme_run))+geom_point()+scale_color_viridis_b()

ggplot(daily_extremes,aes(x=longitude,y=latitude,colour = extremes_non_isolated,size=extremes_non_isolated))+geom_point()+scale_color_viridis_b()

ggplot(daily_extremes,aes(x=longitude,y=latitude,colour = numbers_of_extremes_runs_over_two,size=numbers_of_extremes_runs_over_two))+geom_point()+scale_color_viridis_b()

ggplot(daily_extremes,aes(x=longitude,y=latitude,colour = longest_extreme_run,size=extremes_non_isolated))+geom_point()+scale_color_viridis_b()




