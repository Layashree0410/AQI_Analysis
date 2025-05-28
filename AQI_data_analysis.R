library(tidyverse)
library(janitor)
library(lubridate)


read_csv("INDIA-AQI-DATA-2015-2020.csv") %>% 
  clean_names()-> aqidf

aqidf %>% 
  mutate(year = date %>% year(),
         month = date %>% month,
         day = date %>% day(),
         week = date %>% week(),
         weekday = date %>% wday(label=T)) -> aqidf1

colnames(aqidf1)

unique(aqidf1$city)

aqidf1 %>% 
  pivot_longer(3:14, names_to = "pollutant", values_to = "Values") -> aqidf2


###1. year wise pollutant compound trends for every city
aqidf2 %>% 
  group_by(year,pollutant) %>% 
  summarise(mean_value = mean(Values,na.rm = T))-> aqi_yearwise


library(ggplot2)
## line graph 
aqi_yearwise %>% 
  ggplot(aes(x = year, y = mean_value))+
  geom_line(color = "red")+
  facet_wrap(~pollutant,scales = "free_y")+ ## y axis should be free within its range,as its not for comparision between the pollutants free y axis will be helpfull to visualize each pollutant trend
  labs(title = "Air Pollutants Trend", subtitle = "From 2015=2020",
       x = NULL, y = "Pollutant mean value",caption = "Source: AIQ DATA"
       )+
  theme_linedraw()-> plot1
ggsave("Air Pollutants Trend.pdf",
       plot = plot1,
       units = "in",
       width = 10,
       height = 6)


##2 Bengaluru year wise pollutant compound trends for every city
aqidf2 %>%
  filter(city == "Bengaluru") %>%
  group_by(year, pollutant) %>%
  summarise(mean_value = mean(Values, na.rm = TRUE)) -> bengaluru_aqi_yearwise


library(ggplot2)
## line graph 
bengaluru_aqi_yearwise %>% 
  ggplot(aes(x = year, y = mean_value))+
  geom_line(color = "red")+
  facet_wrap(~pollutant,scales = "free_y")+ ## y axis should be free within its range,as its not for comparision between the pollutants free y axis will be helpfull to visualize each pollutant trend
  labs(title = "Bengaluru Air Pollutants Trend", subtitle = "From 2015=2020",
       x = NULL, y = "Pollutant mean value",caption = "Source: AIQ DATA"
  )+
  theme_linedraw()-> plot2
ggsave("Bengaluru Air Pollutants Trend.pdf",
       plot = plot2,
       units = "in",
       width = 10,
       height = 6)



#3.  co pollutant  year wise  trends for every city
aqidf2 %>%
  filter(pollutant == "co") %>%
  group_by(year, pollutant, city) %>%
  summarise(mean_value = mean(Values, na.rm = TRUE), .groups = 'drop') -> Co_aqi_yearwise

# Plot
Co_aqi_yearwise %>% 
  ggplot(aes(x = year, y = mean_value, color = city)) +
  geom_line(size = 1) +
  facet_wrap(~city, scales = "free_y") +
  labs(
    title = "CO Air Pollutants Trend",
    subtitle = "From 2015–2020",
    x = NULL,
    y = "Pollutant Mean Value",
    caption = "Source: AQI DATA"
  ) +
  theme_linedraw() -> plot3

# Save the plot
ggsave("Co_Air_Pollutants_Trend.pdf",
       plot = plot3,
       units = "in",
       width = 15,
       height = 8)


##4. Air quality trends for bengaluru , Chennai , Mumbai , Hyderabad
aqidf2 %>%
  filter(city %in% c("Bengaluru","Chennai","Mumbai","Hyderabad")) %>%
  group_by(year,city, pollutant) %>% 
  summarise(mean_value = mean(Values, na.rm = TRUE)) -> BCMB_aqi_yearwise

BCMB_aqi_yearwise %>% 
  ggplot(aes(x = year, y = mean_value, color = city)) +
  geom_line() +
  facet_wrap(~pollutant, scales = "free_y") +
  labs(
    title = " Metro citiesAir Pollutants Trend",
    subtitle = "From 2015–2020",
    x = NULL,
    y = "Pollutant Mean Value",
    caption = "Source: AQI DATA"
  ) +
  theme_linedraw() -> plot4

# Save the plot
ggsave("Metro_Cities_Air_Pollutants_Trend.pdf",
       plot = plot4,
       units = "in",
       width = 15,
       height = 8)

#5. pm2.5 trend for Bengaluru for 2015-2020
aqidf2 %>%
  filter(pollutant == "pm2_5",city=="Bengaluru",year>=2015 & year <=2020) %>%
  group_by(year, pollutant, city) %>%
  summarise(mean_value = mean(Values, na.rm = TRUE), .groups = 'drop') -> pm5_aqi_yearwise

# Plot
pm5_aqi_yearwise %>% 
  ggplot(aes(x = year, y = mean_value, color = city)) +
  geom_line(size = 1) +
  facet_wrap(~city, scales = "free_y") +
  labs(
    title = "pm_5 Air Pollutants Trend",
    subtitle = "From 2015–2020",
    x = NULL,
    y = "Pollutant Mean Value",
    caption = "Source: AQI DATA"
  ) +
  theme_linedraw() -> plot5

# Save the plot
ggsave("pm_5_Air_Pollutants_Trend.pdf",
       plot = plot5,
       units = "in",
       width = 15,
       height = 8)


### Heat Map
aqidf2 %>%
  filter(pollutant == "co") %>%
  group_by(week, weekday, month) %>%
  summarise(meanval = mean(Values, na.rm = TRUE)) %>%
  ggplot(aes(x = week,
             y = weekday,
             fill = meanval)) +
  geom_tile() +
  facet_wrap(~month, scales = "free_x") +
  # scale_fill_gradient(low = "yellow", high = "red") +
  scale_fill_gradientn(colours = c("darkgreen", "yellow", "red")) +
  theme_minimal() +
  labs(title = "CO heat map",
       subtitle = "For all cities",
       x = NULL,
       y = NULL)
