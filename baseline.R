library("dplyr")
library("tidyverse")
library("tidyr")
library("tidymodels")
library("glue")
library("vroom")
library("lubridate")
library("zoo")
library("xgboost")

info = read_csv("site_info.csv")
energy = read_csv("energy.csv")[-25632,]
energy$time = energy$time %>% as_datetime %>% as.character()
colnames(energy)[1] = 'Forecast_time' 

fcst_dj = read_csv('1_hour_dj.csv') 
fcst_dj$Cloud = fcst_dj$Cloud %>% round()
fcst_us = read_csv('1_hour_us.csv')
fcst_us$Cloud = fcst_us$Cloud %>% round()

energy_dj = energy %>% 
  mutate(year = str_sub(Forecast_time, 1,4),
         month = str_sub(Forecast_time, 6,7),
         day = str_sub(Forecast_time,9,10),
         hour = str_sub(Forecast_time, 12, str_length(Forecast_time)-6)) %>% 
  select(Forecast_time, year, month, day, hour, dangjin) %>% 
  filter(year != '2018' | month != '03'| day !='01')

energy_dj_f = energy %>% 
  mutate(year = str_sub(Forecast_time, 1,4),
         month = str_sub(Forecast_time, 6,7),
         day = str_sub(Forecast_time,9,10),
         hour = str_sub(Forecast_time, 12, str_length(Forecast_time)-6)) %>% 
  select(Forecast_time, year, month, day, hour, dangjin_floating) %>% 
  filter(year != '2018' | month != '03'| day !='01')

energy_dj_w = energy %>% 
  mutate(year = str_sub(Forecast_time, 1,4),
         month = str_sub(Forecast_time, 6,7),
         day = str_sub(Forecast_time,9,10),
         hour = str_sub(Forecast_time, 12, str_length(Forecast_time)-6)) %>% 
  select(Forecast_time, year, month, day, hour, dangjin_warehouse) %>% 
  filter(year != '2018' | month != '03'| day !='01')

energy_us = energy %>% 
  mutate(year = str_sub(Forecast_time, 1,4),
         month = str_sub(Forecast_time, 6,7),
         day = str_sub(Forecast_time,9,10),
         hour = str_sub(Forecast_time, 12, str_length(Forecast_time)-6)) %>% 
  select(Forecast_time, year, month, day, hour, ulsan) %>% 
  filter(year != '2018' | month != '03'| day !='01')

fcst_dj = fcst_dj %>% 
  mutate(year = str_sub(Forecast_time, 1,4),
         month = str_sub(Forecast_time, 6,7),
         day = str_sub(Forecast_time,9,10),
         hour = str_sub(Forecast_time, 12, str_length(Forecast_time)-6)) %>% 
  select(Forecast_time, year, month, day, hour, 
         Temperature, Humidity, WindSpeed, WindDirection, Cloud) %>% 
  filter(year != '2021' | month != '02') %>% 
  filter(year != '2021' | month != '03') %>% 
  filter(year != '2018' | month != '03' | day != '01')

fcst_us = fcst_us[,-1] %>% 
  mutate(year = str_sub(Forecast_time, 1,4),
         month = str_sub(Forecast_time, 6,7),
         day = str_sub(Forecast_time,9,10),
         hour = str_sub(Forecast_time, 12, str_length(Forecast_time)-6)) %>% 
  select(Forecast_time, year, month, day, hour, 
         Temperature, Humidity, WindSpeed, WindDirection, Cloud) %>% 
  filter(year != '2021' | month != '02') %>% 
  filter(year != '2021' | month != '03') %>% 
  filter(year != '2018' | month != '03' | day != '01')

dj_train_data = fcst_dj %>% 
  left_join(energy_dj, by = c('year', 'month', 'day', 'hour'))

dj_w_train_data = fcst_dj %>% 
  left_join(energy_dj_w, by = c('year', 'month', 'day', 'hour')) 

dj_f_train_data = fcst_dj %>% 
  left_join(energy_dj_f, by = c('year', 'month', 'day', 'hour')) 

us_train_data = fcst_us %>% 
  left_join(energy_us, by = c('year', 'month', 'day', 'hour')) 




dj_train_data_1 = dj_train_data %>% 
  filter(hour %in% c('08','09','10','11')) %>% 
  select(Temperature, Humidity, WindSpeed, WindDirection, Cloud, dangjin)

dj_train_data_2 = dj_train_data %>% 
  filter(hour %in% c('12','13','14','15')) %>% 
  select(Temperature, Humidity, WindSpeed, WindDirection, Cloud, dangjin)

dj_train_data_3 = dj_train_data %>% 
  filter(hour %in% c('16','17','18','19')) %>% 
  select(Temperature, Humidity, WindSpeed, WindDirection, Cloud, dangjin)






dj_f_train_data_1 = dj_f_train_data %>% 
  filter(hour %in% c('08','09','10','11')) %>% 
  select(Temperature, Humidity, WindSpeed, WindDirection, Cloud, dangjin_floating)

dj_f_train_data_2 = dj_f_train_data %>% 
  filter(hour %in% c('12','13','14','15')) %>% 
  select(Temperature, Humidity, WindSpeed, WindDirection, Cloud, dangjin_floating)

dj_f_train_data_3 = dj_f_train_data %>% 
  filter(hour %in% c('16','17','18','19')) %>% 
  select(Temperature, Humidity, WindSpeed, WindDirection, Cloud, dangjin_floating)






dj_w_train_data_1 = dj_w_train_data %>% 
  filter(hour %in% c('08','09','10','11')) %>% 
  select(Temperature, Humidity, WindSpeed, WindDirection, Cloud, dangjin_warehouse)

dj_w_train_data_2 = dj_w_train_data %>% 
  filter(hour %in% c('12','13','14','15')) %>% 
  select(Temperature, Humidity, WindSpeed, WindDirection, Cloud, dangjin_warehouse)

dj_w_train_data_3 = dj_w_train_data %>% 
  filter(hour %in% c('16','17','18','19')) %>% 
  select(Temperature, Humidity, WindSpeed, WindDirection, Cloud, dangjin_warehouse)






us_train_data_1 = us_train_data %>% 
  filter(hour %in% c('08','09','10','11')) %>% 
  select(Temperature, Humidity, WindSpeed, WindDirection, Cloud, ulsan)

us_train_data_2 = us_train_data %>% 
  filter(hour %in% c('12','13','14','15')) %>% 
  select(Temperature, Humidity, WindSpeed, WindDirection, Cloud, ulsan)

us_train_data_3 = us_train_data %>% 
  filter(hour %in% c('16','17','18','19')) %>% 
  select(Temperature, Humidity, WindSpeed, WindDirection, Cloud, ulsan)









dj_train_data %>% 
  filter(year== '2018') %>% 
  ggplot(mapping = aes(Temperature, dangjin)) + 
  geom_point() +
  facet_wrap(~month, ncol = 3)
dj_train_data %>% 
  filter(year== '2019') %>% 
  ggplot(mapping = aes(Temperature, dangjin)) + 
  geom_point() +
  facet_wrap(~month, ncol = 3)

dj_train_data %>% 
  filter(year== '2020') %>% 
  ggplot(mapping = aes(Temperature, Humidity)) + 
  geom_point() +
  facet_wrap(~month, ncol = 3)


dj_train_data %>% 
  filter(year== '2019') %>% 
  ggplot(mapping = aes(Temperature, Humidity)) + 
  geom_point() +
  facet_wrap(~month, ncol = 3)

dj_train_data %>% 
  filter(year== '2018') %>% 
  ggplot(mapping = aes(Temperature, Humidity)) + 
  geom_point() +
  facet_wrap(~month, ncol = 3)




x11()
dj_train_data %>% 
  filter(year == '2018', month == '03') %>% 
  ggplot(aes(day, dangjin, group = hour, colour = hour)) +
  geom_point() + 
  geom_smooth(se = F)
x11()
dj_train_data %>% 
  filter(year == '2019', month == '03') %>% 
  ggplot(aes(day, dangjin, group = hour, colour = hour)) +
  geom_point() + 
  geom_smooth(se = F)
x11()
dj_train_data %>% 
  filter(year == '2020', month == '03') %>% 
  ggplot(aes(day, dangjin, group = hour, colour = hour)) +
  geom_point() + 
  geom_smooth()






x11()
dj_train_data %>% 
  filter(year == '2018', month == '04') %>% 
  ggplot(aes(day, dangjin, group = hour, colour = hour)) +
  geom_point() + 
  geom_smooth(se = F)
x11()
dj_train_data %>% 
  filter(year == '2019', month == '04') %>% 
  ggplot(aes(day, dangjin, group = hour, colour = hour)) +
  geom_point() + 
  geom_smooth(se = F)
x11()
dj_train_data %>% 
  filter(year == '2020', month == '04') %>% 
  ggplot(aes(day, dangjin, group = hour, colour = hour)) +
  geom_point() + 
  geom_smooth(se = F)
  

x11()
dj_train_data %>% 
  filter(year == '2018', month == '08') %>% 
  ggplot(aes(day, WindSpeed, group = hour, colour = hour), main = '2018') +
  geom_point() + 
  geom_smooth(se = F) +
  ggtitle('2018')
x11()
dj_train_data %>% 
  filter(year == '2019', month == '08') %>% 
  ggplot(aes(day, WindSpeed, group = hour, colour = hour) ) +
  geom_point() + 
  geom_smooth(se = F) +
  ggtitle('2019')
x11()
dj_train_data %>% 
  filter(year == '2019', month == '02') %>% 
  ggplot(aes(day, WindSpeed, group = hour, colour = hour), main = '2020') +
  geom_point() + 
  geom_smooth(se = F) +
  ggtitle('2019')
dj_train_data %>% 
  filter(year == '2019', month == '02') %>% 
  select(day) %>% unique

dj_train_data %>% 
  filter(year== '2018') %>% 
  ggplot(mapping = aes(Temperature, Humidity)) + 
  geom_point() +
  facet_wrap(~hour, ncol = 3)




dj_train_data %>% 
  transmute(day_  = yday(Forecast_time))






dj_train_data = dj_train_data %>% 
  mutate(wind_y = sin((pi/180)*dj_train_data$WindDirection),
         wind_x = cos((pi/180)*dj_train_data$WindDirection))

dj_train_data %>% 
  ggplot(aes(Temperature*wind_x, Temperature*wind_y))+
  geom_point()


dj_train_data %>%
  ggplot(aes(Temperature, dangjin)) + 
  geom_point()




x11()
dj_train_data %>% 
  filter(year == '2018') %>% 
  ggplot(aes(yday, Temperature, group = hour, colour = hour)) +
  geom_point() + 
  geom_smooth(se = F) +
  ggtitle('2018')
x11()
dj_train_data %>% 
  filter(year == '2019') %>% 
  ggplot(aes(yday, Temperature, group = hour, colour = hour)) +
  geom_point() + 
  geom_smooth(se = F) +
  ggtitle('2019')
x11()
dj_train_data %>% 
  filter(year == '2019') %>% 
  ggplot(aes(yday, Temperature, group = hour, colour = hour)) +
  geom_point() + 
  geom_smooth(se = F) +
  ggtitle('2020')





x11()
dj_train_data %>% 
  filter(year == '2018') %>% 
  filter(as.numeric(hour) %in% 08:20) %>% 
  ggplot(aes(yday, dangjin, group = hour, colour = hour)) +
  geom_point() + 
  geom_smooth(se = F) +
  ggtitle('2018')
x11()
dj_train_data %>% 
  filter(year == '2019') %>% 
  filter(as.numeric(hour) %in% 08:20) %>% 
  ggplot(aes(yday, dangjin, group = hour, colour = hour)) +
  geom_point() + 
  geom_smooth(se = F) +
  ggtitle('2019')
x11()
dj_train_data %>% 
  filter(year == '2020') %>% 
  filter(as.numeric(hour) %in% 08:20) %>% 
  ggplot(aes(yday, dangjin, group = hour, colour = hour)) +
  geom_point() + 
  geom_smooth(se = F) +
  ggtitle('2020')