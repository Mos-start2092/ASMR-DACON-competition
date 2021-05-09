### Predict Solar power generation

library(tidyverse)
library(lubridate)
library(ggcorrplot)

### data --------------------------------------------------------------------
info = read.csv("site_info.csv", header=T,  fileEncoding="UTF-8", encoding = "CP949")
fcst_dj = read.csv("dangjin_fcst_data.csv",header=T)
fcst_us = read.csv("ulsan_fcst_data.csv",header=T)
obs_dj = read.csv("dangjin_obs_data.csv", header=T,fileEncoding="UTF-8", encoding = "CP949")
obs_us = read.csv("ulsan_obs_data.csv", header=T, fileEncoding="UTF-8", encoding = "CP949")
energy = read.csv("energy.csv",header=T)

str(info)
info$Id = c("sea","ware","dj","us")

#시계열 데이터(chr->ymd_hms)
str(fcst_dj)
str(fcst_us)
fcst_dj$Forecast.time = ymd_hms(fcst_dj$Forecast.time)
fcst_us$Forecast.time = ymd_hms(fcst_us$Forecast.time)

#forecast 계산
time_dj = fcst_dj$Forecast.time
hour(time_dj) = hour(time_dj) + fcst_dj$forecast
fcst_dj$forecast = time_dj

time_us = fcst_us$Forecast.time
hour(time_us) = hour(time_us) + fcst_us$forecast
fcst_us$forecast = time_us

#branch - 지점코드에 따른 지점명 데이터
str(obs_dj)
str(obs_us)
branch = as.matrix(unique(obs_dj[,c(1,2)]))
obs_dj = obs_dj[,-2]
branch = rbind(branch,as.matrix(unique(obs_us[,c(1,2)])))
obs_us = obs_us[,-2]

# 2018-03-01 00:00:00 전력 생산량 data 없으므로 제거
# dangjin observe data , 2018-07-24 11:00 ~ 2018-07-24 16:00 없음
obs_dj$일시 = ymd_hm(obs_dj$일시)
colnames(obs_dj) = c("branch", "time", "Temperature", "WindSpeed",
                     "WindDirection", "Humidity", "Cloud")
obs_dj = obs_dj[-1,]

obs_us$일시 = ymd_hm(obs_us$일시)
colnames(obs_us) = c("branch", "time", "Temperature", "WindSpeed",
                     "WindDirection", "Humidity", "Cloud")
obs_us = obs_us[-1,]

#energy
str(energy)
energy$time = ymd_hms(energy$time)
colnames(energy) = c("time","sea","ware","dj","us")
energy = energy[,-1]


# EDA ---------------------------------------------------------------------

tail(obs_us)
obs_us


# NA ----------------------------------------------------------------------
obs_us[obs_us %>% select(Temperature) %>% is.na %>% which,]
obs_us %>% 
  filter(ymd_hms("2020-08-26 01:00:00") <= time & time<= ymd_hms("2020-08-27 23:00:00"))
obs_us_temp_na = obs_us %>% select(Temperature) %>% is.na %>% which
for (i in obs_us_temp_na) {
  obs_us[i,3] = (obs_us[i-1,3] + obs_us[i+1,3])/2
}

obs_us_wind_na = obs_us %>% select(WindSpeed) %>% is.na %>% which
obs_us %>% 
  filter(ymd_hms("2019-06-12 01:00:00") <= time & time<= ymd_hms("2019-06-12 23:00:00"))
for (i in obs_us_wind_na) {
  obs_us[i,4] = (obs_us[i-1,4] + obs_us[i+1,4])/2
  obs_us[i,5] = (obs_us[i-1,5] + obs_us[i+1,5])/2
}

obs_us_hum_na = obs_us %>% select(Humidity) %>% is.na %>% which
for (i in obs_us_hum_na) {
  obs_us[i,4] = (obs_us[i-1,6] + obs_us[i+1,6])/2
}

#Cloud-선형보간법
obs_us[obs_us %>% select(Cloud) %>% is.na %>% which %>% length,] #굉장히 많음
obs_us_cloud_na = obs_us %>% select(Cloud) %>% is.na %>% which
t = obs_us[-obs_us_cloud_na, "time"]
t_out = obs_us[obs_us_cloud_na, "time"]
cloud = obs_us[-obs_us_cloud_na, "Cloud"]
cloud_na_fill = approx(t,cloud, xout=t_out)

obs_us[obs_us_cloud_na,"Cloud"] = cloud_na_fill$y


tail(obs_us)
# Energy ------------------------------------------------------------------
us = cbind(obs_us[,-1], energy[-nrow(energy),5])
colnames(us) = c("time", "Temperature", "WindSpeed", "WindDirection",
                 "Humidity", "Cloud", "energy")

head(us)

##### time series화
#time_index <- seq(from = as.POSIXct("2018-03-01 01:00"), 
#                  to = as.POSIXct("2021-01-31 23:00"), by = "hour")
#us_energy_ts = xts(us$energy, order.by=time_index)
#decompose(us_energy_ts, type="additive")

## 시계열화
us %>% 
  ggplot(aes(x=time, y=Temperature)) + 
  geom_smooth() +geom_point(alpha=0.1)
us %>% 
  ggplot(aes(x=time, y=Humidity)) + 
  geom_smooth() +geom_point(alpha=0.1)
us %>% 
  ggplot(aes(x=time, y=WindSpeed)) + 
  geom_smooth() +geom_point(alpha=0.1)
us %>% 
  ggplot(aes(x=time, y=WindDirection)) + 
  geom_smooth() +geom_point(alpha=0.1)
us %>% 
  ggplot(aes(x=time, y=Cloud)) + 
  geom_smooth() +geom_point(alpha=0.1)
us %>% 
  ggplot(aes(x=time, y=energy)) + 
  geom_smooth() +geom_point(alpha=0.1)


## energy에 대한 관계
us_lm = lm(energy~. ,data=us[,-c(1,8,9)])
scale_us = scale
install.packages('QuantPsyc')
library(QuantPsyc)
lm.beta(us_lm)

----------------------------------------
us %>% 
  ggplot(aes(x=WindSpeed, y=energy)) + 
  geom_smooth() +geom_point(alpha=0.1)
us %>% 
  ggplot(aes(x=log(WindSpeed), y=energy)) + 
  geom_smooth() +geom_point(alpha=0.1)

us %>% 
  ggplot(aes(x=Temperature, y=energy)) + 
  geom_smooth() +geom_point(alpha=0.1)
us %>% 
  ggplot(aes(x=Humidity, y=energy)) + 
  geom_smooth() +geom_point(alpha=0.1)
us %>% 
  ggplot(aes(x=Cloud, y=energy)) + 
  geom_smooth() +geom_point(alpha=0.1)




## energy가 0인구간
library(reshape2)

time_split = data.frame(do.call('rbind', strsplit(as.character(us$time), split=" ", fixed=TRUE)))
colnames(time_split) = c("Day", "hour")
us = cbind(us, time_split)

#겨울(아침 7시까지, 저녁 7시이후)
us %>% 
  filter(energy==0, ymd_hms("2019-01-01 00:00:00") <= time & time<= ymd_hms("2019-01-31 23:00:00")) %>% 
  group_by(hour) %>% 
  summarise(n=n()) 
#여름(아침 6시까지, 저녁 8시이후)
us %>% 
  filter(energy==0, ymd_hms("2018-08-01 00:00:00") <= time & time<= ymd_hms("2018-08-31 23:00:00")) %>% 
  group_by(hour) %>% 
  summarise(n=n()) %>% 
  tail()

library(chron)
us$hour = as.times(us$hour)
us = us %>% 
  filter(!(hour<=as.times("06:00:00")) & !( hour>=as.times("20:00:00")))



##us data - 시계열화
#시간별
us_bytime = us %>% 
  group_by(hour) %>% 
  summarise(mean_temp = mean(Temperature),
            mean_hum = mean(Humidity, na.rm=TRUE), #8338번째 NA 존재 why?
            mean_WindSpeed = mean(WindSpeed),
            mean_WindDirection = mean(WindDirection),
            mean_cloud = mean(Cloud),
            mean_energy = mean(energy))

us_bytime_ts = ts(us_bytime, start=7, frequency=1)
plot(us_bytime_ts, type="single")

#날짜별
us$Day = as.Date(us$Day)
us_byday = us[,-c(1,9)] %>% 
  group_by(Day) %>% 
  summarise(mean_temp = mean(Temperature),
            mean_hum = mean(Humidity, na.rm=TRUE), #8338번째 NA 존재 why?
            mean_WindSpeed = mean(WindSpeed),
            mean_WindDirection = mean(WindDirection),
            mean_cloud = mean(Cloud),
            mean_energy = mean(energy))
us_byday_ts = ts(us_byday, start=c(2018,3,1))
plot(us_byday_ts, type="single")


## energy에 대한 관계
us_lm = lm(energy~. ,data=us[,-c(1,8,9)])
scale_us = scale
install.packages('QuantPsyc')
library(QuantPsyc)
us_lm_scale = lm.beta(us_lm)

scale_us = scale(us[,-c(1,8,9)])
scale_us = data.frame(scale_us)
scale_us_lm = lm(energy~., data=scale_us)
summary(scale_us_lm)

install.packages("car")
library(car)
vif(scale_us_lm)

library(forecast)
accuracy(scale_us_lm)







