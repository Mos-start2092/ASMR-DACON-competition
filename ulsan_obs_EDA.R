### Predict Solar power generation

library(tidyverse)
library(lubridate)
library(ggcorrplot)

### data --------------------------------------------------------------------
info = read.csv("C:\\Users\\eunju\\Desktop\\Data Analysis\\Solar power generation\\data\\site_info.csv", 
                header=T,  fileEncoding="UTF-8", encoding = "CP949")
fcst_dj = read.csv("C:\\Users\\eunju\\Desktop\\Data Analysis\\Solar power generation\\data\\dangjin_fcst_data.csv",header=T)
fcst_us = read.csv("C:\\Users\\eunju\\Desktop\\Data Analysis\\Solar power generation\\data\\ulsan_fcst_data.csv",header=T)
obs_dj = read.csv("C:\\Users\\eunju\\Desktop\\Data Analysis\\Solar power generation\\data\\dangjin_obs_data.csv", header=T,fileEncoding="UTF-8", encoding = "CP949")
obs_us = read.csv("C:\\Users\\eunju\\Desktop\\Data Analysis\\Solar power generation\\data\\ulsan_obs_data.csv", header=T, fileEncoding="UTF-8", encoding = "CP949")
energy = read.csv("C:\\Users\\eunju\\Desktop\\Data Analysis\\Solar power generation\\data\\energy.csv",header=T)

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


# EDA ---------------------------------------------------------------------

tail(obs_us)
obs_us


# NA ----------------------------------------------------------------------
obs_us[obs_us %>% select(Temperature) %>% is.na %>% which,]
obs_us %>% 
  filter(ymd_hms("2020-08-26 01:00:00") <= time & time<= ymd_hms("2020-08-27 23:00:00"))
obs_us_temp_na = obs_us %>% select(Temperature) %>% is.na %>% which
t = obs_us[-obs_us_temp_na, "time"]
t_out = obs_us[obs_us_temp_na, "time"]
temp = obs_us[-obs_us_temp_na, "Temperature"]
temp_na_fill = approx(t,temp, xout=t_out)
obs_us[obs_us_temp_na,"Temperature"] = temp_na_fill$y


obs_us[obs_us %>% select(WindSpeed) %>% is.na %>% which,  ]
obs_us_wind_na = obs_us %>% select(WindSpeed) %>% is.na %>% which
obs_us %>% 
  filter(ymd_hms("2019-06-12 01:00:00") <= time & time<= ymd_hms("2019-06-12 23:00:00"))
for (i in obs_us_wind_na) {
  obs_us[i,4] = (obs_us[i-1,4] + obs_us[i+1,4])/2
  obs_us[i,5] = (obs_us[i-1,5] + obs_us[i+1,5])/2
}


obs_us[obs_us %>% select(Humidity) %>% is.na %>% which,]
obs_us_hum_na = obs_us %>% select(Humidity) %>% is.na %>% which
for (i in obs_us_hum_na) {
  obs_us[i,6] = (obs_us[i-1,6] + obs_us[i+1,6])/2
}

#Cloud-선형보간법
obs_us[obs_us %>% select(Cloud) %>% is.na %>% which ,] #굉장히 많음
obs_us_cloud_na = obs_us %>% select(Cloud) %>% is.na %>% which
t = obs_us[-obs_us_cloud_na, "time"]
t_out = obs_us[obs_us_cloud_na, "time"]
cloud = obs_us[-obs_us_cloud_na, "Cloud"]
cloud_na_fill = approx(t,cloud, xout=t_out)
obs_us[obs_us_cloud_na,"Cloud"] = cloud_na_fill$y

obs_us[which(is.na(obs_us)),]

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

write.csv(us,"us.csv")

# 추가변수 Rain,Snow,Iz,Is ----------------------------------------------------
obs_2018 = read.csv("obs_rain,snow,sun_2018.csv")
obs_2019 = read.csv("obs_rain,snow,sun_2019.csv")
obs_2020 = read.csv("obs_rain,snow,sun_2020.csv")
obs_2021 = read.csv("obs_rain,snow,sun_2021.csv")

obs_plus = rbind(obs_2018, obs_2019, obs_2020, obs_2021)
obs_us_plus = obs_plus[,-c(1,2,5,7,10)]
colnames(obs_us_plus) = c("time", "rain", "iz", "is", "snow")

time_split = data.frame(do.call('rbind', strsplit(obs_us_plus$time, split=" ", fixed=TRUE)))
colnames(time_split) = c("Day", "hour")
obs_us_plus = cbind(obs_us_plus, time_split)
obs_us_plus$hour = as.times(paste(obs_us_plus$hour , ":00"))

obs_us_plus = obs_us_plus %>% 
  filter(!(hour<=as.times("06:00:00")) & !( hour>=as.times("20:00:00")))

us_plus = cbind(us, obs_us_plus[,-c(1,6,7)])


# 추가변수 전처리 ----------------------------------------------------------------
head(us_plus)

#is
unique(us_plus$is) #일사량은 제외
us_plus = us_plus[,-12]

#iz -> 선형보간법
us_plus_del = us_plus[-nrow(us_plus),]
us_plus_iz_na = us_plus_del$iz %>% is.na %>% which
t = us_plus_del[-us_plus_iz_na, "time"]
t_out = us_plus_del[us_plus_iz_na, "time"]
izz = us_plus_del[-us_plus_iz_na, "iz"]
izz_na_fill = approx(t,izz, xout=t_out)
us_plus_del[us_plus_iz_na,"iz"] = izz_na_fill$y
which(is.na(us_plus_del$iz))

us_plus = rbind(us_plus_del, us_plus[nrow(us_plus),])
us_plus[nrow(us_plus),"iz"] = us_plus[nrow(us_plus)-1,"iz"]
which(is.na(us_plus$iz))

#snow -> 내렸다1 내리지않았다0
unique(us_plus$snow)
us_plus[which(is.na(us_plus$snow)), "snow"] = 0
us_plus$snow = ifelse(us_plus$snow>0, 1,0)
us_plus$snow = as.factor(us_plus$snow)

#rain - 기상청(약한비:3미만(1), 보통비:3~15(2), 강한비:15이상(3))
unique(us_plus$rain) #0.00은 무엇인가? - 말그대로 0을 의미하는 듯함
us_plus[which(is.na(us_plus$rain)), "rain"] = 0
us_plus[which(0 < us_plus$rain & us_plus$rain < 3), "rain"] =1
us_plus[which(3 <= us_plus$rain & us_plus$rain < 15), "rain"] =2
us_plus[which(us_plus$rain >= 15), "rain"] =3
us_plus$rain = as.factor(us_plus$rain)

us_plus[which(is.na(us_plus)),]
write.csv(us_plus, "us_plus.csv")















