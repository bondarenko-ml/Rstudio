setwd("C:/Rzan")

#Загружаем библиотеку
library(tidyverse)
library(rnoaa)
library(nycflights13)
library(tidyr)
library(stringr)
library(dplyr)
library(tibble)
library(readr)
#station_data = ghcnd_stations() #Может занять несколько минут лучше выполнить один раз в месте с хорошим интернетом и сохранить результат
#station_data
#write.csv(station_data, file = "station_data.csv")
station_data = read.csv("station_data.csv")
#После получения списка всех станций, получите список станций ближайших к столице вашего региона,создав таблицу с именем региона и координатами его столицы
Obluchye = data.frame(id = "Obluchye", latitude = 49.0178200,  longitude = 131.0496000)
obluchye_around = meteo_nearby_stations(lat_lon_df = Obluchye, station_data = station_data,
                                    limit = 20, var = c("TAVG"),
                                    year_min = 2009, year_max = 2019)
#birobidzhan_around это список единственным элементом которого является таблица, содержащая идентификаторы метеостанций отсортированных по их 
# удалленности от Облучье, очевидно что первым элементом таблицы будет идентификатор метеостанции Тулы, его то мы и попытаемся получить
obluchye_id1 = obluchye_around[["Obluchye"]][["id"]][1]
obluchye_id2 = obluchye_around[["Obluchye"]][["id"]][2]
obluchye_id3 = obluchye_around[["Obluchye"]][["id"]][3]
obluchye_id4 = obluchye_around[["Obluchye"]][["id"]][4]
obluchye_id5 = obluchye_around[["Obluchye"]][["id"]][5]
obluchye_id6 = obluchye_around[["Obluchye"]][["id"]][6]
obluchye_id7 = obluchye_around[["Obluchye"]][["id"]][7]
obluchye_id8 = obluchye_around[["Obluchye"]][["id"]][8]
obluchye_id9 = obluchye_around[["Obluchye"]][["id"]][9]
obluchye_id10 = obluchye_around[["Obluchye"]][["id"]][10]
obluchye_id11 = obluchye_around[["Obluchye"]][["id"]][11]
obluchye_id12 = obluchye_around[["Obluchye"]][["id"]][12]
obluchye_id13 = obluchye_around[["Obluchye"]][["id"]][13]
obluchye_id14 = obluchye_around[["Obluchye"]][["id"]][14]
obluchye_id15 = obluchye_around[["Obluchye"]][["id"]][15]
obluchye_id16 = obluchye_around[["Obluchye"]][["id"]][16]
obluchye_id17 = obluchye_around[["Obluchye"]][["id"]][17]
obluchye_id18 = obluchye_around[["Obluchye"]][["id"]][18]
obluchye_id19 = obluchye_around[["Obluchye"]][["id"]][19]
obluchye_id20 = obluchye_around[["Obluchye"]][["id"]][20]
obluchye_id = obluchye_around[["Obluchye"]][["id"]]
#Для получения всех данных с метеостанции, зная ее идентификатор, используйте след. команду
all_obluchye_data1 = meteo_tidy_ghcnd(stationid = obluchye_id1)
all_obluchye_data2 = meteo_tidy_ghcnd(stationid = obluchye_id2)
all_obluchye_data3 = meteo_tidy_ghcnd(stationid = obluchye_id3)
all_obluchye_data4 = meteo_tidy_ghcnd(stationid = obluchye_id4)
all_obluchye_data5 = meteo_tidy_ghcnd(stationid = obluchye_id5)
all_obluchye_data6 = meteo_tidy_ghcnd(stationid = obluchye_id6)
all_obluchye_data7 = meteo_tidy_ghcnd(stationid = obluchye_id7)
all_obluchye_data8 = meteo_tidy_ghcnd(stationid = obluchye_id8)
all_obluchye_data9 = meteo_tidy_ghcnd(stationid = obluchye_id9)
all_obluchye_data10 = meteo_tidy_ghcnd(stationid = obluchye_id10)
all_obluchye_data11 = meteo_tidy_ghcnd(stationid = obluchye_id11)
all_obluchye_data12 = meteo_tidy_ghcnd(stationid = obluchye_id12)
all_obluchye_data13 = meteo_tidy_ghcnd(stationid = obluchye_id13)
all_obluchye_data14 = meteo_tidy_ghcnd(stationid = obluchye_id14)
all_obluchye_data15 = meteo_tidy_ghcnd(stationid = obluchye_id15)
all_obluchye_data16 = meteo_tidy_ghcnd(stationid = obluchye_id16)
all_obluchye_data17 = meteo_tidy_ghcnd(stationid = obluchye_id17)
all_obluchye_data18 = meteo_tidy_ghcnd(stationid = obluchye_id18)
all_obluchye_data19 = meteo_tidy_ghcnd(stationid = obluchye_id19)
all_obluchye_data20 = meteo_tidy_ghcnd(stationid = obluchye_id20)
all_obluchye_data1
#Уберём значения NA и соберём их в вектор
good<-complete.cases(all_obluchye_data1)
all_obluchye_data1[good,]
all_obluchye_data1<-all_obluchye_data1[good,]

#Сумма активных температур
summary(all_obluchye_data1$tavg>50)
#apply(as.matrix(all_obluchye_data1$tavg>50),1,function(x){sum(x)})
apply(as.matrix(all_obluchye_data1$tavg>50),2,sum)

