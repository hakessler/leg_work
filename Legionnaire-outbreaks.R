library(devtools)
#install_github("ropenscilabs/rnoaa")
library(rnoaa)
library(ggmap)
library(countyweather)
library(dplyr)
library(plyr)
library(tidyr)
library(weathermetrics)
library(ggplot2)

outbreak_loc <- data.frame("id" = c("portugal","pittsburgh","quebec",
                                  "stoke-on-trent","edinburgh","miyazaki","pas-de-calais",
                                  "pamplona","rapid city","christchurch","sarpsborg",
                                  "barrow-in-furness","murcia","melbourne","bovenkarspel",
                                  "london","stafford","philadelphia"),
                           "latitude" = c(38.96,40.43,46.85,53.02,55.94,31.89,50.51,42.81,44.06,-43.51,59.28,54.10,37.98,-37.86,52.7,51.52,52.80,40.00),
                           "longitude" = c(-8.99,-79.98,-71.34,-2.15,-3.20,131.34,2.37,-1.65,-103.22,172.59,11.08,-3.22,-1.12,145.07,5.24,-0.10,-2.10,-75.11)
                           )

#station_data <- ghcnd_stations()[[1]]


####DATA####

#df_all is not working correctly
#df_all <- meteo_nearby_stations(lat_lon_df = outbreak_loc,
                                     #station_data = station_data,
                                     #var = c("PRCP","TAVG","TMAX","TMIN","AWND","MDPR"),
                                     #year_min = 1966, year_max = 2015,
                                     #limit = 5)

st_portugal <- meteo_nearby_stations(lat_lon_df = outbreak_loc[1,],
                            station_data = station_data,
                            var = c("PRCP","TAVG","TMAX","TMIN","AWND","MDPR"),
                            year_min = 2004, year_max = 2014,
                            limit = 5)

st_pittsburgh <- meteo_nearby_stations(lat_lon_df = outbreak_loc[2,],
                             station_data = station_data,
                             var = c("PRCP","TAVG","TMAX","TMIN","AWND","MDPR"),
                             year_min = 2003, year_max = 2013,
                             limit = 5)

st_quebec <- meteo_nearby_stations(lat_lon_df = outbreak_loc[3,],
                                       station_data = station_data,
                                       var = c("PRCP","TAVG","TMAX","TMIN","AWND","MDPR"),
                                       year_min = 2002, year_max = 2012,
                                       limit = 5)

st_stoke_on_trent <- meteo_nearby_stations(lat_lon_df = outbreak_loc[4,],
                                       station_data = station_data,
                                       var = c("PRCP","TAVG","TMAX","TMIN","AWND","MDPR"),
                                       year_min = 2002, year_max = 2012,
                                       limit = 5)

st_edinburgh <- meteo_nearby_stations(lat_lon_df = outbreak_loc[5,],
                                       station_data = station_data,
                                       var = c("PRCP","TAVG","TMAX","TMIN","AWND","MDPR"),
                                       year_min = 2002, year_max = 2012,
                                       limit = 5)

st_miyazaki <- meteo_nearby_stations(lat_lon_df = outbreak_loc[6,],
                                     station_data = station_data,
                                     var = c("PRCP","TAVG","TMAX","TMIN","AWND","MDPR"),
                                     year_min = 1992, year_max = 2002,
                                     limit = 5)

st_pas_de_calais <- meteo_nearby_stations(lat_lon_df = outbreak_loc[7,],
                                     station_data = station_data,
                                     var = c("PRCP","TAVG","TMAX","TMIN","AWND","MDPR"),
                                     year_min = 1993, year_max = 2003,
                                     limit = 5)

st_pamplona <- meteo_nearby_stations(lat_lon_df = outbreak_loc[8,],
                                          station_data = station_data,
                                          var = c("PRCP","TAVG","TMAX","TMIN","AWND","MDPR"),
                                          year_min = 1996, year_max = 2006,
                                          limit = 5)

st_rapid_city <- meteo_nearby_stations(lat_lon_df = outbreak_loc[9,],
                                     station_data = station_data,
                                     var = c("PRCP","TAVG","TMAX","TMIN","AWND","MDPR"),
                                     year_min = 1995, year_max = 2005,
                                     limit = 5)

st_christchurch <- meteo_nearby_stations(lat_lon_df = outbreak_loc[10,],
                                       station_data = station_data,
                                       var = c("PRCP","TAVG","TMAX","TMIN","AWND","MDPR"),
                                       year_min = 1995, year_max = 2005,
                                       limit = 5)

st_sarpsborg <- meteo_nearby_stations(lat_lon_df = outbreak_loc[11,],
                                       station_data = station_data,
                                       var = c("PRCP","TAVG","TMAX","TMIN","AWND","MDPR"),
                                       year_min = 1995, year_max = 2005,
                                       limit = 5)

st_barrow_in_furness <- meteo_nearby_stations(lat_lon_df = outbreak_loc[12,],
                             station_data = station_data,
                             var = c("PRCP","TAVG","TMAX","TMIN","AWND","MDPR"),
                             year_min = 1992, year_max = 2002,
                             limit = 5)

st_murcia <- meteo_nearby_stations(lat_lon_df = outbreak_loc[13,],
                             station_data = station_data,
                             var = c("PRCP","TAVG","TMAX","TMIN","AWND","MDPR"),
                             year_min = 1991, year_max = 2001,
                             limit = 5)

st_melbourne <- meteo_nearby_stations(lat_lon_df = outbreak_loc[14,],
                             station_data = station_data,
                             var = c("PRCP","TAVG","TMAX","TMIN","AWND","MDPR"),
                             year_min = 1990, year_max = 2000,
                             limit = 5)

st_bovenkarspel <- meteo_nearby_stations(lat_lon_df = outbreak_loc[15,],
                             station_data = station_data,
                             var = c("PRCP","TAVG","TMAX","TMIN","AWND","MDPR"),
                             year_min = 1989, year_max = 1999,
                             limit = 5)

st_london <- meteo_nearby_stations(lat_lon_df = outbreak_loc[16,],
                             station_data = station_data,
                             var = c("PRCP","TAVG","TMAX","TMIN","AWND","MDPR"),
                             year_min = 1979, year_max = 1989,
                             limit = 5)

st_stafford <- meteo_nearby_stations(lat_lon_df = outbreak_loc[17,],
                             station_data = station_data,
                             var = c("PRCP","TAVG","TMAX","TMIN","AWND","MDPR"),
                             year_min = 1975, year_max = 1985,
                             limit = 5)

st_philadelphia <- meteo_nearby_stations(lat_lon_df = outbreak_loc[18,],
                             station_data = station_data,
                             var = c("PRCP","TAVG","TMAX","TMIN","AWND","MDPR"),
                             year_min = 1966, year_max = 1976,
                             limit = 5)


###PORTUGAL###
# countyweather

meteo_df <- meteo_pull_monitors(monitors = st_portugal$portugal$id,
                                keep_flags = FALSE,
                                date_min = "2004-01-01",
                                date_max = "2014-12-31",
                                var = "all")

coverage_df <- meteo_coverage(meteo_df, verbose = FALSE)

filtered <- filter_coverage(coverage_df, 0.90)
good_monitors <- unique(filtered$id)

filtered_data <- filter(meteo_df, id %in% good_monitors)

averaged <- ave_weather(filtered_data)

ggplot(averaged, aes(x=date, y=prcp)) + geom_line() + theme_minimal()
ggplot(averaged, aes(x=date, y=tmax)) + geom_line() + theme_minimal()


#radius = 30
#hourly
#loops

# 10:30-11:00

#getmap("london")
#ggmap
