#install_github("ropenscilabs/rnoaa")
library(devtools)
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
                           "latitude" = c(38.96, 40.43, 46.85, 53.02, 55.94,
                                          31.89, 50.51, 42.81, 44.06, -43.51,
                                          59.28, 54.10, 37.98, -37.86, 52.7,
                                          51.52, 52.80, 40.00),
                           "longitude" = c(-8.99, -79.98, -71.34, -2.15, -3.20,
                                           131.34, 2.37, -1.65, -103.22,
                                           172.59, 11.08, -3.22, -1.12,
                                           145.07, 5.24, -0.10, -2.10, -75.11),
                           "year_min" = c(2004, 2003, 2002, 2002, 2002, 1992, 
                                          1993, 1996, 1995, 1995, 1995, 1992, 
                                          1991, 1990, 1989, 1979, 1975, 1966),
                           "year_max" = c(2014, 2013, 2012, 2012, 2012, 2002,
                                          2003, 2006, 2005, 2005, 2005, 2002,
                                          2001, 2000, 1999, 1989, 1985, 1976))
                           

#station_data <- ghcnd_stations()[[1]]


####DATA####

#df_all is not working correctly
#df_all <- meteo_nearby_stations(lat_lon_df = outbreak_loc,
                                     #station_data = station_data,
                                     #var = c("PRCP","TAVG","TMAX","TMIN","AWND","MDPR"),
                                     #year_min = 1966, year_max = 2015,
                                     #limit = 5)

city_names <- c("portugal","pittsburgh","quebec",
                  "stoke-on-trent","edinburgh","miyazaki","pas-de-calais",
                  "pamplona","rapid city","christchurch","sarpsborg",
                  "barrow-in-furness","murcia","melbourne","bovenkarspel",
                  "london","stafford","philadelphia")

for(i in 1:length(city_names))
            {
              df[i] <- (meteo_nearby_stations(lat_lon_df = outbreak_loc[i,],
                                  station_data = station_data,
                                  var = c("PRCP","TAVG","TMAX","TMIN",
                                          "AWND","MDPR"),
                                  year_min = outbreak_loc[i, "year_min"],
                                  year_max = outbreak_loc[i, "year_max"],
                                  radius = 30))
            }

names(df) <- city_names
stations <- df

###DATA###
# countyweather

meteo_df <- meteo_pull_monitors(monitors = stations$quebec$id,
                                keep_flags = FALSE,
                                date_min = "2002-01-01",
                                date_max = "2012-12-31",
                                var = "all")

coverage_df <- meteo_coverage(meteo_df, verbose = FALSE)

filtered <- filter_coverage(coverage_df, 0.90)
good_monitors <- unique(filtered$id)

filtered_data <- filter(meteo_df, id %in% good_monitors)

averaged <- ave_weather(filtered_data)

ggplot(averaged, aes(x=date, y=prcp)) + 
  ylab("PRCP (10ths of mm)") + xlab("Year") + ggtitle("Quebec PRCP") +
  geom_line() + theme_minimal()


#hourly

# 10:30-11:00 W