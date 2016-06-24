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
library(lubridate)

outbreak_loc <- data.frame("id" = c("portugal","pittsburgh","quebec",
                                  "stoke-on-trent","edinburgh","miyazaki","pas-de-calais",
                                  "pamplona","rapid city","christchurch","sarpsborg",
                                  "barrow-in-furness","murcia","melbourne","bovenkarspel",
                                  "london","stafford","philadelphia", "sydney","genesee1",
                                  "genesee2","columbus", "bronx"),
                           "file_id" = c("portugal","pittsburgh","quebec",
                                         "stoke_on_trent","edinburgh","miyazaki","pas_de_calais",
                                         "pamplona","rapid_city","christchurch","sarpsborg",
                                         "barrow_in_furness","murcia","melbourne","bovenkarspel",
                                         "london","stafford","philadelphia","sydney","genesee1",
                                         "genesee2","columbus", "bronx"),
                           "latitude" = c(38.96, 40.43, 46.85, 53.02, 55.94,
                                          31.89, 50.51, 42.81, 44.06, -43.51,
                                          59.28, 54.10, 37.98, -37.86, 52.7,
                                          51.52, 52.80, 40.00, -33.85, 43.09,
                                          43.09, 39.98, 40.82),
                           "longitude" = c(-8.99, -79.98, -71.34, -2.15, -3.20,
                                           131.34, 2.37, -1.65, -103.22,
                                           172.59, 11.08, -3.22, -1.12,
                                           145.07, 5.24, -0.10, -2.10, -75.11,
                                           150.93, -83.63, -83.63, -82.99, -73.92),
                           "year_min" = c(2004, 2002, 2002, 2002, 2002, 1992, 
                                          1993, 1996, 1995, 1995, 1995, 1992, 
                                          1991, 1990, 1989, 1979, 1975, 1966,
                                          2006, 2004, 2005, 2003, 2005),
                           "date_min" = c("2004-01-01", "2002-01-01", "2002-01-01", "2002-01-01", "2002-01-01", "1992-01-01", 
                                          "1993-01-01", "1996-01-01", "1995-01-01", "1995-01-01", "1995-01-01", "1992-01-01", 
                                          "1991-01-01", "1990-01-01", "1989-01-01", "1979-01-01", "1975-01-01", "1966-01-01",
                                          "2006-01-01", "2004-01-01", "2005-01-01", "2003-01-01", "2005-01-01"),
                           "year_max" = c(2014, 2012, 2012, 2012, 2012, 2002,
                                          2003, 2006, 2005, 2005, 2005, 2002,
                                          2001, 2000, 1999, 1989, 1985, 1976,
                                          2016, 2014, 2015, 2013, 2015),
                           "date_max" = c("2014-12-31", "2012-12-31", "2012-12-31", "2012-12-31", "2012-12-31", "2002-12-31",
                                          "2003-12-31", "2006-12-31", "2005-12-31", "2005-12-31", "2005-12-31", "2002-12-31",
                                          "2001-12-31", "2000-12-31", "1999-12-31", "1989-12-31", "1985-12-31", "1976-12-31",
                                          "2016-12-31", "2014-12-31", "2015-12-31", "2013-12-31", "2015-12-31")
                           )

outbreak_loc$date_min <- as.character(outbreak_loc$date_min)
outbreak_loc$date_max <- as.character(outbreak_loc$date_max)

station_data <- ghcnd_stations()[[1]]

####DATA####

#df_all is not working correctly
#df_all <- meteo_nearby_stations(lat_lon_df = outbreak_loc,
                                     #station_data = station_data,
                                     #var = c("PRCP","TAVG","TMAX","TMIN","AWND","MDPR"),
                                     #year_min = 1966, year_max = 2015,
                                     #limit = 5)

city_names <- c("portugal","pittsburgh","quebec",
                  "stoke-on-trent","edinburgh","miyazaki","pas-de-calais",
                  "pamplona","rapid_city","christchurch","sarpsborg",
                  "barrow-in-furness","murcia","melbourne","bovenkarspel",
                  "london","stafford","philadelphia","sydney","genesee1",
                  "genesee2","columbus", "bronx")

df <- list()
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
stations

### DATA AND PLOTS ###
for(i in c(2,3,6,8,9,10,11,13,14,15,16,19,20,21,22))
{
   meteo_df <- meteo_pull_monitors(monitors = stations[[i]]$id,
                                keep_flags = FALSE,
                                date_min = outbreak_loc$date_min[i],
                                date_max = outbreak_loc$date_max[i],
                                var = c("prcp","snow","snwd","tmax","tmin","tavg"))
  coverage_df <- meteo_coverage(meteo_df, verbose = FALSE)
  filtered <- filter_coverage(coverage_df, 0.90)
  good_monitors <- unique(filtered$id)
  filtered_data <- filter(meteo_df, id %in% good_monitors)
  averaged <- ave_weather(filtered_data)

  file_name <- paste0("weather_files/", outbreak_loc$file_id[i], ".rds")
  saveRDS(averaged, file_name)
  #readRDS(file_name)
  
}

outbreak_start <- data.frame("id"=c("bovenkarspel","bronx","christchurch","columbus","genesee1",
                                    "genesee2","london","melbourne","miyazaki","murcia","pamplona",
                                    "pittsburgh","quebec","rapid_city","sarpsborg","sydney"),
                             "start_date"=c("1999-02-25","2015-07-12","2005-04-04","2013-07-09",
                                            "2014-06-06","2015-05-04","1989-01-01","2000-04-17",
                                            "2002-07-18","2001-06-26","2006-06-01","2012-08-26",
                                            "2012-07-18","2005-05-26","2005-05-12","2016-04-25")
                             )
outbreak_start$start_date <- ymd(outbreak_start$start_date)
for(i in 1:length(outbreak_start$start_date)) {
  a <- as.Date(outbreak_start$start_date[i])
  b <- a - 14
  outbreak_start[i,3] <- paste(b)
}
outbreak_start <- rename(outbreak_start, replace = c("V3"="int_start"))
ggplot(outbreak_start, aes(yday)) + geom_histogram(binwidth = 1)



for(file in list.files("weather_files"))
  {
  city_name <- gsub(".rds", "", file)
  averaged <- readRDS(paste0("weather_files/", file))
  
  ex <- averaged %>%
    select(-ends_with("reporting")) %>%
    gather("metric", "value", -date)
  
  a <- ggplot(ex, aes(x = date, y = value)) + geom_line() +
    facet_wrap(~ metric, ncol = 2, scales = "free_y") + 
    ggtitle(city_name)
  print(a)
  
  to_plot <- filter(ex, metric %in% c("tmax", "tmin"))
  b <- ggplot(to_plot, aes(x = date, y = value, color = metric)) + 
    geom_line() + ggtitle(city_name)
  print(b)
  
  }


for(file in list.files("weather_files"))
{
  city_name <- gsub(".rds", "", file)
  averaged <- readRDS(paste0("weather_files/", file))
  
  ex <- averaged %>%
    select(-ends_with("reporting")) %>%
    gather("metric", "value", -date)
  
  for(i in 1:length(outbreak_start$id)) {
    c_plot <- filter(ex, metric %in% c("prcp"))
    int <- interval(ymd(outbreak_start$int_start[i]), ymd(outbreak_start$start_date[i]))
    c_outbreak <- filter(c_plot, date %within% int)
    c <- ggplot(c_plot, aes(value)) + geom_histogram(binwidth = 0.5)
      + geom_vline(xintercept = c_outbreak$value)
    print(c)
  }
}

outbreak_start[] <- NULL
outbreak_start_n <- outbreak_start[,3] <- NULL

#outbreak day of year plot - each hemisphere
#yday lubridate yday(start_date)
#ymd to convert
