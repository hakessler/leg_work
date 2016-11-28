library(devtools)
#install.packages("rnoaa")
#install_github("ropenscilabs/riem")
library(rnoaa)
#library(riem)
#install.packages("countyweather")
library(countyweather)
library(dplyr)
library(plyr)
library(tidyr)
library(weathermetrics)
library(ggplot2)
library(lubridate)

outbreak_loc <- data.frame("id" = c("portugal","pittsburgh","quebec",
                                    "stoke-on-trent","edinburgh","miyazaki","pas-de-calais",
                                    "pamplona","rapid city","sarpsborg",
                                    "barrow-in-furness","murcia","melbourne","bovenkarspel",
                                    "london","sydney","genesee1","genesee2","columbus", "bronx"),
                           "file_id" = c("portugal","pittsburgh","quebec",
                                         "stoke_on_trent","edinburgh","miyazaki","pas_de_calais",
                                         "pamplona","rapid_city","sarpsborg","barrow_in_furness",
                                         "murcia","melbourne","bovenkarspel","london","sydney",
                                         "genesee1","genesee2","columbus","bronx"),
                           "latitude" = c(38.96, 40.43, 46.85, 53.02, 55.94,
                                          31.89, 50.51, 42.81, 44.06, 59.28,
                                          54.10, 37.98,-37.86, 52.70, 51.52,
                                          -33.85, 43.09, 43.09, 39.98, 40.82),
                           "longitude" = c(-8.99, -79.98, -71.34, -2.15, -3.20,
                                           131.34, 2.37, -1.65, -103.22, 11.08, 
                                           -3.22, -1.12, 145.07, 5.24, -0.10,
                                           150.93, -83.63, -83.63, -82.99, -73.92),
                           "year_min" = c(2004, 2002, 2002, 2002, 2002, 1992, 
                                          1993, 1996, 1995, 1995, 1992, 1991, 
                                          1990, 1989, 1979, 2006, 2004, 2005,
                                          2003, 2005),
                           "date_min" = c("2004-01-01", "2002-01-01", "2002-01-01", "2002-01-01", "2002-01-01", "1992-01-01", 
                                          "1993-01-01", "1996-01-01", "1995-01-01", "1995-01-01", "1992-01-01", "1991-01-01", 
                                          "1990-01-01", "1989-01-01", "1979-01-01", "2006-01-01", "2004-01-01", "2005-01-01",
                                          "2003-01-01", "2005-01-01"),
                           "year_max" = c(2014, 2012, 2012, 2012, 2012, 2002,
                                          2003, 2006, 2005, 2005, 2002, 2001,
                                          2000, 1999, 1989, 2016, 2014, 2015,
                                          2013, 2015),
                           "date_max" = c("2014-12-31", "2012-12-31", "2012-12-31", "2012-12-31", "2012-12-31", "2002-12-31",
                                          "2003-12-31", "2006-12-31", "2005-12-31", "2005-12-31", "2002-12-31", 
                                          "2001-12-31", "2000-12-31", "1999-12-31", "1989-12-31", "2016-12-31", 
                                          "2014-12-31", "2015-12-31", "2013-12-31", "2015-12-31"),
                           "onset" = c("2004-10-14", "2012-08-26", "2012-07-18", "2012-07-02", "2012-05-01",
                                       "2002-07-18", "2003-11-28", "2006-06-01", "2005-05-26", "2005-05-12",
                                       "2002-07-30", "2001-06-26", "2000-04-17", "1999-02-25", "1989-01-01",
                                       "2016-04-25", "2014-06-06", "2015-05-04", "2013-07-09", "2015-07-12"),
                           stringsAsFactors = FALSE
)

outbreak_loc$date_min <- as.character(outbreak_loc$date_min)
outbreak_loc$date_max <- as.character(outbreak_loc$date_max)
outbreak_loc$onset <- ymd(outbreak_loc$onset)
for(i in 1:length(outbreak_loc$onset)) {
  a <- as.Date(outbreak_loc$onset[i])
  b <- a - 14
  outbreak_loc[i,10] <- paste(b)
}
outbreak_loc <- rename(outbreak_loc, replace = c("V10"="before_onset"))
#knitr::kable(outbreak_loc)


####STATIONS####
#Run the following code once

# station_data <- ghcnd_stations()[[1]]
# df <- list()
# for(i in 1:length(outbreak_loc$id))
# {
#   df[[i]] <- (meteo_nearby_stations(lat_lon_df = outbreak_loc[i,],
#                                     station_data = station_data,
#                                     var = c("PRCP","TAVG","TMAX","TMIN",
#                                             "AWND","MDPR"),
#                                     year_min = outbreak_loc[i, "year_min"],
#                                     year_max = outbreak_loc[i, "year_max"],
#                                     radius = 30)[[1]])
# }
# 
# names(df) <- outbreak_loc$id
# stations <- df
# saveRDS(stations, file = "stations.RData")

stations <- readRDS("stations.RData")

has_stations <- sapply(stations, function(x) nrow(x) > 0)
outbreak_loc_true <- outbreak_loc %>%  filter(has_stations)


#### DATA GATHER & SAVE ####
# Only need to run once to save the data 

for(i in which(has_stations))
{
  meteo_df <- meteo_pull_monitors(monitors = stations[[i]]$id,
                                  keep_flags = FALSE,
                                  date_min = outbreak_loc$date_min[i],
                                  date_max = outbreak_loc$date_max[i],
                                  var = c("prcp","snow","snwd","tmax","tmin","tavg"))

  coverage_df <- rnoaa::meteo_coverage(meteo_df, verbose = FALSE)
  filtered <- countyweather:::filter_coverage(coverage_df, 0.90)
  good_monitors <- unique(filtered$id)
  filtered_data <- dplyr::filter(meteo_df, id %in% good_monitors)
  averaged <- countyweather:::ave_weather(filtered_data)

  # For metrics that are reported in tenths of units (precipitation
  # and temperature), divide by 10 to get values in degrees Celsius and
  # millimeters
  which_tenth_units <- which(colnames(averaged) %in%
                               c("prcp", "tavg", "tmax", "tmin"))
  averaged[ , which_tenth_units] <- averaged[ , which_tenth_units] / 10

  file_name <- paste0("weather_files_temp/", outbreak_loc$file_id[i], ".rds")
  saveRDS(averaged, file_name)
}


###DATA ANALYZE###
# Only for locations that have >1 stations
# Files in "weather_files/" are in alphabetical order
df_stations <- arrange(outbreak_loc_true, id)

#Adding yday to the dataframe
for(i in 1:length(df_stations$onset)) {
  a <- yday(df_stations$onset[i])
  df_stations[i,11] <- paste(a)
  b <- yday(df_stations$before_onset[i])
  df_stations[i,12] <- paste(b)
}

df_stations <- rename(df_stations, replace = c("V11"="onset_yday"))
df_stations <- rename(df_stations, replace = c("V12"="before_onset_yday"))

####PLOTS####

# PLOT 1: show outbreak distribution
df_stations$yday <- yday(df_stations$onset)
df_stations$hemisphere <- c("N","N","N","N","N","N","S","N","N","N","N","N","N","N","S")
ggplot(df_stations, aes(yday)) + geom_histogram(binwidth = 1) + 
  xlim(c(0,366)) + facet_grid(. ~ hemisphere)

#PLOT 2: All weather data facetted 
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
}

#PLOT 3: TMAX and TMIN
for(file in list.files("weather_files"))
{
    city_name <- gsub(".rds", "", file)
    averaged <- readRDS(paste0("weather_files/", file))
    
    ex <- averaged %>%
      select(-ends_with("reporting")) %>%
      gather("metric", "value", -date)
  
    to_plot <- filter(ex, metric %in% c("tmax", "tmin"))
    b <- ggplot(to_plot, aes(x = date, y = value, color = metric)) + 
    geom_line() + ggtitle(city_name)
   print(b)
   
  }

# PLOT 4: PRCP
for(i in 1:length(list.files("weather_files")))
{
  file <- list.files("weather_files")[i]
  city_name <- gsub(".rds", "", file)
  averaged <- readRDS(paste0("weather_files/", file))
  
  ex <- averaged %>%
    select(-ends_with("reporting")) %>%
    gather("metric", "value", -date)
  
  to_plot <- filter(ex, metric=="prcp")
  a <- ggplot(to_plot, aes(x = date, y = value)) + ylab("PRCP (mm)") +
    geom_line() + ggtitle(city_name)
  print(a)
  
  c_plot <- filter(ex, metric=="prcp")
  int <- interval(ymd(df_stations[df_stations$id == 
                                     gsub("_", " ", city_name),
                                   "before_onset"]),
                  ymd(df_stations[df_stations$id == 
                                     gsub("_", " ", city_name),
                                   "onset"]))
  c_outbreak <- filter(c_plot, date %within% int) #%>%
  c_outbreak <- mutate(c_outbreak, day_in_seq = 1:nrow(c_outbreak))
  c <- ggplot(c_plot, aes(value)) + geom_histogram(binwidth = 2) +
    geom_vline(data = c_outbreak, 
               aes(xintercept = value, color = day_in_seq), 
               alpha = 0.25) + 
    ggtitle(city_name) + xlab("PRCP (mm)") +
    scale_color_gradientn(colors=rainbow(4))
  
  print(c)
  
  #percentiles    
  city_percentile <- ecdf(c_plot$value)(c_outbreak$value)
  c_outbreak$percentile <- city_percentile * 100
  
  d <- ggplot(c_outbreak, aes(x = day_in_seq, y = percentile)) +
    geom_bar(stat="identity") +
    ggtitle(city_name) +
    ylim(c(0,100))
  print(d)
  
  file_name <- paste0("percentile_data/prcp_year/", df_stations$file_id[i], "_prcp_year.rds")
  saveRDS(ex_outbreak, file_name)
  
}

# PLOT 5: TMAX
for(i in 1:length(list.files("weather_files")))
{
  file <- list.files("weather_files")[i]
  city_name <- gsub(".rds", "", file)
  averaged <- readRDS(paste0("weather_files/", file))
  
  ex <- averaged %>%
    select(-ends_with("reporting")) %>%
    gather("metric", "value", -date)
  
  to_plot <- filter(ex, metric=="tmax")
  to_plot$value <- to_plot$value * 10
  a <- ggplot(to_plot, aes(x = date, y = value)) + ylab("TMIN (C)") +
    geom_line() + ggtitle(city_name)
  print(a)
  
  c_plot <- filter(ex, metric=="tmax")
  c_plot$value <- c_plot$value * 10
  int <- interval(ymd(df_stations[df_stations$id == 
                                    gsub("_", " ", city_name),
                                  "before_onset"]),
                  ymd(df_stations[df_stations$id == 
                                    gsub("_", " ", city_name),
                                  "onset"]))
  c_outbreak <- filter(c_plot, date %within% int) #%>%
  c_outbreak <- mutate(c_outbreak, day_in_seq = 1:nrow(c_outbreak))
  c <- ggplot(c_plot, aes(value)) + geom_histogram(binwidth = 2) +
    geom_vline(data = c_outbreak, 
               aes(xintercept = value, color = day_in_seq), 
               alpha = 0.25) + 
    ggtitle(city_name) + xlab("TMAX (C)") +
    scale_color_gradientn(colors=rainbow(4))
  
  print(c)
  
  #percentiles
  city_percentile <- ecdf(c_plot$value)(c_outbreak$value)
  c_outbreak$percentile <- city_percentile * 100
  
  d <- ggplot(c_outbreak, aes(x = day_in_seq, y = percentile)) +
    geom_bar(stat="identity") +
    ggtitle(city_name) +
    ylim(c(0,100))
  print(d)
  
  file_name <- paste0("percentile_data/tmax_year/", df_stations$file_id[i], "_tmax_year.rds")
  saveRDS(ex_outbreak, file_name)
}
  
# PLOT 6: TMIN
for(i in 1:length(list.files("weather_files")))
{
  file <- list.files("weather_files")[i]
  city_name <- gsub(".rds", "", file)
  averaged <- readRDS(paste0("weather_files/", file))
  
  ex <- averaged %>%
    select(-ends_with("reporting")) %>%
    gather("metric", "value", -date)
  
  to_plot <- filter(ex, metric=="tmin")
  to_plot$value <- to_plot$value * 10
  a <- ggplot(to_plot, aes(x = date, y = value)) + ylab("TMIN (C)") +
    geom_line() + ggtitle(city_name)
  print(a)
  
  c_plot <- filter(ex, metric=="tmin")
  c_plot$value <- c_plot$value * 10
  int <- interval(ymd(df_stations[df_stations$id == 
                                     gsub("_", " ", city_name),
                                   "before_onset"]),
                  ymd(df_stations[df_stations$id == 
                                     gsub("_", " ", city_name),
                                   "onset"]))
  c_outbreak <- filter(c_plot, date %within% int) #%>%
  c_outbreak <- mutate(c_outbreak, day_in_seq = 1:nrow(c_outbreak))
  c <- ggplot(c_plot, aes(value)) + geom_histogram(binwidth = 2) +
    geom_vline(data = c_outbreak, 
               aes(xintercept = value, color = day_in_seq), 
               alpha = 0.25) + 
    ggtitle(city_name) + xlab("TMIN (C)") +
    scale_color_gradientn(colors=rainbow(4))
  
  print(c)
  
  #percentiles
  city_percentile <- ecdf(c_plot$value)(c_outbreak$value)
  c_outbreak$percentile <- city_percentile * 100
  
  d <- ggplot(c_outbreak, aes(x = day_in_seq, y = percentile)) +
    geom_bar(stat="identity") +
    ggtitle(city_name) +
    ylim(c(0,100))
  print(d)
  
  file_name <- paste0("percentile_data/tmin_year/", df_stations$file_id[i], "_tmin_year.rds")
  saveRDS(ex_outbreak, file_name)
}




#PRCP - SUBSET IN 2-WEEK SEASONAL RANGE 

for(i in 1:length(list.files("weather_files")))
{
  file <- list.files("weather_files")[i]
  city_name <- gsub(".rds", "", file)
  averaged <- readRDS(paste0("weather_files/", file))
  
  ex <- averaged %>%
    select(-ends_with("reporting")) %>%
    gather("metric", "value", -date)
  
  ex_perc <- ex
  ex_perc$date <- yday(ex_perc$date)
  ex_perc <- filter(ex_perc, date %in% seq(as.numeric(df_stations$before_onset_yday[i]), as.numeric(df_stations$onset_yday[i])))
  ex_perc <- filter(ex_perc, metric=="prcp")
  int <- interval(ymd(df_stations[df_stations$id == 
                                     gsub("_", " ", city_name),
                                   "before_onset"]),
                  ymd(df_stations[df_stations$id == 
                                     gsub("_", " ", city_name),
                                   "onset"]))
  ex_outbreak <- filter(ex, date %within% int)
  ex_outbreak <- filter(ex_outbreak, metric=="prcp")
  ex_outbreak <- mutate(ex_outbreak, day_in_seq = 1:nrow(ex_outbreak))
  
  #PERCENTILE DATA
  city_percentile <- ecdf(ex_perc$value)(ex_outbreak$value)
  ex_outbreak$percentile <- city_percentile * 100
  file_name <- paste0("percentile_data/prcp_seas/", df_stations$file_id[i], "_prcp_seas.rds")
  saveRDS(ex_outbreak, file_name)
  
}  

for(file in list.files("percentile_data/prcp_seas/")) 
{
  city_name <- gsub(".rds", "", file)
  ex_outbreak <- readRDS(paste0("percentile_data/prcp_seas", file))
d <- ggplot(ex_outbreak, aes(x = day_in_seq, y = percentile)) +
    geom_bar(stat="identity") +
    ggtitle(city_name) +
    ylim(c(0,100))
  print(d)
}

#TMAX - SUBSET IN 2-WEEK SEASONAL RANGE 

for(i in 1:length(list.files("weather_files")))
{
  file <- list.files("weather_files")[i]
  city_name <- gsub(".rds", "", file)
  averaged <- readRDS(paste0("weather_files/", file))
  
  ex <- averaged %>%
    select(-ends_with("reporting")) %>%
    gather("metric", "value", -date)
  
  ex_perc <- ex
  ex_perc$date <- yday(ex_perc$date)
  ex_perc <- filter(ex_perc, date %in% seq(as.numeric(df_stations$before_onset_yday[i]), as.numeric(df_stations$onset_yday[i])))
  ex_perc <- filter(ex_perc, metric=="tmax")
  int <- interval(ymd(df_stations[df_stations$id == 
                                    gsub("_", " ", city_name),
                                  "before_onset"]),
                  ymd(df_stations[df_stations$id == 
                                    gsub("_", " ", city_name),
                                  "onset"]))
  ex_outbreak <- filter(ex, date %within% int)
  ex_outbreak <- filter(ex_outbreak, metric=="tmax")
  ex_outbreak <- mutate(ex_outbreak, day_in_seq = 1:nrow(ex_outbreak))
  
  #PERCENTILE DATA
  city_percentile <- ecdf(ex_perc$value)(ex_outbreak$value)
  ex_outbreak$percentile <- city_percentile * 100
  file_name <- paste0("percentile_data/tmax_seas/", df_stations$file_id[i], "_tmax_seas.rds")
  saveRDS(ex_outbreak, file_name)
  
}  

for(file in list.files("percentile_data/tmax_seas/")) 
{
  city_name <- gsub(".rds", "", file)
  ex_outbreak <- readRDS(paste0("percentile_data/tmax_seas", file))
  d <- ggplot(ex_outbreak, aes(x = day_in_seq, y = percentile)) +
    geom_bar(stat="identity") +
    ggtitle(city_name) +
    ylim(c(0,100))
  print(d)
}

#TMIN - SUBSET IN 2-WEEK SEASONAL RANGE 

for(i in 1:length(list.files("weather_files")))
{
  file <- list.files("weather_files")[i]
  city_name <- gsub(".rds", "", file)
  averaged <- readRDS(paste0("weather_files/", file))
  
  ex <- averaged %>%
    select(-ends_with("reporting")) %>%
    gather("metric", "value", -date)
  
  ex_perc <- ex
  ex_perc$date <- yday(ex_perc$date)
  ex_perc <- filter(ex_perc, date %in% seq(as.numeric(df_stations$before_onset_yday[i]), as.numeric(df_stations$onset_yday[i])))
  ex_perc <- filter(ex_perc, metric=="tmin")
  int <- interval(ymd(df_stations[df_stations$id == 
                                    gsub("_", " ", city_name),
                                  "before_onset"]),
                  ymd(df_stations[df_stations$id == 
                                    gsub("_", " ", city_name),
                                  "onset"]))
  ex_outbreak <- filter(ex, date %within% int)
  ex_outbreak <- filter(ex_outbreak, metric=="tmin")
  ex_outbreak <- mutate(ex_outbreak, day_in_seq = 1:nrow(ex_outbreak))
  
  
  
  #PERCENTILE DATA
  city_percentile <- ecdf(ex_perc$value)(ex_outbreak$value)
  ex_outbreak$percentile <- city_percentile * 100
  file_name <- paste0("percentile_data/tmin_seas/", df_stations$file_id[i], "_tmin_seas.rds")
  saveRDS(ex_outbreak, file_name)
  
}  

for(file in list.files("percentile_data/tmax_seas/")) 
{
  city_name <- gsub(".rds", "", file)
  ex_outbreak <- readRDS(paste0("percentile_data/tmax_seas", file))
  d <- ggplot(ex_outbreak, aes(x = day_in_seq, y = percentile)) +
    geom_bar(stat="identity") +
    ggtitle(city_name) +
    ylim(c(0,100))
  print(d)
}

for(i in 1:length(df_stations$id))
{
file <- list.files("percentile_data/prcp_seas")[i]
prcp_seas <- readRDS(paste0("percentile_data/prcp_seas/", file))
prcp_seas <- arrange(prcp_seas, desc(day_in_seq))

file <- list.files("percentile_data/tmax_seas")[i]
tmax_seas <- readRDS(paste0("percentile_data/tmax_seas/", file))
tmax_seas <- arrange(tmax_seas, desc(day_in_seq))

file <- list.files("percentile_data/tmin_seas")[i]
tmin_seas <- readRDS(paste0("percentile_data/tmin_seas/", file))
tmin_seas <- arrange(tmin_seas, desc(day_in_seq))

file <- list.files("percentile_data/prcp_year")[i]
prcp_year <- readRDS(paste0("percentile_data/prcp_year/", file))
prcp_year <- arrange(prcp_year, desc(day_in_seq))

file <- list.files("percentile_data/tmax_year")[i]
tmax_year <- readRDS(paste0("percentile_data/tmax_year/", file))
tmax_year <- arrange(tmax_year, desc(day_in_seq))

file <- list.files("percentile_data/tmin_year")[i]
tmin_year <- readRDS(paste0("percentile_data/tmin_year/", file))
tmin_year <- arrange(tmin_year, desc(day_in_seq))

city_name <- df_stations$file_id[i]
city_name <- data.frame("days_before_onset" = c(0:14),
                      "TMAX_year" = c(tmax_year$percentile),
                      "TMAX_seasonal" = c(tmax_seas$percentile),
                      "TMIN_year" = c(tmin_year$percentile),
                      "TMIN_seasonal" = c(tmin_seas$percentile),
                      "PRCP_year" = c(prcp_year$percentile),
                      "PRCP_seasonal" = c(prcp_seas$percentile)
                    ) 
file_name <- paste0("percentile_tables/", df_stations$file_id[i], ".rds")
saveRDS(city_name, file_name)
}


#percentiles, look at literature time frames 