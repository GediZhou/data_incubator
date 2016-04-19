setwd("~/Documents/some files/data_incubator/Q3")
library("readr")
library("ggmap")
library("ggplot2")
library("dplyr")

crime <- read_csv("Crimes_-_2001_to_present.csv")
crime <- crime[!is.na(crime$Latitude) & !is.na(crime$Longitude),]
location <- crime[,c("Primary Type","District","Longitude", "Latitude")]
sample_loc <- location[sample.int(n = length(location$Longitude), size = 100000),
                       c("Primary Type","Longitude", "Latitude")]
counts_type <- group_by(sample_loc, `Primary Type`) %>%
    summarise(count = length(`Primary Type`))
#ignore crime types that happen less than 0.1%
type <- counts_type$`Primary Type`[counts_type$count > 100]
sample_loc <- sample_loc[sample_loc$`Primary Type` %in% type,]
map_chi <- get_map(location = 'Chicago', zoom = 11)
maxPoints <- ggmap(map_chi) +
    geom_point(aes(x = Longitude, y = Latitude, color = `Primary Type`), data = sample_loc,
               size = 1, alpha = 0.1) +
    guides(colour = guide_legend(override.aes = list(alpha = 1.0, size = 5.0)), 
           title = "Type of Crime")
ggsave("crime_map.png", maxPoints,w = 14, h = 10, units = "in")

crime_time <- crime[,"Date"]
crime_time[] <- lapply(crime_time
                    ,strptime, format = "%m/%d/%Y %I:%M:%S %p")
crime$Date <- crime_time$Date
rm(crime_time)
crime$hour <- crime$Date$hour

