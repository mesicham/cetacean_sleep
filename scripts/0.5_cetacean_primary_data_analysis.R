# Section 0: Packages -----------------------------------------------------
library(ape) 
library(phangorn)
library(stringr)
library(here)
library(gsheet)
library(dplyr)
library(phytools)
library(geiger)
library(dplyr)
library(readxl)
library(tidyr)
library(lubridate)
library(suncalc)
library(ggplot2)
setwd(here())
#to find sunset sunrise times
#install.packages("suntools")
library(suntools)
#to find timezones based on coordinates
#install.packages("lutz")
library(lutz)
library(parsedate)
#install.packages("R.matlab")
library(R.matlab)

setwd(here())
source("scripts/fish_sleep_functions.R")

# Section 1: Hyperoodon planifrons data ---------------------------------

#data from https://doi.org/10.1111/mms.12216 
hyper <- read_xlsx(here("Artiodactyla_activity_data/Trickey_2015_Hyperoodon_planifrons.xlsx"))

hyper <- hyper %>% separate("Encounter ID Date/time (GMT) Latitude Longitude Signal count", into = c("Encounter_ID", "Date", "Month", "Year", "Time", "Latitude", "Longitude", "Signal_count"), sep = " ")
hyper <- hyper %>% separate("Time", into = c("Start_time", "End_time"), sep = "–")

hyper$Signal_count <- as.numeric(hyper$Signal_count)
hyper$Start_time <- as.numeric(hyper$Start_time)
hyper$End_time <- as.numeric(hyper$End_time)

hyper <- hyper %>% mutate(Start_time = Start_time/100) %>% 
  mutate(Start_hour = as.integer(Start_time), Start_min = ((Start_time - as.integer(Start_time)) * 100/60)) %>%
  mutate(Start_time = Start_hour + Start_min)

hyper <- hyper %>% mutate(date = paste("2014", "02", Date, sep = "-")) 
hyper$date <- as.Date(parse_date_time(hyper$date, orders = "ymd"))

#LOESS (locally estimated scatterplot smoothing) which does not require you to describe a model
#It takes small subsets of the data along the independent variable and makes many models (usually first or second degree polynomials) and joins them together.
#from https://andrewirwin.github.io/data-visualization/working-models.html
ggplot(hyper, aes(y = Signal_count, x = Start_time)) + geom_point() + geom_smooth(method = "loess", formula = "y~x")

#since we are south and west, transform the coordinates to be negative 
hyper$lat <- substr(hyper$Latitude, start= 1, stop = 2)
hyper$lon <- substr(hyper$Longitude, start= 1, stop = 2)
hyper <- transform(hyper, lat = as.numeric(lat)*(-1))
hyper <- transform(hyper, lon = as.numeric(lon)*(-1))

#function to determine timezone from coordinates
hyper$timezone <- tz_lookup_coords(hyper$lat, hyper$lon, method = "accurate")

#test for first row. Sunrise at 9:21 pm (21:21), sunset at 12:18 (12:28) pm
#returns 4:23 sunrise and 19:19 sunset
getSunlightTimes(date = as.Date(parse_date_time("2014-2-19", orders = "ymd")), 
                 lat = -60, lon = -54, tz = tz_lookup_coords(-60, -54, method = "accurate"))

#find out the coordinates of each location and use to calculate sunrise and set times
#use nautical dawn as the start of dawn and sunrise end as the end (encompasses dawn) 
#use sunset start as the start of dusk and nautical dusk as the end (encompasses dusk) 

sun_times <- getSunlightTimes(data = hyper[, c("date", "lon", "lat")], 
                              keep = c("sunriseEnd", "sunsetStart", "nauticalDawn", "nauticalDusk"),
                              tz = hyper[, c("timezone")])

sun_times <- sun_times %>% separate_wider_delim(cols = c(4:7), delim = " ", names_sep = "_") %>% select(date, nauticalDawn_2, nauticalDusk_2, sunriseEnd_2 ,sunsetStart_2)
sun_times <- sun_times %>% separate_wider_delim(cols = c(2:5), delim = ":", names_sep = "_") %>% select(date, nauticalDawn_2_1, nauticalDawn_2_2, nauticalDusk_2_1, nauticalDusk_2_2, sunriseEnd_2_1, sunriseEnd_2_2, sunsetStart_2_1, sunsetStart_2_2)
sun_times <- sun_times %>% mutate(dawn_start = as.numeric(nauticalDawn_2_1) + as.numeric(nauticalDawn_2_2)/60) %>% 
  mutate(dawn_end = as.numeric(sunriseEnd_2_1) + as.numeric(sunriseEnd_2_2)/60) %>% 
  mutate(dusk_start = as.numeric(sunsetStart_2_1) + as.numeric(sunsetStart_2_2)/60) %>% 
  mutate(dusk_end = as.numeric(nauticalDusk_2_1) + as.numeric(nauticalDusk_2_2)/60) %>% select(date, dusk_start, dusk_end, dawn_start, dawn_end)

hyper <- cbind(hyper, sun_times) %>% select("Date", "Month", "Year", "Start_time", "Signal_count", "date", "dusk_start", "dusk_end", "dawn_start", "dawn_end")
#study took place in the anarctic (South Orkney Islands, South Shetland Islands, and Antarctic Peninsula)

#with suncalc times
hyper_plot1 <- 
ggplot(hyper, aes(y = Signal_count, x = Start_time)) + 
  theme_classic() +
  annotate(geom = "rect", xmin = mean(hyper$dawn_start), xmax = mean(hyper$dawn_end), ymin = -Inf, ymax = Inf, fill = "pink") +
  annotate(geom = "rect", xmin = mean(hyper$dusk_start), xmax = mean(hyper$dusk_end), ymin = -Inf, ymax = Inf, fill = "pink") +
  annotate(geom = "rect", xmin = 0, xmax = mean(hyper$dawn_start), ymin = -Inf, ymax = Inf, fill = "grey70") +
  annotate(geom = "rect", xmin = mean(hyper$dusk_end), xmax = 24, ymin = -Inf, ymax = Inf, fill = "grey70") +
  #geom_point() + 
  scale_x_continuous(breaks = c(0,5,10,15,20,25)) + 
  labs(x = "Hour", y = "Signal count") +
  geom_smooth(method = "loess", formula = "y~x", colour = "black") +
  ggtitle("Hyperoodon planifrons") + theme(plot.title = element_text(size = 11))

###new hyperoodon data from Barlow et al, 2021
#https://doi.org/10.1016/j.dsr2.2021.104973

hyper2 <- read_xlsx(here("Artiodactyla_activity_data/Barlow_2021_Hyperoodon_planifrons.xlsx"))
hyper2$`Event sequential number Event ID Start date/time (UTC) Event type Number of echolocation signals South latitude West longitude` <- str_replace(hyper2$`Event sequential number Event ID Start date/time (UTC) Event type Number of echolocation signals South latitude West longitude`, pattern = "Southern bottlenose whale", replacement = "Southern_bottlenose_whale")

hyper2 <- hyper2 %>% separate("Event sequential number Event ID Start date/time (UTC) Event type Number of echolocation signals South latitude West longitude", into = c("Event", "sequential_number", "Event_ID", "Start_date_time", "Event_type", "Signal_count", "South_latitude", "West_longitude"), sep = " ")

#remove anything that isn't a southern bottlenose whale 
hyper2 <- filter(hyper2, Event_type == "Southern_bottlenose_whale")
hyper2$Start_date_time <- str_replace(hyper2$Start_date_time, pattern = ":", replacement = "")
hyper2$Signal_count <- as.numeric(hyper2$Signal_count)

#since we are south and west, transform the coordinates to be negative 
hyper2 <- mutate(hyper2, lat = as.numeric(South_latitude)*(-1))
hyper2 <- mutate(hyper2, lon = as.numeric(West_longitude)*(-1))

#make columns for the dates so it can be interpreted by the sunriset function
hyper2 <- hyper2 %>% separate(Event_ID, into = c("Month", "Day", "Year"), sep = "/")
hyper2$date <- as.Date(as.POSIXct(paste0(hyper2$Year, "-", hyper2$Month, "-", hyper2$Day)))
     
hyper2$Start_date_time <- as.numeric(hyper2$Start_date_time)      
hyper2 <- hyper2 %>% mutate(Start_time = Start_date_time/100) %>% 
  mutate(Start_hour = as.integer(Start_time), Start_min = ((Start_time - as.integer(Start_time)) * 100/60)) %>%
  mutate(Start_time = Start_hour + Start_min)

#function to determine timezone from coordinates
hyper2$timezone <- tz_lookup_coords(hyper2$lat, hyper2$lon, method = "accurate")

#test run
#off the Falkland islands (-55, -47) on January 1 sunrise was at 4:33am and sunset at 9:48pm (21:48)
#https://www.timeanddate.com/sun/@-55,-47?month=1&year=2020
#suncalc returns 3:34 sunrise and 20:50 sunset. Not perfect but close
getSunlightTimes(date = as.Date(parse_date_time("2020-01-01", orders = "ymd")), 
                 lat = -55, lon = -47, tz = tz_lookup_coords(-54, -47, method = "accurate"))

#find out the coordinates of each location and use to calculate sunrise and set times
#use nautical dawn as the start of dawn and sunrise end as the end (encompasses dawn) 
#use sunset start as the start of dusk and nautical dusk as the end (encompasses dusk) 

#using timezones calculated above gives an invalid tz error so input manually
sun_times <- getSunlightTimes(data = hyper2[, c("date", "lon", "lat")], 
                              keep = c("sunriseEnd", "sunsetStart", "nauticalDawn", "nauticalDusk"),
                              tz =  "Atlantic/South_Georgia")

sun_times <- sun_times %>% separate_wider_delim(cols = c(4:7), delim = " ", names_sep = "_") %>% select(date, nauticalDawn_2, nauticalDusk_2, sunriseEnd_2 ,sunsetStart_2)
sun_times <- sun_times %>% separate_wider_delim(cols = c(2:5), delim = ":", names_sep = "_") %>% select(date, nauticalDawn_2_1, nauticalDawn_2_2, nauticalDusk_2_1, nauticalDusk_2_2, sunriseEnd_2_1, sunriseEnd_2_2, sunsetStart_2_1, sunsetStart_2_2)
sun_times <- sun_times %>% mutate(dawn_start = as.numeric(nauticalDawn_2_1) + as.numeric(nauticalDawn_2_2)/60) %>% 
  mutate(dawn_end = as.numeric(sunriseEnd_2_1) + as.numeric(sunriseEnd_2_2)/60) %>% 
  mutate(dusk_start = as.numeric(sunsetStart_2_1) + as.numeric(sunsetStart_2_2)/60) %>% 
  mutate(dusk_end = as.numeric(nauticalDusk_2_1) + as.numeric(nauticalDusk_2_2)/60) %>% select(dusk_start, dusk_end, dawn_start, dawn_end)

#replace 0 with 24 so mean dusk end is calculated properly later
sun_times$dusk_end[is.na(sun_times$dusk_end)] <- 1
sun_times$dusk_end[as.integer(sun_times$dusk_end) == 0] <- sun_times$dusk_end[as.integer(sun_times$dusk_end) == 0] + 24
sun_times$dusk_end[sun_times$dusk_end == 1] <- NA

hyper2 <- cbind(hyper2, sun_times) %>% select(c(Signal_count, lat, lon, date, Start_time, timezone, dusk_start, dusk_end, dawn_start, dawn_end))

hyper_plot2 <- 
ggplot(hyper2, aes(y = Signal_count, x = Start_time)) + 
  theme_classic() +
  annotate(geom = "rect", xmin = mean(hyper2$dawn_start, na.rm = TRUE), xmax = mean(hyper2$dawn_end, na.rm = TRUE), ymin = -Inf, ymax = Inf, fill = "pink") +
  annotate(geom = "rect", xmin = mean(hyper2$dusk_start, na.rm = TRUE), xmax = mean(hyper2$dusk_end, na.rm = TRUE), ymin = -Inf, ymax = Inf, fill = "pink") +
  annotate(geom = "rect", xmin = 0, xmax = mean(hyper2$dawn_start, na.rm = TRUE), ymin = -Inf, ymax = Inf, fill = "grey70") +
  annotate(geom = "rect", xmin = mean(hyper2$dusk_end, na.rm = TRUE), xmax = 24, ymin = -Inf, ymax = Inf, fill = "grey70") +
  scale_x_continuous(breaks = c(0, 5, 10, 20, 25)) +
  labs(x = "Hour", y = "Signal count") +
  geom_smooth(method = "loess", formula = "y~x", colour = "black") +
  ggtitle("Hyperoodon planifrons") + theme(plot.title = element_text(size = 11))

#study took place in  Falkland Islands to the South Sandwich Islands and South Georgia from December 30, 2019 to January 29, 2020 (Leg 1)
#only one detection occurred on the second leg of the trip from King George Island to Puerto Williams, Chile via the Antarctic Peninsula from February 11 to 27, 2020 (Leg 2)

hyper_merged <- rbind(hyper[, c("Start_time", "Signal_count", "dusk_start", "dusk_end", "dawn_start", "dawn_end")], hyper2[, c("Start_time", "Signal_count", "dusk_start", "dusk_end", "dawn_start", "dawn_end")])

hyper_plot3 <-
  ggplot(hyper_merged, aes(y = Signal_count, x = Start_time)) + 
  theme_classic() +
  annotate(geom = "rect", xmin = mean(hyper_merged$dawn_start, na.rm = TRUE), xmax = mean(hyper_merged$dawn_end, na.rm = TRUE), ymin = -Inf, ymax = Inf, fill = "pink") +
  annotate(geom = "rect", xmin = mean(hyper_merged$dusk_start, na.rm = TRUE), xmax = mean(hyper_merged$dusk_end, na.rm = TRUE), ymin = -Inf, ymax = Inf, fill = "pink") +
  annotate(geom = "rect", xmin = 0, xmax = mean(hyper_merged$dawn_start, na.rm = TRUE), ymin = -Inf, ymax = Inf, fill = "grey70") +
  annotate(geom = "rect", xmin = mean(hyper_merged$dusk_end, na.rm = TRUE), xmax = 24, ymin = -Inf, ymax = Inf, fill = "grey70") +
  scale_x_continuous(breaks = c(0, 5, 10, 20, 25)) +
  labs(x = "Hour", y = "Signal count") + #geom_rug() + 
  geom_smooth(method = "loess", formula = "y~x", colour = "black") +
  ggtitle("Hyperoodon planifrons")

# pdf(here("Figure_folder/Hyperoodon_activity_pattern.pdf"), width = 5, height = 3)
# hyper_plot3
# dev.off()

# Section 2: vaquita ---------------------------------------------------------
#study: https://iucn-csg.org/wp-content/uploads/2023/06/Vaquita-Survey-2023-Main-Report.pdf
#data from appendix: https://iucn-csg.org/wp-content/uploads/2023/06/Vaquita-Survey-2023-Appendices-FINAL.pdf

#load in the data
vaquita <- read.csv(here("Artiodactyla_activity_data/Jaramillo-Legorreta_2023_phocoena_sinus.csv"))
vaquita$datetime <- parse_date(vaquita$Start)
vaquita$hour <- hour(vaquita$datetime)
vaquita <- vaquita[order(vaquita$hour, decreasing = TRUE), ]

#red lines indicate onset of dawn and dusk
ggplot(vaquita, aes(x = hour, fill = Date)) + geom_bar() + geom_vline(xintercept = 5.75, color = "red", size = 1) + geom_vline(xintercept = 19.5, color = "red", size = 1)

ggplot(vaquita, aes(x = hour)) + geom_density(size = 1) + geom_vline(xintercept = 5.75, color = "red", size = 1) + geom_vline(xintercept = 19.5, color = "red", size = 1)

#all data is taken from May in San Felipe, Mexico so sunrise times are about 5:45am and sunset is around 19:30pm
vaquita$diel <- "day"
for(i in 1:60)
  if(vaquita[i, "hour"] > 19.5){
    vaquita[i, "diel"] <- "night"
  } 

for(i in 1:60)
  if(vaquita[i, "hour"] < 5.75){
    vaquita[i, "diel"] <- "night"
  } 

#civil twilight lasts for about an 45 minutes in san felipe 
#so dawn is from 5am-5:45 and dusk is 7:30-8:15 (19:30-20:15)

for(i in 1:60)
  if(vaquita[i, "hour"] >= 19.5 & vaquita[i, "hour"] <= 20.25){
    vaquita[i, "diel"] <- "dusk"
  } 

for(i in 1:60)
  if(vaquita[i, "hour"] <= 5.75 & vaquita[i, "hour"] >= 5){
    vaquita[i, "diel"] <- "dawn"
  } 

ggplot(vaquita, aes(x = hour, fill = diel)) + geom_bar() + geom_vline(xintercept = 5.75, color = "red", size = 1) + geom_vline(xintercept = 19.5, color = "red", size = 1)

vaquita_plot <- 
  ggplot(vaquita, aes(x = hour)) +   
  theme_classic() +
  annotate(geom = "rect", xmin = 5, xmax = 5.75, ymin = -Inf, ymax = Inf, fill = "pink") +
  annotate(geom = "rect", xmin = 19.5, xmax = 20.25, ymin = -Inf, ymax = Inf, fill = "pink") +
  annotate(geom = "rect", xmin = 0, xmax = 5, ymin = -Inf, ymax = Inf, fill = "grey") +
  annotate(geom = "rect", xmin = 20.25, xmax = 24, ymin = -Inf, ymax = Inf, fill = "grey") +
  labs(x = "Hour", y = "Density of detections") +
  geom_density(size = 1) + ggtitle("Phocoena sinus") + theme(plot.title = element_text(size = 11))
   
# pdf(here("Figure_folder/Phoeca_sinus_activity_pattern.pdf"), width = 5, height = 3)
# vaquita_plot
# dev.off()

# Section 3: Camera trap ungulates -------------------------------------------
#data from https://doi.org/10.1002/ecy.4237 
#For this analysis, we focused on six camera trap grids in the savanna biome in 
#northern South Africa: the Associated Private Nature Reserves (around Kruger National Park),
#Kruger National Park, Madikwe Game Reserve, Pilanesberg National Park, Somkhanda Game Reserve and Venetia Limpopo Nature Reserve 

#Camera trap data for impala, kudu and wildebeest
camera_trap.df <- read.csv(here("Artiodactyla_activity_data/Nicvert_2024_ruminants.csv"))
camera_trap.df$eventDateTime <- paste(camera_trap.df$eventDate, camera_trap.df$eventTime, sep = " ")
camera_trap.df$eventDate <- parse_date_time(camera_trap.df$eventDate, orders = "ymd")
camera_trap.df$eventDateTime <- as_datetime(camera_trap.df$eventDateTime)
camera_trap.df$hour <- hour(camera_trap.df$eventDateTime)
#convert minute to fraction of hour (out of 60)
camera_trap.df$min <- minute(camera_trap.df$eventDateTime)/60
camera_trap.df$hourmin <- as.numeric(camera_trap.df$hour) + as.numeric(camera_trap.df$min)

#plot number of detections per hour and minute, separate by location (six different parks) 
ggplot(camera_trap.df, aes(x = hourmin)) + geom_density(size = 1) + facet_wrap(~locationID)

#sites are labelled A-F so its undetermined which site is which park
#therefore we can't use the specific coordinates for each so use coordinates roughly equidistant from all (-25, 30)

#use suncalc to get sunrise and sunset times
#test run: sunrise should be 5:02, sunset at 6:32. Returns 5:03 sunrise and 6:33 sunset!
getSunlightTimes(date = as.Date(parse_date_time("2019-11-26", orders = "ymd")), lat = -25, lon = 30, tz = tz_lookup_coords(-25, 30))

#find out the coordinates of each location and use to calculate sunrise and set times
#use nautical dawn as the start of dawn and sunrise end as the end (encompasses dawn) -about an hour
#use sunset start as the start of dusk and nautical dusk as the end (encompasses dusk) -also about an hour
sun_times <- getSunlightTimes(date = as.Date(camera_trap.df$eventDate), 
                              lat = -25, lon = 30, 
                              keep = c("sunriseEnd", "sunsetStart", "nauticalDawn", "nauticalDusk"),
                              tz = tz_lookup_coords(-25, 30))

sun_times <- sun_times %>% separate_wider_delim(cols = c(4:7), delim = " ", names_sep = "_") %>% select(date, nauticalDawn_2, nauticalDusk_2, sunriseEnd_2 ,sunsetStart_2)
sun_times <- sun_times %>% separate_wider_delim(cols = c(2:5), delim = ":", names_sep = "_") %>% select(date, nauticalDawn_2_1, nauticalDawn_2_2, nauticalDusk_2_1, nauticalDusk_2_2, sunriseEnd_2_1, sunriseEnd_2_2, sunsetStart_2_1, sunsetStart_2_2)
sun_times <- sun_times %>% mutate(dawn_start = as.numeric(nauticalDawn_2_1) + as.numeric(nauticalDawn_2_2)/60) %>% 
  mutate(dawn_end = as.numeric(sunriseEnd_2_1) + as.numeric(sunriseEnd_2_2)/60) %>% 
  mutate(dusk_start = as.numeric(sunsetStart_2_1) + as.numeric(sunsetStart_2_2)/60) %>% 
  mutate(dusk_end = as.numeric(nauticalDusk_2_1) + as.numeric(nauticalDusk_2_2)/60) %>% select(date, dusk_start, dusk_end, dawn_start, dawn_end)
  
camera_trap.df <- cbind(camera_trap.df, sun_times)

#sunset and sunrise times seem to vary most from month/season?
camera_trap.df$month <- month(camera_trap.df$eventDate)

#sun times vary by only 1.7 hours across the year, so taking the mean should be a good approximation
camera_trap.df %>% summarize(dawn_end = max(dawn_end) - min(dawn_end),  dawn_start = max(dawn_start) - min(dawn_start),
                             dusk_end = max(dusk_start) - min(dusk_start), dusk_start = max(dusk_start) - min(dusk_start))

#Impala Aepyceros melampus
impala <- camera_trap.df %>% filter(snapshotName == "impala")
impala_plot <-
  ggplot(impala, aes(x = hourmin, colour = locationID)) +
  theme_classic() + 
  scale_colour_viridis_d(name = "Location", option = "magma", begin = 0, end = 0.8) + 
  annotate(geom = "rect", xmin = mean(impala$dawn_start), xmax = mean(impala$dawn_end), ymin = -Inf, ymax = Inf, fill = "pink") +
  annotate(geom = "rect", xmin = mean(impala$dusk_start), xmax = mean(impala$dusk_end), ymin = -Inf, ymax = Inf, fill = "pink") +
  annotate(geom = "rect", xmin = 0, xmax = mean(impala$dawn_start), ymin = -Inf, ymax = Inf, fill = "grey70") +
  annotate(geom = "rect", xmin = mean(impala$dusk_end), xmax = 24, ymin = -Inf, ymax = Inf, fill = "grey70") +
  labs(x = "Hour", y = "Density of detections") + 
  geom_density(size = 1) + ggtitle("Aepyceros melampus")  + theme(plot.title = element_text(size = 11))

#Greater kudu Tragelaphus strepsiceros
kudu <- camera_trap.df %>% filter(snapshotName == "kudu")
kudu_plot <- 
  ggplot(kudu, aes(x = hourmin, colour = locationID)) +
  theme_classic() + 
  scale_colour_viridis_d(name = "Location", option = "magma", begin = 0, end = 0.8) +
  annotate(geom = "rect", xmin = mean(kudu$dawn_start), xmax = mean(kudu$dawn_end), ymin = -Inf, ymax = Inf, fill = "pink") +
  annotate(geom = "rect", xmin = mean(kudu$dusk_start), xmax = mean(kudu$dusk_end), ymin = -Inf, ymax = Inf, fill = "pink") +
  annotate(geom = "rect", xmin = 0, xmax = mean(kudu$dawn_start), ymin = -Inf, ymax = Inf, fill = "grey70") +
  annotate(geom = "rect", xmin = mean(kudu$dusk_end), xmax = 24, ymin = -Inf, ymax = Inf, fill = "grey70") +
  labs(x = "Hour", y = "Density of detections") +
  geom_density(size = 1) + ggtitle("Tragelaphus strepsiceros")  + theme(plot.title = element_text(size = 11))

#Blue wildebeest Connochaetes taurinus
blue <- camera_trap.df %>% filter(snapshotName == "wildebeestblue")
blue_plot <-
  ggplot(blue, aes(x = hourmin, colour = locationID)) +
  theme_classic() + 
  scale_colour_viridis_d(name = "Location", option = "magma", begin = 0, end = 0.8) +
  annotate(geom = "rect", xmin = mean(blue$dawn_start), xmax = mean(blue$dawn_end), ymin = -Inf, ymax = Inf, fill = "pink") +
  annotate(geom = "rect", xmin = mean(blue$dusk_start), xmax = mean(blue$dusk_end), ymin = -Inf, ymax = Inf, fill = "pink") +
  annotate(geom = "rect", xmin = 0, xmax = mean(blue$dawn_start), ymin = -Inf, ymax = Inf, fill = "grey70") +
  annotate(geom = "rect", xmin = mean(blue$dusk_end), xmax = 24, ymin = -Inf, ymax = Inf, fill = "grey70") +
  labs(x = "Hour", y = "Density of detections") +
  geom_density(size = 1) + ggtitle("Connochaetes taurinus")  + theme(plot.title = element_text(size = 11))

# Section 4: Beaked whale HARP data ----------------------------------------------------

#this is passive acoustic monitoring data on rare beaked whale species from dryad
#https://doi.org/10.5061/dryad.gf1vhhmw0

#MTT: Time of event as Matlab datenumber (days elapsed since January 0, 0000). Each row represents one detection.
#we need to convert this into a readable datetime from the matlab format into the r format
#then plot the number of detections per hour (or half hour)

#for mesoplodon mirus
filenames1 <- list.files(here("Artiodactyla_activity_data/Solsona-Berga_2024_M_mirus"), pattern = "*.mat", full.names = TRUE)
#each file is named after the site it was recorded at and the disk number (tends to be multiple per site)
files1 <- lapply(filenames1, readMat)
#make a dataframe out of just the time column from each mat file, all other information is irrelevant
files1 <- lapply(files1, function(x) as.data.frame(x$MTT))
#extract the location name from the file name
filenames1 <- str_remove(filenames1, "C:/Users/ameli/Documents/R_projects/cetacean_sleep/Artiodactyla_activity_data/Solsona-Berga_2024_M_mirus")
#add in metadata from file names
names(files1) <- filenames1
#append all mat files together into one large dataframe of observations
detections1 <- do.call(rbind,files1)
detections1$Species <- "Mesoplodon mirus" #184,801 detections

# # #for mesoplodon europaeus
filenames2 <- list.files(here("Artiodactyla_activity_data/Solsona-Berga_2024_M_europaeus"), pattern = "*.mat", full.names = TRUE)
#each file is named after the site it was recorded at and the disk number (tends to be multiple per site)
files2 <- lapply(filenames2, readMat)
#make a dataframe out of just the time column from each mat file, all other information is irrelevant
files2 <- lapply(files2, function(x) as.data.frame(x$MTT))
#extract the location name from the file name
filenames2 <- str_remove(filenames2, "C:/Users/ameli/Documents/R_projects/cetacean_sleep/Artiodactyla_activity_data/Solsona-Berga_2024_M_europaeus")
#add in metadata from file names
names(files2) <- filenames2
#append all mat files together into one large dataframe of observations
detections2 <- do.call(rbind,files2)
detections2$Species <- "Mesoplodon europaeus" #129,3807 detections

#for kogiia sima
filenames3 <- list.files(here("Artiodactyla_activity_data/Solsona-Berga_2024_Kogia"), pattern = "*.mat", full.names = TRUE)
#each file is named after the site it was recorded at and the disk number (tends to be multiple per site)
files3 <- lapply(filenames3, readMat)
#make a dataframe out of just the time column from each mat file, all other information is irrelevant
files3 <- lapply(files3, function(x) as.data.frame(x$MTT))
#extract the location name from the file name
filenames3 <- str_remove(filenames3, "C:/Users/ameli/Documents/R_projects/cetacean_sleep/Artiodactyla_activity_data/Solsona-Berga_2024_Kogia")
#add in metadata from file names
names(files3) <- filenames3
#append all mat files together into one large dataframe of observations
detections3 <- do.call(rbind,files3)
detections3$Species <- "Kogia sima" #1,970 detections

#bind together all three species
detections <- rbind(detections1, detections2, detections3)

#now we have a df with 1,479,858 rows, each with the timepoint of a detection in MATLAB datetime format
detections$metadata <- row.names(detections)
colnames(detections) <- c("MATLAB_datetime", "Species", "metadata")

#MTT: Time of event as Matlab datenumber (days elapsed since January 0, 0000).
#refer to https://stackoverflow.com/questions/30072063/how-to-extract-the-time-using-r-from-a-matlab-serial-date-number
Matlab2Rdate <- function(val) as.Date(val - 1, origin = '0000-01-01') 
Matlab2Rdate(733038.6)
"2006-12-27"
Matlab2Rdate(735147.4)
"2012-10-05"

(735147.4 - 719529)*86400
#test datetime conversion
as.POSIXct(1349427015.16854, origin = "1970-01-01", tz = "UTC")
"2012-10-05 08:50:15 UTC"

#first convert the times into the R format 
detections <- detections %>% mutate(R_datetime = (MATLAB_datetime - 719529)*86400)

#extract the datetime 
#Kogia includes Sept - Nov 2011, October - Dec 2012,May -June + Nov-Dec 2016 and March + May 2017
#M mirus includes observations from many months in 2015-2018
detections <- detections %>% mutate(times = as.POSIXct(R_datetime, origin = "1970-01-01", tz = "UTC"))

#separate out into component parts 
detections$year <- year(detections$times)
detections$month <- month(detections$times)
detections$day <- day(detections$times)
detections$hour <- hour(detections$times)
detections$minute <- minute(detections$times)
detections$second <- second(detections$times)
detections$hourmin <- as.numeric(detections$hour) + as.numeric((detections$minute)/60)

#plot the total detections in each hour bin
ggplot(detections, aes(x = hourmin, colour = Species)) + geom_density() + theme_classic()

#for each site add the latitude and longitude (manually)
detections$location <-str_replace(detections$metadata, pattern = r"([a-zA-Z0-9|/]*_([a-zA-Z0-9]*)_.*)", replacement = r"(\1)")

detections$lat <- "Unknown"
detections$lon <- "Unknown"
unique(detections$location)

###Kogia sites
#HH site: 25 (N), -85 (W)
detections[detections$location == "HH01", "lat"] <- 25
detections[detections$location == "HH01", "lon"] <- -85

#BS site: 30 (N), -77 (W)
detections[detections$location == "BS", "lat"] <- 30
detections[detections$location == "BS", "lon"] <- -77

###M mirus sites
#HZ site: 41 (N), -66 (W)
detections[detections$location == "HZ", "lat"] <- 41
detections[detections$location == "HZ", "lon"] <- -66

#BR site: 40 (N), -68 (W)
detections[detections$location == "BR", "lat"] <- 40
detections[detections$location == "BR", "lon"] <- -68

#NC site: 40 (N), -70 (W)
detections[detections$location == "NC", "lat"] <- 40
detections[detections$location == "NC", "lon"] <- -70

#NFC-A site: 37 (N), -75 (W)
detections[detections$location == "A", "lat"] <- 37
detections[detections$location == "A", "lon"] <- -75

#M europaeus sites (also BS and HH)
#DT site: 25 (N), -85 (W)
detections[detections$location %in% c("DT", "DT01", "DT02"), "lat"] <- 25
detections[detections$location %in% c("DT", "DT01", "DT02"), "lon"] <- -85

#MC site: 28 (N), -88 (W)
detections[detections$location %in% c("MC05", "MC06"), "lat"] <- 28
detections[detections$location %in% c("MC05", "MC06"), "lon"] <- -88

#GC site: 27(N), -92 (W)
detections[detections$location %in% c("GC01", "GC02", "GC03", "GC04"), "lat"] <- 27
detections[detections$location %in% c("GC01", "GC02", "GC03", "GC04"), "lon"] <- -92

#BM disk4 site: 32(N), -65 (W)
detections[detections$location %in% c("disk04"), "lat"] <- 32
detections[detections$location %in% c("disk04"), "lon"] <- -65

#HAT B site: 35(N), -75 (W)
detections[detections$location %in% c("B"), "lat"] <- 35
detections[detections$location %in% c("B"), "lon"] <- -75

#JAX site (disk10,13,16): 30(N), -80 (W)
detections[detections$location %in% c("disk10", "disk13", "disk16"), "lat"] <- 30
detections[detections$location %in% c("disk10", "disk13", "disk16"), "lon"] <- -80

#for now filter out the observations with unknown locations
detections <- detections %>% filter(!location %in% c("disk01", "disk02", "disk03"))

detections$lat <- as.numeric(detections$lat)
detections$lon <- as.numeric(detections$lon)
detections$timezone <- tz_lookup_coords(detections$lat, detections$lon, method = "accurate")
detections$date <- as.Date(round_date(detections$times))
                     
#use suncalc to get sunrise and sunset times
#site is the Golf of Mexico, Howell Hook. Lat = 25N, long = 85W
#test run: sunrise should be x, sunset at x. Returns 5:35 sunrise and 5:23pm (17:23) sunset!
getSunlightTimes(date = as.Date(parse_date_time("2012-10-05", orders = "ymd")), lat = 25, lon = -85, tz = tz_lookup_coords(25, -85))

#get sunrise and sunset times for all locations
#use nautical dawn as the start of dawn and sunrise end as the end (encompasses dawn) -about an hour
#use sunset start as the start of dusk and nautical dusk as the end (encompasses dusk) -also about an hour

sun_times <- getSunlightTimes(data = detections[, c("date", "lon", "lat")], 
                              keep = c("sunriseEnd", "sunsetStart", "nauticalDawn", "nauticalDusk"),
                              tz = detections[, c("timezone")])


sun_times <- sun_times %>% separate_wider_delim(cols = c(4:7), delim = " ", names_sep = "_") %>% select(date, nauticalDawn_2, nauticalDusk_2, sunriseEnd_2 ,sunsetStart_2)
sun_times <- sun_times %>% separate_wider_delim(cols = c(2:5), delim = ":", names_sep = "_") %>% select(date, nauticalDawn_2_1, nauticalDawn_2_2, nauticalDusk_2_1, nauticalDusk_2_2, sunriseEnd_2_1, sunriseEnd_2_2, sunsetStart_2_1, sunsetStart_2_2)
sun_times <- sun_times %>% mutate(dawn_start = as.numeric(nauticalDawn_2_1) + as.numeric(nauticalDawn_2_2)/60) %>% 
  mutate(dawn_end = as.numeric(sunriseEnd_2_1) + as.numeric(sunriseEnd_2_2)/60) %>% 
  mutate(dusk_start = as.numeric(sunsetStart_2_1) + as.numeric(sunsetStart_2_2)/60) %>% 
  mutate(dusk_end = as.numeric(nauticalDusk_2_1) + as.numeric(nauticalDusk_2_2)/60) %>% select(date, dusk_start, dusk_end, dawn_start, dawn_end)

detections <- cbind(detections, sun_times[, -c(1)])

#replace location codes with their names
unique(detections$location)
detections$location <- str_replace(detections$location, "A", "Norfolk canyon")
detections$location <- str_replace(detections$location, "B", "Hattaras")
detections$location <- str_replace(detections$location, "HattarasS", "Blake Spur")
detections$location <- str_replace(detections$location, "disk04", "Bermuda")

detections$location <- str_replace(detections$location, "HattarasR", "Bear Seamount")
detections$location <- str_replace(detections$location, "HZ", "Heezen canyon")
detections$location <- str_replace(detections$location, "NC", "Nantucket canyon")
detections$location <- str_replace(detections$location, "HH01", "Howell Hook")

detections$location <- str_replace_all(detections$location, c("DT" = "Dry Tortugas", "Dry Tortugas01" = "Dry Tortugas", "Dry Tortugas02" ="Dry Tortugas"))
detections$location <- str_replace_all(detections$location, c("GC01" =  "Green Canyon", "GC02" =  "Green Canyon", "GC03" =  "Green Canyon", "GC04" =  "Green Canyon"))
detections$location <- str_replace_all(detections$location, c("MC05" = "Mississippi Canyon", "MC06" = "Mississippi Canyon"))
detections$location <- str_replace_all(detections$location, c("disk10" ="Jacksonville", "disk13" =  "Jacksonville", "disk16" = "Jacksonville"))

#save out as RDS file to back up to github
saveRDS(detections, file = here("Artiodactyla_activity_data/Solsona-Berga_2024_final.rds"))

#use below
detections <- readRDS(here("Artiodactyla_activity_data/Solsona-Berga_2024_final.rds"))

#plot overall activity 
all_sps_plot <-  detections %>% filter(location != "Bermuda") %>%
  ggplot(., aes(x = hourmin, colour = location)) + 
  theme_classic() +
  scale_colour_viridis_d(name = "Location", option = "viridis") +
  annotate(geom = "rect", xmin = mean(detections$dawn_start, na.rm = TRUE), xmax = mean(detections$dawn_end, na.rm = TRUE), ymin = -Inf, ymax = Inf, fill = "pink") +
  annotate(geom = "rect", xmin = mean(detections$dusk_start, na.rm = TRUE), xmax = mean(detections$dusk_end, na.rm = TRUE), ymin = -Inf, ymax = Inf, fill = "pink") +
  annotate(geom = "rect", xmin = 0, xmax = mean(detections$dawn_start, na.rm = TRUE), ymin = -Inf, ymax = Inf, fill = "grey70") +
  annotate(geom = "rect", xmin = mean(detections$dusk_end, na.rm = TRUE), xmax = 24, ymin = -Inf, ymax = Inf, fill = "grey70") +
  geom_density(size = 1) +
  labs(x = "Hour", y = "Density of detections") + ggtitle("Kogia sima                                Mesoplodon europaeus            Mesoplodon mirus") +
  facet_wrap(~Species, scales = "free") + theme(strip.text = element_blank(), plot.title = element_text(size = 11))


# Section 5: Narwhal ---------------------------------------------------------

#### Narwhals https://www.science.org/doi/10.1126/sciadv.ade0440?adobe_mc=MCMID%3D53649406453315412110550155571971043555%7CMCORGID%3D242B6472541199F70A4C98A6%2540AdobeOrg%7CTS%3D1695155886#supplementary-materials

narwhal <- read.csv(here("Artiodactyla_activity_data/Tervo_2023_Monodon_monoceros.txt"), sep = "\t")

## Parse time into datetime format
narwhal$datetime <- parse_date(narwhal$GPS_time)
narwhal$hour <- hour(narwhal$datetime)
narwhal$minute <- minute(narwhal$datetime)
narwhal$day <- day(narwhal$datetime)
narwhal$month <- month(narwhal$datetime) #all observations in 2018
narwhal$hourmin <- as.numeric(narwhal$hour) + as.numeric((narwhal$minute)/60)
narwhal$date <- as.Date(round_date(narwhal$datetime))

#conducted in East Greenland, coordinates based on average location from figure 3
narwhal$lat <- 70.5
narwhal$lon <- -27
#get the timezone
tz_lookup_coords(70.5, -27, method = "accurate")

#use suncalc to get sunrise and sunset times
#site is East Greenland. Lat = 70.5N, long = -27W
#test run: sunrise should be 3:23, sunset at 20:13. Returns 3:24 sunrise and 20:18 sunset!
getSunlightTimes(date = as.Date(parse_date_time("2018-08-24", orders = "ymd")), lat = 70.5, lon = -27, tz = tz_lookup_coords(70.5, -27))

#get sunrise and sunset times for all locations
#use nautical dawn as the start of dawn and sunrise end as the end (encompasses dawn) -about an hour
#use sunset start as the start of dusk and nautical dusk as the end (encompasses dusk) -also about an hour

#cannot calculate nautical twilight so use dawn and dusk instead
sun_times <- getSunlightTimes(data = narwhal[, c("date", "lon", "lat")], 
                              keep = c("sunriseEnd", "sunsetStart", "dawn", "dusk"),
                              tz = "America/Godthab")


sun_times <- sun_times %>% separate_wider_delim(cols = c(4:7), delim = " ", names_sep = "_") %>% select(date, dawn_2, dusk_2, sunriseEnd_2 ,sunsetStart_2)
sun_times <- sun_times %>% separate_wider_delim(cols = c(2:5), delim = ":", names_sep = "_") %>% select(date, dawn_2_1, dawn_2_2, dusk_2_1, dusk_2_2, sunriseEnd_2_1, sunriseEnd_2_2, sunsetStart_2_1, sunsetStart_2_2)
sun_times <- sun_times %>% mutate(dawn_start = as.numeric(dawn_2_1) + as.numeric(dawn_2_2)/60) %>% 
  mutate(dawn_end = as.numeric(sunriseEnd_2_1) + as.numeric(sunriseEnd_2_2)/60) %>% 
  mutate(dusk_start = as.numeric(sunsetStart_2_1) + as.numeric(sunsetStart_2_2)/60) %>% 
  mutate(dusk_end = as.numeric(dusk_2_1) + as.numeric(dusk_2_2)/60) %>% select(date, dusk_start, dusk_end, dawn_start, dawn_end)

narwhal <- cbind(narwhal, sun_times[, -c(1)])

#save out as RDS file to back up to github
saveRDS(narwhal, file = here("Artiodactyla_activity_data/Tervo_2023_final.rds"))

#use below
narwhal <- readRDS(here("Artiodactyla_activity_data/Tervo_2023_final.rds"))

#plot overall activity 
narwhal_PAM <- 
  narwhal %>% filter(Buzz == 1) %>% 
  ggplot(., aes(x = hourmin, colour = Ind)) + 
  theme_classic() +
  scale_colour_viridis_d(name = "Individual", option = "mako") +
  annotate(geom = "rect", xmin = mean(narwhal$dawn_start, na.rm = TRUE), xmax = mean(narwhal$dawn_end, na.rm = TRUE), ymin = -Inf, ymax = Inf, fill = "pink") +
  annotate(geom = "rect", xmin = mean(narwhal$dusk_start, na.rm = TRUE), xmax = mean(narwhal$dusk_end, na.rm = TRUE), ymin = -Inf, ymax = Inf, fill = "pink") +
  annotate(geom = "rect", xmin = 0, xmax = mean(narwhal$dawn_start, na.rm = TRUE), ymin = -Inf, ymax = Inf, fill = "grey70") +
  annotate(geom = "rect", xmin = mean(narwhal$dusk_end, na.rm = TRUE), xmax = 24, ymin = -Inf, ymax = Inf, fill = "grey70") +
  geom_density(size = 1) + 
  labs(y = "Density of detections", x = "Hour") +
  ggtitle("Monodon monoceros") + theme(plot.title = element_text(size = 11))


#plot depth vs time 
mean_dive <- narwhal %>% filter(day %in% c(25, 26, 27, 28, 29, 30)) %>% #filter for days with full recordings
  mutate(hourmin = round(hourmin, 2)) %>%
  group_by(month, day, hour, minute, dusk_start, dusk_end, dawn_start, dawn_end, Ind) %>% 
  summarize(mean_depth = mean(Depth), buzz = mean(Buzz)) 

mean_dive$hourmin <- as.numeric(mean_dive$hour) + as.numeric((mean_dive$minute)/60)

narwhal_dive <-
  ggplot(mean_dive, aes(x = hourmin, y = mean_depth, colour = Ind)) +
  theme_classic() +
  scale_colour_viridis_d(name = "Individual", option = "mako") +
  annotate(geom = "rect", xmin = mean(mean_dive$dawn_start, na.rm = TRUE), xmax = mean(mean_dive$dawn_end, na.rm = TRUE), ymin = -Inf, ymax = Inf, fill = "pink") +
  annotate(geom = "rect", xmin = mean(mean_dive$dusk_start, na.rm = TRUE), xmax = mean(mean_dive$dusk_end, na.rm = TRUE), ymin = -Inf, ymax = Inf, fill = "pink") +
  annotate(geom = "rect", xmin = 0, xmax = mean(mean_dive$dawn_start, na.rm = TRUE), ymin = -Inf, ymax = Inf, fill = "grey70") +
  annotate(geom = "rect", xmin = mean(mean_dive$dusk_end, na.rm = TRUE), xmax = 24, ymin = -Inf, ymax = Inf, fill = "grey70") +
  labs(x = "Hour", y = "Average depth") +
  geom_smooth() + ggtitle("Monodon monoceros") + theme(plot.title = element_text(size = 11))
  
#does average depth vary with time of day?
narwhal %>%
  group_by(month, day, hour, Ind) %>% 
  summarize(mean_depth = mean(Depth)) %>%
  ggplot(., aes(x = as.factor(hour), y = mean_depth)) + 
  theme_classic() +
  annotate(geom = "rect", xmin = mean(mean_dive$dawn_start, na.rm = TRUE), xmax = mean(mean_dive$dawn_end, na.rm = TRUE), ymin = -Inf, ymax = Inf, fill = "pink") +
  annotate(geom = "rect", xmin = mean(mean_dive$dusk_start, na.rm = TRUE), xmax = mean(mean_dive$dusk_end, na.rm = TRUE), ymin = -Inf, ymax = Inf, fill = "pink") +
  annotate(geom = "rect", xmin = 0, xmax = mean(mean_dive$dawn_start, na.rm = TRUE), ymin = -Inf, ymax = Inf, fill = "grey70") +
  annotate(geom = "rect", xmin = mean(mean_dive$dusk_end, na.rm = TRUE), xmax = 24, ymin = -Inf, ymax = Inf, fill = "grey70") +
  geom_boxplot(outlier.shape = NA)# + facet_wrap(~Ind)


# Section 6: Save out combined plots --------------------------------------

pdf(here("Figure_folder/primary_data_activity_patterns.pdf"), width = 8.5, height = 8)
((impala_plot + theme(legend.position = "none")) + (kudu_plot + theme(legend.position = "none", axis.title.y = element_blank())) + (blue_plot + theme(legend.position = "right", axis.title.y = element_blank())))/
  (vaquita_plot + hyper_plot1 + hyper_plot2) /
  (all_sps_plot + theme(legend.position = "right"))/
  ((narwhal_dive + theme(legend.position = "none")) + (narwhal_PAM + theme(legend.position = "right")) + plot_spacer()) +
  plot_layout(guides = "collect") 
dev.off()
