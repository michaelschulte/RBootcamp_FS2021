# Checking out dataset for final project in Bern R Bootcamp
# Michael

# packages
library(tidyverse)
#library("tidylog", warn.conflicts = FALSE)
library(geonames)
library(nasapower)
options(geonamesUsername="schultem")
options(geonamesHost="api.geonames.org")

weather <- read_csv('_sessions/FinalProject/1_Data/weather_per_coordinate.csv')
appointments <- read_csv('_sessions/FinalProject/1_Data/medical_noshows.csv')
# neighbourhood <- read_csv('_sessions/FinalProject/1_Data/latlon_brazil.csv')

summary(appointments)

# check out age
range(appointments$Age, na.rm = TRUE)
hist(appointments$Age)

# there are two persons with age -1 - replace with NA
appointments <-
appointments %>%
  mutate(Age = case_when(Age < 0 ~ as.integer(NA),
                               TRUE ~ as.integer(Age)))


# how many patients in the df
length(unique(appointments$PatientId))
# distribution of appointments
plot(table(appointments$PatientId))
hist(appointments$PatientId)










# fusion weather into correct position for loaction and day
# there seems to be some digits not showing after the comma ..
weather <-
  weather %>%
  mutate(LON = round(LON,1),
         LAT = round(LAT,1))

neighbourhood <-
neighbourhood %>%
  rename('LON' = 'lng',
         'LAT' = 'lat') %>%
  mutate(LON = round(LON,1),
         LAT = round(LAT,1))

# join weather (based on long lat) with neighbourhood (to add neighbourhood name)
  neighbourhood <-
  neighbourhood %>%
    left_join(weather)

  write_csv(neighbourhood, '_sessions/FinalProject/1_Data/weather.csv')


# PROCESS APPOINTMENTS ----------

# load dataset
appointments <- read_csv('_sessions/FinalProject/1_Data/medical_noshows.csv')

# dplyr magic
appointments <- appointments %>%
  #rename(NoShow = "No-show") %>%
  mutate_if(function(x) all(x %in% c(0, 1)), as.logical) %>%
  select(-Scholarship)

appointments$AppointmentDay <- date(appointments$AppointmentDay)
# write_csv(appointments, '_sessions/FinalProject/1_Data/appointments.csv') # run once!


# GET COORDINATES -----------

# get unique neighborhoods
uni_neighbourhood <- unique(appointments$Neighbourhood)

# function to request lon lat from geonames API
GNsearchAF <- function(x) {  
  res <- GNsearch(name=x, country="BR")  
  return(res[1, ])  
  }

GNsearch(name='Vitória, Espírito Santo', country="BR")  
# 20.2976° S, 40.2958° W



# # loop over city names and reformat  
# GNresult <- sapply(uni_neighbourhood, GNsearchAF)  
# GNresult <- do.call(rbind, GNresult)  
# GNresult <- cbind(city=row.names(GNresult),  
#                   GNresult %>% select(lng, lat, adminName1)) %>%
#                   as.tibble()
# 
# write_csv(GNresult, '_sessions/FinalProject/1_Data/latlon_brazil.csv')


# GET WHEATHER -----------

GNresult = read_csv('_sessions/FinalProject/1_Data/latlon_brazil.csv')

res <- list()
for (i in 1:nrow(GNresult)){
  print(i)
  tmp <-  get_power(community = "AG",
                         lonlat = GNresult %>% slice(i) %>% 
                         select(lng, lat) %>% unlist(),
                         pars = c("RH2M", "T2M", "PRECTOT"),
                         dates = c("2016-04-29", "2016-06-08"),
                         temporal_average = "DAILY")
  class(tmp) = class(tmp)[-1] 
  res[[i]] = tmp
  }
weather = do.call(rbind, res)
weather

# get daily weather
weather <- 
get_power(community = "AG",
          lonlat = c(20.2976, 40.2958),
          pars = c("RH2M", "T2M", "PRECTOT"),
          dates = c("2016-04-29", "2016-06-08"),
          temporal_average = "DAILY")


write_csv(weather, '_sessions/FinalProject/1_Data/weather_per_coordinate.csv')
