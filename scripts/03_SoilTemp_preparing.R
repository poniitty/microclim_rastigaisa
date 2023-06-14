library(tidyverse)
library(lubridate)
library(data.table)
library(ggplot2)
library(cowplot)
library(sf)

# Function for site identifiers
add_zeros <- function(x){
  if(nchar(x) == 1){
    return(as.character(paste0("00",x)))
  }
  if(nchar(x) == 2){
    return(as.character(paste0("0",x)))
  }
  if(nchar(x) > 2){
    return(as.character(x))
  }
}

##################################################################################
# Rastigaisa

d <- fread("output/tomst_data.csv") %>% 
  mutate(id_code = paste0("RAS",unlist(lapply(site, add_zeros)))) %>% 
  rename(error_tomst = probl) %>% 
  select(id_code, datetime, error_tomst, T1:T3, moist)

d %>% mutate(moist = ifelse(T1 < 1, NA, moist),
             T1 = ifelse(error_tomst %in% c(1,2,4,9,11), NA, T1),
             T2 = ifelse(error_tomst %in% c(1,2,7,8,11), NA, T2),
             T3 = ifelse(error_tomst %in% c(1:5,7,8,10), NA, T3),
             moist = ifelse(error_tomst %in% c(1,2,6,8:10), NA, moist)) -> d

# Calibration function for the moisture count values for unknown soils from Kopecky et al. 2020
cal_funNA <- function(x) {((-1.34e-8) * (x^2) + (2.50e-4) * x + (-1.58e-1))*100 }

# Calibrate the moisture values
d %>% mutate(moist = round(cal_funNA(moist),1)) -> d

d %>% filter(if_any(T1:moist, ~ !is.na(.))) %>% 
  select(-error_tomst) -> d

d %>% mutate(Year = year(datetime),
             Month = month(datetime),
             Day = mday(datetime),
             Time = format(datetime, "%H:%M", tz = "GMT")) -> d

d %>% mutate(Raw_data_identifier = paste0(id_code,"_tomst")) %>% 
  rename(Soil_moisture = moist) %>% 
  select(Raw_data_identifier, Year, Month, Day, Time, T1, T2, T3, Soil_moisture) -> d

############################################
# META

p <- st_read("data/points.gpkg")

p %>% mutate(Location_code = paste0(area, unlist(lapply(id, add_zeros))),
             Latitude = st_coordinates(.)[,"Y"],
             Longitude = st_coordinates(.)[,"X"]) %>% 
  filter(logger == "Tomst") %>% 
  st_drop_geometry() %>% 
  select(Location_code:Longitude) %>% 
  mutate(Raw_data_identifier = paste0(Location_code, "_tomst"),
         EPSG = 4326,
         GPS_accuracy = 5) -> p


d %>% select(Raw_data_identifier, T1:Soil_moisture) %>% 
  mutate(Country_code = "NO") %>% 
  pivot_longer(cols = T1:Soil_moisture, names_to = "Sensor_code") %>% 
  group_by(Country_code, Raw_data_identifier, Sensor_code) %>% 
  count() %>% 
  select(-n) -> meta

meta %>% mutate(Location_code = gsub("_tomst", "", Raw_data_identifier),
                Logger_code = Raw_data_identifier) %>% 
  relocate(Location_code, .after = Country_code) %>% 
  relocate(Logger_code, .after = Location_code) %>% 
  mutate(Sensor_height = ifelse(Sensor_code == "Soil_moisture", -15, NA),
         Sensor_height = ifelse(Sensor_code == "T1", -6, Sensor_height),
         Sensor_height = ifelse(Sensor_code == "T2", 2, Sensor_height),
         Sensor_height = ifelse(Sensor_code == "T3", 15, Sensor_height)) %>% 
  relocate(Raw_data_identifier, .after = Sensor_height) %>% 
  mutate(Microclimate_measurement = "Temperature",
         Microclimate_measurement = ifelse(Sensor_code == "Soil_moisture", "Soil_moisture", Microclimate_measurement)) -> meta


left_join(meta, p) %>% 
  mutate(Logger_brand = "Tomst",
         Logger_type = "TMS-4",
         Sensor_accuracy = 0.5,
         Logger_shielding = "Shield") %>% 
  mutate(Logger_shielding = ifelse(Sensor_code %in% c("T1","Soil_moisture"), "No shield", Logger_shielding)) -> meta



d %>% mutate(date = as_date(paste(Year,Month,Day,sep = "_"))) %>% 
  select(Raw_data_identifier, date, T1:Soil_moisture) %>% 
  pivot_longer(cols = T1:Soil_moisture, names_to = "Sensor_code") %>% 
  filter(!is.na(value)) %>% 
  group_by(Raw_data_identifier, Sensor_code) %>% 
  summarise(mind = min(date),
            maxd = max(date)) %>% 
  mutate(Start_date_year = year(mind),
         Start_date_month = month(mind),
         Start_date_date = day(mind),
         End_date_year = year(maxd),
         End_date_month = month(maxd),
         End_date_date = day(maxd)) %>% 
  select(-mind,-maxd) -> dates

left_join(meta, dates) %>% 
  mutate(Temporal_resolution = 15,
         Timezone = "UTC") %>% 
  mutate(Species_composition = "yes") %>% 
  mutate(Species_trait = "no") -> meta

meta %>% mutate(Plot_size = NA,
                Forest_canopy_cover = NA,
                Data_open_access = "yes",
                Meta_data_open_access = "yes",
                FirstName = "Tuuli",
                LastName = "Rissanen",
                Email = "tuuli.rissanen@helsinki.fi",
                Institute = "University of Helsinki",
                Other_contributors = "") -> meta

# HABITAT CLASSES

meta %>% 
  mutate(Habitat_type = "4",
         Habitat_sub_type = "4.2") -> meta

meta %>% relocate(Habitat_type:Habitat_sub_type, .after = Logger_shielding) -> meta

fwrite(meta, "output/SoilTemp_NO_TR_RASTI_meta.csv")
fwrite(d, "output/SoilTemp_NO_TR_RASTI.csv")
