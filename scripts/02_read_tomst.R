
library(tidyverse)
library(lubridate)
library(data.table)
library(cowplot)
library(zoo)

# Set date limits to remove implausible dates
mind <- as.Date("2018-06-01", tz = "Etc/GMT-2")
maxd <- as.Date("2022-08-04", tz = "Etc/GMT-2")

# raw_data_dir <- "E:/RASTI22"
raw_data_dir <- "/scratch/project_2007415/microclim/RASTI22"

# List logger data files to read
f <- list.files(raw_data_dir, pattern = "data_", full.names = T, recursive = T)

fi <- data.frame(file = f)

fi$file2 <- gsub("_..csv", "", fi$file)

fi$site <- parse_number(unlist(lapply(fi$file, function(x) rev(strsplit(x, "/")[[1]])[2])))

fi <- fi[order(fi$site),]

fi$tomst_id <- unlist(lapply(fi$file, function(x) as.numeric(strsplit(gsub("data_","",rev(strsplit(x, "/")[[1]])[1]), "_")[[1]][1])))

readdata <- function(i){
  nn <- sum(grepl(i, fi$file2))
  
  if(nn > 1){
    
    fi2 <- fi %>% filter(grepl(i, fi$file2))
    
    df2 <- data.frame()
    for(ii in fi2$file2){
      print(ii)
      d <- fread(ii)
      
      d %>% select(V2,V3,V4,V5,V6,V7) -> d
      
      d %>% filter(!duplicated(.$V2, fromLast = T)) -> d
      
      df2 <- bind_rows(df2, d)
    }
    
    df2 %>% filter(!duplicated(.$V2, fromLast = T)) -> df2
    
    df2$site <- fi[which(fi$file2 == ii),"id"]
    d$tomst_id <- fi[which(fi$file2 == i),"tomst_id"]
    
    df2 %>% mutate(across(V4:V6, ~as.numeric(gsub(",",".\\",.)))) -> df2
    
    df2 %>% mutate(V2 = ymd_hm(V2, tz = "UTC")) %>% 
      mutate(V2 = with_tz(V2, tzone = "Etc/GMT-2")) -> df2
    
    
    return(df2)
    
  } else {
    
    print(i)
    d <- fread(fi$file[fi$file2 == i])
    
    d %>% select(V2,V3,V4,V5,V6,V7) -> d
    
    d %>% filter(!duplicated(.$V2, fromLast = T)) -> d
    
    d %>% mutate(across(V4:V6, ~as.numeric(gsub(",",".\\",.)))) -> d
    
    d$site <- fi[which(fi$file2 == i),"site"]
    d$tomst_id <- fi[which(fi$file2 == i),"tomst_id"]
    
    d %>% mutate(V2 = ymd_hm(V2, tz = "UTC")) %>% 
      mutate(V2 = with_tz(V2, tzone = "Etc/GMT-2")) -> d
    
    return(d)
    
  }
  
}


mylist <- lapply(fi$file2, readdata)
df <- rbindlist( mylist )

# Rename columns
df %>% rename(datetime = V2,
              zone = V3,
              T1 = V4,
              T2 = V5,
              T3 = V6,
              moist = V7) -> df

df %>% arrange(site, datetime) -> df

df %>% group_by(site) %>% 
  summarise(maxdt = max(datetime)) -> maxdt
maxdt <- full_join(maxdt, fi %>% select(site, tomst_id) %>% filter(!duplicated(.)))
fwrite(maxdt, "data/reading_times_2022.csv")
maxdt %>% arrange(maxdt)
maxdt %>% arrange(desc(maxdt))

# Remove implausible dates
df %>% filter(datetime > mind,
              datetime < maxd) -> df

sites <- unique(df$site)

# Calculate different daily values for diagnostics
df %>% mutate(date = as_date(datetime)) %>% 
  group_by(site,date,tomst_id) %>% 
  summarise(soil_mean = mean(T1),
            air_mean = mean(T3)) %>% 
  as.data.frame() -> df2

# create column for error codes
df2 %>% mutate(probl = 0) -> df2

############################################################################
# PLOTTINGS
############################################################################

# Months to plot
times <- seq(floor_date(as_date(min(df2$date)), "month"),
             ceiling_date(as_date(max(df2$date)), "month") + months(1) - days(1),
             by = "month")

# Plot each site month by month
for(siteid in sites){
  # siteid <- "SAA1195"
  print(siteid)
  pdf(paste0("visuals/monthly_", siteid, ".pdf"), 10, 6)
  temp <- df %>% filter(site == siteid)
  
  if(length(na.omit(unique(temp$tomst_id))) > 1){
    
    for(ii in na.omit(unique(temp$tomst_id))){
      
      for (tt in 1:(length(times) - 1)) {
        temp %>% filter(tomst_id == ii) %>%
          filter(datetime >= ymd(times[tt]),
                 datetime < ymd(times[tt + 1])) -> dft
        
        if(nrow(dft %>% filter(complete.cases(.)) > 0)){
          dft %>%
            ggplot(aes_string(x = "datetime")) +
            geom_line(aes_string(y = "T3"), col = "cornflowerblue") +
            geom_line(aes_string(y = "T2"), col = "brown1") +
            geom_line(aes_string(y = "T1"), col = "darkgoldenrod") +
            theme_minimal() +
            ylab("Temperature") + xlab("Date") +
            ggtitle(paste("Site: ", siteid, "; Tomst: ", ii, "; Time: ", times[tt])) +
            scale_x_datetime(date_minor_breaks = "1 day") -> GG1
          
          dft %>%
            ggplot(aes_string(x = "datetime")) +
            geom_line(aes_string(y = "moist"), col = "blue") +
            theme_minimal() +
            ylab("Moisture") + xlab("Date") +
            ggtitle(paste("Site: ", siteid, "; Time: ", times[tt])) +
            scale_x_datetime(date_minor_breaks = "1 day") -> GG2
          
          print(plot_grid(plotlist = list(GG1, GG2), nrow = 2))
        }
      }
    }
  } else {
    for (tt in 1:(length(times) - 1)) {
      
      temp %>% 
        filter(datetime >= ymd(times[tt]),
               datetime < ymd(times[tt + 1])) -> dft
      
      if(nrow(dft %>% filter(complete.cases(.)) > 0)){
        dft %>%
          ggplot(aes_string(x = "datetime")) +
          geom_line(aes_string(y = "T3"), col = "cornflowerblue") +
          geom_line(aes_string(y = "T2"), col = "brown1") +
          geom_line(aes_string(y = "T1"), col = "darkgoldenrod") +
          theme_minimal() +
          ylab("Temperature") + xlab("Date") +
          ggtitle(paste("Site: ", siteid, "; Time: ", times[tt])) +
          scale_x_datetime(date_minor_breaks = "1 day") -> GG1
        
        dft %>%
          ggplot(aes_string(x = "datetime")) +
          geom_line(aes_string(y = "moist"), col = "blue") +
          theme_minimal() +
          ylab("Moisture") + xlab("Date") +
          ggtitle(paste("Site: ", siteid, "; Time: ", times[tt])) +
          scale_x_datetime(date_minor_breaks = "1 day") -> GG2
        
        print(plot_grid(plotlist = list(GG1, GG2), nrow = 2))
      }
    }
  }
  dev.off()
}  

#################################################################################
# Screening each site for possible errors

# SITE = 1
siteid <- 1

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
probls <- c(as_date(as_date("2020-05-28"):as_date("2020-08-01")),
            as_date(as_date("2022-07-21"):as_date("2022-08-02")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = 3
siteid <- 3

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = 8
siteid <- 8

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
probls <- c(as_date(as_date("2020-07-21"):as_date("2020-08-01")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 11
siteid <- 11

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
probls <- c()
hattu <- c() # hattu broken during summer 2020, but does not  seem to affect data

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = 12 
siteid <- 12

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
probls <- c()
hattu <- c() #  hattu broken when visited 2021, but does not seem to affect data

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 15
siteid <- 15

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-14")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = 17
siteid <- 17

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-14")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = 22
siteid <- 22

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-12")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = 25
siteid <- 25

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-29")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = 27
siteid <- 27

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-12")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 29
siteid <- 29

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-29")))
probls <- c(as_date(as_date("2021-09-09"):as_date("2022-08-03")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = 31
siteid <- 31

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-29")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = 36
siteid <- 36

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-14")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = 40
siteid <- 40

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
probls <- c(as_date(as_date("2022-07-21"):as_date("2022-08-02")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = 42
siteid <- 42

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = 44
siteid <- 44

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2
# SITE = 46
siteid <- 46

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 50
siteid <- 50

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 52
siteid <- 52

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 56
siteid <- 56

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
probls <- c(as_date(as_date("2019-09-12"):as_date("2020-08-01")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 60
siteid <- 60

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 63
siteid <- 63

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 64
siteid <- 64

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 65
siteid <- 65

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 66
siteid <- 66

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 68 
siteid <- 68

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 70
siteid <- 70

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
probls <- c()
hattu <- c(as_date(as_date("2020-05-28"):as_date("2020-08-02"))) #hattu detached when visited 2020

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 71
siteid <- 71

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-29")))
probls <- c(as_date(as_date("2019-09-13"):as_date("2020-08-02")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 73
siteid <- 73

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-29")))
probls <- c()
hattu <- c(as_date(as_date("2019-10-01"):as_date("2020-08-02")))

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 77
siteid <- 77

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-29")))
probls <- c(as_date(as_date("2021-08-01"):as_date("2021-08-02")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 79
siteid <- 79

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 83
siteid <- 83

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-01")))
probls <- c()
hattu <- c()
T1moist_probl <- c(as_date(as_date("2022-05-31"):as_date("2022-08-02")))

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% T1moist_probl,
                        9, probl)) -> df2

# SITE = 88
siteid <- 88

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
probls <- c()
hattu <- c(as_date(as_date("2021-06-29"):as_date("2021-08-01")))

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 92
siteid <- 92

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
probls <- c()
hattu <- c()
T3moist_probl <- c(as_date(as_date("2021-06-16"):as_date("2021-08-01")))

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% T3moist_probl,
                        10, probl)) -> df2

# SITE = 95
siteid <- 95

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
probls <- c(as_date(as_date("2022-07-15"):as_date("2022-08-01")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 97
siteid <- 97

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 98
siteid <- 98

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
probls <- c()
hattu <- c(as_date(as_date("2021-06-25"):as_date("2021-08-01")))

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 99
siteid <- 99

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-30")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 102
siteid <- 102

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-01")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 104
siteid <- 104

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-01")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 107
siteid <- 107

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-29")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 109
siteid <- 109

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-29")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 110
siteid <- 110

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-29")))
probls <- c(as_date(as_date("2020-06-24"):as_date("2020-07-28")))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 113
siteid <- 113

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-29")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 116
siteid <- 116

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-06-29")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 118
siteid <- 118

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-01")))
probls <- c(c(as_date(as_date("2021-10-03"):as_date("2022-08-02"))))
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 121
siteid <- 121

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-01")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 123
siteid <- 123

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-01")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2

# SITE = 125
siteid <- 125

office <- c(as_date(as_date(min(df$datetime)):as_date("2019-07-01")))
probls <- c()
hattu <- c()

df2 %>% mutate(probl = ifelse(site == siteid &
                                date %in% office,
                              2, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% probls,
                        1, probl)) %>% 
  mutate(probl = ifelse(site == siteid &
                          date %in% hattu,
                        3, probl)) -> df2


########################################################################
# FILL MISSING TIMESTAMPS WITH NA

df2 <- df2 %>% 
  mutate(site_id = paste0(site, "_", tomst_id))
df <- df %>% 
  mutate(site_id = paste0(site, "_", tomst_id))

sites2 <- unique(df$site_id)
sites2 <- sites2[!grepl("_NA", sites2)]

df3 <- data.frame()
for(i in sites2){
  #i <- "AIL152_94194008"
  
  df %>% filter(site_id == i) -> temp
  
  temp %>% mutate(timediff = as.numeric(datetime - lag(datetime))) -> temp
  temp[1,"timediff"] <- 15
  holes <- table(temp$timediff)
  
  if(max(temp$timediff, na.rm = T) > 15){
    
    print(i)
    
    missingt <- c()
    for(ii in which(temp %>% pull(timediff) > 15)){
      
      temp %>% slice((ii-1):(ii+1)) %>% pull(timediff) -> diffs
      
      if(diffs[1] %% 15 == 0L){
        seq(temp %>% slice(ii-1) %>% pull(datetime),
            temp %>% slice(ii) %>% pull(datetime), by = "15 mins") -> seqs
        
        missingt <- c(missingt, 
                      as.character(seqs[which(!seqs %in% (temp %>% slice((ii-1):(ii+1)) %>% pull(datetime)))]))
        
      } else {
        seq(temp %>% slice(ii-1) %>% pull(datetime),
            temp %>% slice(ii) %>% pull(datetime), by = "10 mins") -> seqs
        
        missingt <- c(missingt, 
                      as.character(seqs[which(!seqs %in% (temp %>% slice((ii-1):(ii+1)) %>% pull(datetime)))]))
        
      }
    }
    
    missingdf <- data.frame(datetime = ymd_hms(missingt),
                            site_id = i)
    
    print(NROW(missingdf))
    
    temp %>% full_join(., missingdf, by = c("datetime", "site_id")) %>% 
      arrange(datetime) %>% 
      select(-timediff) -> temp
    
    df3 <- bind_rows(df3, temp)
    
  } else {
    
    temp %>% select(-timediff) -> temp
    
    df3 <- bind_rows(df3, temp)
  }
}

#################################################################################
# CALCULATE BIASES BASED ON THE NOT-IN-FIELD DATA
#

diffs_all <- data.frame()
for(i in sites2){
  # i <- "2_94194338"
  office <- df2 %>% filter(site_id == i) %>% 
    filter(probl == 2) %>% pull(date)
  office <- office[-which(office == max(office))]
  
  df %>% filter(site_id == i) %>%
    mutate(date = as_date(datetime)) %>% 
    filter(date %in% office) %>% 
    mutate(change1a = abs(T3 - lag(T3,1)),
           change1b = abs(T3 - lag(T3,2)),
           change1c = abs(T3 - lag(T3,3)),
           change1d = abs(T3 - lag(T3,4)),
           change1e = abs(T3 - lag(T3,5)),
           change1f = abs(T3 - lag(T3,6)),
           change1g = abs(T3 - lead(T3,1)),
           change1h = abs(T3 - lead(T3,2)),
           change1i = abs(T3 - lead(T3,3))) %>% 
    rowwise() %>%
    mutate(change1 = max(change1a, change1b, change1c,
                         change1d, change1e, change1f,
                         change1g, change1g, change1i, na.rm = T)) %>%
    mutate(T3 = ifelse(change1 > 0.1250, NA, T3)) %>% 
    filter(!is.na(T3)) %>% 
    as.data.frame() %>% 
    filter(complete.cases(.)) -> temp
  
  means <- c(T1 = mean(temp$T1),
             T2 = mean(temp$T2),
             T3 = mean(temp$T3))
  
  diffs <- round(means - median(means),4)
  
  print(i)
  print(diffs)
  
  diffs_all <- bind_rows(diffs_all,
                         bind_cols(data.frame(site_id = i), 
                                   as.data.frame(t(as.data.frame(diffs)))))
}

fwrite(diffs_all, "output/Correction_temperatures.csv")

###################################################################################
# Write out a error log

elog <- df2 %>% 
  mutate(runid = rleid(probl)) %>% 
  group_by(site, tomst_id, runid) %>% 
  summarise(start_date = min(date),
            end_date = max(date),
            probl = max(probl)) %>% 
  filter(probl != 0)

elog %>% select(-runid) %>% write_csv("output/error_log.csv")

###################################################################################
# Delete erroneous data
#

# Delete not in field data
df3 %>% mutate(date = as_date(datetime)) %>%
  left_join(., df2 %>% select(site, date, probl)) %>% 
  filter(probl != 2) -> df3

# Look for weird extra measurements between real ones
df3 <- df3 %>% 
  mutate(mins = minute(datetime))

for(i in unique(df3$site_id)){
  print(i)
  
  td <- df3 %>% 
    filter(site_id == i)
  
  tb <- table(td$mins)/nrow(td)
  tb <- tb[tb < 0.0001]
  if(sum(tb)*nrow(td) > 0){
    print(paste0("Removing ", sum(tb)*nrow(td), " rows..."))
  }
  
  df3 <- df3 %>% 
    filter(!(site_id == i & mins %in% as.numeric(names(tb))))
  
}

####################################################################
# MASK IMPOSSIBLE VALUES

df3 %>% mutate(T1 = ifelse(T1 < (-60) | T1 > 50, NA, T1),
               T2 = ifelse(T2 < (-60) | T2 > 50, NA, T2),
               T3 = ifelse(T3 < (-60) | T3 > 50, NA, T3),
               moist = ifelse(moist < 200 | moist >= 4096, NA, moist)) -> df3

###############################################################################
# PLOT CORRECTED

pdf("visuals/Temperature_graphs_corrected.pdf", 12, 10)
for(i in sites){
  #i <- "L12
  print(i)
  df3 %>% filter(site == i) %>% 
    mutate(T1 = as.numeric(ifelse(probl %in% c(1,4,9), NA, T1))) %>% 
    mutate(T2 = as.numeric(ifelse(probl %in% c(1,7,8), NA, T2))) %>% 
    mutate(T3 = as.numeric(ifelse(probl %in% c(1,4,5,7,8), NA, T3))) %>% 
    #group_by(date) %>% 
    #summarise_at(vars(i, "soil"), funs(mean, min, max), na.rm = T) %>% 
    #lapply(function(x) replace(x, is.infinite(x),NA)) %>% as_tibble() %>% 
    ggplot(aes_string(x="datetime")) +
    geom_line(aes_string(y = "T3"), col = "cornflowerblue") +
    geom_line(aes_string(y = "T2"), col = "brown1") +
    geom_line(aes_string(y = "T1"), col = "darkgoldenrod") +
    theme_minimal() +
    ylab("Temperature") + xlab("Date")+
    scale_y_continuous(limits = c(-20, 35))+
    ggtitle(i) -> GG1
  
  df3 %>% filter(site == i) %>% 
    mutate(moist = as.numeric(ifelse(probl %in% c(1,6,8,9), NA, moist))) %>% 
    mutate(moist = as.numeric(ifelse(T1 <= 1, NA, moist))) %>% 
    #group_by(date) %>% 
    #summarise_at(vars(i, "soil"), funs(mean, min, max), na.rm = T) %>% 
    #lapply(function(x) replace(x, is.infinite(x),NA)) %>% as_tibble() %>% 
    ggplot(aes_string(x="datetime")) +
    geom_line(aes_string(y = "moist"), col = "black") +
    theme_minimal() +
    ylab("Soil moisture count") + xlab("Date")+
    scale_y_continuous(limits = c(500, 4000))+
    ggtitle(i) -> GG2
  
  print(plot_grid(plotlist = list(GG1, GG2), nrow = 2))
  
}
dev.off()

fwrite(df3 %>% select(-c(zone, site_id, date, mins)) %>% 
         relocate(site, tomst_id) %>% rename(error_tomst = probl), 
       "output/tomst_data_raw.csv")

####################################################################################