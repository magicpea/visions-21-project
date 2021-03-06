# load packages
library(tidyverse)
library(ncdf4)
library(IDPmisc)

# #get files
# nc_data <- nc_open("~/Downloads/flor_20190616-20190916.nc") #depends on your directory
# 
# #create basic dataframe
# fluoro <- NULL
# fluoro$fluoro_a <- ncvar_get(nc_data, "fluorometric_chlorophyll_a")
# fluoro <- as.data.frame(fluoro)
# fluoro$time <- ncvar_get(nc_data, "time")
# fluoro$time <- as.POSIXct(fluoro$time,origin="1900-01-02")
# fluoro$pressure <- ncvar_get(nc_data, "int_ctd_pressure")
# 
# #add in separated out profiles from CSV
# all <- read.csv("~/Downloads/ocean-main/profiles/osb2019.csv") #depends on your directory
# profiles <- NULL
# profiles$start_ascent <- all$X1
# profiles$end_ascent <- all$X3
# profiles <- as.data.frame(profiles)
# profiles$start_ascent <- as.POSIXct(profiles$start_ascent, format = "%Y-%m-%d %H:%M:%S")
# profiles$end_ascent <- as.POSIXct(profiles$end_ascent, format = "%Y-%m-%d %H:%M:%S")
# 
# # subset 8 days in March (1st through 9th)
# time1 <- "2021-03-01 00:00:00"
# time2 <- "2021-03-09 23:59:59"
# time1 <- "2019-06-18 00:00:00"
# time2 <- "2019-06-25 23:59:59"
# profiles <- subset(profiles, start_ascent > time1 & end_ascent < time2)
# fluoro <- subset(fluoro, time > time1 & time < time2)
# fluoro$profile <- "none"
# 
# # add start times to dataframe
# profile_start <- unique(profiles$start_ascent)
# for (i in 1:length(profile_start)){
#   timestamp <- profile_start[i]
#   ind <- which.min(abs(fluoro$time - timestamp))
#   fluoro[ind,]$profile <- "start"
# }
# 
# # add end times to dataframe
# profile_end <- unique(profiles$end_ascent)
# for (i in 1:length(profile_end)){
#   timestamp <- profile_end[i]
#   ind <- which.min(abs(fluoro$time - timestamp))
#   fluoro[ind,]$profile <- "end"
# }
# 
# # add column for profile number
# fluoro$profile_separate <- "none"
# small_dataset <- NULL
# start_ind <- which(fluoro$profile == "start")
# end_ind <- which(fluoro$profile == "end")
# for (start_index in start_ind){
#   print(start_index)
#   profile_start <- fluoro[start_index,]
#   closest <- end_ind - start_index
#   number <- min(closest[closest > 0])
#   end_index <- which(closest == number)
#   end_index_real <- end_ind[end_index]
#   smaller_dataset <- fluoro[start_index:end_index_real,]
#   smaller_dataset$profile_separate <- start_index
#   small_dataset <- rbind(smaller_dataset, small_dataset)
#get files
nc_data <- nc_open("~/Downloads/flor_20190616-20190916.nc") #depends on your directory

#create basic dataframe
fluoro <- NULL
fluoro$fluoro_a <- ncvar_get(nc_data, "fluorometric_chlorophyll_a")
fluoro <- as.data.frame(fluoro)
fluoro$time <- ncvar_get(nc_data, "time")
fluoro$time <- as.POSIXct(fluoro$time,origin="1900-01-02")
fluoro$pressure <- ncvar_get(nc_data, "int_ctd_pressure")

#add in separated out profiles from CSV
all <- read.csv("~/Downloads/ocean-main/profiles/osb2019.csv") #depends on your directory
profiles <- NULL
profiles$start_ascent <- all$X1
profiles$end_ascent <- all$X3
profiles <- as.data.frame(profiles)
profiles$start_ascent <- as.POSIXct(profiles$start_ascent, format = "%Y-%m-%d %H:%M:%S")
profiles$end_ascent <- as.POSIXct(profiles$end_ascent, format = "%Y-%m-%d %H:%M:%S")

# subset 8 days in March (1st through 9th)
# time1 <- "2021-03-01 00:00:00"
# time2 <- "2021-03-09 23:59:59"
time1 <- "2019-06-18 00:00:00"
time2 <- "2019-06-25 23:59:59"
profiles <- subset(profiles, start_ascent > time1 & end_ascent < time2)
fluoro <- subset(fluoro, time > time1 & time < time2)
fluoro$profile <- "none"

# add start times to dataframe
profile_start <- unique(profiles$start_ascent)
for (i in 1:length(profile_start)){
  timestamp <- profile_start[i]
  ind <- which.min(abs(fluoro$time - timestamp))
  fluoro[ind,]$profile <- "start"
}

# add end times to dataframe
profile_end <- unique(profiles$end_ascent)
for (i in 1:length(profile_end)){
  timestamp <- profile_end[i]
  ind <- which.min(abs(fluoro$time - timestamp))
  fluoro[ind,]$profile <- "end"
}

# add column for profile number
fluoro$profile_separate <- "none"
small_dataset <- NULL
start_ind <- which(fluoro$profile == "start")
end_ind <- which(fluoro$profile == "end")
for (start_index in start_ind){
  print(start_index)
  profile_start <- fluoro[start_index,]
  closest <- end_ind - start_index
  number <- min(closest[closest > 0])
  end_index <- which(closest == number)
  end_index_real <- end_ind[end_index]
  smaller_dataset <- fluoro[start_index:end_index_real,]
  smaller_dataset$profile_separate <- start_index
  small_dataset <- rbind(smaller_dataset, small_dataset)
}

# make profiles sequential
profile_separate <- unique(small_dataset$profile_separate)
list_numbers <- c(1:length(profile_separate))
profile_separate <- as.data.frame(profile_separate)
profile_separate$list_numbers <- list_numbers
list_profiles <- profile_separate
small_dataset <- merge(small_dataset, list_profiles, by = "profile_separate")

small_dataset <- read.csv("~/data/OOI_code.csv")

# for loop that flags potential thin layers
profile_list <- unique(small_dataset$list_numbers)
potential_thin_layers <- NULL
for (profile in profile_list){ 
  print(profile)
  fluoro_small <- small_dataset[small_dataset$list_numbers == profile,]
  fluoro_small <- subset(fluoro_small, fluoro_a > 0)
  avg_chl <- mean(fluoro_small$fluoro_a)
  peak_chl <- max(fluoro_small$fluoro_a)
  if (peak_chl >= 3*avg_chl & peak_chl > 1){
    peak_width_find <- peaks(x=fluoro_small$fluoro_a, y=fluoro_small$pressure)
    peak_width_find <- subset(peak_width_find, x > 0)
    peak_width <- max(peak_width_find$w)
    if (peak_width < 5){
      to_bind <- NULL
      to_bind$fluoro_a <- fluoro_small$fluoro_a
      to_bind$pressure <- fluoro_small$pressure
      to_bind$time <- fluoro_small$time
      to_bind$profile <- profile
      to_bind$avg_chl <- avg_chl
      to_bind$peak_chl <- peak_chl
      to_bind$peak_width <- peak_width
      to_bind <- as.data.frame(to_bind)
      potential_thin_layers <- rbind(potential_thin_layers, to_bind)
    }
  }
}

#checking to see if thin chlorophyll peaks last at least 2 profiles
thin_profiles <- unique(potential_thin_layers$profile)
thin_layers <- NULL
for (profile in thin_profiles){
  profile_less <- profile - 1
  profile_more <- profile + 1
  less_test <- which(thin_profiles == profile_less)
  profile_less_test <- thin_profiles[less_test]
  more_test <- which(thin_profiles == profile_more)
  profile_more_test <- thin_profiles[more_test]
  if (length(profile_less_test) != 0 | length(profile_more_test) != 0){
    fluoro_small <- small_dataset[small_dataset$list_numbers == profile,]
    to_bind <- NULL
    to_bind$fluoro_a <- fluoro_small$fluoro_a
    to_bind <- as.data.frame(to_bind)
    to_bind$pressure <- fluoro_small$pressure
    to_bind$time <- fluoro_small$time
    to_bind$profile <- profile
    to_bind$avg_chl <- avg_chl
    to_bind$peak_chl <- peak_chl
    to_bind$peak_width <- peak_width
    to_bind$thin_layer <- TRUE
    thin_layers <- rbind(thin_layers, to_bind)
  }
}
  
#export thin layer list
thin_layers <- write.csv(thin_layers, "~/Downloads/thin_layers.csv")

# visualization
thin_layers <- read.csv("~/Downloads/thin_layers.csv")

a <- ggplot(thin_layers) +
  geom_point(aes(x=fluoro_a, y=pressure, color = profile)) +
  theme_bw() +
  scale_y_reverse() +
  xlab("Chlorophyll a fluorescence (mg/m^3)") +
  ylab("Depth (m)")

thin_layers$profile <- as.character(thin_layers$profile)

profile_list <- as.character(unique(thin_layers$profile))
for (profile_number in profile_list){
  
  print(profile_number)
  
  a <- NULL
  
  thin_layers_small <- subset(thin_layers, profile == profile_number)
  
  png(paste0("~/Downloads/OOI/graphs_2019/", profile_number, ".png"),width=12, height=12, unit="in", res=100)
  a <- ggplot(thin_layers_small) +
    geom_point(aes(x=fluoro_a, y=pressure)) +
    theme_bw() +
    scale_y_reverse() +
    xlab("Chlorophyll a fluorescence (mg/m^3)") +
    ylab("Depth (m)") +
    ggtitle(profile_number)
  print(a)
  dev.off()
  
  png(paste0("~/Downloads/OOI/graphs_2019/", profile_number, "_oxygen.png"),width=12, height=12, unit="in", res=100)
  a <- ggplot(thin_layers_small) +
    geom_point(aes(x=oxygen, y=pressure)) +
    theme_bw() +
    scale_y_reverse() +
    xlab("Oxygen Concentration") +
    ylab("Depth (m)") +
    ggtitle(profile_number)
  print(a)
  dev.off()
  
}

#time of thin layers
time_df <- NULL
for (profile_number in profile_list){
  thin_layers_small <- subset(thin_layers, profile == profile_number)
  temp <- NULL
  temp$time <- thin_layers_small$time[1]
  temp <- as.data.frame(temp)
  temp$peak_chl <- max(thin_layers_small$fluoro_a)
  time_df <- rbind(time_df, temp)
}

png("~/Downloads/OOI/graphs_2019/overall_thin_layers.png",width=12, height=12, unit="in", res=100)
a <- ggplot(time_df) +
  geom_point(aes(x=time, y=peak_chl)) +
  theme_bw() +
  xlab("Time") +
  ylab("Peak Chlorophyll-a Fluorescence (mg/m^3)") +
  ggtitle("Thin Layer Detections Over Time")
print(a)
dev.off()
